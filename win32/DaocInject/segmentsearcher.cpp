#include "segmentsearcher.h"

CSegmentSearch::CSegmentSearch(char *fname)
{
	HANDLE hFile;
	DWORD bytesread, signature;
	IMAGE_DOS_HEADER dos_header;
	IMAGE_FILE_HEADER pe_header;
	IMAGE_OPTIONAL_HEADER opt_header;
	PIMAGE_SECTION_HEADER objtbl_entry;

	if ((hFile = CreateFile(fname, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, 
		NULL, OPEN_EXISTING, FILE_ATTRIBUTE_ARCHIVE, 0)) == 0)
		return;

	ReadFile(hFile, &dos_header, sizeof(dos_header), &bytesread, NULL);
	if (bytesread != sizeof(dos_header) || dos_header.e_magic != IMAGE_DOS_SIGNATURE)
		goto cleanup;

	SetFilePointer(hFile, dos_header.e_lfanew, NULL, FILE_BEGIN);
	ReadFile(hFile, &signature, sizeof(signature), &bytesread, NULL);
	if (bytesread != sizeof(signature) || signature != IMAGE_NT_SIGNATURE)
		goto cleanup;

	ReadFile(hFile, &pe_header, sizeof(pe_header), &bytesread, NULL);
	if (bytesread == sizeof(pe_header) && pe_header.SizeOfOptionalHeader > 0) {
		ReadFile(hFile, &opt_header, sizeof(opt_header), &bytesread, NULL);
		m_ImageBase = opt_header.ImageBase;

		for (int i=0; i<pe_header.NumberOfSections; i++) {
			objtbl_entry = (IMAGE_SECTION_HEADER *)malloc(sizeof(IMAGE_SECTION_HEADER));
			ReadFile(hFile, objtbl_entry, sizeof(IMAGE_SECTION_HEADER), &bytesread, NULL);
			sectionarray.push_back(objtbl_entry);
		}  /* for i -> numsections */
	}  /* if sizeofoptheader */
	

	boymoo = new CBoyMooFileSearch(fname);
cleanup:
	CloseHandle(hFile);
}

PIMAGE_SECTION_HEADER CSegmentSearch::FindSegment(char *segment)
{
	std::vector<PIMAGE_SECTION_HEADER>::iterator it;
	PIMAGE_SECTION_HEADER item;
	for (it=sectionarray.begin(); it!=sectionarray.end(); it++) {
		item = (PIMAGE_SECTION_HEADER)*it;
		if (strcmpi((char *)(item->Name), segment) == 0)
			return *it;
	}
	return NULL;
}

PBYTE CSegmentSearch::FindFirst(char *segment, char *needle, int needlelen)
{
	if (!boymoo)
		return NULL;

	PIMAGE_SECTION_HEADER pseg = FindSegment(segment);
	if (!pseg)
		return NULL;

	boymoo->SetStartPos(pseg->PointerToRawData);
	boymoo->SetEndPos(pseg->PointerToRawData + pseg->SizeOfRawData);
	int offset = boymoo->FindFirst(needle, needlelen);
	if (offset == -1)
		return NULL;
	
	/* convert the file offset to offset within the segment */
	offset -= pseg->PointerToRawData;
	if ((DWORD)offset > pseg->SizeOfRawData)
		return NULL;

	/* return the virtual address of the needle */
	return (PBYTE)IntToPtr(m_ImageBase + pseg->VirtualAddress + offset);
}

CSegmentSearch::~CSegmentSearch(void)
{
	if (boymoo)
		delete boymoo;

	std::vector<PIMAGE_SECTION_HEADER>::iterator it;
	PIMAGE_SECTION_HEADER item;
	for (it=sectionarray.begin(); it!=sectionarray.end(); it++) {
		item = (PIMAGE_SECTION_HEADER)*it;
		free(item);
	}
	sectionarray.clear();
}