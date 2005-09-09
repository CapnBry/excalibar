#include "segmentsearcher.h"

#define EXPECTED_MODULE_BASE  0x400000

CSegmentSearch::CSegmentSearch(void) : m_ImageBase(0), boymoo(NULL), m_LastSearchSeg(NULL)
{
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

	m_LastSearchSeg = FindSegment(segment);
	if (!m_LastSearchSeg)
		return NULL;

	DWORD srchstart = GetSearchStartForSegment(m_LastSearchSeg);
	boymoo->SetStartPos(srchstart);
	boymoo->SetEndPos(srchstart + m_LastSearchSeg->SizeOfRawData);
	int offset = boymoo->FindFirst(needle, needlelen);

	return AdjustSearchResult(offset);
}

PBYTE CSegmentSearch::FindNext(void)
{
	if (!boymoo)
		return NULL;

	if (!m_LastSearchSeg)
		return NULL;

	int offset = boymoo->FindNext();

	return AdjustSearchResult(offset);
}

PBYTE CSegmentSearch::AdjustSearchResult(int offset)
{
	if (offset == -1)
		return NULL;

	DWORD srchstart = GetSearchStartForSegment(m_LastSearchSeg);

	/* convert the file offset to offset within the segment */
	offset -= srchstart;
	if ((DWORD)offset > m_LastSearchSeg->SizeOfRawData)
		return NULL;

	/* return the virtual address of the needle */
	return (PBYTE)IntToPtr(m_ImageBase + m_LastSearchSeg->VirtualAddress + offset);
}

DWORD CSegmentSearch::GetSearchStartForSegment(PIMAGE_SECTION_HEADER segment)
{
	return 0;
}

CSegmentFileSearch::CSegmentFileSearch(const char *fname)
{
	LoadSectionListFromFile(fname);
	if (m_ImageBase)
		boymoo = new CBoyMooFileSearch(fname);
}

void CSegmentFileSearch::LoadSectionListFromFile(const char *fname)
{
	HANDLE hFile;
	DWORD bytesread;
	IMAGE_DOS_HEADER dos_header;
	IMAGE_NT_HEADERS nt_header;
	PIMAGE_SECTION_HEADER objtbl_entry;

	if ((hFile = CreateFile(fname, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, 
		NULL, OPEN_EXISTING, FILE_ATTRIBUTE_ARCHIVE, 0)) == 0)
		return;

	ReadFile(hFile, &dos_header, sizeof(dos_header), &bytesread, NULL);
	if (bytesread != sizeof(dos_header) || dos_header.e_magic != IMAGE_DOS_SIGNATURE)
		goto cleanup;

	SetFilePointer(hFile, dos_header.e_lfanew, NULL, FILE_BEGIN);
	ReadFile(hFile, &nt_header, sizeof(nt_header), &bytesread, NULL);
	if (bytesread != sizeof(nt_header) || nt_header.Signature != IMAGE_NT_SIGNATURE ||
		nt_header.FileHeader.SizeOfOptionalHeader == 0) 
		goto cleanup;

	m_ImageBase = nt_header.OptionalHeader.ImageBase;

	for (int i=0; i<nt_header.FileHeader.NumberOfSections; i++) {
		objtbl_entry = (IMAGE_SECTION_HEADER *)malloc(sizeof(IMAGE_SECTION_HEADER));
		ReadFile(hFile, objtbl_entry, sizeof(IMAGE_SECTION_HEADER), &bytesread, NULL);
		sectionarray.push_back(objtbl_entry);
	}  /* for i -> numsections */
	
cleanup:
	CloseHandle(hFile);
}

DWORD CSegmentFileSearch::GetSearchStartForSegment(PIMAGE_SECTION_HEADER segment)
{
	return segment->PointerToRawData;
}

CSegmentProcessSearch::CSegmentProcessSearch(const DWORD processid)
{
	LoadSectionListFromProcess(processid);
	boymoo = new CBoyMooProcessSearch(processid);
}

void CSegmentProcessSearch::LoadSectionListFromProcess(const DWORD processid)
{
	HANDLE hProcess;
	char *pMemOffset;
	DWORD bytesread;
	IMAGE_DOS_HEADER dos_header;
	IMAGE_NT_HEADERS nt_header;
	PIMAGE_SECTION_HEADER objtbl_entry;
	if ((hProcess = OpenProcess(PROCESS_VM_READ, false, processid)) == NULL)
		return;

	pMemOffset = (char *)EXPECTED_MODULE_BASE;
	ReadProcessMemory(hProcess, pMemOffset, &dos_header, sizeof(dos_header), &bytesread);
	if (bytesread != sizeof(dos_header) || dos_header.e_magic != IMAGE_DOS_SIGNATURE)
		goto cleanup;

	pMemOffset += dos_header.e_lfanew;

	ReadProcessMemory(hProcess, pMemOffset, &nt_header, sizeof(nt_header), &bytesread);
	pMemOffset += bytesread;
	if (bytesread != sizeof(nt_header) || nt_header.Signature != IMAGE_NT_SIGNATURE ||
		nt_header.FileHeader.SizeOfOptionalHeader == 0)
		goto cleanup;

	m_ImageBase = nt_header.OptionalHeader.ImageBase;

	for (int i=0; i<nt_header.FileHeader.NumberOfSections; i++) {
		objtbl_entry = (IMAGE_SECTION_HEADER *)malloc(sizeof(IMAGE_SECTION_HEADER));
		ReadProcessMemory(hProcess, pMemOffset, objtbl_entry, sizeof(IMAGE_SECTION_HEADER), &bytesread);
		pMemOffset += bytesread;
		sectionarray.push_back(objtbl_entry);
	}  /* for i -> numsections */

cleanup:
	CloseHandle(hProcess);
}

DWORD CSegmentProcessSearch::GetSearchStartForSegment(PIMAGE_SECTION_HEADER segment)
{
	/* virtual address is actually the RVA, so add the image base */
	return m_ImageBase + segment->VirtualAddress;
}
