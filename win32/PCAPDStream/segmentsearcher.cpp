#include <vector>
#include <windows.h>
#include "boyermoore.h"
#include "segmentsearcher.h"

const char* const EXPECTED_MODULE_BASE=(const char*)0x400000;

CSegmentSearch::CSegmentSearch(void) : m_ImageBase(0)
{
	boymoo = NULL;
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

PIMAGE_SECTION_HEADER CSegmentSearch::FindSegment(const char *segment)
{
	std::vector<PIMAGE_SECTION_HEADER>::iterator it;
	PIMAGE_SECTION_HEADER item;
	for (it=sectionarray.begin(); it!=sectionarray.end(); it++) {
		item = (PIMAGE_SECTION_HEADER)*it;
		if (strcmpi((const char *)(item->Name),(const char *)segment) == 0)
			return *it;
	}
	return NULL;
}

PBYTE CSegmentSearch::FindFirst(const char *segment,const char *needle, const size_t needlelen)
{
	if (!boymoo)
		return NULL;

	PIMAGE_SECTION_HEADER pseg = FindSegment(segment);
	if (!pseg)
		return NULL;

	DWORD srchstart = GetSearchStartForSegment(pseg);
	boymoo->SetStartPos(srchstart);
	boymoo->SetEndPos(srchstart + pseg->SizeOfRawData);
	int offset = boymoo->FindFirst(needle, needlelen);
	if (offset == -1)
		return NULL;
	
	/* convert the file offset to offset within the segment */
	offset -= srchstart;
	if ((DWORD)offset > pseg->SizeOfRawData)
		return NULL;

	/* return the virtual address of the needle */
	return (PBYTE)IntToPtr(m_ImageBase + pseg->VirtualAddress + offset);
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
