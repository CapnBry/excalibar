#include <windows.h>
#include <stdio.h>
#include "boyermoore.h"

CBoyMooSearch::CBoyMooSearch(void) : m_StartPos(0), m_EndPos(0),
	m_haystack(NULL), m_haystacklen(0)
{
}

void CBoyMooSearch::InitializeFor(char* needle, int needlelen)
{
	m_needle = (unsigned char *)needle;
	m_needlelen = needlelen;

	int i;
	for(i=0; i<256; i++)  
		m_skip[i] = m_needlelen;
	for(i=0; i<m_needlelen; i++) 
		m_skip[m_needle[i]] = m_needlelen - 1 - i;
}

void CBoyMooSearch::BeforeFindFirst(void)
{
	m_FindPos = m_StartPos - 1;
}

int CBoyMooSearch::FindFirst(char* needle, int needlelen) 
{
	BeforeFindFirst();
	InitializeFor(needle, needlelen);
	return FindNext();
}

int CBoyMooSearch::FindNext(void)
{
	int i;

	if (m_FindPos == -1)
		m_FindPos = m_needlelen - 1;
	else
		m_FindPos += m_needlelen;

	if (m_FindPos > m_haystacklen) {
		m_FindPos = -1;
		return m_FindPos;
	}

	for(i=m_needlelen-1; i>=0; m_FindPos--, i--) {
		while (m_haystack[m_FindPos] != m_needle[i]) {
			// printf("%d %02x %d\n", m_FindPos, m_haystack[m_FindPos], m_skip[m_haystack[m_FindPos]]);
			m_FindPos += max(m_needlelen-i, m_skip[m_haystack[m_FindPos]]);

			if ((m_FindPos >= m_haystacklen) || (m_EndPos && m_FindPos >= m_EndPos)) {
				m_FindPos = -1;
				return m_FindPos;
			}

			i = m_needlelen - 1;
		}  /* while [m_FindPos] != [i] */
	}  /* for */
  
	m_FindPos++;
	return m_FindPos;
}

CBoyMooFileSearch::CBoyMooFileSearch(const char *fname)
{
	InitializeFileMMap(fname);
}

void CBoyMooFileSearch::InitializeFileMMap(const char *fname)
{
	HANDLE hFile, hFileMapping;
	DWORD filesize, filesize_high;

	if ((hFile = CreateFile(fname, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, 
		NULL, OPEN_EXISTING, FILE_ATTRIBUTE_ARCHIVE, 0)) == 0)
		return;

	filesize = GetFileSize(hFile, &filesize_high);
	hFileMapping = CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL);

	if (GetLastError() == ERROR_ALREADY_EXISTS) 
		hFileMapping = 0;
	if (hFileMapping)
		m_haystack = (unsigned char *)MapViewOfFile(hFileMapping, FILE_MAP_READ, 0, 0, 0);
	if (m_haystack)
		m_haystacklen = filesize;
  
	CloseHandle(hFileMapping);
	CloseHandle(hFile);
}

CBoyMooFileSearch::~CBoyMooFileSearch(void)
{
	if (m_haystack) {
		UnmapViewOfFile(m_haystack);
		m_haystack = NULL;
	}
}

CBoyMooProcessSearch::CBoyMooProcessSearch(const unsigned int processid)
{
	m_ProcessID = processid;
	m_LastStartPos = 0;
	m_LastEndPos = 0;
}

CBoyMooProcessSearch::~CBoyMooProcessSearch(void)
{
	FreeHaystack();
}

void CBoyMooProcessSearch::FreeHaystack(void)
{
	if (m_haystack) {
		free(m_haystack);
		m_haystack = NULL;
		m_haystacklen = 0;
	}
}

void CBoyMooProcessSearch::BeforeFindFirst(void)
{
	/* don't call the inherited because it sets the findpos to startpos
	   which is wrong because the processsearch searches the offset
	   starting at startpos */
	m_FindPos = -1;

	if (m_LastStartPos != m_StartPos || m_LastEndPos != m_EndPos) {
		HANDLE hProcess;

		FreeHaystack();
		if ((hProcess = OpenProcess(PROCESS_VM_READ, false, m_ProcessID)) != NULL) {
			DWORD bytesread;

			m_haystacklen = m_EndPos - m_StartPos;
			m_haystack = (unsigned char *)malloc(m_haystacklen);
			ReadProcessMemory(hProcess, (void *)m_StartPos, m_haystack, m_haystacklen, &bytesread);
			if (bytesread != m_haystacklen)
				FreeHaystack();
			CloseHandle(hProcess);
		}  /* if openprocess */

		m_LastStartPos = m_StartPos;
		m_LastEndPos = m_EndPos;
	}  /* if not the same start / end pos */
}

int CBoyMooProcessSearch::FindNext(void)
{
	/* since the return value is an offset into the section of memory, 
	   we need to adjust it */
	int retVal = CBoyMooSearch::FindNext();
	if (retVal != -1)
		retVal += m_StartPos;
	return retVal;
}
