#include <windows.h>
#include <stdio.h>
#include "boyermoore.h"

CBoyMooSearch::CBoyMooSearch(void) : m_StartPos(0), m_EndPos(0)
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

int CBoyMooSearch::FindFirst(char* needle, int needlelen)
{
	InitializeFor(needle, needlelen);
	m_FindPos = m_StartPos - 1;
	return FindNext();
}

int CBoyMooSearch::FindNext(void)
{
	int i;

	if (m_FindPos == -1)
		m_FindPos = m_needlelen - 1;
	else
		m_FindPos += m_needlelen;

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

