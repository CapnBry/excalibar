#ifndef _SEGMENTSEARCHER_H_
#define _SEGMENTSEARCHER_H_

#include <windows.h>
#include <vector>
#include "boyermoore.h"

class CSegmentSearch
{
protected:
	DWORD m_ImageBase;
	CBoyMooSearch *boymoo;
	std::vector<PIMAGE_SECTION_HEADER> sectionarray;

	PIMAGE_SECTION_HEADER FindSegment(char *segment);
	virtual DWORD GetSearchStartForSegment(PIMAGE_SECTION_HEADER segment);

public:
	CSegmentSearch(void);
	~CSegmentSearch(void);
	PBYTE FindFirst(char *segment, char *needle, int needlelen);
	PBYTE FindFirst(char *segment, char *needle) { return FindFirst(segment, needle, strlen(needle)); }
};

class CSegmentFileSearch : public CSegmentSearch
{
protected:
	void LoadSectionListFromFile(const char *fname);
	DWORD GetSearchStartForSegment(PIMAGE_SECTION_HEADER segment);

public:
	CSegmentFileSearch(const char *fname);
};

class CSegmentProcessSearch : public CSegmentSearch
{
protected:
	void LoadSectionListFromProcess(const DWORD processid);
	DWORD GetSearchStartForSegment(PIMAGE_SECTION_HEADER segment);

public:
	CSegmentProcessSearch(const DWORD processid);
};

#endif