#ifndef _SEGMENTSEARCHER_H_
#define _SEGMENTSEARCHER_H_

#include <windows.h>
#include <vector>
#include "boyermoore.h"

class CSegmentSearch
{
protected:
	DWORD m_ImageBase;
	CBoyMooFileSearch *boymoo;
	std::vector<PIMAGE_SECTION_HEADER> sectionarray;

	PIMAGE_SECTION_HEADER FindSegment(char *segment);

public:
	CSegmentSearch(char *fname);
	~CSegmentSearch(void);
	PBYTE FindFirst(char *segment, char *needle, int needlelen);
};

#endif