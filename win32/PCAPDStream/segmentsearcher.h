#ifndef _SEGMENTSEARCHER_H_
#define _SEGMENTSEARCHER_H_

// predefine
class CBoyMooFileSearch;

class CSegmentSearch
{
protected:
	DWORD m_ImageBase;
	CBoyMooFileSearch *boymoo;
	std::vector<PIMAGE_SECTION_HEADER> sectionarray;

	PIMAGE_SECTION_HEADER FindSegment(const char *segment);

public:
	CSegmentSearch(const char *fname);
	~CSegmentSearch(void);
	PBYTE FindFirst(const char *segment,const unsigned char *needle, int needlelen);
};

#endif