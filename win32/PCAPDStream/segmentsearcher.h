#ifndef _SEGMENTSEARCHER_H_
#define _SEGMENTSEARCHER_H_

// predefine
class CBoyMooSearch;

class CSegmentSearch
{
private:
    CSegmentSearch(const CSegmentSearch&); // disallow
    CSegmentSearch& operator=(const CSegmentSearch&); // disallow

protected:
	DWORD m_ImageBase;
	CBoyMooSearch *boymoo;
	std::vector<PIMAGE_SECTION_HEADER> sectionarray;

	PIMAGE_SECTION_HEADER FindSegment(const char *segment);
	virtual DWORD GetSearchStartForSegment(PIMAGE_SECTION_HEADER segment);

public:
	CSegmentSearch(void);
	~CSegmentSearch(void);
	PBYTE FindFirst(const char *segment,const char *needle, const size_t needlelen);
	PBYTE FindFirst(const char *segment,const char *needle) 
	{
	    return FindFirst((const char*)segment,(const char*)needle, strlen(needle));
	}
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