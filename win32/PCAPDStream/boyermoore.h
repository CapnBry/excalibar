#ifndef _BOYERMOORE_H_
#define _BOYERMOORE_H_


class CBoyMooSearch 
{
private:
protected:
	int m_skip[256];
	int m_FindPos;
	unsigned char *m_needle;
	int m_needlelen;
	unsigned char *m_haystack;
	int m_haystacklen;

	int m_StartPos;
	int m_EndPos;

	void InitializeFor(const unsigned char* needle, int needlelen);
public:
	CBoyMooSearch(void);

	int FindFirst(const unsigned char* needle, int needlelen);
	int FindFirst(const char* needle) { return FindFirst((const unsigned char*)needle, (int)strlen(needle)); };
	int FindNext(void);

	void SetStartPos(int val) { m_StartPos = val; };
	int GetStartPos(void) { return m_StartPos; };
	void SetEndPos(int val) { m_EndPos = val; };
	int GetEndPos(void) { return m_EndPos; };
};

class CBoyMooTextSearch : public CBoyMooSearch
{
public:
	CBoyMooTextSearch(const char *haystack, int haystacklen) { m_haystack = (unsigned char *)haystack; m_haystacklen = haystacklen; };
	CBoyMooTextSearch(const char *haystack) { m_haystack = (unsigned char *)haystack; m_haystacklen = (int)strlen(haystack); };
};

class CBoyMooFileSearch : public CBoyMooSearch
{
private:
	void InitializeFileMMap(const char *fname);
public:
	CBoyMooFileSearch(const char *fname);
	~CBoyMooFileSearch(void);
};

#endif 

