#ifndef _BOYERMOORE_H_
#define _BOYERMOORE_H_

#include <string.h>

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

	void InitializeFor(char* needle, int needlelen);
public:
	CBoyMooSearch(void);

	int FindFirst(char* needle, int needlelen);
	int FindFirst(char* needle) { return FindFirst(needle, (int)strlen(needle)); };
	int FindNext(void);

	void SetStartPos(int val) { m_StartPos = val; };
	int GetStartPos(void) { return m_StartPos; };
	void SetEndPos(int val) { m_EndPos = val; };
	int GetEndPos(void) { return m_EndPos; };
};

class CBoyMooTextSearch : public CBoyMooSearch
{
public:
	CBoyMooTextSearch(char *haystack, int haystacklen) { m_haystack = (unsigned char *)haystack; m_haystacklen = haystacklen; };
	CBoyMooTextSearch(char *haystack) { m_haystack = (unsigned char *)haystack; m_haystacklen = (int)strlen(haystack); };
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

