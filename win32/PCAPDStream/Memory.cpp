#include "pcapserver.h"

//decrypt source from dol project, thx :p
void cMemory::Decrypt( unsigned char *buf, unsigned int len) 
{
	if( CryptKey ==NULL) return;
	if( buf ==NULL) return;
	if( len ==0) return;
	unsigned char tmpkey[256];
	memcpy( tmpkey, CryptKey, 256);

	unsigned char var_1 =0;
	unsigned char bl =0;
	unsigned int esi =len /2;
	for( unsigned int edi =esi;edi<len;edi++) {
		var_1++;
		unsigned char cl =tmpkey[var_1];
		bl +=cl;
		unsigned char arg_tmp =cl;
		unsigned char dl =tmpkey[bl];
		tmpkey[var_1] =dl;
		dl =arg_tmp;
		tmpkey[bl] =dl;
		unsigned char al =tmpkey[var_1];
		al +=dl;
		al =tmpkey[al];
		buf[edi] ^=al;
		bl +=buf[edi];
	}
	for( edi=0;edi<esi;edi++) {
		var_1++;
		unsigned char dl =tmpkey[var_1];
		bl +=dl;
		unsigned char arg_tmp =bl;
		bl =tmpkey[arg_tmp];
		tmpkey[var_1] =bl;
		tmpkey[arg_tmp] =dl;
		unsigned char al =tmpkey[var_1];
		al +=dl;
		bl =arg_tmp;
		al =tmpkey[al];
		buf[edi] ^=al;
		bl +=buf[edi];
	}
}

DWORD cMemory::FindGameProcess() 
{ 
    HANDLE         hProcessSnap = NULL; 
    BOOL           bRet			= FALSE; 
    PROCESSENTRY32 pe32			= {0}; 
 
    hProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); 
    if(hProcessSnap == (HANDLE)-1)return(0); 
    pe32.dwSize = sizeof(PROCESSENTRY32); 

    if (Process32First(hProcessSnap, &pe32)) 
    { 
		do
		{
			if(strcmp(pe32.szExeFile,"game.dll") == 0)
			{								
				return(pe32.th32ProcessID);
			} 
		}
        while (Process32Next(hProcessSnap, &pe32));         
    }     

    CloseHandle (hProcessSnap); 
    return(0); 
} 

unsigned long cMemory::FindMemOffset(HANDLE hProcess)
{
#define SEARCHSIZE 0x10000
	int i,i2;

	unsigned char string[] =//hauptschleife die den key generiert
	{ 0x0F, 0xB6, 0xC3,		//movzx eax, bl
	  0x03, 0xFA,			//add edi, edx
	  0x03, 0xC7,			//add eax, edi
	  0x8B, 0xFE,			//mov edi, esi
	  0x99,					//cdq
	  0xF7, 0xFF,			//idiv edi
	  0x41,					//inc ecx
	  0x3B, 0xCE,			//cmp ecx, esi
	  0x8B, 0xFA,			//mov edi, edx
	  0x8D, 0x87 };			//lea offset

	unsigned char *ptr = (unsigned char *)VirtualAlloc(NULL,SEARCHSIZE,MEM_COMMIT,PAGE_READWRITE);	
	ZeroMemory(ptr,SEARCHSIZE);

	ReadProcessMemory(hProcess,(LPCVOID)0x400000,ptr,SEARCHSIZE,0);

	for(i = 0;i < SEARCHSIZE;i++)
	{
		for(i2 = 0;i2 < sizeof(string);i2++)
		{
			if(ptr[i2] != string[i2])
				break;
		}
		if(i2 == sizeof(string))
		{			
			char tbuf[4];
			tbuf[0] = ptr[i2];
			tbuf[1] = ptr[i2+1];
			tbuf[2] = ptr[i2+2];
			tbuf[3] = ptr[i2+3];
			return ((unsigned long *)tbuf)[0];			
		}
		ptr++;
	}

	return 0;
}

bool cMemory::GetKey()
{
	unsigned long dwGameID;	
	HANDLE hGameProc;

	if((dwGameID = FindGameProcess()) == NULL)
	{
		pMain->StatusUpdate("couldn't find DAoC process\r\n");
        return false;
    }

	if((hGameProc = OpenProcess(PROCESS_ALL_ACCESS, FALSE, dwGameID)) == NULL)
	{
		pMain->StatusUpdate("can't open DAoC process\r\n");
        return false;
    }

	unsigned long KeyOffset;// = 0xA46A48;
	KeyOffset = FindMemOffset(hGameProc);

	if(KeyOffset == 0)
	{
		pMain->StatusUpdate("crypt key not found in memory\r\n");
		return false;
	}
	
	do
	{
		ReadProcessMemory(hGameProc,(LPCVOID)KeyOffset,CryptKey,256,0);
		Sleep(50);
	}
    while(((long *)&CryptKey)[0] == 0);   	


	pMain->StatusUpdate("crypt key found\r\n");

	CloseHandle(hGameProc);
	CryptKeySet = true;

	return true;
}
