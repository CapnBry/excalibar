#include "pcapserver.h"
#include <fstream>
#include <string>

unsigned char* search_string=NULL;
size_t sizeof_search_string=0;
size_t search_size=0;
bool allocated_search_string=false;

cMemory::cMemory()
{
    // attempt to load searching info from file
    std::ifstream f("key_search");
    std::string line;
    if(f.is_open())
        {
        f >> std::hex // use hex
          >> std::ws // skip whitespace
          >> search_size // get search size then
          >> std::ws // skip whitespace
          >> sizeof_search_string; // get sizeof search string then
          
        // sanity check
        if(sizeof_search_string > 4096)
            {
            throw "sizeof_search_string > 4096!";
            }
        
        // allocate space for search string
        search_string=new unsigned char[sizeof_search_string];
        allocated_search_string=true;
        
        // finally, get the search string
        unsigned int d;
        for(size_t i=0;i<sizeof_search_string;++i)
            {
            f >> std::ws >> d;
            search_string[i]=(unsigned char)d;
            }
        } // end if file opened successfully
} 
cMemory::~cMemory()
{
    if(allocated_search_string)
        {
        delete[] search_string;
        }
}

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
	size_t i,i2;

	if(search_string==NULL)
	    {
        std::cerr << "[cMemory::FindMemOffset] using defaults for search string\n";
	    // init search string to default
	    
	    static unsigned char string[] =//hauptschleife die den key generiert
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
	    
	    search_string=&(string[0]);
	    sizeof_search_string=sizeof(string);
	    search_size=0x10000;
	    }
	else
	    {
	    #ifdef _DEBUG
        std::cout << "[cMemory::FindMemOffset] using key_search file for search string\n";
        std::cout << "search size=" << search_size << "\n"
                  << "sizeof(search_string)=" << sizeof_search_string << "\n"
                  << "search string=\n";
        std::cout << std::hex;
        for(size_t xx=0;xx<sizeof_search_string;++xx)
            {
            std::cout << (unsigned int)(search_string[xx]) << "\n";
            }
        std::cout << std::dec;
        #endif
	    }

	unsigned char *ptr = (unsigned char *)VirtualAlloc(NULL,search_size,MEM_COMMIT,PAGE_READWRITE);	
	unsigned char *const original_mem=ptr;
	ZeroMemory(ptr,search_size);
	unsigned char *BaseAddr=(unsigned char *)0x400000;

	while(ReadProcessMemory(hProcess,BaseAddr,ptr,search_size,0)==TRUE)
	    {
	    ptr=original_mem; // init to the original
	    
	    for(i = 0;i < search_size;i++)
    	    {
		    for(i2 = 0;i2 < sizeof_search_string;i2++)
	    	    {
			    if(ptr[i2] != search_string[i2])
			        {
				    break;
				    }
		        }
		    if(i2 == sizeof_search_string)
		        {			
			    char tbuf[4];
			    tbuf[0] = ptr[i2];
			    tbuf[1] = ptr[i2+1];
			    tbuf[2] = ptr[i2+2];
			    tbuf[3] = ptr[i2+3];
    			
			    // don't forget to free the memory
			    VirtualFree(original_mem,0,MEM_RELEASE);
			    return ((unsigned long *)tbuf)[0];			
		        }
		    ptr++;
	        } // end for i
	    
	    // move along
	    BaseAddr+=search_size;
	    } // end while ReadProcessMemory

	// don't forget to free the memory
	VirtualFree(original_mem,0,MEM_RELEASE);
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
