#include "pcapserver.h"
#include <io.h>
#include <fcntl.h>
#include <conio.h>
#include <string>
#include <iostream>
#include <fstream>

cMain *pMain = NULL;
cSniffer *pSniffer = NULL;
cDStream *pDStream = NULL;
bool bContinue=true;
Aurelia_T Aurelia=NULL;
Brutus_T Brutus=NULL;
trolok45_T trolok45=NULL;
abfallanfang_T abfallanfang=NULL;
void* p=NULL;
unsigned short port=0;
std::string account_name;
bool euro_server;

// bit of code courtesy Cheyenne :)
void SET_CHECK_BOOL(HWND hwnd,UINT control,bool bool_value)
{
    SendDlgItemMessage(hwnd,control,BM_SETCHECK,(LPARAM)(bool_value ? BST_CHECKED:BST_UNCHECKED),0);
}

bool GET_CHECK_BOOL(HWND hwnd,UINT control)
{
    return(SendDlgItemMessage(hwnd,control,BM_GETCHECK,0,0)==BST_CHECKED?true:false);
}

void SET_EDIT_STRING(HWND hwnd,UINT control,const std::string& std_str)
{
    SendDlgItemMessage(hwnd,control,WM_SETTEXT,0,(LPARAM)(std_str.c_str()));
}

void GET_EDIT_STRING(HWND hwnd,UINT control,std::string& std_str) 
{
    char* edit_str;
    LRESULT len=1+SendDlgItemMessage(hwnd,control,WM_GETTEXTLENGTH,0,0);
    edit_str=new char[len];
    edit_str[0]='\0';
    SendDlgItemMessage(hwnd,control,WM_GETTEXT,len,(WPARAM)(edit_str));
    std_str=edit_str;
    delete[] edit_str;
}

void AttachConsole(void)
{
    // code copied from Microsoft Knowledge Base Article - 105305
    // and the silly article had an error in it (!)
    int hCrt;
    FILE *hf;

    AllocConsole();
    hCrt = _open_osfhandle((long) GetStdHandle(STD_OUTPUT_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "w" );
    *stdout = *hf;
    setvbuf( stdout, NULL, _IONBF, 0 );

    // do it again for stdin
    hCrt = _open_osfhandle((long) GetStdHandle(STD_INPUT_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "r" );
    *stdin = *hf;
    setvbuf( stdin, NULL, _IONBF, 0 );

    // and again for stderr
    hCrt = _open_osfhandle((long) GetStdHandle(STD_ERROR_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "w" );
    *stderr = *hf;
    setvbuf( stderr, NULL, _IONBF, 0 );

    // done
    return;
} // end AtttachConsole

void DetachConsole(void)
{
    // free the console
    FreeConsole();

    // done
    return;
} // end DetatchConsole

void ReadIni(const std::string& filename)
{
    std::ifstream file(filename.c_str());
    if(!file.is_open())
        {
        return;
        }
    
    file >> std::ws;
    std::getline(file,::account_name);
    file >> std::ws >> ::euro_server;
} // end ReadIni

void WriteIni(const std::string& filename)
{
    std::ofstream file(filename.c_str());
    if(!file.is_open())
        {
        return;
        }
    
    file << (::account_name.length()?account_name:"<no account>") << std::endl
         << ::euro_server << std::endl;
} // end WriteIni

unsigned short GetWord(unsigned char *data)
{
	char buf[2];
	buf[0] = data[1];
	buf[1] = data[0];
	return ((unsigned short *)buf)[0];
}

int APIENTRY WinMain( HINSTANCE hInstance,
					  HINSTANCE hPrevInstance,
					  LPSTR lpCmdLine,
					  int nCmdShow )
{

	#ifdef _DEBUG
	AttachConsole();
	#endif
	
	// read ini file
	ReadIni("pcapdstream.ini");
	
	// look for port override on the command line
	if(strlen(lpCmdLine)>0)
	    {
    	// if port !=0 then use it and don't do the test seg search
    	::port=atoi(lpCmdLine);
    	if(::port=0)
    	    {
    	    #ifdef _DEBUG
	        TestSegSearch(lpCmdLine);
    	    #endif
    	    }
	    }
	
    HMODULE BrutusLib=LoadLibrary("UndSieBrutus.dll");
    if(!BrutusLib)
        {
        MessageBox(NULL,"missing UndSieBrutus.dll","Error",MB_OK);
        return(-1);
        }
    
    // import functions
    Aurelia=(Aurelia_T)GetProcAddress(BrutusLib,(LPCSTR)1);
    Brutus=(Brutus_T)GetProcAddress(BrutusLib,(LPCSTR)2);
    abfallanfang=(abfallanfang_T)GetProcAddress(BrutusLib,(LPCSTR)3);
    trolok45=(trolok45_T)GetProcAddress(BrutusLib,(LPCSTR)5);
    p=Aurelia();
	
	// select network device and get accoun/euro info
	SelectDevice(hInstance);
	
	{
	cMain MainApp(hInstance);
	cDStream DStream;
	cSniffer Sniffer;

	MainApp.Run();
    
	bContinue=false;
	Sleep(1000);
	}

	Brutus(p);
	FreeLibrary(BrutusLib);
	
	// write ini file
	WriteIni("pcapdstream.ini");

	#ifdef _DEBUG
	DetachConsole();
	#endif
	return 1;
}

bool GameRunning(void)
{
  HWND hDAOCWnd; 
  DWORD retVal; 
  
  hDAOCWnd = FindWindow("DAoCMWC", NULL); 
  if (hDAOCWnd) 
    {
    if (GetWindowThreadProcessId(hDAOCWnd,&retVal))
        {
        return true; 
        }
    }
  return false; 
} // end GameRunning