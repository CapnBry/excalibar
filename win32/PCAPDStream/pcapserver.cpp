#include "pcapserver.h"
#include <io.h>
#include <fcntl.h>
#include <conio.h>
#include <stdio.h>

cMain *pMain = NULL;
cSniffer *pSniffer = NULL;
cDStream *pDStream = NULL;
bool bContinue=true;

// bit of code courtesy Cheyenne :)
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
	
	#ifdef _DEBUG
	if(strlen(lpCmdLine)>0)
	    {
	    TestSegSearch(lpCmdLine);
	    }
	#endif
	cMain MainApp(hInstance);
	cSniffer Sniffer;
	cDStream DStream;

	MainApp.Run();
    
	bContinue=false;
	Sleep(1000);

	#ifdef _DEBUG
	DetachConsole();
	#endif
	return 1;
}