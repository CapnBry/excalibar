#include "pcapserver.h"

cMain *pMain = NULL;
cSniffer *pSniffer = NULL;
cDStream *pDStream = NULL;

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

	//test if already run
	SECURITY_ATTRIBUTES sa;
	HANDLE	hMutex;
	sa.nLength              = sizeof(sa);
	sa.bInheritHandle       = TRUE;
	sa.lpSecurityDescriptor = NULL;
    hMutex = CreateMutex(&sa, FALSE, "TESTMUTEX");
	DWORD dwError = GetLastError();
	if(dwError == ERROR_ALREADY_EXISTS)
	{
		ReleaseMutex(hMutex);
		return 0;
	}

	cMain MainApp(hInstance);
	cSniffer Sniffer;
	cDStream DStream;

	MainApp.Run();

	return 1;
}