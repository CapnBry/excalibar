#include "pcapserver.h"

cMain::cMain(HINSTANCE hInstance)
{
	hInst = hInstance;
	pMain = this;
}

void cMain::StatusUpdate(const char* fmt, ...)
{
	char sBuf[2048];

	va_list marker;
	va_start(marker, fmt);
	char sNewText[1024*4];
	vsprintf(sNewText, fmt, marker);
	va_end(marker);

	GetDlgItemText(hMainWnd,IDC_STATUSEDIT,(LPSTR)&sBuf,sizeof(sBuf));
	strcat(sBuf,sNewText);
	SetDlgItemText(hMainWnd,IDC_STATUSEDIT,sBuf);
}

bool cMain::InitMain()
{
	StatusUpdate("%s %s\r\n\r\n",CAPTIONSTRING,
 								 VERSIONSTRING);

	if(!pSniffer->StartSniffer())
		return false;

	if(!pDStream->StartDStream())
		return false;

	return true;
}

LRESULT CALLBACK cMain::MainDlgProc( HWND hDlg, 
									 UINT msg, 
									 WPARAM wParam, 
									 LPARAM lParam )
{
	switch(msg) 
	{		
		case WM_INITDIALOG:
		{
			pMain->hMainWnd = hDlg;
			pMain->InitMain();

			return true;
		}
		case WM_COMMAND:
		{
			switch(LOWORD(wParam))
			{
				case IDCANCEL:
				{
					EndDialog(hDlg,LOWORD(wParam));
					PostQuitMessage(0);
					return true;
				}
			}
		}
		default:
			return false;
	}
	return true;
}

bool cMain::Run()
{
	MSG  msg;

	//create the dialog box
	HWND hWnd = CreateDialog(hInst, 
							 MAKEINTRESOURCE(ID_MAINDIALOG), 
							 0, 
							 (DLGPROC)MainDlgProc);

	if(hWnd == NULL)
		return false;

	ShowWindow(hWnd,SW_SHOW);

	//main msg loop
	while(GetMessage(&msg, NULL, 0U, 0U)==TRUE)
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
    }

	return true;
}
