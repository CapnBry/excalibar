/******************************************************************************
Cheyenne: a real-time packet analyzer/sniffer for Dark Age of Camelot
Copyright (C) 2003, the Cheyenne Developers

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
******************************************************************************/
#include "global.h"
#include "sharenet.h"
#include "central.h"
#include "resource.h"
#include "centraldialogs.h"
#include <sstream>

struct CLIENT_DISPLAY_DATA
{
    SOCKADDR_IN local_addr;
    SOCKADDR_IN remote_addr;
    CheyenneTime LastPingTime;
    CheyenneTime LastPingSendTime;
    DWORD IntervalBytes;
};


class ClientDisplayFunctor
{
public:
    ClientDisplayFunctor(std::list<CLIENT_DISPLAY_DATA>& s):data_list(s){};
    ClientDisplayFunctor(const ClientDisplayFunctor& s):data_list(s.data_list){};
    ~ClientDisplayFunctor(){};

    void operator()(const ShareNetClientData& s)
    {
        CLIENT_DISPLAY_DATA data;
        data.local_addr=s.GetLocalAddr();
        data.remote_addr=s.GetRemoteAddr();
        data.LastPingTime=s.GetLastPingTime();
        data.LastPingSendTime=s.GetLastPingSendTime();
        data.IntervalBytes=s.GetAndResetIntervalBytes();
        
        data_list.insert(data_list.end(),data);
        return;
    }
    
private:
    std::list<CLIENT_DISPLAY_DATA>& data_list;
};


Central::Central() : RedrawTimerId(1)
{
    hMainWnd=NULL;
    hTahoma=NULL;
    hTahomaBig=NULL;
} // end Central

Central::~Central()
{
    // don't need the font anymore
    DeleteObject(hTahoma);
    DeleteObject(hTahomaBig);

} // end ~Central

WPARAM Central::Go(HINSTANCE hInst)
{
    hInstance=hInst;

    if(!Init())
        {
        return(-1);
        }
       
    ShareNet.Go();

    MSG msg;
    CheyenneTime LastRenderTime=Clock.Current();

    while(GetMessage(&msg,hMainWnd,0,0)==TRUE)
        {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
        }
       
    ShareNet.Stop();

    return(msg.wParam);

} // end Go

bool Central::Init(void)
{
    // register window class

    WNDCLASS wc;
    ZeroMemory(&wc,sizeof(wc));
    wc.hbrBackground=(HBRUSH)(COLOR_WINDOW+1);
    wc.hCursor=LoadCursor(NULL,IDC_ARROW);
    wc.hIcon=LoadIcon(hInstance,MAKEINTRESOURCE(IDI_A));
    wc.hInstance=hInstance;
    wc.lpfnWndProc=Central::WindowProc;
    wc.lpszClassName="ShareServerMainClass";
    wc.lpszMenuName=MAKEINTRESOURCE(IDR_MAIN_MENU);
    wc.style=CS_DBLCLKS;

    if(!RegisterClass(&wc))
        {
        return(false);
        }

    // create window
    hMainWnd=CreateWindowEx
        (
        0,
        "ShareServerMainClass",
        "Share Server",
        WS_OVERLAPPEDWINDOW|WS_VISIBLE|WS_CLIPCHILDREN,
        0,
        0,
        640,
        480,
        NULL,
        NULL,
        hInstance,
        LPVOID(this)
        );

    if(!hMainWnd || hMainWnd==INVALID_HANDLE_VALUE)
        {
        return(false);
        }

    ShowWindow(hMainWnd,SW_SHOW);
    UpdateWindow(hMainWnd);

    MoveWindow(hMainWnd,0,0,320,200,TRUE);

    return(true);
} // end Init

LRESULT CALLBACK Central::WindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    // get pointer to "me"
    Central* pMe=(Central*)(GetWindowLong(hWnd,GWL_USERDATA));

    switch(uMsg)
        {
        case WM_CREATE:
            {
            CREATESTRUCT* pcs=(CREATESTRUCT*)(lParam);

            SetWindowLong(hWnd,GWL_USERDATA,LONG(pcs->lpCreateParams));
            pMe=(Central*)LONG(pcs->lpCreateParams);

            LOGFONT lf;
            ZeroMemory(&lf,sizeof(lf));
            lf.lfHeight=12;
            lf.lfWidth=0;
            lf.lfEscapement=0;
            lf.lfOrientation=0;
            lf.lfWeight=FW_NORMAL;
            lf.lfCharSet=ANSI_CHARSET;
            lf.lfOutPrecision=OUT_DEFAULT_PRECIS;
            lf.lfClipPrecision=CLIP_DEFAULT_PRECIS;
            lf.lfQuality=ANTIALIASED_QUALITY;
            lf.lfPitchAndFamily=DEFAULT_PITCH;
            strcpy(lf.lfFaceName,"Tahoma");

            pMe->hTahoma=CreateFontIndirect(&lf);

            ZeroMemory(&lf,sizeof(lf));
            lf.lfHeight=14;
            lf.lfWidth=0;
            lf.lfEscapement=0;
            lf.lfOrientation=0;
            lf.lfWeight=FW_NORMAL;
            lf.lfCharSet=ANSI_CHARSET;
            lf.lfOutPrecision=OUT_DEFAULT_PRECIS;
            lf.lfClipPrecision=CLIP_DEFAULT_PRECIS;
            lf.lfQuality=ANTIALIASED_QUALITY;
            lf.lfPitchAndFamily=DEFAULT_PITCH;
            strcpy(lf.lfFaceName,"Tahoma");

            pMe->hTahomaBig=CreateFontIndirect(&lf);

            SetTimer(hWnd,pMe->RedrawTimerId,1000,NULL);
            }
            break;

        case WM_DESTROY:
            PostQuitMessage(0);
            break;

        case WM_COMMAND:
            pMe->HandleCommand(hWnd,uMsg,wParam,lParam);
            break;

        case WM_TIMER:
            {
            HDC hDC=GetDC(hWnd);
            pMe->PaintWindow(hDC);
            ReleaseDC(hWnd,hDC);
            }
            break;
            
        case WM_PAINT:
            {
            PAINTSTRUCT ps;
            BeginPaint(hWnd,&ps);
            pMe->PaintWindow(ps.hdc);
            EndPaint(hWnd,&ps);
            }
            break;
            
        default:
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
        } // end switch message
    return(0);
} // end WindowProc

void Central::HandleCommand(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    switch(LOWORD(wParam))
        {
        case ID_SYSTEM_EXIT:
            // bye!
            DestroyWindow(hMainWnd);
            break;

        case ID_SYSTEM_ALLOWEDHOSTS:
            {
            ShareNetConfig TempCfg=::Config;
            
            // do dialog
            int res=DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_SET_ALLOWEDHOSTS),
                hWnd,
                (DLGPROC)AllowedHostsDlgProc,
                (LPARAM)&TempCfg
                );
             
             if(res==IDOK)
                {
                // store temp config into global config
                ::Config=TempCfg;
                }
             }
            break;
        
        default:
            break;
        }

    // done
    return;
} // end HandleCommand

void Central::PaintWindow(HDC hFront)const
{
    // build list of connections
    std::list<CLIENT_DISPLAY_DATA> conn_list;
    ShareNet.EnumerateConnectedClients(ClientDisplayFunctor(conn_list));
    
    // make display string
    std::ostringstream oss;
    std::list<CLIENT_DISPLAY_DATA>::iterator it;
    
    oss << unsigned int(Clock.Current().Seconds()) << ": " << conn_list.size() << " Active Connections:\n";
    for(it=conn_list.begin();it!=conn_list.end();++it)
        {
        oss << it->local_addr << " <--> " << it->remote_addr
            << "\n    " << it->IntervalBytes << " bytes last interval"
            << "\n    Last ping time: " << it->LastPingTime.Seconds()
            << "\n    Last ping send time: " << it->LastPingSendTime.Seconds() << "\n";
        }
    
    // double buffer
    HDC hBack=CreateCompatibleDC(hFront);
    RECT r;
    GetClientRect(hMainWnd,&r);
    RECT rDraw;
    CopyRect(&rDraw,&r);
    HBITMAP hBitmap=CreateCompatibleBitmap(hBack,r.right-r.left,r.bottom-r.top);
    HFONT hOldFont=(HFONT)SelectObject(hBack,hTahomaBig);
    HBITMAP hOldBitmap=(HBITMAP)SelectObject(hBack,hBitmap);
    
    // empty
    FillRect(hBack,&r,(HBRUSH)(COLOR_WINDOW+1));
    rDraw.left++;
    // draw text
    DrawText
        (
        hBack,
        oss.str().c_str(),
        -1,
        &rDraw,
        DT_WORDBREAK
        );
        
    // bit blit
    BitBlt
        (
        hFront,
        0,
        0,
        r.right-r.left,
        r.bottom-r.top,
        hBack,
        0,
        0,
        SRCCOPY
        );
    
    // done with these
    SelectObject(hBack,hOldBitmap);
    SelectObject(hBack,hOldFont);
    DeleteObject(hBitmap);
    DeleteDC(hBack);
    
    // done
    return;
} // end PaintWindow
