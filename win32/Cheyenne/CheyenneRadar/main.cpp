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
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#define MAIN_FUNCTION
#include <iostream>
#include <string>
#include "..\Utils\CodeUtils.h"
#include "..\Utils\Logger.h" // for the logger
#include "..\Utils\mapinfo.h" // for the zones class
#include "..\Utils\times.h" // for the clock
#include "..\GLPPI\GLPPI.h" // for actor render prefs
#include "config.h" // for the global configuration
#include "main.h"
#include "resource.h"

// prototypes
HWND Init(const char* class_name,HINSTANCE hInst);

int WINAPI WinMain
    (
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPSTR lpCmdLine,
    int nCmdShow
    )
{
    // attach a console
    AttachConsole();
    // init logger
    Logger.Init("CheyenneRadar.log");
    
    LOG_FUNC << "Hi!\n";
    
    // load config
    ::RadarConfig.Load("CheyenneRadar.cfg");
    
    // read zones
    Zones.ReadZoneFile();

    // read region offsets file
    Zones.ReadRegionOffsets();
    
    // start the clock
    ::Clock.Go();
    
    {
    // save initial directory
    TCHAR* cd=new TCHAR[MAX_PATH+1];
    GetCurrentDirectory(MAX_PATH,cd);
    ::InitialDir=cd;
    delete cd;
    // make sure theres a trailing '\' in there
    if(::InitialDir.at(::InitialDir.length()-1) != '\\')
        {
        ::InitialDir+='\\';
        }
    }
    
    MSG msg;
    CheyenneTime LastRenderTime=Clock.Current();

    if(::RadarConfig.GetRaisePriority())
        {
        // raise priority
        SetPriorityClass(GetCurrentProcess(),HIGH_PRIORITY_CLASS);
        }

    // lower GUI priority (we do this regardless of process base priority
    SetThreadPriority(GetCurrentThread(),THREAD_PRIORITY_BELOW_NORMAL);
    
    // create window
    HWND hMainWnd;
    WPARAM result=-1;
    if((hMainWnd=Init("cheyenne_main_class",hInstance)) == 0)
        {
        LOG_FUNC << "init failed!\n";
        result=-1;
        goto Exit;
        }
    
    // do forever
    while(1)
        {
        while(PeekMessage(&msg,NULL,0,0,PM_REMOVE)==TRUE)
            {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
            }

        if(msg.message != WM_QUIT)
            {
            // see if time has advanced at all
            if((Clock.Current() - LastRenderTime).Seconds() >= 0.033) // <-- this sets FPS to a max of 30
                {
                // save time
                LastRenderTime=Clock.Current();

                // we advanced in time, render!
                PostMessage(hMainWnd,RENDER_NOW,0,0);
                }
            else
                {
                // time has not changed, sleep a little bit
                Sleep(10);
                }
            }
        else
            {
            // got quit message
            break;
            }
        }

    result=msg.wParam;
    
    // exit point
    Exit:
    
    // save config
    ::RadarConfig.Save("CheyenneRadar.cfg");

    LOG_FUNC << "Exiting!\n";
    
    // done with console
    DetachConsole();
    
    // done
    return(result);
} // end WinMain

HWND Init(const char* class_name,HINSTANCE hInst)
{
    // register window class
    WNDCLASS wc;
    ZeroMemory(&wc,sizeof(wc));
    wc.hbrBackground=(HBRUSH)(COLOR_WINDOW+1);
    wc.hCursor=LoadCursor(NULL,IDC_ARROW);
    wc.hIcon=LoadIcon(hInst,MAKEINTRESOURCE(IDI_A));
    wc.hInstance=hInst;
    wc.lpfnWndProc=MainWndProc;
    wc.lpszClassName=class_name;
    wc.lpszMenuName=MAKEINTRESOURCE(IDR_MAIN_MENU);
    wc.style=CS_DBLCLKS;

    if(!RegisterClass(&wc))
        {
        return(0);
        }

    // create window
    HWND h=CreateWindowEx
        (
        0,
        class_name,
        "Cheyenne",
        WS_OVERLAPPEDWINDOW|WS_VISIBLE|WS_CLIPCHILDREN,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        800,
        600,
        NULL,
        NULL,
        hInst,
        NULL
        );

    if(!h || h==INVALID_HANDLE_VALUE)
        {
        return(0);
        }

    LOG_FUNC << "created main window with handle 0x" << std::hex << h << std::dec << "\n";
    
    ShowWindow(h,SW_SHOW);
    UpdateWindow(h);

    // done
    return(h);
} // end Init
