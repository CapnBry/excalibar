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
#include "central.h"
#include "centralfunctors.h"
#include "resource.h"
#include "..\Utils\CheyenneMessages.h" // for message class definitions
#include "..\Database\database.h" // for database class definition
#include "..\Sniffer\sniffer.h" // for sniffer class definition
#include "..\Sharenet\MiniShareServer.h" // for mini share server definition
#include "..\Sharenet\TelnetClient.h" // for telnet server definition
#include "..\Sharenet\Sharenet.h" // for share client definition
#include "..\Script\CSLScriptHost.h" // for script host definition
#include "..\..\Common\PacketStore\PacketStore.h" // include for offline packet storage, if you are not using 
                                                  // offline packet storage, this line can be removed
#include <sstream> // for string stream
#include "CentralDialogs.h" // for dialog proc prototypes

Central::Central()
{
    hMainWnd=0;
    hInstance=0;
    hTahoma=0;
    
    // create fifos here
    MessageFifo=new tsfifo<CheyenneMessage*>;
    TelnetFifoSend=new tsfifo<std::string*>;
    TelnetFifoReceive=new tsfifo<std::string*>;
    
    // create database
    database=new Database;
    // create sniffer
    sniffer=new SNIFFER_T;
    // create sharenet
    sharenet=new MiniShareServer<ShareNetClientData>(::g_Config.GetServerPort());
    // create script host
    scripthost=new csl::CSLScriptHost;
    // assign db pointer to scripthost
    scripthost->ChangeDatabase(database);
    // create script telnet server
    scriptserver=new MiniShareServer<TelnetClientData>(23); // use std telnet port
} // end Central

Central::~Central()
{
    // done with these
    delete database;
    delete sniffer;
    delete sharenet;
    delete scripthost;
    delete scriptserver;
    
    // clean up the fifos
    while(MessageFifo->size() != 0)
        {
        delete MessageFifo->Pop();
        }
    delete MessageFifo; // done with this

    while(TelnetFifoReceive->size() != 0)
        {
        delete TelnetFifoReceive->Pop();
        }
    delete TelnetFifoReceive; // done with this
    
    while(TelnetFifoSend->size() != 0)
        {
        delete TelnetFifoSend->Pop();
        }
    delete TelnetFifoSend; // done with this
} // end ~Central

WPARAM Central::Go(HINSTANCE hInst)
{
    if(!Init(hInst))
        {
        ::Logger << "[Central::Go] Init failed!" << std::endl;
        return(-1);
        }
    
    // init functors
    std::auto_ptr<DatabaseFunctor> sharenet_message(new SharenetMessageFunctor(*this));
    database->InstallFunctor(Database::DatabaseEvents::SharenetMessage,sharenet_message);
    
    // init database, sniffer, and sharenet
    database->Go(MessageFifo);
    sniffer->Go(MessageFifo);
    sharenet->Go(MessageFifo);
    
    std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>* param;
    
    param=new std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>;
    param->first=TelnetFifoReceive;
    param->second=TelnetFifoSend;
    scripthost->Go(param); // CSLScriptHost expects a pair<fifo,fifo> as its go param
                           // first is its input, second is its output. It deletes the pair
                           // when it is done with it, but keeps the fifos (we own those)
    param=new std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>;
    param->first=TelnetFifoSend;
    param->second=TelnetFifoReceive;
    scriptserver->Go(param); // TelnetClientData expects a pair<fifo,fifo> as its go param
                             // first is its input, second is its output. It deletes the pair
                             // when it is done with it, but keeps the fifos (we own those)
    
    MSG msg;
    while(GetMessage(&msg,NULL,0,0) == TRUE)
        {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
        }
        
    // stop these
    database->Stop();
    sniffer->Stop();
    sharenet->Stop();
    scripthost->Stop();
    scriptserver->Stop();

    // return result
    return(msg.wParam);
} // end Go

bool Central::Init(HINSTANCE hInst)
{
    // save instance handle
    hInstance=hInst;
    
    // register window class
    WNDCLASS wc;
    ZeroMemory(&wc,sizeof(wc));
    wc.hbrBackground=(HBRUSH)(COLOR_WINDOW+1);
    wc.hCursor=LoadCursor(NULL,IDC_ARROW);
    wc.hIcon=LoadIcon(hInstance,MAKEINTRESOURCE(IDI_A));
    wc.hInstance=hInstance;
    wc.lpfnWndProc=Central::WindowProc;
    wc.lpszClassName="CheyenneSnifferMainClass";
    wc.lpszMenuName=MAKEINTRESOURCE(IDR_MAIN);
    wc.style=CS_HREDRAW|CS_VREDRAW;

    if(!RegisterClass(&wc))
        {
        return(false);
        }

    // create window
    hMainWnd=CreateWindowEx
        (
        0,
        "CheyenneSnifferMainClass",
        "Cheyenne Sniffer",
        WS_OVERLAPPEDWINDOW|WS_VISIBLE|WS_CLIPCHILDREN,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        320,
        275,
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
    
    // done
    return(true);
} // end Init

LRESULT CALLBACK Central::WindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    // get pointer to "me"
    Central* pMe=reinterpret_cast<Central*>(GetWindowLongPtr(hWnd,GWLP_USERDATA));

    switch(uMsg)
        {
        case WM_CREATE:
            {
            CREATESTRUCT* pcs=(CREATESTRUCT*)(lParam);

            // store this pointer
            SetWindowLongPtr(hWnd,GWLP_USERDATA,LONG_PTR(pcs->lpCreateParams));
            
            // reacquire this pointer
            pMe=reinterpret_cast<Central*>(GetWindowLongPtr(hWnd,GWLP_USERDATA));
            
            // set a 1 second timer
            SetTimer(hWnd,1,1000,NULL);
            
            // create tahoma font
            LOGFONT lf;
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

            pMe->hTahoma=CreateFontIndirect(&lf);
            }
            break;

        case WM_SIZE:
            if(pMe)
                {
                pMe->HandleMainWindowSize();
                return(0);
                }
            else
                {
                return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
                }
            break;

        case WM_SIZING:
            if(pMe)
                {
                pMe->HandleMainWindowSizing(reinterpret_cast<RECT*>(lParam));
                return(1);
                }
            else
                {
                return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
                }
            break;

        case WM_DESTROY:
            // kill the timer
            KillTimer(hWnd,1);
            
            // release the font
            DeleteObject(pMe->hTahoma);
            pMe->hTahoma=0;
            
            PostQuitMessage(0);
            break;

        case WM_COMMAND:
            pMe->HandleCommand(hWnd,uMsg,wParam,lParam);
            break;

        case WM_CLOSE:
            // let default handle it
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
            break;
            
        case WM_PAINT:
            {
            PAINTSTRUCT ps;
            BeginPaint(hWnd,&ps);
            // draw
            pMe->Draw(ps.hdc);
            EndPaint(hWnd,&ps);
            }
            break;
            
        case WM_TIMER:
            {
            HDC hFront=GetDC(hWnd);
            // draw
            pMe->Draw(hFront);
            ReleaseDC(hWnd,hFront);
            }
            break;
            
        case WM_KEYDOWN:
        default:
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
        } // end switch message
    return(0);
} // end WindowProc

void Central::OnSharenetMessage(const void* p,const unsigned int len)
{
    // send to sharenet
    sharenet->QueueOutputMessage(p,len);
    
    // done
    return;
} // end OnSharenetMessage

void Central::HandleMainWindowSizing(RECT* r)
{
} // end HandleMainWindowSizing

void Central::HandleMainWindowSize(void)
{
} // end HandleMainWindowSize

void Central::HandleCommand(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    switch(LOWORD(wParam))
        {
        case ID_SYSTEM_EXIT:
            // bye!
            DestroyWindow(hMainWnd);
            break;

        case ID_MINISHARESERVER_SETPORT:
            {
            // do dialog
            DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_SETSERVERPORT),
                hWnd,
                (DLGPROC)SetServerPortDlgProc,
                (LPARAM)&::g_Config
                );
            }
            break;
            
        case ID_SCRIPTS_ACTIVATESCRIPT:
            // do dialog
            DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_ACTIVATESCRIPT),
                hWnd,
                (DLGPROC)ActivateScriptDlgProc,
                (LPARAM)scripthost
                );
            break;
            
        case ID_SCRIPTS_STOPSCRIPT:
            // do dialog
            DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_STOPSCRIPT),
                hWnd,
                (DLGPROC)StopScriptDlgProc,
                (LPARAM)scripthost
                );
            break;
            
        case ID_SCRIPTS_RELOADSCRIPTS:
            // reload scripts
            scripthost->ReloadScripts();
            break;
            
        default:
            break;
        } // end switch(LOWORD(wParam))

    // done
    return;
} // end HandleCommand

void Central::Draw(HDC hFront)const
{
    // build string describing our status
    std::stringstream ss;
    DatabaseStatistics stats;
    database->GetDatabaseStatistics(stats);
    
    // make sharenet mini-server status string
    ss << "Sharenet mini-server status: " << sharenet->GetStatusString() << "\n";
    
    // make sharenet mini-server status string
    ss << "Scriptserver mini-server status: " << scriptserver->GetStatusString() << "\n";
    
    if(sniffer->IsRunning())
        {
        ss << "Sniffer is running\n";
        }
    else
        {
        ss << "Sniffer is NOT running, check log file for details\n";
        }
    
    // make sniffer connections string
    ss << unsigned int(sniffer->GetNumConnections()) << " Sniffer Connections\n";
    
    sniffer->PrintConnections(ss);
    
    // make statistics string
    ss << stats.GetNumAlbs() << " Albs\n"
       << stats.GetNumHibs() << " Hibs\n"
       << stats.GetNumMids() << " Mids\n"
       << stats.GetNumMobs() << " MOBs\n";
       
    // make running scripts string       
    std::list<std::string> running_scripts;
    std::list<std::string>::const_iterator it;
    scripthost->GetRunningScripts(running_scripts);
    
    ss << unsigned int(running_scripts.size()) << " Running Scripts\n";
    for(it=running_scripts.begin();it!=running_scripts.end();++it)
        {
        ss << *it << "\n";
        } // end for all running scripts

    // get drawing rect
    RECT r;
    GetClientRect(hMainWnd,&r);
    
    // double buffer
    HDC hBack=CreateCompatibleDC(hFront);
    HBITMAP hBitmap=CreateCompatibleBitmap(hBack,r.right-r.left,r.bottom-r.top);
    
    // add drawing objects to back buffer
    HFONT hOldFont=(HFONT)SelectObject(hBack,hTahoma);
    HBITMAP hOldBitmap=(HBITMAP)SelectObject(hBack,hBitmap);

    // empty
    FillRect(hBack,&r,(HBRUSH)(COLOR_WINDOW+1));
    
    // offset a little bit to prevent
    // aligning the font pixels with the 
    // window border pixels
    r.left+=1;
    
    // draw
    DrawText
        (
        hBack,
        ss.str().c_str(),
        -1,
        &r,
        DT_WORDBREAK
        );

    // undo offset
    r.left-=1;
    
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
    
    // restore original drawing objects to back buffer
    SelectObject(hBack,hOldBitmap);
    SelectObject(hBack,hOldFont);

    // done with these
    DeleteObject(hBitmap);
    DeleteDC(hBack);
    
    // done
    return;
} // end Draw