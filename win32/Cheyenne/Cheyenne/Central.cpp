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
#pragma warning(disable : 4786)

#include "central.h"
#include "gl\glpng.h"
#include <sstream>
#include <memory>
#include <fstream>
#include "centralfunctors.h"
#include "centraldialogs.h"
#include "resource.h"

const int DataChildId=1;
const int PPIChildId=2;
const int RedrawTimerId=1;

const UINT INIT_GL_MSG=WM_USER+1;
const UINT CALL_INIT_DISPLAY_MATRICES=WM_USER+2;
const UINT RENDER_NOW=WM_USER+3;

Central::Central() :
    FontListBase(1000),NumFontLists(256),NumVectorMapLists(256),VectorMapListBase(0),CircleList(0),
    ProjectionX(600000.0f),ProjectionY(500000.0f),
    XLimit(2000000.0f),YLimit(2000000.0f),ProjectionWidthX(10000.0f),ProjectionWidthY(10000.0f),
    ZoomIncrement(5000.0f),PanIncrement(5000.0f)

{
    hMainWnd=NULL;
    hDataWnd=NULL;
    hPPIWnd=NULL;
    hInstance=NULL;
    hRenderContext=NULL;
    hPPIDC=NULL;
    bDisplayListsCreated=false;
    bTexturesCreated=false;
    ActorVertexX=ProjectionWidthX*0.009f;
    ActorVertexY=ProjectionWidthY*0.0135f;
    IDToFollow=0;
    HookedActor=0;
    FollowedActorHeadingDegrees=90.0f;
    hTahoma=NULL;
    hTahomaBig=NULL;
    ZeroMemory(&TahomaTextMetric,sizeof(TahomaTextMetric));

} // end Central

Central::~Central()
{
    // don't need the font anymore
    DeleteObject(hTahoma);
    DeleteObject(hTahomaBig);

    // stop
    sniffer.Stop();
    db.Stop();

    // delete everything on the message fifo
    while(MessageInputFifo.size() != 0)
        {
        delete MessageInputFifo.Pop();
        }

} // end ~Central

WPARAM Central::Go(HINSTANCE hInst)
{
    hInstance=hInst;

    if(!Init())
        {
        return(-1);
        }

    MSG msg;
    CheyenneTime LastRenderTime=Clock.Current();

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
            if((Clock.Current() - LastRenderTime).Seconds() >= 0.020)
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
    

    /*
    while(GetMessage(&msg,hMainWnd,0,0)==TRUE)
        {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
        }
    */

    return(msg.wParam);

} // end Go

void Central::OnMaintenanceUpdate(const Actor& ThisActor)
{
    if(ThisActor.GetInfoId() == IDToFollow)
        {
        {
        AutoLock al(CentralMutex);

        // recenter display on this actor
        Motion Pos;
        GetRenderPosition(ThisActor,Pos);

        // set followed actor heading
        FollowedActorHeadingDegrees=float(fmod(180.0f+ThisActor.GetMotion().GetHeading()*57.295779513082320876798154814105f,360.0f));

        // move camera to Pos
        ProjectionX=Pos.GetXPos()-(0.5f*ProjectionWidthX);
        ProjectionY=Pos.GetYPos()-(0.5f*ProjectionWidthY);
        }

        // reinit new matrices in gl thread
        PostMessage(hMainWnd,CALL_INIT_DISPLAY_MATRICES,0,0);
        }

    if(ThisActor.GetInfoId() == HookedActor)
        {
        // redraw data window
        InvalidateRect(hDataWnd,NULL,FALSE);
        }

    // done
    return;
} // end OnMaintenanceUpdate

void Central::OnNewActor(const Actor& ThisActor)
{
    if(IDToFollow == 0 && Config.GetAutoFollowFirstActor())
        {
        // set ID to follow
        IDToFollow=ThisActor.GetInfoId();
        }

    //Logger << "[Central::OnNewActor] Added " << ThisActor.GetName().c_str() << " to the list\n";

    if(ThisActor.GetRealm() == Actor::Albion && Config.GetPlaySoundOnAlbCreate())
        {
        // stop playing whatever is playing now
        PlaySound(NULL,NULL,SND_FILENAME|SND_ASYNC);

        // play the midgard create sound
        PlaySound(Config.GetAlbSoundFile().c_str(),NULL,SND_FILENAME|SND_ASYNC);
        }
    else if(ThisActor.GetRealm() == Actor::Hibernia && Config.GetPlaySoundOnHibCreate())
        {
        // stop playing whatever is playing now
        PlaySound(NULL,NULL,SND_FILENAME|SND_ASYNC);

        // play the midgard create sound
        PlaySound(Config.GetHibSoundFile().c_str(),NULL,SND_FILENAME|SND_ASYNC);
        }
    else if(ThisActor.GetRealm() == Actor::Midgard && Config.GetPlaySoundOnMidCreate())
        {
        // stop playing whatever is playing now
        PlaySound(NULL,NULL,SND_FILENAME|SND_ASYNC);

        // play the midgard create sound
        PlaySound(Config.GetMidSoundFile().c_str(),NULL,SND_FILENAME|SND_ASYNC);
        }


    // done
    return;
} // end OnNewActor

void Central::OnDeleteActor(const Actor& ThisActor)
{
    if(ThisActor.GetInfoId() == IDToFollow)
        {
        // unassign
        IDToFollow=0;

        // reset
        FollowedActorHeadingDegrees=0.0f;

        // redraw
        InvalidateRect(hDataWnd,NULL,TRUE);
        }

    //Logger << "[Central::OnDeleteActor] Deleted " << ThisActor.GetName().c_str() << " from the list\n";

    // done
    return;
} // end OnDeleteActor

void Central::OnReassignActor(const Actor& ThisActor)
{
    // remove then reinsert
    OnDeleteActor(ThisActor);
    OnNewActor(ThisActor);

    // done
    return;
} // end OnReassignActor

void Central::OnMaintenanceIntervalDone(void)
{
    // get statistics
    GetDatabase().GetDatabaseStatistics(stats);

    // done
    return;
} // end OnMaintenanceIntervalDone

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
    wc.lpszClassName="CheyenneMainClass";
    wc.lpszMenuName=MAKEINTRESOURCE(IDR_MAIN_MENU);
    wc.style=CS_DBLCLKS|CS_OWNDC;

    if(!RegisterClass(&wc))
        {
        return(false);
        }

    wc.hbrBackground=(HBRUSH)(COLOR_WINDOW+1);
    wc.hCursor=LoadCursor(NULL,IDC_ARROW);
    wc.hIcon=NULL;
    wc.hInstance=hInstance;
    wc.lpfnWndProc=Central::PPIWindowProc;
    wc.lpszClassName="CheyennePPIClass";
    wc.lpszMenuName=NULL;
    wc.style=CS_DBLCLKS|CS_OWNDC; // opengl window needs its own DC

    if(!RegisterClass(&wc))
        {
        return(false);
        }

    wc.hbrBackground=(HBRUSH)(COLOR_WINDOW+1);
    wc.hCursor=LoadCursor(NULL,IDC_ARROW);
    wc.hIcon=NULL;
    wc.hInstance=hInstance;
    wc.lpfnWndProc=Central::DataWindowProc;
    wc.lpszClassName="CheyenneDataClass";
    wc.lpszMenuName=NULL;
    wc.style=CS_DBLCLKS|CS_OWNDC; // opengl window needs its own DC

    if(!RegisterClass(&wc))
        {
        return(false);
        }

    // create window
    hMainWnd=CreateWindowEx
        (
        0,
        "CheyenneMainClass",
        "Cheyenne",
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

    RECT r;
    GetWindowRect(hMainWnd,&r);
    
    long w=r.right-r.left;
    long h=r.bottom-r.top;

    // create window
    hDataWnd=CreateWindowEx
        (
        WS_EX_WINDOWEDGE,
        "CheyenneDataClass",
        "",
        WS_CHILD|WS_BORDER|WS_VISIBLE,
        0,
        0,
        w*1/4,
        h,
        hMainWnd,
        (HMENU)DataChildId,
        hInstance,
        LPVOID(this)
        );

    if(!hDataWnd || hDataWnd==INVALID_HANDLE_VALUE)
        {
        return(false);
        }

    ShowWindow(hDataWnd,SW_SHOW);
    UpdateWindow(hDataWnd);

    // create window
    hPPIWnd=CreateWindowEx
        (
        WS_EX_WINDOWEDGE,
        "CheyennePPIClass",
        "",
        WS_CHILD|WS_VISIBLE|WS_BORDER,
        w*1/4,
        0,
        w*3/4,
        h,
        hMainWnd,
        (HMENU)PPIChildId,
        hInstance,
        LPVOID(this)
        );

    if(!hPPIWnd || hPPIWnd==INVALID_HANDLE_VALUE)
        {
        return(false);
        }

    ShowWindow(hPPIWnd,SW_SHOW);
    UpdateWindow(hPPIWnd);

    MoveWindow(hMainWnd,0,0,800,600,TRUE);

    return(true);
} // end Init

LRESULT CALLBACK Central::DataWindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    // get pointer to "me"
    Central* pMe=(Central*)(GetWindowLong(hWnd,GWL_USERDATA));

    switch(uMsg)
        {
        case WM_CREATE:
            {
            CREATESTRUCT* pcs=(CREATESTRUCT*)(lParam);

            SetWindowLong(hWnd,GWL_USERDATA,LONG(pcs->lpCreateParams));
            }
            break;

        case WM_PAINT:
            {
            PAINTSTRUCT ps;
            BeginPaint(hWnd,&ps);
            if(pMe)
                {
                pMe->DrawDataWindow(ps.hdc);
                }
            EndPaint(hWnd,&ps);
            }
            break;

        default:
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
        } // end switch message
    return(0);
} // end DataWindowProc

void Central::DrawDataWindow(HDC hFront)const
{
    // empty the data area

    RECT rClient;
    GetClientRect(hDataWnd,&rClient);

    FillRect(hFront,&rClient,(HBRUSH)(COLOR_WINDOW+1));

    if(HookedActor == 0)
        {
        // 0 ID is not really an actor
        return;
        }

    Actor Hooked=GetDatabase().CopyActorById(HookedActor);

    // we want to draw:
    /*
    Name Surname
    Guild
    Realm=get realm name
    Level=%level%
    Health=%health&
    Endurance=%endurance%
    Mana=%mana%
    Zone=get zone name
    Loc=%<x,y,z>%  <-- this shoud be in zone relative coords
    Heading=heading degrees
    Speed=%speed%
    Valid Time=%valid time%
    Last Update=%last update%
    */
    
    // get realm name
    std::string RealmName;

    switch(Hooked.GetRealm())
        {
        case Actor::Albion:
            RealmName="Albion";
            break;
        case Actor::Hibernia:
            RealmName="Hibernia";
            break;
        case Actor::Midgard:
            RealmName="Midgard";
            break;
        default:
            RealmName="MOB";
            break;
        }

    // get zone name and coordinates
    unsigned int x,y;
    unsigned short z;
    unsigned char zone;
    const MapInfo::ZoneInfo& HookedZone=Zones.GetZoneFromGlobal
        (
        Hooked.GetRegion(),
        unsigned int(Hooked.GetMotion().GetXPos()),
        unsigned int(Hooked.GetMotion().GetYPos()),
        unsigned int(Hooked.GetMotion().GetZPos()),
        x,
        y,
        z,
        zone
        );

    std::ostringstream oss;
    oss << Hooked.GetName().c_str() << " " << Hooked.GetSurname().c_str() << "\n"
        << "Guild=" << Hooked.GetGuild().c_str() << "\n"
        << RealmName.c_str() << "\n"
        << "Level=" << unsigned int(Hooked.GetLevel()) << "\n"
        << "Health=" << unsigned int(Hooked.GetHealth()) << "%\n"
        << "Endurance=" << "%\n"
        << "Mana=" << "%\n"
        << "Zone=" << HookedZone.ZoneFile.c_str() << "\n"
        << "Loc=<" << x << "," << y << "," << z << ">\n"
        << "Heading=" << Hooked.GetMotion().GetHeading()*57.295779513082320876798154814105f << "°\n"
        << "Speed=" << unsigned int(Hooked.GetMotion().GetSpeed()) << "\n"
        << "Valid Time=" << Hooked.GetMotion().GetValidTime().Seconds() << "\n"
        << "Last Update=" << Hooked.GetLastUpdateTime().Seconds() << "\n"
        << "Current Time=" << Clock.Current().Seconds() << "\n"
        << "Albs=" << stats.GetNumAlbs() << "\n"
        << "Hibs=" << stats.GetNumHibs() << "\n"
        << "Mids=" << stats.GetNumMids() << "\n"
        << "Mobs=" << stats.GetNumMobs() << "\n"
        << "InfoId Mappings=" << stats.GetInfoIdSize() << "\n";

    // inflate a little bit
    rClient.left+=1;

    // set font
    HGDIOBJ hOldObj=SelectObject(hFront,hTahomaBig);

    DrawText
        (
        hFront,
        oss.str().c_str(),
        oss.str().length(),
        &rClient,
        DT_LEFT|DT_TOP|DT_WORDBREAK
        );

    // restore font
    SelectObject(hFront,hOldObj);

    // done
    return;
} // end DrawDataWindow

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

        case INIT_GL_MSG:
            {
            if(!pMe->hTahoma)
                {
                // create font -- this MUST be
                // done before the display lists are initialized
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
                }

            if(!pMe->hTahomaBig)
                {
                // create font -- this MUST be
                // done before the display lists are initialized
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

                pMe->hTahomaBig=CreateFontIndirect(&lf);
                }

            // init pixel format
            pMe->InitPixelFormat();
            // init OpenGL
            pMe->InitOpenGL();
            // init actor events
            pMe->InitActorEvents();
            // init the rest of cheyenne
            pMe->InitCheyenne();

            // get text metrics for tahoma
            HDC hDC=GetDC(hWnd);
            HGDIOBJ hOldObj=SelectObject(hDC,pMe->hTahoma);
            GetTextMetrics(hDC,&pMe->TahomaTextMetric);
            SelectObject(hDC,hOldObj);
            ReleaseDC(hWnd,hDC);

            // set redraw timer
            //SetTimer(hWnd,RedrawTimerId,50,NULL);
            }
            break;

        case CALL_INIT_DISPLAY_MATRICES:
            pMe->InitDisplayMatrices();
            break;

        case WM_LBUTTONDOWN:
            SetFocus(hWnd);
            break;

        case WM_DESTROY:
            // kill timer
            //KillTimer(hWnd,RedrawTimerId);
            PostQuitMessage(0);
            break;

        case WM_TIMER:
        case RENDER_NOW:
            // redraw
            pMe->DrawPPI();
            break;

        case WM_KEYDOWN:
            pMe->HandleKeyDown(hWnd,wParam,lParam);
            break;

        case WM_MOUSEWHEEL:
            {
            short delta=GET_WHEEL_DELTA_WPARAM(wParam)/WHEEL_DELTA;
            short cnt;

            if(delta>0)
                {
                for(cnt=0;cnt<delta;++cnt)
                    {
                    pMe->ZoomIn();
                    }
                }
            else
                {
                for(cnt=0;cnt>delta;--cnt)
                    {
                    pMe->ZoomOut();
                    }
                }
            }
            break;

        case WM_COMMAND:
            pMe->HandleCommand(hWnd,uMsg,wParam,lParam);
            break;

        default:
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
        } // end switch message
    return(0);
} // end WindowProc

LRESULT CALLBACK Central::PPIWindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    // get pointer to "me"
    Central* pMe=(Central*)(GetWindowLong(hWnd,GWL_USERDATA));

    switch(uMsg)
        {
        case WM_CREATE:
            {
            CREATESTRUCT* pcs=(CREATESTRUCT*)(lParam);

            SetWindowLong(hWnd,GWL_USERDATA,LONG(pcs->lpCreateParams));
            // get pointer to "me"
            pMe=(Central*)(GetWindowLong(hWnd,GWL_USERDATA));

            // post message to initialize opengl
            PostMessage(pMe->hMainWnd,INIT_GL_MSG,0,0);
            }
            break;

        case WM_PAINT:
            {
            PAINTSTRUCT ps;
            BeginPaint(hWnd,&ps);
            pMe->DrawPPI();
            EndPaint(hWnd,&ps);
            }
            break;

        case WM_SIZE:
            {
            if(pMe->hRenderContext)
                {
                // reinitialize the viewport

                RECT r;
                GetClientRect(hWnd,&r);
                glViewport
                    (
                    0,
                    0,
                    r.right-r.left,
                    r.bottom-r.top
                    );

                pMe->InitDisplayMatrices();
                }
            }
            break;

        case WM_DESTROY:
            {
            // delete the render context
            if(pMe->hRenderContext)
                {
                wglDeleteContext(pMe->hRenderContext);
                pMe->hRenderContext=NULL;
                }

            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
            }
            break;

        case WM_LBUTTONDOWN:
            SetFocus(hWnd);
            break;

        case WM_KEYDOWN:
            pMe->HandleKeyDown(hWnd,wParam,lParam);
            break;

        case WM_MOUSEWHEEL:
            {
            short delta=GET_WHEEL_DELTA_WPARAM(wParam)/WHEEL_DELTA;
            short cnt;

            if(delta>0)
                {
                for(cnt=0;cnt<delta;++cnt)
                    {
                    pMe->ZoomIn();
                    }
                }
            else
                {
                for(cnt=0;cnt>delta;--cnt)
                    {
                    pMe->ZoomOut();
                    }
                }
            }
            break;

        case WM_LBUTTONDBLCLK:
            pMe->HandleDoubleClick(float(LOWORD(lParam)),float(HIWORD(lParam)));
            break;

        default:
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
        } // end switch message
    return(0);
} // end PPIWindowProc

void Central::HandleDoubleClick(float x,float y)
{
    RECT r;
    GetWindowRect(hPPIWnd,&r);

    float xpct=x/(r.right-r.left);
    float ypct=y/(r.bottom-r.top);

    AutoLock al(CentralMutex);

    float threshold=0.03f*(0.5f*(ProjectionWidthX + ProjectionWidthY));
    //Logger << "[Central::HandleDoubleClick] <" << x << "," << y << ">\n";

    x=ProjectionX+ProjectionWidthX*xpct;
    y=ProjectionY+ProjectionWidthY*ypct;

    // find closest actor in x,y
    ClosestActorFinder closest=GetDatabase().IterateActors(ClosestActorFinder(*this,x,y));

    // see if its close enough
    if(closest.GetDist() < threshold)
        {
        // save hooked actor to follow
        HookedActor=closest.GetId();
        }
    else
        {
        // unhook
        HookedActor=0;
        }

    // redraw
    InvalidateRect(hDataWnd,NULL,TRUE);

    // done
    return;
} // end HandleDoubleClick

void Central::HandleCommand(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    switch(LOWORD(wParam))
        {
        case ID_SYSTEM_EXIT:
            // bye!
            DestroyWindow(hMainWnd);
            break;

        case ID_DISPLAY_CAMERA_FOLLOW:
            // set camera follow
            {
            std::pair<Database*,unsigned int> param=std::make_pair(&db,0);

            DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_SET_CAMERA_FOLLOW),
                hWnd,
                (DLGPROC)CameraFollowDlgProc,
                (LPARAM)&param
                );

            // save
            IDToFollow=param.second;
            }
            break;

        case ID_DISPLAY_CONFIG:
            {
            // do dialog
            DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_DISPLAYCONFIG),
                hWnd,
                (DLGPROC)ConfigDisplayDlgProc,
                (LPARAM)&::Config
                );
            }            
            break;

        case ID_SYSTEM_CONFIG:
            {
            // do dialog
            DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_SETCONFIG),
                hWnd,
                (DLGPROC)ConfigDlgProc,
                (LPARAM)&::Config
                );
            }
            break;

        case ID_DISPLAY_RANGERINGS:
            // do dialog
            DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_SETRANGERINGS),
                hWnd,
                (DLGPROC)ConfigRangeRingsDlgProc,
                (LPARAM)&Config
                );
            break;

        case ID_HELP_ABOUT:
            // do dialog
            DialogBox
                (
                hInstance,
                MAKEINTRESOURCE(IDD_ABOUT),
                hWnd,
                (DLGPROC)AboutDlgProc,
                );
            break;

        case ID_DISPLAY_CONFIGURESOUNDS:
            // do dialog
            DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_SETSOUNDS),
                hWnd,
                (DLGPROC)SetSoundsDlgProc,
                (LPARAM)&Config
                );
            break;

        default:
            break;
        }

    // done
    return;
} // end HandleCommand

void Central::HandleKeyDown(HWND hWnd,WPARAM wParam,LPARAM lParam)
{
    switch(wParam)
        {
        case VK_ADD:
            // zoom in
            ZoomIn();
            break;

        case VK_SUBTRACT:
            // zoom out
            ZoomOut();
            break;

        case VK_LEFT:
            PanLeft();
            break;
        
        case VK_RIGHT:
            PanRight();
            break;
        
        case VK_UP:
            PanUp();
            break;
        
        case VK_DOWN:
            PanDown();
            break;

        case ' ':
            // recenter display
            RecenterDisplay();
            break;

        default:
            break;
        }
} // end HandleKeyDown

void Central::HandleMainWindowSizing(RECT* r)
{
    // minimum width is 320
    if((r->right - r->left) < 320)
        {
        r->right = r->left+320;
        }

    // minimum height is 200
    if((r->bottom - r->top) < 200)
        {
        r->bottom = r->top+200;
        }

    // done
    return;
} // end HandleMainWindowSizing

void Central::HandleMainWindowSize(void)
{
    // size the children
    EnumChildWindows(hMainWnd,Central::SizeChildProc,reinterpret_cast<LPARAM>(this));

    // done
    return;
} // end HandleMainWindowSize

BOOL CALLBACK Central::SizeChildProc(HWND hChild,LPARAM lParam)
{
    Central* pMe=reinterpret_cast<Central*>(lParam);

    // get client area
    RECT rClient;
    GetClientRect(pMe->hMainWnd,&rClient);

    long w=rClient.right-rClient.left;
    long h=rClient.bottom-rClient.top;

    switch(GetWindowLong(hChild,GWL_ID))
        {
        case DataChildId:
            MoveWindow(hChild,0,0,200,h,TRUE);
            break;

        case PPIChildId:
            MoveWindow(hChild,201,0,w-201,h,TRUE);
            break;

        default:
            // ?
            break;
        }

    // done
    return(TRUE);
} // end SizeChildProc

void Central::SetCamera(void)const
{
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    // set initial eye point
    gluLookAt
        (
        0,                        // eyex
        0,                        // eyey
        3,  // eyez (we make this a reasonable value to ease rounding error)
        0,                        // centerx
        0,                        // centery
        0.0,                            // centerz
        0.0,                            // upx
        1.0,                            // upy
        0.0                             // upz
        );

    return;
}

void Central::InitDisplayMatrices(void)const
{
    AutoLock al(CentralMutex);

    // change to projection matrix, and clear it
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    // make orthographic
    // this is WIERD because <0,0> is in the upper-left corner in DAoC
    glOrtho
        (
        ProjectionX, // left
        ProjectionX+ProjectionWidthX, // right
        ProjectionY+ProjectionWidthY, // top
        ProjectionY, // botton
        1.0f, // near
        5.0f // far
        );
    
    // set the camera too
    SetCamera();

    return;
}

void Central::RecenterDisplay(void)
{
    AutoLock al(CentralMutex);

    ProjectionX=0.0f;
    ProjectionY=0.0f;
    ProjectionWidthX=5000.0f;
    ProjectionWidthY=5000.0f;
    ActorVertexX=ProjectionWidthX*0.009f;
    ActorVertexY=ProjectionWidthY*0.0135f;
    InitDisplayMatrices();
    return;
}

void Central::ZoomIn(void)
{
    AutoLock al(CentralMutex);

    if(ProjectionWidthX-ZoomIncrement < 100.0f)
        {
        return;
        }
    
    if(ProjectionWidthY-ZoomIncrement < 100.0f)
        {
        return;
        }

    ProjectionWidthX-=ZoomIncrement;
    ProjectionWidthY-=ZoomIncrement;

    ProjectionX+=ZoomIncrement*0.5f;
    ProjectionY+=ZoomIncrement*0.5f;

    ActorVertexX=ProjectionWidthX*0.009f;
    ActorVertexY=ProjectionWidthY*0.0135f;

    // init display matrices
    InitDisplayMatrices();

    // recalculate increments
    RecalcIncrements();

    // done
    return;
} // end ZoomIn

void Central::ZoomOut(void)
{
    AutoLock al(CentralMutex);

    if(ProjectionWidthX > XLimit)
        {
        return;
        }
    
    if(ProjectionWidthY > YLimit)
        {
        return;
        }
    

    ProjectionWidthX+=ZoomIncrement;
    ProjectionWidthY+=ZoomIncrement;
    ProjectionX-=ZoomIncrement*0.5f;
    ProjectionY-=ZoomIncrement*0.5f;

    ActorVertexX=ProjectionWidthX*0.009f;
    ActorVertexY=ProjectionWidthY*0.0135f;

    // init display matrices
    InitDisplayMatrices();

    // recalculate increments
    RecalcIncrements();

    // done
    return;
} // end ZoomOut

void Central::PanRight(void)
{
    AutoLock al(CentralMutex);

    ProjectionX+=PanIncrement;

    if(ProjectionX > XLimit)
        {
        ProjectionX=XLimit;
        }
    
    InitDisplayMatrices();
} // end PanRight

void Central::PanLeft(void)
{
    AutoLock al(CentralMutex);

    ProjectionX-=PanIncrement;

    if(ProjectionX < -XLimit)
        {
        ProjectionX=-XLimit;
        }
    
    InitDisplayMatrices();
} // end PanLeft

// UP AND DOWN ARE BACKWARDS INTENTIONALLY
// this is WIERD because <0,0> is in the upper-left corner in DAoC
void Central::PanDown(void)
{
    AutoLock al(CentralMutex);

    ProjectionY+=PanIncrement;

    if(ProjectionY > YLimit)
        {
        ProjectionY=YLimit;
        }
    
    InitDisplayMatrices();
} // end PanDown

void Central::PanUp(void)
{
    AutoLock al(CentralMutex);

    ProjectionY-=PanIncrement;

    if(ProjectionY < -YLimit)
        {
        Logger << "[Central::PanUp] ProjectionY=" << ProjectionY << " limit=" << YLimit << "\n";
        ProjectionY=-YLimit;
        }
    
    InitDisplayMatrices();
} // end PanUp

void Central::InitPixelFormat(void)
{
    // this is a private DC so we don't have to release it
    hPPIDC=GetDC(hPPIWnd);

    // set pixel format
    // get the current pixel format index 
    // obtain a detailed description of that pixel format 
    PIXELFORMATDESCRIPTOR pfd;
    ZeroMemory(&pfd,sizeof(pfd));
    pfd.nSize=sizeof(pfd);
    pfd.nVersion=1;
    
    int iPF=GetPixelFormat(hPPIDC);
    iPF=GetLastError();

    DescribePixelFormat(hPPIDC,iPF,sizeof(PIXELFORMATDESCRIPTOR),&pfd);

    pfd.dwFlags |= PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_GENERIC_ACCELERATED|PFD_DOUBLEBUFFER;
    pfd.iPixelType=PFD_TYPE_RGBA;
    pfd.cColorBits=16; // 16-bit color
    pfd.cAlphaBits=8;
    pfd.cDepthBits=16;
    pfd.iLayerType=PFD_MAIN_PLANE;

    iPF=ChoosePixelFormat(hPPIDC,&pfd);

    SetPixelFormat(hPPIDC,iPF,&pfd);

    // done
    return;
}// end InitPixelFormat

void Central::InitCheyenne(void)
{
    // start database
    GetDatabase().Go(&MessageInputFifo);

    // start sniffer
    sniffer.Go(&MessageInputFifo);
    
    // done
    return;
} // end InitCheyenne

void Central::InitActorEvents(void)
{
    std::auto_ptr<DatabaseFunctor> maint(new MaintenanceUpdateFunctor(*this));
    std::auto_ptr<DatabaseFunctor> new_actor(new NewActorFunctor(*this));
    std::auto_ptr<DatabaseFunctor> delete_actor(new DeleteActorFunctor(*this));
    std::auto_ptr<DatabaseFunctor> reassign_actor(new ReassignActorFunctor(*this));
    std::auto_ptr<DatabaseFunctor> maintenance_done(new MaintenanceIntervalDoneFunctor(*this));

    db.InstallFunctor(Database::DatabaseEvents::MaintenanceUpdate,maint);
    db.InstallFunctor(Database::DatabaseEvents::ActorCreated,new_actor);
    db.InstallFunctor(Database::DatabaseEvents::ActorDeleted,delete_actor);
    db.InstallFunctor(Database::DatabaseEvents::ActorReassigned,reassign_actor);
    db.InstallFunctor(Database::DatabaseEvents::MaintenanceIntervalDone,maintenance_done);

    // done
    return;
} // end InitActorEvents

void Central::InitOpenGL(void)
{
    // see if we are being reinitialized -- 
    // if so, then we need to destroy the display lists and textures
    if(bDisplayListsCreated)
        {
        DestroyDisplayLists();
        }

    if(bTexturesCreated)
        {
        DestroyTextures();
        }

    // set new viewport
    wglMakeCurrent(hPPIDC,NULL);

    wglDeleteContext(hRenderContext);

    // create a rendering context
    hRenderContext=wglCreateContext(hPPIDC);

    // make it current
    wglMakeCurrent(hPPIDC,hRenderContext);

    // clear color is dark blue
    glClearColor(0.0f,0.0f,0.5f,1.0f);
    //glClearColor(1.0f,1.0f,1.0f,1.0f);

    // clear all buffers
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_ACCUM_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);

    // select buffer
    glDrawBuffer(GL_BACK);

    // cull
    glCullFace(GL_BACK);
    // this is WIERD because <0,0> is in the upper-left corner in DAoC
    //glFrontFace(GL_CCW);
    glFrontFace(GL_CW);

    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);
    //glDisable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
    glPolygonMode(GL_FRONT,GL_FILL);

    // initialize the display lists
    InitDisplayLists();

    // initialize the textures
    InitTextures();

    // init display matrices
    InitDisplayMatrices();

    // force a redraw
    InvalidateRect(hPPIWnd,NULL,TRUE);

    char* Vendor=(char*)glGetString(GL_VENDOR);
    char* Renderer=(char*)glGetString(GL_RENDERER);
    char* Version=(char*)glGetString(GL_VERSION);
    char* Extensions=(char*)glGetString(GL_EXTENSIONS);

    if(Vendor==NULL)
        {
        Vendor="(null)";
        }
    if(Renderer==NULL)
        {
        Renderer="(null)";
        }
    if(Version==NULL)
        {
        Version="(null)";
        }
    if(Extensions==NULL)
        {
        Extensions="(null)";
        }
    
    Logger << "[Central::InitOpenGL] OpenGL info:\n"
           << "GL_VENDOR=" << Vendor << "\n"
           << "GL_RENDERER=" << Renderer << "\n"
           << "GL_VERSION=" << Version << "\n"
           << "GL_EXTENSIONS=" << Extensions << "\n";

    // done
    return;
} // end InitOpenGL

void Central::InitDisplayLists(void)
{
    Logger << "Initializing display lists\n";

    // select the font
    HGDIOBJ hOldObject=SelectObject(hPPIDC,hTahoma);

    // generate the character display lists
    wglUseFontBitmaps (hPPIDC,0,NumFontLists,FontListBase);

    // put old font back in
    SelectObject(hPPIDC,hOldObject);

    // generate vector map display lists
    VectorMapListBase=glGenLists(NumVectorMapLists);
    // this must be done AFTER the fonts are loaded because it uses them
    LoadVectorMaps();

    // make circle radius 100, 36 slices (every 10°)
    CircleList=glGenLists(1);
    const float slice=10.0f*0.017453292519943295769236907684886f; // 10° in radians
    
    glNewList(CircleList,GL_COMPILE);
    glColor4f(1.0f,1.0f,1.0f,1.0f);
    glBegin(GL_LINE_LOOP);
    for(int i=0;i<36;++i)
        {
        glVertex3d
            (
            100.0*cos(float(i)*slice),
            100.0*sin(float(i)*slice),
            0.0
            );
        }

    glEnd();
    glEndList();

    // flag
    bDisplayListsCreated=true;

    // done
    return;
} // end InitDisplayLists

void Central::DestroyDisplayLists(void)
{
    Logger << "Cleaning up display lists\n";
    
    // destroy the font display lists
    glDeleteLists(FontListBase,NumFontLists);

    // destroy the vector map display lists
    glDeleteLists(VectorMapListBase,NumVectorMapLists);

    // destroy the circle list
    glDeleteLists(CircleList,1);

    bDisplayListsCreated=false;
    // done
    return;
} // end DestroyDisplayLists

void Central::LoadVectorMaps(void)
{
    return;
    std::fstream map_file;

    for(unsigned char map_cnt=0;map_cnt<=254;++map_cnt)
        {
        // check if this zone is valid
        if(Zones.GetZone(map_cnt).bValid)
            {
            // open the .map file
            map_file.open(Zones.GetZone(map_cnt).ZoneFile.c_str(),std::ios::in);

            // make sure it opened
            if(!map_file.is_open())
                {
                // go to next one
                continue;
                } // end if open
            
            while(!map_file.eof() && map_file.good())
                {
                } // end while there is data left in the file

            // close this file
            map_file.close();
            } // end if zone is valid
        } // end for all zones

    // done
    return;
} // end LoadVectorMaps

void Central::InitTextures(void)
{
    Logger << "Initializing textures\n";

    std::ostringstream oss;

    // go through each zone and load a texture for it
    for(unsigned int zone=0;zone<256;++zone)
        {
        // empty it
        oss.str("");

        // put filename in
        oss << "maps\\zone";
        oss.width(3);
        oss.fill('0');
        oss << zone << ".png";

        unsigned int id=pngBind(oss.str().c_str(),PNG_BUILDMIPMAPS,PNG_SOLID,NULL,GL_CLAMP,GL_LINEAR_MIPMAP_NEAREST,GL_LINEAR_MIPMAP_NEAREST);

        if(id != 0)
            {
            Logger << "[Central::InitTextures] loaded texture \"" << oss.str().c_str() << "\""
                   << " with id " << id << "\n";

            TextureMap.insert(TextureMapValueType(zone,id));
            }
        else
            {
            //Logger << "[Central::InitTextures] failed to load texture \"" << oss.str().c_str() << "\"\n";
            }
        }

    // flag
    bTexturesCreated=true;

    // done
    return;
} // end InitTextures

void Central::DestroyTextures(void)
{
    Logger << "Cleaning up textures\n";
    
    // cleanup all textures
    while(TextureMap.begin() != TextureMap.end())
        {
        TextureMapIteratorType it=TextureMap.begin();

        // cleanup texture
        glDeleteTextures(1,&(it->second));

        // erase it
        TextureMap.erase(it);
        }

    // flag
    bTexturesCreated=false;

    // done
    return;
} // end DestroyTextures

void Central::RenderActor(const Actor& ThisActor)const
{
    if((Config.GetDrawDeadActors() == false) && (ThisActor.GetHealth() == 0))
        {
        // dead actor and drawig dead actors is disabled
        return;
        }

    // push matrix stack
    glPushMatrix();

    if(
        ((ThisActor.GetActorType() == Actor::Player) && Config.GetRenderPlayers()) ||
        ((ThisActor.GetActorType() == Actor::Mob) && Config.GetRenderMOBs()) ||
        ((ThisActor.GetActorType() == Actor::Object) && Config.GetRenderObjects())
      )
        { 
        // set color by realm
        switch(ThisActor.GetRealm())
            {
            case Actor::Albion:
                glColor3f(0.75f,0.0f,0.0f);
                break;
            case Actor::Midgard:
                glColor3f(0.0f,0.0f,0.75f);
                break;
            case Actor::Hibernia:
                glColor3f(0.0f,0.75f,0.0f);
                break;
            default:
                glColor3f(0.0f,0.0f,0.0f);
                break;
            }

        // store position with display offsets
        Motion Position;
        GetRenderPosition(ThisActor,Position);

        glTranslatef(Position.GetXPos(),Position.GetYPos(),0.0f);

        if(Config.GetPPIText())
            {
            // draw text
            // name
            glRasterPos3f(0.0f,2.0f*ActorVertexY,0.0f);

            glListBase(FontListBase);

            //glCallLists(ThisActor.GetName().length(),GL_UNSIGNED_BYTE,ThisActor.GetName().c_str());
            DrawGLUTFontString(ThisActor.GetName());

            if(Config.GetSurnameInPPI())
                {
                // surname
                //glCallLists(1,GL_UNSIGNED_BYTE," ");
                DrawGLUTFontString(std::string(" "));
                //glCallLists(ThisActor.GetSurname().length(),GL_UNSIGNED_BYTE,ThisActor.GetSurname().c_str());
                DrawGLUTFontString(ThisActor.GetSurname());
                }

            if(Config.GetGuildInPPI())
                {
                // guild
                //glCallLists(1,GL_UNSIGNED_BYTE," ");
                DrawGLUTFontString(std::string(" "));
                //glCallLists(ThisActor.GetGuild().length(),GL_UNSIGNED_BYTE,ThisActor.GetGuild().c_str());
                DrawGLUTFontString(ThisActor.GetGuild());
                }

            if(Config.GetLevelInPPI())
                {
                // level
                char level[8];
                sprintf(level," %u",unsigned int(ThisActor.GetLevel()));
                //glCallLists(strlen(level),GL_UNSIGNED_BYTE,level);
                DrawGLUTFontString(std::string(level));
                }

            if(Config.GetHealthInPPI())
                {
                // health
                char health[8];
                sprintf(health," %u%%",unsigned int(ThisActor.GetHealth()));
                //glCallLists(strlen(health),GL_UNSIGNED_BYTE,health);
                DrawGLUTFontString(std::string(health));
                }
            } // end if draw text in PPI

        // rotate for heading
        glRotatef(180.0f+Position.GetHeading()*57.295779513082320876798154814105f,0.0f,0.0f,1.0f);
        //                                 convert to degrees for glRotatef

        // draw actor symbol
        glBegin(GL_TRIANGLES);
        glVertex3f(0.0f,ActorVertexY,0.0f);
        glVertex3f(-ActorVertexX,-ActorVertexY,0.0f);
        glVertex3f(ActorVertexX,-ActorVertexY,0.0f);
        glEnd();

        // if it's "hooked" draw a hook
        if(ThisActor.GetInfoId() == HookedActor)
            {
            // radius for hook is (ActorVertexX + ActorVertexY)/2 + 25;
            const float radius=25.0f+(0.5f*(ActorVertexX + ActorVertexY));

            // draw it
            glPushMatrix();

            // scale
            glScalef(radius*0.01f,radius*0.01f,radius*0.01f);
            // draw
            glCallList(CircleList);

            glPopMatrix();
            } // end if this is the hooked actor

        // if its the followed actor, draw range rings
        if(ThisActor.GetInfoId() == IDToFollow)
            {
            for(int i=0;i<6;++i)
                {
                if(Config.GetRangeRings().Rings[i].bEnabled)
                    {
                    // draw it
                    glPushMatrix();

                    // scale
                    glScalef(Config.GetRangeRings().Rings[i].Radius*0.01f,Config.GetRangeRings().Rings[i].Radius*0.01f,Config.GetRangeRings().Rings[i].Radius*0.01f);
                    // draw
                    glCallList(CircleList);

                    glPopMatrix();
                    }
                }
            } // end if this is the id to follow
        } // end if we are supposed to render this

    // pop matrix stack
    glPopMatrix();

    // done
    return;
} // end RenderActor

void Central::RenderWorld(void)const
{
    // push matrix stack
    glPushMatrix();

    // set the list base for font bitmaps
    glListBase(FontListBase);

    // render every zone
    for(unsigned char i=0;i<=254;++i)
        {
        if(Zones.GetZone(i).bValid && ::Zones.GetLimitsFromRegion(Zones.GetZone(i).Region).XOffset!=0)
            {
            // alias 
            const MapInfo::ZoneInfo& zone=Zones.GetZone(i);

            // get display coordinates
            // for this zone
            int BaseX,BaseY;
            int MaxX,MaxY;

            BaseX=int(zone.BaseX) + ::Zones.GetLimitsFromRegion(zone.Region).XOffset;
            BaseY=int(zone.BaseY) + ::Zones.GetLimitsFromRegion(zone.Region).YOffset;

            MaxX=int(zone.MaxX) + ::Zones.GetLimitsFromRegion(zone.Region).XOffset;
            MaxY=int(zone.MaxY) + ::Zones.GetLimitsFromRegion(zone.Region).YOffset;

            // check to make sure zone is visible on screen
            if(IsZoneVisible(BaseX,BaseY,MaxX,MaxY))
                {

                // enable textures
                glEnable(GL_TEXTURE_2D);

                // activate the proper texture
                TextureMapConstIteratorType it=TextureMap.find(i);

                if(it != TextureMap.end())
                    {
                    glBindTexture(GL_TEXTURE_2D,it->second);
                    }
                else
                    {
                    glBindTexture(GL_TEXTURE_2D,0);
                    }

                // draw this zone

                glBegin(GL_QUADS);

                glColor3f(0.5f,0.5f,0.5f);

                glTexCoord2i(0,1);
                glVertex3i(BaseX,MaxY,0);

                glTexCoord2i(0,0);
                glVertex3i(BaseX,BaseY,0);

                glTexCoord2i(1,0);
                glVertex3i(MaxX,BaseY,0);

                glTexCoord2i(1,1);
                glVertex3i(MaxX,MaxY,0);

                glEnd();
            
                glColor3f(0.0f,0.0f,0.0f);

                // disable textures for text
                glDisable(GL_TEXTURE_2D);

                // draw zone name text
                glRasterPos3i
                    (
                    BaseX,
                    BaseY,
                    1
                    );

                //glCallLists(zone.ZoneFile.length(),GL_UNSIGNED_BYTE,zone.ZoneFile.c_str());
                DrawGLUTFontString(zone.ZoneFile);

                // draw vector map with no z buffering
                glPushMatrix();
                glDisable(GL_DEPTH_TEST);
                glCallList(VectorMapListBase + unsigned int(i));
                glEnable(GL_DEPTH_TEST);
                glPopMatrix();
                } // end if zone is visible
            } // end if zone is valid
        } // end for each zone

    // pop matrix stack
    glPopMatrix();

    // done
    return;
} // end RenderWorld

void Central::DrawPPI(void)
{
    if(!hRenderContext)
        {
        // nothing to do
        return;
        }

    // clear all buffers
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

    // init matrices
    glLoadIdentity();
    
    // move camera back a bit
    glTranslatef(0.0f,0.0f,-3.0f);

    if(Config.GetMatchFollowedHeading())
        {
        // rotate for followed actors heading
        glRotatef(FollowedActorHeadingDegrees,0.0f,0.0f,-1.0f);
        }

    // draw the world geometry
    RenderWorld();

    // render all actors with no depth test so they all display
    // and no textures
    glDisable(GL_DEPTH_TEST);
    
    // see which database iteration function we are supposed to use
    if(Config.GetUpdateWhenRendered())
        {
        // update actors when they are rendered
        db.UpdateAndIterateActors(ActorRenderFunctor(*this));
        }
    else
        {
        // do not update actors when they are rendered
        db.IterateActors(ActorRenderFunctor(*this));
        }

    glEnable(GL_DEPTH_TEST);
    
    glPopMatrix();

    // finish and swap
    glFlush();
    glFinish();
    SwapBuffers(hPPIDC);
        
    // done
    return;
} // end DrawPPI


