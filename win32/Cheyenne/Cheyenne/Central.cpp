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

// init font list base statically
const int Central::FontListBase=1000;

void DrawWGLFontString(const std::string& text)
{
    glListBase(Central::FontListBase);
    glCallLists(text.size(),GL_UNSIGNED_BYTE,text.c_str());
}

Central::Central() :
    DataWindowTimerId(1),NumFontLists(256),NumVectorMapLists(256),VectorMapListBase(0),CircleList(0),
    ProjectionX(600000.0f),ProjectionY(500000.0f),
    XLimit(2000000.0f),YLimit(2000000.0f),ProjectionWidthX(10000.0f),ProjectionWidthY(10000.0f),
    ZoomIncrement(5000.0f),PanIncrement(5000.0f),
    IDToFollowZone(255),IDToFollowLevel(0),
    ActorXScale(0.01f),ActorYScale(0.0149f),
    //ActorXScale(0.0135f),ActorYScale(0.0135f),
    GroundTargetXScale(0.018f),GroundTargetYScale(0.018f),
    WindowPosFileName("windowpos.txt"),
    DiscList(0)
{
    hMainWnd=NULL;
    hDataWnd=NULL;
    hPPIWnd=NULL;
    hInstance=NULL;
    hRenderContext=NULL;
    hPPIDC=NULL;
    bDisplayListsCreated=false;
    bTexturesCreated=false;
    ActorVertexX=ProjectionWidthX*ActorXScale;
    ActorVertexY=ProjectionWidthY*ActorYScale;
    GroundTargetVertexX=ProjectionWidthX*GroundTargetXScale;
    GroundTargetVertexY=ProjectionWidthY*GroundTargetYScale;
    IDToFollow=0;
    HookedActor=0;
    FollowedActorHeadingDegrees=0.0f;
    hTahoma=NULL;
    hTahomaBig=NULL;
    LastActorConTexture=0;
    Frames=0;
    FrameMeasureStart=::Clock.Current();
    ZeroMemory(&TahomaTextMetric,sizeof(TahomaTextMetric));

} // end Central

Central::~Central()
{
    // stop
    VmLoader.Stop();
    sniffer.Stop();
    db.Stop();
    Sounds.Stop();
    ShareNet.Stop();

    // delete everything on the message fifo
    while(MessageInputFifo.size() != 0)
        {
        delete MessageInputFifo.Pop();
        }

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
            if((Clock.Current() - LastRenderTime).Seconds() >= 0.020) // <-- this sets FPS to a max of 50
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
        FollowedActorHeadingDegrees=float(fmod(ThisActor.GetMotion().GetHeading()*57.295779513082320876798154814105f,360.0f));

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
        // queue the sound to be played
        Sounds.QueueSound(SoundSpool::AlbCreate);
        }
    else if(ThisActor.GetRealm() == Actor::Hibernia && Config.GetPlaySoundOnHibCreate())
        {
        // queue the sound to be played
        Sounds.QueueSound(SoundSpool::HibCreate);
        }
    else if(ThisActor.GetRealm() == Actor::Midgard && Config.GetPlaySoundOnMidCreate())
        {
        // queue the sound to be played
        Sounds.QueueSound(SoundSpool::MidCreate);
        }
    else if(ThisActor.GetName() == Config.GetNamedMob() && Config.GetPlaySoundOnNamedMobCreate())
        {
        // queue sound to be played
        Sounds.QueueSound(SoundSpool::NamedMobCreate);
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
    
    if(ThisActor.GetInfoId() == HookedActor)
        {
        // unassign
        HookedActor=0;
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

void Central::OnSharenetMessage(const void* p,const unsigned int len)
{
    // send to sharenet
    ShareNet.QueueOutputMessage(p,len);
    
    // log
    //:: Logger << "[Central::OnSharenetMessage] sending " << len << " bytes\n";
    
    // done
    return;
} // end OnSharenetMessage

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

    // get initial window position
    RECT rMain;
    InitWindowPosition(rMain);

    // create window
    hMainWnd=CreateWindowEx
        (
        0,
        "CheyenneMainClass",
        "Cheyenne",
        WS_OVERLAPPEDWINDOW|WS_VISIBLE|WS_CLIPCHILDREN,
        rMain.left,
        rMain.top,
        rMain.right-rMain.left,
        rMain.bottom-rMain.top,
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
        WS_CHILD|WS_VISIBLE|WS_BORDER|WS_CLIPCHILDREN|WS_CLIPSIBLINGS,
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
    GetClientRect(hPPIWnd,&rPPIClient);
    
    MoveWindow(hMainWnd,rMain.left,rMain.top,800,600,TRUE);
    MoveWindow(hMainWnd,rMain.left,rMain.top,rMain.right-rMain.left,rMain.bottom-rMain.top,TRUE);

    return(true);
} // end Init

void Central::InitWindowPosition(RECT& r)const
{
    std::fstream pos_file;
    
    pos_file.open(WindowPosFileName.c_str(),std::ios_base::in);
    
    if(!pos_file.is_open())
        {
        // done
        r.left=0;
        r.top=0;
        r.right=800;
        r.bottom=600;
        return;
        }

    // load stored position
    int x,y,w,h;
    pos_file >> x >> y >> w >> h;
    
    Logger << "[Central::InitWindowPosition] stored coordinates are "  
        << x << " "
        << y << " "
        << w << " "
        << h << "\n";
    
    // get virtual display rect
    ZeroMemory(&r,sizeof(r));
    EnumDisplayMonitors(NULL,NULL,Central::MonitorEnumProc,(LPARAM)&r);
    
    Logger << "[Central::InitWindowPosition] virtual desktop is "  
           << r.right-r.left << "x"
           << r.bottom-r.top << "\n";
    
    // make sure stored position will fit
    if(x+w <= r.right-r.left && y+h <= r.bottom-r.top)
        {
        // stored position is valid, use it
        Logger << "[Central::InitWindowPosition] Cheyenne will be "  
            << w << "x"
            << h
            << " if this is incorrect, delete the windowpos.txt file\n";

        r.left=x;
        r.top=y;
        r.right=x+w;
        r.bottom=y+h;
        }
    else
        {
        Logger << "[Central::InitWindowPosition] (default) Cheyenne will be "  
            << 800 << "x"
            << 600
            << "\n";
        // stored position is no longer valid, set to default
        r.left=0;
        r.top=0;
        r.right=800;
        r.bottom=600;
        }
    
    // done
    return;
} // end InitWindowPosition

BOOL CALLBACK Central::MonitorEnumProc
    (
    HMONITOR hMonitor,
    HDC hdcMonitor,
    LPRECT lprcMonitor,
    LPARAM dwData
    )
{
    // get the rect pointer
    RECT* pr=(RECT*)dwData;
    
    // inflate by the display area of this monitor
    if(pr->left > lprcMonitor->left)
        {
        pr->left=lprcMonitor->left;
        }
    
    if(pr->top > lprcMonitor->top)
        {
        pr->top=lprcMonitor->top;
        }

    if(pr->right < lprcMonitor->right)
        {
        pr->right=lprcMonitor->right;
        }

    if(pr->bottom < lprcMonitor->bottom)
        {
        pr->bottom=lprcMonitor->bottom;
        }
    // done
    return(TRUE);
} // end MonitorEnumProc

void Central::StoreWindowPosition(void)const
{
    // get window rect
    RECT r;
    GetWindowRect(hMainWnd,&r);
    
    // save
    std::string pos_file_name(::InitialDir);
    pos_file_name+=WindowPosFileName;
    std::fstream pos_file(pos_file_name.c_str(),std::ios_base::out);
    
    pos_file << r.left << std::endl
             << r.top << std::endl
             << r.right-r.left << std::endl
             << r.bottom-r.top << std::endl;
    
    Logger << "[Central::StoreWindowPosition] Cheyenne will be "  
        << r.left << " "
        << r.top << " "
        << r.right-r.left << " "
        << r.bottom-r.top << "\n";

    // done
    return;
} // end StoreWindoePosition

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

        case WM_TIMER:
            {
            HDC hDC=GetDC(hWnd);
            if(pMe)
                {
                pMe->DrawDataWindow(hDC);
                }
            ReleaseDC(hWnd,hDC);
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
    std::ostringstream oss;
    std::string RealmName;
    CheyenneTime CurrentTime(::Clock.Current());

    if(HookedActor != 0)
        {
        Actor Hooked=GetDatabase().CopyActorById(HookedActor);

        // we want to draw:
        /*
        Name Surname
        Guild
        Realm=get realm name
        Level=%level%
        Health=%health&
        Zone=get zone name
        Loc=%<x,y,z>%  <-- this shoud be in zone-relative coords
        Heading=heading degrees
        Speed=%speed%
        Valid Time=%valid time%
        Last Update=%last update%
        */
    
        // get realm name
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

        oss << "Hooked Actor:\n"
            << Hooked.GetName().c_str() << " " << Hooked.GetSurname().c_str() << "\n"
            << "Guild=" << Hooked.GetGuild().c_str() << "\n"
            << RealmName.c_str() << "\n"
            << "Level=" << unsigned int(Hooked.GetLevel()) << "\n"
            << "Health=" << unsigned int(Hooked.GetHealth()) << "%\n"
            << "Zone=" << HookedZone.ZoneFile.c_str() << "\n"
            << "Loc=<" << x << "," << y << "," << z << ">\n"
            << "Heading=" << RadToDeg(Hooked.GetMotion().GetHeading()) << "°\n"
            << "Speed=" << Hooked.GetMotion().GetSpeed() << "\n"
            << "Valid Time=" << Hooked.GetMotion().GetValidTime().Seconds() << "\n"
            << "Last Update=" << Hooked.GetLastUpdateTime().Seconds() << "\n\n";
        }
    
    if(IDToFollow != 0)
        {
        // get followed actor
        Actor Followed=GetDatabase().CopyActorById(IDToFollow);
        // get zone name and coordinates
        unsigned int x,y;
        unsigned short z;
        unsigned char zone;
        const MapInfo::ZoneInfo& FollowedZone=Zones.GetZoneFromGlobal
            (
            Followed.GetRegion(),
            unsigned int(Followed.GetMotion().GetXPos()),
            unsigned int(Followed.GetMotion().GetYPos()),
            unsigned int(Followed.GetMotion().GetZPos()),
            x,
            y,
            z,
            zone
            );

        // save zone and level and realm
        IDToFollowZone=zone;
        IDToFollowLevel=Followed.GetLevel();
        IDToFollowRealm=Followed.GetRealm();

        // get realm name
        switch(Followed.GetRealm())
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
            case Actor::MOB:
            default:
                RealmName="MOB";
                break;
            }

        oss << "Followed Actor:\n"
            << Followed.GetName().c_str() << " " << Followed.GetSurname().c_str() << "\n"
            << "Guild=" << Followed.GetGuild().c_str() << "\n"
            << RealmName.c_str() << "\n"
            << "Level=" << unsigned int(Followed.GetLevel()) << "\n"
            << "Health=" << unsigned int(Followed.GetHealth()) << "%\n"
            << "Zone=" << FollowedZone.ZoneFile.c_str() << "\n"
            << "Loc=<" << x << "," << y << "," << z << ">\n"
            << "Heading=" << Followed.GetMotion().GetHeading()*57.295779513082320876798154814105f << "°\n"
            << "Speed=" << Followed.GetMotion().GetSpeed() << "\n"
            << "Valid Time=" << Followed.GetMotion().GetValidTime().Seconds() << "\n"
            << "Last Update=" << Followed.GetLastUpdateTime().Seconds() << "\n\n";
        }
    else
        {
        IDToFollowZone=255;
        }

    oss << "Global Database Statistics:\n"
        << "Current Time=" << CurrentTime.Seconds() << "\n"
        << "Frame Rate=" << (unsigned int)(Frames / (CurrentTime-FrameMeasureStart).Seconds()) << "\n"
        << "Albs=" << stats.GetNumAlbs() << "\n"
        << "Hibs=" << stats.GetNumHibs() << "\n"
        << "Mids=" << stats.GetNumMids() << "\n"
        << "Mobs=" << stats.GetNumMobs() << "\n"
        << "InfoId Mappings=" << stats.GetInfoIdSize() << "\n"
        << "ShareNet Status=" << ShareNet.GetStatusString() << "\n"
        << "ShareNet=" << ShareNet.GetRemoteAddr();

    // save frame measure start and reset frames to 0
    FrameMeasureStart=CurrentTime;
    Frames=0;
    
    // draw double buffered to prevent flickering
    HDC hBack=CreateCompatibleDC(hFront);
    HBITMAP hBmp=CreateCompatibleBitmap(hBack,rClient.right-rClient.left,rClient.bottom-rClient.top);
    
    // set bitmap
    HGDIOBJ hOldBmp=SelectObject(hBack,hBmp);

    // set font
    HGDIOBJ hOldFont=SelectObject(hBack,hTahomaBig);

    // fill with white
    FillRect(hBack,&rClient,(HBRUSH)(COLOR_WINDOW+1));

    // move text over just a little bit
    rClient.left += 1;

    // draw text in the back buffer
    DrawText
        (
        hBack,
        oss.str().c_str(),
        oss.str().length(),
        &rClient,
        DT_LEFT|DT_TOP|DT_WORDBREAK
        );

    // bitblt
    BitBlt
        (
        hFront,
        0,
        0,
        rClient.right-rClient.left,
        rClient.bottom-rClient.top,
        hBack,
        0,
        0,
        SRCCOPY
        );

    // restore original font
    SelectObject(hBack,hOldFont);

    // restore original bitmap
    SelectObject(hBack,hOldBmp);

    // delete bitmap
    DeleteObject(hBmp);

    // delete dc
    DeleteDC(hBack);

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

            // set data window timer
            SetTimer(pMe->hDataWnd,pMe->DataWindowTimerId,1000,NULL);
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
            KillTimer(pMe->hDataWnd,pMe->DataWindowTimerId);
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

        case WM_CLOSE:
            // store window position
            pMe->StoreWindowPosition();
            
            // let default handle it
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
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
                GetClientRect(hWnd,&pMe->rPPIClient);
                glViewport
                    (
                    0,
                    0,
                    pMe->rPPIClient.right-pMe->rPPIClient.left,
                    pMe->rPPIClient.bottom-pMe->rPPIClient.top
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
            // save window position
            StoreWindowPosition();
            
            // bye!
            DestroyWindow(hMainWnd);
            break;

        case ID_DISPLAY_CAMERA_FOLLOW:
            // set camera follow
            {
            std::pair<Database*,unsigned int> param=std::make_pair(&db,IDToFollow);

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
                DLGPROC(AboutDlgProc)
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
                (LPARAM)(&Config)
                );
            break;

        case ID_SHARENET_CONNECT:
            // do dialog
            {
            LRESULT res=DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_SHARENETCONFIG),
                hWnd,
                (DLGPROC)ConfigShareNetDlgProc,
                (LPARAM)(&Config)
                );
            
            if(res==IDOK)
                {
                // open sharenet
                OpenShareNet();
                }
            }
            break;
            
        case ID_SYSTEM_REINITIALIZEOPENGL:
            // reinitialize opengl
            InitOpenGL();
            break;
            
        case ID_MOBSIGHT_FINDAMOBTOHUNT:
            // find us a mob, Jeeves!
            
            // mobfinder1 stores its info in an array of 3 integers:
            // realm id, min level, and max level.
            // mobfinder2 stores its info in a single int.
            
            INT_PTR result;
            {
            // objects from the MobsightParse library
            MobsightContentHandler Content;
            MobsightIdHandler Id;

            // other locals
            int mf1_array[3];
            std::list<NarrowMobDef> CandidateList;
            std::pair<std::list<NarrowMobDef>*,NarrowIdDef*> Dlg2Param;
            NarrowIdDef IdDef;
            Dlg2Param.first=&CandidateList;
            Dlg2Param.second=&IdDef;
            
            // first dialog label
            do_find_dialog_1:
            
            // get realm, min and max level
            result=DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_MOBFINDER_1),
                hWnd,
                (DLGPROC)Mobfinder1DlgProc,
                (LPARAM)&mf1_array[0]
                );
                
            if(result==IDCANCEL)
                {
                // we're done
                break;
                }
            
            // ok, now we have realm, min and max level
            // get the candidate list
            
            // must convert to wide string
            std::wstring url;
            ::ToWString(::Config.GetMobsightURL(),url);
            Content.BuildMobList
                (
                url,
                MobsightContentHandler::REALM(mf1_array[0]),
                mf1_array[1],
                mf1_array[2],
                CandidateList
                );
            
            // get the user to select one from the list
            result=DialogBoxParam
                (
                hInstance,
                MAKEINTRESOURCE(IDD_MOBFINDER_2),
                hWnd,
                (DLGPROC)Mobfinder2DlgProc,
                (LPARAM)&Dlg2Param
                );
            
            // free this, we're done with it
            Content.FreeMobList(CandidateList);

            if(result==ID_BACK)
                {
                goto do_find_dialog_1;
                }
            
            // ok, we have the id of the mob that we want,
            // go get its definition (the dialog filled out IdDef.id)

            // must convert to wide string
            ::ToWString(::Config.GetMobsightURL(),url);

            Id.GetMobInfo
                (
                url,
                IdDef.id,
                IdDef
                );
            
            // store in Central storage
            MobfinderResults.loc_list.clear();
            MobfinderResults=IdDef;
            
            // free it, we're done
            Id.FreeMobInfo(IdDef);
            }
                
            break;
            
        default:
            break;
        } // end switch(LOWORD(wParam))

    // done
    return;
} // end HandleCommand

void Central::OpenShareNet(void)
{
    // make sure its closed
    ShareNet.Close();
    
    // open with new parameters
    ShareNet.Open(Config.GetShareNetAddress(),Config.GetShareNetPort());

    // tell database to request a full update
    db.RequestFullUpdate();
    
    // done
    return;
} // end OpenShareNet

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
    ActorVertexX=ProjectionWidthX*ActorXScale;
    ActorVertexY=ProjectionWidthY*ActorYScale;
    GroundTargetVertexX=ProjectionWidthX*GroundTargetXScale;
    GroundTargetVertexY=ProjectionWidthY*GroundTargetYScale;
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

    ActorVertexX=ProjectionWidthX*ActorXScale;
    ActorVertexY=ProjectionWidthY*ActorYScale;
    GroundTargetVertexX=ProjectionWidthX*GroundTargetXScale;
    GroundTargetVertexY=ProjectionWidthY*GroundTargetYScale;

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

    ActorVertexX=ProjectionWidthX*ActorXScale;
    ActorVertexY=ProjectionWidthY*ActorYScale;
    GroundTargetVertexX=ProjectionWidthX*GroundTargetXScale;
    GroundTargetVertexY=ProjectionWidthY*GroundTargetYScale;

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
    
    SetLastError(0);
    int iPF=GetPixelFormat(hPPIDC);
    //iPF=GetLastError();
    if(iPF==0)
        {
        Logger << "[Central::InitPixelFormat] GetPixelFormat() returned error " << GetLastError() << "\n";
        }

    DescribePixelFormat(hPPIDC,iPF,sizeof(PIXELFORMATDESCRIPTOR),&pfd);

    pfd.dwFlags |= PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_GENERIC_ACCELERATED|PFD_DOUBLEBUFFER;
    pfd.iPixelType=PFD_TYPE_RGBA;
    pfd.cColorBits=16; // 16-bit color
    pfd.cAlphaBits=8;
    pfd.cDepthBits=16;
    pfd.iLayerType=PFD_MAIN_PLANE;

    iPF=ChoosePixelFormat(hPPIDC,&pfd);
    
    DescribePixelFormat(hPPIDC,iPF,sizeof(PIXELFORMATDESCRIPTOR),&pfd);
    
    Logger << "[Central::InitPixelFormat] closest match pixel format:\n"
           << "\tpfd.dwFlags=" << pfd.dwFlags << "\n"
           << "\tpfd.iPixelType=" << (int)pfd.iPixelType << "\n"
           << "\tpfd.cColorBits=" << (int)pfd.cColorBits << "\n"
           << "\tpfd.cAlphaBits=" << (int)pfd.cAlphaBits << "\n"
           << "\tpfd.cDepthBits=" << (int)pfd.cDepthBits << "\n"
           << "\tpfd.iLayerType=" << (int)pfd.iLayerType << "\n";

    SetPixelFormat(hPPIDC,iPF,&pfd);

    Logger << "[Central::InitPixelFormat] using pixel format:\n" 
           << (pfd.dwFlags&PFD_GENERIC_ACCELERATED ? "PFD_GENERIC_ACCELERATED" : "no PFD_GENERIC_ACCELERATED") << "\n"
           << (pfd.dwFlags&PFD_GENERIC_FORMAT ? "PFD_GENERIC_FORMAT" : "no PFD_GENERIC_FORMAT") << "\n";

    // done
    return;
}// end InitPixelFormat

void Central::InitCheyenne(void)
{
    // start sound spooler
    Sounds.Go();

    // start database
    GetDatabase().Go(&MessageInputFifo);

    // start sniffer
    sniffer.Go(&MessageInputFifo);
    
    // start sharenet (connection will be made later)
    ShareNet.Go(&MessageInputFifo);
    
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
    std::auto_ptr<DatabaseFunctor> sharenet_message(new SharenetMessageFunctor(*this));

    db.InstallFunctor(Database::DatabaseEvents::MaintenanceUpdate,maint);
    db.InstallFunctor(Database::DatabaseEvents::ActorCreated,new_actor);
    db.InstallFunctor(Database::DatabaseEvents::ActorDeleted,delete_actor);
    db.InstallFunctor(Database::DatabaseEvents::ActorReassigned,reassign_actor);
    db.InstallFunctor(Database::DatabaseEvents::MaintenanceIntervalDone,maintenance_done);
    db.InstallFunctor(Database::DatabaseEvents::SharenetMessage,sharenet_message);

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
    glShadeModel(GL_SMOOTH);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_BLEND);

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

    // make disc radius 100, 36 slices (every 10°)
    DiscList=glGenLists(1);
    glNewList(DiscList,GL_COMPILE);
    glBegin(GL_TRIANGLE_FAN);

    glVertex3d
        (
        0.0,
        0.0,
        0.0
        );

    // use <= to complete the disc
    for(int i=0;i<=36;++i)
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
    
    // destroy the disc list
    glDeleteLists(DiscList,1);

    bDisplayListsCreated=false;
    // done
    return;
} // end DestroyDisplayLists

void Central::LoadVectorMaps(void)
{
    // start it up, it will post a message to us when it finishes
    VmLoader.Go();

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
        oss.seekp(0);
        oss.clear();

        // put filename in
        oss << "maps\\zone";
        oss.width(3);
        oss.fill('0');
        oss << zone << ".png";

        unsigned int id=PngBindContainer(ZoneTextureMap,zone,oss.str().c_str());
        //pngBind(oss.str().c_str(),PNG_BUILDMIPMAPS,PNG_SOLID,NULL,GL_CLAMP,GL_LINEAR_MIPMAP_NEAREST,GL_LINEAR_MIPMAP_NEAREST);

        if(id != 0)
            {
            Logger << "[Central::InitTextures] loaded texture \"" << oss.str().c_str() << "\""
                   << " with id " << id << "\n";
            }
        } // end for each zone

    // load the actor con textures
    PngBindContainer(ConTextureMap,Central::alb_gray,"skins\\alb_gray.png");
    PngBindContainer(ConTextureMap,Central::alb_green,"skins\\alb_green.png");
    PngBindContainer(ConTextureMap,Central::alb_blue,"skins\\alb_blue.png");
    PngBindContainer(ConTextureMap,Central::alb_yellow,"skins\\alb_yellow.png");
    PngBindContainer(ConTextureMap,Central::alb_orange,"skins\\alb_orange.png");
    PngBindContainer(ConTextureMap,Central::alb_red,"skins\\alb_red.png");
    PngBindContainer(ConTextureMap,Central::alb_purple,"skins\\alb_purple.png");

    PngBindContainer(ConTextureMap,Central::hib_gray,"skins\\hib_gray.png");
    PngBindContainer(ConTextureMap,Central::hib_green,"skins\\hib_green.png");
    PngBindContainer(ConTextureMap,Central::hib_blue,"skins\\hib_blue.png");
    PngBindContainer(ConTextureMap,Central::hib_yellow,"skins\\hib_yellow.png");
    PngBindContainer(ConTextureMap,Central::hib_orange,"skins\\hib_orange.png");
    PngBindContainer(ConTextureMap,Central::hib_red,"skins\\hib_red.png");
    PngBindContainer(ConTextureMap,Central::hib_purple,"skins\\hib_purple.png");

    PngBindContainer(ConTextureMap,Central::mid_gray,"skins\\mid_gray.png");
    PngBindContainer(ConTextureMap,Central::mid_green,"skins\\mid_green.png");
    PngBindContainer(ConTextureMap,Central::mid_blue,"skins\\mid_blue.png");
    PngBindContainer(ConTextureMap,Central::mid_yellow,"skins\\mid_yellow.png");
    PngBindContainer(ConTextureMap,Central::mid_orange,"skins\\mid_orange.png");
    PngBindContainer(ConTextureMap,Central::mid_red,"skins\\mid_red.png");
    PngBindContainer(ConTextureMap,Central::mid_purple,"skins\\mid_purple.png");

    PngBindContainer(ConTextureMap,Central::mob_gray,"skins\\mob_gray.png");
    PngBindContainer(ConTextureMap,Central::mob_green,"skins\\mob_green.png");
    PngBindContainer(ConTextureMap,Central::mob_blue,"skins\\mob_blue.png");
    PngBindContainer(ConTextureMap,Central::mob_yellow,"skins\\mob_yellow.png");
    PngBindContainer(ConTextureMap,Central::mob_orange,"skins\\mob_orange.png");
    PngBindContainer(ConTextureMap,Central::mob_red,"skins\\mob_red.png");
    PngBindContainer(ConTextureMap,Central::mob_purple,"skins\\mob_purple.png");

    PngBindContainer(ConTextureMap,Central::generic_alb,"skins\\generic_alb.png");
    PngBindContainer(ConTextureMap,Central::generic_hib,"skins\\generic_hib.png");
    PngBindContainer(ConTextureMap,Central::generic_mid,"skins\\generic_mid.png");
    PngBindContainer(ConTextureMap,Central::generic_mob,"skins\\generic_mob.png");
    
    // load the general textures
    PngBindContainer(GeneralTextureMap,Central::ground_target,"skins\\ground_target.png");

    // flag
    bTexturesCreated=true;

    // done
    return;
} // end InitTextures

void Central::DestroyTextures(void)
{
    Logger << "Cleaning up textures\n";
    
    // cleanup all textures
    while(ZoneTextureMap.begin() != ZoneTextureMap.end())
        {
        ZoneTextureMapIteratorType it=ZoneTextureMap.begin();

        // cleanup texture
        glDeleteTextures(1,&(it->second));

        // erase it
        ZoneTextureMap.erase(it);
        }

    while(ConTextureMap.begin() != ConTextureMap.end())
        {
        ConTextureMapIteratorType it=ConTextureMap.begin();
        
        // cleanup texture
        glDeleteTextures(1,&(it->second));

        // erase it
        ConTextureMap.erase(it);
        }

    while(GeneralTextureMap.begin() != GeneralTextureMap.end())
        {
        GeneralTextureMapIteratorType it=GeneralTextureMap.begin();
        
        // cleanup texture
        glDeleteTextures(1,&(it->second));

        // erase it
        GeneralTextureMap.erase(it);
        }
    
    // flag
    bTexturesCreated=false;

    // done
    return;
} // end DestroyTextures

Central::ConTextureMapValueType::second_type Central::GetConTexture
    (
    const Actor& ThisActor,bool bSetColor
    ) const
{
    switch(Actor::GetRelativeCon(IDToFollowLevel,ThisActor.GetLevel()))
        {
        case Actor::Gray:
            switch(ThisActor.GetRealm())
                {
                case Actor::Albion:
                    // return texture id
                    if(bSetColor)
                        {
                        glColor4f(1.0f,0.0f,0.0f,1.0f);
                        }
                    return(GetTexture(Central::alb_gray));
                    break;

                case Actor::Hibernia:
                    // return texture id
                    if(bSetColor)
                        {
                        glColor4f(0.0f,1.0f,0.0f,1.0f);
                        }
                    return(GetTexture(Central::hib_gray));
                    break;

                case Actor::Midgard:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,1.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mid_gray));
                    break;

                case Actor::MOB:
                default:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mob_gray));
                    break;
                }
            break;

        case Actor::Green:
            switch(ThisActor.GetRealm())
                {
                case Actor::Albion:
                    if(bSetColor)
                        {
                        glColor4f(1.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::alb_green));
                    break;

                case Actor::Hibernia:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,1.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::hib_green));
                    break;

                case Actor::Midgard:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,1.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mid_green));
                    break;

                case Actor::MOB:
                default:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mob_green));
                    break;
                }
            break;

        case Actor::Blue:
            switch(ThisActor.GetRealm())
                {
                case Actor::Albion:
                    if(bSetColor)
                        {
                        glColor4f(1.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::alb_blue));
                    break;

                case Actor::Hibernia:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,1.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::hib_blue));
                    break;

                case Actor::Midgard:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,1.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mid_blue));
                    break;

                case Actor::MOB:
                default:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mob_blue));
                    break;
                }
            break;

        case Actor::Yellow:
            switch(ThisActor.GetRealm())
                {
                case Actor::Albion:
                    if(bSetColor)
                        {
                        glColor4f(1.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::alb_yellow));
                    break;

                case Actor::Hibernia:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,1.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::hib_yellow));
                    break;

                case Actor::Midgard:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,1.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mid_yellow));
                    break;

                case Actor::MOB:
                default:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mob_yellow));
                    break;
                }
            break;
    
        case Actor::Orange:
            switch(ThisActor.GetRealm())
                {
                case Actor::Albion:
                    if(bSetColor)
                        {
                        glColor4f(1.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::alb_orange));
                    break;

                case Actor::Hibernia:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,1.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::hib_orange));
                    break;

                case Actor::Midgard:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,1.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mid_orange));
                    break;

                case Actor::MOB:
                default:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mob_orange));
                    break;
                }
            break;

        case Actor::Red:
            switch(ThisActor.GetRealm())
                {
                case Actor::Albion:
                    if(bSetColor)
                        {
                        glColor4f(1.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::alb_red));
                    break;

                case Actor::Hibernia:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,1.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::hib_red));
                    break;

                case Actor::Midgard:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,1.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mid_red));
                    break;

                case Actor::MOB:
                default:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mob_red));
                    break;
                }
            break;

        case Actor::Purple:
            switch(ThisActor.GetRealm())
                {
                case Actor::Albion:
                    if(bSetColor)
                        {
                        glColor4f(1.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::alb_purple));
                    break;

                case Actor::Hibernia:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,1.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::hib_purple));
                    break;

                case Actor::Midgard:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,1.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mid_purple));
                    break;

                case Actor::MOB:
                default:
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,0.0f,1.0f);
                        }
                    // return texture id
                    return(GetTexture(Central::mob_purple));
                    break;
                }
            break;

        default:
            // return texture id
                    if(bSetColor)
                        {
                        glColor4f(0.0f,0.0f,0.0f,1.0f);
                        }
            return(GetTexture(Central::generic_mob));
            break;
        } // end switch relative con
    
} // end GetConTexture

void Central::RenderActor(const Actor& ThisActor)const
{
    // if this is the followed actor, save position for targeting
    if(IDToFollow != 0)
        {
        if(ThisActor.GetInfoId() == IDToFollow)
            {
            // save followed position
            FollowedTargetPair.SetThisActor(ThisActor);

            if(ThisActor.GetTargetId() != 0)
                {
                // set to valid
                FollowedTargetPair.SetValid(true);
                }
            else
                {
                // set to invalid
                FollowedTargetPair.SetValid(false);
                }
            }
        }
    else
        {
        // set to invalid
        FollowedTargetPair.SetValid(false);
        }

    // if this is the targeted actor, save position for targeting
    if(ThisActor.GetInfoId() == FollowedTargetPair.GetThisActor().GetTargetId())
        {
        // this is the target, save position
        FollowedTargetPair.SetTarget(ThisActor);

        // if AutoHookTarget is set and no hooked actor exists already
        // then set the hooked actor equal to the targeted actor
        if(Config.GetAutoHookTarget())
            {
            HookedActor=ThisActor.GetInfoId();
            }
        }

    if((Config.GetDrawDeadActors() == false) && (ThisActor.GetHealth() == 0))
        {
        // dead actor and drawing dead actors is disabled
        return;
        }

    // if mob and rendering of gray mobs is disabled, then we are done
    if(!Config.GetRenderGrayMobs() &&
      (Actor::GetRelativeCon(IDToFollowLevel,ThisActor.GetLevel())==Actor::Gray) &&
       ThisActor.GetRealm()==Actor::MOB)
        {
        // gray mob and rendering gray mobs is disabled
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
        // store position with display offsets
        Motion Position;
        GetRenderPosition(ThisActor,Position);

        glTranslatef(Position.GetXPos(),Position.GetYPos(),0.0f);

        // set texture by realm and con
        ConTextureMapValueType::second_type CurrentActorConTexture=GetConTexture(ThisActor,true);
        if(CurrentActorConTexture != LastActorConTexture)
            {
            glBindTexture(GL_TEXTURE_2D,CurrentActorConTexture);
            LastActorConTexture=CurrentActorConTexture;
            }

        if(Config.GetPPIText())
            {
            // disable textures for this
            glDisable(GL_TEXTURE_2D);

            // draw text
            // name
            glRasterPos3f(0.0f,2.0f*ActorVertexY,0.0f);

            DrawGLFontString(ThisActor.GetName());

            if(Config.GetSurnameInPPI())
                {
                // surname
                DrawGLFontString(std::string(" "));
                DrawGLFontString(ThisActor.GetSurname());
                }

            if(Config.GetGuildInPPI())
                {
                // guild
                DrawGLFontString(std::string(" "));
                DrawGLFontString(ThisActor.GetGuild());
                }

            if(Config.GetLevelInPPI())
                {
                // level
                char level[8];
                sprintf(level," %u",unsigned int(ThisActor.GetLevel()));
                DrawGLFontString(std::string(level));
                }

            if(Config.GetHealthInPPI())
                {
                // health
                char health[8];
                sprintf(health," %u%%",unsigned int(ThisActor.GetHealth()));
                DrawGLFontString(std::string(health));
                }
            } // end if draw text in PPI

        // rotate for heading
        glRotatef(180.0f+Position.GetHeading()*57.295779513082320876798154814105f,0.0f,0.0f,1.0f);
        //                                 convert to degrees for glRotatef

        if(!ThisActor.GetOld())
            {
            // enable textures
            glEnable(GL_TEXTURE_2D);
            }
        else
            {
            // draw with color coding instead of texture
            // for old actors (those that have not been 
            // updated for a while: the data is stale)
            // enable textures
            glDisable(GL_TEXTURE_2D);
            
            // set color to a dark reddish purple color cause
            // its different from everything else ;)
            glColor4f(0.5f,0.0f,0.25f,1.0f);
            }

        // store current color
        float PreAlphaColors[4];
        glGetFloatv(GL_CURRENT_COLOR,&PreAlphaColors[0]);

        if(ThisActor.GetStealth())
            {
            // enable alpha blending
            glEnable(GL_BLEND);
            }
        
        // draw actor symbol
        glBegin(GL_TRIANGLES);

        glColor4f(PreAlphaColors[0],PreAlphaColors[1],PreAlphaColors[2],ThisActor.GetStealthCycleB());
        glTexCoord2f(0.5f,1.0f);
        glVertex3f(0.0f,ActorVertexY,0.0f);

        glColor4f(PreAlphaColors[0],PreAlphaColors[1],PreAlphaColors[2],ThisActor.GetStealthCycleA());
        glTexCoord2f(0.0f,0.0f);
        glVertex3f(-ActorVertexX,-ActorVertexY,0.0f);

        glColor4f(PreAlphaColors[0],PreAlphaColors[1],PreAlphaColors[2],ThisActor.GetStealthCycleC());
        glTexCoord2f(1.0f,0.0f);
        glVertex3f(ActorVertexX,-ActorVertexY,0.0f);

        glEnd();

        if(ThisActor.GetStealth())
            {
            // disable alpha blending
            glDisable(GL_BLEND);
            }
            
        // restore colors
        glColor4fv(&PreAlphaColors[0]);

        // disable textures
        glDisable(GL_TEXTURE_2D);

        // if it's "hooked" draw a hook
        if(ThisActor.GetInfoId() == HookedActor)
            {

            // radius for hook is (ActorVertexX + ActorVertexY)/2 + 25;
            const float radius=25.0f+(0.5f*(ActorVertexX + ActorVertexY));

            // draw circle
            DrawCircle(radius);
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

                    DrawCircle(float(Config.GetRangeRings().Rings[i].Radius));

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
    // enable depth test
    glEnable(GL_DEPTH_TEST);

    // render every zone
    for(unsigned char i=0;i<=254;++i)
        {
        // only render valid zones (XOffset==0 is a special case of an invalid zone)
        if(Zones.GetZone(i).bValid)// && Zones.GetLimitsFromRegion(Zones.GetZone(i).Region).XOffset!=0)
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
                glPushMatrix();
                glTranslatef(float(BaseX),float(BaseY),0);

                if(Config.GetTexturesInPPI())
                    {
                    // enable textures
                    glEnable(GL_TEXTURE_2D);

                    // activate the proper texture
                    ZoneTextureMapConstIteratorType it=ZoneTextureMap.find(i);

                    if(it != ZoneTextureMap.end())
                        {
                        glBindTexture(GL_TEXTURE_2D,it->second);
                        }
                    else
                        {
                        glBindTexture(GL_TEXTURE_2D,0);
                        }
                    }

                // draw this zone

                glBegin(GL_QUADS);

                glColor3f(0.5f,0.5f,0.5f);
                //glColor3f(0.1f,0.1f,0.1f);

                glTexCoord2i(0,1);
                glVertex3i(0,MaxY-BaseY,0);

                glTexCoord2i(0,0);
                glVertex3i(0,0,0);

                glTexCoord2i(1,0);
                glVertex3i(MaxX-BaseX,0,0);

                glTexCoord2i(1,1);
                glVertex3i(MaxX-BaseX,MaxY-BaseY,0);

                glEnd();
            
                //glColor4f(0.75f,0.75f,0.75f,1.0f);
                glColor4f(1.0f,1.0f,1.0f,1.0f);
                
                // disable textures for text and vector maps
                glDisable(GL_TEXTURE_2D);

                // draw zone name text
                glRasterPos3i
                    (
                    0,
                    0,
                    2
                    );

                //glCallLists(zone.ZoneFile.length(),GL_UNSIGNED_BYTE,zone.ZoneFile.c_str());
                DrawGLFontString(zone.ZoneFile);

                // draw vector map with no z buffering
                glDisable(GL_DEPTH_TEST);

                // draw vector map only if enabled in options
                if(Config.GetVectorMapInPPI())
                    {
                    // only draw if we are drawing the followed actor's zone AND only followed
                    // zones are drawn OR if we are drawing vector maps regardless of followed zone
                    if((Config.GetVectorMapOnlyInFollowedZone() && i==IDToFollowZone) || 
                       !Config.GetVectorMapOnlyInFollowedZone())
                        {
                        VmLoader.Draw(i);
                        }
                    }

                // reenable these
                glEnable(GL_DEPTH_TEST);
                glPopMatrix();
                } // end if zone is visible
            } // end if zone is valid
        } // end for each zone

    // done
    return;
} // end RenderWorld

void Central::RenderCloseControl(void)const
{
    if(!FollowedTargetPair.GetValid() || (rPPIClient.bottom-rPPIClient.top)==0)
        {
        // nothing to do
        return;
        }

    // get positions of followed actor and target actor
    Motion FollowedPosition;
    Motion TargetPosition;
    GetRenderPosition(FollowedTargetPair.GetThisActor(),FollowedPosition);
    GetRenderPosition(FollowedTargetPair.GetTarget(),TargetPosition);
    
    // get text metric data so the text and bounding box are always the same size in pixels
    
    // get line height (with padding) in pixels
    float fractional_line_height=float(TahomaTextMetric.tmHeight + 1L);
    // convert it to fraction of display height
    fractional_line_height = fractional_line_height / (float(rPPIClient.bottom-rPPIClient.top));
    // get char width in pixels
    float fractional_line_width=float(TahomaTextMetric.tmAveCharWidth);
    // convert it to fraction of display width
    fractional_line_width = fractional_line_width / (float(rPPIClient.right-rPPIClient.left));
    
    // get position of targetted actor with respect to hooked actor
    std::pair<float,float> rng_az=FollowedPosition.RangeAzimuthTo(TargetPosition);
    
    // draw rectangle

    // render with no depth test so they all display
    glDisable(GL_DEPTH_TEST);

    glPushMatrix();
    // enable alpha blending
    glEnable(GL_BLEND);
    // set to 1/2 transparent black
    glColor4f(0.0f,0.0f,0.0f,0.5f);
    // translate over
    glTranslatef(ProjectionX,ProjectionY,0.0f);
    // draw -- the 2.5 and 32.0 are the number of lines we want to be able to 
    // draw and the number of characters on each line respectively
    glBegin(GL_QUADS);
    glVertex3f(0.0f,(ProjectionWidthY*fractional_line_height*2.5f),0.0f);
    glVertex3f(0.0f,0.0f,0.0f);
    glVertex3f(ProjectionWidthX * (fractional_line_width*32.0f),0.0f,0.0f);
    glVertex3f(ProjectionWidthX * (fractional_line_width*32.0f),(ProjectionWidthY*fractional_line_height*2.5f),0.0f);
    glEnd();
    
    // set to 1/4 transparent con color
    switch(Actor::GetRelativeCon(FollowedTargetPair.GetThisActor().GetLevel(),FollowedTargetPair.GetTarget().GetLevel()))
        {
        case Actor::Green:
            glColor4f(0.0f,1.0f,0.0f,0.75f);
            break;
        case Actor::Blue:
            glColor4f(0.0f,0.0f,1.0f,0.75f);
            break;
        case Actor::Yellow:
            glColor4f(1.0f,1.0f,0.0f,0.75f);
            break;
        case Actor::Orange:
            glColor4f(1.0f,0.5f,0.25f,0.75f);
            break;
        case Actor::Red:
            glColor4f(1.0f,0.0f,0.0f,0.75f);
            break;
        case Actor::Purple:
            glColor4f(0.5f,0.0f,1.0f,0.75f);
            break;
        case Actor::Gray:
        default:
            glColor4f(0.75f,0.75f,0.75f,0.75f);
            break;
        } // end switch relative con

    // draw text
    std::ostringstream oss;
    
    // name
    oss << FollowedTargetPair.GetTarget().GetName() << " " << FollowedTargetPair.GetTarget().GetSurname();
    glRasterPos3i
        (
        int(ProjectionWidthX*fractional_line_width),
        int(ProjectionWidthY*fractional_line_height*0.75f),
        //ProjectionWidthY - (ProjectionWidthY*fractional_line_height*1.75f),
        0
        );
    DrawGLFontString(oss.str());
    oss.str("");
    oss.seekp(0);
    oss.clear();
    
    // range/bearing
    oss << "<" << unsigned int(rng_az.first) << ",";
    oss.precision(3);
    oss << RadToDeg(rng_az.second) << "°>";
    glRasterPos3i
        (
        int(ProjectionWidthX*fractional_line_width),
        int(ProjectionWidthY*fractional_line_height*2.0f),
        //ProjectionWidthY - (ProjectionWidthY*fractional_line_height*0.875f),
        0
        );
    DrawGLFontString(oss.str());

    // disable alpha blending
    glDisable(GL_BLEND);

    glPopMatrix();
    
    // done
    return;
} // end RenderCloseControl

void Central::RenderUncorrelatedStealth(void)const
{
    if(db.IsUncorrelatedStealth())
        {
        // draw stealth disc
        glPushMatrix();
        
        // render with no depth test so it will definately be displayed
        glDisable(GL_DEPTH_TEST);

        // get followed actor render position
        Actor CenterActor=db.GetUncorrelatedStealthCenter();
        Motion Position;
        GetRenderPosition(CenterActor,Position);
        // translate to position
        glTranslatef(Position.GetXPos(),Position.GetYPos(),0.0f);
        
        // draw disc
        glEnable(GL_BLEND);
        glColor4f(1.0f,0.0f,0.0f,0.25f);
        DrawDisc(3000.0f);
        //DrawCircle(3000.0f);
        
        // cleanup and done
        glDisable(GL_BLEND);
        glPopMatrix();
        }
    
    // done
    return;
} // end RenderUncorrelatedStealth

void Central::RenderMobfinderResults(void)const
{
    // only do the first 25 mob locations
    int count;
    NarrowIdDef::loc_list_t::const_iterator it;
    Motion Position;
    
    // disable this
    glDisable(GL_DEPTH_TEST);
    
    // disable textures for this
    glDisable(GL_TEXTURE_2D);
    
    for(it=MobfinderResults.loc_list.begin(),count=0;count<25 && it!=MobfinderResults.loc_list.end();++count,++it)
        {
        // save positions
        unsigned int x;
        unsigned int y;
        unsigned short z;
        
        // turn zone-relative coordinates into region coordinates
        ::Zones.GetGlobalFromZone
            (
            it->zone,
            unsigned short(it->x),
            unsigned short(it->y),
            unsigned short(it->z),
            x,
            y,
            z
            );

        Position.SetXPos(float(x));
        Position.SetYPos(float(y));
        Position.SetZPos(float(z));
        
        // turn into renderable coordinates
        AdjustPositionByRegion(Position,Zones.GetZone((unsigned char)it->zone).Region);
        
        // translate there
        glPushMatrix();
        glTranslatef(Position.GetXPos(),Position.GetYPos(),0.0f);
        // set gold color
        glColor4f(1.0f,1.0f,0.5f,1.0f);
        // draw disk and name at each location
        DrawDisc(50.0f);
        // draw mob name there too
        glRasterPos3f(55.0f,0.0f,0.0f);
        DrawGLFontString(MobfinderResults.name);
        glPopMatrix();
        } // end for the first 25 entries in the loc list
    
    // reenablethis
    glEnable(GL_DEPTH_TEST);

    // done
    return;
} // end RenderMobfinderResults

void Central::DrawPPI(void)
{
    if(!hRenderContext || IsIconic(hMainWnd))
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

    // render all actors and ground target with no depth test so they all display
    glDisable(GL_DEPTH_TEST);
    
    // first, draw ground target
    if(db.IsGroundTargetSet())
        {
        // set to cyan and half-transparent
        glColor4f(0.0f,1.0f,1.0f,0.5f);
        
        // get the display position for the ground target
        Motion gt(db.GetGroundTarget());
        AdjustPositionByRegion(gt,db.GetGroundTargetRegion());
        
        // enable alpha blending
        glEnable(GL_BLEND);

        // enable textures
        glEnable(GL_TEXTURE_2D);

        // set texture 
        glBindTexture(GL_TEXTURE_2D,GetTexture(GeneralAssociations::ground_target));

        glBegin(GL_QUADS);
        glTexCoord2f(0.0f,1.0f);
        glVertex3f(gt.GetXPos()-GroundTargetVertexX,gt.GetYPos()+GroundTargetVertexY,0.0f);
        
        glTexCoord2f(0.0f,0.0f);
        glVertex3f(gt.GetXPos()-GroundTargetVertexX,gt.GetYPos()-GroundTargetVertexY,0.0f);
        
        glTexCoord2f(1.0f,0.0f);
        glVertex3f(gt.GetXPos()+GroundTargetVertexX,gt.GetYPos()-GroundTargetVertexY,0.0f);
        
        glTexCoord2f(1.0f,1.0f);
        glVertex3f(gt.GetXPos()+GroundTargetVertexX,gt.GetYPos()+GroundTargetVertexY,0.0f);
        glEnd();

        // disable textures
        glDisable(GL_TEXTURE_2D);

        // disable alpha blending
        glDisable(GL_BLEND);
        } // end if ground target is set

    // reset this so that we are guaranteed to 
    // set the actor texture at least once each rendering pass
    LastActorConTexture=0;

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

    // draw targeting info
    if(FollowedTargetPair.GetValid())
        {
        // the pair is valid, draw it
        // store position with display offsets
        Motion Position1;
        Motion Position2;
        GetRenderPosition(FollowedTargetPair.GetThisActor(),Position1);
        GetRenderPosition(FollowedTargetPair.GetTarget(),Position2);

        // set to cyan
        glColor4f(0.0f,1.0f,1.0f,1.0f);
        // radius for target is (ActorVertexX + ActorVertexY)/2 + 25;
        const float radius=25.0f+(0.5f*(ActorVertexX + ActorVertexY));

        // move to Position1 and draw a circle
        glPushMatrix();
        glTranslatef(Position1.GetXPos(),Position1.GetYPos(),0.0f);
        DrawCircle(radius);
        glPopMatrix();

        // move to Position2 and draw a circle
        glPushMatrix();
        glTranslatef(Position2.GetXPos(),Position2.GetYPos(),0.0f);
        DrawCircle(radius);
        glPopMatrix();

        // draw a line connecting them
        // for now, this is a simple line. it should
        // stop at the circle edge instead of the actor
        // positions ;)
        glBegin(GL_LINES);
        glVertex3f(Position1.GetXPos(),Position1.GetYPos(),0.0f);
        glVertex3f(Position2.GetXPos(),Position2.GetYPos(),0.0f);
        glEnd();
        }

    // render the close control
    RenderCloseControl();
    
    // render uncorrelated stealth disc
    RenderUncorrelatedStealth();
    
    // render mobfinder results
    RenderMobfinderResults();
    
    glPopMatrix();

    // finish and swap
    glFlush();
    glFinish();
    SwapBuffers(hPPIDC);
    
    // increment frame count
    ++Frames;
        
    // done
    return;
} // end DrawPPI
