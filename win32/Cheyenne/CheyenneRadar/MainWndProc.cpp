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
#include <iostream>
#include <string>
#include <algorithm>
#include <limits>
#include "..\Utils\CodeUtils.h"
#include "..\Utils\Logger.h" // for the logger
#include "..\Utils\mapinfo.h" // for the zones class
#include "..\Utils\times.h" // for the clock
#include "..\Utils\tsdeque.h" // for fifo defs
#include "..\Utils\buffer.h" // for the buffer
#include "..\GLPPI\GLPPI.h" // for the ppi class
#include "..\Database\database.h" // for the database
#include "..\Sniffer\DAOCConnection.h" // for the daoc components of the data connection
#include "..\Sniffer\DStreamConnection.h" // for the data connection
#include "..\Sharenet\sharenet.h" // for sharenet
#include "..\Sharenet\MiniShareServer.h" // for server definition
#include "..\Sharenet\TelnetClient.h" // for telnet server definition
#include "..\Script\CSLScriptHost.h" // for the script host
#include "config.h" // for the global configuration
#include "main.h"
#include "resource.h"

struct USERDATA
{
USERDATA() : scriptserver(23),ppi_id(1),data_id(2),Rendering(false),ReferenceSet(false),
             HookedSet(false),ReferenceTargetSet(false){};
~USERDATA(){};

// Win API members
HFONT hTahoma;
HFONT hTahomaBig;
TEXTMETRIC TahomaTM;
TEXTMETRIC TahomaBigTM;
const int ppi_id;
const int data_id;

// class members
GLPPI ppi;
Database database;
DStreamConnection dstream;
ShareNetClientData sharenet;
csl::CSLScriptHost scripthost;
MiniShareServer<TelnetClientData> scriptserver; // must be created with a parameter

// "IPC" members
tsfifo<CheyenneMessage*> ToDatabaseFifo; // the database Pop()'s this on a regular basis
tsfifo<std::string*> TelnetFifoSend;
tsfifo<std::string*> TelnetFifoReceive;

// utility members
Actor ReferenceActor;
bool ReferenceSet;
Actor HookedActor;
bool HookedSet;
Actor ReferenceTarget;
bool ReferenceTargetSet;
bool Rendering;

// functions
void OnRenderActor(const Actor& a)
{
    // get con of actor 'a' relative to the reference actor
    Actor::RelativeCon con=Actor::GetRelativeCon(ReferenceActor.GetLevel(),a.GetLevel());
    
    // texture id storage
    GLPPI::TextureId tex;
    
    switch(con)
        {
        case Actor::Gray:
            switch(a.GetRealm())
                {
                case Actor::Albion:
                    tex=GLPPI::alb_gray;
                    break;
                case Actor::Hibernia:
                    tex=GLPPI::hib_gray;
                    break;
                case Actor::Midgard:
                    tex=GLPPI::mid_gray;
                    break;
                default:
                case Actor::MOB:
                    tex=GLPPI::mob_gray;
                    break;
                }
            break;
        case Actor::Green:
            switch(a.GetRealm())
                {
                case Actor::Albion:
                    tex=GLPPI::alb_green;
                    break;
                case Actor::Hibernia:
                    tex=GLPPI::hib_green;
                    break;
                case Actor::Midgard:
                    tex=GLPPI::mid_green;
                    break;
                default:
                case Actor::MOB:
                    tex=GLPPI::mob_green;
                    break;
                }
            break;
        case Actor::Blue:
            switch(a.GetRealm())
                {
                case Actor::Albion:
                    tex=GLPPI::alb_blue;
                    break;
                case Actor::Hibernia:
                    tex=GLPPI::hib_blue;
                    break;
                case Actor::Midgard:
                    tex=GLPPI::mid_blue;
                    break;
                default:
                case Actor::MOB:
                    tex=GLPPI::mob_blue;
                    break;
                }
            break;
        case Actor::Yellow:
            switch(a.GetRealm())
                {
                case Actor::Albion:
                    tex=GLPPI::alb_yellow;
                    break;
                case Actor::Hibernia:
                    tex=GLPPI::hib_yellow;
                    break;
                case Actor::Midgard:
                    tex=GLPPI::mid_yellow;
                    break;
                default:
                case Actor::MOB:
                    tex=GLPPI::mob_yellow;
                    break;
                }
            break;
        case Actor::Orange:
            switch(a.GetRealm())
                {
                case Actor::Albion:
                    tex=GLPPI::alb_orange;
                    break;
                case Actor::Hibernia:
                    tex=GLPPI::hib_orange;
                    break;
                case Actor::Midgard:
                    tex=GLPPI::mid_orange;
                    break;
                default:
                case Actor::MOB:
                    tex=GLPPI::mob_orange;
                    break;
                }
            break;
        case Actor::Red:
            switch(a.GetRealm())
                {
                case Actor::Albion:
                    tex=GLPPI::alb_red;
                    break;
                case Actor::Hibernia:
                    tex=GLPPI::hib_red;
                    break;
                case Actor::Midgard:
                    tex=GLPPI::mid_red;
                    break;
                default:
                case Actor::MOB:
                    tex=GLPPI::mob_red;
                    break;
                }
            break;
        case Actor::Purple:
        default:
            switch(a.GetRealm())
                {
                case Actor::Albion:
                    tex=GLPPI::alb_purple;
                    break;
                case Actor::Hibernia:
                    tex=GLPPI::hib_purple;
                    break;
                case Actor::Midgard:
                    tex=GLPPI::mid_purple;
                    break;
                default:
                case Actor::MOB:
                    tex=GLPPI::mob_purple;
                    break;
                }
            break;
        } // end switch on relative CON
    
    // render with preferences
    if(a.GetRealm()==Actor::MOB)
        {
        ppi.RenderActor(a,tex,::RadarConfig.GetPrefsMob(),con);        
        }
    else
        {
        if(ReferenceActor.GetRealm()==a.GetRealm())
            {
            ppi.RenderActor(a,tex,::RadarConfig.GetPrefsSameRealm(),con);        
            }
        else
            {
            ppi.RenderActor(a,tex,::RadarConfig.GetPrefsEnemyRealm(),con);        
            }
        }
    
    // check if this is the reference actor    
    if(a.GetInfoId() == ReferenceActor.GetInfoId())
        {
        // do the range rings
        if(::RadarConfig.GetShowRangeRing1())ppi.RenderRangeRing(ReferenceActor,::RadarConfig.GetRangeRingRange1());
        if(::RadarConfig.GetShowRangeRing2())ppi.RenderRangeRing(ReferenceActor,::RadarConfig.GetRangeRingRange2());
        if(::RadarConfig.GetShowRangeRing3())ppi.RenderRangeRing(ReferenceActor,::RadarConfig.GetRangeRingRange3());
        if(::RadarConfig.GetShowRangeRing4())ppi.RenderRangeRing(ReferenceActor,::RadarConfig.GetRangeRingRange4());
        if(::RadarConfig.GetShowRangeRing5())ppi.RenderRangeRing(ReferenceActor,::RadarConfig.GetRangeRingRange5());
        if(::RadarConfig.GetShowRangeRing6())ppi.RenderRangeRing(ReferenceActor,::RadarConfig.GetRangeRingRange6());
        }
    
    // if hooked actor, draw a hook symbol too
    if(a.GetInfoId() == HookedActor.GetInfoId() && HookedSet)
        {
        ppi.RenderHook(HookedActor,0.0f,1.0f,0.0f);
        //ppi.RenderRangeRing(HookedActor,200.0f,0.0f,1.0f,0.0f);
        }
    
    // if reference's target actor, draw target symbol too
    if(a.GetInfoId() == ReferenceTarget.GetInfoId() && ReferenceTargetSet)
        {
        ppi.RenderHook(ReferenceTarget,1.0f,0.0f,0.0f);
        //ppi.RenderRangeRing(ReferenceTarget,200.0f,1.0f,0.0f,0.0f);
        }
    
    // done
    return;
} // end OnRenderActor

void OnMaintenanceUpdate(const Actor& a)
{
    // chek for reference actor being updated
    if(a.GetInfoId()==ReferenceActor.GetInfoId() && ReferenceSet)
        {
        // save reference actor
        ReferenceActor=a;
        
        // if we're not rendering, then recenter display
        // on newly updated reference actor
        if(!Rendering)
            {
            ppi.CenterOn(ReferenceActor);
            }
        } // end if the reference actor is being updated
    
    // check for reference's target being updated
    if(a.GetInfoId()==ReferenceActor.GetTargetId() && ReferenceSet)
        {
        // save reference's target
        ReferenceTarget=a;
        ReferenceTargetSet=true;
        } // end if reference's target being updated
} // end OnMaintenanceUpdate
void OnNewActor(const Actor& a)
{
    if(!ReferenceSet)
        {
        // only set players as the reference actor
        if(a.IsType(Actor::Player))
            {
            LOG_FUNC << "reference set\n";
            // set new reference actor
            ReferenceSet=true;
            ReferenceActor=a;
            }
        } // end if reference not set
    else
        {
        // see about playing sounds
        switch(a.GetRealm())
            {
            case Actor::Albion:
                if(::RadarConfig.GetPlayAlbSound()) PlaySound(::RadarConfig.GetNewAlbSound().c_str(),NULL,SND_FILENAME|SND_ASYNC|SND_NOSTOP|SND_NOWAIT);
                break;
            case Actor::Hibernia:
                if(::RadarConfig.GetPlayHibSound()) PlaySound(::RadarConfig.GetNewHibSound().c_str(),NULL,SND_FILENAME|SND_ASYNC|SND_NOSTOP|SND_NOWAIT);
                break;
            case Actor::Midgard:
                if(::RadarConfig.GetPlayMidSound()) PlaySound(::RadarConfig.GetNewMidSound().c_str(),NULL,SND_FILENAME|SND_ASYNC|SND_NOSTOP|SND_NOWAIT);
                break;
                
            default:
                // mob or object, this will get handled below
                break;
            } // end switch realm
        // use named mob for players as well as mobs :)
        if(::RadarConfig.GetPlayNamedMobSound())
            {
            if(::RadarConfig.GetNamedMobName() == a.GetName())
                {
                PlaySound(::RadarConfig.GetNamedMobSound().c_str(),NULL,SND_FILENAME|SND_ASYNC|SND_NOSTOP|SND_NOWAIT);
                } // end if name match
            } // end if play named mob sound
        } // end else reference set
} // end OnNewActor

void OnDeleteActor(const Actor& a)
{
    if(a.GetInfoId()==ReferenceActor.GetInfoId())
        {
        LOG_FUNC << "reference unset\n";
        ReferenceSet=false;
        ReferenceTargetSet=false;
        }

    if(a.GetInfoId()==HookedActor.GetInfoId())
        {
        HookedSet=false;
        }
    
    if(a.GetInfoId()==ReferenceTarget.GetInfoId())
        {
        ReferenceTargetSet=false;
        }
} // end OnDeleteActor
void OnReassignActor(const Actor& a){};
void OnMaintenanceIntervalDone(void){};
void OnSharenetMessage(const void* p,const unsigned int len){sharenet.QueueOutputMessage(p,len);};
void OnDatabaseReset(void){ReferenceSet=false;HookedSet=false;ReferenceTargetSet=false;};
bool ConfigPreventsRendering(const Actor& a)const
{
    // things that can prevent rendering:
    // realm
    // level (grays for instance)
    // type (don't render objects for instance)
    
    // do realm first
    switch(a.GetRealm())
        {
        case Actor::Realms::Albion:
            if(!::RadarConfig.GetShowAlbs())return(true);
            break;
            
        case Actor::Realms::Hibernia:
            if(!::RadarConfig.GetShowHibs())return(true);
            break;
            
        case Actor::Realms::Midgard:
            if(!::RadarConfig.GetShowMids())return(true);
            break;
            
        default:
            break;
        } // end switch realm
    
    // do type
    switch(a.GetActorType())
        {
        case Actor::ActorTypes::Mob:
            if(!::RadarConfig.GetShowMobs())return(true);
            break;
            
        case Actor::ActorTypes::Object:
            if(!::RadarConfig.GetShowObjects())return(true);
            break;
            
        default:
            break;
        } // end switch type
    
    // done
    return(false);
} // end ConfigPreventsRendering
}; // end USERDATA

// functors
class ActorRenderFunctor
{
public:
    ActorRenderFunctor(const ActorRenderFunctor& s):data(s.data){};
    explicit ActorRenderFunctor(USERDATA& s):data(s){};
    ~ActorRenderFunctor(){};

    void operator()(const Database::actor_map_value& s)
    {
        // make sure its renderable, if not then bail
        if(data.ConfigPreventsRendering(s.second))return;
        
        data.OnRenderActor(s.second);
    }

private:
    ActorRenderFunctor& operator=(const ActorRenderFunctor& s); // disallow
    USERDATA& data;
};

class UncorrelatedStealthRenderFunctor
{
public:
    UncorrelatedStealthRenderFunctor(const UncorrelatedStealthRenderFunctor& s):data(s.data){};
    explicit UncorrelatedStealthRenderFunctor(USERDATA& s):data(s){};
    ~UncorrelatedStealthRenderFunctor(){};

    void operator()(const Database::stealth_map_value& s)
    {
        // alias
        const Database::id_type& infoid=s.first;
        const UncorrelatedStealthInfo& usi=s.second;
        const StealthMask& sm=usi.GetMask();
        
        // get original id and region
        unsigned short original_region;
        Database::id_type original_id;
        Database::CrackUniqueId(infoid,original_region,original_id);
        MapInfo::RegionIndexType region=(MapInfo::RegionIndexType)original_region;
        
        for(int y=0;y<15;++y)
            {
            for(int x=0;x<15;++x)
                {
                if(sm.Get(x,y))
                    {
                    const float offset_x=StealthMask::SpanX*(8-x);
                    const float offset_y=StealthMask::SpanY*(8-y);
                    
                    data.ppi.RenderUncorrelatedStealthBlock
                        (
                        usi.GetAverageX() + offset_x,
                        usi.GetAverageY() + offset_y,
                        StealthMask::SpanX,
                        StealthMask::SpanY,
                        region
                        );
                    }
                }
            }
    }

private:
    UncorrelatedStealthRenderFunctor& operator=(const UncorrelatedStealthRenderFunctor& s); // disallow
    USERDATA& data;
}; // end class UncorrelatedStealthRenderFunctor

class ClosestActorFinder
{
public:
    ClosestActorFinder(const ClosestActorFinder& s) :
        data(s.data),PointX(s.PointX),PointY(s.PointY),Dist(s.Dist),ClosestActor(s.ClosestActor){};
    
    explicit ClosestActorFinder(const USERDATA& s,const float x,const float y) :
        data(s),PointX(x),PointY(y)
    {
        Dist=std::numeric_limits<float>::max();
    };
    
    ~ClosestActorFinder(){};

    void operator()(const Database::actor_map_value& s)
    {
        const Actor& ThisActor=s.second;
        
        // make sure its renderable, if not then bail
        if(data.ConfigPreventsRendering(ThisActor))return;
        
        Motion Position;
        data.ppi.GetRenderPosition(ThisActor,Position);
        GLdouble x,y,z;
        data.ppi.GetScreenPosition
            (
            Position.GetXPos(),
            Position.GetYPos(),
            Position.GetZPos(),
            &x,
            &y,
            &z
            );

        //LOG_FUNC << "comparing <" << x << "," << y << "> with <" << PointX << "," << PointY << ">\n";

        float DelX=(float)x - PointX;
        float DelY=(float)y - PointY;

        const float ThisDist=float(sqrt(DelX*DelX + DelY*DelY));

        // save
        if(ThisDist < Dist)
            {
            Dist=ThisDist;
            ClosestActor=ThisActor;
            }

        // done
        return;
    }

    float GetDist(void)const{return(Dist);};
    Actor GetClosestActor(void)const{return(ClosestActor);};
private:
    ClosestActorFinder& operator=(const ClosestActorFinder& s); // disallow
    const USERDATA& data;
    const float PointX;
    const float PointY;
    float Dist;
    Actor ClosestActor;
};

class MaintenanceUpdateFunctor : public DatabaseFunctor
{
public:
    MaintenanceUpdateFunctor(USERDATA& s):data(s){};
    explicit MaintenanceUpdateFunctor(USERDATA& c,const MaintenanceUpdateFunctor& s) : data(c),DatabaseFunctor(s){};
    virtual ~MaintenanceUpdateFunctor(){};


protected:
    virtual void DoIt(const Actor& a){data.OnMaintenanceUpdate(a);};

private:
    USERDATA& data;
}; // end class MaintainanceUpdateFunctor

class DatabaseResetFunctor: public DatabaseFunctor
{
public:
    DatabaseResetFunctor(USERDATA& s):data(s){};
    explicit DatabaseResetFunctor(USERDATA& c,const DatabaseResetFunctor& s) : data(c),DatabaseFunctor(s){};
    virtual ~DatabaseResetFunctor(){};


protected:
    virtual void DoIt(void){data.OnDatabaseReset();};

private:
    USERDATA& data;
}; // end class DatabaseResetFunctor

class NewActorFunctor : public DatabaseFunctor
{
public:
    NewActorFunctor(USERDATA& s):data(s){};
    explicit NewActorFunctor(USERDATA& c,const NewActorFunctor& s) : data(c),DatabaseFunctor(s){};
    virtual ~NewActorFunctor(){};


protected:
    virtual void DoIt(const Actor& a){data.OnNewActor(a);};

private:
    USERDATA& data;
}; // end class NewActorFunctor

class DeleteActorFunctor : public DatabaseFunctor
{
public:
    DeleteActorFunctor(USERDATA& s):data(s){};
    explicit DeleteActorFunctor(USERDATA& c,const DeleteActorFunctor& s) : data(c),DatabaseFunctor(s){};
    virtual ~DeleteActorFunctor(){};


protected:
    virtual void DoIt(const Actor& a){data.OnDeleteActor(a);};

private:
    USERDATA& data;
}; // end class DeleteActorFunctor

class ReassignActorFunctor : public DatabaseFunctor
{
public:
    ReassignActorFunctor(USERDATA& s):data(s){};
    explicit ReassignActorFunctor(USERDATA& c,const ReassignActorFunctor& s) : data(c),DatabaseFunctor(s){};
    virtual ~ReassignActorFunctor(){};


protected:
    virtual void DoIt(const Actor& a){data.OnReassignActor(a);};

private:
    USERDATA& data;
}; // end class ReassignActorFunctor

class MaintenanceIntervalDoneFunctor : public DatabaseFunctor
{
public:
    MaintenanceIntervalDoneFunctor(USERDATA& s):data(s){};
    explicit MaintenanceIntervalDoneFunctor(USERDATA& c,const MaintenanceIntervalDoneFunctor& s) : data(c),DatabaseFunctor(s){};
    virtual ~MaintenanceIntervalDoneFunctor(){};


protected:
    virtual void DoIt(void){data.OnMaintenanceIntervalDone();};

private:
    USERDATA& data;
}; // end class MaintenanceIntervalDoneFunctor

class SharenetMessageFunctor : public DatabaseFunctor
{
public:
    SharenetMessageFunctor(USERDATA& s):data(s){};
    explicit SharenetMessageFunctor(USERDATA& c,const SharenetMessageFunctor& s) : data(c),DatabaseFunctor(s){};
    virtual ~SharenetMessageFunctor(){};


protected:
    virtual void DoIt(const void* p,const unsigned int len){data.OnSharenetMessage(p,len);};

private:
    USERDATA& data;
}; // end class SharenetMessageFunctor

// local function prototypes
void HandleCreate(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
void HandleCommand(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam,USERDATA* data);
void HandleSize(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam,USERDATA* data);
void Render(USERDATA* data);
void HandleMouseWheel(HWND hWnd,WPARAM wParam,LPARAM lParam,USERDATA* data);
void HandleKeyDown(HWND hWnd,WPARAM wParam,LPARAM lParam,USERDATA* data);
void DrawDataWindow(HWND hWnd,HDC hFront,USERDATA* data);
void HandleLButtonDoubleClick(HWND hWnd,WPARAM wParam,LPARAM lParam,USERDATA* data);

LRESULT CALLBACK PPIWndProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    // get my parameters
    USERDATA* data=reinterpret_cast<USERDATA*>(GetWindowLongPtr(hWnd,GWLP_USERDATA));
    
    switch(uMsg)
        {
        case WM_CREATE:
            {
            CREATESTRUCT* cs=(CREATESTRUCT*)lParam;
            SetWindowLongPtr(hWnd,GWLP_USERDATA,(LONG_PTR)(cs->lpCreateParams));
            }
            break;
        
        case WM_KEYDOWN:
            HandleKeyDown(hWnd,wParam,lParam,data);
            break;
            
        case WM_MOUSEWHEEL:
            HandleMouseWheel(hWnd,wParam,lParam,data);
            break;
            
        case WM_LBUTTONDBLCLK:
            HandleLButtonDoubleClick(hWnd,wParam,lParam,data);
            break;
        
        default:
            break;
        }
    
    return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
} // end PPIWndProc

LRESULT CALLBACK DataWndProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    // get my parameters
    USERDATA* data=reinterpret_cast<USERDATA*>(GetWindowLongPtr(hWnd,GWLP_USERDATA));

    switch(uMsg)
        {
        case WM_CREATE:
            {
            CREATESTRUCT* cs=(CREATESTRUCT*)lParam;
            SetWindowLongPtr(hWnd,GWLP_USERDATA,(LONG_PTR)(cs->lpCreateParams));
            
            SetTimer(hWnd,1,1000,0);
            }
            break;
        
        case WM_PAINT:
            {
            PAINTSTRUCT ps;
            BeginPaint(hWnd,&ps);
            DrawDataWindow(hWnd,ps.hdc,data);
            EndPaint(hWnd,&ps);
            }
            return(0);
            break;
        
        case WM_TIMER:
            {
            HDC hFront=GetDC(hWnd);
            DrawDataWindow(hWnd,hFront,data);
            ReleaseDC(hWnd,hFront);
            }
            return(0);
            break;
            
        case WM_DESTROY:
            KillTimer(hWnd,1);
            break;
            
        default:
            break;
        }
    
    return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
} // end DataWndProc

LRESULT CALLBACK MainWndProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    // get my parameters
    USERDATA* data=reinterpret_cast<USERDATA*>(GetWindowLongPtr(hWnd,GWLP_USERDATA));
    
    switch(uMsg)
        {
        case WM_CREATE:
            HandleCreate(hWnd,uMsg,wParam,lParam);
            break;

        case WM_DESTROY:
            // replace with null
            SetWindowLongPtr(hWnd,GWLP_USERDATA,0);
            
            // cleanup
            DeleteObject(data->hTahoma);
            DeleteObject(data->hTahomaBig);
            data->sharenet.Stop();
            data->dstream.Stop();
            data->database.Stop();
            data->scripthost.Stop();
            data->scriptserver.Stop();
            data->ppi.Unwrap();
            
            // free this
            delete data;
            
            // bye!
            PostQuitMessage(0);
            break;

        case WM_TIMER:
        case RENDER_NOW:
            // redraw
            Render(data);
            break;

        case WM_KEYDOWN:
            HandleKeyDown(hWnd,wParam,lParam,data);
            break;

        case WM_MOUSEWHEEL:
            HandleMouseWheel(hWnd,wParam,lParam,data);
            break;
        
        case WM_COMMAND:
            HandleCommand(hWnd,uMsg,wParam,lParam,data);
            break;
            
        case WM_CLOSE:
            // let default handle it
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
            break;
            
        case WM_SIZING:
            // use utils function to set limits
            WM_SIZING_LIMIT(wParam,lParam,640,480);
            break;
            
        case WM_SIZE:
            // handle the size message
            HandleSize(hWnd,uMsg,wParam,lParam,data);
            break;
            
        default:
            return(::DefWindowProc(hWnd,uMsg,wParam,lParam));
        } // end switch message
    return(0);
} // end MainWndProc

void HandleCreate(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
    // size window so client area is 800x600
    RECT rClient;
    GetClientRect(hWnd,&rClient);
    LONG x=800-(rClient.right-rClient.left);
    LONG y=600-(rClient.bottom-rClient.top);
    RECT rWindow;
    GetWindowRect(hWnd,&rWindow);
    
    USERDATA* data=new USERDATA;
    
    // create font
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

    data->hTahoma=CreateFontIndirect(&lf);

    // create font
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

    data->hTahomaBig=CreateFontIndirect(&lf);
    
    // get text metrics for tahoma
    HDC hDC=GetDC(hWnd);
    HGDIOBJ hOldObj=SelectObject(hDC,data->hTahoma);
    GetTextMetrics(hDC,&data->TahomaTM);
    SelectObject(hDC,hOldObj);
    ReleaseDC(hWnd,hDC);
    
    // get text metrics for tahoma big
    hDC=GetDC(hWnd);
    hOldObj=SelectObject(hDC,data->hTahomaBig);
    GetTextMetrics(hDC,&data->TahomaBigTM);
    SelectObject(hDC,hOldObj);
    ReleaseDC(hWnd,hDC);
    
    // save as window parameter
    SetWindowLongPtr(hWnd,GWLP_USERDATA,reinterpret_cast<LONG_PTR>(data));
    
    // start components
    data->database.Go(&data->ToDatabaseFifo);
    data->sharenet.Go(&data->ToDatabaseFifo);
    data->dstream.Go(&data->ToDatabaseFifo);
    
    // this must be done before scripthost is started
    data->scripthost.ChangeDatabase(&data->database);
    
    std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>* fifopair;
    
    fifopair=new std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>;
    fifopair->first=&data->TelnetFifoReceive;
    fifopair->second=&data->TelnetFifoSend;
    data->scripthost.Go(fifopair); // CSLScriptHost expects a pair<fifo,fifo> as its go param
                           // first is its input, second is its output. It deletes the pair
                           // when it is done with it, but keeps the fifos (we own those)
    fifopair=new std::pair<tsfifo<std::string*>*,tsfifo<std::string*>*>;
    fifopair->first=&data->TelnetFifoSend;
    fifopair->second=&data->TelnetFifoReceive;
    data->scriptserver.Go(fifopair); // TelnetClientData expects a pair<fifo,fifo> as its go param
                             // first is its input, second is its output. It deletes the pair
                             // when it is done with it, but keeps the fifos (we own those)
    
    // install functors
    data->database.InstallFunctor
        (
        Database::DatabaseEvents::ActorCreated,
        std::auto_ptr<DatabaseFunctor>(new NewActorFunctor(*data))
        );
    
    data->database.InstallFunctor
        (
        Database::DatabaseEvents::ActorReassigned,
        std::auto_ptr<DatabaseFunctor>(new ReassignActorFunctor(*data))
        );

    data->database.InstallFunctor
        (
        Database::DatabaseEvents::MaintenanceIntervalDone,
        std::auto_ptr<DatabaseFunctor>(new MaintenanceIntervalDoneFunctor(*data))
        );
    
    data->database.InstallFunctor
        (
        Database::DatabaseEvents::MaintenanceUpdate,
        std::auto_ptr<DatabaseFunctor>(new MaintenanceUpdateFunctor(*data))
        );
    
    data->database.InstallFunctor
        (
        Database::DatabaseEvents::ActorDeleted,
        std::auto_ptr<DatabaseFunctor>(new DeleteActorFunctor(*data))
        );

    data->database.InstallFunctor
        (
        Database::DatabaseEvents::SharenetMessage,
        std::auto_ptr<DatabaseFunctor>(new SharenetMessageFunctor(*data))
        );
    
    data->database.InstallFunctor
        (
        Database::DatabaseEvents::DatabaseReset,
        std::auto_ptr<DatabaseFunctor>(new DatabaseResetFunctor(*data))
        );
    
    // set config
    data->database.SaveDAoCMessages(::RadarConfig.GetSaveDAoCMessages());
        
    // create child windows
    WNDCLASS wc;
    // register ppi window class
    ZeroMemory(&wc,sizeof(wc));
    wc.hbrBackground=(HBRUSH)(COLOR_WINDOW+1);
    wc.hCursor=LoadCursor(NULL,IDC_ARROW);
    wc.hIcon=NULL;
    wc.hInstance=(HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE);
    wc.lpfnWndProc=PPIWndProc;
    wc.lpszClassName="CheyennePPIChild";
    wc.lpszMenuName=NULL;
    wc.style=CS_DBLCLKS|CS_OWNDC;
    RegisterClass(&wc);

    // create ppi window
    HWND h;
    h=CreateWindowEx
        (
        0,
        "CheyennePPIChild",
        "Cheyenne PPI",
        WS_VISIBLE|WS_CHILD,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        hWnd,
        (HMENU)data->ppi_id,
        (HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE),
        data
        );

    LOG_FUNC << "created PPI window with handle 0x" << std::hex << h << std::dec << "\n";
    
    ShowWindow(h,SW_SHOW);
    UpdateWindow(h);
    
    // wrap the ppi
    data->ppi.Wrap
        (
        h,
        ::RadarConfig.GetUseZoneTextures(),
        ::RadarConfig.GetUseVectorMaps(),
        ::RadarConfig.GetSimplifyLines() ? ::RadarConfig.GetSimplifyLinesTolerance():0
        );

    // register data window class
    ZeroMemory(&wc,sizeof(wc));
    wc.hbrBackground=(HBRUSH)(COLOR_WINDOW+1);
    wc.hCursor=LoadCursor(NULL,IDC_ARROW);
    wc.hIcon=NULL;
    wc.hInstance=(HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE);
    wc.lpfnWndProc=DataWndProc;
    wc.lpszClassName="CheyenneDataChild";
    wc.lpszMenuName=NULL;
    wc.style=CS_DBLCLKS;
    RegisterClass(&wc);

    // create data window
    h=CreateWindowEx
        (
        0,
        "CheyenneDataChild",
        "Cheyenne Data",
        WS_VISIBLE|WS_CHILD|WS_BORDER,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        hWnd,
        (HMENU)data->data_id,
        (HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE),
        data
        );

    LOG_FUNC << "created Data window with handle 0x" << std::hex << h << std::dec << "\n";

    ShowWindow(h,SW_SHOW);
    UpdateWindow(h);
    
    // reposition main window
    SetWindowPos
        (
        hWnd,
        0,
        0,
        0,
        rWindow.right-rWindow.left + x,
        rWindow.bottom-rWindow.top + y,
        SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER
        );
        
    if(::RadarConfig.GetDStreamServer() != "<no server>")
        {
        // go ahead and connect automatically
        data->dstream.Open
            (
            ::RadarConfig.GetDStreamServer().c_str(),
            ::RadarConfig.GetDStreamServerPort()
            );
        }
    
    // done
    return;
} // end HandleCreate

void HandleSize(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam,USERDATA* data)
{
    // get new client area width and height
    LONG width=LOWORD(lParam);
    LONG height=HIWORD(lParam);

    // move data window
    RECT rData;
    rData.left=0;
    rData.right=200;
    rData.top=0;
    rData.bottom=height;
    SetWindowPos
        (
        GetDlgItem(hWnd,data->data_id),
        0,
        rData.left,
        rData.top,
        rData.right-rData.left,
        rData.bottom-rData.top,
        SWP_NOOWNERZORDER|SWP_NOZORDER
        );
    
    // move ppi
    RECT rPPI;
    rPPI.left=200;
    rPPI.right=width;
    rPPI.top=0;
    rPPI.bottom=height;
    SetWindowPos
        (
        GetDlgItem(hWnd,data->ppi_id),
        0,
        rPPI.left,
        rPPI.top,
        rPPI.right-rPPI.left,
        rPPI.bottom-rPPI.top,
        SWP_NOOWNERZORDER|SWP_NOZORDER
        );

    // tell ppi wrapper to resize
    data->ppi.Resize();
    
    // done
    return;
} // end HandleSize

void HandleCommand(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam,USERDATA* data)
{
    switch(LOWORD(wParam))
        {
        case ID_HELP_ABOUT:
            DialogBox(GetModuleHandle(NULL),MAKEINTRESOURCE(IDD_ABOUT),hWnd,(DLGPROC)AboutDialogProc);
            break;
            
        case ID_FILE_EXIT:
            DestroyWindow(hWnd);
            break;
            
        case ID_CONTROLDISPLAY_OPENCONFIGDIALOG:
            {
            HWND h=CreateDialogParam
                (
                (HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE),
                MAKEINTRESOURCE(IDD_CONFIG),
                hWnd,
                ::ConfigDialogProc,
                0
                );
            
            if(h && h!=INVALID_HANDLE_VALUE)
                {
                // disable this menu item
                // the dialog will re-enable the menu item when it exits
                EnableMenuItem(GetMenu(hWnd),ID_CONTROLDISPLAY_OPENCONFIGDIALOG,MF_BYCOMMAND|MF_GRAYED);
                
                ShowWindow(h,SW_SHOW);
                UpdateWindow(h);
                } // end if dialog was created
            }
            break;        
            
        case ID_NETWORKCONNECTIONS_DSTREAMSERVER:
            if(data->dstream.IsConnected())
                {
                int res=MessageBox(hWnd,"DStream is already connected\nAre you sure you want to disconnect?","Confirm Disconnect",MB_YESNO);
                if(res==IDYES)
                    {
                    data->dstream.Close();
                    }
                } // end if is connected
            else
                {
                // make connection
                std::pair<std::string,unsigned short> server;
                
                // populate with existing...
                server.first=::RadarConfig.GetDStreamServer();
                server.second=::RadarConfig.GetDStreamServerPort();
                
                // open dialog
                int res=DialogBoxParam
                    (
                    (HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE),
                    MAKEINTRESOURCE(IDD_CONNECTSERVER),
                    hWnd,
                    ::ConnectServerDialogProc,
                    (LPARAM)&server
                    );
                    
                if(res==IDOK)
                    {
                    // open connection
                    data->dstream.Open(server.first.c_str(),server.second);
                    
                    // save
                    ::RadarConfig.SetDStreamServer(server.first);
                    ::RadarConfig.SetDStreamServerPort(server.second);
                    }
                } // end else not connected
            break;
            
        case ID_NETWORKCONNECTIONS_SHARENETSERVER:
            if(data->sharenet.IsConnected())
                {
                int res=MessageBox(hWnd,"Sharenet is already connected\nAre you sure you want to disconnect?","Confirm Disconnect",MB_YESNO);
                if(res==IDYES)
                    {
                    data->sharenet.Close();
                    }
                } // end if is connected
            else
                {
                // make connection
                std::pair<std::string,unsigned short> server;
                
                // populate with existing...
                server.first=::RadarConfig.GetSharenetServer();
                server.second=::RadarConfig.GetSharenetServerPort();
                
                // open dialog
                int res=DialogBoxParam
                    (
                    (HINSTANCE)GetWindowLongPtr(hWnd,GWLP_HINSTANCE),
                    MAKEINTRESOURCE(IDD_CONNECTSERVER),
                    hWnd,
                    ::ConnectServerDialogProc,
                    (LPARAM)&server
                    );
                    
                if(res==IDOK)
                    {
                    // open connection
                    data->sharenet.Open(server.first.c_str(),server.second);
                    
                    // save
                    ::RadarConfig.SetSharenetServer(server.first);
                    ::RadarConfig.SetSharenetServerPort(server.second);
                    }
                } // end else not connected
            break;

        default:
            break;
        } // end switch
    // done
    return;
} // end HandleCommand

void HandleMouseWheel(HWND hWnd,WPARAM wParam,LPARAM lParam,USERDATA* data)
{
    short delta=GET_WHEEL_DELTA_WPARAM(wParam)/WHEEL_DELTA;
    short cnt;

    if(delta>0)
        {
        for(cnt=0;cnt<delta;++cnt)
            {
            data->ppi.ZoomIn();
            }
        }
    else
        {
        for(cnt=0;cnt>delta;--cnt)
            {
            data->ppi.ZoomOut();
            }
        }

    // done
    return;
} // end HandleMouseWheel

void HandleKeyDown(HWND hWnd,WPARAM wParam,LPARAM lParam,USERDATA* data)
{
    switch(wParam)
        {
        case VK_ADD:
        case '=':
            // zoom in
            data->ppi.ZoomIn();
            break;

        case VK_SUBTRACT:
        case '-':
            // zoom out
            data->ppi.ZoomOut();
            break;

        case VK_LEFT:
            data->ppi.PanLeft();
            break;
        
        case VK_RIGHT:
            data->ppi.PanRight();
            break;
        
        case VK_UP:
            data->ppi.PanUp();
            break;
        
        case VK_DOWN:
            data->ppi.PanDown();
            break;

        default:
            break;
        }
    // done
    return;
} // end HandleKeyDown

void DrawDataWindow(HWND hWnd,HDC hFront,USERDATA* data)
{
    // empty the data area
    RECT rClient;
    GetClientRect(hWnd,&rClient);
    std::ostringstream oss;
    const CheyenneTime CurrentTime(::Clock.Current());

    MapInfo::ZoneIndexType zone_number;
    unsigned int x,y;
    unsigned short z;
    MapInfo::ZoneInfo zone;

    if(data->ReferenceSet)
        {
        zone=::Zones.GetZoneFromGlobal
            (
            data->ReferenceActor.GetRegion(),
            (unsigned int)data->ReferenceActor.GetMotion().GetXPos(),
            (unsigned int)data->ReferenceActor.GetMotion().GetYPos(),
            (unsigned int)data->ReferenceActor.GetMotion().GetZPos(),
            x,
            y,
            z,
            zone_number
            );

        // put followed actor in
        oss << "Reference Actor:\n"
            << data->ReferenceActor.GetName() << " (" << zone.ZoneFile << ")\n"
            << "Hdg: " << ToDegrees(data->ReferenceActor.GetMotion().GetHeading()) << "°\n"
            << "Spd: " << data->ReferenceActor.GetMotion().GetSpeed() << "\n"
            << "l: " << (unsigned int)data->ReferenceActor.GetLevel() << " h: " << (unsigned int)data->ReferenceActor.GetHealth() << "\n"
            << "<" << x << ","
            << "" << y << ","
            << z << ">\n\n";
        }
        
    if(data->ReferenceTargetSet)
        {
        zone=::Zones.GetZoneFromGlobal
            (
            data->ReferenceTarget.GetRegion(),
            (unsigned int)data->ReferenceTarget.GetMotion().GetXPos(),
            (unsigned int)data->ReferenceTarget.GetMotion().GetYPos(),
            (unsigned int)data->ReferenceTarget.GetMotion().GetZPos(),
            x,
            y,
            z,
            zone_number
            );

        // put hooked actor in
        oss << "Target Actor:\n"
            << data->ReferenceTarget.GetName() << "\n"
            << "Hdg: " << ToDegrees(data->ReferenceTarget.GetMotion().GetHeading()) << "°\n"
            << "Spd: " << data->ReferenceTarget.GetMotion().GetSpeed() << "\n"
            << "l: " << (unsigned int)data->ReferenceTarget.GetLevel() << " h: " << (unsigned int)data->ReferenceTarget.GetHealth() << "\n"
            << "<" << x << ","
            << "" << y << ","
            << z << ">\n\n";
        }
        
    if(data->HookedSet)
        {
        zone=::Zones.GetZoneFromGlobal
            (
            data->HookedActor.GetRegion(),
            (unsigned int)data->HookedActor.GetMotion().GetXPos(),
            (unsigned int)data->HookedActor.GetMotion().GetYPos(),
            (unsigned int)data->HookedActor.GetMotion().GetZPos(),
            x,
            y,
            z,
            zone_number
            );

        // put hooked actor in
        oss << "Hooked Actor:\n"
            << data->HookedActor.GetName() << "\n"
            << "Hdg: " << ToDegrees(data->HookedActor.GetMotion().GetHeading()) << "°\n"
            << "Spd: " << data->HookedActor.GetMotion().GetSpeed() << "\n"
            << "l: " << (unsigned int)data->HookedActor.GetLevel() << " h: " << (unsigned int)data->HookedActor.GetHealth() << "\n"
            << "<" << x << ","
            << "" << y << ","
            << z << ">\n"
            << "Type=" << data->HookedActor.GetActorType() << "\n";
        }
        
    DatabaseStatistics stats;
    data->database.GetDatabaseStatistics(stats);
    
    oss << "Global Database Statistics:\n"
        << "Current Time=" << CurrentTime.Seconds() << "\n"
        << stats.GetLiveAlbs() << " Live Albs (" << stats.GetNumAlbs() << " total)\n"
        << stats.GetLiveHibs() << " Live Hibs (" << stats.GetNumHibs() << " total)\n"
        << stats.GetLiveMids() << " Live Mids (" << stats.GetNumMids() << " total)\n"
        << stats.GetLiveMobs() << " Live MOBs (" << stats.GetNumMobs() << " total)\n\n"
        << "ShareNet Status: " << data->sharenet.GetStatusString() << "\n"
        << "ShareNet=" << data->sharenet.GetRemoteAddr() << "\n\n"
        << "DStream Status: " << data->dstream.GetStatusString() << "\n"
        << "DStream=" << data->dstream.GetRemoteAddr() << "\n\n"
        << "Scriptserver mini-server status: " << data->scriptserver.GetStatusString() << "\n";
        
    // make running scripts string       
    std::list<std::string> running_scripts;
    std::list<std::string>::const_iterator it;
    data->scripthost.GetRunningScripts(running_scripts);
    
    oss << unsigned int(running_scripts.size()) << " Running Scripts\n";
    for(it=running_scripts.begin();it!=running_scripts.end();++it)
        {
        oss << *it << "\n";
        } // end for all running scripts

    // draw double buffered to prevent flickering
    HDC hBack=CreateCompatibleDC(hFront);
    HBITMAP hBmp=CreateCompatibleBitmap(hBack,rClient.right-rClient.left,rClient.bottom-rClient.top);
    
    // set bitmap
    HGDIOBJ hOldBmp=SelectObject(hBack,hBmp);

    // set font
    HGDIOBJ hOldFont=SelectObject(hBack,data->hTahomaBig);

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

void HandleLButtonDoubleClick(HWND hWnd,WPARAM wParam,LPARAM lParam,USERDATA* data)
{
    const float MouseX=LOWORD(lParam);
    const float MouseY=HIWORD(lParam);
   
    // look at all actors
    ClosestActorFinder finder(data->database.IterateActors(ClosestActorFinder(*data,MouseX,MouseY)));
    
    // see if we were close enough to "hook" one
    // GetDist() returns pixels
    if(finder.GetDist() < 15.0f)
        {
        // found it, make it the selected actor
        data->HookedActor=finder.GetClosestActor();
        data->HookedSet=true;
        }
    else
        {
        // no hook
        data->HookedSet=false;
        }
    
    // done
    return;
} // end HandleLButtonDoubleClick

void Render(USERDATA* data)
{
    if(data->ReferenceSet)
        {
        // refresh the position of our reference actor
        data->ReferenceActor=data->database.CopyActorById(data->ReferenceActor.GetInfoId());
        
        // and center the ppi on it
        data->ppi.CenterOn(data->ReferenceActor);
        }
    
    if(data->HookedSet)
        {
        // refresh the position of our hooked actor
        data->HookedActor=data->database.CopyActorById(data->HookedActor.GetInfoId());
        }
    
    if(data->ReferenceTargetSet)
        {
        // refresh the position of our reference actor's target
        data->ReferenceTarget=data->database.CopyActorById(data->ReferenceTarget.GetInfoId());
        }

    // set rendering flag and begin
    data->Rendering=true;
    data->ppi.RenderBegin();
    
    // do all the zones -- ppi will only render the ones that are
    // on-screen
    data->ppi.RenderAllZones();
    
    // for the newer stealth mask-based UncorrelatedStealthers member
    // of the database: 
    // we need to iterate over all stealthers the database holds
    // in the map, center the drawing on each stealthers average <x,y,z>
    // and draw the enabled bits in the stealth mask offset by the mask's
    // <x,y> offset
    data->database.IterateUncorrelatedStealthers(UncorrelatedStealthRenderFunctor(*data));
    
    /*
    // if uncorrelated stealth is present, draw that
    if(data->database.IsUncorrelatedStealth())
        {
        data->ppi.RenderUncorrelatedStealth(data->database.GetUncorrelatedStealthCenter());
        }
    */
    
    if(::RadarConfig.GetUpdateActorsOnRender())
        {
        // update and iterate all actors -- ppi will only render the ones
        // that are on-screen
        data->database.UpdateAndIterateActors(ActorRenderFunctor(*data));
        }
    else
        {
        // just iterate all actors -- ppi will only render the ones
        // that are on-screen
        data->database.IterateActors(ActorRenderFunctor(*data));
        }
        
    // render pairing lines
    if(data->ReferenceSet && data->ReferenceTargetSet)
        {
        data->ppi.RenderPairingLine
            (
            data->ReferenceActor,
            data->ReferenceTarget,
            1.0f,
            0.0f,
            0.0f
            );
        }
    if(data->ReferenceSet && data->HookedSet)
        {
        data->ppi.RenderPairingLine
            (
            data->ReferenceActor,
            data->HookedActor,
            0.0f,
            1.0f,
            0.0f
            );
        }
    
    // end rendering and clear flag
    data->ppi.RenderEnd();
    data->Rendering=false;
    
    // done
    return;
} // end Render