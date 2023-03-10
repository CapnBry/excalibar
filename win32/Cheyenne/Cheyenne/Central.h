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
#pragma once

#include "global.h"
#include <map>
#include <windows.h>
#include <gl\gl.h>
#include <gl\glu.h>
#include "database.h"
#include "VectorMapLoader.h"
#include "soundspool.h"
#include "sharenet.h"
#include "..\..\Common\MobsightParse\MobsightParseAPI.h" // for mobfinder stuff
#include "..\..\Common\PacketStore\PacketStore.h" // include for offline packet storage
#include "glfont2.h" // include for text engine

//typedef Sniffer SNIFFER_T; // use this for 'normal' sniffing operation
typedef PktLoadSniffer<PacketStore<std::ifstream> > SNIFFER_T; // use this for loading saved packet files

class Central
{
friend class ActorRenderFunctor;
friend class MaintenanceUpdateFunctor;
friend class NewActorFunctor;
friend class DeleteActorFunctor;
friend class ReassignActorFunctor;
friend class MaintenanceIntervalDoneFunctor;
friend class SharenetMessageFunctor;
friend class VectorMapItem;
public:
    Central();
    virtual ~Central();

    WPARAM Go(HINSTANCE hInst);

    inline void AdjustPositionByRegion(Motion& Position,const unsigned char Region)const
    {
        Position.SetXPos(Position.GetXPos() + float(Zones.GetLimitsFromRegion(Region).XOffset));
        Position.SetYPos(Position.GetYPos() + float(Zones.GetLimitsFromRegion(Region).YOffset));
    }
    inline void GetRenderPosition(const Actor& ThisActor,Motion& Position)const
    {
        Position=ThisActor.GetMotion();
        AdjustPositionByRegion(Position,ThisActor.GetRegion());
    }

    inline bool IsZoneVisible(int BaseX, int BaseY, int MaxX, int MaxY)const
    {
        // check to see if zone is overlapping with screen
        // we can use the Windows GDI call IntersectRect to
        // determine this
        
        RECT rZone={BaseX,BaseY,MaxX,MaxY};
        RECT rDisplay={(LONG)ProjectionX,(LONG)ProjectionY,(LONG)(ProjectionX+ProjectionWidthX),(LONG)(ProjectionY+ProjectionWidthY)};
        RECT rIntersect;

        return(IntersectRect(&rIntersect,&rZone,&rDisplay) ? true:false);
    } // end IsZoneVisible

    static const int FontListBase;
    const int NumFontLists;
    const float XLimit;
    const float YLimit;
    const float ActorXScale;
    const float ActorYScale;
    const float GroundTargetXScale;
    const float GroundTargetYScale;
    const unsigned int NumVectorMapLists;
    const std::string WindowPosFileName;

    enum ConAssociations
    {
    alb_gray,
    alb_green,
    alb_blue,
    alb_yellow,
    alb_orange,
    alb_red,
    alb_purple,
    hib_gray,
    hib_green,
    hib_blue,
    hib_yellow,
    hib_orange,
    hib_red,
    hib_purple,
    mid_gray,
    mid_green,
    mid_blue,
    mid_yellow,
    mid_orange,
    mid_red,
    mid_purple,
    mob_gray,
    mob_green,
    mob_blue,
    mob_yellow,
    mob_orange,
    mob_red,
    mob_purple,
    generic_alb,
    generic_hib,
    generic_mid,
    generic_mob
    };
    
    enum GeneralAssociations
    {
    ground_target,
    font_texture
    };

    typedef std::map<unsigned char,unsigned int> ZoneTextureMapType;
    typedef ZoneTextureMapType::iterator ZoneTextureMapIteratorType;
    typedef ZoneTextureMapType::const_iterator ZoneTextureMapConstIteratorType;
    typedef ZoneTextureMapType::value_type ZoneTextureMapValueType;

    typedef std::map<ConAssociations,unsigned int> ConTextureMapType;
    typedef ConTextureMapType::iterator ConTextureMapIteratorType;
    typedef ConTextureMapType::const_iterator ConTextureMapConstIteratorType;
    typedef ConTextureMapType::value_type ConTextureMapValueType;

    typedef std::map<GeneralAssociations,unsigned int> GeneralTextureMapType;
    typedef GeneralTextureMapType::iterator GeneralTextureMapIteratorType;
    typedef GeneralTextureMapType::const_iterator GeneralTextureMapConstIteratorType;
    typedef GeneralTextureMapType::value_type GeneralTextureMapValueType;
    
    class TargetPair
    {
    public:
        TargetPair()
        {
            SetValid(false);
        };
        TargetPair(const TargetPair& s){set(s);};
        ~TargetPair(){};
        TargetPair& operator=(const TargetPair& s)
        {
            if(this != &s)
                {
                set(s);
                }
            return(*this);
        }
    
    protected:
    private:
        void set(const TargetPair& s)
        {
        MEMBER_ASSIGN(ThisActor);
        MEMBER_ASSIGN(Target);
        MEMBER_ASSIGN(Valid);
        }

        DECL_MEMBER(Actor,ThisActor);
        DECL_MEMBER(Actor,Target);
        DECL_MEMBER(bool,Valid);
    }; // end class TargetPair

protected:

private:
    bool Init(void);
    void InitWindowPosition(RECT& r)const;
    void StoreWindowPosition(void)const;
    static BOOL CALLBACK MonitorEnumProc(HMONITOR hMonitor,HDC hdcMonitor,LPRECT lprcMonitor,LPARAM dwData);
    
    Database& GetDatabase(void){return(db);};
    const Database& GetDatabase(void)const{return(db);};

    void InitActorEvents(void);
    void OnMaintenanceUpdate(const Actor& ThisActor);
    void OnNewActor(const Actor& ThisActor);
    void OnDeleteActor(const Actor& ThisActor);
    void OnReassignActor(const Actor& ThisActor);
    void OnMaintenanceIntervalDone(void);
    void OnSharenetMessage(const void* p,const unsigned int len);
    
    void OpenShareNet(void);

    static LRESULT CALLBACK WindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
    void HandleMainWindowSizing(RECT* r);
    void HandleMainWindowSize(void);
    void HandleCommand(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
    void HandleDoubleClick(float x,float y);
    static BOOL CALLBACK SizeChildProc(HWND hChild,LPARAM lParam);

    static LRESULT CALLBACK PPIWindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);

    static LRESULT CALLBACK DataWindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
    void DrawDataWindow(HDC hFront)const;

    void DrawCircle(const float radius)const
    {
        // draw it
        glPushMatrix();

        // scale
        glScalef(radius*0.01f,radius*0.01f,radius*0.01f);
        // draw
        glCallList(CircleList);

        glPopMatrix();
    }

    void DrawDisc(const float radius)const
    {
        // draw it
        glPushMatrix();

        // scale
        glScalef(radius*0.01f,radius*0.01f,radius*0.01f);
        // draw
        glCallList(DiscList);

        glPopMatrix();
    }

    void InitCheyenne(void);
    void InitOpenGL(void);
    void InitPixelFormat(void);
    void InitDisplayLists(void);
    void DestroyDisplayLists(void);
    void InitTextures(void);
    void DestroyTextures(void);
    void LoadVectorMaps(void);
    
    void SetCamera(void)const;
    void InitDisplayMatrices(void)const;
    void DrawPPI(void);
    void RenderActor(const Actor& ThisActor)const;
    void RenderWorld(void)const;
    void RenderCloseControl(void)const;
    void RenderUncorrelatedStealth(void)const;
    void RenderMobfinderResults(void)const;
    void HandleKeyDown(HWND hWnd,WPARAM wParam,LPARAM lParam);
    
    void RecenterDisplay(void);
    void ZoomIn(void);
    void ZoomOut(void);
    void PanRight(void);
    void PanLeft(void);
    void PanDown(void);
    void PanUp(void);
    void RecalcIncrements(void)
    {
        AutoLock al(CentralMutex);

        float ProjWidth=0.5f*(ProjectionWidthX + ProjectionWidthY);

        float NewInc=0.10202040816326530612244897959184f*ProjWidth -101.0f;
        // y = mx + b =)

        // clamp at 1.0
        // and at 5000.0
        if(NewInc < 1.0f)
            {
            ZoomIncrement=1.0f;
            PanIncrement=1.0f;
            }
        else if(NewInc > 5000.0f)
            {
            ZoomIncrement=5000.0f;
            PanIncrement=5000.0f;
            }
        else
            {
            ZoomIncrement=NewInc;
            PanIncrement=NewInc;
            }
        
        // done
        return;
    }

    inline ConTextureMapValueType::second_type GetTexture(const ConAssociations& con)const
    {
        ConTextureMapConstIteratorType it=ConTextureMap.find(con);

        return((it != ConTextureMap.end()) ? it->second : 0);
    };

    inline GeneralTextureMapValueType::second_type GetTexture(const GeneralAssociations& assoc)const
    {
        GeneralTextureMapConstIteratorType it=GeneralTextureMap.find(assoc);

        return((it != GeneralTextureMap.end()) ? it->second : 0);
    };

    ConTextureMapValueType::second_type GetConTexture(const Actor& ThisActor,bool bSetColor=false) const;
    
    tsfifo<CheyenneMessage*> MessageInputFifo;

    SNIFFER_T sniffer;
    Database db;
    HINSTANCE hInstance;
    HWND hMainWnd;
    HWND hDataWnd;
    HWND hPPIWnd;
    HFONT hTahoma;
    HFONT hTahomaBig;
    HGLRC hRenderContext;
    HDC hPPIDC;
    RECT rPPIClient;
    TEXTMETRIC TahomaTextMetric;
    const UINT DataWindowTimerId;
    bool bDisplayListsCreated;
    bool bTexturesCreated;
    float ProjectionX;
    float ProjectionY;
    float ProjectionWidthX;
    float ProjectionWidthY;
    float ActorVertexX;
    float ActorVertexY;
    float GroundTargetVertexX;
    float GroundTargetVertexY;
    float ZoomIncrement;
    float PanIncrement;
    unsigned int VectorMapListBase;
    unsigned int CircleList;
    unsigned int DiscList;

    unsigned int IDToFollow;
    mutable unsigned int HookedActor;     // this can be modified from RenderActor() -- which
                                          // is "conceptually const", but needs to change this
                                          // variable

    mutable unsigned char IDToFollowZone; // this can be modified from DrawDataWindow() -- which
                                          // is "conceptually const", but needs to change this
                                          // variable
    mutable unsigned char IDToFollowLevel;// this can be modified from DrawDataWindow() -- which
                                          // is "conceptually const", but needs to change this
                                          // variable
    mutable unsigned char IDToFollowRealm;// this can be modified from DrawDataWindow() -- which
                                          // is "conceptually const", but needs to change this
                                          // variable

    mutable TargetPair FollowedTargetPair; // followed actor position, followed actor's target position
                                          // this can be modified from RenderActor() -- which
                                          // is "conceptually const", but needs to change this
                                          // variable

    float FollowedActorHeadingDegrees;

    ZoneTextureMapType ZoneTextureMap;
    ConTextureMapType ConTextureMap;
    GeneralTextureMapType GeneralTextureMap;
    mutable ConTextureMapValueType::second_type LastActorConTexture; // "conceptually const" for render actors function

    // database statistics
    DatabaseStatistics stats;

    mutable MutexLock CentralMutex;

    // vector map loader
    VectorMapLoader VmLoader;

    // sound spooler
    SoundSpool Sounds;
    
    // sharenet client
    ShareNetClientData ShareNet;
    
    // mobfinder results
    NarrowIdDef MobfinderResults;
    
    // framerate storage, these are conceptually const
    mutable unsigned long Frames;
    mutable CheyenneTime FrameMeasureStart;
    
    // text rendering engine
    //glfont::GLFont TextEngine;

}; // end Central
