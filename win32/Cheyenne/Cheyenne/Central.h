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
#ifndef CENTRAL_H
#define CENTRAL_H

#pragma once

#pragma warning(push)

// get rid of the stupid
// "identifier truncated" warnings
#pragma warning(disable : 4786)

#include "global.h"
#include <map>
#include <windows.h>
#include <gl\gl.h>
#include <gl\glu.h>
#include <gl\glut.h>
#include "database.h"
#include "VectorMapLoader.h"

// predefine this

class Central
{
friend class ActorRenderFunctor;
friend class MaintenanceUpdateFunctor;
friend class NewActorFunctor;
friend class DeleteActorFunctor;
friend class ReassignActorFunctor;
friend class MaintenanceIntervalDoneFunctor;
friend class VectorMapItem;
public:
    Central();
    virtual ~Central();

    WPARAM Go(HINSTANCE hInst);

    inline void GetRenderPosition(const Actor& ThisActor,Motion& Position)const
    {
        Position=ThisActor.GetMotion();
        Position.SetXPos(Position.GetXPos() + float(Zones.GetLimitsFromRegion(ThisActor.GetRegion()).XOffset));
        Position.SetYPos(Position.GetYPos() + float(Zones.GetLimitsFromRegion(ThisActor.GetRegion()).YOffset));
    }

    inline bool IsZoneVisible(int BaseX, int BaseY, int MaxX, int MaxY)const
    {
        // check to see if zone is overlapping with screen
        // we can use the Windows GDI call IntersectRect to
        // determine this
        
        RECT rZone={BaseX,BaseY,MaxX,MaxY};
        RECT rDisplay={ProjectionX,ProjectionY,ProjectionX+ProjectionWidthX,ProjectionY+ProjectionWidthY};
        RECT rIntersect;

        return(IntersectRect(&rIntersect,&rZone,&rDisplay) ? true:false);
    } // end IsZoneVisible

    static const int FontListBase;
    const int NumFontLists;
    const float XLimit;
    const float YLimit;
    const unsigned int NumVectorMapLists;

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

    typedef std::map<unsigned char,unsigned int> ZoneTextureMapType;
    typedef ZoneTextureMapType::iterator ZoneTextureMapIteratorType;
    typedef ZoneTextureMapType::const_iterator ZoneTextureMapConstIteratorType;
    typedef ZoneTextureMapType::value_type ZoneTextureMapValueType;

    typedef std::map<ConAssociations,unsigned int> ConTextureMapType;
    typedef ConTextureMapType::iterator ConTextureMapIteratorType;
    typedef ConTextureMapType::const_iterator ConTextureMapConstIteratorType;
    typedef ConTextureMapType::value_type ConTextureMapValueType;

protected:

private:
    bool Init(void);
    
    Database& GetDatabase(void){return(db);};
    const Database& GetDatabase(void)const{return(db);};

    void InitActorEvents(void);
    void OnMaintenanceUpdate(const Actor& ThisActor);
    void OnNewActor(const Actor& ThisActor);
    void OnDeleteActor(const Actor& ThisActor);
    void OnReassignActor(const Actor& ThisActor);
    void OnMaintenanceIntervalDone(void);

    static LRESULT CALLBACK WindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
    void HandleMainWindowSizing(RECT* r);
    void HandleMainWindowSize(void);
    void HandleCommand(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
    void HandleDoubleClick(float x,float y);
    static BOOL CALLBACK SizeChildProc(HWND hChild,LPARAM lParam);

    static LRESULT CALLBACK PPIWindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);

    static LRESULT CALLBACK DataWindowProc(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam);
    void DrawDataWindow(HDC hFront)const;

    void DrawCircle(float radius)const
    {
        // draw it
        glPushMatrix();

        // scale
        glScalef(radius*0.01f,radius*0.01f,radius*0.01f);
        // draw
        glCallList(CircleList);

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

        if(it != ConTextureMap.end())
            {
            return(it->second);
            }
        else
            {
            return(0);
            }
    };

    inline ConTextureMapValueType::second_type GetConTexture(const Actor& ThisActor,bool bSetColor=false) const
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
    tsfifo<CheyenneMessage*> MessageInputFifo;

    Sniffer sniffer;
    Database db;
    HINSTANCE hInstance;
    HWND hMainWnd;
    HWND hDataWnd;
    HWND hPPIWnd;
    HFONT hTahoma;
    HFONT hTahomaBig;
    HGLRC hRenderContext;
    HDC hPPIDC;
    const UINT DataWindowTimerId;
    bool bDisplayListsCreated;
    bool bTexturesCreated;
    float ProjectionX;
    float ProjectionY;
    float ProjectionWidthX;
    float ProjectionWidthY;
    float ActorVertexX;
    float ActorVertexY;
    float ZoomIncrement;
    float PanIncrement;
    unsigned int VectorMapListBase;
    unsigned int CircleList;

    unsigned int IDToFollow;
    unsigned int HookedActor;
    mutable unsigned char IDToFollowZone; // this can be modified from DrawDataWindow() -- which
                                          // is "conceptually const", but needs to change this
                                          // variable
    mutable unsigned char IDToFollowLevel;// this can be modified from DrawDataWindow() -- which
                                          // is "conceptually const", but needs to change this
                                          // variable
    mutable unsigned char IDToFollowRealm;// this can be modified from DrawDataWindow() -- which
                                          // is "conceptually const", but needs to change this
                                          // variable
    float FollowedActorHeadingDegrees;

    ZoneTextureMapType ZoneTextureMap;

    ConTextureMapType ConTextureMap;

    /*typedef std::map<unsigned short,int> ListDataType;
    typedef ListDataType::iterator ListDataIteratorType;
    typedef ListDataType::value_type ListDataValueType;
    ListDataType ListData;*/
    TEXTMETRIC TahomaTextMetric;

    // database statistics
    DatabaseStatistics stats;

    mutable MutexLock CentralMutex;

    // vector map loader
    VectorMapLoader VmLoader;

}; // end Central

#pragma warning(pop)

#endif // CENTRAL_H