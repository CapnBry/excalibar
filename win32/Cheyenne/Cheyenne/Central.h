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

class Central
{
friend class ActorRenderFunctor;
friend class MaintenanceUpdateFunctor;
friend class NewActorFunctor;
friend class DeleteActorFunctor;
friend class ReassignActorFunctor;
friend class MaintenanceIntervalDoneFunctor;
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

    const int FontListBase;
    const int NumFontLists;
    const float XLimit;
    const float YLimit;
    const unsigned int NumVectorMapLists;

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

    void DrawGLUTFontString(const std::string& text,void* font=GLUT_BITMAP_HELVETICA_10)const
    {
        std::string::const_iterator it;
        for(it=text.begin();it!=text.end();++it)
            {
            glutBitmapCharacter(font,int(*it));
            }
    };


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
    float FollowedActorHeadingDegrees;

    typedef std::map<unsigned char,unsigned int> TextureMapType;
    typedef TextureMapType::iterator TextureMapIteratorType;
    typedef TextureMapType::const_iterator TextureMapConstIteratorType;
    typedef TextureMapType::value_type TextureMapValueType;
    TextureMapType TextureMap;

    /*typedef std::map<unsigned short,int> ListDataType;
    typedef ListDataType::iterator ListDataIteratorType;
    typedef ListDataType::value_type ListDataValueType;
    ListDataType ListData;*/
    TEXTMETRIC TahomaTextMetric;

    // database statistics
    DatabaseStatistics stats;

    mutable MutexLock CentralMutex;

}; // end Central

#pragma warning(pop)

#endif // CENTRAL_H