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

// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h> // windows main header files
#include <map> // for std::map
#include <gl\gl.h> // opengl
#include <gl\glu.h> // opengl utilities
#include "..\Database\Actor.h" // for actor and motion definitions
#include "..\Utils\VectorMapLoader.h" // for vector map loader definition
#include "..\Utils\lfontrenderer.h" // for font rendering class (see that header file for
                                    // info: that code is (c) 2002 Lev Povalahev)
#include "..\Utils\Mapinfo.h" // for the map info

class GLRenderState
{
public:
    typedef std::map<unsigned int,bool> StateFlagType;
    typedef StateFlagType::const_iterator StateFlagConstIterator;
    typedef StateFlagType::iterator StateFlagIterator;
    typedef StateFlagType::value_type StateFlagValue;
    
    GLRenderState(){InitFlags();};
    GLRenderState(const GLRenderState& s){set(s);};
    virtual ~GLRenderState(){};
    
    GLRenderState& operator=(const GLRenderState& s)
    {
        set(s);
    }

    void Enable(const unsigned int flag)
    {
        StateFlagIterator it=StateFlags.find(flag);
        
        #ifdef _DEBUG
        if(it == StateFlags.end())
            {
            throw(std::logic_error("[enable] invalid state flag"));
            }
        #endif

        if(it->second == false)
            {
            // set to true and enable
            it->second=true;
            glEnable(flag);
            }
        // done
        return;
    } // end Enable
    
    void Disable(const unsigned int flag)
    {
        StateFlagIterator it=StateFlags.find(flag);
        
        #ifdef _DEBUG
        if(it == StateFlags.end())
            {
            throw(std::logic_error("[disable] invalid state flag"));
            }
        #endif
        
        if(it->second == true)
            {
            // set to false and disable
            it->second=true;
            glDisable(flag);
            }
        // done
        return;
    } // end Disable
    
    void MakeAllCurrent(void)const
    {
        StateFlagConstIterator it;
        
        for(it=StateFlags.begin();it!=StateFlags.end();++it)
            {
            if(it->second)
                {
                glEnable(it->first);
                }
            else
                {
                glDisable(it->first);
                }
            }
    } // end MakeAllCurrent

protected:
private:
    void InitFlags(void)
    {
        StateFlags.insert(StateFlagValue(GL_CULL_FACE,true)); // default to true
        StateFlags.insert(StateFlagValue(GL_DEPTH_TEST,false)); // default to false
        StateFlags.insert(StateFlagValue(GL_TEXTURE_2D,true)); // default to true
        StateFlags.insert(StateFlagValue(GL_BLEND,false)); // default to false
    } // end InitFlags
    
    void set(const GLRenderState& s)
    {
        // set
        StateFlags=s.StateFlags;
    } // end set
    
    StateFlagType StateFlags; // the render state flags
}; // end class GLRenderState


class ActorRenderPrefs
{
public:
    ActorRenderPrefs()
    {
        m_RenderName=true;
        m_RenderSurname=true;
        m_RenderGuild=true;
        m_RenderHealth=true;
        m_RenderLevel=true;
        m_NumSet=5;
    }
    ActorRenderPrefs(const ActorRenderPrefs& s){set(s);};
    ~ActorRenderPrefs(){};
    
    ActorRenderPrefs& operator=(const ActorRenderPrefs& s){set(s);return(*this);};

    void SetRenderName(const bool b){BookKeepSet(b,ModifyRenderName());};
    void SetRenderSurname(const bool b){BookKeepSet(b,ModifyRenderSurname());};
    void SetRenderGuild(const bool b){BookKeepSet(b,ModifyRenderGuild());};
    void SetRenderHealth(const bool b){BookKeepSet(b,ModifyRenderHealth());};
    void SetRenderLevel(const bool b){BookKeepSet(b,ModifyRenderLevel());};
    
protected:

private:
    void BookKeepSet(const bool to,bool& b)
        {
        if(to)
            {
            if(!b)
                {
                ModifyNumSet()++;
                }
            }
        else
            {
            if(b)
                {
                ModifyNumSet()--;
                }
            }
        }

    void set(const ActorRenderPrefs& s)
        {
        MEMBER_ASSIGN(RenderName);
        MEMBER_ASSIGN(RenderSurname);
        MEMBER_ASSIGN(RenderGuild);
        MEMBER_ASSIGN(RenderHealth);
        MEMBER_ASSIGN(RenderLevel);
        }
    
    DECL_MEMBER_ACCESSOR_NOSET(bool,RenderName);
    DECL_MEMBER_ACCESSOR_NOSET(bool,RenderSurname);
    DECL_MEMBER_ACCESSOR_NOSET(bool,RenderGuild);
    DECL_MEMBER_ACCESSOR_NOSET(bool,RenderHealth);
    DECL_MEMBER_ACCESSOR_NOSET(bool,RenderLevel);
    DECL_MEMBER_ACCESSOR_NOSET(int,NumSet);
}; // end ActorRenderPrefs
class GLPPI
{
friend class VmTextProxy;
public:
    GLPPI();
    virtual ~GLPPI();
    
    enum TextureId
            {
            invalid_texture_id=0,
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
            generic_mob,
            ground_target
            };
    
    // typedefs for mapping from an identifier to a texture map
    typedef std::map<MapInfo::ZoneIndexType,GLuint> ZoneTextureMapType;
    typedef ZoneTextureMapType::iterator ZoneTextureMapIteratorType;
    typedef ZoneTextureMapType::const_iterator ZoneTextureMapConstIteratorType;
    typedef ZoneTextureMapType::value_type ZoneTextureMapValueType;
    typedef ZoneTextureMapType::value_type::second_type ZoneTextureIdType;

    typedef std::map<TextureId,GLuint> TextureMapType;
    typedef TextureMapType::iterator TextureMapIteratorType;
    typedef TextureMapType::const_iterator TextureMapConstIteratorType;
    typedef TextureMapType::value_type TextureMapValueType;
    typedef TextureMapType::value_type::second_type GLTextureIdType;
    
    void Wrap(HWND Window); // call one time -- must be first call to this class
    void Unwrap(void); // call one time -- must be last call to this class
    void Resize(void); // call when the wrapped window size changes

    // rendering functions
    void RenderBegin(void);
        void RenderActor
            (
            const Actor& ThisActor,
            const GLPPI::TextureId& TexId,
            const ActorRenderPrefs& Prefs,
            const Actor::RelativeCon& ConColor
            );
        void RenderVectorMap(const VectorMap& Map);
        void RenderZone
            (
            const MapInfo::ZoneInfo& zone,
            const int BaseX,
            const int BaseY,
            const int MaxX,
            const int MaxY
            );
        void RenderAllZones(void);
    void RenderEnd(void);

    // zoom/pan api
    void GLPPI::ZoomIn(void);
    void GLPPI::ZoomOut(void);
    void GLPPI::PanRight(void);
    void GLPPI::PanLeft(void);
    void GLPPI::PanDown(void);
    void GLPPI::PanUp(void);
    
protected:
private:
    GLPPI(const GLPPI& s); // disallow
    GLPPI& operator=(const GLPPI& s); // disallow
    
    // init/cleanup functions
    void InitPixelFormat(void);
    void InitRenderContext(void);
    void InitTextures(void);
    void DestroyTextures(void);
    void InitDisplayLists(void);
    void DestroyDisplayLists(void);
    void InitFonts(void);
    void DestroyFonts(void);
    
    // helper functions for display lists
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

    // helper functions for determining where to draw positions
    void AdjustPositionByRegion(Motion& Position,const unsigned char Region)const;
    void GetRenderPosition(const Actor& ThisActor,Motion& Position)const;
    bool IsVisible(int BaseX, int BaseY, int MaxX, int MaxY)const;
    bool IsVisible(const float x, const float y)const;
    void GetBoundingRectangle(const MapInfo::ZoneInfo& zone,int& BaseX,int& BaseY,int& MaxX,int& MaxY)const;
    bool IsZoneVisible(const MapInfo::ZoneInfo& zone)const;
    void RecalcIncrements(void);
    void InitDisplayMatrices(void)const;
    bool GetScreenPosition
        (
        GLdouble ObjX,
        GLdouble ObjY,
        GLdouble ObjZ,
        GLdouble* ScreenX,
        GLdouble* ScreenY,
        GLdouble* ScreenZ
        )const;
    
    // texture helpers
    bool BindTexture(const TextureId& id)const
    {
        TextureMapConstIteratorType it=TextureMap.find(id);

        if(it!=TextureMap.end())
            {
            if(LastBoundTexture!=it->second)
                {
                glBindTexture(GL_TEXTURE_2D,it->second);
                LastBoundTexture=it->second;
                }
            return(true);
            }
        else
            {
            glBindTexture(GL_TEXTURE_2D,GLPPI::invalid_texture_id);
            LastBoundTexture=GLPPI::invalid_texture_id;
            return(false);
            }
    } // end BindTexture
    
    bool BindTexture(const MapInfo::ZoneIndexType& id)const
    {
        ZoneTextureMapConstIteratorType it=ZoneTextureMap.find(id);
        
        if(it!=ZoneTextureMap.end())
            {
            glBindTexture(GL_TEXTURE_2D,it->second);
            return(true);
            }
        else
            {
            glBindTexture(GL_TEXTURE_2D,GLPPI::invalid_texture_id);
            return(false);
            }
    }
    
    // color helpers
    void GetConColor
        (
        const Actor::RelativeCon& ConColor,
        GLfloat& r,
        GLfloat& g,
        GLfloat& b
        )const;
    
    // private data
    bool Wrapped; // flag: wrapped or not
    HWND WrappedWindow; // window that we wrapped
    RECT rClient; // client rect of window that we wrapped
    HGLRC RenderContext; // render context in WrappedDC
    HDC WrappedDC; // device context that we wrapped
    mutable GLuint LastBoundTexture; // last texture we bound from TextureMap
    ZoneTextureMapType ZoneTextureMap; // for zone# to texture mapping
    TextureMapType TextureMap; // for TextureId to texture mapping
    GLRenderState RenderState; // the render state
    lfont::LFontRenderer TextEngine; // the text engine, thanks to Lev Povalahev
    VectorMapLoader VmLoader; // the vector map loader
    
    // display lists
    GLuint CircleList; // display list for a circle
    GLuint DiscList; // display list for a disc
    
    // display variables
    float ProjectionX; // left side of projection area
    float ProjectionY; // top of projection area
    float ProjectionWidthX; // X width of the projection area
    float ProjectionWidthY; // Y width of the projection area
    float ActorVertexX; // half of the X width of an displayed actor
    float ActorVertexY; // half of the Y width of an displayed actor
    float ZoomIncrement; // increment used when we zoom
    float PanIncrement; // increment used when we pan
    const float XLimit; // X limit of the displayable area
    const float YLimit;  // Y limit of the displayable area
    
    // scaling for actors
    const float ActorXScale; // scale factor for drawing actors
    const float ActorYScale; // scale factor for drawing actors
}; // end class GLPPI
