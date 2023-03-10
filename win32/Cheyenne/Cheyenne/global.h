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
#ifndef GLOBAL_H
#define GLOBAL_H
#pragma once

#ifdef MAIN_FUNCTION
    #define EXTERN 
#else
    #define EXTERN extern
#endif

#define DECL_MEMBER(type,name) \
    public: \
    inline const type & Get##name(void)const{return(m_##name);}; \
    inline type & Modify##name(void){return(m_##name);}; \
    inline const type & Set##name(const type & val){m_##name=val;return(m_##name);}; \
    private: \
    type m_##name;

#define MEMBER_ASSIGN(name) \
    Set##name(s.Get##name());

#include "globaloperators.h"
#include "mapinfo.h"
#include "times.h"
#include "config.h"
#include "gl\gl.h"
#include "gl\glpng.h"

// the logger
EXTERN logger_t Logger;
// map info from mapinfo.txt
EXTERN MapInfo Zones;
// the clock
EXTERN CheyenneClock Clock;
// the config
EXTERN CheyenneConfig Config;
// the initial working directory (doesn't need to be thread safe ;))
EXTERN std::string InitialDir;

// global functions
// the imlpementations of the global graphics functions are in 
// central.cpp
void DrawWGLFontString(const std::string& text);
inline void DrawGLFontString(const std::string& text)
{
    // do WGL
    DrawWGLFontString(text);
}

inline float RadToDeg(float rad)
{
    return(rad*57.295779513082320876798154814105f);
}

inline float DegToRad(float deg)
{
    return(deg*0.017453292519943295769236907684886f);
}

template<class container> typename container::value_type::second_type PngBindContainer
    (
    container& Container,
    typename container::value_type::first_type association,
    const char *filename,
    int mipmap=PNG_BUILDMIPMAPS,
    int trans=PNG_SOLID,
    pngInfo *info=NULL,
    int wrapst=GL_CLAMP,
    int minfilter=GL_LINEAR_MIPMAP_NEAREST,
    int magfilter=GL_LINEAR_MIPMAP_NEAREST
    )
{
    container::value_type::second_type id=pngBind(filename,mipmap,trans,info,wrapst,minfilter,magfilter);

    if(id != 0)
        {
        Logger << "[PngBindContainer] loaded \"" << filename << "\" with id " << id << "\n";
        Container.insert(container::value_type(association,id));
        }

    // done
    return(id);
}; // end PngBindContainer

std::istream& GetLine(std::istream& is,std::string& str);
void ToString(const std::wstring& source_wstring,std::string& dest_string);
void ToWString(const std::string& source_string,std::wstring& dest_wstring);

#endif // GLOBAL_H