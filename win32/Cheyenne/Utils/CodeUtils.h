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
#include <winsock2.h> // for network defs
#include <limits>
#include <iostream> // for stream defs
#include <list> // for list definition
#include "gl\glpng.h" // for png utilities

#define DECL_MEMBER(type,name) \
    public: \
    inline const type & Get##name(void)const{return(m_##name);}; \
    inline type & Modify##name(void){return(m_##name);}; \
    inline const type & Set##name(const type & val){m_##name=val;return(m_##name);}; \
    private: \
    type m_##name;

#define MEMBER_ASSIGN(name) \
    Set##name(s.Get##name());

// replacements for min and max macros
template<typename T> T max(const T a,const T b){return(a > b ? a : b);};
template<typename T> T min(const T a,const T b){return(a < b ? a : b);};

// structures
union word_builder
{
    public:
    word_builder(){dword=0;};
    word_builder(const word_builder& s){dword=s.dword;};
    ~word_builder(){};
    
    word_builder& operator=(const word_builder& s){dword=s.dword;return(*this);};

    unsigned int dword;
    unsigned short word[2];
    unsigned char byte[4];
    float real;
};

// console attach/detatch for GUI-based programs
void AttachConsole(void);
void DetachConsole(void);

// glPng helper
/*
 * Thanks to:
 *
 * PNG loader library for OpenGL v1.45 (10/07/00)
 * by Ben Wyatt ben@wyatt100.freeserve.co.uk
 * Using LibPNG 1.0.2 and ZLib 1.1.3
*/
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
        Container.insert(container::value_type(association,id));
        }

    // done
    return(id);
}; // end PngBindContainer

// std::ostream helper operators
std::ostream& operator<< (std::ostream& str,const struct in_addr& a);
std::ostream& operator<< (std::ostream& str,const SOCKADDR_IN& a);

// common control helpers
void SET_CHECK_BOOL(HWND hwnd,UINT control,bool bool_value);
bool GET_CHECK_BOOL(HWND hwnd,UINT control);
void SET_EDIT_STRING(HWND hwnd,UINT control,const std::string& std_str);
void GET_EDIT_STRING(HWND hwnd,UINT control,std::string& std_str) ;
void GET_LISTVIEW_SELECTED_ITEMS(HWND hwnd,UINT control,std::list<std::string>& std_list);

// intercept template functions
template<typename T> bool is_near(const T& a,const T&b,const T& threshold=T(0.00001))
{
    return(fabs(a-b) < threshold);
}
template<typename T> bool not_near(const T& a,const T&b,const T& threshold=T(0.00001))
{
    return(!is_near(a,b,threshold));
}

template<typename T> struct INTERCEPT_PARAMS
{
    T t0x; // target initial x position
    T t0y; // target initial y position
    T a0x; // reference initial x position
    T a0y; // reference initial y position
    T tvx; // target x velocity
    T tvy; // target y velocity
    T s; // constant speed of reference
}; // end INTERCEPT_PARAMS

template<typename T> struct INTERCEPT_DATA
{
    INTERCEPT_PARAMS<T> params; // parameters for intercept
    T TimeToIntercept; // Time until intercept (only valid if intercept algorithm succeeds)
    T XIntercept; // X intercept in TimeToIntercept time (only valid if intercept algorithm succeeds)
    T YIntercept; // Y intercept in TimeToIntercept time (only valid if intercept algorithm succeeds)
    T XDotToIntercept; // X velocity of reference to intercept in TimeToIntercept time (only valid if intercept algorithm succeeds)
    T YDotToIntercept; // Y velocity of reference to intercept in TimeToIntercept time (only valid if intercept algorithm succeeds)
    T HeadingRadiansToIntercept; // heading in radians for reference to intercept in TimeToIntercept time (only valid if intercept algorithm succeeds)
}; // end INTERCEPT_DATA

/*
    This rather complicated function deserves some explanation.
    
    Given the parameters, they determine the fastest time in which
    the reference can intercept the target. It assumes infinite turn
    rate and infinite acceleration, so it may need to be called more
    than once for a given intercept because reality dictates that
    nothing accelerates or turns instantly. Stupid reality...
    
    The method implemented below is known as "Proportional Navigation"
    and is the basis for many missile intercept equations. Look it
    up on the 'net, many people have written excellent papers, theses,
    etc. on the subject. This is the only free C++ implementation that 
    I know of. Feel free to make use of it!
*/
template<typename T> bool FindIntercept(INTERCEPT_DATA<T>& id)
{
    const T DeltaRx=id.params.t0x-id.params.a0x; // x range to target
    const T DeltaRy=id.params.t0y-id.params.a0y; // y range to target
    const T DeltaR=sqrt(DeltaRx*DeltaRx + DeltaRy*DeltaRy); // total range to target
    const T St=sqrt(id.params.tvx*id.params.tvx + id.params.tvy*id.params.tvy); // target speed
    
    // can't do const here because this must be computed
    T CosTheta; // theta is angle between DeltaR vector and target speed vector
    if(is_near(DeltaR*St,T(0)))
        {
        // we have a 0/0 case here. Taking the limit and whatnot, 
        // CosTheta is 1 (theta is 0°)
        CosTheta=T(1);
        }
    else
        {
        CosTheta=(DeltaRx*id.params.tvx + DeltaRy*id.params.tvy) / (DeltaR*St);
        }
    
    // use trigonometric identity to get this one
    // we ignore the +- thing...
    const T SinTheta=sqrt(T(1) - (CosTheta*CosTheta)); 
    
    // alpha is the DeltaR vector and a vector from the reference ("a" in INTERCEPT_DATA::INTERCEPT_PARAMS)
    // to the predicted intercept point
    // s (speed of reference "a") can NOT be zero!
    const T SinAlpha=SinTheta*(St/id.params.s);
    
    // check for a solution: if |SinAlpha| > 1 then no intercept is possible!
    if(fabs(SinAlpha) > T(1))
        {
        // no possible intercept!
        return(false);
        }
    
    // use trigonometric identity to get this one
    // again, we ignore the +- thing...
    const T CosAlpha=sqrt(T(1)-(SinAlpha*SinAlpha));
    
    // compute ideal time-to-go (ttg). Can't use const
    // because this must be computed
    T ttg;
    {
    T temp=(id.params.s*CosAlpha) - (St*CosTheta);
    
    if(temp > T(0))
        {
        ttg=DeltaR/temp;
        }
    else
        {
        ttg=0; // intercept already occured (!)
        id.TimeToIntercept=ttg;
        id.XIntercept=id.params.t0x;
        id.YIntercept=id.params.t0y;
        id.XDotToIntercept=T(0);
        id.YDotToIntercept=T(0);
        id.HeadingRadiansToIntercept=T(0);
        return(true);
        }
    }

    // populate id
    id.TimeToIntercept=ttg; // time to go
    id.XIntercept=id.params.t0x + (ttg*id.params.tvx); // simple position integration
    id.YIntercept=id.params.t0y + (ttg*id.params.tvy); // simple position integration
    id.XDotToIntercept=(id.XIntercept-id.params.a0x) / ttg; // simple position integration
    id.YDotToIntercept=(id.YIntercept-id.params.a0y) / ttg; // simple position integration
    id.HeadingRadiansToIntercept=atan2(id.XDotToIntercept,id.YDotToIntercept);
    
    // give >0 headings only
    if(id.HeadingRadiansToIntercept < T(0))
        {
        id.HeadingRadiansToIntercept+=T(2)*T(3.1415926535897932384626433832795);
        }

    // done, success!
    return(true);
} // end FindIntercept
