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
template<typename T> bool is_near(const T& a,const T&b)
{
    return(fabs(a-b) < 0.00001);
}
template<typename T> bool not_near(const T& a,const T&b)
{
    return(!is_near(a,b));
}

template<typename T> struct INTERCEPT_PARAMS
{
    T t0x; // target initial x position
    T t0y; // target initial y position
    T a0x; // reference initial x position
    T a0y; // reference initial y position
    T tvx; // target x velocity
    T tvy; // target y velicity
    T s; // constant speed of reference
}; // end INTERCEPT_PARAMS

template<typename T> struct INTERCEPT_DATA
{
    INTERCEPT_PARAMS<T> params; // parameters for intercept
    T TimeToIntercept; // Time until intercept (only valid if intercept algorithm succeeds)
    T XDotToIntercept; // X velocity of reference to intercept in TimeToIntercept time (only valid if intercept algorithm succeeds)
    T YDotToIntercept; // Y velocity of reference to intercept in TimeToIntercept time (only valid if intercept algorithm succeeds)
    T HeadingRadiansToIntercept; // heading in radians for reference to intercept in TimeToIntercept time (only valid if intercept algorithm succeeds)
}; // end INTERCEPT_DATA

/*
    This rather complicated pair of functions deserve some explanation.
    
    Given the parameters, they determine the fastest time in which
    the reference can intercept the target. It assumes infinite turn
    rate and infinite acceleration, so it may need to be called more
    than once for a given intercept because reality dictates that
    nothing accelerates or turns instantly. Stupid reality...
    
    What it does it this (and this is straight from my notes, sorry):
    
    define:
        t0 as initial time
        At as reference position at time t
        A0 as reference position at time t0
        Tt as target position at time t
        T0 as reference position at time t0
        Av as reference velocity to intercept (this is an output of the algorithm)
        Tv as target velocity at time (assumed constant, if it is not then
            you will have to call this function more than once)
        s as squareroot-of-the-sum-of-the-squares: |Av|
    
    define:
        At = A0+(Av*(t-t0))
        Tt = T0+(Tv*(t-t0))
        
    then:
        For intercept at time t, At==Tt
        Setting At and Tt to be equal and rearranging to solve for t:
        
        t=((A0-T0)/Tv-Av)+t0
        
        This is the time-of-intercept
    
    therefore:
        We must minimize the time-to-intercept (t)
        
    define:
        Avx as x component of velocity for reference to intercept target
        Avy as y component of velocity for reference to intercept target
        Tvx as x component of velocity of target
        Tvy as y component of velocity of target
        
        But Avx and Avy are related: Avy is +/- (s^2 - Avx^2)^0.5
        
    therefore:
        Time of x intercept and y intercept is the same, so
        <-- from above, rearrange: t=((A0-T0)/Tv-Av)+t0 -->
        <-- the t cancels and so does t0 -->
        
        {equation 1}
            (A0x-T0x)/(Tvx-Avx) = (A0y-T0y)/(Tvy-Avy)

    define:
        Dx as T0x-A0x
        Dy as T0y-A0y
        
    therefore:
        Rearrange equation 1:
        {equation 2}
            (Dx*(Tvy-Avy)/Dy) - (Tvx-Avx) = 0
        
        To minimize a function f() of 1 variable over a given range:
            (Avx and Avy are both unknown, but they are related)
            (the range is [-s,+s] -- we have a capped velocity!)
            (we want to minimize t, which is what equation 2 is in terms of)
            
        1. We take the derivative of f()
        2. Find the critical points
        3. Solve f(c) for all critical points
        4. Solve for f(-s) and f(+s)
        5. The minimum of f() over the range [-s,+s] is 
           the largest value computed in steps 3 and 4.
           
        d/dAvx for equation 2 (f'()) is:
        {equation 3}
            1-((Dx*Avx/Dy)/sqrt(s^2-Avx^2))
        
        Performing steps 2-4 on equation 3 gives 4 possible solutions:
            (you must use the quadratic equation to solve this one)
        
        ************************************************************
        +s,
        -s,
        +(sqrt(4*((Dx^2+Dy^2)/Dy^2)*s^2) )/2*((Dx^2+Dy^2)/Dy^2)),
        -(sqrt(4*((Dx^2+Dy^2)/Dy^2)*s^2) )/2*((Dx^2+Dy^2)/Dy^2))
        ************************************************************
        
        We have to be careful here: because of the nature of the problem, 
        we can solve for Avx and still not know Avy because there are 2 possible
        Avy solutions (remember that Avy = sqrt(s^2-Avx^2)). This is why 
        Find intercept time tests each solution effectively 4 times:
        +x,+y
        -x,-y
        +x,-y
        -x,+y
        
        The result is the solution that gives the best
        sensible (non-zero, non-infinite) time of intercept.
        
        This result is passed up to FindIntercept() and
        the actual Avx and Avy values are determined based
        upon knowing the time of intercept.
        
        SO: call FindIntercept() and you will get a reasonable result. 
        If it returns false, then you are either at the intercept point already
        or there is no possible way to intercept the target.
        
        I think :D
*/
template<typename T> T FindInterceptTime
    (
    const INTERCEPT_PARAMS<T>& params
    )
{
    const T dx=params.a0x-params.t0x;
    const T dy=params.a0y-params.t0y;
    const T dx_squared=dx*dx;
    const T dy_squared=dy*dy;
    
    // possible solutions:
    // +/- s
    // quadratic
    // 4 total possible solutions
    T solution[4];
    solution[0]=params.s;
    solution[1]=-params.s;
    
    if(not_near(dy_squared,T(0)) && not_near(dx_squared+dy_squared,T(0)))
        {
        solution[2]=
            sqrt(T(4)*((dx_squared+dy_squared)/dy_squared)*params.s*params.s) /
            (T(2)*((dx_squared+dy_squared)/dy_squared));

        solution[3]=
            -(sqrt(T(4)*((dx_squared+dy_squared)/dy_squared)*params.s*params.s)) /
            (T(2)*((dx_squared+dy_squared)/dy_squared));
    
        }
    else
        {
        solution[2]=T(0);
        solution[3]=T(0);
        }
        
    // find
    T avx_temp=T(0);
    T avy_temp=T(0);
    T delta_t=std::numeric_limits<T>::max();
    T delta_t_temp;
    
    for(int i=0;i<4;++i)
        {
        // take y solution
        avy_temp=solution[i];
        avx_temp=sqrt(params.s*params.s - avy_temp*avy_temp); // @@ check both + and - values here!!!
        
        // check for infinity in the division
        if(not_near(params.tvy,avy_temp))
            {
            // find how long until y gets to where it needs to be
            delta_t_temp=((params.a0y-params.t0y)/(params.tvy-avy_temp));
            
            // find how long until x gets to where it needs to be
            // check for infinity in the division
            if(not_near(params.tvx,avx_temp))
                {
                if(not_near(delta_t_temp,((params.a0x-params.t0x)/(params.tvx-avx_temp))))
                    {
                    // not a valid solution, no intercept!
                    delta_t_temp=std::numeric_limits<T>::max();
                    }
                }
            else if(not_near(params.a0x,params.t0x))
                {
                // not a valid solution, no intercept!
                delta_t_temp=std::numeric_limits<T>::max();
                }
            }
        else
            {
            // not a valid solution, no intercept!
            delta_t_temp=std::numeric_limits<T>::max();
            }
        
        if(delta_t_temp < delta_t && not_near(delta_t_temp,T(0)) && delta_t_temp>T(0))
            {
            delta_t=delta_t_temp;
            continue;
            }
        
        // check the x solution
        avy_temp=avx_temp;
        avx_temp=solution[i];
        
        // check for infinity in the division
        if(not_near(params.tvx,avx_temp))
            {
            // find how long until x gets to where it needs to be
            delta_t_temp=((params.a0x-params.t0x)/(params.tvx-avx_temp));

            // find how long until y gets to where it needs to be
            // check for infinity in the division
            if(not_near(params.tvy,avy_temp))
                {
                if(not_near(delta_t_temp,((params.a0y-params.t0y)/(params.tvy-avy_temp))))
                    {
                    // not a valid solution, no intercept!
                    delta_t_temp=std::numeric_limits<T>::max();
                    }
                }
            else if(not_near(params.a0y,params.t0y))
                {
                // not a valid solution, no intercept!
                delta_t_temp=std::numeric_limits<T>::max();
                }
            }
        else
            {
            // not a valid solution, no intercept!
            delta_t_temp=std::numeric_limits<T>::max();
            }
            
        if(delta_t_temp < delta_t && not_near(delta_t_temp,T(0)) && delta_t_temp>T(0))
            {
            delta_t=delta_t_temp;
            continue;
            }

        // take y solution with avx=-avx (sqrt only gives + answers)
        avy_temp=solution[i];
        avx_temp=-sqrt(params.s*params.s - avy_temp*avy_temp); // @@ check both + and - values here!!!
        
        // check for infinity in the division
        if(not_near(params.tvy,avy_temp))
            {
            // find how long until y gets to where it needs to be
            delta_t_temp=((params.a0y-params.t0y)/(params.tvy-avy_temp));
            
            // find how long until x gets to where it needs to be
            // check for infinity in the division
            if(not_near(params.tvx,avx_temp))
                {
                if(not_near(delta_t_temp,((params.a0x-params.t0x)/(params.tvx-avx_temp))))
                    {
                    // not a valid solution, no intercept!
                    delta_t_temp=std::numeric_limits<T>::max();
                    }
                }
            else if(not_near(params.a0x,params.t0x))
                {
                // not a valid solution, no intercept!
                delta_t_temp=std::numeric_limits<T>::max();
                }
            }
        else
            {
            // not a valid solution, no intercept!
            delta_t_temp=std::numeric_limits<T>::max();
            }
        
        if(delta_t_temp < delta_t && not_near(delta_t_temp,T(0)) && delta_t_temp>T(0))
            {
            delta_t=delta_t_temp;
            continue;
            }
        
        // check the x solution
        avy_temp=avx_temp;
        avx_temp=solution[i];
        
        // check for infinity in the division
        if(not_near(params.tvx,avx_temp))
            {
            // find how long until x gets to where it needs to be
            delta_t_temp=((params.a0x-params.t0x)/(params.tvx-avx_temp));

            // find how long until y gets to where it needs to be
            // check for infinity in the division
            if(not_near(params.tvy,avy_temp))
                {
                if(not_near(delta_t_temp,((params.a0y-params.t0y)/(params.tvy-avy_temp))))
                    {
                    // not a valid solution, no intercept!
                    delta_t_temp=std::numeric_limits<T>::max();
                    }
                }
            else if(not_near(params.a0y,params.t0y))
                {
                // not a valid solution, no intercept!
                delta_t_temp=std::numeric_limits<T>::max();
                }
            }
        else
            {
            // not a valid solution, no intercept!
            delta_t_temp=std::numeric_limits<T>::max();
            }
            
        if(delta_t_temp < delta_t && not_near(delta_t_temp,T(0)) && delta_t_temp>T(0))
            {
            delta_t=delta_t_temp;
            continue;
            }
        } // end for all possible solutions

    // done
    return(delta_t);
} // end FindInterceptTime

/*
    For a description of this, see comment for FindInterceptTime
*/
template<typename T> bool FindIntercept(INTERCEPT_DATA<T>& intercept_data)
{
    intercept_data.TimeToIntercept=FindInterceptTime(intercept_data.params);
    
    if(intercept_data.TimeToIntercept < std::numeric_limits<float>::max() && not_near(intercept_data.TimeToIntercept,T(0)))
        {
        // we have a viable solution, get x and y intercept
        // points then convert to speed
        const T x_intercept=intercept_data.params.t0x+(intercept_data.TimeToIntercept*intercept_data.params.tvx);
        const T y_intercept=intercept_data.params.t0y+(intercept_data.TimeToIntercept*intercept_data.params.tvy);
        
        intercept_data.XDotToIntercept=(x_intercept - intercept_data.params.a0x)/intercept_data.TimeToIntercept;
        intercept_data.YDotToIntercept=(y_intercept - intercept_data.params.a0y)/intercept_data.TimeToIntercept;

        // get angle
        intercept_data.HeadingRadiansToIntercept=atan2(intercept_data.XDotToIntercept,intercept_data.YDotToIntercept);
        
        // correct to positive as necessary
        if(intercept_data.HeadingRadiansToIntercept < T(0))
            {
            intercept_data.HeadingRadiansToIntercept += T(2)*T(3.1415926535897932384626433832795);
            }
        
        return(true);
        } // end if FindInterceptTime returned a workable time
    else
        {
        return(false);
        } // end else FindInterceptTime returned an unworkable time
} // end FindIntercept

