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
#include <winsock2.h> // for basic windows definitions
#include <ostream> // for stream defs

#include "stealthmask.h" // for my class defs

const float StealthMask::SpanX=200.0f;
const float StealthMask::SpanY=200.0f;

StealthMask::StealthMask(bool InitSphere)
{
    // zero this out
    ZeroMemory(&Mask[0],sizeof(Mask));

    if(InitSphere)
        {
        MakeSphere();
        }
} // end StealthMask()

StealthMask::StealthMask(const StealthMask& s)
{
    // copy
    CopyMemory(&Mask[0],&s.Mask[0],sizeof(Mask));
} // end StealthMask(const StealthMask&)

StealthMask::~StealthMask()
{
} // end ~StealthMask()

void StealthMask::SetAll(void)
{
    memset(&Mask[0],0xFF,sizeof(Mask));
} // end SetAll

void StealthMask::ClearAll(void)
{
    memset(&Mask[0],0x00,sizeof(Mask));
} // end ClearAll

void StealthMask::Print(std::ostream& str)const
{
    for(int y=0;y<16;++y)
        {
        for(int x=0;x<16;++x)
            {
            if(Get(x,y))
                {
                str << "XX";
                }
            else
                {
                str << "--";
                }
            } // end for all x
        str << "\n";
        } // end for all y
} 

void StealthMask::MakeSphere(void)
{
    // make a filled-in sphere radius 1400
    const int center_x=8;
    const int center_y=8;
    
    for(int y=0;y<16;++y)
        {
        for(int x=0;x<16;++x)
            {
            const int del_x=x-center_x;
            const int del_y=y-center_y;
            const int dist_squared=(del_x*del_x) + (del_y*del_y);
            if(dist_squared <= 7*7)
                {
                // set this bit
                Set(x,y,true);
                }
            } // end for all x
        } // end for all y
} // end MakeSphere

void StealthMask::Set(const int x,const int y,bool val)
{
    // to facilitate processing elsewhere, we do the sanity check here
    if((unsigned int)x>15 || (unsigned int)y>15)
        {
        return;
        }
    
    int byte_offset;
    unsigned char bit_mask;
    GetOffsetAndMask(x,y,byte_offset,bit_mask);
    
    if(val)
        {
        Mask[byte_offset] |= bit_mask;
        }
    else
        {
        Mask[byte_offset] &= (~bit_mask);
        }
} // end Set(x,y,val)

bool StealthMask::Get(const int x,const int y)const
{
    // to facilitate processing elsewhere, we do the sanity check here
    if((unsigned int)x>15 || (unsigned int)y>15)
        {
        return(false);
        }

    int byte_offset;
    unsigned char bit_mask;
    GetOffsetAndMask(x,y,byte_offset,bit_mask);
    return((Mask[byte_offset] & bit_mask)!=0);
} // end Get(x,y)

StealthMask& StealthMask::operator=(const StealthMask& s)
{
    if(this!=&s)
        {
        CopyMemory(&Mask[0],&s.Mask[0],sizeof(Mask));
        }
    
    return(*this);
} // end operator=
bool StealthMask::operator==(const StealthMask& s)const
{
    return(memcmp(&Mask[0],&s.Mask[0],sizeof(Mask))==0);
} // end operator==

StealthMask StealthMask::operator&(const StealthMask& a)const
{
    StealthMask result;
    
    const size_t max=sizeof(Mask)/sizeof(unsigned char);
    for(size_t count=0;count<max;++count)
        {
        result.Mask[count]=Mask[count] & a.Mask[count];
        } // end for each unsigned char in Mask
        
    return(result);
} // end operator&

StealthMask StealthMask::operator|(const StealthMask& a)const
{
    StealthMask result;
    
    const size_t max=sizeof(Mask)/sizeof(unsigned char);
    for(size_t count=0;count<max;++count)
        {
        result.Mask[count]=Mask[count] | a.Mask[count];
        } // end for each unsigned char in Mask
        
    return(result);
} // end operator|

StealthMask& StealthMask::operator&=(const StealthMask& a)
{
    const size_t max=sizeof(Mask)/sizeof(unsigned char);
    for(size_t count=0;count<max;++count)
        {
        Mask[count]=Mask[count] & a.Mask[count];
        } // end for each unsigned char in Mask
        
    return(*this);
} // end operator&=

StealthMask& StealthMask::operator|=(const StealthMask& a)
{
    const size_t max=sizeof(Mask)/sizeof(unsigned char);
    for(size_t count=0;count<max;++count)
        {
        Mask[count]=Mask[count] | a.Mask[count];
        } // end for each unsigned char in Mask
        
    return(*this);
} // end operator|=
