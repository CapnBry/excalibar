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

class StealthMask
{
public:
    StealthMask(bool InitSphere=false);
    StealthMask(const StealthMask& s);
    virtual ~StealthMask();
    
    void Print(std::ostream& str)const;
    void SetAll(void);
    void ClearAll(void);
    
    void MakeSphere(void);
    // x and y --> [0,15]
    void Set(const int x,const int y,bool val);
    // x and y --> [0,15]
    bool Get(const int x,const int y)const;
    StealthMask& operator=(const StealthMask& s);
    bool operator==(const StealthMask& s)const;
    bool operator!=(const StealthMask& s)const{return(!(*this==s));};
    StealthMask operator&(const StealthMask& a)const;
    StealthMask operator|(const StealthMask& a)const;
    StealthMask& operator&=(const StealthMask& a);
    StealthMask& operator|=(const StealthMask& a);
    
    static const float SpanX;
    static const float SpanY;
protected:
private:
    // x and y --> [0,15]
    inline void GetOffsetAndMask(const int x,const int y,int& byte_offset,unsigned char& bit_mask)const
    {
        const int bit_offset=x+(y*16);
        byte_offset=bit_offset/8;
        switch(bit_offset%8)
            {
            case 0: bit_mask=0x01; return;
            case 1: bit_mask=0x02; return;
            case 2: bit_mask=0x04; return;
            case 3: bit_mask=0x08; return;
            case 4: bit_mask=0x10; return;
            case 5: bit_mask=0x20; return;
            case 6: bit_mask=0x40; return;
            case 7: default: bit_mask=0x80; return;
            } // end switch remainder
    } // end GetOffsetAndMask
    
    unsigned char Mask[32]; // 32 bytes in a bit array. Each bit
                            // represents a 200x200 square area of 
                            // the world. The result is that this array
                            // holds a 3200x3200 area.
                            //
                            // By convention, the center is at 8,8
                            // or 1600,1600 if you prefer
                            // c                       c                    c
                            // o                       o                    o
                            // l                       l                    l
                            // 
                            // 00                      08                   15
                            //________________________________________________
                 // row 15  // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                  // row 8  // 00 01 02 03 04 05 06 07 XX 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                            // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
                  // row 0  // 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
}; // end class StealthMask