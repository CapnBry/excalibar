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
#include "global.h"

bool operator==(const tuple4& a,const tuple4& b)
{
    return(memcmp(&a,&b,sizeof(tuple4)) == 0 ? true:false);
} // end operator==tuple4)

bool operator!=(const tuple4& a,const tuple4& b)
{
    return(memcmp(&a,&b,sizeof(tuple4)) == 0 ? false:true);
} // end operator!=tuple4)

bool operator<(const tuple4& a,const tuple4& b)
{
    const unsigned char* pa=reinterpret_cast<const unsigned char*>(&a);
    const unsigned char* pb=reinterpret_cast<const unsigned char*>(&b);

    for(int i=0;i<sizeof(tuple4);++i)
        {
        if(pa[i] < pb[i])
            {
            return(true);
            }
        
        if(pa[i] > pb[i])
            {
            return(false);
            }
        }
    
    return(false);
} // end operator<(tuple4)

bool operator<=(const tuple4& a,const tuple4& b)
{
    return(a==b ? true : a<b);
} // end operator<=(tuple4)

bool operator>(const tuple4& a,const tuple4& b)
{
    const unsigned char* pa=reinterpret_cast<const unsigned char*>(&a);
    const unsigned char* pb=reinterpret_cast<const unsigned char*>(&b);

    for(int i=0;i<sizeof(tuple4);++i)
        {
        if(pa[i] > pb[i])
            {
            return(true);
            }
        
        if(pa[i] < pb[i])
            {
            return(false);
            }
        }
    
    return(false);
} // end operator>(tuple4)

bool operator>=(const tuple4& a,const tuple4& b)
{
    return(a==b ? true : a>b);
} // end operator>=(tuple4)

/********************************************************************
*                    std::ostream operators                         *
*********************************************************************/
std::ostream& operator<< (std::ostream& str,const tuple4& a)
{
    char src[16];
    char dst[16];

    strcpy(src,inet_ntoa(*reinterpret_cast<const in_addr*>(&a.saddr)));
    strcpy(dst,inet_ntoa(*reinterpret_cast<const in_addr*>(&a.daddr)));

    str << src << 
        ":" <<
        a.source <<
        " " <<
        dst <<
        ":" <<
        a.dest;

    return(str);
} // end operator<< (tuple4)

std::ostream& operator<< (std::ostream& str,const struct in_addr& a)
{
    str << unsigned int(a.S_un.S_un_b.s_b1) << "."
        << unsigned int(a.S_un.S_un_b.s_b2) << "."
        << unsigned int(a.S_un.S_un_b.s_b3) << "."
        << unsigned int(a.S_un.S_un_b.s_b4);
        
    return(str);
} // end operator<< (std::ostream& str,const struct in_addr& a)
std::ostream& operator<< (std::ostream& str,const SOCKADDR_IN& a)
{
    str << a.sin_addr << ":" << ntohs(a.sin_port);
    return(str);
} // end operator<< (std::ostream& str,const SOCKADDR_IN& a)

std::istream& GetLine(std::istream& is,std::string& str)
{
    std::istream::char_type ch;
    
    // get first char
    is.read(&ch,1);
    while(ch!=0x0D && ch!=0x0A)
        {
        if(is.eof() || !is.good())
            {
            // end of stream
            break;
            }

        // store in string
        str += ch;
        
        // get next char
        is.read(&ch,1);
        }

    // skip remainder of line (CR/LF)
    is >> std::ws;
    
    return(is);
} // end GetLine(istream,string)
