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
#ifndef GLOBALOPERATORS_H
#define GLOBALOPERATORS_H

#pragma once

#include <iostream>
#include "logger.h"

/********************************************************************
*                       tuple4 operators                            *
*********************************************************************/
bool operator==(const tuple4& a,const tuple4& b);
bool operator!=(const tuple4& a,const tuple4& b);
bool operator<(const tuple4& a,const tuple4& b);
bool operator<=(const tuple4& a,const tuple4& b);
bool operator>(const tuple4& a,const tuple4& b);
bool operator>=(const tuple4& a,const tuple4& b);
/********************************************************************
*                    std::ostream operators                         *
*********************************************************************/
std::ostream& operator<< (std::ostream& str,const tuple4& a);
std::ostream& operator<< (std::ostream& str,const struct in_addr& a);
std::ostream& operator<< (std::ostream& str,const SOCKADDR_IN& a);
/********************************************************************
*                   CheyenneLogger operators                        *
*********************************************************************/
template<class arg_t> logger_t& operator<<(logger_t& str,const arg_t& arg)
{
    return(::Logger.store(arg));
}

#endif // GLOBALOPERATORS_H
