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
#include "logger.h"

extern logger_t Logger;

CheyenneLogger::CheyenneLogger()
{
} // end CheyenneLogger()

CheyenneLogger::~CheyenneLogger()
{
} // end ~CheyenneLogger

bool CheyenneLogger::Init(const char* const filename)
{
    if(file.is_open())
        {
        // can't re-open
        return(false);
        }
        
    // open it
    file.open(filename);
    
    // return status
    return(file.is_open());
} // end Init
