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
#include <assert.h>
#include <crtdbg.h>
#include <iostream>

#include "times.h"

CheyenneClock::CheyenneClock()
{
} // end CheyenneClock

CheyenneClock::~CheyenneClock()
{
    // stop
    Stop();
} // end ~CheyenneClock

DWORD CheyenneClock::Run(const bool& bContinue)
{
    // initialize LastTime to now
    LastTime=CheyenneTime(double(GetTickCount()) * 0.001);

    while(bContinue)
        {
        // get time
        CheyenneTime Temp(double(GetTickCount()) * 0.001);

        // check for wrap-around
        if(Temp < LastTime)
            {
            // time wrapped around
            // so we have to adjust
            CurrentTime += (tick_count_max - LastTime + Temp);
            }
        else
            {
            // time did not wrap around, so we 
            // can just add the difference
            CurrentTime += (Temp-LastTime);
            }

        // if clock changed, pulse event
        if(Temp != LastTime)
            {
            NewTimeSignal.signal();
            }

        // save for next iteration
        LastTime=Temp;

        // sleep a bit
        Sleep(10);
        } // end forever

    return(0);
} // end Run
