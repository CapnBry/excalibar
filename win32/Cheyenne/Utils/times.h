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
#include "..\Utils\signals.h"
#include "..\Utils\threads.h"
#include <limits>

#pragma once

class CheyenneTime
{
public:
    inline CheyenneTime(){Time=0.0;};
    inline CheyenneTime(const CheyenneTime& s){set(s);};
    inline explicit CheyenneTime(const double& s){Time=s;};
    inline ~CheyenneTime(){};

    inline double Seconds(void)const{return(Time);};
    inline void FromSeconds(const double d){Time=d;};

    inline CheyenneTime& operator=(const CheyenneTime& s)
    {
        if(this != &s)
            {
            set(s);
            }
        return(*this);
    }

    inline CheyenneTime operator+(const CheyenneTime& s)const{return(CheyenneTime(Time+s.Time));};
    inline CheyenneTime operator-(const CheyenneTime& s)const{return(CheyenneTime(Time-s.Time));};

    inline CheyenneTime& operator+=(const CheyenneTime& s){Time+=s.Time;return(*this);};
    inline CheyenneTime& operator-=(const CheyenneTime& s){Time-=s.Time;return(*this);};

    inline bool operator==(const CheyenneTime& s)const{return(Time == s.Time);};
    inline bool operator!=(const CheyenneTime& s)const{return(Time != s.Time);};
    inline bool operator>(const CheyenneTime& s)const{return(Time > s.Time);};
    inline bool operator<(const CheyenneTime& s)const{return(Time < s.Time);};
    inline bool operator>=(const CheyenneTime& s)const{return(Time >= s.Time);};
    inline bool operator<=(const CheyenneTime& s)const{return(Time <= s.Time);};

protected:

private:
    inline void set(const CheyenneTime& s)
    {
        Time=s.Time;
    }

    double Time;
};

// maximum value for GetTickCount()
const CheyenneTime tick_count_max(double(UINT_MAX)*0.001);

class CheyenneClock : public Thread
{
public:
    CheyenneClock();
    virtual ~CheyenneClock();

    PulsedEventSignal NewTimeSignal; // event is pulsed when the time changes

    inline const CheyenneTime& Current(void)const{return(CurrentTime);};

protected:
private:

    // thread related stuff
    virtual DWORD Run(const bool& bContinue);

    // the time
    CheyenneTime CurrentTime;

    // the last time
    CheyenneTime LastTime;
};
