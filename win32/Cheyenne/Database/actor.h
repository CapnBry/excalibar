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

//#include "times.h"
//#include "global.h"
#include <string>
#include "..\Utils\CodeUtils.h" // include macros for DECL_XXX
#include "..\Utils\Times.h" // include for time defintitions
//#include <bitset>

class Motion
{
public:
    Motion(){SetXPos(0.0f);SetYPos(0.0f);SetZPos(0.0f);SetHeading(0.0f);SetSpeed(0.0f);};
    Motion(const Motion& s){set(s);};
    virtual ~Motion(){};

    Motion& operator=(const Motion& s)
    {
        if(this != &s)
            {
            set(s);
            }
        return(*this);
    }

    void IntegrateMotion(const CheyenneTime& CurrentAge)
    {
    // get <x,y>
    float x=float(GetXPos());
    float y=float(GetYPos());

    // adjust for the strangeness due to +y being the "south" direction
    float xvel=(GetSpeed() * sin(GetHeading()));
    float yvel=(GetSpeed() * -cos(GetHeading()));

    // update with velocity & heading & delta time
    x = x + ((float)CurrentAge.Seconds()*xvel);
    y = y + ((float)CurrentAge.Seconds()*yvel);

    // store back
    SetXPos(x);
    SetYPos(y);
    
    // set time
    ModifyValidTime() += CurrentAge;
    } // end IntegrateMotion
    
    float RangeTo(const Motion& To)const;
    
    std::pair<float,float> RangeAzimuthTo(const Motion& To)const;
    
    void Print(std::ostream& os)const;

    void GetPointRelative
        (
        const float bearing, // radians
        const float distance,
        float& x,
        float& y
        )const;

protected:

private:
    void set(const Motion& s)
    {
        MEMBER_ASSIGN(XPos);
        MEMBER_ASSIGN(YPos);
        MEMBER_ASSIGN(ZPos);
        MEMBER_ASSIGN(Heading);
        MEMBER_ASSIGN(Speed);
        MEMBER_ASSIGN(ValidTime);
    }

    DECL_MEMBER(float,XPos);
    DECL_MEMBER(float,YPos);
    DECL_MEMBER(float,ZPos);
    DECL_MEMBER(float,Heading);
    DECL_MEMBER(float,Speed);
    DECL_MEMBER(CheyenneTime,ValidTime); // time this position was valid
}; // end class Motion

class Actor
{
public:
    Actor()
    {
        SetHealth(100);
        SetLevel(0);
        SetRealm(0);
        SetId(0);
        SetInfoId(0);
        SetTargetId(0);
        SetRegion(0);
        SetActorType(Player);
        SetStealth(false);
        SetStealthCycleA(0.0f);
        SetStealthCycleB(0.0f);
        SetStealthCycleC(0.0f);
        SetGroundTargetX(0.0f);
        SetGroundTargetY(0.0f);
        SetGroundTargetZ(0.0f);
    };
    Actor(const Actor& s){set(s);};
    virtual ~Actor(){};

    static float DAOCHeadingToDegrees(unsigned short heading);
    static float DAOCHeadingToRadians(unsigned short heading);
    
    Actor& operator=(const Actor& s)
    {
        if(this != &s)
            {
            set(s);
            }

        return(*this);
    }

    enum ActorTypes
    {
        Player,
        Mob,
        Object
    };

    enum Realms
    {
        MOB,
        Albion,
        Midgard,
        Hibernia
    };

    bool IsType(ActorTypes t)const{return(GetActorType()==t);};

    enum RelativeCon
    {
        Gray,
        Green,
        Blue,
        Yellow,
        Orange,
        Red,
        Purple
    };

    // this function returns the con of actor2 relative to actor1
    static Actor::RelativeCon GetRelativeCon(const unsigned char actor1,const unsigned char actor2);
    void Print(std::ostream& os)const;

    typedef unsigned int id_type;
protected:

private:
    void set(const Actor& s)
    {
        MEMBER_ASSIGN(Motion);
        MEMBER_ASSIGN(Health);
        MEMBER_ASSIGN(Level);
        MEMBER_ASSIGN(Realm);
        MEMBER_ASSIGN(Id);
        MEMBER_ASSIGN(InfoId);
        MEMBER_ASSIGN(TargetId);
        MEMBER_ASSIGN(Name);
        MEMBER_ASSIGN(Guild);
        MEMBER_ASSIGN(Surname);
        MEMBER_ASSIGN(ActorType);
        MEMBER_ASSIGN(LastUpdateTime);
        MEMBER_ASSIGN(Region);
        MEMBER_ASSIGN(Old);
        MEMBER_ASSIGN(Stealth);
        MEMBER_ASSIGN(StealthCycleA);
        MEMBER_ASSIGN(StealthCycleB);
        MEMBER_ASSIGN(StealthCycleC);
        MEMBER_ASSIGN(Net);
        MEMBER_ASSIGN(NetTime);
        MEMBER_ASSIGN(LastLocalTime);
        MEMBER_ASSIGN(GroundTargetX);
        MEMBER_ASSIGN(GroundTargetY);
        MEMBER_ASSIGN(GroundTargetZ);
    }

    DECL_MEMBER(Motion,Motion);
    DECL_MEMBER(unsigned char,Health);
    DECL_MEMBER(unsigned char,Level);
    DECL_MEMBER(unsigned char,Realm);
    DECL_MEMBER(id_type,Id);
    DECL_MEMBER(id_type,InfoId);
    DECL_MEMBER(id_type,TargetId);
    DECL_MEMBER(float,GroundTargetX);
    DECL_MEMBER(float,GroundTargetY);
    DECL_MEMBER(float,GroundTargetZ);
    DECL_MEMBER(std::string,Name);
    DECL_MEMBER(std::string,Guild);
    DECL_MEMBER(std::string,Surname);
    DECL_MEMBER(ActorTypes,ActorType);
    DECL_MEMBER(CheyenneTime,LastUpdateTime); // time this actor was last updated with live info
    DECL_MEMBER(unsigned char,Region); // the region this actor is currently in
    DECL_MEMBER(bool,Old); // true if the actor is "old"
    DECL_MEMBER(bool,Stealth); // true when stealthed
    DECL_MEMBER(float,StealthCycleA); // used for display purposes
                                     // goes from 0 to 1 to 0 every 2 seconds
    DECL_MEMBER(float,StealthCycleB); // used for display purposes
                                     // goes from 0 to 1 to 0 every 2 seconds
    DECL_MEMBER(float,StealthCycleC); // used for display purposes
                                     // goes from 0 to 1 to 0 every 2 seconds
    DECL_MEMBER(Motion,Net); // network motion data
    DECL_MEMBER(CheyenneTime,NetTime); // time this actor was last seen on the shared network
    DECL_MEMBER(CheyenneTime,LastLocalTime); // time this actor was last seen locally (via sniffer)
}; // end class Actor
