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
#ifndef ACTOR_H
#define ACTOR_H
#pragma once

//#include "times.h"
#include "global.h"
#include <string>
#include <bitset>

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

    void Print(std::ostream& os)const
    {
        os << "[Motion::Print]\n"
           << "ValidTime=" << GetValidTime().Seconds() << "\n"
           << "XPos=" << GetXPos() << "\n"
           << "YPos=" << GetYPos() << "\n"
           << "ZPos=" << GetZPos() << "\n"
           << "Heading=" << GetHeading()*180.0f/3.1415926535897932384626433832795f << "\n"
           << "Speed=" << GetSpeed() << "\n";

        // done
        return;
    } // end Print

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
    Actor(){SetHealth(100);SetLevel(0);SetRealm(0);SetId(0);SetInfoId(0);SetTargetId(0);SetRegion(0);SetActorType(Player);};
    Actor(const Actor& s){set(s);};
    virtual ~Actor(){};

    static inline float DAOCHeadingToDegrees(unsigned short heading)
    {
        float val=float(heading)*(360.0f/4096.0f);
        //                        conv to deg

        // all daoc headings are off 180° because +y axis is SOUTH
        val+=180.0f;
        val=fmod(val,360.0f);
        return(val);
        
    }

    static inline float DAOCHeadingToRadians(unsigned short heading)
    {
        float val=float(heading)*(360.0f/4096.0f) * (3.1415926535897932384626433832795f/180.0f);
        //                        conv to deg        convert from deg to rad

        // all daoc headings are off 180° because +y axis is SOUTH
        val+=3.1415926535897932384626433832795f;
        val=fmod(val,2.0f*3.1415926535897932384626433832795f);
        return(val);
    }

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
        Friend,
        Albion,
        Midgard,
        Hibernia
    };

    bool IsType(ActorTypes t)const{return(GetActorType()==t);};

    void Print(std::ostream& os)const
    {
        os << "[Actor::Print]\n";
        GetMotion().Print(os);
        os << "Health=" << (int)GetHealth() << "\n"
           << "Level=" << (int)GetLevel() << "\n"
           << "Realm=" << (int)GetRealm() << "\n"
           << "Id=" << GetId() << "\n"
           << "InfoId=" << GetInfoId() << "\n"
           << "Name=" << GetName() << "\n"
           << "Guild=" << GetGuild() << "\n"
           << "Surname=" << GetSurname() << "\n"
           << "LastUpdateTime=" << GetLastUpdateTime().Seconds() << "\n"
           << "TargetId=" << GetTargetId() << "\n"
           << "InRegion=" << (int)GetRegion() << "\n"
           << "ActorType=" << GetActorType() << std::endl;

        // done
        return;
    }

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
    }

    DECL_MEMBER(Motion,Motion);
    DECL_MEMBER(unsigned char,Health);
    DECL_MEMBER(unsigned char,Level);
    DECL_MEMBER(unsigned char,Realm);
    DECL_MEMBER(unsigned int,Id);
    DECL_MEMBER(unsigned int,InfoId);
    DECL_MEMBER(unsigned int,TargetId);
    DECL_MEMBER(std::string,Name);
    DECL_MEMBER(std::string,Guild);
    DECL_MEMBER(std::string,Surname);
    DECL_MEMBER(ActorTypes,ActorType);
    DECL_MEMBER(CheyenneTime,LastUpdateTime); // time this actor was last updated with live info
    DECL_MEMBER(unsigned char,Region); // the region this actor is currently in
}; // end class Actor

#endif // ACTOR_H