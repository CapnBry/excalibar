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

#include <string>
#include <iostream>

class CheyenneConfig
{
public:
    CheyenneConfig();
    CheyenneConfig(const CheyenneConfig& s) : m_ConfigFileName("cheyenne.cfg")
    {
        set(s);
    }

    ~CheyenneConfig()
    {
    };

    CheyenneConfig& operator=(const CheyenneConfig& s)
    {
        if(this != &s)
            {
            set(s);
            }
        return(*this);
    }

    bool Load(const std::string& str)
    {
        SetConfigFileName(str);
        return(Load());
    }

    bool Save(const std::string& str)
    {
        SetConfigFileName(str);
        return(Save());
    }

    bool Load(void);
    bool Save(void)const;

    struct RANGE_RING
    {
    unsigned int Radius;
    bool bEnabled;

    RANGE_RING& operator=(const RANGE_RING& s)
    {
        Radius=s.Radius;
        bEnabled=s.bEnabled;
        return(*this);
    }
    };


    struct RANGE_RINGS
    {
    RANGE_RING Rings[6];

    RANGE_RINGS& operator=(const RANGE_RINGS& s)
        {
        for(int i=0;i<6;++i)
            {
            Rings[i]=s.Rings[i];
            }
        return(*this);
        }
    };

protected:
private:
    bool operator==(const CheyenneConfig& s);

    void set(const CheyenneConfig& s)
    {
        MEMBER_ASSIGN(PPIText);
        MEMBER_ASSIGN(GuildInPPI);
        MEMBER_ASSIGN(SurnameInPPI);
        MEMBER_ASSIGN(LevelInPPI);
        MEMBER_ASSIGN(HealthInPPI);
        MEMBER_ASSIGN(AutoFollowFirstActor);
        MEMBER_ASSIGN(RenderMOBs);
        MEMBER_ASSIGN(RenderPlayers);
        MEMBER_ASSIGN(RenderObjects);
        MEMBER_ASSIGN(ConfigFileName);
        MEMBER_ASSIGN(RaisePriority);
        MEMBER_ASSIGN(MatchFollowedHeading);
        MEMBER_ASSIGN(RangeRings);
        MEMBER_ASSIGN(PlaySoundOnAlbCreate);
        MEMBER_ASSIGN(AlbSoundFile);
        MEMBER_ASSIGN(PlaySoundOnHibCreate);
        MEMBER_ASSIGN(HibSoundFile);
        MEMBER_ASSIGN(PlaySoundOnMidCreate);
        MEMBER_ASSIGN(MidSoundFile);
        MEMBER_ASSIGN(LogUnknownPackets);
        MEMBER_ASSIGN(DrawDeadActors);
        MEMBER_ASSIGN(UpdateWhenRendered);
        MEMBER_ASSIGN(TexturesInPPI);
        MEMBER_ASSIGN(VectorMapInPPI);
        MEMBER_ASSIGN(VectorMapOnlyInFollowedZone);
    }

    DECL_MEMBER(bool,PPIText);
    DECL_MEMBER(bool,GuildInPPI);
    DECL_MEMBER(bool,SurnameInPPI);
    DECL_MEMBER(bool,LevelInPPI);
    DECL_MEMBER(bool,HealthInPPI);
    DECL_MEMBER(bool,AutoFollowFirstActor);
    DECL_MEMBER(bool,RenderMOBs);
    DECL_MEMBER(bool,RenderPlayers);
    DECL_MEMBER(bool,RenderObjects);
    DECL_MEMBER(bool,RaisePriority);
    DECL_MEMBER(bool,MatchFollowedHeading);
    DECL_MEMBER(std::string,ConfigFileName);
    DECL_MEMBER(RANGE_RINGS,RangeRings);
    DECL_MEMBER(bool,PlaySoundOnAlbCreate);
    DECL_MEMBER(std::string,AlbSoundFile);
    DECL_MEMBER(bool,PlaySoundOnHibCreate);
    DECL_MEMBER(std::string,HibSoundFile);
    DECL_MEMBER(bool,PlaySoundOnMidCreate);
    DECL_MEMBER(std::string,MidSoundFile);
    DECL_MEMBER(bool,LogUnknownPackets);
    DECL_MEMBER(bool,DrawDeadActors)
    DECL_MEMBER(bool,UpdateWhenRendered);
    DECL_MEMBER(bool,TexturesInPPI);
    DECL_MEMBER(bool,VectorMapInPPI);
    DECL_MEMBER(bool,VectorMapOnlyInFollowedZone);

}; // end class CheyenneConfig