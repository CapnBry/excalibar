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
class Config
{
public:
    Config();
    Config(const Config& s);
    ~Config();
    
    const Config& operator=(const Config& s);
    
    bool Load(const std::string& filename);
    bool Save(const std::string& filename)const;

protected:
private:
    void set(const Config& s);

    DECL_MEMBER(ActorRenderPrefs,PrefsSameRealm);
    DECL_MEMBER(ActorRenderPrefs,PrefsEnemyRealm);
    DECL_MEMBER(ActorRenderPrefs,PrefsMob);
    DECL_MEMBER(bool,RaisePriority);
    DECL_MEMBER(bool,UseZoneTextures);
    DECL_MEMBER(bool,UseVectorMaps);
    DECL_MEMBER(bool,UpdateActorsOnRender);
    DECL_MEMBER(bool,ShowAlbs);
    DECL_MEMBER(bool,ShowHibs);
    DECL_MEMBER(bool,ShowMids);
    DECL_MEMBER(bool,ShowMobs);
    DECL_MEMBER(std::string,DStreamServer);
    DECL_MEMBER(unsigned short,DStreamServerPort);
    DECL_MEMBER(std::string,SharenetServer);
    DECL_MEMBER(unsigned short,SharenetServerPort);
    DECL_MEMBER(std::string,NewAlbSound);
    DECL_MEMBER(bool,PlayAlbSound);
    DECL_MEMBER(std::string,NewHibSound);
    DECL_MEMBER(bool,PlayHibSound);
    DECL_MEMBER(std::string,NewMidSound);
    DECL_MEMBER(bool,PlayMidSound);
    DECL_MEMBER(std::string,NamedMobSound);
    DECL_MEMBER(bool,PlayNamedMobSound);
    DECL_MEMBER(std::string,NamedMobName);
    DECL_MEMBER(bool,ShowRangeRing1);
    DECL_MEMBER(unsigned int,RangeRingRange1);
    DECL_MEMBER(bool,ShowRangeRing2);
    DECL_MEMBER(unsigned int,RangeRingRange2);
    DECL_MEMBER(bool,ShowRangeRing3);
    DECL_MEMBER(unsigned int,RangeRingRange3);
    DECL_MEMBER(bool,ShowRangeRing4);
    DECL_MEMBER(unsigned int,RangeRingRange4);
    DECL_MEMBER(bool,ShowRangeRing5);
    DECL_MEMBER(unsigned int,RangeRingRange5);
    DECL_MEMBER(bool,ShowRangeRing6);
    DECL_MEMBER(unsigned int,RangeRingRange6);
}; // end Config
