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

// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <string>
#include <fstream>
#include "..\Utils\Logger.h" // for the logger
#include "..\Utils\CodeUtils.h" // nice dialog utilities in here
#include "..\GLPPI\GLPPI.h" // for actor render prefs
#include "config.h"

extern logger_t Logger; // the logger -- this way prevents a circular #include

Config::Config()
{
    // setup initial config
    ModifyPrefsSameRealm().SetRenderSurname(false);
    ModifyPrefsSameRealm().SetRenderLevel(false);
    ModifyPrefsMob().SetRenderLevel(false);
    ModifyPrefsMob().SetRenderHealth(false);
    SetRaisePriority(false);
    SetUseZoneTextures(false);
    SetUseVectorMaps(false);
    SetUpdateActorsOnRender(true);
    SetShowAlbs(true);
    SetShowHibs(true);
    SetShowMids(true);
    SetShowMobs(true);
    SetSaveDAoCMessages(false);
    SetShowObjects(true);
    SetDStreamServer("<no server>");
    SetDStreamServerPort(9867);
    SetSharenetServer("<no server>");
    SetSharenetServerPort(10001);
    SetNewAlbSound("<no sound>");
    SetPlayAlbSound(false);
    SetNewHibSound("<no sound>");
    SetPlayHibSound(false);
    SetNewMidSound("<no sound>");
    SetPlayMidSound(false);
    SetNamedMobSound("<no sound>");
    SetPlayNamedMobSound(false);
    SetNamedMobName("<no mob name>");
    SetShowRangeRing1(false);
    SetRangeRingRange1(350);
    SetShowRangeRing2(false);
    SetRangeRingRange2(1500);
    SetShowRangeRing3(false);
    SetRangeRingRange3(1875);
    SetShowRangeRing4(false);
    SetRangeRingRange4(0);
    SetShowRangeRing5(false);
    SetRangeRingRange5(0);
    SetShowRangeRing6(false);
    SetRangeRingRange6(0);
    SetSimplifyLines(false);
    SetSimplifyLinesTolerance(0);
    SetSaveChatMessages(false);
    
} // end Config

Config::Config(const Config& s)
{
    set(s);
} // end Config

Config::~Config()
{
} // end ~Config

const Config& Config::operator=(const Config& s)
{
    if(this != &s)
        {
        set(s);
        }
    
    return(*this);
} // end operator=

bool Config::Load(const std::string& filename)
{
    std::ifstream file(filename.c_str());
    
    if(!file.is_open())
        {
        LOG_FUNC << "failed to open " << filename << "\n";
        return(false);
        }
    
    file >> ModifyPrefsSameRealm() >> std::ws
         >> ModifyPrefsEnemyRealm() >> std::ws
         >> ModifyPrefsMob() >> std::ws
         >> ModifyRaisePriority() >> std::ws
         >> ModifyUseZoneTextures() >> std::ws
         >> ModifyUseVectorMaps() >> std::ws
         >> ModifyUpdateActorsOnRender() >> std::ws
         >> ModifyShowAlbs() >> std::ws
         >> ModifyShowHibs() >> std::ws
         >> ModifyShowMids() >> std::ws
         >> ModifyShowMobs() >> std::ws
         >> ModifyShowObjects() >> std::ws;

    std::getline(file,ModifyDStreamServer());
    file >> ModifyDStreamServerPort() >> std::ws;
    
    std::getline(file,ModifySharenetServer());
    file >> ModifySharenetServerPort() >> std::ws;
    
    std::getline(file,ModifyNewAlbSound());
    file >> ModifyPlayAlbSound() >> std::ws;
    
    std::getline(file,ModifyNewHibSound());
    file >> ModifyPlayHibSound() >> std::ws;
    
    std::getline(file,ModifyNewMidSound());
    file >> ModifyPlayMidSound() >> std::ws;
    
    std::getline(file,ModifyNamedMobSound());
    file >> ModifyPlayNamedMobSound() >> std::ws;
    std::getline(file,ModifyNamedMobName());
    
    file >> ModifyShowRangeRing1() >> std::ws;
    file >> ModifyRangeRingRange1() >> std::ws;
    
    file >> ModifyShowRangeRing2() >> std::ws;
    file >> ModifyRangeRingRange2() >> std::ws;

    file >> ModifyShowRangeRing3() >> std::ws;
    file >> ModifyRangeRingRange3() >> std::ws;

    file >> ModifyShowRangeRing4() >> std::ws;
    file >> ModifyRangeRingRange4() >> std::ws;

    file >> ModifyShowRangeRing5() >> std::ws;
    file >> ModifyRangeRingRange5() >> std::ws;

    file >> ModifyShowRangeRing6() >> std::ws;
    file >> ModifyRangeRingRange6() >> std::ws;
    
    file >> ModifySimplifyLines() >> std::ws;
    file >> ModifySimplifyLinesTolerance() >> std::ws;
    
    file >> ModifySaveDAoCMessages() >> std::ws;
    file >> ModifySaveChatMessages() >> std::ws;
    
    // done
    return(true);
} // end Load

bool Config::Save(const std::string& filename)const
{
    std::ofstream file(filename.c_str());
    
    if(!file.is_open())
        {
        LOG_FUNC << "failed to open " << filename << "\n";
        return(false);
        }
    
    file << GetPrefsSameRealm() << std::endl
         << GetPrefsEnemyRealm() << std::endl
         << GetPrefsMob() << std::endl
         << GetRaisePriority() << std::endl
         << GetUseZoneTextures() << std::endl
         << GetUseVectorMaps() << std::endl
         << GetUpdateActorsOnRender() << std::endl
         << GetShowAlbs() << std::endl
         << GetShowHibs() << std::endl
         << GetShowMids() << std::endl
         << GetShowMobs() << std::endl
         << GetShowObjects() << std::endl
         << (GetDStreamServer().length()? GetDStreamServer():"<no server>") << std::endl
         << GetDStreamServerPort() << std::endl
         << (GetSharenetServer().length()? GetSharenetServer():"<no server>") << std::endl
         << GetSharenetServerPort() << std::endl
         << (GetNewAlbSound().length()? GetNewAlbSound():"<no sound>") << std::endl
         << GetPlayAlbSound() << std::endl
         << (GetNewHibSound().length()? GetNewHibSound():"<no sound>") << std::endl
         << GetPlayHibSound() << std::endl
         << (GetNewMidSound().length()? GetNewMidSound():"<no sound>") << std::endl
         << GetPlayMidSound() << std::endl
         << (GetNamedMobSound().length()? GetNamedMobSound():"<no sound>") << std::endl
         << GetPlayNamedMobSound() << std::endl
         << (GetNamedMobName().length()? GetNamedMobName():"<no mob name>") << std::endl
         << GetShowRangeRing1() << std::endl
         << GetRangeRingRange1() << std::endl
         << GetShowRangeRing2() << std::endl
         << GetRangeRingRange2() << std::endl
         << GetShowRangeRing3() << std::endl
         << GetRangeRingRange3() << std::endl
         << GetShowRangeRing4() << std::endl
         << GetRangeRingRange4() << std::endl
         << GetShowRangeRing5() << std::endl
         << GetRangeRingRange5() << std::endl
         << GetShowRangeRing6() << std::endl
         << GetRangeRingRange6() << std::endl
         << GetSimplifyLines() << std::endl
         << GetSimplifyLinesTolerance() << std::endl
         << GetSaveDAoCMessages() << std::endl
         << GetSaveChatMessages() << std::endl;

    // done
    return(true);
} // end Save

void Config::set(const Config& s)
{
    MEMBER_ASSIGN(PrefsSameRealm);
    MEMBER_ASSIGN(PrefsEnemyRealm);
    MEMBER_ASSIGN(PrefsMob);
    MEMBER_ASSIGN(RaisePriority);
    MEMBER_ASSIGN(UseZoneTextures);
    MEMBER_ASSIGN(UseVectorMaps);
    MEMBER_ASSIGN(UpdateActorsOnRender);
    MEMBER_ASSIGN(ShowAlbs);
    MEMBER_ASSIGN(ShowHibs);
    MEMBER_ASSIGN(ShowMids);
    MEMBER_ASSIGN(ShowMobs);
    MEMBER_ASSIGN(ShowObjects);
    MEMBER_ASSIGN(DStreamServer);
    MEMBER_ASSIGN(DStreamServerPort);
    MEMBER_ASSIGN(SharenetServer);
    MEMBER_ASSIGN(SharenetServerPort);
    MEMBER_ASSIGN(NewAlbSound);
    MEMBER_ASSIGN(PlayAlbSound);
    MEMBER_ASSIGN(NewHibSound);
    MEMBER_ASSIGN(PlayHibSound);
    MEMBER_ASSIGN(NewMidSound);
    MEMBER_ASSIGN(PlayMidSound);
    MEMBER_ASSIGN(NamedMobSound);
    MEMBER_ASSIGN(PlayNamedMobSound);
    MEMBER_ASSIGN(NamedMobName);
    MEMBER_ASSIGN(ShowRangeRing1);
    MEMBER_ASSIGN(RangeRingRange1);
    MEMBER_ASSIGN(ShowRangeRing2);
    MEMBER_ASSIGN(RangeRingRange2);
    MEMBER_ASSIGN(ShowRangeRing3);
    MEMBER_ASSIGN(RangeRingRange3);
    MEMBER_ASSIGN(ShowRangeRing4);
    MEMBER_ASSIGN(RangeRingRange4);
    MEMBER_ASSIGN(ShowRangeRing5);
    MEMBER_ASSIGN(RangeRingRange5);
    MEMBER_ASSIGN(ShowRangeRing6);
    MEMBER_ASSIGN(RangeRingRange6);
    MEMBER_ASSIGN(SimplifyLines);
    MEMBER_ASSIGN(SimplifyLinesTolerance);
    MEMBER_ASSIGN(SaveDAoCMessages);
    MEMBER_ASSIGN(SaveChatMessages);

} // end set
