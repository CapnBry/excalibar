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
// get rid of the stupid
// "identifier truncated" warnings
#pragma warning(disable : 4786)

#include "global.h"
#include "config.h"
#include <iosfwd>
#include <fstream>

CheyenneConfig::CheyenneConfig() : m_ConfigFileName("cheyenne.cfg")
{
    SetPPIText(true);
    SetSurnameInPPI(true);
    SetGuildInPPI(true);
    SetLevelInPPI(true);
    SetAutoFollowFirstActor(true);
    SetRenderMOBs(true);
    SetRenderPlayers(true);
    SetRenderObjects(true);
    SetRaisePriority(false);
    SetMatchFollowedHeading(false);
    SetHealthInPPI(true);
    ModifyRangeRings().Rings[0].Radius=100;
    ModifyRangeRings().Rings[0].bEnabled=false;
    ModifyRangeRings().Rings[1].Radius=100;
    ModifyRangeRings().Rings[1].bEnabled=false;
    ModifyRangeRings().Rings[2].Radius=100;
    ModifyRangeRings().Rings[2].bEnabled=false;
    ModifyRangeRings().Rings[3].Radius=100;
    ModifyRangeRings().Rings[3].bEnabled=false;
    ModifyRangeRings().Rings[4].Radius=100;
    ModifyRangeRings().Rings[4].bEnabled=false;
    ModifyRangeRings().Rings[5].Radius=100;
    ModifyRangeRings().Rings[5].bEnabled=false;

    SetPlaySoundOnAlbCreate(false);
    SetAlbSoundFile(std::string(""));
    SetPlaySoundOnHibCreate(false);
    SetHibSoundFile(std::string(""));
    SetPlaySoundOnMidCreate(false);
    SetMidSoundFile(std::string(""));

    SetLogUnknownPackets(false);

    SetDrawDeadActors(true);

    SetUpdateWhenRendered(false);

} // end CheyenneConfig

bool CheyenneConfig::Load(void)
{
    std::fstream file(GetConfigFileName().c_str(),std::ios::in);

    if(!file.is_open())
        {
        return(false);
        }

    file.seekg(0,std::ios::beg);

    // this must be in the same order as Save()

    file >> ModifyPPIText();
    file >> ModifySurnameInPPI();
    file >> ModifyGuildInPPI();
    file >> ModifyLevelInPPI();
    file >> ModifyAutoFollowFirstActor();
    file >> ModifyRenderMOBs();
    file >> ModifyRenderPlayers();
    file >> ModifyRenderObjects();
    file >> ModifyRaisePriority();
    file >> ModifyMatchFollowedHeading();
    file >> ModifyHealthInPPI();

    for(int i=0;i<6;++i)
        {
        file >> ModifyRangeRings().Rings[i].bEnabled;
        file >> ModifyRangeRings().Rings[i].Radius;
        }

    file >> ModifyPlaySoundOnAlbCreate();
    file >> ModifyAlbSoundFile();

    file >> ModifyPlaySoundOnHibCreate();
    file >> ModifyHibSoundFile();

    file >> ModifyPlaySoundOnMidCreate();
    file >> ModifyMidSoundFile();

    file >> ModifyDrawDeadActors();

    file >> ModifyUpdateWhenRendered();

    // UNKNOWN PACKET LOG FLAG IS NOT STORED IN THE CONFIG FILE

    // done
    return(true);
} // end Load

bool CheyenneConfig::Save(void)const
{
    std::fstream file(GetConfigFileName().c_str(),std::ios::out);

    if(!file.is_open())
        {
        return(false);
        }

    file.seekp(0,std::ios::beg);

    // this must be in the same order as Load

    file << GetPPIText() << std::endl;
    file << GetSurnameInPPI() << std::endl;
    file << GetGuildInPPI() << std::endl;
    file << GetLevelInPPI() << std::endl;
    file << GetAutoFollowFirstActor() << std::endl;
    file << GetRenderMOBs() << std::endl;
    file << GetRenderPlayers() << std::endl;
    file << GetRenderObjects() << std::endl;
    file << GetRaisePriority() << std::endl;
    file << GetMatchFollowedHeading() << std::endl;
    file << GetHealthInPPI() << std::endl;

    for(int i=0;i<6;++i)
        {
        file << GetRangeRings().Rings[i].bEnabled << std::endl;
        file << GetRangeRings().Rings[i].Radius << std::endl;
        }

    file << GetPlaySoundOnAlbCreate() << std::endl;
    if(GetAlbSoundFile().length())
        {
        file << GetAlbSoundFile() << std::endl;
        }
    else
        {
        file << "no_sound" << std::endl;
        }

    file << GetPlaySoundOnHibCreate() << std::endl;
    if(GetHibSoundFile().length())
        {
        file << GetHibSoundFile() << std::endl;
        }
    else
        {
        file << "no_sound" << std::endl;
        }

    file << GetPlaySoundOnMidCreate() << std::endl;
    if(GetMidSoundFile().length())
        {
        file << GetMidSoundFile() << std::endl;
        }
    else
        {
        file << "no_sound" << std::endl;
        }

    file << GetDrawDeadActors() << std::endl;

    file << GetUpdateWhenRendered() << std::endl;

    // UNKNOWN PACKET LOG FLAG IS NOT STORED IN THE CONFIG FILE

    // done
    return(true);
} // end Save
