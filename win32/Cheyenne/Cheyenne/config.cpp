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

    SetTexturesInPPI(true);
    SetVectorMapInPPI(true);
    SetVectorMapOnlyInFollowedZone(false);
    SetAutoHookTarget(true);
    SetRenderGrayMobs(true);
    
    SetShareNetAddress("127.0.0.1");
    SetShareNetPort("10001");
    
    SetNamedMobCreateSoundFile(std::string(""));
    SetPlaySoundOnNamedMobCreate(false);
    SetNamedMob(std::string(""));
    
    SetMobsightURL(std::string("http://capnbry.net/daoc/mobs.php"));

} // end CheyenneConfig

bool CheyenneConfig::Load(void)
{
    std::fstream file;
    
    
    std::string file_name(::InitialDir);
    file_name+=GetConfigFileName();
    
    file.open(file_name.c_str(),std::ios_base::in);

    if(!file.is_open())
        {
        Logger << "[CheyenneConfig::Load] unable to open config file!\n";
        return(false);
        }

    //file.seekg(0,std::ios::beg);

    // this must be in the same order as Save()

    file >> ModifyPPIText() >> std::ws;
    file >> ModifySurnameInPPI() >> std::ws;
    file >> ModifyGuildInPPI() >> std::ws;
    file >> ModifyLevelInPPI() >> std::ws;
    file >> ModifyAutoFollowFirstActor() >> std::ws;
    file >> ModifyRenderMOBs() >> std::ws;
    file >> ModifyRenderPlayers() >> std::ws;
    file >> ModifyRenderObjects() >> std::ws;
    file >> ModifyRaisePriority() >> std::ws;
    file >> ModifyMatchFollowedHeading() >> std::ws;
    file >> ModifyHealthInPPI() >> std::ws;

    for(int i=0;i<6;++i)
        {
        file >> ModifyRangeRings().Rings[i].bEnabled >> std::ws;
        file >> ModifyRangeRings().Rings[i].Radius >> std::ws;
        }

    file >> ModifyPlaySoundOnAlbCreate() >> std::ws;
    ::GetLine(file,ModifyAlbSoundFile());
    //file >> ModifyAlbSoundFile();

    file >> ModifyPlaySoundOnHibCreate() >> std::ws;
    ::GetLine(file,ModifyHibSoundFile());
    //file >> ModifyHibSoundFile();

    file >> ModifyPlaySoundOnMidCreate() >> std::ws;
    ::GetLine(file,ModifyMidSoundFile());
    //file >> ModifyMidSoundFile();

    file >> ModifyDrawDeadActors() >> std::ws;

    file >> ModifyUpdateWhenRendered() >> std::ws;

    file >> ModifyTexturesInPPI() >> std::ws;
    file >> ModifyVectorMapInPPI() >> std::ws;
    file >> ModifyVectorMapOnlyInFollowedZone() >> std::ws;

    file >> ModifyAutoHookTarget() >> std::ws;
    
    file >> ModifyRenderGrayMobs() >> std::ws;
    
    file >> ModifyShareNetAddress() >> std::ws;
    file >> ModifyShareNetPort() >> std::ws;
    
    file >> ModifyPlaySoundOnNamedMobCreate() >> std::ws;
    ::GetLine(file,ModifyNamedMobCreateSoundFile());
    //file >> ModifyNamedMobCreateSoundFile();
    ::GetLine(file,ModifyNamedMob());
    //file >> ModifyNamedMob();
    
    std::getline(file,ModifyMobsightURL());
    //::GetLine(file,ModifyMobsightURL());

    // UNKNOWN PACKET LOG FLAG IS NOT STORED IN THE CONFIG FILE
    
    file.close();

    // done
    return(true);
} // end Load

bool CheyenneConfig::Save(void)const
{
    std::fstream file;
    
    std::string file_name(::InitialDir);
    file_name+=GetConfigFileName();
    
    SetLastError(ERROR_SUCCESS);
    file.open(file_name.c_str(),std::ios_base::out);

    if(!file.is_open())
        {
        Logger << "[CheyenneConfig::Save] unable to save config file (" << GetLastError() << ")!\n";
        return(false);
        }

    //file.seekp(0,std::ios::beg);

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

    file << GetTexturesInPPI() << std::endl;
    file << GetVectorMapInPPI() << std::endl;
    file << GetVectorMapOnlyInFollowedZone() << std::endl;

    file << GetAutoHookTarget() << std::endl;
    
    file << GetRenderGrayMobs() << std::endl;
    
    file << GetShareNetAddress() << std::endl;
    file << GetShareNetPort() << std::endl;
    
    file << GetPlaySoundOnNamedMobCreate() << std::endl;
    if(GetNamedMobCreateSoundFile().length())
        {
        file << GetNamedMobCreateSoundFile() << std::endl;
        }
    else
        {
        file << "no_sound" << std::endl;
        }
    
    if(GetNamedMob().length())
        {
        file << GetNamedMob() << std::endl;
        }
    else
        {
        file << "no_mob" << std::endl;
        }
    
    file << GetMobsightURL() << std::endl;
    
    // UNKNOWN PACKET LOG FLAG IS NOT STORED IN THE CONFIG FILE
    
    if(file.bad() || !file.good() || file.fail())
        {
        Logger << "[CheyenneConfig::Save] config saved, but the file is no good!\n";
        }
    
    file.close();
    
    Logger << "[CheyenneConfig::Save] config saved\n";
    
    // done
    return(true);
} // end Save
