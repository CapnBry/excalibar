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
         >> ModifyUseVectorMaps() >> std::ws;
    
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
         << GetUseVectorMaps() << std::endl;
    
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

} // end set
