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

Config::Config()
{
    SetConfigFileName(std::string("cheyenne_sniffer.cfg"));
    SetServerPort(10001);
    
} // end Config

bool Config::Load()
{
    std::fstream file;
    
    
    std::string file_name(::InitialDir);
    file_name+=GetConfigFileName();
    
    file.open(file_name.c_str(),std::ios_base::in);

    if(!file.is_open())
        {
        Logger << "[Config::Load] unable to open config file!\n";
        return(false);
        }

    // this must be in the same order as Save
    file >> ModifyServerPort();

    file.close();

    // done
    return(true);
} // end Load

bool Config::Save(void)const
{
    std::fstream file;
    
    std::string file_name(::InitialDir);
    file_name+=GetConfigFileName();
    
    SetLastError(ERROR_SUCCESS);
    file.open(file_name.c_str(),std::ios_base::out);

    if(!file.is_open())
        {
        Logger << "[Config::Save] unable to save config file (" << GetLastError() << ")!\n";
        return(false);
        }

    // this must be in the same order as Load
    file << GetServerPort() << std::endl;

    if(file.bad() || !file.good() || file.fail())
        {
        Logger << "[Config::Save] config saved, but the file is no good!\n";
        }
    
    file.close();
    
    Logger << "[Config::Save] config saved\n";
    
    // done
    return(true);
} // end Save
