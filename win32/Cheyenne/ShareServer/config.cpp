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
#include <fstream>

ShareNetConfig::ShareNetConfig() : m_ConfigFileName("sharenet.cfg")
{
    SetListenPort(10001);
} // end ShareNetConfig

bool ShareNetConfig::Load(void)
{
    std::fstream file(GetConfigFileName().c_str(),std::ios::in);

    if(!file.is_open())
        {
        return(false);
        }

    file.seekg(0,std::ios::beg);
    
    int num_hosts;
    // get number of hosts
    file >> num_hosts;
    
    std::string host;
    std::string mask;
    for(int i=0;i<num_hosts;++i)
        {
        file >> host;
        file >> mask;
        host_list_value_type host_mask_pair(host,mask);
        ModifyAllowedHostsList().insert(ModifyAllowedHostsList().end(),host_mask_pair);
        } // end for all hosts

    file >> ModifyListenPort(); 

    return(true);
} // end Load

bool ShareNetConfig::Save(void)const
{
    std::fstream file(GetConfigFileName().c_str(),std::ios::out);

    if(!file.is_open())
        {
        return(false);
        }

    file.seekp(0,std::ios::beg);
    
    // save number of hosts
    file << GetAllowedHostsList().size() << std::endl;
    host_list_type::const_iterator it;
    for(it=GetAllowedHostsList().begin();it!=GetAllowedHostsList().end();++it)
        {
        file << it->first << " " << it->second << std::endl;
        } // end for each allowed host
    
    file << GetListenPort() << std::endl;
    
    // done
    return(true);
} // end save

bool ShareNetConfig::IsHostAllowed(const SOCKADDR_IN& Host)const
{
    // compare Host to all allowed host/mask pairs
    ShareNetConfig::host_list_type::const_iterator it;
    
    for(it=GetAllowedHostsList().begin();it!=GetAllowedHostsList().end();++it)
        {
        const unsigned long Mask(inet_addr(it->second.c_str()));
        const unsigned long MaskedHost(Host.sin_addr.S_un.S_addr & Mask);
        const unsigned long AllowedHost(inet_addr(it->first.c_str()));
        
        if(MaskedHost == AllowedHost)
            {
            // match: allow it!
            return(true);
            }
        } // end for all allowed hosts
    
    // done: not found return false
    return(false);
} // end IsHostAllowed
