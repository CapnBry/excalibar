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

class ShareNetConfig
{
public:
    ShareNetConfig();
    ShareNetConfig(const ShareNetConfig& s) : m_ConfigFileName(s.m_ConfigFileName)
    {
        set(s);
    }

    ~ShareNetConfig()
    {
    };

    ShareNetConfig& operator=(const ShareNetConfig& s)
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
    
    typedef std::pair<std::string,std::string> host_list_value_type;
    typedef std::list<host_list_value_type> host_list_type;
    
    void EraseAllowedHosts(void)
    {
        ModifyAllowedHostsList().clear();
    }
    void InsertAllowedHost(const host_list_value_type& value)
    {
        ModifyAllowedHostsList().insert(ModifyAllowedHostsList().begin(),value);
    }

    bool IsHostAllowed(const SOCKADDR_IN& Host)const;
protected:
private:
    bool operator==(const ShareNetConfig& s);

    void set(const ShareNetConfig& s)
    {
        MEMBER_ASSIGN(ConfigFileName);
        MEMBER_ASSIGN(AllowedHostsList);
        MEMBER_ASSIGN(ListenPort);
    }

    DECL_MEMBER(std::string,ConfigFileName);
    DECL_MEMBER(host_list_type,AllowedHostsList);
    DECL_MEMBER(unsigned short,ListenPort);
}; // end class ShareNetConfig