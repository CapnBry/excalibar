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

#include "..\Utils\CodeUtils.h" // for member macros
#include <string> // for string defs

class Config
{
public:
    Config();
    Config(const Config& s)
    {
        set(s);
    }

    ~Config(){};

    Config& operator=(const Config& s)
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

protected:
private:
    bool operator==(const Config& s); // disallow

    void set(const Config& s)
    {
        MEMBER_ASSIGN(ConfigFileName);
        MEMBER_ASSIGN(ServerPort);
    }

    DECL_MEMBER(std::string,ConfigFileName);
    DECL_MEMBER(unsigned short,ServerPort);
    
}; // end class Config