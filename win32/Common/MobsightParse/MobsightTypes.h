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
#include <list>

class IdDef
{
public:
    int id;
    std::wstring name;

    class Loc
    {
    public:
        int zone;
        unsigned int x;
        unsigned int y;
        unsigned int z;
    };

    std::list<Loc> loc_list;

    Loc CurrentLoc;
};

class NarrowIdDef
{
public:
    typedef std::list<IdDef::Loc> loc_list_t;
    int id;
    std::string name;
    loc_list_t loc_list;
};

class MobDef
{
public:
int id;
std::wstring name;
};

class NarrowMobDef
{
public:
int id;
std::string name;
};
