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
#include "stdafx.h"
#include "MobsightParseAPI.h"

MOBSIGHTPARSE_API MobsightContentHandler* CreateMobsightContentHandler(void)
{
    return(new MobsightContentHandler);
} // end CreateMobsightContentHandler

MOBSIGHTPARSE_API MobsightIdHandler* CreateMobsightIdHandler(void)
{
    return(new MobsightIdHandler);
} // end CreateMobsightIdHandler

MOBSIGHTPARSE_API void DestroyMobsightContentHandler(MobsightContentHandler* p)
{
    delete p;
} // end DestroyMobsightContentHandler

MOBSIGHTPARSE_API void DestroyMobsightIdHandler(MobsightIdHandler* p)
{
    delete p;
} // end DestroyMobsightIdHandler

MOBSIGHTPARSE_API bool GetMobInfo
    (
    MobsightIdHandler& obj,
    const int Id,
    NarrowIdDef& MobInfo
    )
{
    return(obj.GetMobInfo(Id,MobInfo));
} // end GetMobInfo

MOBSIGHTPARSE_API bool BuildMobList
    (
    MobsightContentHandler& obj,
    const MobsightContentHandler::REALM realm,
    const unsigned char MinLevel,
    const unsigned char MaxLevel,
    std::list<NarrowMobDef>& List
    )
{
    return(obj.BuildMobList(realm,MinLevel,MaxLevel,List));
} // end GetMobInfo