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
#include "MobsightParse.h"
#include "IMobsightContentHandler.h"
#include "IMobsightIdHandler.h"

/*
This file contains the public API for the MobsightParse library. The
functions here are "C" accessible so you can use LoadLibrary() and 
GetProcAddress() on them. The objects referenced here are also 
part of the public API. This should make some language integration
a bit easier.
*/

#ifdef __cplusplus
extern "C" 
{
#endif

MOBSIGHTPARSE_API MobsightContentHandler* CreateMobsightContentHandler(void);
MOBSIGHTPARSE_API MobsightIdHandler* CreateMobsightIdHandler(void);
MOBSIGHTPARSE_API void DestroyMobsightContentHandler(MobsightContentHandler* p);
MOBSIGHTPARSE_API void DestroyMobsightIdHandler(MobsightIdHandler* p);
MOBSIGHTPARSE_API bool GetMobInfo(MobsightIdHandler& obj,const int Id,NarrowIdDef& MobInfo);
MOBSIGHTPARSE_API bool BuildMobList(MobsightContentHandler& obj,const MobsightContentHandler::REALM realm,const unsigned char MinLevel,const unsigned char MaxLevel,std::list<NarrowMobDef>& List);

#ifdef __cplusplus
}
#endif
