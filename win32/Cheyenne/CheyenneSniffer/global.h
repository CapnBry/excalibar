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
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h> // grab windows headers + socket headers
#include <string>
#include "..\Utils\Logger.h" // the logger
#include "..\Utils\Times.h" // for the clock
#include "..\Utils\Mapinfo.h" // for the map info
#include "config.h" // for the config

#ifdef MAIN_FUNCTION
    #define EXTERN
#else
    #define EXTERN extern
#endif

EXTERN std::string InitialDir; // initial directory we were started in
EXTERN logger_t Logger; // the global logger
EXTERN CheyenneClock Clock; // global clock
EXTERN MapInfo Zones; // global zone info
EXTERN Config g_Config; // global config
