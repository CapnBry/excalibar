/*
 * Copyright 2002 the Excalibur contributors (http://excalibar.sourceforge.net/)
 *
 * Portions of this software are based on the work of Slicer/Hackersquest.
 * Those portions, Copyright 2001 Slicer/Hackersquest <slicer@hackersquest.org)
 * 
 * This file is part of Excalibur.
 *
 * Excalibur is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Excalibur is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */

#ifndef _EXCONSYSTEM_H
#define _EXCONSYSTEM_H

#ifdef __cplusplus
  extern "C" {
#endif

struct con_range_definition {
    short gray_max;
    short green_max;
    short blue_max;
    short yellow_max;
    short orange_max;
    short red_max;
};
typedef struct con_range_definition CON_RANGE_DEFINITION;

enum concolors {
    ccGray,
    ccGreen,
    ccBlue,
    ccYellow,
    ccOrange,
    ccRed,
    ccPurple
};
typedef enum concolors CONCOLOR;

CONCOLOR concolor_for_level(int AObserverLvl, int ATargetLvl);

#ifdef __cplusplus
  }
#endif

#endif
