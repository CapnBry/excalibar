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

#ifndef _EXCALIBUR_H
#define _EXCALIBUR_H

#include <qmodules.h>
#ifndef QT_MODULE_NETWORK
#define QT_MODULE_NETWORK
#endif

#undef ALPHA_QUALITY
#ifdef ALPHA_QUALITY
#warning "This is a ALPHA quality release. It most likely WILL break."
#endif

#include "exPrefs.h"

#define EX_VERSION "1.0.5"

typedef long int exTimeType;
extern exTimeType exTick;
extern void updateTick();
extern exPrefs prefs;

enum Realm {
    rFriend,
    rAlbion,
    rMidgaard,
    rHibernia
};
        
#endif
