/*
 * Copyright 2001 Slicer/HackersQuest (slicer@hackersquest.org)
 *
 * This file is part of Odin's Eye.
 *
 * Odin's Eye is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Odin's Eye is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */

#ifndef _ODINSEYE_H
#define _ODINSEYE_H

#undef ALPHA_QUALITY
#ifdef ALPHA_QUALITY
#warning "This is a ALPHA quality release. It most likely WILL break."
#endif

#include "oePrefs.h"

#define OE_CURRENT_VERSION 79

typedef long int oeTimeType;
extern oeTimeType oeTick;
extern void updateTick();
extern oePrefs prefs;

enum Realm {
    rFriend,
    rAlbion,
    rMidgaard,
    rHibernia
};
        
#endif
