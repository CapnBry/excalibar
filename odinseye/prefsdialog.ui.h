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

/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

#include <qmessagebox.h>

#include "oeMob.h"

void PrefsDialog::SliderAliveColor_valueChanged(int v)
{
    FriendAlive->setPaletteBackgroundColor(oeMob::getColor(rFriend).light(v));
    HibAlive->setPaletteBackgroundColor(oeMob::getColor(rHibernia).light(v));
    AlbAlive->setPaletteBackgroundColor(oeMob::getColor(rAlbion).light(v));
    MidAlive->setPaletteBackgroundColor(oeMob::getColor(rMidgaard).light(v));
    FriendAlive->repaint();
    HibAlive->repaint();
    AlbAlive->repaint();
    MidAlive->repaint();
}

void PrefsDialog::SliderDeadColor_valueChanged( int v)
{
    FriendDead->setPaletteBackgroundColor(oeMob::getColor(rFriend).light(v));
    HibDead->setPaletteBackgroundColor(oeMob::getColor(rHibernia).light(v));
    AlbDead->setPaletteBackgroundColor(oeMob::getColor(rAlbion).light(v));
    MidDead->setPaletteBackgroundColor(oeMob::getColor(rMidgaard).light(v));
    FriendDead->repaint();
    HibDead->repaint();
    AlbDead->repaint();
    MidDead->repaint();
}


void PrefsDialog::ItemAutoUpload_toggled( bool)
{
}

void PrefsDialog::MapSimplifyRange_valueChanged( int val)
{
    MapSimplifyRangeLabel->setText(QString("Tolerance: ").append(QString::number(val)));
}
