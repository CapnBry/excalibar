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

/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

#include <qmessagebox.h>

#include "exMob.h"

void PrefsDialog::SliderAliveColor_valueChanged(int v)
{
    FriendAlive->setPaletteBackgroundColor(exMob::getColor(rFriend).light(v));
    HibAlive->setPaletteBackgroundColor(exMob::getColor(rHibernia).light(v));
    AlbAlive->setPaletteBackgroundColor(exMob::getColor(rAlbion).light(v));
    MidAlive->setPaletteBackgroundColor(exMob::getColor(rMidgaard).light(v));
    FriendAlive->repaint();
    HibAlive->repaint();
    AlbAlive->repaint();
    MidAlive->repaint();
}

void PrefsDialog::SliderDeadColor_valueChanged( int v)
{
    FriendDead->setPaletteBackgroundColor(exMob::getColor(rFriend).light(v));
    HibDead->setPaletteBackgroundColor(exMob::getColor(rHibernia).light(v));
    AlbDead->setPaletteBackgroundColor(exMob::getColor(rAlbion).light(v));
    MidDead->setPaletteBackgroundColor(exMob::getColor(rMidgaard).light(v));
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


void PrefsDialog::PrefsDialog_destroyed( QObject * )
{

}

void PrefsDialog::TextLabel1_3_destroyed( QObject * )
{

}


