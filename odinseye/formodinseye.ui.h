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

#include <qgl.h>
#include <qsettings.h>
#include <qvaluelist.h>
#include <qmessagebox.h>
#include <qaction.h>
#include <qcolor.h>
#include "odinseye.h"
#include "oeItem.h"

void FormOdinsEye::resizeEvent(QResizeEvent *e) {
    HSplitter->resize(centralWidget()->size());
    QWidget::resizeEvent(e);
 }

void FormOdinsEye::showEvent(QShowEvent *e) {
    HSplitter->resize(centralWidget()->size());
    QMainWindow::showEvent(e);
}

void FormOdinsEye::init()
{
    prefs.addWindow(this);
}

void FormOdinsEye::destroy()
{
    prefs.removeWindow(this);
    oeItem::save();
    oeItem::upload(FALSE);
}

void FormOdinsEye::ReloadMaps_activated()
{
  oeMapInfo::setup("maps/mapinfo.txt");
  Map->setMap(NULL);
}


void FormOdinsEye::MapSlider_valueChanged( int range)
{
  prefs.map_range=Map->range=range;
  Map->dirty();
}

void FormOdinsEye::GroupPlayers_toggled( bool ena )
{
  prefs.sort_group_players=ena;
  prefs.activate();
}

void FormOdinsEye::GLObjectSizes_selected( QAction * act)
{
    if (act == GLObjectsSmall)
      Map->setObjectSize(40);
    else if (act == GLObjectsMedium)
      Map->setObjectSize(80);
    else
      Map->setObjectSize(160);
}

void FormOdinsEye::SortDistance_toggled( bool ena )
{
  prefs.sort_distance=ena;
  prefs.activate();
}

void FormOdinsEye::Preferences_activated()
{
  prefs.show();
}

void FormOdinsEye::GLRulers_toggled( bool ena)
{
  prefs.map_rulers = ena;
  prefs.activate();
}

void FormOdinsEye::ItemUpload_activated()
{
    oeItem::upload(TRUE);
}


void FormOdinsEye::GLMapFade_toggled( bool ena )
{
  prefs.map_fade = ena;
  prefs.activate();
}

void FormOdinsEye::ProfileExit_activated()
{
  exit(0);
}


void FormOdinsEye::EditorSpawn_activated()
{
  Map->c->spawnEditor();
}
