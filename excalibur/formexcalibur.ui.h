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

#include <qgl.h>
#include <qsettings.h>
#include <qvaluelist.h>
#include <qmessagebox.h>
#include <qstatusbar.h>
#include <qlabel.h>
#include <qlabel.h>
#include <qaction.h>
#include <qcolor.h>
#include "excalibur.h"
#include "exItem.h"
#include "exMob.h"

void FormExcalibur::resizeEvent(QResizeEvent *e) {
    HSplitter->resize(centralWidget()->size());
    QWidget::resizeEvent(e);
 }

void FormExcalibur::showEvent(QShowEvent *e) { 
    HSplitter->resize(centralWidget()->size());
    QMainWindow::showEvent(e);
}

void FormExcalibur::init()
{
    prefs.addWindow(this);

    Zone      = new QLabel( statusBar(), "Zone" );
    xyzstatus = new QLabel( statusBar(), "xyzstatus" );

    xyzstatus->setMinimumWidth(125);
    xyzstatus->setAlignment(AlignHCenter);
    Zone->setMinimumWidth(110);
    Zone->setAlignment(AlignHCenter);

    statusBar()->addWidget(Zone, 0, TRUE);
    statusBar()->addWidget(xyzstatus, 0, TRUE);

}

void FormExcalibur::destroy()
{
    prefs.removeWindow(this);
    exItem::save();
    exItem::upload(FALSE);
}

void FormExcalibur::ReloadMaps_activated()
{
  exMapInfo::setup("maps/mapinfo.txt");
  Map->setMap(NULL);
}


void FormExcalibur::MapSlider_valueChanged( int range)
{
  prefs.map_range=Map->range=range;
  Map->dirty();
}

void FormExcalibur::GroupPlayers_toggled( bool ena )
{
  prefs.sort_group_players=ena;
  prefs.activate();
}

void FormExcalibur::GLObjectSizes_selected( QAction * act)
{
    if (act == GLObjectsSmall)
      Map->setObjectSize(40);
    else if (act == GLObjectsMedium)
      Map->setObjectSize(80);
    else
      Map->setObjectSize(160);
}

void FormExcalibur::SortDistance_toggled( bool ena )
{
  prefs.sort_distance=ena;
  prefs.activate();
}

void FormExcalibur::Preferences_activated()
{
  prefs.show();
}

void FormExcalibur::GLRulers_toggled( bool ena)
{
  prefs.map_rulers = ena;
  prefs.activate();
}

void FormExcalibur::ItemUpload_activated()
{
    exItem::upload(TRUE);
}


void FormExcalibur::GLMapFade_toggled( bool ena )
{
  prefs.map_fade = ena;
  prefs.activate();
}

void FormExcalibur::ProfileExit_activated()
{
  exit(0);
}


void FormExcalibur::vaderWarn_toggled( bool ena)
{
  if( Map->c)
  	Map->c->vaderWarn = ena;
  prefs.vaderWarn = ena;
}

void FormExcalibur::MobFilter_returnPressed()
{
  if( !Map->c) return;
  
  Map->c->setFilter( MobFilter->text());
}


void FormExcalibur::showMsgs( )
{
    Map->c->msgui->show( );
}
