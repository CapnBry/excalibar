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
#include <qpushbutton.h>
#include <qprogressbar.h>
#include <qaction.h>
#include <qcolor.h>
#include <qframe.h>
#include "excalibur.h"
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

    Zoom       = new QLabel( statusBar(), "Zoom");
    FPS         = new QLabel( statusBar(), "FPS" );
    Zone        = new QLabel( statusBar(), "Zone" );
    xyzstatus   = new QLabel( statusBar(), "xyzstatus" );
    MapStatus   = new QLabel( statusBar(), "MapStatus" );
    MapProgress = new QProgressBar( statusBar(), "MapProgress" );
    MapCancel   = new QPushButton( "Cancel", statusBar(), "MapCancel" );

    Zoom->setMinimumWidth(100);
    Zoom->setAlignment(AlignHCenter | AlignTop);
    xyzstatus->setMinimumWidth(125);
    xyzstatus->setAlignment(AlignHCenter | AlignTop);
    Zone->setMinimumWidth(110);
    Zone->setAlignment(AlignHCenter | AlignTop);
    FPS->setMinimumWidth(60);
    FPS->setAlignment(AlignHCenter | AlignTop);
    MapStatus->setMinimumWidth(150);
    MapStatus->setAlignment(AlignHCenter | AlignTop);
    MapProgress->setMinimumWidth(100);
    MapCancel->setMinimumWidth(40);

      /* Set the height of the status bar to the height of the contained font */
    QFontMetrics bar_font_metrics(statusBar()->font());
      /* we add a little fudge factor for the bevels */
    statusBar()->setMaximumHeight(bar_font_metrics.height() + 4);
    
    connect(MapCancel, SIGNAL(pressed()), this, SLOT(MapCancel_pressed()));

    statusBar()->addWidget(Zoom       , 0, TRUE);
    statusBar()->addWidget(MapStatus  , 0, TRUE);
    statusBar()->addWidget(MapProgress, 0, TRUE);
    statusBar()->addWidget(MapCancel,   0, TRUE);
    statusBar()->addWidget(FPS,         0, TRUE);
    statusBar()->addWidget(Zone,        0, TRUE);
    statusBar()->addWidget(xyzstatus,   0, TRUE);
    
    last_zoom = prefs.zoom_default;

    Zoom->setText(QString("Zoom: ").append(QString::number(Map->range)));
}

void FormExcalibur::destroy()
{
    if (MapCancel != NULL)
        delete MapCancel;

    if (MapProgress != NULL)
        delete MapProgress;
    
    if (MapStatus != NULL)
        delete MapStatus;

    if (FPS != NULL)
        delete FPS;

    if (Zone != NULL)
        delete Zone;

    if (xyzstatus != NULL)
        delete xyzstatus;
    
    if (Zoom != NULL)
        delete Zoom;

    prefs.removeWindow(this);
}

void FormExcalibur::ReloadMaps_activated()
{
  exMapInfo::setup("maps/mapinfo.txt");
  Map->setMap(NULL);
}

void FormExcalibur::MapSlider_valueChanged( int range)
{
  prefs.map_range=Map->range=range;
  Zoom->setText(QString("Zoom: ").append(QString::number(range)));
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
      Map->setObjectSize(75);
    else if (act == GLObjectsMedium)
      Map->setObjectSize(150);
    else
      Map->setObjectSize(300);
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
  prefs.vader_warn = ena;
  prefs.activate();
}

void FormExcalibur::MapPNGs_toggled( bool ena )
{
  prefs.map_load_png_maps = ena;
  prefs.activate();
}

void FormExcalibur::MapAdjacentZones_toggled( bool ena )
{
  prefs.map_load_adjacent_zones = ena;
  prefs.activate();
}

void FormExcalibur::MapPlayerNames_toggled( bool ena )
{
  prefs.map_rasterize_player_names = ena;
  prefs.activate();
}

void FormExcalibur::MapMerchatTypes_toggled( bool ena )
{
  prefs.map_rasterize_merchant_types = ena;
  prefs.activate();
}

void FormExcalibur::MapRecache_activated( void )
{
    Map->dirty();
}

void FormExcalibur::MobFilter_returnPressed()
{
  if( !Map->c) return;
  
  Map->c->setFilter( MobFilter->text());
}

void FormExcalibur::MapCancel_pressed()
{
  if( Map->c == NULL)
    return;

  Map->c->cancelMap();
}

void FormExcalibur::GroupItems_toggled( bool ena )
{
  prefs.sort_group_items=ena;
  prefs.activate();
}

void FormExcalibur::MaxFPS_toggled( bool ena )
{
    prefs.maxfps = ena;
}

void FormExcalibur::atnDumpMobInfo_activated()
{
    if(!Map->c) return;

    const exMobList<exMob> mobs = Map->c->getMobs();
    QPtrDictIterator<exMob> mobi(mobs);

    for (;mobi.current(); ++mobi)
        cout << *mobi.current();
    cout << flush;
    //cout << mobs << flush;
}

void FormExcalibur::atnRenderObjects_toggled( bool ena )
{
  prefs.render_objects = ena;
  prefs.activate();
}

void FormExcalibur::atnRenderDead_toggled( bool ena )
{
  prefs.render_dead = ena;
  prefs.activate();
}

void FormExcalibur::atnCrafting_toggled( bool ena )
{
  prefs.crafting_alerts = ena;
  prefs.activate();
}

void FormExcalibur::AutoSelectTarget_toggled( bool ena )
{
  prefs.select_target = ena;
  prefs.activate();
}

void FormExcalibur::ExpWindow_toggled( bool ena )
{
  if (ena)
    xpFrame->show();
  else
    xpFrame->hide();

  prefs.exp_window = ena;
  prefs.activate();
}

void FormExcalibur::ZoomClose_activated()
{
    if (Map->range != prefs.zoom_close) {
        // Remember the last custom zoom, not one of the default zooms
        if (Map->range != prefs.zoom_default &&
            Map->range != prefs.zoom_far)
        	last_zoom = Map->range;

        prefs.map_range = Map->range = prefs.zoom_close; 
        MapSlider->setValue(prefs.zoom_close);
        Map->dirty();
    }
}

void FormExcalibur::ZoomDefault_activated()
{
    if (Map->range != prefs.zoom_default) {
        if (Map->range != prefs.zoom_close &&
            Map->range != prefs.zoom_far)
        	last_zoom = Map->range;

        prefs.map_range = Map->range = prefs.zoom_default; 
        MapSlider->setValue(prefs.zoom_default);
        Map->dirty();
    }
}

void FormExcalibur::ZoomFar_activated()
{
    if (Map->range != prefs.zoom_far) {
        if (Map->range != prefs.zoom_close &&
            Map->range != prefs.zoom_default)
        	last_zoom = Map->range;

        prefs.map_range = Map->range = prefs.zoom_far; 
        MapSlider->setValue(prefs.zoom_far);
        Map->dirty();
    }
}

void FormExcalibur::ZoomLast_activated()
{
   	prefs.map_range = Map->range = last_zoom;
   	MapSlider->setValue(last_zoom);
   	Map->dirty();
}

void FormExcalibur::comboXPRate_activated( int sel )
{
    if (sel == 0)
        Map->c->xpStats->setDisplayRateMult(1);
    else
        Map->c->xpStats->setDisplayRateMult(60);
    Map->c->xpStats->Update();
}

void FormExcalibur::comboXPTick_activated( int sel )
{
    int mult;
    if (sel == 0)
        mult = 1;
    else if (sel == 1)
        mult = 10;
    else
        mult = 100;

    Map->c->xpStats->setDisplayEEPTMult(mult);
    Map->c->xpStats->Update();
}

void FormExcalibur::comboDuration_activated( int sel )
{
    cout << sel << endl;

    int mult;
    if (sel == 0)
        mult = -1;
    else if (sel == 1)
        mult = 5;
    else if (sel == 2)
        mult = 10;
    else if (sel == 3)
        mult = 15;
    else if (sel == 4)
        mult = 20;
    else if (sel == 5)
        mult = 30;
    else
        mult = 60;

    Map->c->xpStats->setDuration(mult);
    Map->c->xpStats->Update();
}
