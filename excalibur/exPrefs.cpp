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

#include <qslider.h>
#include <qlistview.h>
#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qradiobutton.h>
#include <qsplitter.h>
#include <qvaluelist.h>
#include <qaction.h>
#include <qlineedit.h>
#include <qsettings.h>

#include "exPrefs.h"
#include "exItem.h"
#include "exMap.h"

exPrefs::exPrefs() {
  loadSettings();
  activate();
}

void exPrefs::activate() {
  FormExcalibur *frm;
  for (frm=f.first();(frm != NULL);frm=f.next())
    activate(frm);
}

void exPrefs::activate(FormExcalibur *frm, bool initial) {
  QValueList<int> sz;

  if (initial) {
    frm->resize(win_width, win_height);

    sz.clear();
    sz.append(hsplit_size1);
    sz.append(hsplit_size2);
    frm->HSplitter->setSizes(sz);

    sz.clear();
    sz.append(vsplit_size1);
    sz.append(vsplit_size2);
    frm->VSplitter->setSizes(sz);

    frm->Map->range = map_range;  
    frm->GLRulers->setOn(map_rulers);
    frm->AutoSelectTarget->setOn(select_target);
    frm->GroupPlayers->setOn(sort_group_players);
    frm->GLMapFade->setOn(map_fade);
    frm->MapSlider->setValue(map_range);
    frm->SortDistance->setOn(sort_distance);
  }
  frm->Map->makeObjects(map_simple);
  frm->Map->dirty();
  frm->ListViewMobs->sort();
  frm->ListViewMobs->repaint();
}

void exPrefs::show() {
  if (! dlg) {
    dlg=new PrefsDialog();
    connect( dlg->buttonOk, SIGNAL( clicked() ), this, SLOT( accept() ));
  }
  dlg->GLSmoothPoints->setChecked(gl_smooth_points);
  dlg->GLSmoothLines->setChecked(gl_smooth_lines);
  dlg->GLSmoothPolygons->setChecked(gl_smooth_polygons);
  dlg->GLMapRotate->setChecked(map_rotate);
  dlg->GLSimpleObjects->setChecked(map_simple);
  dlg->GLMapFill->setChecked(map_fill);
  dlg->AgroCircles->setChecked(agro_circles);
  dlg->MapSimplifyRange->setValue(map_autosimplifyrange);

  dlg->SliderAliveColor->setValue(brightness_alive);
  dlg->SliderDeadColor->setValue(brightness_dead);


  switch (sort_when) {
    case sortNever:
	dlg->SortNever->setChecked(TRUE);
	break;
    case sortPlayer:
	dlg->SortPlayer->setChecked(TRUE);
	break;
    case sortAlways:
	dlg->SortAlways->setChecked(TRUE);
	break;
  }

  dlg->SliderAliveColor_valueChanged(dlg->SliderAliveColor->value());
  dlg->SliderDeadColor_valueChanged(dlg->SliderDeadColor->value());

  dlg->ItemAutoUpload->setChecked(items_autoupload);
  dlg->ItemContributor->setText(items_contributor);

  dlg->show();
  dlg->raise();
}

void exPrefs::accept() {
  FormExcalibur *frm;
  QValueList<int> sz;

  dlg->hide();

  gl_smooth_points=dlg->GLSmoothPoints->isOn();
  gl_smooth_lines=dlg->GLSmoothLines->isOn();
  gl_smooth_polygons=dlg->GLSmoothPolygons->isOn();

  map_rotate=dlg->GLMapRotate->isOn();
  map_simple=dlg->GLSimpleObjects->isOn();

  map_autosimplifyrange=dlg->MapSimplifyRange->value();

  map_fill=dlg->GLMapFill->isOn();

  agro_circles=dlg->AgroCircles->isOn();

  brightness_alive=dlg->SliderAliveColor->value();
  brightness_dead=dlg->SliderDeadColor->value();

  sort_when = (dlg->SortNever->isChecked() ? sortNever : (dlg->SortPlayer->isChecked() ? sortPlayer : sortAlways));

  items_autoupload=dlg->ItemAutoUpload->isChecked();
  items_contributor=dlg->ItemContributor->text();

  frm=f.first();
  if (frm) {
    win_width = frm->width();
    win_height= frm->height();
    sz=frm->HSplitter->sizes();
    hsplit_size1=sz[0];
    hsplit_size2=sz[1];
    sz=frm->VSplitter->sizes();
    vsplit_size1=sz[0];
    vsplit_size2=sz[1];
    map_range=frm->MapSlider->value();
    map_fade=frm->GLMapFade->isOn();
  }

  saveSettings();
  activate();
}


void exPrefs::loadSettings() {
  QSettings s;

  win_width=s.readNumEntry("/Excalibur/MainWindowW",750);
  win_height=s.readNumEntry("/Excalibur/MainWindowH",550);
  hsplit_size1=s.readNumEntry("/Excalibur/SplitH1",200);
  hsplit_size2=s.readNumEntry("/Excalibur/SplitH2",550);
  vsplit_size1=s.readNumEntry("/Excalibur/SplitV1",100);
  vsplit_size2=s.readNumEntry("/Excalibur/SplitV2",450);

  map_simple=s.readBoolEntry("/Excalibur/GLSimpleObjects",TRUE);
  gl_smooth_points=s.readBoolEntry("/Excalibur/GLSmoothPoints",FALSE);
  gl_smooth_lines=s.readBoolEntry("/Excalibur/GLSmoothLines",FALSE);
  gl_smooth_polygons=s.readBoolEntry("/Excalibur/GLSmoothPolygons",FALSE);
  map_rotate=s.readBoolEntry("/Excalibur/RotateMap", FALSE);
  map_rulers=s.readBoolEntry("/Excalibur/GLRulers", TRUE);
  map_range=s.readNumEntry("/Excalibur/MapRange",8000);
  map_fade=s.readBoolEntry("/Excalibur/GLMapFade", TRUE);
  map_fill=s.readBoolEntry("/Excalibur/GLMapFill", TRUE);
  map_autosimplifyrange=s.readNumEntry("/Excalibur/MapAutoSimplifyRange", 50);
  range_circles=s.readBoolEntry("/Excalibur/RangeCircles", TRUE);
  agro_circles=s.readBoolEntry("/Excalibur/AgroCircles", TRUE);

  select_target=s.readBoolEntry("/Excalibur/AutoSelectTarget",TRUE);

  sort_group_players=s.readBoolEntry("/Excalibur/GroupPlayers",TRUE);
  sort_distance=s.readBoolEntry("/Excalibur/SortDistance",FALSE);
  sort_when=(enum SortOptions) s.readNumEntry("/Excalibur/SortWhen",(int) sortNever);
  brightness_alive=s.readNumEntry("/Excalibur/BrightnessAlive", 140);
  brightness_dead=s.readNumEntry("/Excalibur/BrightnessDead", 180);

  items_autoupload=s.readBoolEntry("/Excalibur/ItemsAutoUpload", FALSE);
  items_contributor=s.readEntry("/Excalibur/ItemsContributor", "");
}

void exPrefs::saveSettings() {
  QSettings s;

  s.writeEntry("/Excalibur/MainWindowW",win_width);
  s.writeEntry("/Excalibur/MainWindowH",win_height);
  s.writeEntry("/Excalibur/SplitH1",hsplit_size1);
  s.writeEntry("/Excalibur/SplitH2",hsplit_size2);
  s.writeEntry("/Excalibur/SplitV1",vsplit_size1);
  s.writeEntry("/Excalibur/SplitV2",vsplit_size2);
  s.writeEntry("/Excalibur/GLSimpleObjects",map_simple);
  s.writeEntry("/Excalibur/GLSmoothPoints",gl_smooth_points);
  s.writeEntry("/Excalibur/GLSmoothLines",gl_smooth_lines);
  s.writeEntry("/Excalibur/GLSmoothPolygons",gl_smooth_polygons);
  s.writeEntry("/Excalibur/RotateMap", map_rotate);
  s.writeEntry("/Excalibur/GLRulers", map_rulers);
  s.writeEntry("/Excalibur/MapRange", map_range);
  s.writeEntry("/Excalibur/GLMapFade", map_fade);
  s.writeEntry("/Excalibur/GLMapFill", map_fill);
  s.writeEntry("/Excalibur/MapAutoSimplifyRange", map_autosimplifyrange);
  s.writeEntry("/Excalibur/AutoSelectTarget", select_target);
  s.writeEntry("/Excalibur/GroupPlayers", sort_group_players);
  s.writeEntry("/Excalibur/SortDistance", sort_distance);
  s.writeEntry("/Excalibur/SortWhen", (int) sort_when);
  s.writeEntry("/Excalibur/BrightnessAlive", brightness_alive);
  s.writeEntry("/Excalibur/BrightnessDead", brightness_dead);
  s.writeEntry("/Excalibur/ItemsAutoUpload", items_autoupload);
  s.writeEntry("/Excalibur/ItemsContributor", items_contributor);
  s.writeEntry("/Excalibur/RangeCircles", range_circles);
  s.writeEntry("/Excalibur/AgroCircles", agro_circles);
  exItem::save();
}

void exPrefs::addWindow(FormExcalibur *frm) {
  f.append(frm);
  activate(frm, TRUE);
}

void exPrefs::removeWindow(FormExcalibur *frm) {
  f.remove(frm);
}
