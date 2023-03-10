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

#include "oePrefs.h"
#include "oeItem.h"
#include "oeMap.h"

oePrefs::oePrefs() {
  loadSettings();
  activate();
}

void oePrefs::activate() {
  FormOdinsEye *frm;
  for (frm=f.first();(frm != NULL);frm=f.next())
    activate(frm);
}

void oePrefs::activate(FormOdinsEye *frm, bool initial) {
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

void oePrefs::show() {
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

void oePrefs::accept() {
  FormOdinsEye *frm;
  QValueList<int> sz;

  dlg->hide();

  gl_smooth_points=dlg->GLSmoothPoints->isOn();
  gl_smooth_lines=dlg->GLSmoothLines->isOn();
  gl_smooth_polygons=dlg->GLSmoothPolygons->isOn();

  map_rotate=dlg->GLMapRotate->isOn();
  map_simple=dlg->GLSimpleObjects->isOn();

  map_autosimplifyrange=dlg->MapSimplifyRange->value();

  map_fill=dlg->GLMapFill->isOn();

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


void oePrefs::loadSettings() {
  QSettings s;

  win_width=s.readNumEntry("/OdinsEye/MainWindowW",750);
  win_height=s.readNumEntry("/OdinsEye/MainWindowH",550);
  hsplit_size1=s.readNumEntry("/OdinsEye/SplitH1",200);
  hsplit_size2=s.readNumEntry("/OdinsEye/SplitH2",550);
  vsplit_size1=s.readNumEntry("/OdinsEye/SplitV1",100);
  vsplit_size2=s.readNumEntry("/OdinsEye/SplitV2",450);

  map_simple=s.readBoolEntry("/OdinsEye/GLSimpleObjects",TRUE);
  gl_smooth_points=s.readBoolEntry("/OdinsEye/GLSmoothPoints",FALSE);
  gl_smooth_lines=s.readBoolEntry("/OdinsEye/GLSmoothLines",FALSE);
  gl_smooth_polygons=s.readBoolEntry("/OdinsEye/GLSmoothPolygons",FALSE);
  map_rotate=s.readBoolEntry("/OdinsEye/RotateMap", FALSE);
  map_rulers=s.readBoolEntry("/OdinsEye/GLRulers", TRUE);
  map_range=s.readNumEntry("/OdinsEye/MapRange",8000);
  map_fade=s.readBoolEntry("/OdinsEye/GLMapFade", TRUE);
  map_fill=s.readBoolEntry("/OdinsEye/GLMapFill", TRUE);
  map_autosimplifyrange=s.readNumEntry("/OdinsEye/MapAutoSimplifyRange", 50);

  select_target=s.readBoolEntry("/OdinsEye/AutoSelectTarget",TRUE);

  sort_group_players=s.readBoolEntry("/OdinsEye/GroupPlayers",TRUE);
  sort_distance=s.readBoolEntry("/OdinsEye/SortDistance",FALSE);
  sort_when=(enum SortOptions) s.readNumEntry("/OdinsEye/SortWhen",(int) sortNever);
  brightness_alive=s.readNumEntry("/OdinsEye/BrightnessAlive", 140);
  brightness_dead=s.readNumEntry("/OdinsEye/BrightnessDead", 180);

  items_autoupload=s.readBoolEntry("/OdinsEye/ItemsAutoUpload", FALSE);
  items_contributor=s.readEntry("/OdinsEye/ItemsContributor", "");
}

void oePrefs::saveSettings() {
  QSettings s;

  s.writeEntry("/OdinsEye/MainWindowW",win_width);
  s.writeEntry("/OdinsEye/MainWindowH",win_height);
  s.writeEntry("/OdinsEye/SplitH1",hsplit_size1);
  s.writeEntry("/OdinsEye/SplitH2",hsplit_size2);
  s.writeEntry("/OdinsEye/SplitV1",vsplit_size1);
  s.writeEntry("/OdinsEye/SplitV2",vsplit_size2);
  s.writeEntry("/OdinsEye/GLSimpleObjects",map_simple);
  s.writeEntry("/OdinsEye/GLSmoothPoints",gl_smooth_points);
  s.writeEntry("/OdinsEye/GLSmoothLines",gl_smooth_lines);
  s.writeEntry("/OdinsEye/GLSmoothPolygons",gl_smooth_polygons);
  s.writeEntry("/OdinsEye/RotateMap", map_rotate);
  s.writeEntry("/OdinsEye/GLRulers", map_rulers);
  s.writeEntry("/OdinsEye/MapRange", map_range);
  s.writeEntry("/OdinsEye/GLMapFade", map_fade);
  s.writeEntry("/OdinsEye/GLMapFill", map_fill);
  s.writeEntry("/OdinsEye/MapAutoSimplifyRange", map_autosimplifyrange);
  s.writeEntry("/OdinsEye/AutoSelectTarget", select_target);
  s.writeEntry("/OdinsEye/GroupPlayers", sort_group_players);
  s.writeEntry("/OdinsEye/SortDistance", sort_distance);
  s.writeEntry("/OdinsEye/SortWhen", (int) sort_when);
  s.writeEntry("/OdinsEye/BrightnessAlive", brightness_alive);
  s.writeEntry("/OdinsEye/BrightnessDead", brightness_dead);
  s.writeEntry("/OdinsEye/ItemsAutoUpload", items_autoupload);
  s.writeEntry("/OdinsEye/ItemsContributor", items_contributor);
  oeItem::save();
}

void oePrefs::addWindow(FormOdinsEye *frm) {
  f.append(frm);
  activate(frm, TRUE);
}

void oePrefs::removeWindow(FormOdinsEye *frm) {
  f.remove(frm);
}
