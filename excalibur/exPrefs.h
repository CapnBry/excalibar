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

#ifndef _EXPREFS_H
#define _EXPREFS_H

#include <qptrlist.h>

#include "formexcalibur.h"
#include "prefsdialog.h"

class exPrefs : public QObject {
  Q_OBJECT
public:
  enum SortOptions {
    sortNever,
    sortPlayer,
    sortAlways
  };
protected:
  PrefsDialog *dlg;
  QPtrList<FormExcalibur> f;  
public slots:
  void accept();
  void show();
public:
  bool sort_group_players;
  bool sort_distance;
  bool vaderWarn;
  exPrefs::SortOptions sort_when;
  int brightness_alive;
  int brightness_dead;

  bool map_rotate;
  bool map_rulers; 
  bool map_simple;
  bool map_fade;
  int map_autosimplifyrange;
  int player_circle_1;
  int player_circle_2;
  bool gl_smooth_points;
  bool gl_smooth_lines;
  bool gl_smooth_polygons;
  bool map_fill;
  bool map_rasterize_player_names;
  bool map_rasterize_merchant_types;
  bool map_load_adjacent_zones;
  bool map_load_png_maps;
  bool map_linear_filter;
  bool map_mipmap;
  bool agro_circles;
  bool filter_circles;
  bool agro_fading;
  bool alpha_circles;
  bool alpha_speed;
  bool alpha_quality;
  bool alpha_borders;
  bool dump_unknown_packets;
  bool MobListColors;
  bool sticky_list;
  bool select_target;

  int win_width;
  int win_height;
  int hsplit_size1, hsplit_size2;
  int vsplit_size1, vsplit_size2;
  int map_range;

  bool items_autoupload;
  QString items_contributor;

  bool enable_experimental_code;

  bool druppy_leak;

  exPrefs();
  void loadSettings();
  void saveSettings();
  void activate();
  void activate(FormExcalibur *fex, bool initial = FALSE);
  void addWindow(FormExcalibur *fex);
  void removeWindow(FormExcalibur *fex);
};    

#else
class exPrefs;
#endif
