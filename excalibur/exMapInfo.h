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


#ifndef _EXMAPINFO_H
#define _EXMAPINFO_H

#include <qptrdict.h>
#include <qptrlist.h>
#include <qmutex.h>

class exMapInfo;

typedef QPtrList<exMapInfo> exMapInfoList;

class exMapInfo {
  private:
    int region;
    int basex;
    int basey;
    int maxx;
    int maxy;
    int zonetype;
    int zone;
    QString fileName;
    static QPtrDict<exMapInfoList> maps;
  public:
    exMapInfo(int nregion, int bx, int by, int mx, int my, int type, QString fname, int nzone);
    bool right(int nregion, int x, int y);
    bool adjoin(int nregion, int xbase, int ybase, int xmax, int ymax);
    QString getName();
    int getBaseX() const;
    int getBaseY() const;
    int getZoneType() const;
    int getZoneNum() const;
    QString getZoneName();
    exMapInfo *getAdjacentZones (int iZoneCheck = -1);
    static void setup(QString infofile);
    static exMapInfo *get(int region, int x, int y);
};

#else
class exMapInfo;
#endif
