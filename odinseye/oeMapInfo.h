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


#ifndef _OEMAPINFO_H
#define _OEMAPINFO_H

#include <qptrdict.h>
#include <qptrlist.h>
#include <qmutex.h>

class oeMapInfo;

typedef QPtrList<oeMapInfo> oeMapInfoList;

class oeMapInfo {
  private:
    int zone;
    int basex;
    int basey;
    int maxx;
    int maxy;
    int rotations;
    int zonenum;
    QString fileName;
    static QPtrDict<oeMapInfoList> maps;
  public:
    oeMapInfo(int nzone, int bx, int by, int mx, int my, int rots, QString fname, int nzonenum);
    bool right(int nzone, int x, int y);
    QString getName() const;
    int getBaseX() const;
    int getBaseY() const;
    int getRotate() const;
    int getZoneNum() const;
    static void setup(QString infofile);
    static oeMapInfo *get(int zone, int x, int y);
};

#else
class oeMapInfo;
#endif
