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


#include "oeMapInfo.h"

#include <qfile.h>

QPtrDict<oeMapInfoList> oeMapInfo::maps;

oeMapInfo::oeMapInfo(int nzone, int bx, int by, int mx, int my, int rots, QString fname, int nzonenum) {
  zone=nzone;
  basex=bx;
  basey=by;
  maxx=mx;
  maxy=my;
  rotations=rots;
  fileName=fname;
  zonenum=nzonenum;
}

bool oeMapInfo::right(int nzone, int x, int y) {
  return ((nzone == zone) && (x >= basex) && (y >= basey) && (x <= maxx) && (y <= maxy));
}

QString oeMapInfo::getName() const {
  return fileName;
}

int oeMapInfo::getBaseX() const {
  return basex;
}

int oeMapInfo::getBaseY() const {
  return basey;
}

int oeMapInfo::getRotate() const {
  return rotations * 90;
}

int oeMapInfo::getZoneNum() const {
  return zonenum;
}

void oeMapInfo::setup(QString infofile) {
  QFile f(infofile);
  QString line;
  oeMapInfo *mi;
  oeMapInfoList *mil;
  bool ok;
  int nzone;
  int bx;
  int by;
  int mx;
  int my;
  int r;
  QString fname;
  int zn;

  maps.setAutoDelete(TRUE);
  maps.clear();

  if (! f.open(IO_ReadOnly)) {
    qWarning("Failed to read mapinfo from %s",(const char *)infofile);
    return;
  }

  while(f.readLine(line, 1024)!=-1) {
    line=line.simplifyWhiteSpace();
    line=line.stripWhiteSpace();

    nzone=line.section(" ",0,0).toInt(&ok, 10);
    if (!ok) {
      qWarning("Error in mapinfo reading zone in line %s",(const char *)line);
      continue;
    }
    bx=line.section(" ",1,1).toInt(&ok, 16);
    if (!ok) {
      qWarning("Error in mapinfo reading base x in line %s",(const char *)line);
      continue;
    }
    by=line.section(" ",2,2).toInt(&ok, 16);
    if (!ok) {
      qWarning("Error in mapinfo reading base y in line %s",(const char *)line);
      continue;
    }
    mx=line.section(" ",3,3).toInt(&ok, 16);
    if (!ok) {
      qWarning("Error in mapinfo reading max x in line %s",(const char *)line);
      continue;
    }
    my=line.section(" ",4,4).toInt(&ok, 16);
    if (!ok) {
      qWarning("Error in mapinfo reading max y in line %s",(const char *)line);
      continue;
    }
    r=line.section(" ",5,5).toInt(&ok, 10);
    if (!ok) {
      qWarning("Error in mapinfo reading rotation in line %s",(const char *)line);
      continue;
    }
    fname=line.section(" ",6,6);
    if (fname.length() < 1) {
      qWarning("Error in mapinfo reading filename in line %s",(const char *)line);
      continue;
    }
    zn=line.section(" ",7,7).toInt(&ok, 10);
    if (!ok) {
      qWarning("Error in mapinfo reading zonenum in line %s",(const char *)line);
      continue;
    }

    mi=new oeMapInfo(nzone,bx,by,mx,my,r,fname,zn);    

    mil=maps.find((void *)nzone);
    if (! mil) {
      mil=new oeMapInfoList();
      mil->setAutoDelete(true);
      maps.insert((void *)nzone, mil);
    }
    mil->append(mi);
  }
}

oeMapInfo *oeMapInfo::get(int zone, int x, int y) {
  oeMapInfoList *mil;
  oeMapInfo *mi;
  mil=maps.find((void *)zone);

  if (! mil)
    return NULL;
  for (mi = mil->first(); mi; mi = mil->next()) {
    if (mi->right(zone,x,y)) {
      mi=new oeMapInfo(*mi);
      return mi;
    }
  }
  return NULL;
}
