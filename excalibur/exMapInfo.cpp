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


#include "exMapInfo.h"
#include <qregexp.h>
#include <qfile.h>

QPtrDict<exMapInfoList> exMapInfo::maps;

exMapInfo::exMapInfo(int nregion, int bx, int by, int mx, int my, int type,
                     QString fname, int nzone, int nrotate)
{
  region=nregion;
  basex=bx;
  basey=by;
  maxx=mx;
  maxy=my;
  zonetype=type;
  fileName=fname;
  zone=nzone;
  rotate=nrotate;
}

bool exMapInfo::right(int nregion, int x, int y) {
  return ((nregion == region) && (x >= basex) && (y >= basey) && (x <= maxx) && (y <= maxy));
}

bool exMapInfo::adjoin(int nregion, int xbase, int ybase, int xmax, int ymax) {

  if (nregion != region)
    return false;
   
   if (xmax == basex || xbase == maxx)
    if (ybase <= maxy && ymax >= basey)
      return true;

  if (xbase == basex || xmax == maxx)
    if (ybase == maxy || ymax == basey)
      return true;

  if (ymax == basey || ybase == maxy) 
    if (xbase <= maxx && xmax >= basex)
      return true;

  if (ybase == basey || ymax == maxy)
    if (xbase == maxx || xmax == basex)
      return true;

  return (false);
}

QString exMapInfo::getName() {
  if (*fileName == 0x0 || fileName.length() <= 4)
    return "UNKNOWN";
  else
    return fileName;
}

int exMapInfo::getBaseX() const {
  return basex;
}

int exMapInfo::getBaseY() const {
  return basey;
}

int exMapInfo::getZoneType() const {
  return zonetype;
}

int exMapInfo::getZoneNum() const {
  return zone;
}

QString exMapInfo::getZoneName() {
  if (*fileName == 0x0 || fileName.length() <= 4)
    return "UNKNOWN";
  else
    return (QString(fileName).replace(QRegExp("_"), " ").replace(QRegExp(".map"), "")).ascii();
}

exMapInfo *exMapInfo::getAdjacentZones(int iZoneCheck = -1) {
  if (&maps == NULL || maps.isEmpty() || &region == NULL) {
    Q_ASSERT (true);
    return NULL;
  }

  exMapInfoList *mil;
  exMapInfo *mi;
  mil=maps.find((void *)region);

  if (! mil)
    return NULL;

  if (iZoneCheck == -1)
    return (exMapInfo*)this;

  /* Since we look for a zone with a HIGHER zone ID, we'd end up
     skipping Camelot Hills without this !! */
  if (iZoneCheck == 0)
    iZoneCheck = -1;

  for (mi = mil->first(); mi; mi = mil->next()) {
    if (mi->adjoin(region,this->basex,this->basey,this->maxx,this->maxy)) {
      if (mi->getZoneNum() > iZoneCheck) {
	return mi;
      }
    }
  }
  return NULL;
}

void exMapInfo::setup(QString infofile) {
  QFile f(infofile);
  QString line;
  exMapInfo *mi;
  exMapInfoList *mil;
  bool ok;
  int nregion;
  int bx;
  int by;
  int mx;
  int my;
  int zt;
  QString fname;
  int zn;
  int nrotate;

  if (&maps == NULL)
    Q_ASSERT(true);

  maps.setAutoDelete(TRUE);
  maps.clear();

  if (! f.open(IO_ReadOnly)) {
    qWarning("Failed to read mapinfo from %s",(const char *)infofile);
    return;
  }

  while(f.readLine(line, 1024)!=-1) {
    line=line.simplifyWhiteSpace();
    line=line.stripWhiteSpace();

    nregion=line.section(" ",0,0).toInt(&ok, 10);
    if (!ok) {
      qWarning("Error in mapinfo reading region in line %s",(const char *)line);
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
    mx=line.section(" ",4,4).toInt(&ok, 16);
    if (!ok) {
      qWarning("Error in mapinfo reading max x in line %s",(const char *)line);
      continue;
    }
    my=line.section(" ",5,5).toInt(&ok, 16);
    if (!ok) {
      qWarning("Error in mapinfo reading max y in line %s",(const char *)line);
      continue;
    }
    zt=line.section(" ",6,6).toInt(&ok, 10);
    if (!ok) {
      qWarning("Error in mapinfo reading zone type in line %s",(const char *)line);
      continue;
    }
    fname=line.section(" ",7,7);
    if (fname.length() < 1) {
      qWarning("Error in mapinfo reading filename in line %s",(const char *)line);
      continue;
    }
    zn=line.section(" ",8,8).toInt(&ok, 10);
    if (!ok) {
      qWarning("Error in mapinfo reading zone in line %s",(const char *)line);
      continue;
    }
    nrotate=line.section(" ",9,9).toInt(&ok, 10);
    if (!ok) {
      qWarning("Error in mapinfo reading rotate in line %s",(const char *)line);
      continue;
    }

    mi=new exMapInfo(nregion,bx,by,mx,my,zt,fname,zn,nrotate);

    mil=maps.find((void *)nregion);
    if (! mil) {
      mil=new exMapInfoList();
      mil->setAutoDelete(true);
      maps.insert((void *)nregion, mil);
    }
    mil->append(mi);
  }
}

exMapInfo *exMapInfo::get(int region, int x, int y) {
  exMapInfoList *mil;
  exMapInfo *mi;
  mil=maps.find((void *)region);

  if (! mil)
    return NULL;

  for (mi = mil->first(); mi; mi = mil->next()) {
    if (mi->right(region,x,y)) {
      mi=new exMapInfo(*mi);
      return mi;
    }
  }
  return NULL;
}
