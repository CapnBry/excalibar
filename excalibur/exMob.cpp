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

#include <math.h>
#include "exMob.h"
#include "exPrefs.h"

static const QColor cMidgaard(0,0,255);
static const QColor cHibernia(0,255,0);
static const QColor cAlbion(255,0,0);
static const QColor cFriendly(0,255,255);

exMob::exMob(QListView *view, exConnection *con, bool newmob, unsigned int newid, unsigned int newinfoid, QString newname, int newlevel, int nx, int ny, int nz, int nhp) 
 : QListViewItem(view)
{
  id=newid;
  infoid=newinfoid;
  name=newname;
  x=nx;
  y=ny;
  z=nz;
  level=newlevel;
  hp=nhp;
  mana=0;
  mob=newmob;
  head=0x800;
  speed=0;
  c=con;

  current = true;

  realm = rFriend;

  _lasttick = exTick;
  _lastdist = 0;
  playerDist();
}

int exMob::compare(QListViewItem *i, int col, bool ascending) const {
  exMob *mob;
  int a;
  int b;
  int v;
  bool updown;

  mob=(exMob *)i;

  if (prefs.sort_group_players && ((isMob() && !mob->isMob()) || (!isMob() && mob->isMob()))) {
    updown=isMob();
    if (! ascending)
      updown=!updown;
    if (updown)
      return 1;
    else
      return -1;
  }

  if (prefs.sort_distance) {
    updown=((exMob *)this)->playerDist() > mob->playerDist();
    if (! ascending)
      updown=!updown;
    if (updown)
      return 1;
    else
      return -1;
  }

  switch (col) {
    case 1:
      a=level;
      b=mob->level;
      break;
    case 2:
      a=hp;
      b=mob->hp;
      break;
    case 3:
      a=x;
      b=mob->x;
      break;
    case 4:
      a=y;
      b=mob->y;
      break;
    case 5:
      a=z;
      b=mob->z;
      break;
    default:
      return key(col, ascending).compare(i->key(col,ascending));
  }
  if (a < b)
   v=-1;
  else if (a==b)
   v=0;
  else 
   v=1;
  return v;
}

QString exMob::text(int column) const {
  exMapInfo *mi;

  mi=c->ex->Map->getMap();

  switch (column) {
    case 0:
      return name;
    case 1:
      return QString::number(level);
    case 2:
      return QString::number(hp);
    case 3:
      return QString::number((mi) ? x - mi->getBaseX() : x);
    case 4:
      return QString::number((mi) ? y - mi->getBaseY() : y);
    case 5:
      return QString::number(z);
    default:
      return NULL;
  }
}

void exMob::paintCell(QPainter *p, const QColorGroup &cg, int column, int width, int align) {
  QColorGroup cols(cg);
  QColor c;

  if (! isMob()) {
    c=getColor().light(isDead() ? prefs.brightness_dead : prefs.brightness_alive);
    cols.setColor(QColorGroup::Base, c);
  }

  QListViewItem::paintCell(p,cols,column,width,align);
}


void exMob::setPosition(unsigned int nx, unsigned int ny, unsigned int nz) {
  x=nx;
  y=ny;
  z=nz;
  touch();
  repaint();
}

void exMob::setHead(unsigned int nhead) {
  head=nhead & 0xfff;
  /* (((head * 360.0) / 4096.0) * M_PI) / 180.0; */
  headrad = (head / 2048.0) * M_PI;
  touch();
}

void exMob::setHP(unsigned int nhp) {
  hp=nhp;
  touch();
  repaint();
}

void exMob::setSpeed(unsigned int nspeed) {
  speed=nspeed;
  touch();
}

void exMob::setRealm(Realm nrealm) {
  realm=nrealm;
}


unsigned int exMob::getID() const {
  return id;
}

unsigned int exMob::getInfoID() const {
  return infoid;
}

QString exMob::getName() const {
  return name;
}

bool exMob::isMob() const {
  return mob;
}

bool exMob::isDead() const {
  return (hp == 0) ? true : false;
}

bool exMob::isCurrent() const {
  return current;
}

bool exMob::isInvader() const {
  return (c->playerrealm != realm);
}

unsigned int exMob::getX() const {
   return x;
}

unsigned int exMob::getY() const {
   return y;
}

#ifdef ALPHA_QUALITY
unsigned int exMob::getProjectedX() {
    if (exTick == _lastprojectedX)
	return projectedX;
    
    if (speed)
	projectedX = x - (int)(sin(headrad) * 
          ((double)speed * (double)(exTick - _lasttick) / 1000.0));
    else
	projectedX = x;
    
    _lastprojectedX = exTick;
    return projectedX;
}

unsigned int exMob::getProjectedY() {
    if (exTick == _lastprojectedY)
	return projectedY;
    
    if (speed)
	projectedY = y + (int)(cos(headrad) * 
          ((double)speed * (double)(exTick - _lasttick) / 1000.0));
    else
	projectedY = y;
    
    _lastprojectedY = exTick;
    return projectedY;
}
#endif

unsigned int exMob::getZ() const {
   return z;
}

unsigned int exMob::getHead() const {
   return head;
}

unsigned int exMob::getSpeed() const {
   return speed;
}

unsigned int exMob::getLevel() const {
   return level;
}

Realm exMob::getRealm() const {
   return realm;
}

const QColor exMob::getColor() const {
  if (! isInvader())
    return cFriendly;
  switch (realm) {
    case rMidgaard:
	return cMidgaard;
    case rAlbion:
	return cAlbion;
    default:
	return cHibernia;
  }
}

QColor exMob::getColor(Realm r) {
  switch (r) {
    case rMidgaard:
	return cMidgaard;
    case rAlbion:
	return cAlbion;
    case rHibernia:
	return cHibernia;
    default:
	return cFriendly;
  }
}

double exMob::playerDist() {
  int xdist;
  int ydist;
  int zdist;

  if (exTick == _lastdist)
    return lastdist;

  xdist = x - c->playerx;
  ydist = y - c->playery;
  zdist = z - c->playerz;

  lastdist=sqrt(xdist*xdist+ydist*ydist+zdist*zdist);

  _lastdist=exTick;
  return lastdist;
}


void exMob::touch() {
  _lasttick=exTick;

  if (current)
    return;

  current=true;
  c->ex->ListViewMobs->insertItem(this);
}

void exMob::stale() {
  int maxtime;

  if (! current)
    return;

  if ((speed & 0xFF) == 0)
    maxtime=60000;
  else
    maxtime=10000;

  if (((exTick - _lasttick) > maxtime) || (playerDist()>15000.0)) {
    current = false;
    c->ex->ListViewMobs->takeItem(this);
  }
}
