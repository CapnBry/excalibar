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

#include <math.h>
#include "oeMob.h"
#include "oePrefs.h"

static const QColor cMidgaard(0,0,255);
static const QColor cHibernia(0,255,0);
static const QColor cAlbion(255,0,0);
static const QColor cFriendly(0,255,255);

oeMob::oeMob(QListView *view, oeConnection *con, bool newmob, unsigned int newid, unsigned int newinfoid, QString newname, int newlevel, int nx, int ny, int nz, int nhp) 
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

  _lasttick = oeTick;
  _lastdist = 0;
  playerDist();
}

int oeMob::compare(QListViewItem *i, int col, bool ascending) const {
  oeMob *mob;
  int a;
  int b;
  int v;
  bool updown;

  mob=(oeMob *)i;

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
    updown=((oeMob *)this)->playerDist() > mob->playerDist();
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

QString oeMob::text(int column) const {
  oeMapInfo *mi;

  mi=c->oe->Map->getMap();

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

void oeMob::paintCell(QPainter *p, const QColorGroup &cg, int column, int width, int align) {
  QColorGroup cols(cg);
  QColor c;

  if (! isMob()) {
    c=getColor().light(isDead() ? prefs.brightness_dead : prefs.brightness_alive);
    cols.setColor(QColorGroup::Base, c);
  }

  QListViewItem::paintCell(p,cols,column,width,align);
}


void oeMob::setPosition(unsigned int nx, unsigned int ny, unsigned int nz) {
  x=nx;
  y=ny;
  z=nz;
  touch();
  repaint();
}

void oeMob::setHead(unsigned int nhead) {
  head=nhead & 0xfff;
  touch();
}

void oeMob::setHP(unsigned int nhp) {
  hp=nhp;
  touch();
  repaint();
}

void oeMob::setSpeed(unsigned int nspeed) {
  speed=nspeed;
  touch();
}

void oeMob::setRealm(Realm nrealm) {
  realm=nrealm;
}


unsigned int oeMob::getID() const {
  return id;
}

unsigned int oeMob::getInfoID() const {
  return infoid;
}

QString oeMob::getName() const {
  return name;
}

bool oeMob::isMob() const {
  return mob;
}

bool oeMob::isDead() const {
  return (hp == 0) ? true : false;
}

bool oeMob::isCurrent() const {
  return current;
}

bool oeMob::isInvader() const {
  return (c->playerrealm != realm);
}

unsigned int oeMob::getX() const {
   return x;
}

unsigned int oeMob::getY() const {
   return y;
}

unsigned int oeMob::getZ() const {
   return z;
}

unsigned int oeMob::getHead() const {
   return head;
}

unsigned int oeMob::getSpeed() const {
   return speed;
}

unsigned int oeMob::getLevel() const {
   return level;
}

Realm oeMob::getRealm() const {
   return realm;
}

const QColor oeMob::getColor() const {
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

QColor oeMob::getColor(Realm r) {
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

double oeMob::playerDist() {
  int xdist;
  int ydist;
  int zdist;

  if (oeTick == _lastdist)
    return lastdist;

  xdist = x - c->playerx;
  ydist = y - c->playery;
  zdist = z - c->playerz;

  lastdist=sqrt(xdist*xdist+ydist*ydist+zdist*zdist);

  _lastdist=oeTick;
  return lastdist;
}


void oeMob::touch() {
  _lasttick=oeTick;

  if (current)
    return;

  current=true;
  c->oe->ListViewMobs->insertItem(this);
}

void oeMob::stale() {
  int maxtime;

  if (! current)
    return;

  if ((speed & 0xFF) == 0)
    maxtime=60000;
  else
    maxtime=10000;

  if (((oeTick - _lasttick) > maxtime) || (playerDist()>15000.0)) {
    current = false;
    c->oe->ListViewMobs->takeItem(this);
  }
}
