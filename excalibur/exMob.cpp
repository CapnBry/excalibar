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
#include <stdio.h>
#include <stdlib.h>
#include <qstring.h>
#include <qregexp.h>
#include <qstatusbar.h>
#include "exMob.h"
#include "exPrefs.h"

static const QColor cMidgaard(0,0,255);
static const QColor cHibernia(0,255,0);
static const QColor cAlbion(255,0,0);
static const QColor cFriendly(0,255,255);

static double max_stale_range = 7500.0;

exMob::exMob(QListView *view, exConnection *con, bool newmob, unsigned int
newid, unsigned int newinfoid, QString newname, QString newsurname, QString newguild, int newlevel, int nx, int ny, int nz, int nhp, bool newobj)
 : QListViewItem(view)
{ 
  id=newid;
  infoid=newinfoid;
  name=newname;
  surname=newsurname;
  guild=newguild;
  x=nx;
  y=ny;
  z=nz;
  level=newlevel;
  hp=nhp;
  mana=0;
  mob=newmob;
  obj=newobj;
  head=0x800;
  speed=0;
  stealth = 0;
  c=con;
  isKnown=false;
  
  current = true;
  
  realm = rFriend;
  
  _lasttick = exTick;
  _lastdist = 0;
  playerDist();

  /*
   if we see anything that's further out than our stale range,
   increase the stale range to encompass this object
   */
//  if (lastdist > max_stale_range)
//      max_stale_range = lastdist;
}

int exMob::compare(QListViewItem *i, int col, bool ascending) const {
  exMob *mob;
  int a;
  int b;
  int v;
  bool updown;

  mob=(exMob *)i;

  if (prefs.sort_group_players &&
      ((isPlayer() && !mob->isPlayer()) || (!isPlayer() && mob->isPlayer()))
     ) {
    if (isPlayer())
      updown=false;
    else
      updown=true;
    if (! ascending)
      updown=!updown;
    if (updown)
      return 1;
    else
      return -1;
  }

  // Players should be sorted out now if group_players is true
  if (prefs.sort_group_items &&
      ((isObj() && !mob->isObj()) || (!isObj() && mob->isObj()))
     ) {
    if (!isObj())
      updown=false;
    else
      updown=true;
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
      return QString::number( static_cast< int >(const_cast< exMob *>(this)->playerDist()));
    case 4:
      return QString::number((mi) ? x - mi->getBaseX() : x);
    case 5:
      return QString::number((mi) ? y - mi->getBaseY() : y);
    case 6:
      return QString::number(z);
    default:
      return NULL;
  }
}

void exMob::paintCell(QPainter *p, const QColorGroup &cg, int column, int width, int align) {
  if (p == NULL)
    return;

  QColorGroup cols(cg);
  QColor clr;

  if( !isMobOrObj() && isInvader() && getLevel() >= 15 && c->vaderWarn && !isDead() && !isKnown)
		{
		this->isKnown = true;
		qWarning( "*** INVADER DETECTED *** Name: %s, Level: %d, Distance: %f\n", name.ascii(), getLevel(), playerDist());
		c->ex->statusBar()->message( QString( "*** INVADER DETECTED *** Name: %1, Level: %2, Distance: %3").arg( name).arg(level).arg(playerDist()), 10000);
		qApp->beep();
		}

  if (!isMobOrObj()) {
    clr=getRealmColor().light(isDead() ? prefs.brightness_dead : prefs.brightness_alive);
    cols.setColor(QColorGroup::Base, clr);
  }
  else if ( !isObj() && prefs.MobListColors)
  {
    cols.setColor( QColorGroup::Text, getConColor(c->playerlevel).dark(175));
  }

  if( isFiltered())
	{
    cols.setColor( QColorGroup::Base, QColor(255,255,153));
    cols.setColor( QColorGroup::Text, black);
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
  headrad = ((head / 2048.0) * M_PI);
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

QString exMob::getSurname() const {
  return surname;
}

QString exMob::getGuild() const {
  return guild;
}

bool exMob::isMob() const {
  return mob;
}

bool exMob::isObj() const {
  return obj;
}

bool exMob::isMobOrObj() const {
  return mob || obj;
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

unsigned int exMob::getProjectedX() {
    int real_speed;
    real_speed = getSpeed();

    if (!real_speed)
        return x;

    if (exTick == _lastprojectedX)
	return projectedX;
    
    projectedX = x - (int)(sin(headrad) *
      ((double)real_speed * (double)(exTick - _lasttick) / 1000.0));
    
    _lastprojectedX = exTick;
    return projectedX;
}

unsigned int exMob::getProjectedY() {
    int real_speed;
    real_speed = getSpeed();

    if (!real_speed)
        return y;

    if (exTick == _lastprojectedY)
	return projectedY;
    
    projectedY = y + (int)(cos(headrad) *
      ((double)real_speed * (double)(exTick - _lasttick) / 1000.0));
    
    _lastprojectedY = exTick;
    return projectedY;
}

unsigned int exMob::getZ() const {
   return z;
}

unsigned int exMob::getHead() const {
   return head;
}

int exMob::getSpeed() const {
    /* the bit 9 is the sign, 10 = swimming? */
    if (speed & 0x0200)
        return -((speed & 0x3ff) & 0x1ff);
    else
        return (speed & 0x3ff);
}

unsigned int exMob::getLevel() const {
   return level;
}

Realm exMob::getRealm() const {
   return realm;
}

const QColor exMob::getRealmColor() const {
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

const QColor exMob::getConColor(unsigned int to_level) const
{
    int l_quanta;
    int l_steps_taken;

    l_steps_taken = 0;
    while ((to_level > 0) && (to_level < 100) &&
           (l_steps_taken > -3) && (l_steps_taken < 3))  {

        l_quanta = (to_level / 10) + 1;
        if (l_quanta > 5)
            l_quanta = 5;

        if ((level > (to_level - l_quanta)) &&
            (level <= to_level))
            break;

        if (level < to_level)  {
            to_level -= l_quanta;
            l_steps_taken--;
        }
        else  {
            to_level += l_quanta;
            l_steps_taken++;
        }
    }  /* while to_level in range */

    switch (l_steps_taken)  {
    case -3:
        return gray;
        break;
    case -2:
        return green;
        break;
    case -1:
        return blue;
        break;
    case  0:
        return yellow;
        break;
    case  1:
        return QColor(255, 127, 0); // orange
        break;  
    case  2:
        return red;
        break;
    case  3:
        return magenta;
        break;
    default:
        return black;
        break;
    }
}

QColor exMob::getColorForRealm(Realm r) {
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

  lastdist=sqrt(xdist*xdist+ydist*ydist); // zdist*zdist

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

void exMob::checkStale() {
  int maxtime;

  if (! current)
    return;

//  if ((speed & 0xFF) == 0)
    maxtime=120000;
//  else
//    maxtime=10000;

      /* mobs get updated to around 7000-7500 */
  if ((playerDist() > max_stale_range) ||
      (!obj && ((exTick - _lasttick) > maxtime))
     ) {
    current = false;
    c->mobWentStale(this);
  }
}

bool exMob::isFiltered() 
{
 
  QRegExp rx( c->MobFilter.getFilter());

  if (-1 != rx.search( name) && "" != c->MobFilter.getFilter()) return true;

  return false;
}

ostream& operator << (ostream& os, const exMob &p)
{
  return os << "exMob ID: " << p.id << " InfoID: " << p.infoid << " = "
        << (p.isMob() ? "mob" : (p.isObj() ? "object" : "player")) << endl
        << "  Name: [" << p.name << "] Surname: [" << p.surname << "] Guild: <" << p.guild << ">" << " Realm: " << p.realm << endl
        << "  X: " << p.x << " Y: " << p.y << " Z: " << p.z << endl
        << "  Head: " << p.head << " Speed: " << p.speed << endl;
}

bool exMob::isStealthed() const
{
    return stealth;
}

void exMob::setStealth(bool bstealth)
{
    stealth = bstealth;
    touch();
}

bool exMob::insideRect(QRect &r)
{
    return r.contains(x, y);
}

bool exMob::isPlayer(void) const
{
    return !(mob || obj);
}
