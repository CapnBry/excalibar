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

  current = true;
  
  realm = rFriend;
  inventory.setAutoDelete(true);
  playerclass = exMob::Unknown;
  
  _lasttick = exTick;
  _lastdist = 0;
  _lastprojectedX = 0;
  _lastprojectedY = 0;
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
        if (playerclass != exMob::Unknown)
            if (isInvader())
                return getClassName() + " " + name;
            else
                return name + " (" + getClassName() + ")";
        else
            return name;
    case 1:
      return QString::number(level);
    case 2:
      return QString::number(hp);
    case 3:
      return QString::number( static_cast< int >(const_cast< exMob *>(this)->playerDist()));
    default:
      return NULL;
  }
}

void exMob::paintCell(QPainter *p, const QColorGroup &cg, int column, int width, int align)
{
  QColorGroup new_colors(cg);
  QColor clr;

  if (isPlayer())
  {
    clr = getRealmColor().light(isDead() ? prefs.brightness_dead : prefs.brightness_alive);
    new_colors.setColor(QColorGroup::Base, clr);
  }
  else if (!isObj() && prefs.MobListColors)
  {
    clr = getConColor(c->playerlevel).dark(175);
    new_colors.setColor(QColorGroup::Text, clr);
  }

  if(isFiltered())
  {
    new_colors.setColor(QColorGroup::Base, QColor(255,255,153));
    new_colors.setColor(QColorGroup::Text, black);
  }

  QListViewItem::paintCell(p, new_colors, column, width, align);
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

  lastdist=sqrt((double)xdist*xdist+ydist*ydist+zdist*zdist);

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
    if (!c->MobFilter.isFilterSet())
        return false;

    QRegExp rx(c->MobFilter.getFilter());

    if (rx.search(name) != -1)
        return true;
    else
        return false;
}

ostream& operator << (ostream& os, const exMob &p)
{
  return os << "exMob ID: " << p.id << " InfoID: " << p.infoid << " = "
        << (p.isMob() ? "mob" : (p.isObj() ? "object" : "player"))
        << (p.isCurrent() ? "" : " (STALE)") << endl
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

QString exMob::getClassName(void) const
{
    switch (playerclass)
    {
    case Unknown:     return "";
    case Armsman:     return "Armsman";
    case Cabalist:    return "Cabalist";
    case Cleric:      return "Cleric";
    case Friar:       return "Friar";
    case Infiltrator: return "Infiltrator";
    case Mercenary:   return "Mercenary";
    case Minstrel:    return "Minstrel";
    case Paladin:     return "Paladin";
    case Scout:       return "Scout";
    case Sorcerer:    return "Sorcerer";
    case Theurgist:   return "Theurgist";
    case Wizard:      return "Wizard";

    case Bard:        return "Bard";
    case Blademaster: return "Blademaster";
    case Champion:    return "Champion";
    case Druid:       return "Druid";
    case Eldritch:    return "Eldritch";
    case Enchanter:   return "Enchanter";
    case Hero:        return "Hero";
    case Mentalist:   return "Mentalist";
    case Nightshade:  return "Nightshade";
    case Ranger:      return "Ranger";
    case Warden:      return "Warden";

    case Berserker:   return "Berserker";
    case Healer:      return "Healer";
    case Hunter:      return "Hunter";
    case Runemaster:  return "Runemaster";
    case Shadowblade: return "Shadowblade";
    case Shaman:      return "Shaman";
    case Skald:       return "Skald";
    case Spiritmaster: return "Spiritmaster";
    case Thane:       return "Thane";
    case Warrior:     return "Warrior";
    default:
        return "???Class";
    }
}

void exMob::updateInventory(exInventoryItem *ii)
{
    void *iSlot;
    playerClass itemclass;

    iSlot = (void *)ii->getSlot();

    if (inventory.find(iSlot))
        inventory.remove(iSlot);

    inventory.insert(iSlot, ii);


    itemclass = ii->getClassRestriction();
    if (itemclass != Unknown)
        playerclass = itemclass;
}

void exMob::clearInventory(void)
{
    inventory.clear();
}

exInventoryItem::exInventoryItem(int newslot, int newlist, int newindex, int newcolor)
{
    slot = newslot;
    obj_list = newlist;
    obj_index = newindex;
    obj_color = newcolor;
}

QString exInventoryItem::getDescription(void)
{
    switch (obj_list & 0x0f)  {
    case 0x00:
        switch (obj_index)  {
        case 0x34:  return "bow";
        case 0x39:  return "cloak";
        case 0x3a:  return "robe";
        case 0x3f:  return "axe" + getSlotDesc();
        case 0x60:  return "cloak";
        case 0xe6:  
        case 0xe7:
        case 0xe8:  
        case 0xe9:  
        case 0xea:  return "S(1) " + getSlotDesc();
        case 0xeb:  
        case 0xec:  
        case 0xed:  
        case 0xee:  
        case 0xef:  return "C(2) " + getSlotDesc();
        case 0xf5:  
        case 0xf8:  
        case 0xf9:  return "P(1) " + getSlotDesc();
        case 0xfa:  
        case 0xfb:  
        case 0xfc:  
        case 0xfd:  
        case 0xfe:  return "S(3) " + getSlotDesc();
        default:
            return getSlotDesc() + QString().sprintf(" %02x %02x %02x",
                                                     obj_list, obj_index, obj_color);
        }  /* b1=0x00 b2 */
           

    case 0x01:
        switch (obj_index)  {
        case 0x04:  
        case 0x05:  
        case 0x06:  
        case 0x07:  
        case 0x08:  return "L(2) " + getSlotDesc();
        case 0x0a:  
        case 0x0b:  return "P(1) " + getSlotDesc();
        case 0x0e:  
        case 0x11:  
        case 0x12:  return "S(2) " + getSlotDesc();
        case 0x13:  return "C(3) " + getSlotDesc();
        case 0x18:  
        case 0x19:  
        case 0x1a:  
        case 0x1b:  
        case 0x1c:  return "L(3) " + getSlotDesc();
        case 0x36:  
        case 0x38:  return "sword " + getSlotDesc();
        case 0x42:  return "hammer " + getSlotDesc();
        case 0x46:  return "cloak";
        case 0x47:  return "staff " + getSlotDesc();
        // case 0x50:  return "RM " + getSlotDesc();
        // case 0x51:  return "SB " + getSlotDesc();
        // case 0xb6:  return "HE " + getSlotDesc();
        case 0xbb:  return "cloak";
        case 0xff:  return "Pcrown " + getSlotDesc();
        default:
            return getSlotDesc() + QString().sprintf(" %02x %02x %02x",
                                                     obj_list, obj_index, obj_color);
        }  /* b1=0x01 b2 */


    case 0x02:
        switch (obj_index)  {
        case 0x2d:  return "cloak";
        case 0x2f:  return "cloak";
        case 0x36:  return "staff" + getSlotDesc();
        case 0x3f:  return "hammer" + getSlotDesc();
        case 0x41:  return "axe " + getSlotDesc();
        case 0xaa:  return "CA " + getSlotDesc();  // cabalist robe
        case 0xb0:  return "AR " + getSlotDesc();  // armsman chest
        case 0xb1:  return "AR " + getSlotDesc();  // leggings
        case 0xb2:  return "AR " + getSlotDesc();  // sleeves
        case 0xb3:  return "AR " + getSlotDesc();  // gloves
        case 0xb4:  return "AR " + getSlotDesc();  // boots
        case 0xb5:  return "PA " + getSlotDesc();  // paladin chest
        case 0xb6:  return "PA " + getSlotDesc();  // leggings
        case 0xb7:  return "PA " + getSlotDesc();  // sleeves
        case 0xb8:  return "PA " + getSlotDesc();  // gloves
        case 0xb9:  return "PA " + getSlotDesc();  // boots
        case 0xba:  return "HA " + getSlotDesc();  // healer chest
        case 0xbb:  return "HA " + getSlotDesc();  // leggings
        case 0xbc:  return "HA " + getSlotDesc();  // sleeves
        case 0xbd:  return "HA " + getSlotDesc();  // gloves
        case 0xbe:  return "HA " + getSlotDesc();  // boots
        case 0xbf:  return "RM " + getSlotDesc();  // runemaster chest
        case 0xc0:  return "RM " + getSlotDesc();  // leggings
        case 0xc1:  return "RM " + getSlotDesc();  // sleeves
        case 0xc2:  return "RM " + getSlotDesc();  // gloves
        case 0xc3:  return "RM " + getSlotDesc();  // boots
        case 0xc4:  return "HE " + getSlotDesc();  // hero chest
        case 0xc5:  return "HE " + getSlotDesc();  // leggings
        case 0xc6:  return "HE " + getSlotDesc();  // sleeves
        case 0xc7:  return "HE " + getSlotDesc();  // gloves
        case 0xc8:  return "HE " + getSlotDesc();  // boots
        case 0xc9:  return "CL " + getSlotDesc();  // cleric chest
        case 0xca:  return "CL " + getSlotDesc();  // leggings
        case 0xcb:  return "CL " + getSlotDesc();  // sleeves
        case 0xcc:  return "CL " + getSlotDesc();  // gloves
        case 0xcd:  return "CL " + getSlotDesc();  // boots
        case 0xce:  return "MY " + getSlotDesc();  // mercenary chest
        case 0xcf:  return "MY " + getSlotDesc();  // leggings
        case 0xd0:  return "MY " + getSlotDesc();  // sleeves
        case 0xd1:  return "MY " + getSlotDesc();  // gloves
        case 0xd2:  return "MY " + getSlotDesc();  // boots
        case 0xd3:  return "MI " + getSlotDesc();  // minstrel chest
        case 0xd4:  return "MI " + getSlotDesc();  // leggings
        case 0xd5:  return "MI " + getSlotDesc();  // sleeves
        case 0xd6:  return "MI " + getSlotDesc();  // gloves
        case 0xd7:  return "MI " + getSlotDesc();  // boots
        case 0xd8:  return "SC " + getSlotDesc();  // scout chest
        case 0xd9:  return "SC " + getSlotDesc();  // leggings
        case 0xda:  return "SC " + getSlotDesc();  // sleeves
        case 0xdb:  return "SC " + getSlotDesc();  // boots
        case 0xdc:  return "SC " + getSlotDesc();  // gloves
        case 0xdd:  return "TH " + getSlotDesc();  // theurgist chest
        case 0xde:  return "BA " + getSlotDesc();  // bard chest
        case 0xdf:  return "BA " + getSlotDesc();  // leggings
        case 0xe0:  return "BA " + getSlotDesc();  // sleeves
        case 0xe1:  return "BA " + getSlotDesc();  // gloves
        case 0xe2:  return "BA " + getSlotDesc();  // boots
        case 0xe3:  return "DR " + getSlotDesc();  // druid chest
        case 0xe4:  return "DR " + getSlotDesc();  // leggings
        case 0xe5:  return "DR " + getSlotDesc();  // sleeves
        case 0xe6:  return "DR " + getSlotDesc();  // gloves
        case 0xe7:  return "DR " + getSlotDesc();  // boots
        case 0xe8:  return "EL " + getSlotDesc();  // eldritch chest
        case 0xe9:  return "ME " + getSlotDesc();  // mentalist chest
        case 0xea:  return "NI " + getSlotDesc();  // nightshade chest
        case 0xeb:  return "NI " + getSlotDesc();  // leggings
        case 0xec:  return "NI " + getSlotDesc();  // sleeves
        case 0xed:  return "NI " + getSlotDesc();  // gloves
        case 0xee:  return "NI " + getSlotDesc();  // boots
        case 0xef:  return "BZ " + getSlotDesc();  // Berserker chest
        case 0xf0:  return "BZ " + getSlotDesc();  // leggings
        case 0xf1:  return "BZ " + getSlotDesc();  // sleeves
        case 0xf2:  return "BZ " + getSlotDesc();  // gloves
        case 0xf3:  return "BZ " + getSlotDesc();  // boots
        case 0xf4:  return "HU " + getSlotDesc();  // hunter chest
        case 0xf5:  return "HU " + getSlotDesc();  // leggings
        case 0xf6:  return "HU " + getSlotDesc();  // sleeves
        case 0xf7:  return "HU " + getSlotDesc();  // gloves
        case 0xf8:  return "HU " + getSlotDesc();  // boots
        case 0xf9:  return "SB " + getSlotDesc();  // shadowblade chest
        case 0xfa:  return "SB " + getSlotDesc();  // leggings
        case 0xfb:  return "SB " + getSlotDesc();  // sleeves
        case 0xfc:  return "SB " + getSlotDesc();  // gloves
        case 0xfd:  return "SB " + getSlotDesc();  // boots
        case 0xfe:  return "SH " + getSlotDesc();  // shaman chest
        case 0xff:  return "SH " + getSlotDesc();  // leggings
        default:           
            return getSlotDesc() + QString().sprintf(" %02x %02x %02x",
                                                     obj_list, obj_index, obj_color);
        }  /* b1=0x02 b2 */

    case 0x03:
        switch (obj_index)  {
        case 0x00:  return "SH " + getSlotDesc();  // sleeves
        case 0x01:  return "SH " + getSlotDesc();  // gloves
        case 0x02:  return "SH " + getSlotDesc();  // boots
        case 0x03:  return "SK " + getSlotDesc();  // skald chest
        case 0x04:  return "SK " + getSlotDesc();  // leggings
        case 0x05:  return "SK " + getSlotDesc();  // sleeves
        case 0x06:  return "SK " + getSlotDesc();  // gloves
        case 0x07:  return "SK " + getSlotDesc();  // boots
        case 0x08:  return "WR " + getSlotDesc();  // warrior chest
        case 0x09:  return "WR " + getSlotDesc();  // leggings
        case 0x0a:  return "WR " + getSlotDesc();  // sleeves
        case 0x0b:  return "WR " + getSlotDesc();  // gloves
        case 0x0c:  return "WR " + getSlotDesc();  // boots
        case 0x0d:  return "EN " + getSlotDesc();  // enchanter chest
        case 0x0e:  return "BL " + getSlotDesc();  // blademaster chest
        case 0x0f:  return "BL " + getSlotDesc();  // leggings
        case 0x10:  return "BL " + getSlotDesc();  // sleeves
        case 0x11:  return "BL " + getSlotDesc();  // gloves
        case 0x12:  return "BL " + getSlotDesc();  // boots
        case 0x13:  return "TN " + getSlotDesc();  // thane chest
        case 0x14:  return "TN " + getSlotDesc();  // leggings
        case 0x15:  return "TN " + getSlotDesc();  // sleeves
        case 0x16:  return "TN " + getSlotDesc();  // gloves
        case 0x17:  return "TN " + getSlotDesc();  // boots
        case 0x18:  return "IN " + getSlotDesc();  // infiltrator chest
        case 0x19:  return "IN " + getSlotDesc();  // leggings
        case 0x1a:  return "IN " + getSlotDesc();  // sleeves
        case 0x1b:  return "IN " + getSlotDesc();  // gloves
        case 0x1c:  return "IN " + getSlotDesc();  // boots
        case 0x1d:  return "FR " + getSlotDesc();  // friar chest
        case 0x1e:  return "WI " + getSlotDesc();  // wizard chest
        case 0x1f:  return "SP " + getSlotDesc();  // spiritmaster chest
        case 0x20:  return "SP " + getSlotDesc();  // leggings
        case 0x21:  return "SP " + getSlotDesc();  // sleeves
        case 0x22:  return "SP " + getSlotDesc();  // gloves
        case 0x23:  return "SP " + getSlotDesc();  // boots
        case 0x24:  return "SO " + getSlotDesc();  // sorcerer chest
        case 0x25:  return "WA " + getSlotDesc();  // warden chest
        case 0x26:  return "WA " + getSlotDesc();  // leggings
        case 0x27:  return "WA " + getSlotDesc();  // sleeves
        case 0x28:  return "WA " + getSlotDesc();  // gloves
        case 0x29:  return "WA " + getSlotDesc();  // boots
        case 0x2a:  return "CH " + getSlotDesc();  // champion chest
        case 0x2b:  return "CH " + getSlotDesc();  // leggings
        case 0x2c:  return "CH " + getSlotDesc();  // sleeves
        case 0x2d:  return "CH " + getSlotDesc();  // gloves
        case 0x2e:  return "CH " + getSlotDesc();  // boots
        case 0x2f:  return "RA " + getSlotDesc();  // ranger chest
        case 0x30:  return "RA " + getSlotDesc();  // leggings
        case 0x31:  return "RA " + getSlotDesc();  // sleeves
        case 0x32:  return "RA " + getSlotDesc();  // gloves
        case 0x33:  return "RA " + getSlotDesc();  // boots
            return getSlotDesc() + QString().sprintf(" %02x %02x %02x",
                                                     obj_list, obj_index, obj_color);
        }  /* b1=0x03 b2 */

    default:
        return getSlotDesc() + QString().sprintf(" %02x %02x %02x",
                                                 obj_list, obj_index, obj_color);
    }  /* b1 */
}

QString exInventoryItem::getSlotDesc(void)
{
    switch (slot)  {
    case 0x0a:  return "right hand";
    case 0x0b:  return "left hand";
    case 0x0c:  return "2h";
    case 0x0d:  return "ranged";
    case 0x15:  return "helm";
    case 0x16:  return "gloves";
    case 0x17:  return "boots";
    case 0x19:  return "chest";
    case 0x1a:  return "cloak";
    case 0x1b:  return "leggings";
    case 0x1c:  return "sleeves";
    default:
        return QString().sprintf("slot (0x%02x)", slot);
    }
}

int exInventoryItem::getSlot(void) const
{
    return slot;
}

int exInventoryItem::getList(void) const
{
    return obj_list;
}

int exInventoryItem::getIndex(void) const
{
    return obj_index;
}

int exInventoryItem::getColor(void) const
{
    return obj_color;
}

exMob::playerClass exInventoryItem::getClassRestriction(void)
{
    /* DO NOT use Helms to identify a class, because most classes
     use the same helm mesh */
    switch (obj_list & 0x0f)
    {
    case 0x00:  return exMob::Unknown;
    case 0x01:
        switch (obj_index)
        {
            // case 0x50:  return exMob::Runemaster;  Helm
            // case 0x51:  return exMob::Shadowblade; Helm
            // case 0xb6:  return exMob::Hero; Helm
        default:    return exMob::Unknown;
        }  /* case list 0x01 */
    case 0x02:
        switch (obj_index)
        {
        case 0xaa:  return exMob::Cabalist;  // chest
        case 0xb0:  return exMob::Armsman;  // chest
        case 0xb1:  return exMob::Armsman;  // leggings
        case 0xb2:  return exMob::Armsman;  // sleeves
        case 0xb3:  return exMob::Armsman;  // gloves
        case 0xb4:  return exMob::Armsman;  // boots
        case 0xb5:  return exMob::Paladin;  // chest
        case 0xb6:  return exMob::Paladin;  // leggings
        case 0xb7:  return exMob::Paladin;  // sleeves
        case 0xb8:  return exMob::Paladin;  // gloves
        case 0xb9:  return exMob::Paladin;  // boots
        case 0xba:  return exMob::Healer;  // chest
        case 0xbb:  return exMob::Healer;  // leggings
        case 0xbc:  return exMob::Healer;  // sleeves
        case 0xbd:  return exMob::Healer;  // gloves
        case 0xbe:  return exMob::Healer;  // boots
        case 0xbf:  return exMob::Runemaster;  // chest
        case 0xc0:  return exMob::Runemaster;  // leggings
        case 0xc1:  return exMob::Runemaster;  // sleeves
        case 0xc2:  return exMob::Runemaster;  // boots
        case 0xc3:  return exMob::Runemaster;  // gloves
        case 0xc4:  return exMob::Hero;  // chest
        case 0xc5:  return exMob::Hero;  // leggings
        case 0xc6:  return exMob::Hero;  // sleeves
        case 0xc7:  return exMob::Hero;  // gloves
        case 0xc8:  return exMob::Hero;  // boots
        case 0xc9:  return exMob::Cleric;  // chest
        case 0xca:  return exMob::Cleric;  // leggings
        case 0xcb:  return exMob::Cleric;  // sleeves
        case 0xcc:  return exMob::Cleric;  // gloves
        case 0xcd:  return exMob::Cleric;  // boots
        case 0xce:  return exMob::Mercenary;  // chest
        case 0xcf:  return exMob::Mercenary;  // leggings
        case 0xd0:  return exMob::Mercenary;  // sleeves
        case 0xd1:  return exMob::Mercenary;  // gloves
        case 0xd2:  return exMob::Mercenary;  // boots
        case 0xd3:  return exMob::Minstrel;  // chest
        case 0xd4:  return exMob::Minstrel;  // leggings
        case 0xd5:  return exMob::Minstrel;  // sleeves
        case 0xd6:  return exMob::Minstrel;  // gloves
        case 0xd7:  return exMob::Minstrel;  // boots
        case 0xd8:  return exMob::Scout;  // chest
        case 0xd9:  return exMob::Scout;  // legs
        case 0xda:  return exMob::Scout;  // sleeves
        case 0xdb:  return exMob::Scout;  // gloves
        case 0xdc:  return exMob::Scout;  // boots
        case 0xdd:  return exMob::Theurgist;  // chest
        case 0xde:  return exMob::Bard;  // chest
        case 0xdf:  return exMob::Bard;  // leggings
        case 0xe0:  return exMob::Bard;  // sleeves
        case 0xe1:  return exMob::Bard;  // gloves
        case 0xe2:  return exMob::Bard;  // boots
        case 0xe3:  return exMob::Druid;  // chest
        case 0xe4:  return exMob::Druid;  // leggings
        case 0xe5:  return exMob::Druid;  // sleeves
        case 0xe6:  return exMob::Druid;  // gloves
        case 0xe7:  return exMob::Druid;  // boots
        case 0xe8:  return exMob::Eldritch;  // chest
        case 0xe9:  return exMob::Mentalist;  // chest
        case 0xea:  return exMob::Nightshade;  // chest
        case 0xeb:  return exMob::Nightshade;  // leggings
        case 0xec:  return exMob::Nightshade;  // sleeves
        case 0xed:  return exMob::Nightshade;  // gloves
        case 0xee:  return exMob::Nightshade;  // boots
        case 0xef:  return exMob::Berserker;  // chest
        case 0xf0:  return exMob::Berserker;  // leggings
        case 0xf1:  return exMob::Berserker;  // sleeves
        case 0xf2:  return exMob::Berserker;  // gloves
        case 0xf3:  return exMob::Berserker;  // boots
        case 0xf4:  return exMob::Hunter;  // chest
        case 0xf5:  return exMob::Hunter;  // leggings
        case 0xf6:  return exMob::Hunter;  // sleeves
        case 0xf7:  return exMob::Hunter;  // gloves
        case 0xf8:  return exMob::Hunter;  // boots
        case 0xf9:  return exMob::Shadowblade;  // chest
        case 0xfa:  return exMob::Shadowblade;  // leggings
        case 0xfb:  return exMob::Shadowblade;  // sleeves
        case 0xfc:  return exMob::Shadowblade;  // gloves
        case 0xfd:  return exMob::Shadowblade;  // boots
        case 0xfe:  return exMob::Shaman;  // chest
        case 0xff:  return exMob::Shaman;  // leggings
        default:    return exMob::Unknown;
        } /* case list 0x02 */
    case 0x03:
        switch (obj_index)
        {
        case 0x00:  return exMob::Shaman;  // sleeves
        case 0x01:  return exMob::Shaman;  // gloves
        case 0x02:  return exMob::Shaman;  // boots
        case 0x03:  return exMob::Skald;  // chest
        case 0x04:  return exMob::Skald;  // leggings
        case 0x05:  return exMob::Skald;  // sleeves
        case 0x06:  return exMob::Skald;  // gloves
        case 0x07:  return exMob::Skald;  // boots
        case 0x08:  return exMob::Warrior;  // chest
        case 0x09:  return exMob::Warrior;  // leggings
        case 0x0a:  return exMob::Warrior;  // sleeves
        case 0x0b:  return exMob::Warrior;  // gloves
        case 0x0c:  return exMob::Warrior;  // boots
        case 0x0d:  return exMob::Enchanter; // chest
        case 0x0e:  return exMob::Blademaster; // chest
        case 0x0f:  return exMob::Blademaster; // leggings
        case 0x10:  return exMob::Blademaster; // sleeves
        case 0x11:  return exMob::Blademaster; // gloves
        case 0x12:  return exMob::Blademaster; // boots
        case 0x13:  return exMob::Thane;  // chest
        case 0x14:  return exMob::Thane;  // leggings
        case 0x15:  return exMob::Thane;  // sleeves
        case 0x16:  return exMob::Thane;  // gloves
        case 0x17:  return exMob::Thane;  // boots
        case 0x18:  return exMob::Infiltrator; // chest
        case 0x19:  return exMob::Infiltrator; // leggings
        case 0x1a:  return exMob::Infiltrator; // sleeves
        case 0x1b:  return exMob::Infiltrator; // gloves
        case 0x1c:  return exMob::Infiltrator; // boots
        case 0x1d:  return exMob::Friar; // chest
        case 0x1e:  return exMob::Wizard; // chest
        case 0x1f:  return exMob::Spiritmaster; // chest
        case 0x20:  return exMob::Spiritmaster; // leggings
        case 0x21:  return exMob::Spiritmaster; // sleeves
        case 0x22:  return exMob::Spiritmaster; // gloves
        case 0x23:  return exMob::Spiritmaster; // boots
        case 0x24:  return exMob::Sorcerer; // chest
        case 0x25:  return exMob::Warden; // chest
        case 0x26:  return exMob::Warden; // leggings
        case 0x27:  return exMob::Warden; // sleeves
        case 0x28:  return exMob::Warden; // gloves
        case 0x29:  return exMob::Warden; // boots
        case 0x2a:  return exMob::Champion; // chest
        case 0x2b:  return exMob::Champion; // leggings
        case 0x2c:  return exMob::Champion; // sleeves
        case 0x2d:  return exMob::Champion; // gloves
        case 0x2e:  return exMob::Champion; // boots
        case 0x2f:  return exMob::Ranger; // chest
        case 0x30:  return exMob::Ranger; // leggings
        case 0x31:  return exMob::Ranger; // sleeves
        case 0x32:  return exMob::Ranger; // gloves
        case 0x33:  return exMob::Ranger; // boots
        default:    return exMob::Unknown;
        }  /* case list 0x03 */
    default:
        return exMob::Unknown;
    }
}

