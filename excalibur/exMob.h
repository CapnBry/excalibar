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

class exMob;

#ifndef _EXMOB_H
#define _EXMOB_H

#include <qlistview.h>
#include <qstring.h>
#include <iostream>
#include "excalibur.h"
#include "exConnection.h"
#include "exFilter.h"

using namespace std;

class exInventoryItem;

#define DAOCHEAD_TO_DEGREES(x) (float)(x & 0x0fff) * (360.0 / 4096.0)

class exMob : public QListViewItem {
public:
    enum playerClass {
        Unknown,
        Armsman, Cabalist, Cleric, Friar, Infiltrator, Mercenary,
        Minstrel, Paladin, Scout, Sorcerer, Theurgist, Wizard,
        Bard, Blademaster, Champion, Druid, Eldritch, Enchanter,
        Hero, Mentalist, Nightshade, Ranger, Warden,
        Berserker, Healer, Hunter, Runemaster, Shadowblade, Shaman,
        Skald, Spiritmaster, Thane, Warrior
    };

private:
    unsigned int id;
    unsigned int infoid;
    QString name;
    QString surname;
    QString guild;
    unsigned int level;
    unsigned int hp;
    unsigned int mana;
    unsigned int x,y,z;
    float head;
    float headrad;
    unsigned int speed;
    bool mob;
    bool obj;
    bool current;
    bool stealth;
    exTimeType _lasttick;
    exTimeType _lastdist;
    float lastdist;
    exTimeType _lastprojectedPos;
    unsigned int projectedX, projectedY;
    exConnection *c;
    Realm realm;
    QPtrDict<exInventoryItem> inventory;
    playerClass playerclass;
    QColor lastconcolor;
    unsigned int lastconcolortolevel;
    exTimeType lastDist2DL1Ticks;
    unsigned int lastDist2DL1;

    void exMob::setConnection( exConnection *con);


public:
    exMob(QListView *view, exConnection *con, bool newmob, unsigned int newid, unsigned int newinfoid, QString newname, QString newsurname, QString newguild, int newlevel, int nx, int ny, int nz, int nhp, bool newobj);
    virtual int compare(QListViewItem *i, int col, bool ascending) const;
    virtual QString text(int column) const;
    void paintCell(QPainter *p, const QColorGroup &cg, int column, int width, int align);
	
    static QColor getColorForRealm(Realm r);
    static void setFilter( QString );

    const unsigned int getID() const;
    const unsigned int getInfoID() const;
    const QString getName() const;
    const QString getSurname() const;
    const QString getGuild() const;
    const QString getClassName() const;
    const bool isMob() const;
    const bool isObj() const;
    const bool isMobOrObj() const;
    const bool isPlayer() const;
    const bool isInvader() const;
    const bool isDead() const;
    const bool isCurrent() const;
    const bool isStealthed() const;
    const unsigned int getX() const;
    const unsigned int getY() const;
    const unsigned int getZ() const;
    const float getHead() const;
    const int getSpeed() const;
    const unsigned int getLevel() const;
    const unsigned int playerDist2DL1();
    const Realm getRealm() const;
    const QColor getRealmColor() const;
    const QColor getConColor(unsigned int to_level);
    const bool insideRect(QRect &r) const;

    unsigned int getProjectedX();
    unsigned int getProjectedY();
    void updateProjectedPosition();
    float playerDist();

    void touch();
    void checkStale();
    void setPosition(unsigned int nx, unsigned int ny, unsigned int nz);
    void setHead(unsigned int head);
    void setHP(unsigned int hp);
    void setSpeed(unsigned int speed);
    void setRealm(Realm newr);
    void setStealth(bool stealth);
    bool isFiltered();
    void updateInventory(exInventoryItem *ii);
    void clearInventory(void);

    friend ostream& operator << (ostream& os, const exMob &p);
};

class exInventoryItem : public QObject {
private:
    int slot;
    int obj_list;
    int obj_index;
    int obj_color;
public:
    exInventoryItem(int newslot, int newlist, int newindex, int newcolor);
    QString getDescription(void);
    QString getSlotDesc(void);
    int getSlot(void) const;
    int getList(void) const;
    int getIndex(void) const;
    int getColor(void) const;
    exMob::playerClass getClassRestriction(void);
};

#endif
