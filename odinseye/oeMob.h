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

class oeMob;

#ifndef _OEMOB_H
#define _OEMOB_H

#include <qlistview.h>
#include "odinseye.h"
#include "oeConnection.h"

class oeMob : public QListViewItem {
  private:
    unsigned int id;
    unsigned int infoid;
    QString name;
    unsigned int level;
    unsigned int hp;
    unsigned int mana;
    unsigned int x,y,z;
    unsigned int head;
    unsigned int speed;
    bool mob;
    bool current;
    oeTimeType _lasttick;
    oeTimeType _lastdist;
    double lastdist;
    oeConnection *c;
    Realm realm;
  public:
    oeMob(QListView *view, oeConnection *con, bool newmob, unsigned int newid, unsigned int newinfoid, QString newname, int newlevel, int nx, int ny, int nz, int nhp);
    virtual int compare(QListViewItem *i, int col, bool ascending) const;
    virtual QString text(int column) const;
    void paintCell(QPainter *p, const QColorGroup &cg, int column, int width, int align);

    unsigned int getID() const;
    unsigned int getInfoID() const;
    QString getName() const;
    bool isMob() const;
    bool isInvader() const;
    bool isDead() const;
    bool isCurrent() const;
    unsigned int getX() const;
    unsigned int getY() const;
    unsigned int getZ() const;
    unsigned int getHead() const;
    unsigned int getSpeed() const;
    unsigned int getLevel() const;
    double playerDist();
    Realm getRealm() const;
    const QColor getColor() const; 
    static QColor getColor(Realm r);

    void touch();
    void stale();
    void setPosition(unsigned int nx, unsigned int ny, unsigned int nz);
    void setHead(unsigned int head);
    void setHP(unsigned int hp);
    void setSpeed(unsigned int speed);
    void setRealm(Realm newr);
};

#endif
