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
#include "excalibur.h"
#include "exConnection.h"

class exMob : public QListViewItem {
  private:
    unsigned int id;
    unsigned int infoid;
    QString name;
    unsigned int level;
    unsigned int hp;
    unsigned int mana;
    unsigned int x,y,z;
    unsigned int head;
    double headrad;
    unsigned int speed;
    bool mob;
    bool current;
    exTimeType _lasttick;
    exTimeType _lastdist;
    double lastdist;
    exTimeType _lastprojectedX;
    exTimeType _lastprojectedY;
    unsigned int projectedX, projectedY;
    exConnection *c;
    Realm realm;
  public:
    exMob(QListView *view, exConnection *con, bool newmob, unsigned int newid, unsigned int newinfoid, QString newname, int newlevel, int nx, int ny, int nz, int nhp);
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
    unsigned int getProjectedX();
    unsigned int getProjectedY();
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
