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

#ifndef _OEITEM_H
#define _OEITEM_H

#include "odinseye.h"

#include <qdict.h>
#include <qcstring.h>
#include <qurloperator.h>

class oeItemUpload : public QObject {
  Q_OBJECT
protected:
  QUrlOperator *op;
  QByteArray data;
public slots:
  void finished(QNetworkOperation *nop);
public:
  oeItemUpload(QByteArray d);
  ~oeItemUpload();
};

class oeItem : public QObject {
  protected:
    QString name;
    QByteArray data;
    QString info;
  public:
    oeItem(QString n, QByteArray d);
    int quality();
    
    static void seen(QString n, QByteArray d);
    static void seen(QString n, QString i);

    static void init();
    static void load();
    static void save(QDataStream *ds);
    static void save();
    
    static void upload(bool fromgui = TRUE);
};

#else
class oeItemUpload;
class oeItem;
#endif
