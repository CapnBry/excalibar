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

#ifndef _EXITEM_H
#define _EXITEM_H

#include "excalibur.h"

#include <qdict.h>
#include <qcstring.h>
#include <qurloperator.h>

class exItemUpload : public QObject {
  Q_OBJECT
protected:
  QUrlOperator *op;
  QByteArray data;
public slots:
  void finished(QNetworkOperation *nop);
public:
  exItemUpload(QByteArray d);
  ~exItemUpload();
};

class exItem : public QObject {
  protected:
    QString name;
    QByteArray data;
    QString info;
  public:
    exItem(QString n, QByteArray d);
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
class exItemUpload;
class exItem;
#endif
