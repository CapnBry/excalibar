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


#ifndef _OEUPDATE_H
#define _OEUPDATE_H

#include <qurloperator.h>
#include <qnetworkprotocol.h>
#include <qcstring.h>

class oeUpdate : public QObject {
  Q_OBJECT
protected:
  QUrlOperator *uop;
  QByteArray d;
  bool autoupdate;
public:
  oeUpdate(bool doupdate = FALSE);
  ~oeUpdate();
  void fetch();
public slots:
  void data(const QByteArray &, QNetworkOperation *);
  void version_finished(QNetworkOperation *);
  void copy_finished(QNetworkOperation *);
};

class oeHttpCopyFile : public QObject {
  Q_OBJECT
protected:
  QUrlOperator *uop;
  QString fname;
  QByteArray d;
public:
  oeHttpCopyFile(QString filename);
  ~oeHttpCopyFile();
public slots:
  void data(const QByteArray &, QNetworkOperation *);
  void finished(QNetworkOperation *);
};

#else
class oeUpdate;
#endif
