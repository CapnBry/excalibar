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


class oeConnection;

#ifndef _OECONNECTION_H
#define _OECONNECTION_H

#include <qthread.h>
#include <qtextbrowser.h>
#include <qapplication.h>
#include <qptrqueue.h>
#include <qsemaphore.h>
#include <qptrdict.h>
#include <qdict.h>
#include <netinet/in.h>
#include <qmemarray.h>
#include <qdatetime.h>
#include <qtimer.h>
#include "formodinseye.h"
#include "oePacket.h"
#include "oeMob.h"
#include "oeMap.h"
#include "oeSniffer.h"
#include "oePrefs.h"
#include "oeLink.h"

class oeConnection : public QObject {
  Q_OBJECT
protected:
  QPtrDict<oeMob> mobs;
  QPtrDict<oeMob> players;
  QPtrDict<oeMob> mobinfo;
  QDict<int> playerzones;
  QDict<Realm> playerrealms;
  QByteArray cryptkey;
  unsigned int selfid;
  QString *replayfile;
  oeNet *sniff;
  QDataStream *ds;
  QFile *file;
  QTimer timer;
  oePacket *nextpacket;
  oeLink *link;
public slots:
  void listSelectionChanged(QListViewItem *i);
  void replaytimer();
public:
  FormOdinsEye *oe;
  QString playername;
  int playerx, playery, playerz, playerhead, playerspeed,playerzone, playerlevel;
  Realm playerrealm;
  int numPaints;
  bool alive;
  unsigned int selectedid;

  oeConnection(oeNet *s, bool do_link);
  oeConnection(QString *f);
  virtual ~oeConnection();
  void customEvent(QCustomEvent *e);

  void setup();
  void replay();
  void shutdown(QString why = NULL);
  void processPacket(oePacket *p);
  const QPtrDict<oeMob> &getMobs() const;
  void selectID(unsigned int id);
  void spawnEditor();
};

#endif
