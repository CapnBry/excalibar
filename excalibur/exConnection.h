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


class exConnection;

#ifndef _EXCONNECTION_H
#define _EXCONNECTION_H

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
#include "formexcalibur.h"
#include "exPacket.h"
#include "exMob.h"
#include "exMap.h"
#include "exSniffer.h"
#include "exPrefs.h"
#include "exLink.h"

class exConnection : public QObject {
  Q_OBJECT
protected:
  QPtrDict<exMob> mobs;
  QPtrDict<exMob> players;
  QPtrDict<exMob> mobinfo;
  QDict<int> playerzones;
  QDict<Realm> playerrealms;
  QByteArray cryptkey;
  unsigned int selfid;
  QString *replayfile;
  exNet *sniff;
  QDataStream *ds;
  QFile *file;
  QTimer timer;
  exPacket *nextpacket;
  exLink *link;
  
  void parseObjectStopped(exPacket *p);
  void parsePlayerPosUpdate(exPacket *p);
  void parseMobPosUpdate(exPacket *p);
  void parsePlayerHeadUpdate(exPacket *p);
  void dumpPacket(unsigned int command, exPacket *p);
public slots:
  void listSelectionChanged(QListViewItem *i);
  void replaytimer();
public:
  FormExcalibur *ex;
  QString playername;
  int playerx, playery, playerz, playerhead, playerspeed,playerzone, playerlevel;
  Realm playerrealm;
  int numPaints;
  bool alive;
  unsigned int selectedid;

  exConnection(exNet *s, bool do_link);
  exConnection(QString *f);
  virtual ~exConnection();
  void customEvent(QCustomEvent *e);

  void setup();
  void replay();
  void shutdown(QString why = NULL);
  void processPacket(exPacket *p);
  const QPtrDict<exMob> &getMobs() const;
  void selectID(unsigned int id);
  void spawnEditor();
};

#endif
