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
#include <sys/types.h>
#include <netinet/in.h>
#include <qmemarray.h>
#include <qdatetime.h>
#include <qtimer.h>
#include <iostream>
#include "formexcalibur.h"
#include "exPacket.h"
#include "exMob.h"
#include "exMap.h"
#include "exSniffer.h"
#include "exPrefs.h"
#include "exLink.h"
#include "exFilter.h"
#include "exMessage.h"
#include "messages.h"

using namespace std;

template <class T>
class exMobList : public QPtrDict<T> {
public:
    template<class S> friend ostream
        &operator<< (ostream& os, const exMobList<S> &p);
};

class exConnection : public QObject {
  Q_OBJECT
protected:
  exMobList<exMob> mobs;
  exMobList<exMob> objs;
  exMobList<exMob> players;
  exMobList<exMob> mobinfo;
  QPtrList<exMessage> messageList;
  QDict<int> playerregions;
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
  bool writecapture;
  bool m_bMapCancel;

  void parseCraftingTimer(exPacket *p);
  void parseSetGroundTarget(exPacket *p);
  void parsePlayerInventoryChange(exPacket *p);
  void parseObjectEquipment(exPacket *p);
  void parseObjectStopped(exPacket *p);
  void parsePlayerPosUpdate(exPacket *p);
  void parseMobPosUpdate(exPacket *p);
  void parsePlayerHeadUpdate(exPacket *p);
  void parseSystemMessage(exPacket *p);
  void parseTouchMob(exPacket *p, unsigned int id_offset); 
  void parseSelfHealthUpdate(exPacket *p);
  void dumpPacket(unsigned int command, exPacket *p);
  void clearGroundTarget(void);

public slots:
  void listSelectionChanged(QListViewItem *i);
  void replaytimer();
  void cancelMap (void);
  void resetMap  (void);
  bool checkMap  (void);
public:
  FormExcalibur *ex;
  exMessagesUi  *msgui;
  QString playername;
  int playerx, playery, playerz, playerhead, playerspeed, playerregion, playerlevel;
  int groundtarget_x, groundtarget_y, groundtarget_z;
  exTimeType player_last_update;
  int playerProjectedX, playerProjectedY;
  int player_health, player_mana, player_endurance;
  Realm playerrealm;
  int numPaints;
  bool alive;
  unsigned int selectedid;
  exFilter MobFilter;

  exConnection(exNet *s, bool do_link, bool docapture);
  exConnection(QString *f);
  virtual ~exConnection();
  void customEvent(QCustomEvent *e);

  void setup();
  void replay();
  void shutdown(QString why = NULL);
  void processPacket(exPacket *p);
  const exMobList<exMob> &getMobs() const;
  void selectID(unsigned int infoid);
  void spawnEditor();
  void setFilter( QString Filter);
  void updateProjectedPlayer(void);
  void updateObjectTypeCounts(void);
  void mobWentStale(exMob *m);
  void parseCharacterInfoList(exPacket *p);
};

#endif
