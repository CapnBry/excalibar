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


#ifndef _OESNIFFER_H
#define _OESNIFFER_H

#include <qthread.h>
#include <qcstring.h>
#include <qptrlist.h>
#include <qevent.h>
#include <qobject.h>
#include <qptrdict.h>
#include <qapplication.h>
#include "oePacket.h"
#include "oeConnection.h"

extern "C" {
#include <pcap.h>
}

#define OESNIFFER_EVENT_TYPE ((QEvent::Type)3210)
#define OESNIFFER_EVENT_PACKET ((QEvent::Type)3211)

class oePacketEvent : public QCustomEvent {
private:
  oePacket *pck;
public:
  oePacketEvent(oePacket *p);
  virtual ~oePacketEvent();
  oePacket *getPacket() const;
};

class oeTCPFragment {
  public:
    unsigned int from;
    unsigned int dlen;
    QByteArray data;
    oeTCPFragment(QByteArray *tcppacket, unsigned int baseseq);
};

class oeTCP {
  private:
    QPtrList<oeTCPFragment> frags;
    unsigned int baseseq;
    unsigned int nextseq;
    int lost;
  public:
    QByteArray data;
    oeTCP(unsigned int seq);
    bool add(QByteArray *packet);
    oePacket *grab(bool serv, oeTimeType basetick);
};

class oeNet {
  public:
    unsigned int n_client_addr;
    unsigned int n_server_addr;
    oeTCP *client;
    oeTCP *server;
    oeConnection *c;
    oeTimeType basetick;
    
    oeNet();
    ~oeNet();
};

class oeSniffer : public QObject, public QThread {
  Q_OBJECT
  private:
    pcap_t *pcap;
    QPtrList<QByteArray> packets;
    QPtrDict<oeNet> nets;
    QMutex m;
    bool realtime;
    bool link;
  public:
    oeSniffer(bool doreal, bool dolink);
    virtual void run();
    virtual void customEvent(QCustomEvent *e);
    
    void handlePacket(const struct pcap_pkthdr *ph, const u_char *data);
    void add(QByteArray *p);
    QByteArray *get();
    void processPacket(QByteArray *a);
};

#else
class oeNet;
class oeTCP;
class oeTCPFragment;
class oeSniffer;
class oePacketEvent;
#endif
