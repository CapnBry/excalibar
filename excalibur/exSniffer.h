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


#ifndef _EXSNIFFER_H
#define _EXSNIFFER_H

#include <qthread.h>
#include <qcstring.h>
#include <qptrlist.h>
#include <qevent.h>
#include <qobject.h>
#include <qptrdict.h>
#include <qapplication.h>
#include "exPacket.h"
#include "exConnection.h"

extern "C" {
#include <pcap.h>
}

#define EXSNIFFER_EVENT_TYPE ((QEvent::Type)3210)
#define EXSNIFFER_EVENT_PACKET ((QEvent::Type)3211)

class exPacketEvent : public QCustomEvent {
private:
  exPacket *pck;
public:
  exPacketEvent(exPacket *p);
  virtual ~exPacketEvent();
  exPacket *getPacket() const;
};

class exTCPFragment {
  public:
    unsigned int from;
    unsigned int dlen;
    QByteArray data;
    exTCPFragment(QByteArray *tcppacket, unsigned int baseseq);
};

class exTCP {
  private:
    QPtrList<exTCPFragment> frags;
    unsigned int baseseq;
    unsigned int nextseq;
    int lost;
  public:
    QByteArray data;
    exTCP(unsigned int seq);
    bool add(QByteArray *packet);
    exPacket *grab(bool serv, exTimeType basetick);
};

class exNet {
  public:
    unsigned int n_client_addr;
    unsigned int n_server_addr;
    exTCP *client;
    exTCP *server;
    exConnection *c;
    exTimeType basetick;
    
    exNet();
    ~exNet();
};

class exSniffer : public QObject, public QThread {
  Q_OBJECT
  private:
    pcap_t *pcap;
    QPtrList<QByteArray> packets;
    QPtrDict<exNet> nets;
    QMutex m;
    bool realtime;
    bool link;
    bool capture;

  public:
    exSniffer(bool doreal, bool dolink, bool docapture);
    virtual void run();
    virtual void customEvent(QCustomEvent *e);
    
    void handlePacket(const struct pcap_pkthdr *ph, const u_char *data);
    void add(QByteArray *p);
    QByteArray *get();
    void processPacket(QByteArray *a);
};

#else
class exNet;
class exTCP;
class exTCPFragment;
class exSniffer;
class exPacketEvent;
#endif
