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

#ifndef _OELINK_H
#define _OELINK_H

#include <qsocket.h>
#include <qserversocket.h>
#include <qptrlist.h>
#include <qstring.h>

class oeLinkServerSocket : public QServerSocket {
  protected:
    class oeLink *link;
  public:  
    oeLinkServerSocket(class oeLink *lnk);
    void newConnection(int socket);
};

class oeLinkSocket : public QSocket {
  Q_OBJECT
  protected:
    class oeLink *link;
  public:
    oeLinkSocket(int sockfd, class oeLink *lnk);
  public slots:
    void closed();
};

class oeLink : public QObject {
  private:
    friend class oeLinkServerSocket;
    friend class oeLinkSocket;
  protected:
    oeLinkServerSocket *serv_sock;
    QPtrList<oeLinkSocket> socks;
  public:
    oeLink();
    virtual ~oeLink();
    void send(QString txt);
    QString hostport();
    QString descr();
};

#else
class oeLinkServerSocket;
class oeLinkSocket;
class oeLink;
#endif
