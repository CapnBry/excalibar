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

#ifndef _EXLINK_H
#define _EXLINK_H

#include <qsocket.h>
#include <qserversocket.h>
#include <qptrlist.h>
#include <qstring.h>

class exLinkServerSocket : public QServerSocket {
  protected:
    class exLink *link;
  public:  
    exLinkServerSocket(class exLink *lnk);
    void newConnection(int socket);
};

class exLinkSocket : public QSocket {
  Q_OBJECT
  protected:
    class exLink *link;
  public:
    exLinkSocket(int sockfd, class exLink *lnk);
  public slots:
    void closed();
};

class exLink : public QObject {
  private:
    friend class exLinkServerSocket;
    friend class exLinkSocket;
  protected:
    exLinkServerSocket *serv_sock;
    QPtrList<exLinkSocket> socks;
  public:
    exLink();
    virtual ~exLink();
    void send(QString txt);
    QString hostport();
    QString descr();
};

#else
class exLinkServerSocket;
class exLinkSocket;
class exLink;
#endif
