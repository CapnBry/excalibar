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


#include "exLink.h"

exLinkServerSocket::exLinkServerSocket(exLink *lnk) : 
  QServerSocket::QServerSocket(0, 5, lnk)
{
  link = lnk;
}

void exLinkServerSocket::newConnection(int socket) {
  exLinkSocket *sck=new exLinkSocket(socket, link);
  sck->writeBlock("1\r\n", 3);
}

exLinkSocket::exLinkSocket(int sockfd, exLink *lnk) :
  QSocket::QSocket(lnk)
{
  link = lnk;
  setSocket(sockfd);
  connect(this, SIGNAL(connectionClosed()), this, SLOT(closed()));
  lnk->socks.append(this);
}

void exLinkSocket::closed() {
  link->socks.remove(this);
  deleteLater();
}

exLink::exLink() {
  serv_sock=new exLinkServerSocket(this);
}

exLink::~exLink() {
  exLinkSocket *sck;
  while ((sck = socks.first())) 
   delete sck;
  delete serv_sock;
}

void exLink::send(QString txt) {
  exLinkSocket *sck;
  const char *t;
  unsigned int l;

  t=txt.latin1();
  l=strlen(t);
  for (sck = socks.first(); sck; sck = socks.next()) {
    sck->writeBlock(t, l);
    sck->writeBlock("\r\n",2);
  }
}

QString exLink::hostport() {
  return QString("127.0.0.1:%1").arg(serv_sock->port());
}

QString exLink::descr() {
  if (! serv_sock->ok()) 
    return QString("Link: Faulty");
  return QString("Link: Listening on port %1").arg(serv_sock->port());
}
