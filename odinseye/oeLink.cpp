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


#include "oeLink.h"

oeLinkServerSocket::oeLinkServerSocket(oeLink *lnk) : 
  QServerSocket::QServerSocket(0, 5, lnk)
{
  link = lnk;
}

void oeLinkServerSocket::newConnection(int socket) {
  oeLinkSocket *sck=new oeLinkSocket(socket, link);
  sck->writeBlock("1\r\n", 3);
}

oeLinkSocket::oeLinkSocket(int sockfd, oeLink *lnk) :
  QSocket::QSocket(lnk)
{
  link = lnk;
  setSocket(sockfd);
  connect(this, SIGNAL(connectionClosed()), this, SLOT(closed()));
  lnk->socks.append(this);
}

void oeLinkSocket::closed() {
  link->socks.remove(this);
  deleteLater();
}

oeLink::oeLink() {
  serv_sock=new oeLinkServerSocket(this);
}

oeLink::~oeLink() {
  oeLinkSocket *sck;
  while ((sck = socks.first())) 
   delete sck;
  delete serv_sock;
}

void oeLink::send(QString txt) {
  oeLinkSocket *sck;
  const char *t;
  unsigned int l;

  t=txt.latin1();
  l=strlen(t);
  for (sck = socks.first(); sck; sck = socks.next()) {
    sck->writeBlock(t, l);
    sck->writeBlock("\r\n",2);
  }
}

QString oeLink::hostport() {
  return QString("127.0.0.1:%1").arg(serv_sock->port());
}

QString oeLink::descr() {
  if (! serv_sock->ok()) 
    return QString("Link: Faulty");
  return QString("Link: Listening on port %1").arg(serv_sock->port());
}
