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

#ifndef _OEPACKET_H
#define _OEPACKET_H

#include <sys/types.h>
#include <qmemarray.h>
#include <qdatastream.h>
#include <stdlib.h>
#include <stdio.h>
#include "daoccrypt.h"
#include "odinseye.h"

class oePacket {
  public:
    unsigned char *data;
    unsigned int offset;
    QByteArray d;
    bool from_server;
    bool is_udp;
    oeTimeType tick;
    oePacket();
    oePacket(char *dt, ssize_t l, bool serv, bool udp, int basetick);
    ssize_t getlen();
    unsigned char getByte();
    char getChar();
    unsigned short getShort();
    unsigned int getLong();
    QString getPascalString();
    QString getZeroString(unsigned int minlen=0);
    QByteArray getBytes(int l);
    void skip(int l);
    void decrypt(QString key);
};

QDataStream &operator<<( QDataStream &s, const oePacket &p);
QDataStream &operator>>( QDataStream &s, oePacket &p);

#else
class oePacket;
#endif
