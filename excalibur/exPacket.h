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

#ifndef _EXPACKET_H
#define _EXPACKET_H

#include <sys/types.h>
#include <qmemarray.h>
#include <qdatastream.h>
#include <stdlib.h>
#include <stdio.h>
#include "excalibur.h"

class exPacket {
private:
    void exCrypt_c(char *data, int data_size, const char *key, int key_size);
public:
    uint8_t      *data;
    unsigned int  offset;
    QByteArray    d;
    bool          from_server;
    bool          is_udp;
    exTimeType    tick;
    
    exPacket (void);
    exPacket (char *dt, ssize_t l, bool serv, bool udp, int basetick);
    
    ssize_t        getlen          (void);
    uint8_t        getByte         (void);
    char           getChar         (void);
    uint16_t       getShort        (void);
    uint32_t       getLong         (void);
    QString        getPascalString (void);
    QString        getZeroString   (const unsigned int &minlen = 0);
    QByteArray     getBytes        (const unsigned int &l);
    QString        getDataAsString (void);
    
    void           seek            (const signed int &l);
    
    void           decrypt         (QString key);

    bool isAtEOF(void);
};

QDataStream &operator<<( QDataStream &s, const exPacket &p );
QDataStream &operator>>( QDataStream &s, exPacket &p );

#else
class exPacket;
#endif
