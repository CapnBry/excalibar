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


#include "exPacket.h"

exPacket::exPacket() {
  data=NULL;
  offset = 0; 
  from_server=is_udp=false;
  tick=exTick;
}

exPacket::exPacket(char *dt, ssize_t l, bool serv, bool udp, int basetick) {
  d.duplicate(dt, l);
  data=(unsigned char *)((char *)d.data()); 
  from_server = serv; 
  is_udp=udp; 
  offset = 0;
  tick=exTick - basetick;
}

ssize_t exPacket::getlen() {
  return d.size();
}

void exPacket::decrypt(QString key) {
  if (key.length() == 12) { 
    daoccrypt((char *)data,d.size(),key,key.length());
  }
}

QDataStream &operator<<(QDataStream &s, const exPacket &p) {
  int is_server=p.from_server;
  int is_udp=p.is_udp;
  return s<<is_server<<is_udp<<p.tick<<p.d;
}

QDataStream &operator>>(QDataStream &s, exPacket &p) {
  QByteArray qba;
  int from_server;
  int is_udp;
  s>>from_server>>is_udp>>p.tick>>p.d;
  p.from_server=from_server;
  p.is_udp=is_udp;
  p.data=(unsigned char *) ((char *)p.d.data());
  return s;
}

unsigned char exPacket::getByte() {
  unsigned char v;  
  Q_ASSERT(d.size() >= (offset+1));
  v=(unsigned char) (data[offset]);
  offset++;
  return v;
}

char exPacket::getChar() {
  char v;  
  Q_ASSERT(d.size() >= (offset));
  v=(char) (data[offset]);
  offset++;
  return v;
}

unsigned short exPacket::getShort() {
  unsigned short v;  
  Q_ASSERT(d.size() >= (offset+2));
  v=(unsigned short) ((data[offset]<<8)+(data[offset+1]));
  offset+=2;
  return v;
}

unsigned int exPacket::getLong() {
  unsigned int v;  
  Q_ASSERT(d.size() >= (offset+4));
  v=(unsigned int) ((data[offset]<<24)+(data[offset+1]<<16)+(data[offset+2]<<8)+(data[offset+3]));
  offset+=4;
  return v;
}

QString exPacket::getPascalString() {
  QString v;
  int l;
  l=getByte();
  Q_ASSERT(d.size() >= (offset+l));
  for (int i=0;i<l;i++) {
     v.append((char)(data[offset+i]));
  }
  offset+=l;
  return v;
}

QString exPacket::getZeroString(unsigned int minlen) {
  QString v;
  char c;
  unsigned int start;

  start=offset;
  do {
    Q_ASSERT(d.size() >= (offset + 1));
    c=(char)(data[offset]);
    if (c != 0) {
      v.append(c);
    }
    offset++;
  } while (c!=0);
  if (offset < start+minlen)
    offset=start+minlen;
  return v;
}

QByteArray exPacket::getBytes(int l) {
  QByteArray v(l);
  Q_ASSERT(d.size() >= (offset + l));
  for (int i=0;i<l;i++) {
     v[i]=data[offset+i];
  }
  offset+=l;
  return v;
}  

void exPacket::skip(int l) {
  Q_ASSERT(d.size() >= (offset + l));
  offset+=l;
}
