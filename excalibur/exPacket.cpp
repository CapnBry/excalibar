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
    exCrypt_c((char *)data,d.size(),key,key.length());
  }
}

void exPacket::exCrypt_c(char *data, int data_size, const char *key, int key_size)
{
    int data_pos;
    int key_pos;
    int status_vect;
    int seed_1;
    int seed_2;

    int work_val;

    if (!data)
        return;

    if (!data_size)
        return;

    if (!key)
        return;

    data_pos = 0;
    key_pos = 0;
    status_vect = 0;
    seed_1 = 1;  // esi
    seed_2 = 2;  // edi

    do {
        if (key_pos == key_size)
            key_pos = 0;

        work_val = key[key_pos] + data_pos + key_pos;
        seed_1 = work_val * seed_1 + 1;
        seed_2 = work_val + seed_2;

        status_vect = status_vect + (seed_1 * seed_2);
        data[data_pos] = data[data_pos] ^ status_vect;

        data_pos++;
        key_pos++;
    } while (data_pos < data_size);
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

uint8_t exPacket::getByte (void) {
  uint8_t v;  
  Q_ASSERT(d.size() >= (offset+1));
  v=(uint8_t) (data[offset]);
  offset++;
  return v;
}

char exPacket::getChar (void) {
  char v;
  Q_ASSERT(d.size() >= (offset));
  v=(char) (data[offset]);
  offset++;
  return v;
}

uint16_t exPacket::getShort (void) {
  uint16_t v;  
  Q_ASSERT(d.size() >= (offset+2));
  v=(uint16_t) ((data[offset]<<8)+(data[offset+1]));
  offset+=2;
  return v;
}

uint32_t exPacket::getLong (void) {
  uint32_t v;
  Q_ASSERT(d.size() >= (offset+4));
  v=(uint32_t) ((data[offset]<<24)+(data[offset+1]<<16)+(data[offset+2]<<8)+(data[offset+3]));
  offset+=4;
  return v;
}

QString exPacket::getPascalString (void) {
  QString v;
  uint8_t l;
  l = getByte();
  Q_ASSERT(d.size() >= (offset+l));
  for (unsigned int i=0; i<l; i++) {
     v.append(((char)(data[offset + i])));
  }
  offset += l;
  return v;
}

QString exPacket::getZeroString(const unsigned int &minlen) {
  QString v;
  char c;
  unsigned int start;

  start=offset;
  if (!isAtEOF())
    do {
      Q_ASSERT(d.size() >= (offset + 1));
      c=(char)(data[offset]);
      if (c) {
        v.append(c);
      }
      offset++;
    } while (c);
  if (offset < start+minlen)
    offset=start+minlen;
  return v;
}

QByteArray exPacket::getBytes(const unsigned int &l) {
  QByteArray v(l);
  Q_ASSERT(d.size() >= (offset + l));
  for (unsigned int i=0; i<l; i++) {
     v[i] = data[offset + i];
  }
  offset+=l;
  return v;
}  

void exPacket::seek(const signed int &l) {
  Q_ASSERT(d.size() >= (offset + l));
  offset+=l;
}

QString exPacket::getDataAsString(void)
{
    QString result;
    QString hex;
    QString ascii;
    unsigned char c;

    for (size_t i=0; i < d.size(); i++)  {
	/* if we're at 16, start a new line */
        if (i && !(i % 16))  {
            result.append(hex + " " + ascii + "\n");
            hex = "";
            ascii = "";
        } 
	/* add some whitespace in the middle of the hex */
	else if ((i % 16) == 8) 
	    hex.append("- ");

        c = (unsigned char)data[i];
        hex.append(QString().sprintf("%02x ", c));
        if ((c < ' ') || (c > '~'))
            ascii.append('.');
        else
            ascii.append(c);
    }  // for i in size

    while (ascii.length() < 16)  {
	if ((ascii.length() % 16) == 8) 
	    hex.append("- ");
	hex.append("   ");
        ascii.append(" ");
    }

    result.append(hex + " " + ascii + "\n");
    return result;
}

bool exPacket::isAtEOF(void)
{
    return offset >= d.size();
}
