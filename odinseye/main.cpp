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

#include <qapplication.h>
#include <qtextbrowser.h>
#include <dlfcn.h>
#include <string.h>
#include <unistd.h>
#include <qfile.h>
#include <qdatastream.h>
#include <qtextstream.h>
#include <qnetwork.h>
#include <sys/mman.h>

#include "oeConnection.h"
#include "oeMapInfo.h"
#include "daoccrypt.h"
#include "oeSniffer.h"
#include "oeUpdate.h"
#include "oeItem.h"

daoccryptfunc daoccrypt;
oeTimeType oeTick;
oePrefs prefs;

QTime _tick;

bool is_replay;

void updateTick() {
  oeTick=_tick.elapsed();
  if (is_replay)
    oeTick *=4;
}

bool prepareCrypt() {
  char name[4096];
  void *handle;

  getcwd(name, 4096);
  strcat(name, "/daoccrypt.so");

  handle=dlopen(name,RTLD_LAZY);
  if (handle == NULL) {
    return false;
  }

  daoccrypt=(daoccryptfunc) dlsym(handle, "daoccrypt");
  if (daoccrypt == NULL) {
    return false;
  }

  return true;
}

int main( int argc, char ** argv )
{
    QApplication a( argc, argv );
    oeSniffer *os;
    oeUpdate *upd;

    QString opt;
    QStringList opts;
    bool update = false;
    bool realtime = false;
    bool link = false;

    QDict<bool> options;

    qInitNetworkProtocols();

    options.insert("--update",&update);
    options.insert("--realtime",&realtime);
    options.insert("--link",&link);

    if (! prepareCrypt()) {
      qFatal("Failed to load crypt library");
    }

    oeMapInfo::setup("maps/mapinfo.txt");
    oeItem::init();
  
    for(int i=1;i<a.argc();i++) {
      bool *optptr;
      opt=a.argv()[i];
      optptr=options.find(opt);
      if (optptr) {
         *optptr=true;
      } else {
         opts+=opt;
      }
    }      

    _tick.start();

    if (! opts.empty()) {
      oeConnection *c=new oeConnection(&(opts[0]));
      is_replay = TRUE;
      c->replay();
    } else {
      is_replay = FALSE;
      upd=new oeUpdate(update);
      upd->fetch();
      os = new oeSniffer(realtime, link);
      os->start();
    }

    return a.exec();
}
