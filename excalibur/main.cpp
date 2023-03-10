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

#include "excalibur.h"
#include <qapplication.h>
#include <qtextbrowser.h>
#include <string.h>
#include <unistd.h>
#include <qfile.h>
#include <qdatastream.h>
#include <qtextstream.h>
#include <qnetwork.h>
#include <sys/mman.h>

#ifndef __APPLE__
#include <dlfcn.h>
#endif

#include "excalibur.h"
#include "exConnection.h"
#include "exMapInfo.h"
#include "exSniffer.h"
#include "quickmath.h"

exTimeType exTick;
exPrefs prefs;

QTime _tick;

bool is_replay;


void updateTick() {
  exTick=_tick.elapsed();
  if (is_replay)
    exTick *=4;
}

int main( int argc, char ** argv )
{
    printf ("Excalibur %s, released under the GPL.\n\n", EX_VERSION);
    printf ("All Excalibur source code is Copyright (C) 2002 by the Excalibur");
    printf (" developers.\n");
    printf ("Some portions are also Copyright (C) 2001 by Slicer/HackersQuest");
    printf (".\n\n");
    printf ("Binary distribution without source code and resale are explictil");
    printf ("y NOT authorized\nby ANY party. If you have paid for this ");
    printf ("software in any way, shape, or form,\nthen the person selling ");
    printf ("the software is doing so in violation of the express\nwishes ");
    printf ("and intents of the authors of this product.\n\n");
    printf ("Please see http://excalibar.sourceforge.net for further");
    printf (" information.\n\n");

    QString opt;
    QStringList opts;

    bool help     = false;

    bool link     = false;
    bool realtime = false;
    bool capture  = false;

    QDict<bool> options;

    options.insert("--help", &help);

    options.insert("--dumpunknown", &prefs.dump_unknown_packets);
    options.insert("--link",&link);
    options.insert("--realtime",&realtime);
    options.insert("--capture",&capture);

    options.insert("--mobseen",&prefs.dump_mobseen);
//    options.insert("--druppy-leak",&prefs.druppy_leak);
    options.insert("--exclude-local",&prefs.exclude_local);

    QApplication a( argc, argv );

    for(uint8_t ui=1;ui<a.argc();ui++) {
      bool *optptr;
      opt=a.argv()[ui];
      optptr=options.find(opt);
      if (optptr) {
         *optptr=true;
      } else {
         opts+=opt;
      }
    }

    if (help)
    {
      printf ("\n");
      printf ("Usage:\n  %s [<options>] [<capture file>]\n\n", argv[0]);
      printf ("  --help          Shows this help.\n");
      printf ("  --dumpunknown   Display unknown packet data in stdout.\n");
      printf ("  --link          Run Excalibur in a special mode, so that the Java tools\n");
      printf ("                  can link with it.\n");
      printf ("  --capture       Turn packet capture files on\n");
      printf ("  --realtime      Set the network thread realtime.\n");
//      printf ("  --druppy-leak   Enable Druppy's message parsing code..\n");
//      printf ("                  * NOTE: It's a massive memory leak!!!\n");
      printf ("  --mobseen       Dump a mob description to stdout when seen\n");
      printf ("  --exclude-local Do not process packets to/from the local machine.  Use this\n");
      printf ("                  if you pass your DAoC traffic through the local maching and\n");
      printf ("                  you are getting two Excalibur windows per DAoC connection.\n");
      qFatal ("Please run '%s' again, without the --help switch.", argv[0]);
    }

    exSniffer *xs;
   
    qInitNetworkProtocols();
    cos_buildtable();
    sin_buildtable();
    exMapInfo::setup("maps/mapinfo.txt");

    exTick = 0;
    _tick.start();

    if (! opts.empty()) {
      exConnection *c=new exConnection(&(opts[0]));
      is_replay = TRUE;
      c->replay();
    } else {
      is_replay = FALSE;
      xs = new exSniffer(realtime, link, capture);
      xs->start();
    }

    a.exec();

    cos_freetable();
    sin_freetable();
    return 0;

}
