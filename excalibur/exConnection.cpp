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

#include <sys/types.h>
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <qstring.h>
#include <qfile.h>
#include <qsplitter.h>
#include <qtabwidget.h>
#include <qframe.h>
#include <qaction.h>
#include <qlabel.h>
#include <qprocess.h>
#include <math.h>
#include <qmessagebox.h>
#include "exConnection.h"
#include "exMessage.h"
#include "exItem.h"

exConnection::exConnection(exNet * s, bool dolink, bool docapture)
{
    QString fname;
    QString from;
    QString to;
    struct in_addr addr;

    setup();
    sniff = s;
    writecapture=docapture;
    
    addr.s_addr = sniff->n_client_addr;
    from = inet_ntoa(addr);
    addr.s_addr = sniff->n_server_addr;
    to = inet_ntoa(addr);

    if(writecapture){
        fname = "Capture-" + from + "-" + to + "-" + QDateTime::currentDateTime().toString("yyyy.MM.dd-hh:mm:ss");

        file->setName(fname);
        file->open(IO_WriteOnly);
        ds = new QDataStream(file);
    }

    if (dolink)
      link = new exLink();
}

exConnection::exConnection(QString * f)
{
    setup();
    replayfile = f;
}

void exConnection::setup()
{
    replayfile = NULL;
    selectedid = 0;
    replayfile = NULL;
    sniff = NULL;

    playerzone = 0;

    file = new QFile;

    ex = new FormExcalibur;
    ex->Map->setConnection(this);
    connect(ex->ListViewMobs, SIGNAL(selectionChanged(QListViewItem *)), this, SLOT(listSelectionChanged(QListViewItem *)));

	msgui = new exMessagesUi;

    alive = true;
    mobs.setAutoDelete(true);
    objs.setAutoDelete(true);
    players.setAutoDelete(true);
    mobinfo.setAutoDelete(false);
    playerzones.setAutoDelete(true);
    playerrealms.setAutoDelete(true);

    ex->show();

    connect(&timer, SIGNAL(timeout()), this, SLOT(replaytimer()));

    link = NULL;
}

exConnection::~exConnection()
{
    mobs.clear();
    objs.clear();
    players.clear();
    mobinfo.clear();

    if (ex)
    delete ex;

    if ( NULL != ds && writecapture)
	delete ds;

    if (file)
	delete file;

    if (link)
	delete link;

}

void exConnection::customEvent(QCustomEvent * e)
{
    exPacketEvent *pe;
    exPacket *p;
    exPacket np;

    if (e->type() != EXSNIFFER_EVENT_PACKET) {
	QObject::customEvent(e);
	return;
    }

    pe = (exPacketEvent *) e;

    p = pe->getPacket();

    if (!p)
	return;

    if (sniff && writecapture) {
	*ds << *p;
	ds->device()->flush();
    }

    updateTick();

    processPacket(p);

    return;
}

void exConnection::replaytimer()
{
    exTimeType delay;

    updateTick();

    if (nextpacket)
	qApp->postEvent(this, new exPacketEvent(nextpacket));

    if (!ds->atEnd()) {
	nextpacket = new exPacket();
	(*ds) >> (*nextpacket);
	delay = nextpacket->tick - exTick;
	if (delay < 0)
	    delay = 0;
        if (delay > 1000)
            delay = 1000;
	timer.start(delay, TRUE);
    }
}

void exConnection::replay()
{
    file->setName(*replayfile);

    if (!file->open(IO_ReadOnly)) {
	qFatal("Couldn't open replayfile");
    }

    ds = new QDataStream(file);

    replaytimer();
}

void exConnection::shutdown(QString why)
{
    if (!alive) {
	return;
    }

    alive = false;

    if (why) {
	qWarning(why);
    }
}

void exConnection::listSelectionChanged(QListViewItem * i)
{
    exMob *mob = (exMob *) i;

    if (mob)
	selectedid = mob->getInfoID();
    else
	selectedid = 0;
    ex->Map->dirty();
}

void exConnection::processPacket(exPacket * p)
{
    unsigned int id;
    unsigned int infoid;
    unsigned int srcid, destid;
    unsigned char bigver;
    unsigned char minver;
    unsigned int head;
    unsigned int linenum;
    unsigned int num;
    QString name;
    QString surname;
    QString guild;
    QString info;
    QString title;
    QByteArray data;
    unsigned int level;
    exMob *mob;
    Realm mobrealm;
    unsigned int x, y, z;
    unsigned int hp;
    bool ismob;
    bool isobj;
    unsigned int command;
    unsigned int seq;
    exMapInfo *mi;
    int what;
    int tp;
    int zone;
    int *intptr;
    Realm *rptr;
    unsigned int i;

    if (!(p->is_udp && !p->from_server)) {
	p->decrypt(cryptkey);
    }

    if (p->is_udp && p->from_server) {
	seq = p->getShort();
	command = p->getByte();
	// dumpPacket(command, p);
	switch (command) {
	  case 0x01:
	    parsePlayerPosUpdate(p);
	    break;
	  case 0x05:
	    parseSelfHealthUpdate(p);
	    break;
          case 0x07:
            parseSystemMessage(p);
            break;
	  case 0x09:
	    parseMobPosUpdate(p);
	    break;
	  case 0x12:
	    parsePlayerHeadUpdate(p);
	    break;
	  case 0xbd:
	    parseObjectStopped(p);
	    break;
          default:
	    if (prefs.dump_unknown_packets)
		dumpPacket(command, p);
	    break;
	}
    } else if (!p->is_udp && !p->from_server) {
	seq = p->getShort();
	srcid = p->getShort();
	p->seek(2);
	command = p->getShort();
	destid = p->getShort();
	switch (command) {
	  case 0x01:
	      p->seek(2);
	      playerz = p->getShort();
	      p->seek(2);
	      playerx = p->getLong();
	      playery = p->getLong();
	      playerhead = (p->getShort()) & 0xfff;
	      mi = ex->Map->getMap();
	      if (mi && !mi->right(playerzone, playerx, playery)) {
		  ex->Map->setMap(NULL);
		  mi = NULL;
	      }
	      if (!mi) {
		  mi = exMapInfo::get(playerzone, playerx, playery);
		  if (mi) {
		      ex->Map->setMap(mi);
		      ex->ListViewMobs->triggerUpdate();
		  }
	      }
              ex->xyzstatus->setText(QString("%1, %2, %3")
                     .arg((mi) ? playerx - mi->getBaseX() : playerx)
                     .arg((mi) ? playery - mi->getBaseY() : playery)
                     .arg(playerz));
              ex->Zone->setText((mi) ? mi->getZoneName() : QString("UNKNOWN"));
	      ex->Map->dirty();
	      if (prefs.sort_when == exPrefs::sortPlayer || prefs.sort_when == exPrefs::sortAlways)
		  ex->ListViewMobs->sort();
              if (link && mi)
                link->send(QString("%1 %2 %3").arg(playerx - mi->getBaseX()).arg(playery - mi->getBaseY()).arg(playerz));
	      break;
	  case 0x12:
	      playerhead = (p->getShort()) & 0xfff;
	      ex->Map->dirty();
	      break;
	  case 0x18:
	      if (ex->AutoSelectTarget->isOn()) {
		  selectID(destid);
		  ex->Map->dirty();
	      }
	      break;
	}
    } else if (!p->is_udp && p->from_server) {
	command = p->getByte();
	// dumpPacket(command, p);
	switch (command) {
	  case 0x01:
	    parsePlayerPosUpdate(p);
	    break;
	  case 0x05:
	    parseSelfHealthUpdate(p);
	    break;
          case 0x07:
            parseSystemMessage(p);
            break;
	  case 0x09:
	    parseMobPosUpdate(p);
	    break;
	  case 0x12:
	    parsePlayerHeadUpdate(p);
	    break;
	  case 0xbd:
	    parseObjectStopped(p);
	    break;
	  case 0x8a:
	      p->seek(2);
	      bigver = p->getByte();
	      minver = p->getByte();
	      p->seek(1);
	      cryptkey = p->getBytes(12);
	      break;
	  case 0x55:
	      p->seek(24);
	      do {
		  name = p->getZeroString(48);
		  p->seek(74);
                  mobrealm = (Realm) p->getByte();
                  p->seek(3);
		  zone = p->getByte();
		  p->seek(57);
		  if ((name.length() > 0) && (zone != 0)) {
		      intptr = new int;

		      *intptr = zone;
		      playerzones.replace(name, intptr);
                      rptr = new Realm;
                      *rptr = mobrealm;
                      playerrealms.replace(name, rptr);
		  }
	      } while (name.length() > 0);
	      break;
	  case 0x88:
	      selfid = id = p->getShort();
	      p->seek(2);
	      playerx = p->getLong();
	      playery = p->getLong();
	      mobinfo.clear();
	      mobs.clear();
              objs.clear();
	      players.clear();
	      ex->Map->dirty();
	      break;
	  case 0x0a:
	      id = p->getShort();
	      mob = mobinfo.take((void *) ((unsigned int) id));
	      if (mob) {
		  if (mob->isMob())
		      mobs.remove((void *) ((unsigned int) mob->getID()));
		  else if (mob->isObj())
                      objs.remove((void *) ((unsigned int) mob->getID()));
                  else
		      players.remove((void *) ((unsigned int) mob->getID()));
	      }
	      ex->Map->dirty();
	      break;
          case 0x71:
	  case 0x72:
          case 0x7c:
	      infoid = 0;
	      head = 0;
              mobrealm = rFriend;
	      if (command == 0x7c) {
		  id = p->getShort();
		  infoid = p->getShort();
		  x = p->getLong();
		  y = p->getLong();
		  z = p->getShort();
		  head = p->getShort();
		  p->seek(4);
                  mobrealm = (Realm) p->getByte();
		  level = p->getByte();
		  p->seek(2);
		  name = p->getPascalString();
		  guild = p->getPascalString();
		  surname = p->getPascalString();
		  ismob = false;
                  isobj = false;
	      } else if (command == 0x72) {
		  infoid = id = p->getShort();
		  p->seek(2);
		  head = p->getShort();
		  z = p->getShort();
		  x = p->getLong();
		  y = p->getLong();
		  p->seek(5);
		  level = p->getByte();
		  p->seek(2);
		  name = p->getPascalString();
		  guild = p->getPascalString();
		  surname = p->getPascalString();
		  ismob = true;
                  isobj = false;
	      } else {
/*000*/           infoid = id = p->getShort();
/*002*/           p->seek(2);
/*004*/           head = p->getShort();
/*006*/           z = p->getShort();
/*008*/           x = p->getLong();
/*012*/           y = p->getLong();
/*016*/           p->seek(4);
/*021*/           level = 0;
/*021*/           name = p->getPascalString();
                  ismob = false;
                  isobj = true;
BEGIN_EXPERIMENTAL_CODE
                  printf("New Object: %4x\n", infoid);
                  dumpPacket(0x71,p);
END_EXPERIMENTAL_CODE
              }

	      mob = NULL;
	      if (ismob)
		  mob = mobs.take((void *) ((unsigned int) id));
	      else if (isobj)
                  mob = objs.take((void *) ((unsigned int) id));
              else
		  mob = players.take((void *) ((unsigned int) id));
	      if (mob) {
		  mobinfo.remove((void *) ((unsigned int) mob->getInfoID()));
		  delete mob;
	      }
	      mobinfo.remove((void *) ((unsigned int) infoid));

	      mob = new exMob(ex->ListViewMobs, this, ismob, id, infoid, name, level, x, y, z, 100, isobj);
	      mob->setHead(head);
              mob->setRealm(mobrealm);

	      if (ismob) {
		  mobs.insert((void *) ((unsigned int) id), mob);
	      } else if (isobj) {
                  objs.insert((void *) ((unsigned int) id), mob);
              } else {
		  players.insert((void *) ((unsigned int) id), mob);
              }
	      mobinfo.insert((void *) ((unsigned int) infoid), mob);
	      ex->Map->dirty();

	      break;
	  case 0x14:
	      p->seek(2);
	      infoid = p->getShort();
	      p->seek(7);
	      hp = p->getByte();
	      mob = mobinfo.find((void *) ((unsigned int) infoid));
	      if (mob) {
		  mob->setHP(hp);
	      }
	      break;
	  case 0x1f:
	      playerzone = p->getShort();
	      intptr = playerzones.find(playername);
	      if (intptr) {
		*intptr = playerzone;
	      }
	      break;
          case 0xaa:
	      num=p->getByte();
              p->seek(3);
              for (i=0;i<num;i++) {
  	        data=p->getBytes(18);
                name=p->getPascalString();
		if (name.length()>0)
                  exItem::seen(name,data);
              }
              break;         
	  case 0x6c:
	      name=p->getPascalString();
              info="";
              do {
                linenum=p->getByte();
                if (linenum != 0) 
                  info=info.append(p->getPascalString()).append("\n");
              } while (linenum != 0);              
              exItem::seen(name,info);              
              break;
	  case 0xbe:
	      what = p->getByte();
	      p->seek(1);
	      tp = p->getByte();
	      if ((what == 3) && (tp == 0)) {
		  p->seek(1);
		  playerlevel = p->getByte();
		  playername = p->getPascalString();
                  title = QString("Excalibur -- ").append(playername);
                  if (link)
                    title = title.append("  ").append(link->descr());
		  ex->setCaption(title);
		  if (playerzone == 0) {
		      intptr = playerzones.find(playername);
		      if (intptr) {
			  playerzone = *intptr;
		      }
		      rptr = playerrealms.find(playername);
		      if (rptr) {
			  playerrealm = *rptr;
		      }
		  }
	      }
	      break;
          case 0x1e: /* Stealth */
          case 0x25: /* OpCodes */
              break;
	  default:
	      if (prefs.dump_unknown_packets)
		  dumpPacket(command, p);	
	      break;
	}
    }
}

void exConnection::parseObjectStopped(exPacket *p)
{
    unsigned int infoid = p->getShort();
    exMob *mob = mobinfo.find((void *)infoid);
    if (mob)  
	mob->setSpeed(0);
}

void exConnection::parseMobPosUpdate(exPacket *p)
{
    unsigned int speed = p->getShort();
    unsigned int head = p->getShort();
    unsigned int x = p->getLong();
    unsigned int y = p->getLong();
    p->seek(8);
    unsigned int z = p->getShort();
    unsigned int id = p->getShort();
    p->seek(2);
    unsigned int hp = p->getByte();
    exMob *mob = mobs.find((void *)id);
    if (mob) {
	mob->setPosition(x, y, z);
	mob->setHead(head);
	mob->setSpeed(speed);
	mob->setHP(hp);
	ex->Map->dirty();
	if (prefs.sort_when == exPrefs::sortAlways)
	    ex->ListViewMobs->sort();
    }
}

void exConnection::parsePlayerPosUpdate(exPacket *p)
{
    unsigned int id = p->getShort();
    unsigned int speed = p->getShort();
    unsigned int z = p->getShort();
    p->seek(2);
    unsigned int x = p->getLong();
    unsigned int y = p->getLong();
    unsigned int head = p->getShort();
    exMob *mob = players.find((void *)id);
    if (mob) {
	mob->setPosition(x, y, z);
	mob->setHead(head);
	mob->setSpeed(speed);
	ex->Map->dirty();
	if (prefs.sort_when == exPrefs::sortAlways)
	    ex->ListViewMobs->sort();
    }
}

void exConnection::parsePlayerHeadUpdate(exPacket *p)
{
    unsigned int id = p->getShort();
    unsigned int head = p->getShort();
    p->seek(4);
    unsigned int hp = p->getByte();
    exMob *mob = players.find((void *)id);
    if (mob) {
	mob->setHead(head);
	if (hp < 100)
	    mob->setHP(hp);
	ex->Map->dirty();
	if (prefs.sort_when == exPrefs::sortAlways)
	    ex->ListViewMobs->sort();
    }
}

void exConnection::parseSystemMessage (exPacket *p)
{
    exMessage *msg;
    QWidget *tab;
    QTextEdit *textbox;
    p->seek(7);

    p->getByte();

    char*        Message     = strdup(p->getZeroString().ascii());

//    qWarning("Type: %x - Message: %s", MessageType, Message);

    msg = new exMessage( new QString( Message));
    msg->parseMsg();

    // Insert it into the ALL tab
    tab = msgui->tabWidget->page( 0);
    textbox = (QTextEdit*)tab->childAt( 10, 10);
    textbox->append( "[" + msg->getMsgType() + "] " + msg->getFormattedText());

    tab = msgui->tabWidget->page( msg->getType());
    textbox = (QTextEdit*)tab->childAt( 10, 10);
    textbox->append( msg->getFormattedText());
}

void exConnection::parseTouchMob(exPacket *p, unsigned int id_offset)
{
    p->seek(id_offset);
    unsigned int infoid = p->getShort();
    exMob *mob = mobinfo.find((void *)infoid);
    if (mob)
	mob->touch();
BEGIN_EXPERIMENTAL_CODE
    if (!mob)
	qWarning("parseTouchMob: mobinfo not found infoid %04x", infoid);
END_EXPERIMENTAL_CODE
}

void exConnection::parseSelfHealthUpdate(exPacket *p)
{
    player_health = p->getByte();
    player_mana = p->getByte();
    p->seek(3);
    player_endurance = p->getByte();
}

void exConnection::selectID(unsigned int id)
{
    exMob *m;

    selectedid = id;

    m = mobinfo.find((void *) id);
    if (m) {
	m->touch();
	ex->ListViewMobs->setSelected(m, true);
	ex->ListViewMobs->ensureItemVisible(m);
    }
    ex->Map->dirty();
}

const QPtrDict < exMob > &exConnection::getMobs() const
{
    return mobinfo;
}

void exConnection::spawnEditor() 
{
  exMapInfo *mi;

  if (! link) {
    QMessageBox::critical(ex, "No Link", "You need to start Excalibur with the --link argument to enable\nposition passing to the external editor");
    return;
  }
  mi=ex->Map->getMap();
  if (! mi) {
    QMessageBox::critical(ex, "No Map Information", "The area you are in must be named in\nmapinfo.txt before you can enable editing.");
    return;
  }
  QProcess *p=new QProcess();
  p->addArgument("java");
  p->addArgument("-jar");
  p->addArgument("editor.jar");
  p->addArgument(link->hostport());
  QFile f;
  f.setName(QString("usermaps/").append(mi->getName()));
  if (f.exists()) {
    p->addArgument(f.name());
  } else {
    p->addArgument(QString("maps/").append(mi->getName()));
  }
  p->addArgument(QString("usermaps/").append(mi->getName()));
  p->start();
}

void exConnection::dumpPacket(unsigned int command, exPacket *p)
{
    printf ("%s packet %s server.  Command %02x  Size %d\n", 
	      (p->is_udp) ? "UDP" : "TCP",
	      (p->from_server) ? "FROM" : "TO",
	      command,
              p->getlen());
    printf ("%s", p->getDataAsString().ascii());
}

void exConnection::setFilter( QString Filter)
{
  MobFilter.setFilter( Filter);
  printf( "New Filter: %s\n", MobFilter.getFilter().ascii());

  this->ex->ListViewMobs->repaint();
}

