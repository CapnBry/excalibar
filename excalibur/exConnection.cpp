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
#include <qregexp.h>
#include <math.h>
#include <qmessagebox.h>
#include <qstatusbar.h>
#include "exConnection.h"
#include "quickmath.h"

template <class S>
    ostream & operator<< (ostream & os, const exMobList<S> &p)
{
  QPtrDictIterator<exMob> mobi(p);
  for (;mobi.current(); ++mobi)
      os << mobi.current();
  return os;
}

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

    qWarning("New connection established: " + from + " -> " + to);

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
    nextpacket = NULL;

    playerregion = 0;
    playerspeed = 0;
    playerhead = 0.0f;
    playerx = 0;
    playery = 0;
    playerz = 0;
    playerrealm = rFriend;
    player_last_update = 0;

    clearGroundTarget();

    file = new QFile;

    ex = new FormExcalibur;
    ex->Map->setConnection(this);
    xpStats = new XPStats();
    xpStats->setForm(ex);
    connect(ex->ListViewMobs, SIGNAL(selectionChanged(QListViewItem *)), this, SLOT(listSelectionChanged(QListViewItem *)));

    // msgui = new exMessagesUi;

    alive = true;
    mobs.setAutoDelete(true);
    objs.setAutoDelete(true);
    players.setAutoDelete(true);
    mobinfo.setAutoDelete(false);
    playerregions.setAutoDelete(true);
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

    if (xpStats)
        delete xpStats;

    //if (msgui)
    //    delete msgui;
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
    else  {
        nextpacket = NULL;
        timer.start(500, TRUE);
        qApp->quit();
    }
}

void exConnection::replay()
{
    file->setName(*replayfile);

    if (!file->open(IO_ReadOnly)) {
        qFatal("Couldn't open replayfile: " + QString(*replayfile));
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
    unsigned int srcid;
    unsigned int command;
    unsigned int seq;

    if (p->is_udp && p->from_server)
        seq = p->getShort();
	
    if (!(p->is_udp && !p->from_server))
	    p->decrypt(cryptkey);

    if (!p->is_udp && !p->from_server) {
	    seq = p->getShort();
	    srcid = p->getShort();
	    p->seek(2);
    }
			
    if (p->from_server) {
	    command = p->getByte();
	    //dumpPacket(command, p);
	    switch (command) {
            case 0x01:  parsePlayerPosUpdate(p); break;
	        case 0x05:  parseSelfHealthUpdate(p); break;
            case 0x07:  parseSystemMessage(p); break;
	        case 0x09:  parseMobPosUpdate(p); break;
	        case 0x0a:  parseDeleteObject(p); break;
	        case 0x12:  parsePlayerHeadUpdate(p); break;
	        case 0x14:  parseCombatSwing(p); break;
	        case 0x1f:  parseRegionChange(p); break;
            case 0x39:  parseXPUpdate(p); break;
            case 0x49:  parseCharStealthed(p); break;
	        case 0x55:  parseCharacterInfoList(p); break;
            case 0x5b:  parseCraftingTimer(p); break;
	        case 0x6c:  parseDetailWindowDisplay(p); break;
            case 0x71:  parseNewObject(p, command); break;
	        case 0x72:  parseNewObject(p, command); break;
            case 0x7c:  parseNewObject(p, command); break;
            case 0x82:  parseLoginGranted(p); break;
	        case 0x88:  parseCharLoginInit(p); break;
            case 0x8a:  parseVersionAndCryptKey(p); break;
          	case 0xaa:  parsePlayerInventoryChange(p); break;         
            case 0xbd:  parseObjectEquipment(p); break;
	        case 0xbe:  parsePlayerStatsUpdate(p); break;
		
    	    /****************************************************************/
	        /* OpCodes that haven't been implemented or with no use.        */
            /* See DOL EMU refers to packet documentation that can be found */
	        /* at the Dawn of Light emulator forums.                        */
            /* http://dolserver.sourceforge.net                             */
	        /****************************************************************/

	        // Dead
	        // 2 bytes - player id
	        // 2 bytes - killer id
	        // 4 bytes - unknown
	        case 0x06: break;
   	        // Player quit
	        // 1 byte - kill client yes/no
	        case 0x0C: break;
	        // Encumberance update
	        // 2 bytes - total
	        // 2 bytes - used
	        case 0x15: break;
	        // Pet window
	        // 2 bytes - mob id of pet
	        // 2 bytes - unused
	        // 1 byte - 0 released, 1 normal, 2 just charmed?
	        // 1 byte - 1 aggressive, 2 defensive, 3 passive
	        // 1 byte - 1 follow, 2 stay, 3 goto, 4 here
	        // 1 byte - unused
	        // list of shorts, null-terminated (byte) - spell icons on pet
	        case 0x20: break;
	        // Character rezzed
	        // 2 bytes - character id
	        // 2 bytes - unused
	        case 0x21: break;
            case 0x29: break; // Show dialog box - See DOL EMU
	        case 0x2b: break; // Quest journal updates - See DOL EMU
	        case 0x2e: break; // Find groups members dialog update - See DOL EMU
            case 0x36: break; // Region server address info - See DOL EMU
	        case 0x51: break; // Player animation - See DOL EMU
	        // Money update
	        // 1 byte - copper
	        // 1 byte - silver
	        // 2 bytes - gold
	        // 2 bytes - platinum
	        // 2 bytes - mithril
	        case 0x52: break;
	        // Character stats
	        // 2 bytes each - str, con, dex, qui, pie, emp, cha
	        // 2 bytes each - strAdd, etc.
	        // 2 bytes - health
	        // 2 bytes - unknown
	        case 0x53: break;
		    // Realm update
            // 1 byte - 0 None, 1 Albion, 2 Midgard, 3 Hibernia
            case 0x56: break;
			// Character create response
			// 24 bytes - character name
			case 0x58: break;
			case 0x64: break; // Name check response - See DOL EMU
			case 0x6b: break; // Name check response - See DOL EMU
			case 0x6d: break; // Online friends list - See DOL EMU
			case 0x6e: break; // Online friends list - See DOL EMU
			// Player model change
			// 2 bytes - player id
			// 2 bytes - model id
			// 4 bytes - unused
			case 0x73: break;
			// Disable player ability (grey-out timed abilities)
			// 2 bytes - duration
			// 1 byte - ability id
			case 0x7e: break;
			// Session id
			// 2 bytes - session id in low endian
			case 0x80: break;
			case 0x81: break; // Reply to server ping request
			case 0x84: break; // Login denied - See DOL EMU
			case 0x85: break; // Game open reply
			case 0xac: break; // Player jump - See DOL EMU
			case 0xbf: break; // Merchant window - See DOL EMU
			case 0xd3: break; // Trainer window - See DOL EMU
			case 0xd6: break; // Set time - See DOL EMU
      	    case 0xd7: break; // Self buffs display - See DOL EMU
			case 0xd8: break; // Player group update (can also update player buffs) - See DOL EMU
			case 0xdb: break; // Stop animation(interruption) - See DOL EMU
			case 0xdc: break; // Atack mode - See DOL EMU
			case 0xdd: break; // Display shared buffs - See DOL EMU
	  		default:
				if (prefs.dump_unknown_packets)
					dumpPacket(command, p);	
	      		break;
		}
	}
	else {
		command = p->getShort();
		//dumpPacket(command, p);
		switch (command) {
	  		case 0x01:  parsePlayerPosUpdateFromClient(p); break;
            case 0x12:  parsePlayerHeadUpdateFromClient(p); break;
	        case 0x18:  parseChangeTarget(p); break;
            case 0x44:  parseSetGroundTarget(p); break;

            // See DOL EMU for more information on the following unused packets
            case 0x07:  // Chat messages in console
            case 0x0b:  // Ping
            case 0x0f:  // Login request
            case 0x13:  // Use skill
            case 0x16:  // Mob creation request
            case 0x1d:  // Get active item request (from ground)
            case 0x22:  // Pet command
            case 0x28:  // Destroy inventory item
            case 0x2a:  // Dialog response
            case 0x2c:  // Looking for group
            case 0x2d:  // Find group/player request
            case 0x2f:  // Group invite
            case 0x30:  // Accept group invite
            case 0x31:  // Click a door
            case 0x35:  // Region id requests
            case 0x37:  // Disband group
			case 0x38:  // Zone jump request
            case 0x40:  // World init for a player
            case 0x45:  // Make crafting product
            case 0x48:  // Appraise item
            case 0x4a:  // Choose emblem
            case 0x50:  // Send remove affect
            case 0x54:  // Realm check type stuff
            case 0x58:  // Character update
            case 0x5c:  // Crypt key request
            case 0x63:  // Name check
            case 0x65:  // Character creation confirmation
            case 0x68:  // Character deletion
            case 0x6a:  // Name check 2
            case 0x6f:  // Sit/stand
            case 0x70:  // Spell detail display request
            case 0x74:  // Update combat message filter
            case 0x75:  // Drop/exchange item
            case 0x7c:  // World init request for a player
            case 0x7d:  // Player creation request
            case 0xb8:  // Character select
            case 0xd0:  // Buy from merchant
            case 0xd1:  // Sell item
            case 0xd2:  // Mob interact
            case 0xd4:  // Train skill, RA
            case 0xd5:  // Send cast spell
            case 0xd9:  // Use item
            case 0xdc:  // Attack mode
                break;           

            default:
	            if (prefs.dump_unknown_packets)
		            dumpPacket(command, p);
	            break;
	    }
    }
}

void exConnection::parseXPUpdate(exPacket *p)
{
    // 4 bytes - realm points
    p->seek(4);
    // True exp is never sent, only a number from 0 to 999
    // to indicate where to move the exp bar
    int exp_ticks = p->getShort();
    xpStats->MoveXPBar(exp_ticks);
	// 2 bytes - spec_points
    // 4 bytes - bounty_points
}

void exConnection::parseCharStealthed(exPacket *p)
{
    unsigned int id;
	unsigned int infoid = id = p->getShort();
    exMob *mob = mobinfo.find((void *) ((unsigned int) infoid));
    if (mob) {
    	mob->setStealth(TRUE);
        ex->Map->dirty();
    }
}

void exConnection::parsePlayerStatsUpdate(exPacket *p)
{
	// Documentation for this packet can be found at DOL EMU.
	// This packet has a lot of formats, each which are quite
	// extensive.
	// For example, this is the type of data that can be extracted.
	// 0x01 (spec, abil, styles, spells, songs, RA)
	// 0x02 (spell trees)
	// 0x03 (name classes)
	// 0x05 (combat stats)
	// 0x06 (group window update)
	// 0x08 (crafting skills)
	int what = p->getByte();
	p->seek(1);
	int tp = p->getByte();
	if ((what == 3) && (tp == 0)) {
		p->seek(1);
		playerlevel = p->getByte();
		playername = p->getPascalString();
        QString title = QString("Excalibur -- ").append(playername);
        if (link)
        	title = title.append("  ").append(link->descr());
        ex->setCaption(title);
		// if (playerregion == 0) {
		int *intptr = playerregions.find(playername);
		if (intptr) {
			playerregion = *intptr;
		}
		Realm *rptr = playerrealms.find(playername);
		if (rptr) {
			playerrealm = *rptr;
		}
		// }
	}
    updateObjectTypeCounts();
}

void exConnection::parseDetailWindowDisplay(exPacket *p)
{
	QString name=p->getPascalString();
    QString info="";
    unsigned int linenum;
    do {
    	linenum=p->getByte();
        if (linenum != 0) 
        	info=info.append(p->getPascalString()).append("\n");
	} while (linenum != 0);              
}

void exConnection::parseRegionChange(exPacket *p)
{
	playerregion = p->getShort();
	int *intptr = playerregions.find(playername);
	if (intptr) {
		*intptr = playerregion;
    }
    /* clear the groundtarget when we change regions, since the
       target is region-specific */
    clearGroundTarget();
}

void exConnection::parseCombatSwing(exPacket *p)
{
	p->seek(2); // attacker id
	unsigned int infoid = p->getShort();
	// 2 bytes - weapon model ID
	// 2 bytes - unknown
	// 2 bytes - style ID
	// 1 byte - swing result (0x00 missed, 0x01 parried, 0x02 blocked, 0x03 evaded, 
	//						  0x0A hit, 0x0B style performed, 0x80 fired weapon)
	p->seek(7);
	unsigned int hp = p->getByte();
	exMob *mob = mobinfo.find((void *) ((unsigned int) infoid));
	if (mob) {
		mob->setHP(hp);
		updateObjectTypeCounts();
	}
}

void exConnection::parseLoginGranted(exPacket *p)
{
	serverprotocol = p->getByte();
    // 1 byte - minor version
	// 1 byte - build version
	// ?? bytes - account name
	// ?? bytes - server name
}

void exConnection::parseDeleteObject(exPacket *p)
{
	unsigned int infoid = p->getShort();
	exMob *mob = mobinfo.take((void *) ((unsigned int) infoid));
	if (mob) {
		if (mob->isMob())
			mobs.remove((void *) ((unsigned int) mob->getID()));
		else if (mob->isObj())
        	objs.remove((void *) ((unsigned int) mob->getID()));
        else
			players.remove((void *) ((unsigned int) mob->getID()));
	}
	// 2 bytes - type to remove (0x0000 objects/npcs, 0x0001 unknown, 0x0002 player)
    updateObjectTypeCounts();
	ex->Map->dirty();
}

void exConnection::parseNewObject(exPacket *p, int command)
{
	unsigned int infoid = 0;
	unsigned int head = 0;
    unsigned int id;
    unsigned int x, y, z;
    unsigned int level;
    bool ismob;
    bool isobj;
    QString name;
    QString guild;
    QString surname;

	Realm mobrealm = rFriend;

	if (command == 0x7c) {
		id = p->getShort();
		infoid = p->getShort();
		x = p->getLong();
		y = p->getLong();
		z = p->getShort();
		head = p->getShort();
		// 2 bytes - HHHS SMMM MMMM MMMM
		// H - hair(0-7)
		// S - size(1 small, 2 medium, 3 tall)
		// M - 11 bit model id
		// 1 byte - player death status (0 dead, 1 alive)
		// 1 byte - unused
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
		// 2 bytes - mob speed
		p->seek(2);
		head = p->getShort();
		z = p->getShort();
		x = p->getLong();
		y = p->getLong();
		// 2 bytes - unknown
		// 2 bytes - model
		// 1 byte - size of mob in % of default model size
		p->seek(5);
		level = p->getByte();
		// 1 byte - RRXX XXXX
		// RR - mob realm (0 any realm can kill, 1 Albion, 2 Migard, 3 Hibneria)
		// X - Unknown
		// 1 byte - maximum stick distance, standard is 0x20
		p->seek(2);
		name = p->getPascalString();
		guild = p->getPascalString();
		ismob = true;
        isobj = false;
	} else {
		infoid = id = p->getShort();
		p->seek(2);
		head = p->getShort();
		z = p->getShort();
		x = p->getLong();
		y = p->getLong();
		// 2 bytes - object model id
		// 2 bytes - unknown
		p->seek(4);
		level = 0;
		name = p->getPascalString();
        ismob = false;
        isobj = true;
BEGIN_EXPERIMENTAL_CODE
        printf("New Object: %4x\n", infoid);
        dumpPacket(0x71,p);
END_EXPERIMENTAL_CODE
	}

	exMob *mob = NULL;
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

	mob = new exMob(ex->ListViewMobs, this, ismob, id, infoid, name, surname, guild, level, x, y, z, 100, isobj);
	mob->setHead(head);
    mob->setRealm(mobrealm);

	if (ismob) {
		mobs.insert((void *) ((unsigned int) id), mob);
        exMapInfo *mi = ex->Map->getMap();
        if (prefs.dump_mobseen)
        	cout << "MOBseen," << ((mi) ? mi->getZoneNum() : 0) <<
                           "," << ((mi) ? x - mi->getBaseX() : x) <<
                           "," << ((mi) ? y - mi->getBaseY() : y) <<
                           "," << z <<
                           "," << level <<
                           "," << name <<
                           "," << (guild.length() ? guild : QString("")) <<
                           endl;
	} else if (isobj) {
    	objs.insert((void *) ((unsigned int) id), mob);
    } else {
    	players.insert((void *) ((unsigned int) id), mob);
        if (prefs.vader_warn && mob->isInvader() && (mob->getLevel() > 15)) {
        	QString title = QString("*** INVADER DETECTED *** Name: %1, Level: %2, Distance: %3").
                          	arg(mob->getName()).arg(mob->getLevel()).arg(mob->playerDist());
            //qWarning(title);
            ex->statusBar()->message(title, 10000);
            qApp->beep();
		}
	}
	mobinfo.insert((void *) ((unsigned int) infoid), mob);
    updateObjectTypeCounts();
	ex->Map->dirty();
}

void exConnection::parseCharLoginInit(exPacket *p)
{
	selfid = p->getShort();
	// 2 bytes - player z
	p->seek(2);
	playerx = p->getLong();
	playery = p->getLong();
	mobinfo.clear();
	mobs.clear();
	objs.clear();
	players.clear();
	ex->Map->dirty();
    xpStats->StartSession();
	// 2 bytes - heading
}

void exConnection::parseVersionAndCryptKey(exPacket *p)
{
	p->seek(1);
	// 0x32 SI, 0x31 Normal
    serverprotocol = p->getByte();
    p->seek(3);  // x.yz version
    cryptkey = p->getBytes(12);
}

void exConnection::parseCraftingTimer(exPacket *p)
{
    int time_count;
    QString product;

	// I have seen this timer sent for stuff besides crafting.
	// Will have to look into this, might be the rez timer.

    if ( prefs.crafting_alerts )
    {
        time_count = p->getShort();
		// 1 byte - size of craft window title
		// 1 byte - 0 stop crafting, 1 start crafting
        p->seek(2);
        product = p->getZeroString();

        if ( time_count > 0 )
        {
            printf( "Crafting timer begin (%i secs): %s\n",
                    time_count, product.ascii() );
        }
        else
        {
            printf( "Crafting finished.\007\n" );
        }
		// ?? bytes - crafting title
    }  /* if crafting_alerts */
}

void exConnection::parseSetGroundTarget(exPacket *p)
{
    groundtarget_x = p->getLong();
    groundtarget_y = p->getLong();
    groundtarget_z = p->getLong();
    ex->Map->dirty();
}

void exConnection::parsePlayerInventoryChange(exPacket *p)
{
    int objcount;
    int i;
    int inv_slot;
    int condition;
    int durability;
    int quality;
    int bonus;
    QString objname;

    objcount = p->getByte();
	// See DOL EMU for more information
    p->seek(3);

    for (i=0; i<objcount; i++)  {
        inv_slot = p->getByte();
		// 1 byte - level
		// 4 bytes - See DOL EMU (item value 1/2, flag, type, damage type),
		// 			 for generic item, cost in copper when you sell it?
		// 1 byte - weight
        p->seek(7);
        condition = p->getByte();
        durability = p->getByte();
        quality = p->getByte();
        bonus = p->getByte();
		// 2 bytes - database id
		// 2 bytes - color
		// 2 bytes - proc
        p->seek(6);
        objname = p->getPascalString();
    }
}

void exConnection::parseObjectEquipment(exPacket *p)
{
    unsigned int infoid;
    int objcount;
    int slot;
    int obj_list, obj_index, obj_color;
//    bool dump_packet;

    exMob *mob;
    exInventoryItem *ii;

    infoid = p->getShort();
    mob = mobinfo.find((void *)infoid);
    if (!mob)
        return;

    mob->setSpeed(0);

    p->seek(2);  // FF FF for mobs?
    objcount = p->getByte();
    slot = 0;
//    dump_packet = FALSE;

//    if (objcount && (mob->getLevel() == 50))
//        cout << mob->getName() << ": ";

	// A good explanation of this packet structure can be found at DOS EMU.
	// I'm not listing it here because its actually quite detailed.  It
	// answers the unknowns about this packet structure.
	
    while (objcount)  {
        slot = p->getByte();
        switch (slot)  {
        case 0x0a:  // right hand
        case 0x0b:  // left hand
        case 0x0c:  // 2h slot
        case 0x0d:  // ranged
        case 0x15:  // helm
        case 0x16:  // gloves
        case 0x17:  // boots
        case 0x19:  // chest
        case 0x1a:  // cloak
        case 0x1b:  // leggings
        case 0x1c:  // sleeves
            obj_list = p->getByte();
            obj_index = p->getByte();
            if (obj_list & 0x40)
                obj_color = p->getByte();
            else  
                obj_color = 0;
            if (obj_list & 0x20)
            {
                p->getShort();  // particle effect?
//                dump_packet = TRUE;
            }
            if (obj_list & 0x80) //
            {
                p->getShort();  // guild emblem?
//                dump_packet = TRUE;
            }
            break;
        default:
            obj_list = 0;
            obj_index = 0;
            obj_color = 0;
        }  /* select slot */

        ii = new exInventoryItem(slot, obj_list, obj_index, obj_color);
        mob->updateInventory(ii);
        //if (mob->getLevel() == 50)
        //    cout << mob->getName() << ": " << ii->getDescription() + "\n";

        /* it appears if the high bit is set, that this is actually
           a particle effect on the preceeding item? */
        if (!(slot & 0x80))
            objcount--;
    }  /* while objcount */

//    if (slot && (mob->getLevel() == 50))
//    {
//        cout << " (" + mob->getClassName() + ")" << endl;
//    }

//    if ((mob->getLevel() == 50) && (dump_packet == TRUE))
//        dumpPacket(0, p);
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
    unsigned int x;
    unsigned int y;
    unsigned int z;
    unsigned int id;
    unsigned int hp;
	unsigned int opp_infoid;

    /* pre 1.62 */
    if (serverprotocol == 0x31) {
        x = p->getLong();
        y = p->getLong();
		opp_infoid=0;
        p->seek(8);
        z = p->getShort();
        id = p->getShort();
        p->seek(2);
        hp = p->getByte();
    }
    else {  // protocol 0x01
        x = p->getShort();
        p->seek(2);  // destinationX
        y = p->getShort();
        p->seek(2);  // destinationY
        z = p->getShort();
        p->seek(2);  // destinationZ
        id = p->getShort();
		opp_infoid = p->getShort();
        hp = p->getByte();
		// MMRF FFFN
		// M - 2 bit pair (0-3) trainer, merchant, killable mob = 0
		// R - running animation on/off and allow mob to fly 
		// F - have to be present for Z-bits to matter and mob to fly
		// N - don't display name
        p->seek(1);
        int zone = p->getByte();
        // p->seek(2);  // destinationZone

        exMapInfo *mi;
        /* I'm guessing that most the time, the mob we'll be
           updating is in our zone, so skip the exhaustive search */
        mi = ex->Map->getMap();
        if (mi && (mi->getZoneNum() != zone))
            /* if not, then do the search */
        	mi = exMapInfo::getZone(playerregion, zone);

        if (mi)  {
            x += mi->getBaseX();
            y += mi->getBaseY();
        }
    }  /* if protocol ver >= 1.62 */

    exMob *mob = mobs.find((void *)id);
    if (mob) {
        mob->setPosition(x, y, z);
        mob->setHead(head);
        mob->setSpeed(speed);
        mob->setHP(hp);
		mob->setOpponentInfoID(opp_infoid);

        ex->Map->dirty();

        if (prefs.sort_when == exPrefs::sortAlways)
            ex->ListViewMobs->sort();
    }  /* if mob */
}

void exConnection::parsePlayerPosUpdate(exPacket *p)
{
    unsigned int id = p->getShort();
    exMob *mob = players.find((void *)id);
    if (mob) {
        unsigned int x, y, z, hp;

        mob->setSpeed(p->getShort());
        z = p->getShort();
        p->seek(2);
        x = p->getLong();
        y = p->getLong();
		mob->setPosition(x, y, z);
		mob->setHead(p->getShort());
		// Flying bits XXXX DSSS SSSS SSSS
		// X - empty
		// D - up/down
		// S - 11 bit speed ... only if groundspeed > 0
        p->seek(2);
		// TUXX XXSG
		// T - torch on/off
		// X - empty
		// S - shadow model
		// G - grid model
        mob->setStealth(p->getByte() & 0x02);  // stealthed but visible
		// High order bit of hp is actually attack off/on
        hp = p->getByte();
        if (hp <= 100)
            mob->setHP(hp);
		// 2 bytes - target visibility???

        ex->Map->dirty();

        if (prefs.sort_when == exPrefs::sortPlayer || prefs.sort_when == exPrefs::sortAlways)
            ex->ListViewMobs->sort();
    }
}

void exConnection::parsePlayerHeadUpdate(exPacket *p)
{
    unsigned int id = p->getShort();
    exMob *mob = players.find((void *)id);

    if (mob) {
        unsigned int hp;

	    mob->setHead(p->getShort());
        p->seek(1);
        mob->setStealth(p->getByte() & 0x02);  // stealthed but visible
        p->seek(2);
        hp = p->getByte();
	    if (hp <= 100)
            mob->setHP(hp);

        ex->Map->dirty();

	    if (prefs.sort_when == exPrefs::sortAlways)
	        ex->ListViewMobs->sort();
    }
}

void exConnection::parseSystemMessage (exPacket *p)
{
	/*	opCodes
		System = 0x00
		Say = 0x01
		Send = 0x02
		Group = 0x03
		Guild = 0x04
		Broadcast = 0x05
		Emote = 0x06
		Help = 0x07
		Friend = 0x08
		Advise = 0x09
		Officer = 0x0a
		Alliance = 0x0b

		Spell = 0x10
		YouHit = 0x11
		YouWereHit = 0x12
		Skill = 0x13
		Merchant = 0x14
		YouDied = 0x15
		PlayerDied = 0x16
		OthersCombat = 0x17
		DamageAdd = 0x18
		SpellExpires = 0x19
		Loot = 0x1a
		SpellResisted = 0x1b
		Important = 0x1c
		Damaged = 0x1d
		Missed = 0x1e
		SpellPulse = 0x1f
	*/

    p->seek(4);
    unsigned int opCode = p->getByte();
    // The following 3 unknown bytes don't seem to provide any consistent
    // method of determining the type of the opCode.
    p->seek(1);
    p->seek(1); //int typeCode = p->getByte();
    p->seek(1);

    // Parse system message for class information
    // available in who and examine.
    if (opCode == 0x00) { 
        QString sysString = p->getZeroString();
        QRegExp rx("\\d+\\).*the Level");

        if (sysString.find(rx) != -1) {
            sysString = sysString.remove(" <AFK>");
            sysString = sysString.remove(" the Level");
		    rx.setPattern(" <.*>");
		    sysString = sysString.remove(rx);
            QString name = sysString.section(' ', 1, 1);
            QString playerclass = sysString.section(' ', 3, 3);
            QString zone = sysString.section(' ', 5);

            if (zone.find(ex->Map->getMap()->getZoneName(),FALSE) != -1) {
                QPtrDictIterator<exMob> playeri(players);

                for(;playeri.current();++playeri) { 
                    exMob *m = playeri.current();
                    if (name.find(m->getName(),FALSE) != -1) {
                        if (!m->setClass(playerclass))
                            cout << "Parse who error!  Failed to set playerclass.  Name: " 
                                 << name << "  Class: " << playerclass << "  Zone: "
                                 << zone << endl;
                        break;
                    }
                }
            }   
        } else {
            rx.setPattern("is a member of");

            if (sysString.find(rx) != -1) {
                QString name = sysString.section(' ',2,2);
                name = name.remove('.');
                QString playerclass = sysString.section(' ',10,10);

                if (name != playername) {  
                    QPtrDictIterator<exMob> playeri(players);
                    bool found=false;

                    for(;playeri.current();++playeri) { 
                        exMob *m = playeri.current();
                        if (name.find(m->getName(),FALSE) != -1) {
                            found = m->setClass(playerclass);
                            break;
                        }
                    }
                    if (!found)
                        cout << "Parse examine error!  Name: " << name << "  Class: "
                             << playerclass << endl;  
                }
            }
        }
    }

    if (opCode == 0x1c) {
        QString xpString = p->getZeroString();
        // For debugging
        cout << xpString << endl;
        QRegExp rx("experience point");

        if (xpString.find(rx) != -1) {
      	    xpString = xpString.section(' ',2,2);
      	    xpString = xpString.remove(',');
      	    unsigned int xp_pnts = xpString.toLong();
            xpStats->RewardXP(xp_pnts);
        }
    }
 
    /*
    exMessage *msg;
    QWidget   *tab;
    QTextEdit *textbox;

    msg = new exMessage(p->getZeroString(), opCode, typeCode);
    msg->parseMsg();

    // Insert it into the ALL tab
    tab = msgui->tabWidget->page(0);
    textbox = (QTextEdit*)tab->childAt( 10, 10);
    textbox->append(QString("[%1] %2").arg(msg->getMsgType()).arg(msg->getFormattedText()));

    for(int x = 1; x < msgui->tabWidget->count(); x++)
        {
        tab = msgui->tabWidget->page(x);
        if(msg->getMsgType() == tab->name())
            {
            textbox = (QTextEdit*)tab->childAt(10, 10);
            textbox->append(msg->getFormattedText());
            break;
            }
        }

    if (msg != NULL)
        delete msg;
    */
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

void exConnection::selectID(unsigned int infoid)
{
    exMob *m;

    selectedid = infoid;

    m = mobinfo.find((void *) infoid);
    if (m) {
	m->touch();
	ex->ListViewMobs->setSelected(m, true);
	ex->ListViewMobs->ensureItemVisible(m);
    }
    ex->Map->dirty();
}

const exMobList <exMob> &exConnection::getMobs() const
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

  this->ex->ListViewMobs->update();
}

void exConnection::cancelMap (void)
{
  m_bMapCancel = true;
}

void exConnection::resetMap (void)
{
  m_bMapCancel = false;
}

bool exConnection::checkMap (void)
{
  return m_bMapCancel;
}

void exConnection::updateProjectedPlayer(void)
{
  float projected_mag;
  float s, c;
  int mag;

  projected_mag = (float)playerspeed * (float)(exTick - player_last_update) *
      (float)(1.0 / 1000.0);

  sincos_quick(playerhead, &s, &c);
  FLOAT_TO_INT((s * projected_mag), mag);
  playerProjectedX = playerx - mag;
  FLOAT_TO_INT((c * projected_mag), mag);
  playerProjectedY = playery + mag;

}

void exConnection::updateObjectTypeCounts(void)
{
    QPtrDictIterator<exMob> mob_iter(mobinfo);
    exMob *mob;
    int cnt_mob = 0;;
    int cnt_players[4];
    int cnt_players_alive[4];

    cnt_players[rFriend] = 0;
    cnt_players[rAlbion] = 0;
    cnt_players[rMidgaard] = 0;
    cnt_players[rHibernia] = 0;

    cnt_players_alive[rFriend] = 0;
    cnt_players_alive[rAlbion] = 0;
    cnt_players_alive[rMidgaard] = 0;
    cnt_players_alive[rHibernia] = 0;

    for (;mob_iter.current(); ++mob_iter)  {
        mob = mob_iter.current();

        if (mob)
            if (!mob->isCurrent())
                continue;
            if (mob->isMob()) {
                cnt_mob++;
            }
            else if (mob->isPlayer())  {
                cnt_players[mob->getRealm()]++;
                if (!mob->isDead())
                    cnt_players_alive[mob->getRealm()]++;
            }
    }  /* for each mob */


    cnt_players[playerrealm] += cnt_players[rFriend];
    cnt_players_alive[playerrealm] += cnt_players_alive[rFriend];

    ex->lblCounts->setText(QString("Albs: %1 (%2)  Mids: %3 (%4)\nHibs: %5 (%6)  Mobs: %7").
                           arg(cnt_players_alive[rAlbion]).arg(cnt_players[rAlbion]).
                           arg(cnt_players_alive[rMidgaard]).arg(cnt_players[rMidgaard]).
                           arg(cnt_players_alive[rHibernia]).arg(cnt_players[rHibernia]).
                           arg(cnt_mob));
}

void exConnection::mobWentStale(exMob *m)
{
    ex->ListViewMobs->takeItem(m);
    updateObjectTypeCounts();
}

void exConnection::parseCharacterInfoList(exPacket *p)
{
    QString name;
    Realm character_realm;
    int zone;
    int characters_left;
    int *intptr;
    Realm *rptr;

    /*
     xxx Mythic said "No account shall ever have more than 4 characters xxx
     xxx because it's too hard to change".  So they make our lives easy xxx
     Mythic lied, now there are 8 slots
     */
    characters_left = 8;

    p->seek(24); // account name
    while (characters_left && !p->isAtEOF()) {
        name = p->getZeroString(48);
	    // 24 bytes - location
        // 24 bytes - class
        // 24 bytes - race
        // 1 byte - level
        // 1 byte - class id
        p->seek(74);
        character_realm = (Realm) p->getByte();
        // 1 byte - gender
        // 1 byte - model
        // 2 bytes - region id
        p->seek(3);
        zone = p->getByte();
        // 4 bytes - unknown
        // 8 bytes - str, con, dex, qui, emp, pie, emp, cha
        // 16 bytes - models (head, gloves, boots, unused, jerkin, cloak, leggings, arm)
        // 16 bytes - colors (see above)
        // 8 bytes - weapon models (right hip, left hip, main back, aux. back)
        // 5 bytes - unknown
        p->seek(57);
        if ((name.length() > 0) && (zone != 0)) {
            intptr = new int;
            *intptr = zone;
            playerregions.replace(name, intptr);
            rptr = new Realm;
            *rptr = character_realm;
            playerrealms.replace(name, rptr);
        }

        characters_left--;
    } // while chars left
}

void exConnection::clearGroundTarget(void)
{
    groundtarget_x = 0;
    groundtarget_y = 0;
    groundtarget_z = 0;
}

void exConnection::parsePlayerPosUpdateFromClient(exPacket *p)
{
    // 2 bytes - player id
    p->seek(2);
    // See exMob::getSpeed() for speed format
    playerspeed = p->getShort() & 0x3ff;
    if (playerspeed & 0x200)
        playerspeed = -(playerspeed & 0x1ff);
	playerz = p->getShort();
    p->seek(2);
	playerx = p->getLong();
	playery = p->getLong();
    // Floor: 1bit (1=player below,0=player above), 
    // Unknown: 3bit, 
    // Heading: 12bit
    playerhead = DAOCHEAD_TO_DEGREES(p->getShort());
    player_last_update = exTick;
	// Flying bits XXXX DSSS SSSS SSSS
	// X - empty
	// D - up/down
	// S - 11 bit speed ... only if groundspeed > 0
	// TUXX XXSG
	// T - torch on/off
	// X - empty
	// S - shadow model
	// G - grid model
    // 1 byte - hp (high order bit is attack on/off)

    exMapInfo *mi = ex->Map->getMap();
    /* If we have a map, see if it is the right map.  If it is
       not, get rid of it */
	if (mi && !mi->right(playerregion, playerx, playery)) {
	    ex->Map->setMap(NULL);
		mi = NULL;
    }
    /* if we don't have a map, load the map */
	if (!mi) {
        mi = exMapInfo::get(playerregion, playerx, playery);
	    if (mi) {
		    ex->Map->setMap(mi);
		    ex->ListViewMobs->triggerUpdate();
		}
    }

    unsigned int x = (mi) ? playerx - mi->getBaseX() : playerx;
    unsigned int y = (mi) ? playery - mi->getBaseY() : playery;
    unsigned int z = playerz;

    ex->xyzstatus->setText(QString("%1, %2, %3").arg(x).arg(y).arg(z));
    ex->Zone->setText((mi) ? mi->getZoneName() : QString("Region %1").arg(playerregion));
	ex->Map->dirty();
	if (prefs.sort_when == exPrefs::sortPlayer || prefs.sort_when == exPrefs::sortAlways)
        ex->ListViewMobs->sort();
    if (link && mi)
        link->send(QString("%1 %2 %3").arg(x).arg(y).arg(z));
}

void exConnection::parsePlayerHeadUpdateFromClient(exPacket *p)
{
    // mob id of client
    p->seek(2);
	playerhead = DAOCHEAD_TO_DEGREES(p->getShort());
    // 1 byte - bit0=wireframe  bit1=seethru  bit7=torch
    // 2 bytes - unknown
    // 1 byte - bit6-0=Health  bit7=Combat stance
    // 1 byte - bit2-0 (position triplet, see exMob::getSpeed()
	ex->Map->dirty();
}

void exConnection::parseChangeTarget(exPacket *p)
{
    unsigned int destid = p->getShort();
    // 2 bytes - LOS check
    if (ex->AutoSelectTarget->isOn()) {
        selectID(destid);
		ex->Map->dirty();
    }
}
