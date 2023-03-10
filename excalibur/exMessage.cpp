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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <qstring.h>
#include <qregexp.h>
#include "exMessage.h"
#include "exPrefs.h"

exMessage::exMessage(QString newMsg, uint8_t opCode, uint8_t typeCode)
{ 
	this->Msg = newMsg;
	this->MsgType = "Unknown";
	this->FormattedText = this->Msg;
	this->opCode = opCode;
	this->typeCode = typeCode;

}

exMessage::~exMessage()
{
}

void exMessage::parseMsg()
{
	/*
	QRegExp rxGuild( "\\[Guild\\] [A-Za-z]+\\:\\ ");
	QRegExp rxGroup( ".*\\[Party\\].*");
	QRegExp rxChat( ".*\\[Chat\\].*");
	QRegExp rxTell( ".*send[s]?\\,\\ \\\".*");
	QRegExp rxBCast( ".*\\*\\*.*\\*\\*$");
	QRegExp rxSay( ".*say[s]?\\,\\ \\\".*");
	*/
	QRegExp rxPML( "^The\\ .*\\ drops[\\ ]+(a\\ )?[A-Z]+.*");
	int p;
	
	switch( this->opCode)
		{
		case 0x00: /* Mostly Random Msgs */
			break;
		case 0x01: /* Say */
        	this->MsgType = "Say";
        	this->Recvr   = "Say";
        	p = this->Msg.find( " ");
        	this->Sender  = this->Msg.mid( 2, (p-2));
        	this->MsgText = this->Msg.mid( 
            	        this->Msg.find( "\""), 
                	    this->Msg.findRev( "\"") - 
                    	      this->Msg.find( "\"") + 1);

        	this->FormattedText = this->Sender;
        	this->FormattedText += 
				this->Sender == "You" ? " say " : " says ";
        	this->FormattedText += this->MsgText;
			break;
		case 0x02: /* Tell */
        	this->MsgType = "Tell";
        	p = this->Msg.find( " ");
        	this->Sender  = this->Msg.mid( 2, (p-2));
        	this->Recvr   = this->Msg.mid( 
            	        this->Msg.findRev( " ")+1, this->Msg.length());
        	this->MsgText = this->Msg.mid( 
           	         this->Msg.find( "\""), 
           	         this->Msg.findRev( "\"") - 
            	              this->Msg.find( "\"") + 1);
        
        	if( "You" != this->Sender) this->Recvr = "You";
        	this->FormattedText = this->Sender;
        	this->FormattedText += 
					this->Sender == "You" ? " tell " : " tells ";
	        this->FormattedText += this->Recvr + " " + this->MsgText;
			break;
		case 0x03: /* Party */
			this->MsgType = "Party";
			this->Recvr   = "Party";
			p = this->Msg.find( ":");
			this->Sender  = this->Msg.mid( 10, (p - 10));
			this->MsgText = this->Msg.mid( p + 2, this->Msg.length());
			this->FormattedText = this->Sender + ": " + this->MsgText;
			break;
		case 0x04: /* Guild */
			this->MsgType = "Guild";
			this->Recvr   = "Guild";
			p = this->Msg.find( ":");
			this->Sender  = this->Msg.mid( 10, (p - 10));
			this->MsgText = this->Msg.mid( p + 2, this->Msg.length());
			this->FormattedText = this->Sender + ": " + this->MsgText;
			break;
		case 0x05: /* Broadcast */
        	this->MsgType = "Broadcast";
			this->Recvr   = "Zone";
        	p = this->Msg.find( ":");
        	this->Sender  = this->Msg.mid( 2, (p-2));
        	this->MsgText = this->Msg.mid( 
            	        this->Msg.find( "**"), this->Msg.length());

			this->FormattedText = this->Sender + " broadcasts: ";
	        this->FormattedText += this->MsgText;
			break;
		case 0x06: /* Dunno yet */
			break;
		case 0x07: /* dunno */
			break;	
		case 0x08: /* Chat Group */
        	this->MsgType = "Chat";
        	this->Recvr   = "Chat";
        	p = this->Msg.find( ":");
        	this->Sender  = this->Msg.mid( 9, (p - 9));
        	this->MsgText = this->Msg.mid( p + 2, this->Msg.length());
        	this->FormattedText = this->Sender + ": " + this->MsgText;
			break;
		case 0x1a:
		    if( -1 != rxPML.search( Msg))
				{
				this->MsgType = "PML";
				this->Sender = "None";
				this->MsgText = this->Msg;
				this->FormattedText = this->MsgText;
				}
			break;
		default:
			break;
	
		}

	if( this->MsgText)
		printf("[%s] %s\n", 
				this->MsgType.ascii(), 
				this->FormattedText.ascii());
	else
		printf("[%s : 0x%02x : 0x%02x] %s\n", "Unknown", this->opCode, this->typeCode, this->Msg.ascii());

}
