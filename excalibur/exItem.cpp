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

#include "exItem.h"

#include <qfile.h>
#include <qdatastream.h>
#include <qmessagebox.h>
#include <qurloperator.h>
#include <zlib.h>

QDict<exItem> items(211);

exItem::exItem(QString n, QByteArray d) {
  if ((d.size() != 16) && (d.size() != 18))
    qFatal("Initializing exItem with wrong sized data array");
  name=n;
  data=d;
  data.detach();
  info="";
}

int exItem::quality() {
  return data[10];
}


void exItem::seen(QString n, QByteArray d) {
  exItem *newitem, *item;
  newitem=new exItem(n,d);

  item=items.find(n);

  if (item) {
    if (item->quality() > newitem->quality()) {
      items.remove(n);
    } else {
      delete newitem;
      return;
    }
  }
  items.insert(n, newitem);
}

void exItem::seen(QString n, QString i) {
  exItem *item;

  item=items.find(n);
  if (item) 
    item->info=i;
  return;
}

void exItem::init() {
  items.setAutoDelete(TRUE);
  load();
}

#define EX_ITEM_FILEVERSION 2

void exItem::load() {
  int version;
  QString name;
  QByteArray data;
  QString info;

  QFile f("items.db");
 
  if (! f.open(IO_ReadOnly)) {
    qWarning("Could not load item database -- Starting fresh");
    return;
  }

  QDataStream ds(&f);

  ds >> version;

  if (version == 1) {
    qWarning("Item database with serious bugs -- worthless -- Starting fresh");
    return;
  } else if (version > EX_ITEM_FILEVERSION) {
    qWarning("Item database has a structure I can't handle");
    return;
  }

  while (! ds.atEnd()) {
     ds >> name >> data >> info;
     seen(name, data);
     seen(name, info);
  }     
}

void exItem::save(QDataStream *ds) {
  QDictIterator<exItem> ii(items);
  exItem *item;

  for (; ii.current(); ++ii) {
    item=ii.current();
    *ds << item->name << item->data << item->info;
  }
}


void exItem::save() {
  QFile f("items.db");

  if (! f.open(IO_WriteOnly)) {
    qWarning("Could not open item database for writing");
    return;
  }

  QDataStream ds(&f);

  ds << EX_ITEM_FILEVERSION;

  save(&ds);
}

void exItem::upload(bool fromgui) {
  QByteArray qba;
  QDataStream ds(qba, IO_WriteOnly);

  if (! fromgui && ! prefs.items_autoupload) {
    return;
  }

  if (prefs.items_contributor.length() < 1) {
    QMessageBox::warning(NULL, "Missing Name","You need to fill out a contributor name\nin the preferences. Go to Settings|Preferences...\nClick on the \"Items\" tab and fill in the\nname you want to use",QMessageBox::Ok,0,0);
    return;
  }

  ds << EX_ITEM_FILEVERSION;
  ds << prefs.items_contributor;
  save(&ds);

  uLongf complen=(int)(qba.size() * 1.1 + 12);
  QByteArray compa(complen);

  compress2((Bytef *) compa.data(), &complen,(Bytef *) qba.data(),qba.size(),Z_BEST_COMPRESSION);
  compa.resize(complen);

  qWarning("Uploading %d items (%d bytes)",items.count(),compa.size());
  new exItemUpload(compa);
}

exItemUpload::exItemUpload(QByteArray d) {
  data=d;
  op=new QUrlOperator("http://www.hackersquest.org/daoc/upload.cgi");
  connect(op, SIGNAL(finished(QNetworkOperation *)), this, SLOT(finished(QNetworkOperation *)));
  op->put(data);
}

exItemUpload::~exItemUpload() {
  delete op;
}

void exItemUpload::finished(QNetworkOperation *nop) {
  if (nop->state() != QNetworkProtocol::StDone) {
    qWarning("Upload of items failed: %s",(const char *) nop->protocolDetail());
    return;
  }
  qWarning("Items uploaded successfully");
  deleteLater();
}
