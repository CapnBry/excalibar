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

#include "oeItem.h"

#include <qfile.h>
#include <qdatastream.h>
#include <qmessagebox.h>
#include <qurloperator.h>
#include <zlib.h>

QDict<oeItem> items(211);

oeItem::oeItem(QString n, QByteArray d) {
  if ((d.size() != 16) && (d.size() != 18))
    qFatal("Initializing oeItem with wrong sized data array");
  name=n;
  data=d;
  data.detach();
  info="";
}

int oeItem::quality() {
  return data[10];
}


void oeItem::seen(QString n, QByteArray d) {
  oeItem *newitem, *item;
  newitem=new oeItem(n,d);

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

void oeItem::seen(QString n, QString i) {
  oeItem *item;

  item=items.find(n);
  if (item) 
    item->info=i;
  return;
}

void oeItem::init() {
  items.setAutoDelete(TRUE);
  load();
}

#define OE_ITEM_FILEVERSION 2

void oeItem::load() {
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
  } else if (version > OE_ITEM_FILEVERSION) {
    qWarning("Item database has a structure I can't handle");
    return;
  }

  while (! ds.atEnd()) {
     ds >> name >> data >> info;
     seen(name, data);
     seen(name, info);
  }     
}

void oeItem::save(QDataStream *ds) {
  QDictIterator<oeItem> ii(items);
  oeItem *item;

  for (; ii.current(); ++ii) {
    item=ii.current();
    *ds << item->name << item->data << item->info;
  }
}


void oeItem::save() {
  QFile f("items.db");

  if (! f.open(IO_WriteOnly)) {
    qWarning("Could not open item database for writing");
    return;
  }

  QDataStream ds(&f);

  ds << OE_ITEM_FILEVERSION;

  save(&ds);
}

void oeItem::upload(bool fromgui) {
  QByteArray qba;
  QDataStream ds(qba, IO_WriteOnly);

  if (! fromgui && ! prefs.items_autoupload) {
    return;
  }

  if (prefs.items_contributor.length() < 1) {
    QMessageBox::warning(NULL, "Missing Name","You need to fill out a contributor name\nin the preferences. Go to Settings|Preferences...\nClick on the \"Items\" tab and fill in the\nname you want to use",QMessageBox::Ok,0,0);
    return;
  }

  ds << OE_ITEM_FILEVERSION;
  ds << prefs.items_contributor;
  save(&ds);

  uLongf complen=(int)(qba.size() * 1.1 + 12);
  QByteArray compa(complen);

  compress2((Bytef *) compa.data(), &complen,(Bytef *) qba.data(),qba.size(),Z_BEST_COMPRESSION);
  compa.resize(complen);

  qWarning("Uploading %d items (%d bytes)",items.count(),compa.size());
  new oeItemUpload(compa);
}

oeItemUpload::oeItemUpload(QByteArray d) {
  data=d;
  op=new QUrlOperator("http://www.hackersquest.org/daoc/upload.cgi");
  connect(op, SIGNAL(finished(QNetworkOperation *)), this, SLOT(finished(QNetworkOperation *)));
  op->put(data);
}

oeItemUpload::~oeItemUpload() {
  delete op;
}

void oeItemUpload::finished(QNetworkOperation *nop) {
  if (nop->state() != QNetworkProtocol::StDone) {
    qWarning("Upload of items failed: %s",(const char *) nop->protocolDetail());
    return;
  }
  qWarning("Items uploaded successfully");
  deleteLater();
}
