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


#include "oeUpdate.h"
#include "odinseye.h"

#include <qnetwork.h>
#include <qfile.h>
#include <qstring.h>
#include <zlib.h>

oeUpdate::oeUpdate(bool doupdate) : QObject() {
  uop=new QUrlOperator("http://www.hackersquest.org/daoc/VERSION_2");

  connect(uop, SIGNAL( data( const QByteArray &, QNetworkOperation *)), this, SLOT ( data( const QByteArray &, QNetworkOperation *)));
  connect(uop, SIGNAL( finished( QNetworkOperation *)), this, SLOT ( version_finished( QNetworkOperation *)));

  autoupdate=doupdate;
}

void oeUpdate::fetch() {
  uop->get();
}

oeUpdate::~oeUpdate() {
  delete uop;
}

void oeUpdate::data(const QByteArray & data, QNetworkOperation * nop) {
  unsigned int i;
  unsigned int dofs;
  QNetworkOperation *IKnowImNotUsingIt;

  IKnowImNotUsingIt = nop;

  dofs=d.size();

  d.resize(dofs + data.size());

  for(i=0;i<data.size();i++)
    d[i+dofs]=data[i];
}

void oeUpdate::version_finished(QNetworkOperation * nop) {
  QDataStream ds(d, IO_ReadOnly);
  int version;
  bool updates;

  if (nop->state() != QNetworkProtocol::StDone) {
    qWarning("Version check failed: %s",(const char *) nop->protocolDetail());
    return;
  }

  delete uop;

  ds >> version;

  if (version != OE_CURRENT_VERSION) {
    qWarning("============\nYou're using build %d, but the latest build is %d\n============",OE_CURRENT_VERSION, version);
  } else {
    qWarning("Your build of OE is current (build %d)",OE_CURRENT_VERSION);
  }

  updates = false;

  while (! ds.atEnd()) {
    QString fname;
    unsigned int csum;
    bool update;

    ds >> fname >> csum;

    update = false;

    QFile sumfile(fname);
    if (! sumfile.open(IO_ReadOnly)) {
      qWarning("The file %s is missing",(const char *)fname);
      update = true;      
    } else {
      QByteArray qba(sumfile.readAll());
      if (qChecksum(qba.data(),qba.size()) != csum) {
        qWarning("The file %s has been updated",(const char *)fname);
        update = true;
      }
    }
    if (update) {
      updates = true;
      if (autoupdate) {
        QString url;
        QString furl;
        url="http://www.hackersquest.org/daoc/";
        furl="file:";
 
        url.append(fname);
        furl.append(fname);

	new oeHttpCopyFile(fname);
      }
    }
  }
  if (updates && ! autoupdate) {
    qWarning("New or changed files found, consider running odinseye with the --update option");
  }
}

void oeUpdate::copy_finished(QNetworkOperation * nop) {
  if (nop->state() != QNetworkProtocol::StDone) {
    qWarning("Update of %s failed: %s",(const char *) nop->arg(0), (const char *) nop->protocolDetail());
    return;
  }
  if (nop->operation() == QNetworkProtocol::OpPut) {
    qWarning("Successfully updated %s",(const char *) nop->arg(0));
  }
}

oeHttpCopyFile::oeHttpCopyFile(QString filename) {
  QString url;
  fname=filename;

  url="http://www.hackersquest.org/daoc/";
  url.append(filename);
  url.append(".z");

  uop=new QUrlOperator(url);

  connect(uop, SIGNAL( data( const QByteArray &, QNetworkOperation *)), this, SLOT ( data( const QByteArray &, QNetworkOperation *)));
  connect(uop, SIGNAL( finished( QNetworkOperation *)), this, SLOT ( finished( QNetworkOperation *)));

  uop->get();
}

oeHttpCopyFile::~oeHttpCopyFile() {
  delete uop;
}

void oeHttpCopyFile::data(const QByteArray &data, QNetworkOperation *nop) {
  unsigned int i;
  unsigned int dofs;
  QNetworkOperation *IKnowImNotUsingIt;

  IKnowImNotUsingIt = nop;

  dofs=d.size();

  d.resize(dofs + data.size());

  for(i=0;i<data.size();i++)
    d[i+dofs]=data[i];
}

void oeHttpCopyFile::finished(QNetworkOperation *nop) {
  QFile f(fname);

  if (nop->state() != QNetworkProtocol::StDone) {
    qWarning("Download of file %s failed: %s",(const char *) fname, (const char *) nop->protocolDetail());
    deleteLater();
    return;
  }

  if (! f.open(IO_WriteOnly)) {
    qWarning("Failed to open %s for writing",(const char *) fname);
    deleteLater();
    return;
  }

  uLongf bufsize=d.size();
  QByteArray qba;
  int zres;

  do {
    bufsize=bufsize * 2;
    qba.resize(bufsize);
    uLongf dlen=bufsize;
    zres=uncompress((Bytef *) qba.data(), &dlen, (Bytef *) d.data(), d.size());
    if (zres == Z_OK)
      qba.resize(dlen);
  } while (zres == Z_BUF_ERROR);

  if (zres != Z_OK) {
    qWarning("Failed decompression of %s",(const char *) fname);
    deleteLater();
    return;
  }

  f.writeBlock(qba);
  f.close();
  qWarning("Successfully downloaded %s",(const char *) fname);
  deleteLater();
}
