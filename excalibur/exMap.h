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

#ifndef _EXMAP_H
#define _EXMAP_H

#include <qgl.h>
#include <qfile.h>
#include <qobject.h>
#include <qimage.h>
#include <qevent.h>
#include <qprogressdialog.h>
#include "exConnection.h"
#include "exMapInfo.h"

#include <pthread.h>
#include <sched.h>

#if !defined(BSD) && !defined(_WIN32)
# define MUST_DO_SELECT
#endif

#ifdef MUST_DO_SELECT
#include <sys/time.h>
#include <unistd.h>
#endif

#ifndef _WIN32
#include <sys/time.h>
#include <sys/resource.h>
#endif

class exMapElement {
  protected:
    double r,g,b;
    QString text;
    GLint displist;
  public:
    exMapElement();
    virtual ~exMapElement();
    void recache(exMap *map);
    void cached_draw();
    void setColor(QString col);
    virtual bool fromString(QStringList lst, int xadd, int yadd);
    virtual void draw(exMap *map) = 0;
    virtual bool visible(QRect &r) = 0;
};

class exMapElementTexture : public exMapElement {
  protected:
    QRect bounds;
    GLuint texture;
  public:
    exMapElementTexture(int px, int py, int pw, int ph, class exMap *map, QImage img, bool bAlreadyInGLFormat = false);
    ~exMapElementTexture();
    void draw(exMap *map);
    bool visible(QRect &r);
};


class exMapElementPoint : public exMapElement {
  protected:
    int xpos, ypos, zpos;
  public:
    exMapElementPoint();
    bool fromString(QStringList lst, int xadd, int yadd);
    void draw(exMap *map);
    bool visible(QRect &r);
};

class exMapElementLinePoint : public QObject {
Q_OBJECT
  public:
    int x, y, z;
    GLdouble glcol[4];
    GLdouble glpos[3];
    exMapElementLinePoint(int nx, int ny, int nz);
};

class exMapElementLineTess {
  public:
    QPtrList<exMapElementLinePoint> points;
    GLenum type;
    exMapElementLineTess(GLenum gltype);
};

class exMapElementLine : public exMapElement {
  protected:
    QPtrList<exMapElementLinePoint> allpoints;
    QPtrList<exMapElementLinePoint> points;
    QPtrList<exMapElementLineTess> tesspoints;
    QRect bounds;
    bool filled;

    bool tessError;
    exMapElementLineTess *tessTess;
    static class exMapElementLine *tessLine;
    static void ErrorCallback(GLenum errno);
    static void BeginCallback(GLenum type);
    static void VertexCallback(void *vertex_data);
    static void CombineCallback(GLdouble coords[3], void *vertex_data[4], GLfloat weight[4], void **outData);
    void tesselate();
  public:
    exMapElementLine();
    bool fromString(QStringList lst, int xadd, int yadd);     
    void draw(exMap *map);
    bool visible(QRect &r);        
};

class exMapPNGLoaderDialog : public QObject, public QThread {
Q_OBJECT
public:
        exMapPNGLoaderDialog  (void);
        ~exMapPNGLoaderDialog (void);

	virtual void run (void);
        virtual bool event (QEvent *e);
protected:
	friend class exMapLoader;
        QProgressDialog pdProgress;
};

class exMapLoader : public QObject, public QThread {
Q_OBJECT
public:
        exMapLoader  (void);
        ~exMapLoader (void);

	void setParent (exMap *parent);

	virtual void run (void);
        virtual bool event (QEvent *e);

	void abort (void);

	exMapPNGLoaderDialog empldProgress;
protected:
	bool m_bGhettoMutex;
        exMap *parent;
};


class exMap : public QGLWidget {
Q_OBJECT
protected:
  friend class exMapLoader;
  exMapLoader  MapLoader;
  bool         m_bNeedMapReload;

  bool is_dirty;
  bool map_load;
  GLuint listTriangle;
  GLuint listCircle;
  GLuint listSquares;
  exMapInfo *mi;
  unsigned int objsize;
  exTimeType _lastDarken;
  bool mobDarken;
  QPtrList<exMapElement> map;
  int edit_xofs;
  int edit_yofs;
  bool lastfade;
  bool lastfill;
  int lastz;
  bool recache;
  
  unsigned int  fps, frames;
  QTime        *_instant_fps;
  exTimeType    _last_fps;
  
  
public:
  exConnection *c;
  int range;
  bool ignore_fill;
  bool has_direct;
  bool has_NVdriver;
  
  exMap(QWidget *parent, const char *name);
  ~exMap();
  bool isNVidiaModuleLoaded();
  void initializeGL();
  void resizeGL(int w, int h);
  void paintGL();
  void mousePressEvent(QMouseEvent *e);
  void keyPressEvent(QKeyEvent *e);

  void setGLColor(double r, double g, double b, int z);
  void setGLColor(QColor col, int z);
  void setGLColor(double *col, double z);
  void dirty();
  void setConnection(exConnection *nc);
  void objRotate(unsigned int daocheading);
  void setMap(exMapInfo *m);
  void mapRead();
  void loadVectorMap (const exMapInfo *mi);
  void textGL(QString text, int x, int y, int z);
  void setObjectSize(unsigned int uiSize);
  int stringInt(QStringList *sl, unsigned int sec);
  exMapInfo *getMap();
  void makeObjects(bool simple);

  void drawCircle(int center_x, int center_y, int radius, uint8_t segments);

  virtual bool event (QEvent *e);
};

#define CALLBACK_PNG_DATA (QEvent::Type)(QEvent::User + 0x01)
#define CALLBACK_PNG_STAT (QEvent::Type)(QEvent::User + 0x02)
#define CALLBACK_PNG_ABRT (QEvent::Type)(QEvent::User + 0x03)
#define CALLBACK_PNG_FNSH (QEvent::Type)(QEvent::User + 0x04)
#define CALLBACK_PNG_INFO (QEvent::Type)(QEvent::User + 0x05)
#define CALLBACK_VCT_LOAD (QEvent::Type)(QEvent::User + 0x06)

 struct PNGCallback {
	int a;
	int b;
	int c;
	int d;
	int x;
	int y;
	QImage img;
};

#else
class exMapElement; 
class exMapElementPoint;
class exMapElementLine;
class exMapPNGLoaderDialog;
class exMapLoader;
class exMap;
#endif
