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

#ifndef _OEMAP_H
#define _OEMAP_H

#include <qgl.h>
#include <qfile.h>
#include "oeConnection.h"
#include "oeMapInfo.h"

class oeMapElement {
  protected:
    double r,g,b;
    QString text;
    GLint displist;
  public:
    oeMapElement();
    virtual ~oeMapElement();
    void recache(oeMap *map);
    void cached_draw();
    void setColor(QString col);
    virtual bool fromString(QStringList lst, int xadd, int yadd);
    virtual void draw(oeMap *map) = 0;
    virtual bool visible(QRect &r) = 0;
};

class oeMapElementTexture : public oeMapElement {
  protected:
    QRect bounds;
    GLuint texture;
  public:
    oeMapElementTexture(int px, int py, int pw, int ph, QImage img, class oeMap *map);
    ~oeMapElementTexture();
    void draw(oeMap *map);
    bool visible(QRect &r);
};


class oeMapElementPoint : public oeMapElement {
  protected:
    int xpos, ypos, zpos;
  public:
    oeMapElementPoint();
    bool fromString(QStringList lst, int xadd, int yadd);
    void draw(oeMap *map);
    bool visible(QRect &r);
};

class oeMapElementLinePoint : public QObject {
  public:
    int x, y, z;
    GLdouble glcol[4];
    GLdouble glpos[3];
    oeMapElementLinePoint(int nx, int ny, int nz);
};

class oeMapElementLineTess {
  public:
    QPtrList<oeMapElementLinePoint> points;
    GLenum type;
    oeMapElementLineTess(GLenum gltype);
};

class oeMapElementLine : public oeMapElement {
  protected:
    QPtrList<oeMapElementLinePoint> allpoints;
    QPtrList<oeMapElementLinePoint> points;
    QPtrList<oeMapElementLineTess> tesspoints;
    QRect bounds;
    bool filled;

    bool tessError;
    oeMapElementLineTess *tessTess;
    static class oeMapElementLine *tessLine;
    static void ErrorCallback(GLenum errno);
    static void BeginCallback(GLenum type);
    static void VertexCallback(void *vertex_data);
    static void CombineCallback(GLdouble coords[3], void *vertex_data[4], GLfloat weight[4], void **outData);
    void tesselate();
  public:
    oeMapElementLine();
    bool fromString(QStringList lst, int xadd, int yadd);     
    void draw(oeMap *map);
    bool visible(QRect &r);        
};

class oeMap : public QGLWidget {
protected:
  bool is_dirty;
  bool map_load;
  GLuint listTriangle;
  GLuint listCircle;
  oeMapInfo *mi;
  int objsize;
  oeTimeType _lastDarken;
  bool mobDarken;
  QPtrList<oeMapElement> map;
  int edit_xofs;
  int edit_yofs;
  bool lastfade;
  bool lastfill;
  int lastz;
  bool recache;
public:
  oeConnection *c;
  int range;
  bool ignore_fill;
  bool has_direct;
  
  oeMap(QWidget *parent, const char *name);
  ~oeMap();
  void initializeGL();
  void resizeGL(int w, int h);
  void paintGL();
  void mousePressEvent(QMouseEvent *e);
  void keyPressEvent(QKeyEvent *e);

  void setGLColor(double r, double g, double b, int z);
  void setGLColor(QColor col, int z);
  void setGLColor(double *col, double z);
  void dirty();
  void setConnection(oeConnection *nc);
  void objRotate(unsigned int daocheading);
  void setMap(oeMapInfo *m);
  void mapRead();
  void textGL(QString text, int x, int y, int z);
  void setObjectSize(int nsize);
  int stringInt(QStringList *sl, unsigned int sec);
  oeMapInfo *getMap();
  void makeObjects(bool simple);
};


#else
class oeMapElement; 
class oeMapElementPoint;
class oeMapElementLine;
class oeMap;
#endif
