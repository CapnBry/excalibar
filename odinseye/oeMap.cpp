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


#include "oeMap.h"
#include "oeMob.h"
#include "oeLineSimplify.h"
#include <math.h>
#include <GL/glut.h>
#include <GL/glu.h>
#include <qaction.h>
#include <qinputdialog.h>
#include <qregexp.h>
#include <qmessagebox.h>
#include <qtextstream.h>
#include <qslider.h>
#include <qimage.h>

oeMap::oeMap(QWidget *parent, const char *name)
 : QGLWidget(QGLFormat(DoubleBuffer|DepthBuffer|Rgba|DirectRendering),parent,name) {
  objsize = 100; 
  range = 8000;
  c = NULL; 
  is_dirty = true; 
  map_load = false; 
  mobDarken = false;
  _lastDarken = 0;
  mi = NULL;
  edit_xofs = edit_yofs = 0;
  recache = true;
  map.setAutoDelete(true);
}

oeMap::~oeMap() {
  if (mi)
    delete mi;
}

void oeMap::dirty() {
  if (! is_dirty) { 
    qApp->postEvent(this, new QPaintEvent(QRect(0,0,0,0)));
    is_dirty = true;
  }
}

void oeMap::setConnection(oeConnection *nc) {
  c=nc;
}

void oeMap::setMap(oeMapInfo *m) {
  if (mi)
    delete mi;
 
  mi=m;
  map_load=true;
}

void oeMap::setObjectSize(int nsize) {
  objsize=nsize;
  makeObjects(prefs.map_simple);
}

oeMapInfo *oeMap::getMap() {
   return mi;
}

void oeMap::makeObjects(bool simple) {
  int w=objsize;
  int l=w*2;

  glNewList(listTriangle, GL_COMPILE);

  glBegin(GL_TRIANGLES);

  if (simple) {
    glNormal3f(0.0,0.0,1.0);
    glVertex3i(w,-w,0);
    glVertex3i(-w,-w,0);
    glVertex3i(0,l,0);
  } else {
    glNormal3f(l+w,w,l);
    glVertex3i(w,-w,0);
    glVertex3i(0,0,w);
    glVertex3i(0,l,0);

    glNormal3f(-(l+w),w,l);
    glVertex3i(0,0,w);
    glVertex3i(-w,-w,0);
    glVertex3i(0,l,0);

    glNormal3f(0,-w,w);
    glVertex3i(w,-w,0);
    glVertex3i(-w,-w,0);
    glVertex3i(0,0,w);
  }

  glEnd();

  glEndList();

  glNewList(listCircle, GL_COMPILE);
  glBegin(GL_TRIANGLES);
  glNormal3f(0.0,0.0,1.0);

  glVertex3f(w*1.5,-w*1.5,-1);
  glVertex3f(-w*1.5,-w*1.5,-1);
  glVertex3f(0,l*1.5,-1);

  glEnd();
  glEndList();
}


void oeMap::initializeGL() {
  static GLfloat lightpos[4]={0.5,-1.0,1.0,0.0};
  static GLfloat diffuse[4]={0.5,0.5,0.5,1.0};
  static GLfloat ambient[4]={-0.0,-0.0,-0.0,1.0};
  static GLfloat material[4]={0.5,0.5,0.5,1.0};

  if (! format().doubleBuffer())
    qWarning("Single Buffer GL only - Flicker might happen");
  if (! format().depth())
    qWarning("NO GL DEPTH BUFFER - No polygon sorting");
  if (! format().directRendering()) {
    qWarning("No Direct Render - This will be SLOW");
    has_direct = FALSE;
  } else {
    has_direct = TRUE;
  }

  glClearColor(0.0,0.0,0.0,0.0);
  glDisable(GL_CULL_FACE);

  glShadeModel(GL_SMOOTH);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_NORMALIZE);
  
  glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
  glLightfv(GL_LIGHT0, GL_POSITION, lightpos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
  glMaterialfv(GL_FRONT, GL_AMBIENT, material);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, material);

  listTriangle=glGenLists(1);
  listCircle=glGenLists(1);

  makeObjects(prefs.map_simple);
}

void oeMap::resizeGL(int w, int h) {
  glViewport(0,0,w,h);
}

void oeMap::paintGL() {
  const QPtrDict<oeMob> mobs=c->getMobs();
  QPtrDictIterator<oeMob> mobi(mobs);
  oeMapElement *mapel;
  oeMob *m;
  int ldif;
  int l;
  double playerhead;
  double playerrad;
  int minx, maxx, miny,maxy;

  if (map_load) {
    map_load=false;
    if (mi) {
      mapRead();
    }
  }
  
  if (prefs.map_simple) {
    glClear(GL_COLOR_BUFFER_BIT);
    glDisable(GL_BLEND);
  } else {
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    glEnable(GL_BLEND);
  }

  if (prefs.gl_smooth_lines)
    glEnable(GL_LINE_SMOOTH);
  else
    glDisable(GL_LINE_SMOOTH);
  
  if (prefs.gl_smooth_points)
    glEnable(GL_POINT_SMOOTH);
  else
    glDisable(GL_POINT_SMOOTH);

  if (prefs.gl_smooth_polygons)
    glEnable(GL_POLYGON_SMOOTH);
  else
    glDisable(GL_POLYGON_SMOOTH);

  glDisable(GL_LIGHTING);

  playerhead=(c->playerhead * 360.0) / 4096.0;
  playerrad=playerhead * M_PI / 180.0;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if (mi)
    glRotatef(mi->getRotate() * 1.0,0.0,0.0,1.0);
  if (prefs.map_rotate)
    glRotatef(180.0+playerhead, 0.0, 0.0, 1.0);
  glRotatef(180.0,1.0,0.0,0.0);

  minx=c->playerx - range + edit_xofs;
  maxx=c->playerx + range + edit_xofs;
  miny=c->playery - range + edit_yofs;
  maxy=c->playery + range + edit_yofs;
  glOrtho(minx, maxx, miny, maxy,0.0,-25000.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glLineWidth(1.0);
  glPointSize(3.0);

  QRect bounds;
  bounds.setCoords(minx, miny, maxx, maxy);
  bounds=bounds.normalize();

  if ((lastfade != prefs.map_fade) || (lastfill != prefs.map_fill))
    recache = true;
  if (lastfill && (lastz != c->playerz))
    recache = true;

  for (mapel=map.first(); mapel; mapel=map.next()) {
     if (recache)
       mapel->recache(this);
     if (mapel->visible(bounds))
       mapel->cached_draw();
  }

  recache = false;

  if (prefs.map_rulers) {
    qglColor( darkGray );
    glLineWidth ( 1.0 );
    glBegin(GL_LINES);
    glVertex3i(c->playerx - range * 2, c->playery, 500);
    glVertex3i(c->playerx + range * 2, c->playery, 500);
    glVertex3i(c->playerx, c->playery - range * 2, 500);
    glVertex3i(c->playerx, c->playery + range * 2, 500);
    glVertex3f(c->playerx * 1.0, c->playery * 1.0, 500.0);
    glVertex3f(c->playerx + cos(playerrad + M_PI_2) * range * 2, c->playery + sin(playerrad + M_PI_2) * range * 2, 500.0);
    glEnd();
  }

  if (! prefs.map_simple ) {
    glEnable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);
  } else {
    glDisable(GL_DEPTH_TEST);
  }

  qglColor( white );

  glPushMatrix();
  glTranslated(c->playerx,c->playery,c->playerz);
  objRotate(c->playerhead);
  glCallList(listTriangle);
  glPopMatrix();

  if ((oeTick - _lastDarken) > 250) {
    mobDarken = ! mobDarken;
    _lastDarken = oeTick;
  }

  for(;mobi.current();++mobi) {
    m=mobi.current();

    if (m->isCurrent()) {
      glPushMatrix();
      glTranslated(m->getX(),m->getY(),m->getZ());
      objRotate(m->getHead());

      if (! m->isMob()) {
        if (m->isDead()) {
          setGLColor (m->getColor().dark(160), m->getZ());
        } else if (! m->isInvader()) {
          setGLColor (m->getColor(), m->getZ());
        } else {
          setGLColor ( (mobDarken) ? m->getColor().dark(150) : m->getColor().light(150), m->getZ());
        }
        glCallList(listCircle);
      }

  
      ldif=(c->playerlevel / 10) + 1;
      l=m->getLevel();
      if (l < (c->playerlevel - ldif * 3)) 
        setGLColor(0.5,0.5,0.5,m->getZ());
      else if (l <= (c->playerlevel - ldif * 2))
        setGLColor(0.0,1.0,0.0,m->getZ());
      else if (l <= (c->playerlevel - ldif * 1))
        setGLColor(0.0,0.0,1.0,m->getZ());
      else if (l <= c->playerlevel)
        setGLColor(1.0,1.0,0.0,m->getZ());
      else if (l <= (c->playerlevel + ldif * 1))
        setGLColor(1.0,0.5,0.0,m->getZ());
      else if (l <= (c->playerlevel + ldif * 2))
        setGLColor(1.0,0.0,0.0,m->getZ());
      else 
        setGLColor(1.0,0.0,1.0,m->getZ());

      glCallList(listTriangle);
      glPopMatrix();
    }
    m->stale();
  }

  m=mobs.find((void *)c->selectedid);
  if (m && m->isCurrent()) {
    qglColor( white );
    glLineWidth ( 2.0 );
    glBegin(GL_LINES);
    glVertex3i(c->playerx,c->playery,c->playerz);
    glVertex3i(m->getX(),m->getY(),m->getZ());
    glEnd();
  }

  is_dirty = false;

  glFlush();
}

void oeMap::mousePressEvent(QMouseEvent *e) {
  oeMob *m;
  unsigned int bestid = 0;
  double bestdist=0.0;
  double xdif;
  double ydif;
  GLdouble cx;
  GLdouble cy;
  GLdouble cz;
  double thisdist;
  GLdouble projmatrix[16];
  GLdouble modmatrix[16];
  GLint viewport[4];

  const QPtrDict<oeMob> mobs=c->getMobs();
  QPtrDictIterator<oeMob> mobi(mobs);

  makeCurrent();

  glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);
  glGetDoublev(GL_MODELVIEW_MATRIX, modmatrix);
  glGetIntegerv(GL_VIEWPORT, viewport);
  
  gluUnProject(e->x(), size().height()-e->y(), 100, modmatrix, projmatrix, viewport, &cx, &cy, &cz);

  for(; mobi.current(); ++mobi) {
    m=mobi.current();
    if (m->isCurrent()) {
      xdif=m->getX()-cx;
      ydif=m->getY()-cy;
      thisdist=sqrt(xdif*xdif+ydif*ydif);
      if (! bestid || (thisdist < bestdist)) {
        bestid=m->getInfoID();
        bestdist=thisdist;
      }
    }
  }

  if (bestid) { 
    c->selectID(bestid);
  }
}

void oeMap::keyPressEvent(QKeyEvent *e) {
  double angle=0.0;
  double dist=c->oe->MapSlider->value();

  switch( e->key() ) {
    case Qt::Key_Right:
      angle=0.0;
      break;
    case Qt::Key_Up:
      angle=1.0;
      break;
    case Qt::Key_Left:
      angle=2.0;
      break;
    case Qt::Key_Down:
      angle=3.0;
      break;
    case Qt::Key_Home:
      edit_xofs = 0;
      edit_yofs = 0;
      dirty();
      return;
      break;
    case Qt::Key_Next:
      c->oe->MapSlider->setValue((int)(dist * 1.5));
      return;
      break;
    case Qt::Key_Prior:
      c->oe->MapSlider->setValue((int)(dist / 1.5));
      return;
      break;
    default:
      e->ignore();
      return;
      break;
  }
  if (mi)
    angle = (mi->getRotate() / 90.0 ) + angle;
  angle = angle * M_PI / 2.0;
  edit_xofs += (int)(cos(angle)*dist / 2.0);
  edit_yofs -= (int)(sin(angle)*dist / 2.0);
  dirty();
}

void oeMap::setGLColor(double r, double g, double b, int z) {
  double col[4];
  
  col[0]=r;
  col[1]=g;
  col[2]=b;
  setGLColor(col, (double) z);
  glColor4d(col[0],col[1],col[2],col[3]);
}

void oeMap::setGLColor(double *col, double z) {
  double darken;

  if (! prefs.map_fade) {
     col[3]=1.0;
     return;
  }
  
  darken=fabs(c->playerz - z) / 500.0;
  if (darken < 0.60) {
    darken=1.0 - darken;
  } else {
    darken=0.40;
  }

  if (prefs.map_simple) {
    col[3]=1.0;
    col[0]=col[0] * darken;
    col[1]=col[1] * darken;
    col[2]=col[2] * darken;
  } else {
    col[3]=darken;
  }
}

void oeMap::setGLColor(QColor col, int z) {
  setGLColor(col.red() / 255.0, col.green() / 255.0, col.blue() / 255.0, z);
}

void oeMap::objRotate(unsigned int daocheading) {
  double r=daocheading;
  r*=360;
  r/=0x1000;
  glRotatef(r,0.0,0.0,1.0);
}

int oeMap::stringInt(QStringList *sl, unsigned int sec) {
  bool ok;
  if ((sl==NULL) || (sec >= sl->size())) {
    return 0;
  }
  int v=(*sl)[sec].toInt(&ok, 10);
  if (!ok) {
    return 0;
  }
  return v;
}


void oeMap::mapRead() {
  QString line;
  QString cmd;
  QString color;
  QString title;
  oeMapElement *elem;
  int xadd;
  int yadd;
  int x;
  int y;
  unsigned int i;
  int w,h;

  map.clear();
  recache = true;

  xadd=mi->getBaseX();
  yadd=mi->getBaseY();

  ignore_fill = false;

  QFile fimg(QString("maps/zone%1.png").arg(mi->getZoneNum(),3,10));
  if (fimg.exists()) {
    QImage img;
    if (img.load(fimg.name())) {
      w=img.width();
      h=img.height();
      ignore_fill = true;
      for(y=0;y<8;y++) {
        for(x=0;x<8;x++) {
          map.append(new oeMapElementTexture(x * 8192 + xadd, y * 8192 + yadd, 8192, 8192, img.copy(w * x / 8, h * y / 8, w / 8, h / 8), this));
        }
      }
    }
  }

  QFile f;
  f.setName(QString("usermaps/").append(mi->getName()));
  if (! f.open(IO_ReadOnly)) {
    f.setName(QString("maps/").append(mi->getName()));
    if (! f.open(IO_ReadOnly)) {
      qWarning("Failed to open map named %s",(const char *) mi->getName());
      return;
    }
  }

  f.readLine(line, 0x10000);


  while (f.readLine(line, 0x10000)!=-1) {
    bool ok;
    QStringList split=QStringList::split(",",line,TRUE);
    QStringList lst;
    for(i=0;i<split.size();i++) 
      lst+=split[i].simplifyWhiteSpace().stripWhiteSpace();

    if (lst.size() < 3) 
      continue;

    ok = false;
    elem = NULL;

    cmd=lst[0];
    if (cmd.length() == 1) {
      switch (cmd[0].latin1()) {
        case 'M':
        case 'F':
          elem=new oeMapElementLine();
          break;
        case 'P':
          elem=new oeMapElementPoint();
          break;
      }
      if (elem && elem->fromString(lst, xadd, yadd)) {
         ok = true;
         map.append(elem);
      }
    }
    if (! ok) {
      qWarning("Mapelement %s was not accepted", (const char *)line);
      if (elem)
        delete elem;
    }
  }
}

oeMapElement::oeMapElement() {
  displist = glGenLists(1);
}

oeMapElement::~oeMapElement() {
  glDeleteLists(displist, 1);
}

void oeMapElement::recache(oeMap *map) {
    glNewList(displist, GL_COMPILE);
    draw(map);
    glEndList();
    glCallList(displist);
}


void oeMapElement::cached_draw() {
  glCallList(displist);
}

void oeMapElement::setColor(QString col) {
  QColor color=QColor(col);
  r=color.red() / 255.0;
  g=color.green() / 255.0;
  b=color.blue() / 255.0;
}

bool oeMapElement::fromString(QStringList lst, int, int) {
  if (lst.size() < 3)
    return FALSE;
  text=lst[1];
  setColor(lst[2]);

  if ((r < 0.1) && (g < 0.1) && (b < 0.1))
    return FALSE;
  if (text.length() < 1)
    return FALSE;

  return TRUE;
}

oeMapElementTexture::oeMapElementTexture(int px, int py, int pw, int ph, QImage img, oeMap *map) {
  bounds.setRect(px, py, pw+1, ph+1);
  QImage tex=QGLWidget::convertToGLFormat(img);
  glGenTextures(1, &texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  if (map->has_direct) {
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB16, tex.width(), tex.height(), GL_RGBA, GL_UNSIGNED_BYTE, tex.bits());
  } else {
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, 3, tex.width(), tex.height(), 0, GL_RGBA, GL_UNSIGNED_BYTE, tex.bits());
  }
}

oeMapElementTexture::~oeMapElementTexture() {
  glDeleteTextures(1, &texture);
}

void oeMapElementTexture::draw(oeMap *) {
  glPushAttrib(GL_ENABLE_BIT);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glEdgeFlag(GL_FALSE);

  glColor4f(1.0,1.0,1.0,1.0);

  glBindTexture(GL_TEXTURE_2D, texture);
  glBegin(GL_QUADS);

  glTexCoord2d(0.0, 1.0);
  glVertex3i(bounds.left(), bounds.top(), 250);

  glTexCoord2d(1.0, 1.0);
  glVertex3i(bounds.right(), bounds.top(), 250);

  glTexCoord2d(1.0, 0.0);
  glVertex3i(bounds.right(), bounds.bottom(), 250);

  glTexCoord2d(0.0, 0.0);
  glVertex3i(bounds.left(), bounds.bottom(), 250);
  glEnd();

  glPopAttrib();
}

bool oeMapElementTexture::visible(QRect &r) {
  return r.intersects(bounds);
}

oeMapElementPoint::oeMapElementPoint() {
  xpos=ypos=zpos=0;
}

void oeMapElementPoint::draw(oeMap *map) {
  unsigned int i;

  map->setGLColor(r, g, b, zpos);

  glBegin(GL_POINTS);
  glVertex3i(xpos,ypos,zpos);
  glEnd();
  map->setGLColor(1.0,1.0,1.0,zpos);

  glRasterPos3i(xpos+50,ypos+20,zpos);
  for (i=0;i<text.length();i++) 
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10, text[i].latin1());
}

bool oeMapElementPoint::visible(QRect &r) {
  return r.contains(xpos, ypos);
}

bool oeMapElementPoint::fromString(QStringList lst, int xadd, int yadd) {
  bool ok;

  if (! oeMapElement::fromString(lst, xadd, yadd))
    return FALSE;

  if (lst.size() != 6)
    return FALSE;

  xpos=abs(lst[3].toInt(&ok, 10))+xadd;
  if (! ok)
    return FALSE;
  ypos=abs(lst[4].toInt(&ok, 10))+yadd;
  if (! ok)
    return FALSE;
  zpos=abs(lst[5].toInt(&ok, 10));
  if (! ok)
    return FALSE;
  return TRUE;
}

oeMapElementLinePoint::oeMapElementLinePoint(int nx, int ny, int nz) {
  x=nx;
  y=ny;
  z=nz;
}

oeMapElementLineTess::oeMapElementLineTess(GLenum gltype) {
  points.setAutoDelete(FALSE);
  type=gltype;
}

oeMapElementLine::oeMapElementLine() {
  points.setAutoDelete(FALSE);
  tesspoints.setAutoDelete(FALSE);
  allpoints.setAutoDelete(FALSE);
  filled=FALSE;
}

bool oeMapElementLine::visible(QRect &r) {
  return r.intersects(bounds);
}

bool oeMapElementLine::fromString(QStringList lst, int xadd, int yadd) {
  bool ok;
  int num;
  int i;
  int x, y, z;
  oeLineSimplify s;
  oeLineSimplifyPoint *ps;
  oeMapElementLinePoint *p;

  if (! oeMapElement::fromString(lst, xadd, yadd))
    return FALSE;

  if (lst.size() < 4)
    return FALSE;

  if (lst[0][0].latin1()=='F')
    filled = TRUE;
  else
    filled = FALSE;

  num=lst[3].toInt(&ok, 10);
  if (!ok || (num < 2))
    return FALSE;
  if (lst.size() != (4 + (unsigned int)num * 3))
    return FALSE;

  for(i=0;i<num;i++) {
    x=abs(lst[4 + i * 3].toInt(&ok, 10))+xadd;
    if (!ok)
      return FALSE;
    y=abs(lst[5 + i * 3].toInt(&ok, 10))+yadd;
    if (!ok)
      return FALSE;
    z=abs(lst[6 + i * 3].toInt(&ok, 10));
    if (!ok)
      return FALSE;
    s.add(x, y, z);
  }
  s.DouglasPeucker(prefs.map_autosimplifyrange);
  points.clear();
  for(ps=s.points.first(); ps; ps=s.points.next()) {
    x = (int) ps->x;
    y = (int) ps->y;
    z = (int) ps->z;
    p=new oeMapElementLinePoint(x, y, z);
    allpoints.append(p);
    points.append(p);
    if (bounds.x() == 0) {
      bounds.setRect(x, y, 1, 1);
    } else {
      if (bounds.left() > x)
        bounds.setLeft(x);
      if (bounds.right() < x)
        bounds.setRight(x);
      if (bounds.top() < y)
        bounds.setTop(y);
      if (bounds.bottom() > y)
        bounds.setBottom(y); 
     }
  }

  if (! filled)
    return TRUE;

  tesselate();
  return TRUE;
}

oeMapElementLine *oeMapElementLine::tessLine;

void oeMapElementLine::BeginCallback(GLenum type) {
  tessLine->tessTess=new oeMapElementLineTess(type);
  tessLine->tesspoints.append(tessLine->tessTess);
}

void oeMapElementLine::ErrorCallback(GLenum err) {
  qWarning("Tesselation failed with error %s",gluErrorString(err));
  tessLine->tessError = TRUE;
}

void oeMapElementLine::VertexCallback(void *vertex_data) {
  oeMapElementLinePoint *p=(oeMapElementLinePoint *)vertex_data;
  tessLine->tessTess->points.append(p);
}

void oeMapElementLine::CombineCallback(GLdouble coords[3], void **, GLfloat *, void **outData) {
  oeMapElementLinePoint *p=new oeMapElementLinePoint((int) coords[0], (int) coords[1], (int) coords[2]);
  tessLine->allpoints.append(p);
  *outData = (void *) p;
}

#ifdef GLU_VERSION_1_2
#define GLU_12
#else
#define GLU_11
#endif

void oeMapElementLine::tesselate() {
  oeMapElementLinePoint *p;
#ifdef GLU_11
  GLUtriangulatorObj *tess;
#else
  GLUtesselator *tess;
#endif

  tess=gluNewTess();
  tessError = FALSE;
  tessTess = NULL;
  tessLine = this;

  gluTessCallback(tess, (GLenum) GLU_TESS_BEGIN, (void (*)()) oeMapElementLine::BeginCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_VERTEX, (void (*)()) oeMapElementLine::VertexCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_ERROR, (void (*)()) oeMapElementLine::ErrorCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_COMBINE, (void (*)()) oeMapElementLine::CombineCallback);

#ifdef GLU_11
  gluBeginPolygon(tess);
#else
  gluTessBeginPolygon(tess, NULL);
  gluTessBeginContour(tess);
#endif
  for (p=points.first(); p; p=points.next()) { 
    p->glpos[0]=p->x;
    p->glpos[1]=p->y;
    p->glpos[2]=p->z;
    gluTessVertex(tess, p->glpos, p);
  }
#ifdef GLU_11
  gluEndPolygon(tess);
#else
  gluTessEndContour(tess);
  gluTessEndPolygon(tess);
#endif
  gluDeleteTess(tess);

  if (tessError) {
    filled=false;
  }
}

void oeMapElementLine::draw(oeMap *map) {
  oeMapElementLinePoint *p;


  for(p=allpoints.first(); p; p=allpoints.next()) {
    p->glcol[0]=r;
    p->glcol[1]=g;
    p->glcol[2]=b; 
    p->glcol[3]=1.0;
    map->setGLColor(p->glcol, (double) p->z);
  }

  if (! map->ignore_fill && filled && prefs.map_fill) {
    oeMapElementLineTess *t;
    for (t=tesspoints.first(); t; t=tesspoints.next()) {
      glBegin(t->type);
      for (p=t->points.first(); p; p=t->points.next()) {
        glColor4f(p->glcol[0] / 2, p->glcol[1] / 2, p->glcol[2] / 2, p->glcol[3]);
        glVertex3i(p->x, p->y, p->z / 10);
      }
      glEnd();
    }
  }

  glBegin(GL_LINE_STRIP);
  for (p=points.first(); p; p=points.next()) { 
    glColor4f(p->glcol[0],p->glcol[1],p->glcol[2],p->glcol[3]);
    glVertex3i(p->x,p->y,p->z);
  }
  glEnd();  
}
