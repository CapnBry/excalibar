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


#include "exMap.h"
#include "exMob.h"
#include "exMapInfo.h"
#include "exLineSimplify.h"
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
#include <qpixmap.h>
#include <qmessagebox.h>

#include <stdio.h>

exMap::exMap(QWidget *parent, const char *name)
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
  PNGLoader.initialize();
  PNGLoader.setParent(this);
}

exMap::~exMap() {
  PNGLoader.abort();
  
  if (mi)
    delete mi;
}

void exMap::dirty() {
  if (! is_dirty) { 
    qApp->postEvent(this, new QPaintEvent(QRect(0,0,0,0),false));
    is_dirty = true;
  }
}

void exMap::setConnection(exConnection *nc) {
  c=nc;
}

void exMap::setMap(exMapInfo *m) {
  if (mi)
    delete mi;
 
  mi=m;
  map_load=true;
}

void exMap::setObjectSize(uint8_t uiSize) {
  objsize = uiSize;
  makeObjects(prefs.map_simple);
}

exMapInfo *exMap::getMap() {
   return mi;
}

void exMap::makeObjects(bool simple) {
  int w = objsize;
  int l = w * 2;

  glEdgeFlag(GL_TRUE);
  glNewList(listTriangle, GL_COMPILE);

  if (simple) {
    glBegin(GL_TRIANGLES);
    glNormal3f(0.0,0.0,1.0);
    glVertex3i(w,-w,0);
    glVertex3i(-w,-w,0);
    glVertex3i(0,l,0);
  } else {
    glBegin(GL_TRIANGLE_FAN);
    glVertex3f(0,0,w);  // origin

    glNormal3f(-(l+w),w,l);
    glVertex3i(-w,-w,0); // left side
    glVertex3i(0,l,0);

    glNormal3f(l+w,w,l);
    glVertex3i(w,-w,0);  // right side

    glNormal3f(0,-w,w);  // bottom
    glVertex3i(-w,-w,0);
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

  glNewList(listSquares, GL_COMPILE);

  if (simple) {
    glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.0,0.0,1.0);
    glVertex3i(-w,w,0);
    glVertex3i(w,w,0);
    glVertex3i(-w,-w,0);
    glVertex3i(w,-w,0);
  } else {
    glBegin(GL_TRIANGLE_FAN);
    glVertex3i(0,0,w);  // origin

    glNormal3f(0.0,w,w);
    glVertex3i(-w,w,-w);
    glVertex3i(w,w,-w);

    glNormal3f(w,0.0,w);
    glVertex3i(w,-w,-w);

    glNormal3f(0.0,-w,w);
    glVertex3i(-w,-w,-w);

    glNormal3f(-w,0.0,w);
    glVertex3i(-w,w,-w);
  }

  glEnd();
  glEndList();
}

/* Search for NVdriver using the QUICK method, as opposed to directly querying
   the Kernel's list of loaded modules. This method ONLY works when the Kernel
   has ProcFS support and a mounted /proc partition. */
bool exMap::isNVidiaModuleLoaded() {

  FILE* fModules;
  char* chModuleList;

  chModuleList = (char*) malloc (16384);  

  if ((fModules = fopen ("/proc/modules","ro")) != NULL) {

    while (fgets (chModuleList, 16384, fModules) != NULL) {

      if (strstr(chModuleList, "NVdriver") != NULL) {

        fclose(fModules);
        delete [] chModuleList;

        return true;

      }
       
    }

  fclose (fModules);
  }

  delete [] chModuleList;

  return false;
}

void exMap::initializeGL() {
  static GLfloat lightpos[4]={0.5,-1.0,1.0,0.0};
  static GLfloat diffuse[4]={0.5,0.5,0.5,1.0};
  static GLfloat ambient[4]={-0.0,-0.0,-0.0,1.0};
  static GLfloat material[4]={0.5,0.5,0.5,1.0};

  if (! format().doubleBuffer())
    qWarning("Single Buffer GL only - Flicker might happen");
  if (! format().depth())
    qWarning("NO GL DEPTH BUFFER - No polygon sorting");

  qWarning("Checking for hardware OpenGL support...");
  if (isNVidiaModuleLoaded()) {
      qWarning(">> YES (NVdriver)\t-\tThis should be VERY FAST!");
      has_NVdriver = TRUE;
      has_direct   = FALSE;
  } else if (format().directRendering()) {
      qWarning(">> YES (Direct Render)\t-\tThis will probably be AVERAGE!");
      has_NVdriver = FALSE;
      has_direct   = TRUE;
  } else {
    qWarning(">> NO (No Direct Render)\t-\tThis will probably be SLOW!");
    has_NVdriver = FALSE;
    has_direct   = FALSE;
  }

  glClearColor(0.0,0.0,0.0,0.0);
  glDisable(GL_CULL_FACE);

  glShadeModel(GL_FLAT);
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

  listTriangle = glGenLists(1);
  listCircle   = glGenLists(1);
  listSquares  = glGenLists(1);

  makeObjects(prefs.map_simple);
}

void exMap::resizeGL(int w, int h) {
  glViewport(0,0,w,h);
}

void exMap::paintGL() {
  const QPtrDict<exMob> mobs=c->getMobs();
  QPtrDictIterator<exMob> mobi(mobs);
  exMapElement *mapel;
  exMob *m;
  uint8_t ldif;
  uint8_t l;
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
    glRotatef(1.0,0.0,0.0,1.0);
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

  qglColor( yellow );

  glPushMatrix();
  glTranslated(c->playerx,c->playery,c->playerz);
  objRotate(c->playerhead);
  glCallList(listTriangle);
  glPopMatrix();

  if ((exTick - _lastDarken) > 250) {
    mobDarken = ! mobDarken;
    _lastDarken = exTick;
  }

  for(;mobi.current();++mobi) {
    m=mobi.current();

    if (m->isCurrent()) {
      glPushMatrix();
      glTranslated(m->getProjectedX(),m->getProjectedY(),m->getZ());
      objRotate(m->getHead());

      if (! m->isMob()) {
        if (m->isDead()) {
          setGLColor (m->getColor().dark(160), m->getZ());
        } else if (m->isObj()) {
          setGLColor (m->getColor().dark(5), m->getZ());
        } else if (! m->isInvader()) {
          setGLColor (m->getColor(), m->getZ());
        } else {
          setGLColor ( (mobDarken) ? m->getColor().dark(150) : m->getColor().light(150), m->getZ());
        }
        if (! m->isObj())
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

      if (m->isObj())
        setGLColor(1.0,1.0,1.0, m->getZ());


      if (! m->isObj())
        glCallList(listTriangle);
      else
        glCallList(listSquares);

      glPopMatrix();

      /* if it is filtered draw a yellow circle around it */
	  if( m->isFiltered())
         {
         qglColor( yellow );
         drawCircle(m->getProjectedX(), m->getProjectedY(), 500, 18);
         }

      /* if the mob is within range, draw an agro circle around it */
      if (prefs.agro_circles) {
        if ((m->isMob()) && (m->playerDist() < 1000))  {
          glLineWidth(1.0);

          if (prefs.agro_fading) {
            QColor qcMyColor ( ((0xff << 24) |
                     (((0xff - ((char)(m->playerDist() / 6))) & 0xff) << 16) |
                        0x00 << 8 | 0x00));
            qglColor( qcMyColor );
          } else {
            qglColor( darkRed );
          }
          drawCircle(m->getProjectedX(), m->getProjectedY(), 500, 18);
        }
      }
    }

    m->checkStale();
  }

  m=mobs.find((void *)c->selectedid);
  if (m && m->isCurrent()) {
    qglColor( white );
    glLineWidth ( 2.0 );
    glBegin(GL_LINES);
    glVertex3i(c->playerx,c->playery,c->playerz);
    glVertex3i(m->getProjectedX(),m->getProjectedY(),m->getZ());
    glEnd();
  }

  /* draw a couple range cirlces around the player */

  if(prefs.player_circle_1 >= 226){
    glLineWidth (1.0);
    qglColor(darkGray);
    drawCircle(c->playerx, c->playery, prefs.player_circle_1, 20);
  }
  if(prefs.player_circle_2 >= 251){
    glLineWidth (1.0);
    qglColor(darkGray);
    drawCircle(c->playerx, c->playery, prefs.player_circle_2, 20);
}
	     
  is_dirty = false;

  glFlush();
}

void exMap::drawCircle(int center_x, int center_y, int radius, uint8_t segments)
{
     GLfloat angle;
     GLfloat vectorx, vectory;

     /* draw a circle from a bunch of short lines */
     glBegin(GL_LINE_LOOP);
     for (angle = -M_PI; angle < M_PI; angle += (2.0f * M_PI / (GLfloat)segments))
     {
         vectorx = (GLfloat)center_x + ((GLfloat)radius * sin(angle));
         vectory = (GLfloat)center_y + ((GLfloat)radius * cos(angle));
         glVertex3f(vectorx, vectory, 500.0f);
     }
     glEnd();
}


void exMap::mousePressEvent(QMouseEvent *e) {
  exMob *m;
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

  const QPtrDict<exMob> mobs=c->getMobs();
  QPtrDictIterator<exMob> mobi(mobs);

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

void exMap::keyPressEvent(QKeyEvent *e) {
  double angle=0.0;
  double dist=c->ex->MapSlider->value();

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
      c->ex->MapSlider->setValue((int)(dist * 1.5));
      return;
      break;
    case Qt::Key_Prior:
      c->ex->MapSlider->setValue((int)(dist / 1.5));
      return;
      break;
    default:
      e->ignore();
      return;
      break;
  }
  angle = angle * M_PI / 2.0;
  edit_xofs += (int)(cos(angle)*dist / 2.0);
  edit_yofs -= (int)(sin(angle)*dist / 2.0);
  dirty();
}

void exMap::setGLColor(double r, double g, double b, int z) {
  double col[4];
  
  col[0]=r;
  col[1]=g;
  col[2]=b;
  setGLColor(col, (double) z);
  glColor4d(col[0],col[1],col[2],col[3]);
}

void exMap::setGLColor(double *col, double z) {
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

void exMap::setGLColor(QColor col, int z) {
  setGLColor(col.red() / 255.0, col.green() / 255.0, col.blue() / 255.0, z);
}

void exMap::objRotate(unsigned int daocheading) {
  double r=daocheading;
  r*=360;
  r/=0x1000;
  glRotatef(r,0.0,0.0,1.0);
}

int exMap::stringInt(QStringList *sl, unsigned int sec) {
  bool ok;
  if ((sl == NULL) || (sec >= sl->size())) {
    return 0;
  }
  int v=(*sl)[sec].toInt(&ok, 10);
  if (!ok) {
    return 0;
  }
  return v;
}


void exMap::mapRead() {
  QString line;
  QString cmd;
  QString color;
  QString title;
  exMapElement *elem;
  int xadd;
  int yadd;
  unsigned int i;

  map.clear();
  recache = true;

  xadd=mi->getBaseX();
  yadd=mi->getBaseY();

  ignore_fill = false;

  if (PNGLoader.running())
    PNGLoader.abort();
 
  if (! PNGLoader.running()) 
    PNGLoader.start();

  QFile f;
  f.setName(QString("usermaps/").append(mi->getName()));
  if (! f.open(IO_ReadOnly)) {
    f.setName(QString("maps/").append(mi->getName()));
    if (! f.open(IO_ReadOnly)) {
      qWarning("Failed to open map named %s",(const char *) mi->getName());
      elem = new exMapElementPoint();
      elem->fromString("NO MAP LOADED", xadd, yadd);
      map.append(elem);
    }
    else {
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
              elem=new exMapElementLine();
              break;
            case 'P':
              elem=new exMapElementPoint();
              break;
          }
          if (elem && elem->fromString(lst, xadd, yadd)) {
             ok = true;
             map.append(elem);
          }
        }
        if (! ok) {
          qWarning("Map element %s was not accepted", (const char *)line);
          if (elem)
            delete elem;
        }
      }
    }
  }
}

exMapElement::exMapElement() {
  displist = glGenLists(1);
}

exMapElement::~exMapElement() {
  glDeleteLists(displist, 1);
}

void exMapElement::recache(exMap *map) {
    glNewList(displist, GL_COMPILE);
    draw(map);
    glEndList();
    glCallList(displist);
}


void exMapElement::cached_draw() {
  glCallList(displist);
}

void exMapElement::setColor(QString col) {
  QColor color=QColor(col);
  r=color.red() / 255.0;
  g=color.green() / 255.0;
  b=color.blue() / 255.0;
}

bool exMapElement::fromString(QStringList lst, int, int) {
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


exMapElementTexture::exMapElementTexture(int px, int py, int pw, int ph, exMap *map, QImage img, bool bAlreadyInGLFormat) {

  bounds.setRect(px, py, pw+1, ph+1);

  if (! bAlreadyInGLFormat)
    img=QGLWidget::convertToGLFormat(img);

  glGenTextures(1, &texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  if (map->has_direct) {
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB16, img.width(), img.height(), GL_RGBA, GL_UNSIGNED_BYTE, img.bits());
  } else {
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, 3, img.width(), img.height(), 0, GL_RGBA, GL_UNSIGNED_BYTE, img.bits());
  }
}

exMapElementTexture::~exMapElementTexture() {
  glDeleteTextures(1, &texture);
}

void exMapElementTexture::draw(exMap *) {
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

bool exMapElementTexture::visible(QRect &r) {
  return r.intersects(bounds);
}

exMapElementPoint::exMapElementPoint() {
  xpos=ypos=zpos=0;
}

void exMapElementPoint::draw(exMap *map) {
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

bool exMapElementPoint::visible(QRect &r) {
  return r.contains(xpos, ypos);
}

bool exMapElementPoint::fromString(QStringList lst, int xadd, int yadd) {
  bool ok;

  if (! exMapElement::fromString(lst, xadd, yadd))
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

exMapElementLinePoint::exMapElementLinePoint(int nx, int ny, int nz) {
  x=nx;
  y=ny;
  z=nz;
}

exMapElementLineTess::exMapElementLineTess(GLenum gltype) {
  points.setAutoDelete(FALSE);
  type=gltype;
}

exMapElementLine::exMapElementLine() {
  points.setAutoDelete(FALSE);
  tesspoints.setAutoDelete(FALSE);
  allpoints.setAutoDelete(FALSE);
  filled=FALSE;
}

bool exMapElementLine::visible(QRect &r) {
  return r.intersects(bounds);
}

bool exMapElementLine::fromString(QStringList lst, int xadd, int yadd) {
  bool ok;
  int num;
  int i;
  int x, y, z;
  exLineSimplify s;
  exLineSimplifyPoint *ps;
  exMapElementLinePoint *p;

  if (! exMapElement::fromString(lst, xadd, yadd))
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
    p=new exMapElementLinePoint(x, y, z);
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

exMapElementLine *exMapElementLine::tessLine;

void exMapElementLine::BeginCallback(GLenum type) {
  tessLine->tessTess=new exMapElementLineTess(type);
  tessLine->tesspoints.append(tessLine->tessTess);
}

void exMapElementLine::ErrorCallback(GLenum err) {
  qWarning("Tesselation failed with error %s",gluErrorString(err));
  tessLine->tessError = TRUE;
}

void exMapElementLine::VertexCallback(void *vertex_data) {
  exMapElementLinePoint *p=(exMapElementLinePoint *)vertex_data;
  tessLine->tessTess->points.append(p);
}

void exMapElementLine::CombineCallback(GLdouble coords[3], void **, GLfloat *, void **outData) {
  exMapElementLinePoint *p=new exMapElementLinePoint((int) coords[0], (int) coords[1], (int) coords[2]);
  tessLine->allpoints.append(p);
  *outData = (void *) p;
}

#ifdef GLU_VERSION_1_2
#define GLU_12
#else
#define GLU_11
#endif

void exMapElementLine::tesselate() {
  exMapElementLinePoint *p;
#ifdef GLU_11
  GLUtriangulatorObj *tess;
#else
  GLUtesselator *tess;
#endif

  tess=gluNewTess();
  tessError = FALSE;
  tessTess = NULL;
  tessLine = this;

  gluTessCallback(tess, (GLenum) GLU_TESS_BEGIN, (void (*)()) exMapElementLine::BeginCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_VERTEX, (void (*)()) exMapElementLine::VertexCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_ERROR, (void (*)()) exMapElementLine::ErrorCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_COMBINE, (void (*)()) exMapElementLine::CombineCallback);

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

void exMapElementLine::draw(exMap *map) {
  exMapElementLinePoint *p;


  for(p=allpoints.first(); p; p=allpoints.next()) {
    p->glcol[0]=r;
    p->glcol[1]=g;
    p->glcol[2]=b; 
    p->glcol[3]=1.0;
    map->setGLColor(p->glcol, (double) p->z);
  }

  if (! map->ignore_fill && filled && prefs.map_fill) {
    exMapElementLineTess *t;
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

exMapPNGLoader::exMapPNGLoader  (void) {m_bGhettoMutex = true;}
exMapPNGLoader::~exMapPNGLoader (void) { abort();  cleanup(); }

void exMapPNGLoader::setParent ( exMap *parent )
{
  this->parent = parent;
}

void exMapPNGLoader::run (void)
{
  if (parent == NULL) {
    m_bGhettoMutex = false;
    return;
  }

  else
    m_bGhettoMutex = true;

  if (empldProgress.pdProgress != NULL)
    empldProgress.pdProgress->reset();

  parent->map.clear();

  exMapInfo *mi;

  for  (mi = parent->mi->getAdjacentZones(); mi && m_bGhettoMutex; mi = parent->mi->getAdjacentZones(mi->getZoneNum())) {

    while (! prefs.map_loadadjacentpngs && mi != NULL && mi->getZoneNum() != parent->mi->getZoneNum()) {

      mi = parent->mi->getAdjacentZones(mi->getZoneNum());

BEGIN_EXPERIMENTAL_CODE
      if (mi != NULL && mi->getZoneNum() != parent->mi->getZoneNum())
        printf("Ignoring Adjacent Zone:\t(ID - %3d)\t- Disabled...\n", mi->getZoneNum());
END_EXPERIMENTAL_CODE

    }

    if (mi == NULL) {
      m_bGhettoMutex = false;
      break;
    }

BEGIN_EXPERIMENTAL_CODE
    printf("Loading Adjacent Zone:\t(ID - %3d)\n", mi->getZoneNum());
END_EXPERIMENTAL_CODE

    if (empldProgress.pdProgress->wasCancelled()) {
      m_bGhettoMutex = false;
      break;
    }

    QFile fimg(QString().sprintf("maps/zone%03d.png", mi->getZoneNum()));

    if (fimg.exists()) {

      QImage img;

      if (img.load(fimg.name())) {

        int w = img.width();
        int h = img.height();

        const int xadd = mi->getBaseX();
        const int yadd = mi->getBaseY();

        parent->ignore_fill = true;

        if (! empldProgress.running())
           empldProgress.start();

        if (empldProgress.running())
          qApp->postEvent(&empldProgress, new QCustomEvent(CALLBACK_PNG_INFO, mi));
 
        for(   int y = 0; y < 8 && m_bGhettoMutex; y++ ) {
          for( int x = 0; x < 8 && m_bGhettoMutex; x++ ) {

            if (empldProgress.running())
              qApp->postEvent(&empldProgress, new QCustomEvent(CALLBACK_PNG_STAT, (void*)(y * 8 + x + 1)));

            struct PNGCallback *pc;
            pc = new struct PNGCallback;

            pc->a   = x * 8192 + xadd;
            pc->b   = y * 8192 + yadd;
            pc->c   = 8192;
            pc->d   = 8192;
            pc->x   = x;
            pc->y   = y;
            pc->img = QGLWidget::convertToGLFormat(img.copy( w * x / 8, h * y /
                                                             8, w / 8, h / 8 ));

            if (parent != NULL)
              qApp->postEvent(parent, new QCustomEvent(CALLBACK_PNG_DATA, (void*)pc));

            if ( empldProgress.pdProgress != NULL && 
                 empldProgress.pdProgress->wasCancelled() )
              m_bGhettoMutex = false;

          }
        }
      }
    }

    if (empldProgress.running())
      qApp->postEvent(&empldProgress, new QCustomEvent(CALLBACK_PNG_FNSH, (void*)0));

  }

  if (empldProgress.pdProgress->wasCancelled()) {
    m_bGhettoMutex = false;
    printf("NOTE:\tThe PNG Loader has been cancelled at the request of the user.\n");
  }
}


bool exMap::event (QEvent *e)
{
  if (e->type() == CALLBACK_PNG_DATA) {
    QCustomEvent *PNGEvent = (QCustomEvent*) e;
    PNGCallback *pc = (PNGCallback*)PNGEvent->data();
    map.append(new exMapElementTexture(pc->a, pc->b, pc->c, pc->d, this, pc->img,true));
    if (pc->y == 7 && pc->x == 7)
    {
      qApp->postEvent(&PNGLoader.empldProgress, new QCustomEvent(CALLBACK_PNG_FNSH, (void*)0));
    }

    pc->img = (QImage)NULL;

    return true;
  }
  QWidget::event( e );
  return false;
}

bool exMapPNGLoader::event (QEvent *e)
{
  if (e->type() == CALLBACK_PNG_ABRT) {
    m_bGhettoMutex = false;
    return true;
  }
  return false;
}

void exMapPNGLoader::abort (void)
{
  m_bGhettoMutex = false;
  cleanup();
}

exMapPNGLoaderDialog::exMapPNGLoaderDialog (void)
{
  pdProgress = new QProgressDialog("Loading PNG map...", "Cancel loading", 64);
}

exMapPNGLoaderDialog::~exMapPNGLoaderDialog (void)
{
  pdProgress->reset();
  cleanup();
}

void exMapPNGLoaderDialog::run (void)
{
  while (true)
    msleep(10);
}

bool exMapPNGLoaderDialog::event (QEvent *e)
{
  if (e->type() == CALLBACK_PNG_STAT) {
    QCustomEvent *PNGEvent = (QCustomEvent*) e;
    pdProgress->setProgress((int)PNGEvent->data());
    return true;
  }
  else if (e->type() == CALLBACK_PNG_FNSH) {
    pdProgress->reset();
    return true;
  }
  else if (e->type() == CALLBACK_PNG_INFO) {
    QCustomEvent *PNGEvent = (QCustomEvent*) e;
    pdProgress->setLabelText(QString().sprintf("Loading PNG for:     %s",((exMapInfo*)PNGEvent->data())->getZoneName().ascii()));
    return true;
  }
  return false;
}
