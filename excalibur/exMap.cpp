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
#include <qpixmap.h>
#include <qpushbutton.h>
#include <qprogressbar.h>
#include <qlabel.h>
#include <qaction.h>
#include <qinputdialog.h>
#include <qregexp.h>
#include <qmessagebox.h>
#include <qtextstream.h>
#include <qslider.h>
#include <qimage.h>
#include <qmessagebox.h>

#include <stdio.h>

exMap::exMap(QWidget *parent, const char *name)
 : QGLWidget(QGLFormat(DoubleBuffer|DepthBuffer|Rgba|DirectRendering),parent,name) {
  objsize = 150; 
  range = 16000;
  c = NULL; 
  is_dirty = true; 
  map_load = false; 
  mobDarken = false;
  _lastDarken = 0;
  mi = NULL;
  edit_xofs = edit_yofs = 0;
  recache = true;
  map.setAutoDelete(true);
  MapLoader.initialize();
  MapLoader.setParent(this);
  m_bNeedMapReload = false;

  connect(&idleTimer, SIGNAL(timeout()), this, SLOT(idleTimeout()));
}

exMap::~exMap() {
  MapLoader.abort();
  
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
  c = nc;

  c->ex->MapStatus->setText("");
  c->ex->MapProgress->setProgress(0);
  c->ex->MapStatus->hide();
  c->ex->MapProgress->hide();
  c->ex->MapCancel->hide();
}

void exMap::setMap(exMapInfo *m) {
  if (mi)
    delete mi;

  mi=m;
  map_load=true;
}

void exMap::setObjectSize(unsigned int uiSize) {
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
    glVertex3i(0,l,0);
    glVertex3i(w,-w,0);
    glVertex3i(-w,-w,0);
  } else {
    glBegin(GL_TRIANGLE_FAN);
    glVertex3f(0,0,w);  // origin

    glNormal3f(-(l+w),w,l);
    glVertex3i(-w,-w,0);  // left side
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
    glVertex3f(-w*1.5,-w*1.5,-1);
    glVertex3f(0,l*1.5,-1);
    glVertex3f(w*1.5,-w*1.5,-1);
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
  static GLfloat lightpos[4] = { 0.5, -1.0,  1.0, 0.0};
  static GLfloat diffuse [4] = { 0.5,  0.5,  0.5, 1.0};
  static GLfloat ambient [4] = {-0.0, -0.0, -0.0, 1.0};
  static GLfloat material[4] = { 0.5,  0.5,  0.5, 1.0};

  if (! format().doubleBuffer())
    qWarning("Single Buffer GL only - Flicker might happen");
  if (! format().depth())
    qWarning("NO GL DEPTH BUFFER - No polygon sorting");

  if (isNVidiaModuleLoaded()) {
      qWarning("NVdriver Direct Rendering (DRI) support enabled.");
      has_NVdriver = TRUE;
      has_direct   = FALSE;
  } else if (format().directRendering()) {
      qWarning("Direct Render (DRI) support enabled.");
      has_NVdriver = FALSE;
      has_direct   = TRUE;
  } else {
    qWarning("No Direct Render (DRI) support, performance will be suboptimal.");
    has_NVdriver = FALSE;
    has_direct   = FALSE;
  }

  if (has_NVdriver || has_direct) {
    if (glutExtensionSupported("GL_EXT_texture_compression_s3tc"))
      has_S3TC = TRUE;
  } else {
    has_S3TC = FALSE;
  }

  glClearColor(0.0, 0.0, 0.0, 0.0);

  glEnable(GL_CULL_FACE);
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);

  glDisable(GL_CLIP_PLANE0);
  glDisable(GL_CLIP_PLANE1);
  glDisable(GL_CLIP_PLANE2);
  glDisable(GL_CLIP_PLANE3);
  glDisable(GL_CLIP_PLANE4);
  glDisable(GL_CLIP_PLANE5);

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

  glFlush();

  makeObjects(prefs.map_simple);
}

void exMap::resizeGL(int w, int h) {
  glViewport(0,0,w,h);

  if (! prefs.map_maintain_aspect)
    return;


  // MAINTAIN ASPECT RATIO

  /* Yes, I know this doesn't work right, but I'm tired of this code!!
     If you have a better idea, by all means, let me know!!! - Andon */

 
  GLint minx = (c->playerx - range + edit_xofs);
  GLint maxx = (c->playerx + range + edit_xofs);
  GLint miny = (c->playery - range + edit_yofs);
  GLint maxy = (c->playery + range + edit_yofs);

  GLdouble AspectRatio = (GLdouble)((maxx - minx) / (maxy - miny));

  if (AspectRatio > (w / h)) {            // Taller
    int iTemp = (int)(h - (int)(w / AspectRatio));
    w += iTemp;
    h += iTemp;
    glViewport(0,0,  w,(int)(w / AspectRatio));
  } if (AspectRatio < (w / h)) {          // Wider
    int iTemp = (int)(w - (h * AspectRatio));
    h += iTemp;
    w += iTemp;
    glViewport(0,0,  (int)(h * AspectRatio), h);
  }
}

void exMap::paintGL() {
  const QPtrDict<exMob> mobs=c->getMobs();
  QPtrDictIterator<exMob> mobi(mobs);
  exMapElement *mapel;
  exMob *m;
  double playerhead;
  double playerrad;
  int minx, maxx, miny,maxy;

  if (map_load) {
    recache = true;
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

  c->updateProjectedPlayer();

  playerhead=(c->playerhead * 360.0) / 4096.0;
  playerrad=playerhead * M_PI / 180.0;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  if (prefs.map_rotate)
      glRotatef(180.0+playerhead, 0.0, 0.0, 1.0);
  else if (mi)
      if (mi->getRotate() != 0)
          glRotatef((GLfloat)mi->getRotate(), 0.0, 0.0, 1.0);
  glRotatef(180.0, 1.0, 0.0, 0.0);

  minx=c->playerProjectedX - range + edit_xofs;
  maxx=c->playerProjectedX + range + edit_xofs;
  miny=c->playerProjectedY - range + edit_yofs;
  maxy=c->playerProjectedY + range + edit_yofs;
  glOrtho(minx, maxx, miny, maxy,0.0,-25000.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glColor3f(0.75, 0.75, 0.75);
  glLineWidth(1.0);
  glPointSize(3.0);

  QRect bounds;
  bounds.setCoords(minx, miny, maxx, maxy);
  bounds=bounds.normalize();

  if ((lastfade != prefs.map_fade) || (lastfill != prefs.map_fill))
      recache = true;
  else if ((prefs.map_fade || prefs.map_fill) && (lastz != c->playerz))
      recache = true;

  lastfade = prefs.map_fade;
  lastfill = prefs.map_fill;

  for (mapel=map.first(); mapel; mapel=map.next()) {
     if (recache)
         mapel->recache(this);
     if (mapel->visible(bounds))
         mapel->cached_draw();
  }

  recache = false;

  if (prefs.map_rulers) {
    qglColor( darkGray );
    glBegin(GL_LINES);
    glVertex3i(c->playerProjectedX - range * 2, c->playerProjectedY, 500);
    glVertex3i(c->playerProjectedX + range * 2, c->playerProjectedY, 500);
    glVertex3i(c->playerProjectedX, c->playerProjectedY - range * 2, 500);
    glVertex3i(c->playerProjectedX, c->playerProjectedY + range * 2, 500);
    glVertex3f(c->playerProjectedX * 1.0, c->playerProjectedY * 1.0, 500.0);
    glVertex3f(c->playerProjectedX + cos(playerrad + M_PI_2) * range * 2, c->playerProjectedY + sin(playerrad + M_PI_2) * range * 2, 500.0);
    glEnd();
  }

  if(prefs.player_circle_1 >= 226 || prefs.player_circle_2 >= 251) {

    glColor3f (0.45f, 0.45f, 0.45f);

    if (prefs.player_circle_1 >= 226)
      drawCircle(c->playerProjectedX, c->playerProjectedY, prefs.player_circle_1, 20);

    if (prefs.player_circle_2 >= 251)
      drawCircle(c->playerProjectedX, c->playerProjectedY, prefs.player_circle_2, 20);
  }

  if (! prefs.map_simple ) {
    glEnable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);
  } else {
    glDisable(GL_DEPTH_TEST);
  }

  glPushMatrix();
  glDepthFunc  (GL_LEQUAL);
  glColor3f    (1.0f, 1.0f, 0.0f);
  glTranslatef (c->playerProjectedX, c->playerProjectedY, c->playerz);
  objRotate    (c->playerhead);
  glCallList   (listTriangle);
  glPopMatrix();


 if ((exTick - _lastDarken) > 250) {
    mobDarken = ! mobDarken;
    _lastDarken = exTick;
  }

  for(;mobi.current();++mobi) {
    m=mobi.current();

    if (m->isCurrent() && m->insideRect(bounds)) {
      glPushMatrix();
      glTranslatef (m->getProjectedX(),m->getProjectedY(),m->getZ());
      objRotate(m->getHead());

      /* if it is filtered draw a yellow circle around it */
      if (prefs.filter_circles && m->isFiltered())
          drawAggroCircle(m->getZ(), 1.0, 1.0, 0.0, 0.0);

      /* if the mob is within range, draw an agro circle around it */
      else if (prefs.agro_circles && ((m->isMob()) && (m->playerDist() < 1000)))
          drawAggroCircle(m->getZ(), 1.0, 0.0, 0.0,
                          (prefs.agro_fading) ? m->playerDist() / 1500.0f : 0.0);

        /* if this is a player */
      if (!m->isMobOrObj()) {
        if (m->isDead()) {
          setGLColor (m->getRealmColor().dark(160), m->getZ());
        } else if (! m->isInvader()) {
          setGLColor (m->getRealmColor(), m->getZ());
        } else {
          setGLColor ( (mobDarken) ? m->getRealmColor().dark(150) : m->getRealmColor().light(150), m->getZ());
        }

        glCallList(listCircle);
      }

      if (m->isObj())  {
        setGLColor(1.0,1.0,1.0, m->getZ());
        glCallList(listSquares);
      } else {
        setGLColor(m->getConColor(c->playerlevel), m->getZ());
        glCallList(listTriangle);
      }  // if !obj

      if ((prefs.map_rasterize_merchant_types && m->isMob()) ||
          (prefs.map_rasterize_player_names && !m->isMobOrObj()))
          drawMobName(m);

      glPopMatrix();
    }  // if isCurrent

    m->checkStale();
  }  // for mobs

  m=mobs.find((void *)c->selectedid);
  if (m && m->isCurrent()) {
    glColor3f(1.0, 1.0, 1.0);
    glLineWidth ( 2.0 );
    glBegin(GL_LINES);
    glVertex3i(c->playerProjectedX,c->playerProjectedY,c->playerz);
    glVertex3i(m->getProjectedX(),m->getProjectedY(),m->getZ());
    glEnd();
  }

  is_dirty = false;

  glFlush();

  frames += 1;
  if ((exTick - _last_fps) >= 1000) {
    c->ex->FPS->setText(QString().sprintf("%.1f FPS",
      (1000.0 * (double)frames) / (double)(exTick - _last_fps)));
    frames = 0;
    _last_fps=exTick;
  }

  if (prefs.gl_debug) {
    GLenum error = glGetError();
    if (error != GL_NO_ERROR) {
      qWarning("Houston, we have a problem..\tGL Error:\t%s (%d)",
                gluErrorString(error), error);
    }
  }

  if (prefs.maxfps && !idleTimer.isActive())
      idleTimer.start(0, true);
}

void exMap::drawMobName(exMob *m)
{
    QString qsFormattedName;

    if (m->isMob())
        qsFormattedName = m->getGuild();
    else {
        if (m->getSurname().length() > 0)
            qsFormattedName = m->getName() + " " + m->getSurname();
        else
            qsFormattedName = m->getName();

        if (m->getGuild().length() > 0)
            qsFormattedName.append(" <" + m->getGuild() + ">");
    }

    if (qsFormattedName.length() <= 0)
        return;

    glPushAttrib (GL_ENABLE_BIT);
    glDisable    (GL_LIGHTING);

    glColor3f  (1.0, 1.0, 1.0);

    glBegin    (GL_POINTS);
    glVertex3f (0.0, 0.0, m->getZ() + (float)(2.5 * objsize));
    glEnd();

    glRasterPos3i(20, 20, m->getZ() + (3 * objsize));
    for (unsigned int i = 0; i < qsFormattedName.length(); i++) {
        glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10, qsFormattedName[i].latin1());
    }

    glPopAttrib();
}

void exMap::drawAggroCircle(GLfloat Z, GLfloat R, GLfloat G, GLfloat B,
                            GLfloat distfade_pct)
{
    /* This function assumes that the X, Y, and Z coordinates are
       already in the translation matrix set for the circle */
    glPushMatrix();
    // 600 to account for the radius of the sphere + the height of the triangle
    if (Z >= 600.0f)
        glTranslatef(0.0f, 0.0f, -600.0f);

    if (prefs.alpha_circles && ! prefs.map_simple) {
        glPushAttrib (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_ENABLE_BIT);
        glEnable     (GL_BLEND);
        glEnable     (GL_DEPTH_TEST);
        glDisable    (GL_LIGHTING);
        glBlendFunc  (GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
        glDepthFunc  (GL_LEQUAL);

        if (prefs.alpha_borders) {
            glColor3f  (R, G, B);
            drawCircle (0, 0, 500, 18);
        }

        if (distfade_pct > 0)
            glColor4f(R, G, B, 0.50f - (distfade_pct / 2.0f));
        else
            glColor4f(R, G, B, 0.25f);

        GLUquadricObj *qoCircle;
        qoCircle = gluNewQuadric();

        if (prefs.alpha_speed)  {
            gluQuadricOrientation(qoCircle, GLU_INSIDE);
            gluDisk(qoCircle, 0, 500, 18, 18);
        } else
            gluSphere(qoCircle, 500, 32, 32);
        glPopAttrib();
    }

    else {
        glPushAttrib (GL_ENABLE_BIT);
        glDisable    (GL_LIGHTING);

        glLineWidth  (1.0);
        if (distfade_pct > 0)
            glColor3f(R - distfade_pct, G - distfade_pct, B - distfade_pct);
        else
            glColor3f(R, G, B);
        drawCircle(0, 0, 500, 18);
        glPopAttrib();
    }

    glPopMatrix();
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
  adjustGLColor(col, (double) z);
  glColor4d(col[0],col[1],col[2],col[3]);
}

void exMap::adjustGLColor(double *col, double z) {
  double darken;

  if (!prefs.map_fade) {
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
  GLfloat r = daocheading;
  r *= 360;
  r /= 0x1000;
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

  map.clear();
  recache = true;

  ignore_fill = false;

  if (MapLoader.running())
    m_bNeedMapReload = true;
  
  if (! MapLoader.running())
    MapLoader.start();

}

void exMap::idleTimeout(void)
{
    dirty();
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

  GLuint  min_filter;
  GLuint  mag_filter;
  GLuint  tex_compression;

  if (prefs.map_linear_filter) {
      if (prefs.map_mipmap) {
          min_filter = GL_LINEAR_MIPMAP_LINEAR;
          mag_filter = GL_LINEAR_MIPMAP_LINEAR;
      }
      else {
          min_filter = GL_LINEAR;
          mag_filter = GL_LINEAR;
      }
  } else {
      if (prefs.map_mipmap)  {
          min_filter = GL_NEAREST_MIPMAP_NEAREST;
          mag_filter = GL_NEAREST_MIPMAP_NEAREST;
      }
      else  {
          min_filter = GL_NEAREST;
          mag_filter = GL_NEAREST;
      }
  }

  if (map->has_direct || map->has_NVdriver)
      if (map->has_S3TC && prefs.map_compress_textures)
          tex_compression = 0x83F0; // DXT1 (4x4 64-bit RGB - NO Alpha Channel)
      else
          tex_compression = GL_RGB16;
  else
      tex_compression = 3;

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, min_filter);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mag_filter);

  if (prefs.map_mipmap)
      gluBuild2DMipmaps(GL_TEXTURE_2D, tex_compression, img.width(), img.height(), GL_RGBA, GL_UNSIGNED_BYTE, img.bits());
  else
      glTexImage2D(GL_TEXTURE_2D, 0, tex_compression, img.width(), img.height(), 0, GL_RGBA, GL_UNSIGNED_BYTE, img.bits());
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

  glTexCoord2f(0.0, 0.0);
  glVertex3i(bounds.left(), bounds.bottom(), 250);

  glTexCoord2f(1.0, 0.0);
  glVertex3i(bounds.right(), bounds.bottom(), 250);

  glTexCoord2f(1.0, 1.0);
  glVertex3i(bounds.right(), bounds.top(), 250);

  glTexCoord2f(0.0, 1.0);
  glVertex3i(bounds.left(), bounds.top(), 250);

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
  map->setGLColor(r, g, b, zpos);

  glBegin(GL_POINTS);
  glVertex3i(xpos,ypos,zpos);
  glEnd();
  map->setGLColor(1.0,1.0,1.0,zpos);

  glRasterPos3i(xpos+50,ypos+20,zpos);
  for (unsigned int i=0;i<text.length();i++) 
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
  zpos += 10;
  return TRUE;
}

exMapElementLinePoint::exMapElementLinePoint(int nx, int ny, int nz) {
  x=nx;
  y=ny;
  z=nz + 10;
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
  if (lst.size() != (4 + (unsigned int)num * 3)) {
    printf("Error: %d elements expected, %d found", (4 + (unsigned int)num * 3), lst.size());
    return FALSE;
  }

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
    z = (int) ps->z + 10;
    p=new exMapElementLinePoint(x, y, z);
    allpoints.append(p);
    points.append(p);
    if (bounds.x() == 0) {
      bounds.setRect(x, y, x, y);
    } else {
      if (bounds.left() > x)
        bounds.setLeft(x);
      if (bounds.right() < x)
        bounds.setRight(x);
      if (bounds.top() > y)
        bounds.setTop(y);
      if (bounds.bottom() < y)
        bounds.setBottom(y); 
     }
  }

  if (! filled)
    return TRUE;

  tesselate();
  return TRUE;
}

exMapElementLine *exMapElementLine::tessLine;

void exMapElementLine::TessBeginCallback(GLenum type) {
  tessLine->tessTess=new exMapElementLineTess(type);
  tessLine->tesspoints.append(tessLine->tessTess);
}

void exMapElementLine::TessErrorCallback(GLenum err) {
  qWarning("Tesselation failed with error %s",gluErrorString(err));
  tessLine->tessError = TRUE;
}

void exMapElementLine::TessVertexCallback(void *vertex_data) {
  exMapElementLinePoint *p=(exMapElementLinePoint *)vertex_data;
  tessLine->tessTess->points.append(p);
}

void exMapElementLine::TessCombineCallback(GLdouble coords[3], void **, GLfloat *, void **outData) {
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

  gluTessCallback(tess, (GLenum) GLU_TESS_BEGIN, (void (*)()) exMapElementLine::TessBeginCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_VERTEX, (void (*)()) exMapElementLine::TessVertexCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_ERROR, (void (*)()) exMapElementLine::TessErrorCallback);
  gluTessCallback(tess, (GLenum) GLU_TESS_COMBINE, (void (*)()) exMapElementLine::TessCombineCallback);

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
    map->adjustGLColor(p->glcol, (double) p->z);
  }

  if (! map->ignore_fill && filled && prefs.map_fill) {
    exMapElementLineTess *t;
    glDisable(GL_CULL_FACE);  // some of our tesselated polys are upside down
    for (t=tesspoints.first(); t; t=tesspoints.next()) {
      glBegin(t->type);
      for (p=t->points.first(); p; p=t->points.next()) {
        glColor4f(p->glcol[0] / 2, p->glcol[1] / 2, p->glcol[2] / 2, p->glcol[3]);
        glVertex3i(p->x, p->y, p->z / 10);
      }
      glEnd();
    }
    glEnable(GL_CULL_FACE);
  }

  glBegin(GL_LINE_STRIP);
  for (p=points.first(); p; p=points.next()) { 
    glColor4f(p->glcol[0],p->glcol[1],p->glcol[2],p->glcol[3]);
    glVertex3i(p->x,p->y,p->z);
  }
  glEnd();  
}

void exMap::loadVectorMap (const exMapInfo *mi) {
  if (mi == NULL)
    return;

  QString line;
  QString cmd;
  QString color;
  QString title;
  exMapElement *elem;
  int xadd;
  int yadd;
  unsigned int i;

  recache = true;

  xadd=mi->getBaseX();
  yadd=mi->getBaseY();


  /* Not sure what the issue here is.. Something isn't thread safe!!! */

  QString qsName;
  try {
    qsName = ((exMapInfo*)mi)->getName();

    if (qsName == NULL) {
      Q_ASSERT(qsName != NULL);
      return;
    }
  } catch (...) {
    qWarning("Error:\texMapInfo == NULL!");
    return;
  }


  QFile f;
  f.setName(QString("usermaps/").append(qsName));

  if (! f.open(IO_ReadOnly)) {
    f.setName(QString("maps/").append(qsName));

    if (! f.open(IO_ReadOnly)) {
      qWarning("Failed to open map named %s",qsName.latin1());
      elem = new exMapElementPoint();
      elem->fromString("NO MAP LOADED", xadd, yadd);
      map.append(elem);
    } else {
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
          qWarning("\nSub-Element: %d\n>>   %s\n>>>  %s\n>>>> was not accepted.\n", i, (const char*)line.left(line.length() -1), qsName.latin1()); 
          if (elem)
            delete elem;
        }
      }
    }
  }
}

exMapLoader::exMapLoader  (void) {m_bGhettoMutex = true;}
exMapLoader::~exMapLoader (void) {    this->abort();    }

void exMapLoader::setParent ( exMap *parent )
{
  this->parent = parent;
}

void exMapLoader::run (void)
{
BeginMapLoaderThread:

#ifndef _WIN32
  struct sched_param sp;
  memset(&sp, 0, sizeof(sp));
  sp.sched_priority=sched_get_priority_min(SCHED_RR) + 1;
  setpriority(PRIO_PROCESS, getpid(), 7);
#endif

  if (parent == NULL) {
    m_bGhettoMutex = false;
    return;
  }

  else
    m_bGhettoMutex = true;

  parent->m_bNeedMapReload = false;
  parent->c->resetMap();

  parent->map.clear();

  exMapInfo *mi;
  mi = parent->mi->getAdjacentZones(-1);

  for  (int i = -1; i < 300 && mi && m_bGhettoMutex && parent && !parent->m_bNeedMapReload; mi = parent->mi->getAdjacentZones(i)) {
    if (i != -1 && parent->mi->getZoneNum() == mi->getZoneNum())
      mi = parent->mi->getAdjacentZones(mi->getZoneNum());

    while (! prefs.map_load_adjacent_zones && mi != NULL && mi->getZoneNum() != parent->mi->getZoneNum()) {

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

    if (parent->c->checkMap()) {
      m_bGhettoMutex = false;
      break;
    }

    QFile fimg(QString().sprintf("maps/zone%03d.png", mi->getZoneNum()));

    if (prefs.map_load_png_maps && fimg.exists()) {

      qApp->postEvent(parent, new QCustomEvent(CALLBACK_PNG_INFO, mi));

      QImage img;

      if (img.load(fimg.name())) {

        const int w = img.width();
        const int h = img.height();

        const int xadd = mi->getBaseX();
        const int yadd = mi->getBaseY();

        parent->ignore_fill = true;

        for(   int y = 0; y < 8 && m_bGhettoMutex && !parent->m_bNeedMapReload; y++ ) {
          for( int x = 0; x < 8 && m_bGhettoMutex && !parent->m_bNeedMapReload; x++ ) {

            qApp->postEvent(parent, new QCustomEvent(CALLBACK_PNG_STAT, (void*)(y * 8 + x + 1)));

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

            if (parent->c->checkMap())
              m_bGhettoMutex = false;
          }
        }
      }
    }

    if (parent != NULL && mi != NULL)
      qApp->postEvent(parent, new QCustomEvent(CALLBACK_VCT_LOAD, (void*)mi));

    if (i < 0 || mi->getZoneNum() == 0)
      i++;

    else if (mi != NULL)
      i = mi->getZoneNum();

    else
      i = 300;

    qApp->postEvent(parent, new QCustomEvent(CALLBACK_PNG_FNSH, (void*)1));

    if (! prefs.map_load_adjacent_zones)
      return;
  }

  if (parent->c->checkMap()) {
    m_bGhettoMutex = false;
    printf("NOTE:\tThe PNG Loader has been cancelled at the request of the user.\n");
  }

  qApp->postEvent(parent, new QCustomEvent(CALLBACK_PNG_FNSH, (void*)1));

  if (parent->m_bNeedMapReload)
    goto BeginMapLoaderThread;
}


bool exMap::event (QEvent *e)
{
  if (e->type() == CALLBACK_PNG_DATA) {
    QCustomEvent *PNGEvent = (QCustomEvent*) e;
    PNGCallback *pc = (PNGCallback*)PNGEvent->data();
    if (pc != NULL)
    {
      map.append(new exMapElementTexture(pc->a, pc->b, pc->c, pc->d, this, pc->img,true));
      delete (struct PNGCallback*)pc;
    }

    if (! MapLoader.running()) {
      qApp->postEvent(this,new QCustomEvent(CALLBACK_PNG_FNSH, (void*)0));
    }

    return true;
  }
  else if (e->type() == CALLBACK_PNG_INFO) {
    QCustomEvent *PNGEvent = (QCustomEvent*) e;

    if (! c->ex->MapStatus->isVisible())
    {
      c->ex->MapStatus->show();
      c->ex->MapProgress->show();
      c->ex->MapCancel->show();
    }

    c->ex->MapProgress->setTotalSteps(64);
    c->ex->MapStatus->setText(QString().sprintf("Loading PNG for:     %s",((exMapInfo*)PNGEvent->data())->getZoneName().latin1()));
    c->ex->MapProgress->setProgress(0);
    
    return true;
  }
  if (e->type() == CALLBACK_PNG_STAT) {
    QCustomEvent *PNGEvent = (QCustomEvent*) e;
    c->ex->MapProgress->setProgress((int)PNGEvent->data());
    return true;
  }
  else if (e->type() == CALLBACK_PNG_FNSH) {
    c->ex->MapStatus->setText("");
    c->ex->MapProgress->setProgress(0);

    if (c->ex->MapStatus->isVisible()) {
      c->ex->MapStatus->hide();
      c->ex->MapProgress->hide();
      c->ex->MapCancel->hide();
    }

    return true;
  }
  else if (e->type() == CALLBACK_VCT_LOAD) {
    QCustomEvent *MapEvent = (QCustomEvent*) e;
    exMapInfo *mi = (exMapInfo*)MapEvent->data();

    if (mi != NULL)
      loadVectorMap(mi);
    return true;
  }
  QWidget::event( e );
  return false;
}

bool exMapLoader::event (QEvent *e)
{
  if (e->type() == CALLBACK_PNG_ABRT) {
    this->abort();
    return true;
  }
  return false;
}

void exMapLoader::abort (void)
{
  m_bGhettoMutex = false;

  if (parent != NULL)
    qApp->postEvent(parent, new QCustomEvent(CALLBACK_PNG_FNSH, (void*)0));
}
