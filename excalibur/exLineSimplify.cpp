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

#include "exLineSimplify.h"
#include <math.h>

exLineSimplifyPoint::exLineSimplifyPoint() {
  x=y=z=0;
}

exLineSimplifyPoint::exLineSimplifyPoint(double nx, double ny, double nz) {
  x=nx;
  y=ny;
  z=nz;
}

void exLineSimplifyPoint::copy(exLineSimplifyPoint *p) {
  x=p->x;
  y=p->y;
  z=p->z;
}

double exLineSimplifyPoint::distLine(double x1, double y1, double z1, double x2, double y2, double z2) const {
  double u;
  double ddist;
  double xm, ym, zm;
  double dist;
  double tdist;    

    ddist=dist3d(x1,y1,z1,x2,y2,z2);

  if (ddist <= 0.0) {
    return dist3d(x1,y1,z1);
  }

    u = (x-x1)*(x2-x1)+(y-y1)*(y2-y1)+(z-z1)*(z2-z1);
    u = u / (ddist*ddist);

    xm = x1 + u * (x2-x1);
    ym = y1 + u * (y2-y1);
    zm = z1 + u * (z2-z1);

   dist=dist3d(xm,ym,zm);
   tdist = dist3d(x1,y1,z1);
   if (tdist < dist)
     dist=tdist;
   tdist = dist3d(x2,y2,z2);
   if (tdist < dist)
     dist=tdist;

   return dist;
}

double exLineSimplifyPoint::distLine(exLineSimplifyPoint *a, exLineSimplifyPoint *b) const {
  return distLine(a->x,a->y,a->z,b->x,b->y,b->z);
}

double exLineSimplifyPoint::dist3d(double x1, double y1, double z1, double x2, double y2, double z2) {
  double xd,yd,zd;
  xd=x1-x2;
  yd=y1-y2;
  zd=z1-z2;
  return sqrt(xd*xd+yd*yd+zd*zd);
}

double exLineSimplifyPoint::dist3d(double x1, double y1, double z1) const {
  return dist3d(x,y,z,x1,y1,z1);
}

exLineSimplify::exLineSimplify() {
  points.setAutoDelete(true);
}

void exLineSimplify::add(double x, double y, double z) {
  points.append(new exLineSimplifyPoint(x,y,z));
}

void exLineSimplify::add(exLineSimplifyPoint *p) {
  add(p->x,p->y,p->z);
}

/*
 * Implementation of the Douglas-Peucker line simplification algorithm
 */

void exLineSimplify::DouglasPeucker(double sigma) {
  unsigned int i;
  unsigned int besti;
  double dist;
  double bdist;
  exLineSimplifyPoint p1, p2;
  exLineSimplifyPoint *p;

  if (points.count() < 3) 
    return;

  besti = 0;
  bdist = -1.0;

  p1.copy(points.first());
  p2.copy(points.last());

  for(i=1;i<points.count()-1;i++) {
    p=points.at(i);
    dist=p->distLine(&p1, &p2);
    if (dist > bdist) {
      besti=i;
      bdist = dist;
    }
  }

  if (bdist > sigma) {
    exLineSimplify s1, s2;
    for(i=0;i<=besti;i++) {
      s1.add(points.at(i));
    }
    for(i=besti;i<points.count();i++) {
      s2.add(points.at(i));
    }
    s1.DouglasPeucker(sigma);
    s2.DouglasPeucker(sigma);

    points.clear();

    for(i=0;i<s1.points.count();i++) 
      add(s1.points.at(i));
    for(i=1;i<s2.points.count();i++)
      add(s2.points.at(i));
  } else {
    points.clear();
    add(&p1);
    add(&p2);
  }
}
