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

#ifndef _EXLINESIMPLIFY_H
#define _EXLINESIMPLIFY_H

#include <qptrlist.h>

class exLineSimplifyPoint {
  public:
    double x, y, z;
    exLineSimplifyPoint();
    exLineSimplifyPoint(double nx, double ny, double nz);
    void copy(exLineSimplifyPoint *p);
    double distLine(double x1, double y1, double z1, double x2, double y2, double z2) const;
    double distLine(exLineSimplifyPoint *a, exLineSimplifyPoint *b) const;
    static double dist3d(double x1, double y1, double z1, double x2, double y2, double z2);
    double dist3d(double x1, double x2, double x3) const;
};

class exLineSimplify {
  public:
    QPtrList<exLineSimplifyPoint> points;
    exLineSimplify();
    void add(double x, double y, double z);
    void add(exLineSimplifyPoint *p);
    void DouglasPeucker(double sigma);
};

#else
class exLineSimplifyPoint;
class exLineSimplify;
#endif
