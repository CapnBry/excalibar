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

#ifndef _OELINESIMPLIFY_H
#define _OELINESIMPLIFY_H

#include <qptrlist.h>

class oeLineSimplifyPoint {
  public:
    double x, y, z;
    oeLineSimplifyPoint();
    oeLineSimplifyPoint(double nx, double ny, double nz);
    void copy(oeLineSimplifyPoint *p);
    double distLine(double x1, double y1, double z1, double x2, double y2, double z2) const;
    double distLine(oeLineSimplifyPoint *a, oeLineSimplifyPoint *b) const;
    static double dist3d(double x1, double y1, double z1, double x2, double y2, double z2);
    double dist3d(double x1, double x2, double x3) const;
};

class oeLineSimplify {
  public:
    QPtrList<oeLineSimplifyPoint> points;
    oeLineSimplify();
    void add(double x, double y, double z);
    void add(oeLineSimplifyPoint *p);
    void DouglasPeucker(double sigma);
};

#else
class oeLineSimplifyPoint;
class oeLineSimplify;
#endif
