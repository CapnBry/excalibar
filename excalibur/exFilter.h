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

class exFilter;

#ifndef _EXFILTER_H
#define _EXFILTER_H

#include <qstring.h>
#include "excalibur.h"

class exFilter
{
  private:
    QString FilterStr;

  public:
	exFilter() { FilterStr = ""; }
	QString getFilter() { return FilterStr; }
        void setFilter(QString s) { FilterStr = s; }
        bool isFilterSet(void) { return (FilterStr.length()); }
};

#endif
