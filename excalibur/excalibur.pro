SOURCES	+= exMob.cpp exMap.cpp exLink.cpp exLineSimplify.cpp exItem.cpp exPrefs.cpp exSniffer.cpp exPacket.cpp exConnection.cpp exMapInfo.cpp main.cpp exMessage.cpp
HEADERS	+= exItem.h exPrefs.h exSniffer.h exConnection.h exLink.h exPacket.h exMap.h exMessage.h
#
# Copyright 2002 the Excalibur contributors (http://excalibar.sourceforge.net/)
#
# Portions of this software are based on the work of Slicer/Hackersquest.
# Those portions, Copyright 2001 Slicer/Hackersquest <slicer@hackersquest.org)
#
# This file is part of Excalibur.
#
# Excalibur is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# Excalibur is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.



unix {
  UI_DIR      = .ui
  MOC_DIR     = .moc
  OBJECTS_DIR = .obj
}
QMAKE_CXXFLAGS += -include excalibur.h
#QMAKE_LFLAGS   += -pg -a
FORMS	= formexcalibur.ui prefsdialog.ui messages.ui
TEMPLATE	=app
CONFIG	+= qt warn_on release thread opengl debug network gprof
INCLUDEPATH	+= /usr/include/pcap/
LIBS	+= -lpcap -lglut -lGLU -lXi
DBFILE	= excalibur.db
LANGUAGE	= C++
