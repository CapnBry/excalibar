# Copyright 2001 Slicer/HackersQuest (slicer@hackersquest.org)
#
# This file is part of Odin's Eye.
#
# Odin's Eye is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# Odin's Eye is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

SOURCES	+= oeMob.cpp oeMap.cpp oeLink.cpp oeLineSimplify.cpp oeItem.cpp oePrefs.cpp oeUpdate.cpp oeSniffer.cpp oePacket.cpp oeConnection.cpp oeMapInfo.cpp main.cpp
HEADERS	+= oeItem.h oePrefs.h oeSniffer.h oeConnection.h  oeUpdate.h oeLink.h

unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}
FORMS	= formodinseye.ui prefsdialog.ui 
TEMPLATE	=app
CONFIG	+= qt warn_on release thread opengl debug network gprof
DEFINES	+= QT_MODULE_NETWORK
INCLUDEPATH	+= /usr/include/pcap/
LIBS	+= -lpcap -lglut -lGLU -lXi
DBFILE	= odinseye.db
LANGUAGE	= C++
#QMAKE_CXXFLAGS += -pg -a
#QMAKE_LFLAGS += -pg -a
