/*
 * Copyright 2002 the Excalibur contributors (http://excalibar.sourceforge.net/) *
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
 
#ifndef _EXXP_H
#define _EXXP_H

#include <iostream>
#include <qdatetime.h>
#include <qptrlist.h>
#include <qmap.h>
#include <formexcalibur.h>

class XPData
{
  public:
    XPData(int ticks, unsigned int val);
    virtual ~XPData();
    const QTime getTime() const;
    const unsigned int val;
    const int ticks;
  private:
    QTime cTime;
};

class XPStats
{
  public:
    XPStats();
    virtual ~XPStats();
    void StartSession();
    void setForm(FormExcalibur *form);
    void setDisplayRateMult(int mult);
    void setDisplayEEPTMult(int mult);
    void setDuration(int time);
    void RewardXP(unsigned int xp);
    void MoveXPBar(int ticks);
    void Update();
  private:
    void UpdateValues();
    QString formatTime(unsigned int seconds) const;
    const char * commaStr(long long number) const;
    FormExcalibur *ex;
    QPtrList<XPData> xp_list;
    QMap<int, double> EEPT_level;
    QTime start;
    // EEPT = Estimated Experience Per Tick
    double EEPT;
    float tick_rate;
    int EEPT_ticks;
    int deaths;
    int lasttick;
    int mobskilled;
    int est_kills;
    int total_ticks;
    int avg_ticks;
    int tick_loss;
    int EEPT_mult;
    int rate_mult;
    int timeperiod;
    int avg_xp;
    int xp_rate;
    long long total_xp;
    long long xp_level;
    unsigned int EEPT_xp;
    unsigned int delta;
    unsigned int current_xp;
    unsigned int xp_loss;
    unsigned int play_sec;
    unsigned int ttl_sec;
    bool firstupdate;
};

#endif
