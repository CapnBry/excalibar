#include <iostream>
#include <qlabel.h>
#include <qstring.h>
#include "exXP.h"

using namespace std;

XPData::XPData(int t, unsigned int v) : val(v), ticks(t)
{
    cTime.start();
}

XPData::~XPData()
{
}

const QTime XPData::getTime() const
{
    return cTime;
}

XPStats::XPStats() : start()
{
    ex = NULL;
    delta = 0;
    current_xp = 0;
    lasttick = 0;
    EEPT_xp = 0;
    EEPT_ticks = 0;
    EEPT_mult = 1;
    rate_mult = 1;
    timeperiod = -1; 
    firstupdate = true;
    xp_list.setAutoDelete(TRUE);
}

XPStats::~XPStats()
{
}

void XPStats::StartSession()
{
    if (start.isNull())
        start.start();
}

void XPStats::setForm(FormExcalibur *form)
{
    ex = form;
}

void XPStats::setDisplayRateMult(int mult)
{
    rate_mult = mult;
}

void XPStats::setDisplayEEPTMult(int mult)
{
    EEPT_mult = mult;
}

void XPStats::setDuration(int time)
{
    timeperiod = time;
}

void XPStats::RewardXP(unsigned int xp)
{
    if (current_xp != 0)
        cout << "XP Error - New reward without XP bar update." << endl;
    else
        current_xp = xp;
}

void XPStats::MoveXPBar(int ticks)
{
    cout << "TICKS " << ticks << endl;

    int move = ticks - lasttick;

    if ((move != 0) && !firstupdate) {
        if (move > 0) {
            // XP Gain
            if (current_xp != 0) {
                EEPT_xp += current_xp;
                EEPT_ticks += move;
            } else {
                cout << "XP Error - Moving XP bar up with no reward." << endl;
            }
            cout << "GAIN EXP: TICKS " << move << " XP " << current_xp << endl; 
            xp_list.append( new XPData(move,current_xp) );
        } else {
            // XP Loss or Level
            // Check for level, I think its safe to assume if you go back
            // over 500 ticks then you leveled
            if (move < -500) {
                if (EEPT_ticks == 0)
                    EEPT_level[delta] = 0;
                else {
                    EEPT_level[delta] = EEPT;
                    EEPT = 0;
                    EEPT_xp = 0;
                    EEPT_ticks = 0;
                }
                delta++;
                cout << "GAINED LEVEL: TICKS " << (1000+move) 
                     << " XP " << current_xp << endl; 
                xp_list.append( new XPData((1000+move),current_xp) );
            } else {
                // If the XP bar goes backwards and you didn't level
                // then you died.  The delta is so we can use the EEPT
                // for the level you died in.
                cout << "LOST EXP: TICKS " << move << " XP " 
                     << current_xp << endl; 
                xp_list.append( new XPData(move,delta) );
            }
        }
        current_xp = 0;
    }

    if (firstupdate)
        firstupdate = false;

    lasttick = ticks;
}

const char * XPStats::commaStr(long long number) const
{
  const int size = 30;    // max number of digits
  static char str[size];  // string to return
  char *ptr1, *ptr2;      // place holders
  char tempStr[size];     // workplace
  int counter = 0;        // three's counter
  bool negative = false;

  if (number < 0) {
    negative = true;
    number = abs(number);
  }

  ptr1 = tempStr;
  do {
    // grab rightmost digit and add value of character zero
    *ptr1++ = (char)(number % 10) + '0';
    // strip off rightmost digit
    number /= 10;
    // if moved over three digits insert comma into string
    if (number &&  !(++counter % 3))
      *ptr1++ = ',';
    // continue until number equal zero
  } while(number);
  if (negative)
      *ptr1++ = '-';
  // this loop reverses characters in a string
  for( --ptr1, ptr2 = str; ptr1 >= tempStr; --ptr1)
    *ptr2++ = *ptr1;
  // add the zero string terminator
  *ptr2 = '\0';
  return str;
}

QString XPStats::formatTime(unsigned int seconds) const
{
    int hours = seconds / 3600;
    seconds -= hours * 3600;
    int minutes = seconds / 60;
    seconds -= minutes * 60;
    return QString().sprintf("%02d:%02d:%02d", hours, minutes, seconds);
}

void XPStats::UpdateValues()
{
    total_xp=0;
    mobskilled=0;
    avg_xp=0;
    deaths=0;
    xp_level=0;
    xp_loss=0;
    tick_loss=0;
    xp_rate=0;
    tick_rate=0;
    est_kills=0;
    EEPT=0;
    ttl_sec=0;
    play_sec=0;
    total_ticks=0;
    avg_ticks=0;
   
    if (EEPT_ticks != 0) {
        EEPT = EEPT_xp / (double)EEPT_ticks;
        xp_level = (long long)((1000 - lasttick) * EEPT);
    }

    for (XPData *data = xp_list.first();data;data = xp_list.next())
    {
        if (timeperiod != -1)
            if (data->getTime().elapsed() > (timeperiod*60000))
                continue;

        if (data->ticks > 0) {
            total_ticks += data->ticks;
            total_xp += data->val;
            mobskilled++;
        } else {
            int xploss;
            if (data->val == delta) {
                xploss = (int)(data->ticks * EEPT);
            } else {
                xploss = (int)(data->ticks * EEPT_level[data->val]);
            }
            total_ticks += data->ticks;
            total_xp += xploss;
            deaths++;
            xp_loss += abs(xploss);
            tick_loss += abs(data->ticks);
            }
    }

    if (start.elapsed() != 0) {
        play_sec = start.elapsed() / 1000;
        if ((timeperiod == -1) || (start.elapsed() < timeperiod*60000)) {
            xp_rate = (int)(total_xp / (start.elapsed() / 60000.0));
            tick_rate = total_ticks / (start.elapsed() / 60000.0);
        } else {
            xp_rate = (int)(total_xp / timeperiod);
            tick_rate = total_ticks / timeperiod;
        }
    }

    if (xp_rate > 0)
        ttl_sec = (unsigned int)(xp_level / (double)xp_rate * 60);

    if (mobskilled != 0) {
        avg_xp = (total_xp + xp_loss) / mobskilled;
        avg_ticks = (total_ticks + tick_loss) / mobskilled;
    }

    if (avg_xp != 0)
        est_kills = xp_level / avg_xp;
}

void XPStats::Update()
{
    if (ex && !start.isNull())
    {
        UpdateValues();    
    
        ex->totalXP->setText(QString().sprintf("%s (%.2f Bub)", 
            commaStr(total_xp), total_ticks/100.0));
        ex->mobsKilled->setText(commaStr(mobskilled));
        ex->avgXPperMob->setText(QString().sprintf("%s (%.3f Bub)", 
            commaStr(avg_xp), avg_ticks/100.0));
        ex->xpRate->setText(QString().sprintf("%s (%.3f Bub)", 
            commaStr((long long)xp_rate * rate_mult), 
            (tick_rate * rate_mult)/100.0));
        ex->deaths->setText(QString().sprintf("%d / %s (%.2f Bub)", 
            deaths, commaStr(xp_loss), tick_loss/100.0));
        ex->playTime->setText(formatTime(play_sec));
        double display_EEPT = EEPT * EEPT_mult;
        if (display_EEPT > 999)
            ex->estXPperTicketc->setText(commaStr((long long)display_EEPT));
        else
            ex->estXPperTicketc->setText(QString::number(display_EEPT));
        ex->estKillstoLevel->setText(commaStr(est_kills));
        ex->estTimetoLevel->setText(formatTime(ttl_sec));
        ex->estXPtoLevel->setText(commaStr(xp_level));
        ex->currentTick->setText(QString().sprintf("%d / 999", lasttick));
    }
}
