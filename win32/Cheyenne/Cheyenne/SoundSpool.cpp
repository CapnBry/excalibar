/******************************************************************************
Cheyenne: a real-time packet analyzer/sniffer for Dark Age of Camelot
Copyright (C) 2003, the Cheyenne Developers

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
******************************************************************************/

#include "global.h"
#include "threads.h"
#include "signals.h"
#include "soundspool.h"

SoundSpool::SoundSpool()
{
} // end SoundSpool

SoundSpool::~SoundSpool()
{
} // end ~SoundSpool

DWORD SoundSpool::Run(const bool& bContinue)
{
    // lower priority
    SetThreadPriority(GetCurrentThread(),THREAD_PRIORITY_IDLE);

    // the set of signals I'm gonna wait on
    MultiWaitSignal MySignal;

    // add signals to the list
    for(int i=0;i<SoundSpool::_SoundTypeLast;++i)
        {
        MySignal.AddSignalToList(SoundSignals[i]);
        }

    while(bContinue)
        {
        EventSignal* signal=MySignal.WaitAny().first;

        if(signal != NULL)
            {
            // reset signal
            signal->reset();

            // stop playing whatever we are currently playing
            PlaySound(NULL,NULL,SND_FILENAME|SND_ASYNC);

            // play sound associated with signal
            // do some funky pointer math just 'cause
            unsigned char* sig_base=reinterpret_cast<unsigned char*>(&SoundSignals[0]);
            unsigned char* sig_current=reinterpret_cast<unsigned char*>(signal);
            unsigned int ndx=(sig_current-sig_base) / sizeof(EventSignal);

            switch(ndx)
                {
                case SoundSpool::AlbCreate:
                    // play the alb create sound
                    PlaySound(Config.GetAlbSoundFile().c_str(),NULL,SND_FILENAME|SND_ASYNC);
                    break;

                case SoundSpool::HibCreate:
                    // play the hib create sound
                    PlaySound(Config.GetHibSoundFile().c_str(),NULL,SND_FILENAME|SND_ASYNC);
                    break;

                case SoundSpool::MidCreate:
                    // play the mid create sound
                    PlaySound(Config.GetMidSoundFile().c_str(),NULL,SND_FILENAME|SND_ASYNC);
                    break;

                case SoundSpool::NamedMobCreate:
                    // play the named mob create sound
                    break;
                    
                default:
                    break;
                } // end switch signal #

            } // end if a signal was set
        } // end forever

    // done
    return(0);
} // end Run

