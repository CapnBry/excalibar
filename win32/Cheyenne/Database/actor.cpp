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
#include "..\Utils\Logger.h"
#include ".\actor.h"

// reference a clock & logger (this must be instantiated in 
// a translation unit somewhere)
extern CheyenneClock Clock;
extern logger_t Logger;

// this file contains the member functions for classes defined in actor.h
// that make sense to NOT inline.

float Motion::RangeTo(const Motion& To)const
{
    float x,y,z;
    
    // get coordinates
    x=To.GetXPos()-GetXPos();
    y=To.GetYPos()-GetYPos();
    z=To.GetZPos()-GetZPos();
    
    return(sqrt(x*x + y*y + z*z));
}

std::pair<float,float> Motion::RangeAzimuthTo(const Motion& To)const
{
    float x,y,z,az;
    
    // get coordinates
    x=To.GetXPos()-GetXPos();
    y=To.GetYPos()-GetYPos();
    z=To.GetZPos()-GetZPos();
    
    // get azimuth
    az=atan2(x,-y);
    
    // make sure its positive, nobody wants to try and
    // turn -90° into "go west, 270°" in their head.
    if(az < 0.0f)
        {
        az+=3.1415926535897932384626433832795f;
        }
    
    //az+=3.1415926535897932384626433832795f;
    //az=fmod(az,6.283185307179586476925286766559f);
    
    /*
    // get bearing 
    bng=GetHeading()-az;
    
    // adjust so that the range is (-PI,+PI] instead of [0,2*PI)
    if(bng > 3.1415926535897932384626433832795f)
        {
        bng = 3.1415926535897932384626433832795f - bng;
        }
    */
    
    return(std::make_pair<float,float>(sqrt(x*x + y*y + z*z),az));
} // end Motion::RangeAzimuthTo

void Motion::Print(std::ostream& os)const
{
    os << "[Motion::Print]\n"
        << "ValidTime=" << GetValidTime().Seconds() << "\n"
        << "XPos=" << GetXPos() << "\n"
        << "YPos=" << GetYPos() << "\n"
        << "ZPos=" << GetZPos() << "\n"
        << "Heading=" << GetHeading()*180.0f/3.1415926535897932384626433832795f << "\n"
        << "Speed=" << GetSpeed() << "\n";

    // done
    return;
} // end Motion::Print

void Motion::GetPointRelative
    (
    const float bearing,
    const float distance,
    float& x,
    float& y
    )const
{
    float az=GetHeading() + bearing;
    
    // make [0,2PI)
    if(az > 2.0f*3.1415926535897932384626433832795f)
        {
        az -= 2.0f*3.1415926535897932384626433832795f;
        }
    else if(az<0.0f)
        {
        az += 2.0f*3.1415926535897932384626433832795f;
        }
    
    // compute x,y
    x=GetXPos() + (distance*sin(az));
    y=GetYPos() + (distance*cos(az));
    
    // done
    return;
} // end GetPointRelative

float Actor::DAOCHeadingToDegrees(unsigned short heading)
{
    float val=float(heading)*(360.0f/4096.0f);
    //                        conv to deg

    // all daoc headings are off 180° because +y axis is SOUTH
    val+=180.0f;
    val=fmod(val,360.0f);
    return(val);
    
} // Actor::DAOCHeadingToDegrees

float Actor::DAOCHeadingToRadians(unsigned short heading)
{
    float val=float(heading)*(360.0f/4096.0f) * (3.1415926535897932384626433832795f/180.0f);
    //                        conv to deg        convert from deg to rad

    // all daoc headings are off 180° because +y axis is SOUTH
    val+=3.1415926535897932384626433832795f;
    val=fmod(val,2.0f*3.1415926535897932384626433832795f);
    return(val);
} // end Actor::DAOCHeadingToRadians

// this function returns the con of actor2 relative to actor1
Actor::RelativeCon Actor::GetRelativeCon
    (
    const unsigned char actor1,
    const unsigned char actor2
    )
{
    struct ConRangeDef
    {
        unsigned char GrayMax;
        unsigned char GreenMax;
        unsigned char BlueMax;
        unsigned char YellowMax;
        unsigned char OrangeMax;
        unsigned char RedMax;
        unsigned char PupleMax;
    };
    static const ConRangeDef ConRanges[]=
    {
    /*
        Gray
        Green
            Blue
                Yellow
                    Orange
                        Red
    */
    { 0,  0,  0,  0,  0,  0}, // level 0
    { 0,  0,  0,  1,  2,  3}, // level 1
    { 0,  0,  1,  2,  3,  4}, // level 2
    { 0,  1,  2,  3,  4,  5}, // you get the idea
    { 1,  2,  3,  4,  5,  6},
    { 2,  3,  4,  5,  6,  7},
    { 3,  4,  5,  6,  7,  8},
    { 4,  5,  6,  7,  8,  9},
    { 5,  6,  7,  8,  9, 10},
    { 6,  7,  8,  9, 10, 11},
    { 6,  7,  9, 10, 11, 13},
    { 6,  7,  9, 11, 13, 15},
    { 6,  8, 10, 12, 14, 16},
    { 7,  9, 11, 13, 15, 17},
    { 8, 10, 12, 14, 16, 18},
    { 9, 11, 13, 15, 17, 19},
    {10, 12, 14, 16, 18, 20},
    {11, 13, 15, 17, 19, 21},
    {12, 14, 16, 18, 20, 22},
    {13, 15, 17, 19, 21, 23},
    {13, 15, 18, 20, 22, 25},
    {13, 15, 18, 21, 24, 27},
    {13, 16, 19, 22, 25, 28},
    {14, 17, 20, 23, 26, 29},
    {15, 18, 21, 24, 27, 30},
    {16, 19, 22, 25, 28, 31},
    {17, 20, 23, 26, 29, 32},
    {18, 21, 24, 27, 30, 33},
    {19, 22, 25, 28, 31, 34},
    {20, 23, 26, 29, 32, 35},
    {21, 24, 27, 30, 33, 36},
    {22, 25, 28, 31, 34, 37},
    {23, 26, 29, 32, 35, 38},
    {24, 27, 30, 33, 36, 39},
    {25, 28, 31, 34, 37, 40},
    {25, 28, 31, 35, 39, 42},
    {25, 28, 31, 36, 41, 45},
    {25, 29, 32, 37, 42, 47},
    {25, 29, 33, 38, 43, 48},
    {25, 29, 34, 39, 44, 49},
    {25, 30, 35, 40, 45, 50},
    {26, 31, 36, 41, 46, 51},
    {27, 32, 37, 42, 47, 52},
    {28, 33, 38, 43, 48, 53},
    {29, 34, 39, 44, 49, 54},
    {30, 35, 40, 45, 50, 55},
    {31, 36, 41, 46, 51, 56},
    {32, 37, 42, 47, 52, 57},
    {33, 38, 43, 48, 53, 58},
    {34, 39, 44, 49, 54, 59},
    {35, 40, 45, 50, 55, 60} 
    };

    if(actor2 > ConRanges[actor1].RedMax)
        {
        // its purple
        return(Actor::Purple);
        }
    else if(actor2 > ConRanges[actor1].OrangeMax)
        {
        // its red
        return(Actor::Red);
        }
    else if(actor2 > ConRanges[actor1].YellowMax)
        {
        // its orange
        return(Actor::Orange);
        }
    else if(actor2 > ConRanges[actor1].BlueMax)
        {
        // its yellow
        return(Actor::Yellow);
        }
    else if(actor2 > ConRanges[actor1].GreenMax)
        {
        // its blue
        return(Actor::Blue);
        }
    else if(actor2 > ConRanges[actor1].GrayMax)
        {
        // its green
        return(Actor::Green);
        }
    else
        {
        return(Actor::Gray);
        }
}

void Actor::Print(std::ostream& os)const
{
    os << "[Actor::Print]\n";
    GetMotion().Print(os);
    os << "Health=" << (int)GetHealth() << "\n"
        << "Level=" << (int)GetLevel() << "\n"
        << "Realm=" << (int)GetRealm() << "\n"
        << "Id=" << GetId() << "\n"
        << "InfoId=" << GetInfoId() << "\n"
        << "Name=" << GetName() << "\n"
        << "Guild=" << GetGuild() << "\n"
        << "Surname=" << GetSurname() << "\n"
        << "LastUpdateTime=" << GetLastUpdateTime().Seconds() << "\n"
        << "TargetId=" << GetTargetId() << "\n"
        << "InRegion=" << (int)GetRegion() << "\n"
        << "ActorType=" << GetActorType() << "\n"
        << "Old=" << GetOld() << std::endl;

    // done
    return;
} // end Actor::Print
