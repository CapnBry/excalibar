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
#include "logger.h"
#include "mapinfo.h"
#include <iosfwd>
#include <istream>
#include <fstream>
#include <algorithm>

extern logger_t Logger;

MapInfo::MapInfo()
{
    int i;

    // empty arrays
    for(i=0;i<256;++i)
        {
        Zone[i].Region=0;
        Zone[i].BaseX=0;
        Zone[i].BaseY=0;
        Zone[i].MaxX=0;
        Zone[i].MaxY=0;
        Zone[i].ZoneType=0;
        Zone[i].ZoneFile="";
        Zone[i].ZoneNum=0;
        Zone[i].RotateRadians=0.0f;

        RegionLimit[i].XMin=-1;
        RegionLimit[i].XMax=0;
        RegionLimit[i].YMin=-1;
        RegionLimit[i].YMax=0;
        RegionLimit[i].ZMin=0; // set this to 0 because mapinfo.txt does not have z info in it
        RegionLimit[i].ZMax=0;
        }

} // end MapInfo

MapInfo::~MapInfo()
{
} // end ~MapInfo

const MapInfo::ZoneInfo& MapInfo::GetZoneFromGlobal
    (
    const unsigned char region,
    const unsigned int global_x,
    const unsigned int global_y,
    const unsigned int global_z,
    unsigned int& x,
    unsigned int& y,
    unsigned short& z,
    unsigned char& zone
    )
{
    // init to 0
    zone=0;
    x=0;
    y=0;
    z=0;

    // get list of zones that are candidates
    for(ZonesByRegionIterator it=ZonesByRegion[region].begin();it!=ZonesByRegion[region].end();++it)
        {
        // find the right one
        if(global_x >= Zone[*it].BaseX && global_x < Zone[*it].MaxX)
            {
            if(global_y >= Zone[*it].BaseY && global_y < Zone[*it].MaxY)
                {
                // found it!

                // save info
                zone=*it;
                x=global_x - Zone[*it].BaseX;
                y=global_y - Zone[*it].BaseY;
                z=global_z;

                break;
                } // end if match y
            } // end if match x
        } // end for each zone in the candidate list

    // done
    return(Zone[zone]);
} // end GetZoneFromLocal

void MapInfo::GetGlobalFromZone
    (
    const unsigned char zone,
    const unsigned short x,
    const unsigned short y,
    const unsigned short z,
    unsigned int& global_x,
    unsigned int& global_y,
    unsigned short& global_z
    )const
{
    // regions are not unique, zones are.
    global_x=Zone[zone].BaseX + x;
    global_y=Zone[zone].BaseY + y;
    global_z=z;

    // done
    return;
} // end GetGlobalFromZone

bool MapInfo::ReadZoneFile(void)
{
    Logger << "[MapInfo::ReadZoneFile] reading zone file\n";

    // open file
    std::fstream mapinfo("mapinfo.txt",std::ios::in);

    // sanity check
    if(!mapinfo.is_open())
        {
        return(false);
        }

    ZoneInfo cur;

    // format is:
    // region basex basey - maxx maxy type filename zone# rotation_degrees

    // region and zone are easily misunderstood: region change occurs when
    // you get the loading screen, zone change occurs when you get a message
    // saying something like "you have entered West Svealand".
    
    // I have confused this throughout Cheyenne! At this point, Cheyenne does not
    // care about zones, it only cares about regions; so for some unknown reason,
    // I used the term zone throughout when I really meant region. Suck it up :P

    // read file
    while(!mapinfo.eof() && mapinfo.good())
        {
        mapinfo >> cur.Region;
        mapinfo >> std::hex >> cur.BaseX;
        mapinfo >> std::hex >> cur.BaseY;
        mapinfo.ignore(5); // skip the "  -  "
        mapinfo >> std::hex >> cur.MaxX;
        mapinfo >> std::hex >> cur.MaxY;
        mapinfo >> std::dec >> cur.ZoneType;
        mapinfo >> std::ws; // skip whitespace for this one
        mapinfo.width(32); // set max width for next field
        mapinfo >> cur.ZoneFile;
        mapinfo >> cur.ZoneNum;
        mapinfo >> cur.RotateRadians;

        mapinfo >> std::ws; // skip whitespace for remainder of this line

        // get rid of the whitespace in the ZoneFile
        // cur.ZoneFile.resize(cur.ZoneFile.find_first_of(' '));

        // print it
        
        /*
        Logger << "[MapInfo::ReadZoneFile] read zone:\n"
               << "Region: " << cur.Region << "\n"
               << "BaseX: " << cur.BaseX << "\n"
               << "BaseY: " << cur.BaseY << "\n"
               << "MaxX: " << cur.MaxX << "\n"
               << "MaxY: " << cur.MaxY << "\n"
               << "ZoneType: " << cur.ZoneType << "\n"
               << "ZoneFile: " << cur.ZoneFile.c_str() << "\n"
               << "ZoneNum: " << cur.ZoneNum << "\n"
               << "RotateDegrees: " << cur.RotateRadians << "\n";
        */

        // convert to radians here
        cur.RotateRadians *= 0.017453292519943295769236907684886f; // PI/180

        // set valid flag
        cur.bValid=true;

        // store in array
        Zone[cur.ZoneNum]=cur;
        } // end while file not completely read

    // add 'em to the ZonesByRegion table
    // and RegionLimit table

    for(int i=0;i<256;++i)
        {
        if(Zone[i].Region == 0)
            {
            continue;
            }

        ZonesByRegion[Zone[i].Region].insert(ZonesByRegion[Zone[i].Region].end(),unsigned char(i));
        RegionLimit[Zone[i].Region].Region=Zone[i].Region;

        if(RegionLimit[Zone[i].Region].XMin > Zone[i].BaseX)
            {
            RegionLimit[Zone[i].Region].XMin=Zone[i].BaseX;
            }

        if(RegionLimit[Zone[i].Region].XMax < Zone[i].MaxX)
            {
            RegionLimit[Zone[i].Region].XMax=Zone[i].MaxX;
            }

        if(RegionLimit[Zone[i].Region].YMin > Zone[i].BaseY)
            {
            RegionLimit[Zone[i].Region].YMin=Zone[i].BaseY;
            }

        if(RegionLimit[Zone[i].Region].YMax < Zone[i].MaxY)
            {
            RegionLimit[Zone[i].Region].YMax=Zone[i].MaxY;
            }

        // no Z values
        } // end for all zones

    
    Logger << "Region,XMin,XMax,YMin,YMax,ZMin,ZMax,Name\n";
    for(int j=0;j<256;++j)
        {
        // make sure this region has been written to
        if(RegionLimit[j].XMin != -1)
            {
            Logger << j << ","
                   << RegionLimit[j].XMin << ","
                   << RegionLimit[j].XMax << ","
                   << RegionLimit[j].YMin << ","
                   << RegionLimit[j].YMax << ","
                   << RegionLimit[j].ZMin << ","
                   << RegionLimit[j].ZMax << ","
                   << Zone[(*ZonesByRegion[RegionLimit[j].Region].begin())].ZoneFile.c_str() << "\n";
                   // the above line is a real doozy eh?
            }
        }
    
    /*
    // debug only
    #ifdef CHEYENNE_DEBUG
    // print the zone array
    for(int i=0;i<256;++i)
        {
        Logger << "[MapInfo::ReadZoneFile] zone " << i << ":\n"
               << "Region: " << Zone[i].Region << "\n"
               << "BaseX: " << Zone[i].BaseX << "\n"
               << "BaseY: " << Zone[i].BaseY << "\n"
               << "MaxX: " << Zone[i].MaxX << "\n"
               << "MaxY: " << Zone[i].MaxY << "\n"
               << "ZoneType: " << Zone[i].ZoneType << "\n"
               << "ZoneFile: " << Zone[i].ZoneFile.c_str() << "\n"
               << "ZoneNum: " << Zone[i].ZoneNum << "\n"
               << "RotateDegrees: " << Zone[i].RotateRadians << "\n";
        }

    #endif // CHEYENNE_DEBUG
    */

    // done
    return(true);
} // end ReadZoneFile

bool MapInfo::ReadRegionOffsets(void)
{
    Logger << "[MapInfo::ReadRegionOffsets] reading region offsets\n";

    // open file
    std::fstream offsetfile("regionoffsets.txt",std::ios::in);

    if(!offsetfile.is_open())
        {
        return(false);
        }

    /*
    format is:
    region x_offset y_offset
    */

    int ui_region;
    int x_offset;
    int y_offset;

    while(!offsetfile.eof() && offsetfile.good())
        {
        offsetfile >> ui_region >> std::ws;
        offsetfile >> x_offset >> std::ws;
        offsetfile >> y_offset >> std::ws;

        offsetfile >> std::ws; // skip whitespace for remainder of this line

        // store in the limits structure
        unsigned char region=unsigned char(ui_region);
        RegionLimit[region].XOffset=x_offset;
        RegionLimit[region].YOffset=y_offset;
        } // end while file not completely read

    // done
    return(true);
} // end ReadRegionOffsets
