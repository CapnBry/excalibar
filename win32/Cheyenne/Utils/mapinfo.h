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
#pragma once

#include <string>
#include <list>

class MapInfo
{
public:
    typedef unsigned short ZoneIndexType;
    typedef unsigned char RegionIndexType;
    const ZoneIndexType MaxZone;
    const RegionIndexType MaxRegion;
    
    MapInfo();
    virtual ~MapInfo();

    struct ZoneInfo
        {
        ZoneInfo()
        {
            Region=0;
            BaseX=0;
            BaseY=0;
            MaxX=0;
            MaxY=0;
            ZoneType=0;
            ZoneFile="";
            ZoneNum=0;
            RotateRadians=0.0f;
            bValid=false;
        };

        unsigned int Region;
        unsigned int BaseX;
        unsigned int BaseY;
        unsigned int MaxX;
        unsigned int MaxY;
        unsigned int ZoneType;
        std::string ZoneFile;
        unsigned int ZoneNum;
        float RotateRadians;
        bool bValid;
        ZoneInfo& operator=(const ZoneInfo& s)
            {
            Region=s.Region;
            BaseX=s.BaseX;
            BaseY=s.BaseY;
            MaxX=s.MaxX;
            MaxY=s.MaxY;
            ZoneType=s.ZoneType;
            ZoneFile=s.ZoneFile;
            ZoneNum=s.ZoneNum;
            RotateRadians=s.RotateRadians;
            bValid=s.bValid;
            return(*this);
            }
        }; // end struct ZoneInfo

    struct RegionLimits
        {
        RegionLimits()
        {
            XMin=0;
            XMax=0;
            YMin=0;
            YMax=0;
            ZMin=0;
            ZMax=0;
            XOffset=0;
            YOffset=0;
            Region=0;
        };

        unsigned int XMin;
        unsigned int XMax;
        unsigned int YMin;
        unsigned int YMax;
        unsigned int ZMin;
        unsigned int ZMax;
        int XOffset; // the offsets are used for display purposes
        int YOffset; // the offsets are used for display purposes
        RegionIndexType Region;
        }; // end struct RegionLimits

    const MapInfo::ZoneInfo& GetZoneFromGlobal
        (
        const RegionIndexType region,
        const unsigned int global_x,
        const unsigned int global_y,
        const unsigned int global_z,
        unsigned int& x,
        unsigned int& y,
        unsigned short& z,
        ZoneIndexType& zone
        );

    void GetGlobalFromZone
        (
        const ZoneIndexType zone,
        const unsigned short x,
        const unsigned short y,
        const unsigned short z,
        unsigned int& global_x,
        unsigned int& global_y,
        unsigned short& global_z
        )const;

    const RegionLimits& GetLimitsFromRegion
        (
        const RegionIndexType region
        )const
    {
        return(RegionLimit[region]);
    }

    const MapInfo::ZoneInfo& GetZone(const ZoneIndexType zone)const
    {
        return(Zone[zone]);
    } // end GetZone

    bool ReadZoneFile(void);
    bool ReadRegionOffsets(void);
    void OffsetRegion
        (
        const RegionIndexType region,
        const int x_increment,
        const int y_increment
        );
    bool SaveRegionOffsets(void)const;

protected:

private:

    ZoneInfo Zone[65535];
    typedef std::list<RegionIndexType> ZonesByRegionType;
    typedef ZonesByRegionType::iterator ZonesByRegionIterator;
    typedef ZonesByRegionType::value_type ZonesByRegionValueType;
    ZonesByRegionType ZonesByRegion[256];
    RegionLimits RegionLimit[256];

}; // end class MapInfo
