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
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#include <sstream>
#include <fstream>
#include <limits>
#include "vectormaploader.h"
#include "mapinfo.h"
#include "logger.h"

extern logger_t Logger;
extern MapInfo Zones;
extern std::string InitialDir;

void VectorMapItem::BuildColorMap(void)
{
    // add color map values
    ColorMap.insert(std::make_pair(std::string("black"),VectorMapColor(0.0f,0.0f,0.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("white"),VectorMapColor(1.0f,1.0f,1.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("gray"),VectorMapColor(0.5f,0.5f,0.5f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("green"),VectorMapColor(0.0f,1.0f,0.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("red"),VectorMapColor(1.0f,0.0f,0.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("blue"),VectorMapColor(0.0f,0.0f,1.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("orange"),VectorMapColor(250.0f/255.0f,226.0f/255.0f,133.0f/255.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("pink"),VectorMapColor(255.0f/255.0f,226.0f/255.0f,219.0f/255.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("magenta"),VectorMapColor(230.0f/255.0f,113.0f/255.0f,242.0f/255.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("brown"),VectorMapColor(193.0f/255.0f,183.0f/255.0f,162.0f/255.0f,1.0f)));
    ColorMap.insert(std::make_pair(std::string("yellow"),VectorMapColor(248.0f/255.0f,240.0f/255.0f,105.0f/255.0f,1.0f)));
} // end VectorMapItem::BuildColorMap

void VectorMapItem::Draw(void)const
{
    // draw

    switch(GetType())
        {
        case 'P':
            glBegin(GL_POINTS);
            break;

        default:
            glBegin(GL_LINE_STRIP);
            break;
        }
    
    // get color
    std::map<std::string,VectorMapColor>::const_iterator cit;
    cit=ColorMap.find(Color);

    if(cit==ColorMap.end())
        {
        // not defined, make black
        glColor4f(0.0f,0.0f,0.0f,1.0f);
        }
    else
        {
        // use predefined color
        glColor4f(cit->second.r,cit->second.g,cit->second.b,cit->second.a);
        }

    std::list<VectorMapTriplet>::const_iterator it;
    for(it=Triplets.begin();it!=Triplets.end();++it)
        {
        glVertex3f(it->x,it->y,1.0f);//it->z); render it flat for now

        /*
        if(GetType() == 'P')
            {
            // draw name too
            glRasterPos3f(it->x+10.0f,it->y,1.0f);
            glColor4f(1.0f,1.0f,1.0f,1.0f);
            DrawGLFontString(Name);

            }
        */
        }

    if(GetType() == 'P' || GetType() == 'I')
        {
        // end current drawing operation
        glEnd();

        // draw name too
        it=Triplets.begin();
        glRasterPos3f(it->x+10.0f,it->y,1.0f);
        glColor4f(1.0f,1.0f,1.0f,1.0f);
        // this needs to be uncommented later
        // removed for lib port, sf_devel.
        //DrawGLFontString(Name);
        }
    else
        {
        // end here
        glEnd();
        }
} // end VectorMapItem::Draw

VectorMapLoader::VectorMapLoader()
{
    bIsDone=false;
    ListBase=0;
} // end VectorMapLoader

VectorMapLoader::~VectorMapLoader()
{
} // end ~VectorMapLoader

bool VectorMapLoader::MakeDisplayLists(void)
{
    // make display lists here

    if(!IsDone())
        {
        return(false);
        }

    for(unsigned int u=0;u<256;++u)
        {
        Maps[u].SetListNum(ListBase+u);
        Maps[u].MakeDisplayList();
        }

    return(true);
} // end MakeDisplayLists

void VectorMapLoader::Draw(const unsigned char MapNumber)const
{
    Maps[MapNumber].Draw();

    // done
    return;
} // end Draw

DWORD VectorMapLoader::Run(const bool& bContinue)
{
    // load maps here

    // reduce priority
    SetThreadPriority(GetCurrentThread(),THREAD_PRIORITY_IDLE);

    //std::fstream map_file;
    //VectorMap vm;
    
    for(unsigned char map_cnt=0;(map_cnt<=254 && bContinue);++map_cnt)
        {
        // make it empty
        Maps[map_cnt].MakeEmpty();

        // check if this zone is valid
        if(Zones.GetZone(map_cnt).bValid)
            {
            // open the .map file
            std::stringstream filename;
            filename << ::InitialDir << "maps\\" << Zones.GetZone(map_cnt).ZoneFile;
            
            // load this map
            LoadMap(filename.str(),map_cnt);
            } // end if zone is valid
        } // end for all zones

    // flag we are done
    bIsDone=true;
    
    // done
    return(0);
} // end Run

bool VectorMapLoader::LoadMap(const std::string& filename,const unsigned char map_cnt)
{
    // open the file
    std::ifstream file(filename.c_str());
    
    if(!file.is_open())
        {
        Logger << "[VectorMapLoader::LoadMap] map file \"" << filename.c_str() << "\" is not present\n";
        return(false);
        }
    
    Logger << "[VectorMapLoader::LoadMap] loading map file \"" << filename.c_str() << "\"\n";

    // first line, to first comma, is the map name
    std::string map_name;
    std::getline(file,map_name,',');
    
    // skip rest of first line after first comma
    file.ignore(std::ifstream::int_type(std::numeric_limits<int>::max),'\n');
    // and skip whitespace
    file >> std::ws;
    
    // the item we are about to load from the file
    VectorMapItem vmi;
    
    while(file.good() && !file.eof())
        {
        // erase it
        vmi.MakeEmpty();
        
        // get type
        char Type;
        file >> Type;
        // skip comma
        file.ignore(1,'\n');
        
        // get name
        std::string line_name;
        std::getline(file,line_name,',');
        
        // get color
        std::string line_color;
        std::getline(file,line_color,',');
        
        int num_triplets;
        if(Type == 'P' || Type == 'I')
            {
            // this is a point, get a single triplet
            num_triplets=1;
            }
        else
            {
            // this is a line,
            // get number of triplets
            file >> num_triplets;
            
            // ignore comma
            file.ignore(1,'\n');
            }
        
        // do all but the last one (this is for comma vs. eol handling)
        for(int i=0;i<num_triplets-1;++i)
            {
            // get x
            float x;
            file >> x >> std::ws;
            // ignore comma
            file.ignore(1,'\n');
            
            // get y
            float y;
            file >> y >> std::ws;
            // ignore comma
            file.ignore(1,'\n');
            
            // get z
            float z;
            file >> z >> std::ws;
            // ignore comma
            file.ignore(1,'\n');
            
            // add triplet to vmi's list
            vmi.AddTriplet(VectorMapTriplet(x,y,z));
            
            //::Logger << "[VectorMapLoader::LoadMap] triplet: <" << x << "," << y << "," << z << ">\n";
            } // end for all triplets
        
        // now do the last one
        // get x
        float x;
        file >> x >> std::ws;
        // ignore comma
        file.ignore(1,'\n');
        
        // get y
        float y;
        file >> y >> std::ws;
        // ignore comma
        file.ignore(1,'\n');
        
        // get z
        float z;
        file >> z >> std::ws;
        
        // add triplet to vmi's list
        vmi.AddTriplet(VectorMapTriplet(x,y,z));
        
        // finish populating this map item
        vmi.SetName(line_name);
        vmi.SetType(Type);
        vmi.SetColor(line_color);

        // add to vector map
        Maps[map_cnt].AddItem(vmi);
        } // end while file has data

    // done
    return(true);
} // end LoadMap