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

// Thanks to DecentCoder for porting the Douglas-Peucker
// algorithm from Excalibur

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
#include "times.h"
#include "codeutils.h"

extern logger_t Logger;
extern MapInfo Zones;
extern std::string InitialDir;
extern CheyenneClock Clock;

// declare this here -- private static member
std::map<std::string,VectorMapColor> VectorMapItem::ColorMap;
void VectorMapItem::BuildColorMap(void)
{
    // this is a bad way to do what amounts to a singleton, but
    // I'm lazy ;)
    // only do once
    if(ColorMap.size()==0)
        {
        // add color map values
        ColorMap.insert(std::make_pair(std::string("black"),VectorMapColor(0.0f,0.0f,0.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("white"),VectorMapColor(1.0f,1.0f,1.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("gray"),VectorMapColor(0.6f,0.6f,0.6f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("green"),VectorMapColor(0.0f,1.0f,0.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("red"),VectorMapColor(1.0f,0.0f,0.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("blue"),VectorMapColor(0.0f,0.0f,1.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("orange"),VectorMapColor(250.0f/255.0f,226.0f/255.0f,133.0f/255.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("pink"),VectorMapColor(255.0f/255.0f,226.0f/255.0f,219.0f/255.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("magenta"),VectorMapColor(230.0f/255.0f,113.0f/255.0f,242.0f/255.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("brown"),VectorMapColor(193.0f/255.0f,183.0f/255.0f,162.0f/255.0f,1.0f)));
        ColorMap.insert(std::make_pair(std::string("yellow"),VectorMapColor(248.0f/255.0f,240.0f/255.0f,105.0f/255.0f,1.0f)));
        }
} // end VectorMapItem::BuildColorMap

void VectorMapItem::Draw(VectorMapTextRenderer& Text)const
{
    // draw

    switch(GetType())
        {
        case 'P':
        case 'I':
            glBegin(GL_POINTS);
            break;

        default:
            glBegin(GL_LINE_STRIP);
            break;
        }
    
    // get color
    std::map<std::string,VectorMapColor>::const_iterator cit;
    cit=ColorMap.find(Color);
    
    GLfloat r,g,b,a;

    if(cit==ColorMap.end())
        {
        // not defined, make black
        r=0.0f;
        g=0.0f;
        b=0.0f;
        a=1.0f;
        }
    else
        {
        // use predefined color
        r=cit->second.r;
        g=cit->second.g;
        b=cit->second.b;
        a=cit->second.a;
        }

    glColor4f(r,g,b,a);

    std::list<VectorMapTriplet>::const_iterator it;
    for(it=Triplets.begin();it!=Triplets.end();++it)
        {
        glVertex3f(it->x,it->y,1.0f);//it->z); render it flat for now
        }

    if(GetType() == 'P' || GetType() == 'I')
        {
        // end current drawing operation
        glEnd();

        // draw name too
        it=Triplets.begin();
        Text.RenderText(it->x,it->y,r,g,b,Name);
        }
    else
        {
        // end here
        glEnd();
        }
} // end VectorMapItem::Draw

VectorMapLoader::VectorMapLoader()
{
    // begin code from DecentCoder
    //DPSimpTolerance=0; // do not simplify lines by default
    // end code from DecentCoder
    DPSimpTolerance=100;
    bIsDone=false;
} // end VectorMapLoader

VectorMapLoader::~VectorMapLoader()
{
} // end ~VectorMapLoader

void VectorMapLoader::Draw(const unsigned int MapNumber,VectorMapTextRenderer& Text)const
{
    Maps[MapNumber].Draw(Text);

    // done
    return;
} // end Draw

// begin code from DecentCoder
// Calculate the distance between this point and the line represented by 
// (x1,y1,z1)->(x2,y2,z2) 
double VectorMapTriplet::distLine
    (
    double x1,
    double y1,
    double z1, 
    double x2,
    double y2,
    double z2)const
{ 
    double u; 
    double ddist; 
    double xm, ym, zm; 
    double dist; 
    double tdist; 

    ddist=dist3d(x1,y1,z1,x2,y2,z2); 

    if (ddist <= 0.0)
        {
        return dist3d(x1,y1,z1); 
        } 

    u = (x-x1)*(x2-x1)+(y-y1)*(y2-y1)+(z-z1)*(z2-z1); 
    u = u / (ddist*ddist); 

    xm = x1 + u * (x2-x1); 
    ym = y1 + u * (y2-y1); 
    zm = z1 + u * (z2-z1); 

    dist=dist3d(xm,ym,zm); 
    tdist = dist3d(x1,y1,z1); 
    if (tdist < dist) 
        {
        dist=tdist; 
        }
    
    tdist = dist3d(x2,y2,z2); 
    if (tdist < dist) 
        {
        dist=tdist; 
        }
    return dist; 
} // end distLine

double VectorMapTriplet::distLine(VectorMapTriplet *a,VectorMapTriplet *b)const
{ 
    return distLine(a->x,a->y,a->z,b->x,b->y,b->z); 
} // end distLine (triplet form)

// calculate the distance between 2 points in 3d 
double VectorMapTriplet::dist3d
    (
    double x1,
    double y1,
    double z1,
    double x2,
    double y2,
    double z2
    )
{ 
    double xd,yd,zd; 
    xd=x1-x2; 
    yd=y1-y2; 
    zd=z1-z2; 
    return sqrt(xd*xd+yd*yd+zd*zd); 
} // end dist3d

double VectorMapTriplet::dist3d(double x1, double y1, double z1)const
{ 
    return dist3d(x,y,z,x1,y1,z1); 
} // end dist3d

// Implementation of the Douglas-Peucker line simplification algorithm lifted 
// from Excalibur, modified for Cheyenne 
void VectorMapItem::Simplify(double sigma) 
{ 
    if(Triplets.size() < 3)
        {
        return; 
        }

    VectorMapTriplet p1 = Triplets.front(); 
    VectorMapTriplet p2 = Triplets.back(); 
    std::list<VectorMapTriplet>::iterator j, bestj; 

    double bdist = -1.0; 
    for(j=Triplets.begin();j!=Triplets.end();j++)
        { 
        const double dist=j->distLine(&p1,&p2); 
        if (dist > bdist)
            { 
            bestj=j; 
            bdist=dist; 
            }
        } // end for all triplets

    if(bdist > sigma)
        { 
        VectorMapItem vm1, vm2; 
        for(j = Triplets.begin(); j != bestj; j++)
            { 
            vm1.AddTriplet(*j); 
            } 
            
        for(j = bestj; j != Triplets.end(); j++)
            { 
            vm2.AddTriplet(*j); 
            } 
            
        vm1.Simplify(sigma); 
        vm2.Simplify(sigma); 

        Triplets.erase(Triplets.begin(), Triplets.end()); 

        for (j = vm1.Triplets.begin(); j != vm1.Triplets.end(); j++) 
            {
            AddTriplet(*j); 
            }
        for (j = vm2.Triplets.begin(); j != vm2.Triplets.end(); j++) 
            {
            AddTriplet(*j); 
            }
        } 
    else 
        { 
        Triplets.erase(Triplets.begin(), Triplets.end()); 
        AddTriplet(p1); 
        AddTriplet(p2); 
        } 
} // end Simplify
// end code from DecentCoder

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

        //LOG_FUNC << "map " << line_name << " before simplification has " << vmi.Size() << " triplets\n";
        //CheyenneTime Start(::Clock.Current());

        // begin code from DecentCoder
        // if we have a tolerance factor, apply the Douglas-Peucker line 
        // simplification algorithm. 
        if (DPSimpTolerance > 0)
            {
            vmi.Simplify(DPSimpTolerance); 
            }
        // end code from DecentCoder

        //CheyenneTime Elapsed(Start-::Clock.Current());
        //LOG_FUNC << "map " << line_name << " after simplification has " << vmi.Size() << " triplets\n";
        //LOG_FUNC << "simplification took " << Elapsed.Seconds() << " seconds\n";

        // add to vector map
        Maps[map_cnt].AddItem(vmi);
        } // end while file has data

    // done
    return(true);
} // end LoadMap