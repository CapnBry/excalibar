#pragma warning(disable : 4786)

#include "vectormaploader.h"
#include <sstream>
#include <memory>
#include <fstream>

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

    std::fstream map_file;
    VectorMap vm;
    std::stringstream ss;

    for(unsigned char map_cnt=0;(map_cnt<=254 && bContinue);++map_cnt)
        {
        // make it empty
        Maps[map_cnt].MakeEmpty();

        // check if this zone is valid
        if(Zones.GetZone(map_cnt).bValid)
            {
            // open the .map file
            std::stringstream filename;
            filename << "maps\\" << Zones.GetZone(map_cnt).ZoneFile << ".map";
            
            // clear mapfile status and pointers
            map_file.seekg(0);
            map_file.seekp(0);
            map_file.clear();

            map_file.open(filename.str().c_str(),std::ios::in);

            // make sure it opened
            if(!map_file.is_open() || filename.str()=="maps\\Tir_na_Nog.map")
                {
                map_file.close();
                // go to next one
                continue;
                } // end if open

            Logger << "[VectorMapLoader::Run] loading map \"" << filename.str().c_str() << "\"\n"; 
            
            // get first ("title") line
            char ch='\0';
            ss.str("");
            ss.seekg(0);
            ss.seekp(0);

            while(ch!=',' && !map_file.eof() && map_file.good())
                {
                // get next char
                map_file >> ch;

                if(ch==',')
                    {
                    // done with title line
                    break;
                    }
                else
                    {
                    // valid character
                    ss << ch;
                    }
                } // end while (for title line)

            // set name
            Maps[map_cnt].SetName(ss.str());

            // ignore rest of line
            map_file.read(&ch,1);
            while(ch!=0x0D && ch!=0x0A && bContinue)
                {
                if(map_file.eof() || !map_file.good())
                    {
                    // end of stream
                    break;
                    }

                map_file.read(&ch,1);
                }

            map_file >> std::ws;
            
            // reset stringstream
            ss.str("");
            ss.seekp(0);
            ss.seekg(0);

            // get the data from the next line
            while(!map_file.eof() && map_file.good() && bContinue)
                {
                // get next char -- don't skip the eol indicator
                // because we use that
                map_file.read(&ch,1);

                if(ch==0x0D || ch==0x0A)
                    {
                    // end of line, skip remaining white space
                    map_file >> std::ws;

                    // seek to beginning
                    ss.seekg(0);
                    ss.seekp(0);

                    // process the line
                    VectorMapItem vmi;
                    char temp;
                    std::stringstream curr;

                    // get type
                    char Type;
                    ss >> Type;

                    // get name
                    std::string Name;
                    ss.ignore(1);
                    while((temp=ss.get()) != ',')
                        {
                        if(ss.eof() || !ss.good())
                            {
                            // end of stream
                            break;
                            }

                        curr << temp;
                        }
                    Name=curr.str();
                    // reset current
                    curr.str("");
                    curr.seekp(0);
                    curr.seekg(0);
                    curr.clear();

                    // get color
                    std::string Color;
                    while((temp=ss.get()) != ',')
                        {
                        if(ss.eof() || !ss.good())
                            {
                            // end of stream
                            break;
                            }

                        curr << temp;
                        }
                    Color=curr.str();
                    // reset current
                    curr.str("");
                    curr.seekp(0);
                    curr.seekg(0);
                    curr.clear();

                    // get number of triplets
                    unsigned int NumTriplets;
                    if(Type != 'P')
                        {
                        while((temp=ss.get()) != ',')
                            {
                            if(ss.eof() || !ss.good())
                                {
                                // end of stream
                                break;
                                }

                            curr << temp;
                            }
                        curr >> NumTriplets;
                        // reset current
                        curr.str("");
                        curr.seekp(0);
                        curr.seekg(0);
                        curr.clear();
                        }
                    else
                        {
                        // just get 1 set of triplets for a point
                        NumTriplets=1;
                        }

                    for(unsigned int count=0;count<NumTriplets;++count)
                        {
                        // get a triplet
                        float x;
                        while((temp=ss.get()) != ',')
                            {
                            if(ss.eof() || !ss.good())
                                {
                                // end of stream
                                break;
                                }

                            curr << temp;
                            }
                        curr >> x;
                        // reset current
                        curr.str("");
                        curr.seekp(0);
                        curr.seekg(0);
                        curr.clear();

                        float y;
                        while((temp=ss.get()) != ',')
                            {
                            if(ss.eof() || !ss.good())
                                {
                                // end of stream
                                break;
                                }

                            curr << temp;
                            }
                        curr >> y;
                        // reset current
                        curr.str("");
                        curr.seekp(0);
                        curr.seekg(0);
                        curr.clear();

                        float z;
                        while((temp=ss.get()) != ',')
                            {
                            if(ss.eof() || !ss.good())
                                {
                                // end of stream
                                break;
                                }

                            curr << temp;
                            }
                        curr >> z;
                        // reset current
                        curr.str("");
                        curr.seekp(0);
                        curr.seekg(0);
                        curr.clear();

                        // add triplet to list
                        vmi.AddTriplet(VectorMapTriplet(x,y,z));
                        } // end for each triplet

                    // finish populating this map item
                    vmi.SetName(Name);
                    vmi.SetType(Type);
                    vmi.SetColor(Color);

                    // add to vector map
                    Maps[map_cnt].AddItem(vmi);

                    // reset string stream
                    ss.str("");
                    ss.seekg(0);
                    ss.seekp(0);
                    ss.clear();
                    } // end if eol detected
                else
                    {
                    // valid character
                    ss << ch;
                    } // end else valid character
                } // end while there is data left in the file

            // close this file
            map_file.close();

            } // end if zone is valid
        } // end for all zones

    // flag we are done
    bIsDone=true;
    
    // done
    return(0);
} // end Run
