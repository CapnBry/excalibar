#include "pcapserver.h"
#include <vector>
#include "segmentsearcher.h"

void TestSegSearch(const char* cmd_line)
{
    static const unsigned char needle[] =//hauptschleife die den key generiert
        {
        0x0F, 0xB6, 0xC3,	//movzx eax, bl
        0x03, 0xFA,			//add edi, edx
        0x03, 0xC7,			//add eax, edi
        0x8B, 0xFE,			//mov edi, esi
        0x99,				//cdq
        0xF7, 0xFF,			//idiv edi
        0x41,				//inc ecx
        0x3B, 0xCE,			//cmp ecx, esi
        0x8B, 0xFA,			//mov edi, edx
        0x8D, 0x87			//lea offset
        };
    CSegmentSearch search(cmd_line);
    void* needle_address=search.FindFirst(".text",&needle[0],sizeof(needle));
    
    std::cout << "needle_address= 0x" << needle_address << "\n";
    
    // done
    return;
} // end TestSegSearch
