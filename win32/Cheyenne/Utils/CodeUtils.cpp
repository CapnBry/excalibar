#include "codeutils.h"
// grab these for console output redirection
// this makes std::cout (and other stuff) work for GUI applications :)
// we have to define NOMINMAX so that the stupid windows header files do 
// not make macros out of min and max :-/
#define NOMINMAX
#include <winsock2.h>
#include <crtdbg.h> // for assertions
#include <commctrl.h> // for common control defs
#include <io.h>
#include <fcntl.h>
#include <conio.h>
#include <stdio.h>
#include <sstream> // for stringstream defs
// end console output redirection includes

void AttachConsole(void)
{
    // code copied from Microsoft Knowledge Base Article - 105305
    // and the silly article had an error in it (!)
    int hCrt;
    FILE *hf;

    AllocConsole();
    hCrt = _open_osfhandle((long) GetStdHandle(STD_OUTPUT_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "w" );
    *stdout = *hf;
    setvbuf( stdout, NULL, _IONBF, 0 );

    // do it again for stdin
    hCrt = _open_osfhandle((long) GetStdHandle(STD_INPUT_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "r" );
    *stdin = *hf;
    setvbuf( stdin, NULL, _IONBF, 0 );

    // and again for stderr
    hCrt = _open_osfhandle((long) GetStdHandle(STD_ERROR_HANDLE),_O_TEXT);
    hf = _fdopen( hCrt, "w" );
    *stderr = *hf;
    setvbuf( stderr, NULL, _IONBF, 0 );

    // done
    return;
} // end AtttachConsole

void DetachConsole(void)
{
    // free the console
    FreeConsole();

    // done
    return;
} // end DetatchConsole

// functions for string manipulation
std::string AppendFileToPath(const std::string& path,const std::string& file)
{
    std::ostringstream oss;
    oss << path << file;
    return(oss.str());
} // end AppendFileToPath

std::ostream& operator<< (std::ostream& str,const struct in_addr& a)
{
    str << unsigned int(a.S_un.S_un_b.s_b1) << "."
        << unsigned int(a.S_un.S_un_b.s_b2) << "."
        << unsigned int(a.S_un.S_un_b.s_b3) << "."
        << unsigned int(a.S_un.S_un_b.s_b4);
        
    return(str);
} // end operator<< (std::ostream& str,const struct in_addr& a)
std::ostream& operator<< (std::ostream& str,const SOCKADDR_IN& a)
{
    str << a.sin_addr << ":" << ntohs(a.sin_port);
    return(str);
} // end operator<< (std::ostream& str,const SOCKADDR_IN& a)

void SET_CHECK_BOOL(HWND hwnd,UINT control,bool bool_value)
{
    SendDlgItemMessage(hwnd,control,BM_SETCHECK,(LPARAM)(bool_value ? BST_CHECKED:BST_UNCHECKED),0);
}

bool GET_CHECK_BOOL(HWND hwnd,UINT control)
{
    return(SendDlgItemMessage(hwnd,control,BM_GETCHECK,0,0)==BST_CHECKED?true:false);
}

void SET_EDIT_STRING(HWND hwnd,UINT control,const std::string& std_str)
{
    SendDlgItemMessage(hwnd,control,WM_SETTEXT,0,(LPARAM)(std_str.c_str()));
}

void GET_EDIT_STRING(HWND hwnd,UINT control,std::string& std_str) 
{
    char* edit_str;
    LRESULT len=1+SendDlgItemMessage(hwnd,control,WM_GETTEXTLENGTH,0,0);
    edit_str=new char[len];
    edit_str[0]='\0';
    SendDlgItemMessage(hwnd,control,WM_GETTEXT,len,(WPARAM)(edit_str));
    std_str=edit_str;
    delete[] edit_str;
}

void GET_LISTVIEW_SELECTED_ITEMS(HWND hwnd,UINT control,std::list<std::string>& std_list)
{
    // get selected items
    LRESULT res=-1;
    LVITEM item;
    ZeroMemory(&item,sizeof(item));
    item.mask=LVIF_TEXT;

    // get first item
    res=ListView_GetNextItem
        (
        GetDlgItem(hwnd,control),
        res,
        MAKELPARAM ((UINT)LVNI_SELECTED,0)
        );

    while(res != -1)
        {
        char text[257];

        // res is the ndx of a selected item, get it's text
        ListView_GetItemText
            (
            GetDlgItem(hwnd,control),
            res,
            0,
            &text[0],
            256
            );

        // add to list
        std_list.insert(std_list.end(),text);
        
        // go to next item
        res=ListView_GetNextItem
            (
            GetDlgItem(hwnd,control),
            res,
            MAKELPARAM ((UINT)LVNI_SELECTED,0)
            );
        } // end while result != -1
} // end GET_LISTVIEW_SELECTED_ITEMS

void WM_SIZING_LIMIT
    (
    const WPARAM WM_SIZING_WPARAM,
    const LPARAM WM_SIZING_LPARAM,
    const LONG MinWidth,
    const LONG MinHeight
    )
{
    // sanity checks
    //_ASSERTE(MinWidth <= MaxWidth);
    //_ASSERTE(MinHeight <= MaxHeight);
    
    // get rect pointer
    RECT* const pr=reinterpret_cast<RECT* const>(WM_SIZING_LPARAM);
    
    // save current width and height
    const LONG w=pr->right-pr->left;
    const LONG h=pr->bottom-pr->top;
    
    switch(WM_SIZING_WPARAM)
        {
        case WMSZ_BOTTOM:
            if(h < MinHeight)
                {
                pr->bottom=pr->top+MinHeight;
                }
            break;
            
        case WMSZ_TOP:
            if(h < MinHeight)
                {
                pr->top=pr->bottom-MinHeight;
                }
            break;
            
        case WMSZ_LEFT:
            if(w < MinWidth)
                {
                pr->left=pr->right-MinWidth;
                }
            break;
            
        case WMSZ_RIGHT:
            if(w < MinWidth)
                {
                pr->right=pr->left+MinWidth;
                }
            break;
            
        case WMSZ_BOTTOMLEFT:
            if(h < MinHeight)
                {
                pr->bottom=pr->top+MinHeight;
                }
            if(w < MinWidth)
                {
                pr->left=pr->right-MinWidth;
                }
            break;
            
        case WMSZ_BOTTOMRIGHT:
            if(h < MinHeight)
                {
                pr->bottom=pr->top+MinHeight;
                }
            if(w < MinWidth)
                {
                pr->right=pr->left+MinWidth;
                }
            break;
            
        case WMSZ_TOPLEFT:
            if(h < MinHeight)
                {
                pr->top=pr->bottom-MinHeight;
                }
            if(w < MinWidth)
                {
                pr->left=pr->right-MinWidth;
                }
            break;
            
        case WMSZ_TOPRIGHT:
            if(h < MinHeight)
                {
                pr->top=pr->bottom-MinHeight;
                }
            if(w < MinWidth)
                {
                pr->right=pr->left+MinWidth;
                }
            break;
        }
    
    // done
    return;
} // end WM_SIZING_LIMIT

