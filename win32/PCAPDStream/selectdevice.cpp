#include "pcapserver.h"
#include "pcap.h"
#include <vector>
#include <string>
typedef std::pair<std::string,std::string> devlist_value_t;
typedef std::vector<devlist_value_t> devlist_t;

static char device[MAX_PATH+1];

void GET_LIST_SEL_STRING(HWND hwnd,UINT control,std::string& std_str)
{
    LRESULT sel=SendDlgItemMessage(hwnd,control,LB_GETCURSEL,0,0);
    
    if(sel!=LB_ERR)
        {
        LRESULT len=SendDlgItemMessage(hwnd,control,LB_GETTEXTLEN,(WPARAM)sel,0);
        TCHAR* text=new TCHAR[len+1];
        text[0]='\0';
        SendDlgItemMessage(hwnd,control,LB_GETTEXT,(WPARAM)sel,(LPARAM)&text[0]);
        std_str=text;
        delete[] text;
        }
    return;
} // end GET_LIST_SEL_STRING


INT_PTR CALLBACK SelectAdapterProc
    (
    HWND hWnd,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam
    )
{
    // retrieve context
    devlist_t* pdevlist=(devlist_t*)GetWindowLongPtr(hWnd,GWLP_USERDATA);
    
    switch(uMsg)
        {
        case WM_INITDIALOG:
            // save context
            SetWindowLongPtr(hWnd,GWLP_USERDATA,(LONG_PTR)lParam);
            pdevlist=(devlist_t*)lParam;
            
            // populate list
            for(devlist_t::size_type i=0;i<pdevlist->size();++i)
                {
                SendDlgItemMessage(hWnd,IDC_ADAPTERLIST,LB_ADDSTRING,0,(LPARAM)pdevlist->at(i).second.c_str());
                }
            
            SendDlgItemMessage(hWnd,IDC_ADAPTERLIST,LB_SETCURSEL,0,0);
            break;
            
        case WM_COMMAND:
            // look at control id
            switch(LOWORD(wParam))
                {
                case IDOK:
                    // find match
                    {
                    std::string desc;
                    GET_LIST_SEL_STRING(hWnd,IDC_ADAPTERLIST,desc);
                    for(devlist_t::size_type i=0;i<pdevlist->size();++i)
                        {
                        if(desc==pdevlist->at(i).second)
                            {
                            // description match, use it
                            strcpy(device,pdevlist->at(i).first.c_str());
                            }
                        }
                    }
                    // end dialog
                    EndDialog(hWnd,IDOK);
                    break;
                    
                default:
                    return(FALSE); // did not process message
                    break;
                } // end switch control ID
            break;
            
        default:
            return(FALSE); // did not process message
        } // end switch uMSg
    // return TRUE (processed message)
    return(TRUE);
} // end SelectAdapterProc

void SelectDevice(HINSTANCE hInst)
{
    device[0]='\0'; // init to emty string
    nids_params.device=device; // save for libnids
    
    /* Retrieve the device list */
    pcap_if_t *alldevs;
    char errbuf[PCAP_ERRBUF_SIZE];
    if (pcap_findalldevs(&alldevs, errbuf) == -1)
        {
        // done, use default
        return;
        }
    
    // make a std::list of of this, using pairs of strings
    devlist_t devlist;
    for(pcap_if_t *d=alldevs; d; d=d->next)
        {
        devlist.push_back(devlist_value_t(d->name,d->description));
        }
    // At this point, we don't need any more the device list. Free it.
    pcap_freealldevs(alldevs);
    
    // release only:
    // if there is only 1 device on the list, the user has nothing to choose
    // from, so return now.
    #ifndef _DEBUG
    if(devlist.size()==0)
        {
        return;
        }
    #endif
    
    // do dialog to select the adapter
    DialogBoxParam
        (
        hInst,
        MAKEINTRESOURCE(IDD_SELECTADAPTER),
        NULL,
        SelectAdapterProc,
        (LPARAM)&devlist
        );
    // done
    return;
} // end SelectDevice