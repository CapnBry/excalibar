#include "global.h"
#include "sharenet.h"

ShareNet::ShareNet()
{
    State=ShareNet::NotInitialized;
    MyIdOffset=0;
    Socket=INVALID_SOCKET;
    ZeroMemory(&RemoteAddr,sizeof(RemoteAddr));

    WSADATA wsa;

    // startup windows sockets
    WSAStartup(MAKEWORD(2,0),&wsa);

} // end ShareNet

ShareNet::~ShareNet()
{
    // make sure network is shut down
    ShutdownNet();

    // cleanup winsock
    WSACleanup();
} // end ~ShareNet

DWORD ShareNet::Run(const bool& bContinue)
{
    while(bContinue)
        {
        switch(State)
            {
            case ShareNet::NotInitialized:
                Sleep(100);
                break;
            case ShareNet::Initialized:
                Sleep(100);
                break;
            case ShareNet::Connecting:
                Sleep(100);
                break;
            case ShareNet::Connected:
                Sleep(100);
                break;
            case ShareNet::Disconnecting:
                Sleep(100);
                break;
            case ShareNet::Disconnected:
                Sleep(100);
                break;
            default:
                Sleep(100);
                break;
            } // end switch state
        } // end forever

    // shutdown net
    ShutdownNet();

    // done
    return(0);
} // end Run

void ShareNet::InitializeNet(const char* remote_machine, const unsigned short& remote_port)
{
    AutoLock al(ShareMutex);

    if(State != ShareNet::NotInitialized)
        {
        // must shutdown before reinitializing
        return;
        }

    unsigned long addr=inet_addr(remote_machine);

    if(addr == INADDR_NONE)
        {
        // not an IP address
        HOSTENT* host=gethostbyname(remote_machine);

        if(!host)
            {
            return;
            }

        addr=*((unsigned long*)host->h_addr_list[0]);

        if(addr == INADDR_NONE)
            {
            // still couldn't find the host
            return;
            }
        }

    // save address
    RemoteAddr.sin_family=AF_INET;
    RemoteAddr.sin_port=htons(remote_port);
    RemoteAddr.sin_addr.S_un.S_addr=addr;

    // set state
    State=ShareNet::Initialized;

    // done
    return;
} // end InitializeNet

void ShareNet::ShutdownNet(void)
{
    AutoLock al(ShareMutex);

    // if socked is open, close it
    if(Socket != INVALID_SOCKET)
        {
        shutdown(Socket,SD_BOTH);
        // sleep a moment
        Sleep(100);
        closesocket(Socket);
        Socket=INVALID_SOCKET;
        }

    // set state
    State=ShareNet::NotInitialized;

    // done
    return;
} // end ShutdownNet
