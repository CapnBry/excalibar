/******************************************************************************
PCAPDStream: a real-time packet analyzer/sniffer for Dark Age of Camelot
Copyright (C) 2003, the PCAPDStream Developers

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
#include "DServerClient.h"

DServerClientConnection::DServerClientConnection()
{
} // end DServerClientConnection
DServerClientConnection::~DServerClientConnection()
{
    // make sure closed
    Close();
} // end ~DServerClientConnection

void DServerClientConnection::CloseSocket(void)
{
    if(!IsInUse())
        {
        // done
        return;
        }

    // close down send side
    shutdown(GoParam.s,SD_SEND);

    // close down receive side
    shutdown(GoParam.s,SD_RECEIVE);

    // close socket
    closesocket(GoParam.s);

    // mark invalid
    GoParam.s=INVALID_SOCKET;
    
    // done
    return;
} // end CloseSocket

void DServerClientConnection::Close(void)
{
    // close socket
    CloseSocket();
    
    // stop thread
    Stop();
    
    // done
    return;
} // end Disconnect

DWORD DServerClientConnection::Run(const bool& bContinue)
{
    // start read
    DoInputMaintenance();
    
    // functor to be called to implement an alertable wait
    AlertableWaitSingleFunctor awsf;

    // do forever
    while(bContinue&&IsInUse())
        {
        // wait for output data to be ready
        switch(OutputBuffer.Wait(awsf,1000))
            {
            case WAIT_OBJECT_0:
                // there is data to be sent
                DoOutputMaintenance();
                break;
                
            case WAIT_IO_COMPLETION:
            case WAIT_TIMEOUT:
            default:
                // nothing to do
                break;
            } // end switch output buffer wait status
        } // end forever
    
    // done
    return(0);
} // end Run

bool DServerClientConnection::DoInputMaintenance(void)
{
    ZeroMemory(&ReadOverlapped,sizeof(ReadOverlapped));
    // store original info: Having to do this
    // is HIDEOUS, but thats the way Microsoft wrote their
    // completion routines: no easy way to store a decent
    // "context" parameter.
    ReadOverlapped.hEvent=(HANDLE)this;
    SetLastError(ERROR_SUCCESS);
    BOOL status=ReadFileEx
        (
        HANDLE(GoParam.s),
        &ReadBuf[0],
        sizeof(ReadBuf),
        &ReadOverlapped,
        DServerClientConnection::ReadCompletionRoutine
        );

    if(status)
        {
        // done: read in progress
        return(true);
        }
    
    // done, read failed
    CloseSocket();
    
    return(false);
} // end DoInputMaintenance

bool DServerClientConnection::DoOutputMaintenance(void)
{
    // send in 2k chunks
    char buf[2048];
    
    const unsigned int size=min(sizeof(buf),OutputBuffer.Size());

    // copy the data to temp buffer
    OutputBuffer.Peek(&buf[0],size);

    // send
    unsigned int err=send(GoParam.s,&buf[0],size,0);

    if(err!=0 && err!=SOCKET_ERROR)
        {
        // we sent at least something,
        // take it off the output queue
        OutputBuffer.Extract(&buf[0],err);
        }
    
    // done, return true to indicate that there is 
    // something else to do, false to indicate
    // that the output queue is empty
    return(OutputBuffer.Size() != 0);
} // end DoOutputMaintenance

VOID CALLBACK DServerClientConnection::ReadCompletionRoutine
    (
    DWORD dwErrorCode,
    DWORD dwNumberOfBytesTransfered,
    LPOVERLAPPED lpOverlapped
    )
{
    // extract original info: Having to do this
    // is HIDEOUS, but thats the way Microsoft wrote their
    // completion routines: no easy way to store a decent
    // "context" parameter.
    DServerClientConnection* pMe=(DServerClientConnection*)lpOverlapped->hEvent;
    
    // if we get here and there's no data, assume something bad happened
    if(dwNumberOfBytesTransfered == 0)
        {
        pMe->CloseSocket();
        return;
        }
    
    // we got some data,
    // for now, ignore it

    // see if we are still running
    if(pMe->IsInUse())
        {
        // start another read
        pMe->DoInputMaintenance();
        }

    // done
    return;
} // end DServerClientConnection

bool DServerClientConnection::SendData(const void* data,const size_t num_bytes)
{
    return(OutputBuffer.Insert(data,num_bytes));
} // end SendData