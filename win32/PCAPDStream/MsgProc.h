#pragma once
namespace Network
{
template<class USER_STORAGE_T=unsigned short>class Buffer
{
public:
    // store user data (default is 16 bits of storage)
    USER_STORAGE_T UserStorage;
    

    Buffer(const size_t buf_size=0xFFFF) : BufSize(buf_size) // default initital size=64K
    {
        TheBuffer=reinterpret_cast<BYTE*>(VirtualAlloc(NULL,BufSize,MEM_COMMIT,PAGE_READWRITE));
        Used=0;
    }

    Buffer(const Buffer<USER_STORAGE_T>& s) : BufSize(s.BufSize)
    {
        TheBuffer=reinterpret_cast<BYTE*>(VirtualAlloc(NULL,BufSize,MEM_COMMIT,PAGE_READWRITE));
        Used=0;
        set(s);
    }
    ~Buffer(){VirtualFree(TheBuffer,0,MEM_RELEASE);};
    
    size_t NumFreeBytes(void)const{return(BufSize-Used);};
    size_t NumUsedBytes(void)const{return(Used);};
    size_t BufferSize(void)const{return(BufSize);};
    
    size_t AddData(const BYTE* NewData,const size_t NewDataBytes)
    {
        // check for overflow
        if(NumFreeBytes() < NewDataBytes){return(0);};
        // copy the data
        memcpy(&TheBuffer[Used],NewData,NewDataBytes);
        // increment number of bytes used
        Used+=NewDataBytes;
        // done, return number of bytes copied
        return(NewDataBytes);
    } // end AddData
    
    const BYTE* ShowTheBuffer(const size_t offset=0)const{return(&(TheBuffer[offset]));};
    BYTE* ShowTheBuffer(const size_t offset=0){return(&(TheBuffer[offset]));};
    
    size_t CopyData(BYTE* DestinationBuffer,const size_t DestinationMaxSize)const
    {
        // copy the number of bytes available or the maximum, whichever is smaller
        size_t BytesToCopy;
        if(NumUsedBytes() > DestinationMaxSize)
            {
            BytesToCopy=DestinationMaxSize;
            }
        else
            {
            BytesToCopy=NumUsedBytes();
            }
        // copy the data
        memcpy(&DestinationBuffer[0],&TheBuffer[0],BytesToCopy);
        // return amount copied
        return(BytesToCopy);
    } // end CopyData
    
    size_t RemoveData(const size_t AmountToRemove)
    {
        // if we are emptying the buffer, then take a shortcut
        const size_t UsedAtBeginning=NumUsedBytes();
        if(UsedAtBeginning <= AmountToRemove)
            {
            Used=0;
            return(UsedAtBeginning);
            }
        // remove and shift data in the buffer
        memmove(&TheBuffer[0],&TheBuffer[AmountToRemove],Used-AmountToRemove);
        // decrement number of bytes used
        Used-=AmountToRemove;
        // return amount actually removed
        return(AmountToRemove);
    } // end RemoveData
        
    Buffer<USER_STORAGE_T>& operator=(const Buffer<USER_STORAGE_T>& s)
    {
        // disallow self-assignment
        if(this!=&s){set(s);}
        // done
        return(*this);
    } // end operator=
protected:
private:
    void set(const Buffer<USER_STORAGE_T>& s)
    {
        // empty buffer
        RemoveData(NumUsedBytes());
        // copy user storage
        UserStorage=s.UserStorage;
        // copy buffer contents
        Used=s.CopyData(TheBuffer,BufferSize());
    }
    
    size_t Used; // number of bytes used in the buffer
    const size_t BufSize; // total number of bytes the buffer can hold
    BYTE* TheBuffer; // the buffer
    
}; // end class Buffer

class cMsgProc
{
public:
	struct MY_USER_STORAGE_T
	{
	    MY_USER_STORAGE_T(){DAOCMessageSize=0;};
	    ~MY_USER_STORAGE_T(){};
	    
	    unsigned short DAOCMessageSize;
	};
	typedef Buffer<MY_USER_STORAGE_T> MY_BUFFER_T;

	cMsgProc(void);	
	~cMsgProc(void);	

	bool HandleTCPPacket(char *data,int data_size,bool from_server);
	bool HandleUDPPacket(char *data,int data_size,bool from_server);
	
	unsigned short GetSizeFromBuf
	    (
	    const bool tcp,
	    const bool from_server,
	    const BYTE* buf
	    )const;
private:
	
	bool HandleGeneralPacket
	    (
	    const bool tcp,
	    const bool from_server,
	    MY_BUFFER_T* buf
	    );
	
	cMemory Mem;
	int CntPackets; // count of packets so we know
	                // when to get the key
	                // from memory

    MY_BUFFER_T TCPFromServerBuf;
    MY_BUFFER_T TCPFromClientBuf;
    MY_BUFFER_T UDPFromServerBuf;
    MY_BUFFER_T UDPFromClientBuf;
    
};
};