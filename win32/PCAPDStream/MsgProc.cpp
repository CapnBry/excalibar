#include "pcapserver.h"
#include <iostream>
//#include <fstream>

// some code courtesy of Cheyenne version 0.07 :)
union word_builder
{
    public:
    word_builder(){dword=0;};
    word_builder(const word_builder& s){dword=s.dword;};
    ~word_builder(){};
    
    word_builder& operator=(const word_builder& s){dword=s.dword;return(*this);};

    unsigned int dword;
    unsigned short word[2];
    unsigned char byte[4];
    float real;
};

void GetData(unsigned char& res,int& start,const unsigned char* buf)
{
    res=buf[start];

    start+=sizeof(unsigned char);

    // done
    return;
}
void GetData(char& res,int& start,const unsigned char* buf)
{
    res=buf[start];

    start+=sizeof(char);

    // done
    return;
}

void GetData(unsigned short& res,int& start,const unsigned char* buf)
{
    const word_builder* pwb=reinterpret_cast<const word_builder*>(&buf[start]);
    word_builder rb;

    rb.byte[0]=pwb->byte[1];
    rb.byte[1]=pwb->byte[0];

    res=rb.word[0];

    start+=sizeof(unsigned short);

    // done
    return;
}

void GetData(short& res,int& start,const unsigned char* buf)
{
    const word_builder* pwb=reinterpret_cast<const word_builder*>(&buf[start]);
    word_builder rb;

    rb.byte[0]=pwb->byte[1];
    rb.byte[1]=pwb->byte[0];

    res=rb.word[0];

    start+=sizeof(short);

    // done
    return;
}

void GetData(unsigned int& res,int& start,const unsigned char* buf)
{
    const word_builder* pwb=reinterpret_cast<const word_builder*>(&buf[start]);
    word_builder rb;

    rb.byte[0]=pwb->byte[3];
    rb.byte[1]=pwb->byte[2];
    rb.byte[2]=pwb->byte[1];
    rb.byte[3]=pwb->byte[0];

    res=rb.dword;

    start+=sizeof(unsigned int);

    // done
    return;
}

void GetData(int& res,int& start,const unsigned char* buf)
{
    const word_builder* pwb=reinterpret_cast<const word_builder*>(&buf[start]);
    word_builder rb;

    rb.byte[0]=pwb->byte[3];
    rb.byte[1]=pwb->byte[2];
    rb.byte[2]=pwb->byte[1];
    rb.byte[3]=pwb->byte[0];

    res=rb.dword;

    start+=sizeof(int);

    // done
    return;
}

void GetData(void* variable,int len,int& start,const unsigned char* buf)
{
    // copy
    memcpy(variable,&buf[start],len);

    // increment
    start+=len;
    
    // done
    return;
} // end GetData variable

void GetPascalString(char** unallocated_string,int& start,const unsigned char* buf)
{
    char len;

    // get the length of the string
    GetData(len,start,buf);

    // not doing length validation here is really bad form, however,
    // we know that the messages are all very small
    // and the buffer they are in is very large (buffer_space::buffer_size, 64k)
    // and that the max a char can hold is 255. So there should not be any 
    // danger of overrunning the buffer and attempting to read memory
    // that we shouldn't. The real reason is that I don't feel like passing
    // the message length all the way down to this function just to prevent
    // a problem that should never happen anyway ;)

    char* sz=new char[len+1]; // +1 for null terminator

    // get characters
    GetData(sz,len,start,buf);

    // add null terminator
    sz[len]='\0';

    // put new string into unallocated_string, making it allocated now :P
    *unallocated_string=sz;

    // done
    return;
} // end GetPascalString

void GetZeroString(char** unallocated_string,int& start,const unsigned char* buf,const unsigned int& minlen)
{
    int current=start;

    // find length
    while(buf[current] != '\0' && current < 65535)
        {
        ++current;
        }

    int len=current-start;

    // allocate string
    char* sz=new char[len+1];

    // get characters
    memcpy(sz,&buf[start],len);
    //GetData(sz,len,start,buf);

    // add null terminator
    sz[len]='\0';

    // if len < minlen then adjust
    if(len<int(minlen))
        {
        start += minlen;
        }
    else
        {
        start += len;
        }

    // put new string into unallocated_string, making it allocated now :P
    *unallocated_string=sz;

    // done
    return;
} // end GetZeroString

void SkipData(int& start,int bytes)
{
    start += bytes;

    // done
    return;
} // end SkipData


//std::ofstream log_file;
//std::ofstream from_tcp_server_file;

cMsgProc::cMsgProc(void)
{
    //log_file.open("log.txt",std::ios::out);
    //from_tcp_server_file.open("from_tcp_server.txt",std::ios::out);
    CntPackets=0;
}

cMsgProc::~cMsgProc(void)
{
    //log_file.close();
    //from_tcp_server_file.close();
}

void PrintToStream(std::ostream& str,const BYTE* data,const unsigned short num_bytes)
{
    /*
    In the output stream, we want to see:
    byte0
    byte1 ushort0
    byte2
    byte3 ushort1 uint0
    
    byte4
    byte5 ushort2
    byte6
    byte7 ushort3 uint1
    
    and so forth
    */
    
    int temp;
    int uint_count=0;
    int ushort_count=0;
    unsigned short us;
    unsigned int ui;
    for(int count=0;count<num_bytes;++count)
        {
        str << "byte" << count << "=" << (int)data[count];
        if(!((count+1)%2))
            {
            temp=count-1;
            GetData(us,temp,data);
            str << " short" << ushort_count << "=" << us;
            ++ushort_count;
            }
        if(!((count+1)%4))
            {
            temp=count-3;
            GetData(ui,temp,data);
            str << " uint" << uint_count << "=" << ui << "\n"; // add extra \n here
            ++uint_count;
            }
        str << "\n";
        }
    
    // done
    return;
} // end PrintToFile

unsigned short cMsgProc::GetSizeFromBuf
	(
	const bool tcp,
	const bool from_server,
	const BYTE* buf
	)const
{
    // This function corrects the size
    // of a message based upon the message
    // source and protocol. DAoC seems
    // to intentionally mangle this in
    // order to create security through
    // obscurity.
    
    unsigned short size;
    int ndx=0;

    GetData(size,ndx,buf);

    if(tcp)
        {
        if(from_server)
            {
            // tcp from server
            size+=1;
            }
        else
            {
            // tcp from client
            size+=10;
            }
        }
    else
        {
        // from server or from client,
        // the size computation is the same
        size+=3;
        }

    // done
    return(size);
} // end GetSizeFromBuf

bool cMsgProc::HandleGeneralPacket
	(
	const bool tcp,
	const bool from_server,
	MY_BUFFER_T* buf
	)
{
    //log_file << "[" << __FUNCTION__ << "]"
    //    << " tcp=" << tcp
    //    << " from_server=" << from_server
    //    << " buf=" << buf
    //    << " buf->UserStorage.DAOCMessageSize=" << buf->UserStorage.DAOCMessageSize
    //    << " buf->NumUsedBytes()=" << buf->NumUsedBytes()
    //    << "\n";
        
    // if we need to get the size and have enough data
    // to get the size, then get the size. DAOCMessageSize==0
    // indicates an invalid size, which means we do not know
    // the size yet.
    if(buf->UserStorage.DAOCMessageSize==0 && buf->NumUsedBytes() >= 2)
        {
        buf->UserStorage.DAOCMessageSize=
            GetSizeFromBuf
                (
                tcp,
                from_server,
                buf->ShowTheBuffer()
                );
        //log_file << "new: buf->UserStorage.DAOCMessageSize=" << buf->UserStorage.DAOCMessageSize << "\n"; 
        } // end if need size and have enough data to get size
    
    size_t TotalMessageBytes=buf->UserStorage.DAOCMessageSize+2;
    while(buf->NumUsedBytes() >= TotalMessageBytes)
        {
        //log_file << "decrypting a message of size " << buf->UserStorage.DAOCMessageSize << "\n";
        
        // print stuff
        //PrintToStream(log_file,buf->ShowTheBuffer(),buf->NumUsedBytes());
        
        // decrypt a message in-place, starting just after the size
        Mem.Decrypt
            (
            buf->ShowTheBuffer(2),
            buf->UserStorage.DAOCMessageSize
            );
        
        // send to dstream
        pDStream->x07
            (
            from_server==true?0:1, // 0==from server, 1==from client
            tcp==true?0:1, // 0==tcp, 1==udp
            buf->UserStorage.DAOCMessageSize,
            buf->ShowTheBuffer() // the x07 function will offset for the ushort size prepended to the message
            );
        
        // remove all data regarding the message we just sent
        buf->RemoveData(TotalMessageBytes);
        
        if(buf->NumUsedBytes() >= 2)
            {
            // we have a new size to look at
            buf->UserStorage.DAOCMessageSize=
                GetSizeFromBuf
                    (
                    tcp,
                    from_server,
                    buf->ShowTheBuffer()
                    );
            }
        else
            {
            // flag size as invalid so we compute it again later
            buf->UserStorage.DAOCMessageSize=0;
            }
        
        //log_file << "next: buf->UserStorage.DAOCMessageSize=" << buf->UserStorage.DAOCMessageSize 
        //            << " buf->NumUsedBytes()=" << buf->NumUsedBytes()
        //            << "\n"; 
        
        // readjust this for next message
        TotalMessageBytes=buf->UserStorage.DAOCMessageSize+2;
        } // end while whole messages are on the buffer
        
    //log_file << "\n";
    // done
    return(true);
} // end HandleGeneralPacket

bool cMsgProc::HandleTCPPacket(char* data,int data_size,bool from_server)
{
    // ignore the first two packets, then 
    // find the key when the third packet arrives 
    if(CntPackets == 2) 
        { 
        Mem.GetKey(); 
        CntPackets++;       
        } 
    else if(CntPackets < 2) 
        { 
        CntPackets++; 
        } 
        
    // alias a pointer to the correct buffer
    MY_BUFFER_T* buf;
    if(from_server)
        {
        //PrintToStream(from_tcp_server_file,reinterpret_cast<const BYTE*>(data),(unsigned short)data_size);
        buf=&TCPFromServerBuf;
        }
    else
        {
        buf=&TCPFromClientBuf;
        }
    
    // add the new data to the buffer
    buf->AddData(reinterpret_cast<const BYTE*>(data),data_size);
    
    // return status of general handler function
    return(HandleGeneralPacket(true,from_server,buf));
} // end HandleTCPPacket

bool cMsgProc::HandleUDPPacket(char *data,int data_size,bool from_server)
{
    // alias a pointer to the correct buffer
    MY_BUFFER_T* buf;
    if(from_server)
        {
        buf=&UDPFromServerBuf;
        }
    else
        {
        // no known packets
        return(false);
        //buf=&UDPFromClientBuf;
        }
    
    // add the new data to the buffer
    buf->AddData(reinterpret_cast<const BYTE*>(data),data_size);
    
    // return status of general handler function
    return(HandleGeneralPacket(false,from_server,buf));
} // end HandleUDPPacket
