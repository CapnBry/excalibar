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

#include "buffer.h"
#include "memory.h"
#include <assert.h>
#include <crtdbg.h>

// use the namespace
using namespace buffer_space;

Buffer::Buffer()
{
    // initialize members
    last_error=buffer_space::no_err;
    write=0;
    read=0;
    size=0;
    memset(&buf[0],0,sizeof(buf));

    // done
    return;
} // end Buffer()

Buffer::Buffer(const Buffer& s)
{
    // set
    set(s);

    // done
    return;
} // end Buffer(const Buffer&)

Buffer::~Buffer()
{
    // done
    return;
} // end ~Buffer

bool Buffer::Peek(void* data,const unsigned int length)
{
    // auto lock
    AutoLock al(&l);

    // save size
    unsigned int sz=size;
    unsigned int rd=read;

    // grab data
    bool result=Extract(data,length);

    // restore old size and read
    size=sz;
    read=rd;

    // done
    return(result);
} // end Peek

void Buffer::InsertWrapped(const void* data,const unsigned int length)
{
    // this is the case where write < read

    // copy data
    memcpy(&buf[write],data,length);

    // increment write
    write += length;

    // done
    return;
} // end InsertWrapped

void Buffer::InsertUnwrapped(const void* data,const unsigned int length)
{
    // this is the case where write >= read
    // if we get here and write == read then 
    // length is zero and we do nothing

    if(length > (buffer_size - write))
        {
        // the buffer will be wrapped when we are done
        // because this data will not fit in the area
        // [write,buffer_size)

        // store size at end of buffer
        unsigned int buffer_tail=buffer_size - write;

        // first, copy what will fit between [write,buffer_size)
        memcpy(&buf[write],data,buffer_tail);

        // get remaining length
        unsigned int remainder=length - buffer_tail;
        
        // set to index 0 because we exactly filled
        // the remaining space in the end of the buffer
        write=0;

        // alias a pointer so we can index it
        const char* alias=reinterpret_cast<const char*>(data);

        // copy remaining data
        memcpy(&buf[write],&alias[buffer_tail],remainder);

        // increment write
        write += remainder;
        }
    else
        {
        // the buffer may not be wrapped when we are
        // done because it may fit in the area
        // [write,buffer_size)
        memcpy(&buf[write],data,length);
        write += length;

        if(write == buffer_space::buffer_size)
            {
            // we are now wrapped because the data
            // fit exactly into the space in
            // [write,buffer_size)

            // set to index 0
            write=0;
            }
        } // end else maybe not wrapped

    // done
    return;
} // end InsertUnwrapped

bool Buffer::Insert(const void* data,const unsigned int length)
{
    // auto lock
    AutoLock al(&l);

    // check invalid pointer
    _ASSERTE(data != 0);

    // all insertions of zero size are successful
    if(length == 0)
        {
        return(true);
        }

    /*
    write is the next available index that can be written
    to, likewise, read is the first available index that
    contains data -- if size==0 then write==read
    */

    // check for overflow
    if(length > buffer_space::buffer_size - size)
        {
        last_error=buffer_space::overflow;

        return(false);
        }
    
    // see where the data needs to be inserted
    if(write < read)
        {
        // the buffer is wrapped around

        // this increments write, but
        // leaves size alone
        InsertWrapped(data,length);
        }
    else
        {
        // the buffer is not wrapped around

        // this increments write, but
        // leaves size alone
        InsertUnwrapped(data,length);
        }

    // increment size
    size+=length;

    // set signal
    signal.signal();
    
    // done
    return(true);
} // end Insert

void Buffer::ExtractWrapped(void* data,const unsigned int length)
{
    // this is the case when
    // length > (buffer_size - read)

    // store size at end of buffer
    unsigned int buffer_tail=buffer_size - read;
    unsigned int remainder=length - buffer_tail;

    // copy the tail of the buffer
    memcpy(data,&buf[read],buffer_tail);

    // set read to 0 since we just read to the 
    // end of the buffer
    read=0;

    // alias a pointer so we can index it
    char* alias=reinterpret_cast<char*>(data);

    // copy the remainder of the data
    memcpy(&alias[buffer_tail],&buf[read],remainder);

    // increment read
    read += remainder;

    // done
    return;
} // end ExtractWrapped

void Buffer::ExtractUnwrapped(void* data,const unsigned int length)
{
    // this is the case when
    // length <= (buffer_size - read)

    // copy the data
    memcpy(data,&buf[read],length);

    // increment read
    read += length;

    // check for read past end of buffer
    if(read == buffer_space::buffer_size)
        {
        // we are now wrapped because the length
        // fit exactly into the space in
        // [read,buffer_size)

        // set to index 0
        read=0;
        }

    // done
    return;
} // end ExtractUnwrapped

bool Buffer::Extract(void* data,const unsigned int length)
{
    // auto lock
    AutoLock al(&l);

    // check invalid pointer
    _ASSERTE(data != 0);

    // all extractions of zero size are successful
    if(length == 0)
        {
        return(true);
        }

    /*
    write is the next available index that can be written
    to, likewise, read is the first available index that
    contains data -- if size==0 then write==read
    */

    // check for underflow
    if(length > size)
        {
        last_error=buffer_space::underflow;

        return(false);
        }

    // see if requested length wraps around
    if(length > (buffer_size - read))
        {
        // requested size will wrap around
        // the buffer

        // this increments read, but
        // leaves size alone
        ExtractWrapped(data,length);
        }
    else
        {
        // requested size will not wrap around
        // the buffer

        // this increments read, but
        // leaves size alone
        ExtractUnwrapped(data,length);
        }

    // decrement size
    size -= length;

    // reset signal if we are empty
    if(size==0)
        {
        signal.reset();
        }
    
    // done
    return(true);
} // end Extract

buffer_space::Buffer& Buffer::operator=(const buffer_space::Buffer& s)
{
    // auto lock
    AutoLock al(&l);

    // check for self-assignment
    if(this != &s)
        {
        // assign
        set(s);
        }

    // done
    return(*this);
} // end operator=(const Buffer&)

void Buffer::set(const buffer_space::Buffer& s)
{
    // this function is NOT THREAD SAFE

    last_error=s.last_error;
    write=s.write;
    read=s.read;
    size=s.size;
    memcpy(&buf[0],&s.buf[0],sizeof(buf));
    
    if(size)
        {
        signal.signal();
        }
    else
        {
        signal.reset();
        }

    // done
    return;
} // end set

