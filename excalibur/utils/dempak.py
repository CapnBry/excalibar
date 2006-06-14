#!/usr/bin/env python

# dempak.py: Dark Age of Camelot archive (.mpk/.npk) extractor.
# See http://www.randomly.org/projects/mapper/ for updates and sample output.

# Copyright (c) 2002, Oliver Jowett <oliver@randomly.org>
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Updated: 2002/01/26

# This module is both importable and directly runnable.

"""Dark Age of Camelot .npk/.mpk archive extraction module."""

import zlib, struct, weakref, string
from cStringIO import StringIO

class error(IOError): pass

_test = zlib.decompressobj()
if not hasattr(_test, 'unused_data'):
    raise error, 'Update your Python: this zlib module is too old'
del _test

def _readstream(f):
    """_readstream(f) -> string
Decompress a zlib stream from a file 'f'; ensure 'f' points immediately after
the end of the stream on return. Returns the decompressed stream data.
"""
    do = zlib.decompressobj()

    output = ''
    while do.unused_data == '':
        str = f.read(1024)
        if str == '':
            # Might have (unluckily!) hit EOF just as decompression
            # ended. Prod the stream and see if that's considered
            # "unused" data.
            
            output += do.decompress('x')
            if do.unused_data == '':
                raise error, 'unexpected end of stream'
            else:
                f.seek(1 - len(do.unused_data), 1)
                return output
            
        output += do.decompress(str)

    f.seek(0 - len(do.unused_data), 1)
    return output

class MPAKFile:
    """Object wrapping an archive file."""
    
    def __init__(self, path):
        """MPAKFile(path) -> object

Open a new MPAKFile object for the archive located at 'path'.
Throws dempak.error or IOError on errors.
"""
        
        self.path = path
        self.f = open(path, 'rb')

        # check signature
        if self.f.read(4) != 'MPAK':
            raise error, 'not a MPAK archive'

        self.f.seek(21)

        # discard first stream
        _readstream(self.f)

        # read second stream: directory
        dirdata = _readstream(self.f)

        # decode directory.
        baseoffset = self.f.tell()
        self.directory = {}
        offset = 0
        while offset + 0x11b < len(dirdata):
            name = ''
            i = 0
            while offset + i < len(dirdata) and dirdata[offset+i] != '\0':
                name += dirdata[offset+i]
                i += 1

            (entryoffset,) = struct.unpack('<I', dirdata[offset+0x110:offset+0x114])
            self.directory[name.lower()] = baseoffset + entryoffset
            offset += 0x11c

        self.entries = self.directory.keys()
        # done.

    def open(self, entry):
        """open(entry) -> file

Return a file-like object for the archive entry 'entry'.
Throws dempak.error or IOError on errors.
"""
        if not self.f:
            raise error, 'file is closed'

        entry = entry.lower()
        if not self.directory.has_key(entry):
            raise error, 'unknown entry: ' + entry

        self.f.seek(self.directory[entry])
        data = _readstream(self.f)
        return StringIO(data)

    def close(self):
        self.f.close()
        self.f = None

_filecache = {}

def getMPAKEntry(path, entry):
    """getMPAKEntry(path, entry) -> file

Get a file-like object for a particular entry of an archive.
Throws dempak.error or IOError on errors."""
    
    global _filecache

    if not _filecache.has_key(path):
        _filecache[path] = MPAKFile(path)

    return _filecache[path].open(entry)

def run():
    import sys, os

    if len(sys.argv) != 2:
        print >>sys.stderr, "Usage: %s <path to .mpk/.npk>" % sys.argv[0]
        sys.exit(1)

    name = sys.argv[1]
    f = MPAKFile(name)
    base = os.path.basename(name) + '.out'

    if not os.path.isdir(base):
        os.mkdir(base)
    
    for e in f.entries:
        outpath = os.path.join(base, e)
        print >>sys.stderr, "Extracting", e, "=>", outpath,
        
        infile = f.open(e)
        data = infile.read()
        infile.close()

        outfile = open(outpath, 'wb')
        outfile.write(data)
        outfile.close()

        print >>sys.stderr, "(" + `len(data)` + " bytes)"

    f.close()
    sys.exit(0)

if __name__ == '__main__': run()

# Changelog:
#   26-Jan-2002: Initial version. Requires Python 2.1 for
#                the more recent zlib module.
