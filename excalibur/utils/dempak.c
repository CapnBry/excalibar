/*
 * dempak.c: extracts Dark Age of Camelot .mpk/.npk archives.
 * See http://www.randomly.org/projects/mapper/ for more info and updates.
 *
 * Copyright (c) 2001-2002, Oliver Jowett <oliver@randomly.org>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * The file format consists of:
 *  . a signature ('MPAK')
 *  . a constant-size header (details unknown)
 *  . a zlib stream containing the name of the archive
 *  . a zlib stream containing the archive directory (see below)
 *  . a zlib stream for each entry in the directory.
 *
 * The archive directory is a series of fixed-length records, each 0x11c
 * bytes. The first part of the entry is a null-terminated string 
 * (the filename); the rest is unexamined.
 *
 * Usage:
 *  dempak <path-to-mpk>
 *
 * The resulting files are written to a directory based on the mpk filename
 * ('.out' is appended)
 *
 * Generated paths are unix-style (/-separated); this will require modification
 * for other systems.
 *
 */

/* Yeah, I know the code is grotty, it's a quick hack. It'd be in Python if
 * the zlib module allowed you to handle stream ends properly..
 */

#include <zlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/malloc.h>
#include <ctype.h>
#include <sys/stat.h>

void processData(int stage, char *data, int len)
{
    static char *dir;
    static int dirlen;
    static int laststage;
    static FILE *lastfile;
    static char *name;

    switch (stage) {
	case -1: /* perform cleanup on EOF */
	    printf("End of MPAK.\n");
	    if (lastfile != NULL) fclose(lastfile);
	    return;
	case 0:  /* archive name */
	    /* FIXME: assumes the name is in a single decompressed block */
	    /* FIXME: we leak here */
	    /* FIXME: error checking */
	    name = malloc(len+1);
	    memcpy(name, data, len);
	    name[len] = 0;
	    printf("MPAK: %s\n", name);
	    return;
	case 1:  /* directory */
	    /* Accumulate data. */
	    dir = realloc(dir, dirlen + len);
	    memcpy(dir + dirlen, data, len);
	    dirlen += len;
	    return;
	default: { /* file data */
	    int offset = 0x11c * (stage - 2);
	    if (offset >= dirlen) {
		printf("off the end of the directory!\n");
		exit(1);
	    }

	    if (laststage != stage) {
		/* New file. */

		char path[1024];
		char *p, *q;

		if (lastfile != NULL) fclose(lastfile);

		/* FIXME: buffer overflows coming out my ears.. */
		strcpy(path, name);
		strcat(path, ".out");
		p = path;
		while (*p)
		    *p = tolower(*p), p++;

		mkdir(path, 0700);
		strcat(path, "/"); /* FIXME: system-dependent separator */

		p = dir+offset;
		q = path + strlen(path); 

		while (*p)
		    *q++ = tolower(*p++);

		*q = 0;

		printf("Writing [%d]: %s\n", stage, path);
		
		if (!(lastfile = fopen(path, "w"))) {
		    perror(path);
		    exit(1);
		}

		laststage = stage;
	    }

	    /* Write data. */
	    /* FIXME: error checking */
	    fwrite(data, 1, len, lastfile);
	    return;
	}	    
    } 
}

int main(int argc, char *argv[])
{
    FILE *fp;
    static z_stream stream;
    char inbuf[1024], outbuf[1024];
    int inlen = 0;
    int stage = 0;

    if (argc < 2) {
	printf("syntax: %s <mpk filename>\n", argv[0]);
	return 1;
    }

    if (!(fp = fopen(argv[1], "r"))) {
	perror(argv[1]);
	return 1;
    }

    /* skip the static header */
    /* FIXME: check the signature */
    fseek(fp, 21, SEEK_SET);

    /* keep a stream open always */
    /* FIXME: error checking */
    inflateInit(&stream);
    
    while (!feof(fp) || inlen > 0) {
	int rc;

	/* .. read some data */
	inlen += fread(inbuf + inlen, 1, sizeof(inbuf) - inlen, fp);
	
	/* .. decompress it */
	stream.next_in = inbuf;
	stream.avail_in = inlen;
	
	stream.next_out = outbuf;
	stream.avail_out = sizeof(outbuf);

	rc = inflate(&stream, 0);
	if (rc != Z_STREAM_END && rc != Z_OK) {
	    /* decompression error */
	    printf("inflate returned %d\n", rc);
	    return 2;
	}

	/* if we have some decompressed data, process it */
	if ((char*)stream.next_out > outbuf)
	    processData(stage, outbuf, (char*)stream.next_out - outbuf);
	
	/* if zlib consumed some data, rearrange our buffers */
	if ((char*)stream.next_in > inbuf) {
	    memmove(inbuf, stream.next_in, stream.avail_in);
	    inlen = stream.avail_in;
	}

	if (rc == Z_STREAM_END) {
	    /* end of compression stream, reinit the decompressor for the */
	    /* next stream */
	    ++stage;
	    inflateEnd(&stream);
	    inflateInit(&stream);
	}
    }

    /* allow processData to clean up */
    processData(-1, NULL, 0);

    /* all done.. */
    fclose(fp);
    inflateEnd(&stream);

    return 0;
}

/* Changelog:
 *
 * 04-Dec-2001: Initial release
 * 19-Jan-2002: Force mpak directory name to lowercase
 */
