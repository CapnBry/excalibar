/*

public domain software
Tue Mar  9 13:17:03 EST 2004 MastaMappa
Nov 20 15:16 MastaMappa Fixed the heading code

this program allows you to use the java map editor with the DStream app
1. start DStream
2. start this app with an argument of the ip address of the DStream host
3. start the java editor, connect to the host running the proxy with a port of 19789

example:

DAOC and DStream are running on you PC @ 192.168.1.50
proxy2DStream 192.168.1.50 # on your *nix box at 192.168.1.51
java -jar editor.jar & # then Link to 192.168.1.51:19789

this is a chatty (verbose) version...

*/

/*
to build:

`grep cc proxy2DStream.c | grep $(uname)`

gcc -o proxy2DStream  -lsocket -lnsl proxy2DStream.c	# SunOS
gcc -o proxy2DStream proxy2DStream.c 			# Linux
cc -o proxy2DStream proxy2DStream.c 			# Darwin

*/

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <unistd.h>
#include <netinet/in.h>
#include <limits.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/wait.h>

#define uint32 unsigned long
#define uint16 unsigned short
#define uchar unsigned char
#define int32 long
#define int16 short

uchar buffer[1024*8];
uchar *p;
char DStreamConnected, MapConnected;
uint32 x, y, z, newdata;

#define DPACKET_HELO 0x1
#define DPACKET_LOG 0x2
#define DPACKET_CONNECTION_QUERY 0x3
#define DPACKET_CONNECTION_DETAILS 0x4
#define DPACKET_DAOC_CONNECTION_OPEN 0x5
#define DPACKET_DAOC_CONNECTION_CLOSED 0x6
#define DPACKET_DAOC_DATA 0x7
#define DPACKET_SET_PACKETFILTER 0x8
#define DPACKET_AUTH_CREDENTIALS 0x9
#define DPACKET_AUTH_RESPONSE 0xa
#define DPACKET_SQUELCH 0xb
#define DPACKET_RESUME 0xc

#define DAOC_LocalPosUpdateFromClient 0xa9

struct dstream_header {
	uint16 total_length;
	uchar command_id;
	};

struct _DPACKET_HELO {
	char signature[4]; char null;/* DSTM (null not part of official def.) */
	int32 version_no;
	uchar authentication_required; /* 0 - no, 1 - yes */
	};

struct _DPACKET_DAOC_DATA {
	uint32 connectionid;
	uchar origin; /* 0 - from server, 1 from client */
	uchar protocol; /* 0 - TCP, 1 - UDP */
	uint16 data_size;
	uchar *data;
	};

uint16 get_BEshort()	/* Big Endian short (Intel crap) */
{
uint16 val;
	val = *p++;
	val |= (*p++ << 8);
	return val;
}

uint16 get_LEshort()	/* Little Endian short (the REST of the world) */
{
uint16 val;
	val = (*p++ << 8);
	val |= *p++;
	return val;
}

uint32 get_BElong()	/* Big Endian long */
{
uint32 val;
	val = *p++;
	val |= (*p++ << 8);
	val |= (*p++ << 16);
	val |= (*p++ << 24);
	return val;
}

uint32 get_LElong()	/* Little Endian short */
{
uint32 val;
	val = (*p++ << 24);
	val |= (*p++ << 16);
	val |= (*p++ << 8);
	val |= *p++;
	return val;
}

round(double z)
{
int i;
	i = z;
	z -= i;
	if (z >= .5)
		i++;
	return i;
}

parsepacket()
{
struct dstream_header dheader;
struct _DPACKET_HELO hello;
struct _DPACKET_DAOC_DATA daoc_data;
int i;

	
	p = buffer;
	dheader.total_length = get_BEshort(); /* dang Intel */
	dheader.command_id = *p++;
	switch (dheader.command_id)
	{
	case DPACKET_HELO:
		hello.signature[0] = *p++;
		hello.signature[1] = *p++;
		hello.signature[2] = *p++;
		hello.signature[3] = *p++;
		hello.null = 0;
		hello.version_no = get_BElong();
		hello.authentication_required = *p++; /* 0 - no, 1 - yes */

		printf("connected to %s version %d authentication %s required\n", hello.signature, hello.version_no, (hello.authentication_required ? "" : "not"));
		break;
	case DPACKET_LOG:
	case DPACKET_CONNECTION_QUERY:
	case DPACKET_CONNECTION_DETAILS:
		printf("dstream command %d not handled\n", dheader.command_id);
		break;
	case DPACKET_DAOC_CONNECTION_OPEN:
		printf("DAOC CONNECTION OPEN\n");
		break;
	case DPACKET_DAOC_CONNECTION_CLOSED:
		p = buffer;
		showraw(30); 
		printf("DAOC CONNECTION CLOSED\n");
		break;
	case DPACKET_DAOC_DATA:
		daoc_data.connectionid = get_BElong();
		daoc_data.origin = *p++;
		daoc_data.protocol = *p++;
		daoc_data.data_size = get_BEshort();
		daoc_data.data = p;
	/*
	** Wanna use this code for your application? here is where you hook
	** your code
	*/
#if 0
		showraw(daoc_data.data_size); 
#endif
		if (daoc_data.origin == 0) /* looking for client side */
		{
			switch (daoc_data.data[0])
			{
			case 0xaf: printf("%d %s\n", daoc_data.data[5], &daoc_data.data[9]);
			}
			break;
		}
		if (daoc_data.data[7] != DAOC_LocalPosUpdateFromClient) 
			break;
		showraw(daoc_data.data_size); 
		p = &daoc_data.data[12];
		z = get_LEshort();
		x = get_LEshort();
		y = get_LEshort();
		{
		int zone;
		int heading;
			zone = get_BEshort();
			heading = (((get_LEshort() & 0xfff) * 90 / 0x400) + 180) % 360;
		newdata = 1; /* tell the main loop we got new data */
		printf("%d %d %d zone=%d head=%d\n", x, y, z, zone, heading);
		}
#if 0
		printf("DAOC data, %s %s server len %d\n", (daoc_data.protocol ? "TCP" : "UDP"), (daoc_data.origin ? "to" : "from"), daoc_data.data_size);
		printf("\n");
#endif
		break;
	case DPACKET_SET_PACKETFILTER:
	case DPACKET_AUTH_CREDENTIALS:
	case DPACKET_AUTH_RESPONSE:
	case DPACKET_SQUELCH:
	case DPACKET_RESUME:
		printf("dstream command %d not handled\n", dheader.command_id);
		break;
	default:
		printf("unknown DSTREAM command %d\n", dheader.command_id);
	}
}

/* This ignores the SIGPIPE signal.  This is usually a good idea, since
   the default behaviour is to terminate the application.  SIGPIPE is
   sent when you try to write to an unconnected socket.  You should
   check your return codes to make sure you catch this error! */

void ignore_pipe(void)
{
struct sigaction sig;

	sig.sa_handler = SIG_IGN;
	sig.sa_flags = 0;
	sigemptyset(&sig.sa_mask);
	sigaction(SIGPIPE,&sig,NULL);
}

/* Take a service name, and a service type, and return a port number.  If the
   service name is not found, it tries it as a decimal number.  The number
   returned is byte ordered for the network. */
int atoport(service, proto)
char *service;
char *proto;
{
int port;
long int lport;
struct servent *serv;
char *errpos;

	/* First try to read it from /etc/services */
	serv = getservbyname(service, proto);
	if (serv != NULL)
		port = serv->s_port;
	else /* Not in services, maybe a number? */
	{
		lport = strtol(service,&errpos,0);
		if ( (errpos[0] != 0) || (lport < 1) || (lport > 65535) )
			return -1; /* Invalid port address */
		port = htons(lport);
	}
return port;
}

/* Converts ascii text to in_addr struct.  NULL is returned if the address
  can not be found. */
struct in_addr *atoaddr(address)
char *address;
{
struct hostent *host;
static struct in_addr saddr;

	/* First try it as aaa.bbb.ccc.ddd. */
	saddr.s_addr = inet_addr(address);
	if (saddr.s_addr != -1)
	{
		return &saddr;
	}
	host = gethostbyname(address);
	if (host != NULL)
	{
		return (struct in_addr *) *host->h_addr_list;
	}
	return NULL;
}

/* This function listens on a port, and returns connections.  It forks
   returns off internally, so your main function doesn't have to worry
   about that.  This can be confusing if you don't know what is going on.
   The function will create a new process for every incoming connection,
   so in the listening process, it will never return.  Only when a connection
   comes in, and we create a new process for it will the function return.
   This means that your code that calls it should _not_ loop.

   The parameters are as follows:
     socket_type: SOCK_STREAM or SOCK_DGRAM (TCP or UDP sockets)
     port: The port to listen on.  Remember that ports < 1024 are
       reserved for the root user.  Must be passed in network byte
       order (see "man htons").
     listener: This is a pointer to a variable for holding the file
       descriptor of the socket which is being used to listen.  It
       is provided so that you can write a signal handler to close
       it in the event of program termination.  If you aren't interested,
       just pass NULL.  Note that all modern unixes will close file
       descriptors for you on exit, so this is not required. */
int get_connection(socket_type, port, listener)
int socket_type;
u_short port;
int *listener;
{
struct sockaddr_in address;
int listening_socket;
int connected_socket = -1;
int new_process;
int reuse_addr = 1;

  /* Setup internet address information.  
     This is used with the bind() call */
	memset((char *) &address, 0, sizeof(address));
	address.sin_family = AF_INET;
	address.sin_port = port;
	address.sin_addr.s_addr = htonl(INADDR_ANY);
	
	listening_socket = socket(AF_INET, socket_type, 0);
	if (listening_socket < 0)
	{
		perror("socket");
		exit(EXIT_FAILURE);
	}
	
	if (listener != NULL)
		*listener = listening_socket;
	
	setsockopt(listening_socket, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, 
	sizeof(reuse_addr));
	if (bind(listening_socket, (struct sockaddr *) &address, sizeof(address)) < 0)
	{
		perror("bind");
		close(listening_socket);
		exit(EXIT_FAILURE);
	}
	if (socket_type == SOCK_STREAM)
	{
		listen(listening_socket, 5); /* Queue up to five connections before
						having them automatically rejected. */
		while(connected_socket < 0)
		{
			connected_socket = accept(listening_socket, NULL, NULL);
			if (connected_socket < 0)
			{
				/* Either a real error occured, or blocking was interrupted for
				** some reason.  Only abort execution if a real error occured. */
				if (errno != EINTR)
				{
					perror("accept");
					close(listening_socket);
					exit(EXIT_FAILURE);
				}
				else
				{
					continue;    /* don't fork - do the accept again */
				}
			}
			new_process = fork();
			if (new_process < 0)
			{
				perror("fork");
				close(connected_socket);
				connected_socket = -1;
			}
			else /* We have a new process... */
			{
				if (new_process == 0)
				{
					/* This is the new process. */
					close(listening_socket); /* Close our copy of this socket */
		  			if (listener != NULL)
		          			*listener = -1; /* Closed in this process.  We are not 
					     			responsible for it. */
				}
				else
				{
					/* This is the main loop.  Close copy of connected socket, and
					continue loop. */
					close(connected_socket);
					connected_socket = -1;
				}
			}
		}
		return connected_socket;
	}
	else
		return listening_socket;
}

/* This is a generic function to make a connection to a given server/port.
   service is the port name/number,
   type is either SOCK_STREAM or SOCK_DGRAM, and
   netaddress is the host name to connect to.
   The function returns the socket, ready for action.*/
int make_connection(service, type, netaddress)
char *service;
int type;
char *netaddress;
{
/* First convert service from a string, to a number... */
int port = -1;
struct in_addr *addr;
int sock;
struct sockaddr_in address;

	if (type == SOCK_STREAM) 
		port = atoport(service, "tcp");
	if (type == SOCK_DGRAM)
		port = atoport(service, "udp");
	if (port == -1)
	{
		fprintf(stderr,"make_connection:  Invalid socket type.\n");
		return -1;
	}
	addr = atoaddr(netaddress);
	if (addr == NULL)
	{
		fprintf(stderr,"make_connection:  Invalid network address.\n");
		return -1;
	}

	memset((char *) &address, 0, sizeof(address));
	address.sin_family = AF_INET;
	address.sin_port = (port);
	address.sin_addr.s_addr = addr->s_addr;

	sock = socket(AF_INET, type, 0);

	printf("Connecting to %s on port %d.\n",inet_ntoa(*addr),htons(port));

	if (type == SOCK_STREAM)
	{
		DStreamConnected = connect(sock, (struct sockaddr *) &address, sizeof(address));
		if (DStreamConnected < 0)
		{
			perror("connect");
			return -1;
		}
		return sock;
	}
	/* Otherwise, must be for udp, so bind to address. */
	if (bind(sock, (struct sockaddr *) &address, sizeof(address)) < 0)
	{
		perror("bind");
		return -1;
	}
	return sock;
}

read_packet(sock)
{
int n;
unsigned short len = 0, rest;

	if ((n = read(sock, buffer, 2)) == 2)
		len = (buffer[1] << 8) | buffer[0];
	else
		DStreamConnected = -1;
	if (len > 0)
	{
		rest = len - 2;
		if ((n = read(sock, buffer+2, rest)) == rest)
			return len;
		if (n == -1)
			DStreamConnected = -1;
	}
	return n;
}

showraw(int n)
{
int i;

	for (i = 0; i <= n; i++)
	{
		printf(" %02x", p[i]);
	}
	printf("\n");	
}

/* This waits for all children, so that they don't become zombies. */
void sig_chld(signal_type)
int signal_type;
{
int pid;
int status;

	while ((pid = wait3(&status, WNOHANG, NULL)) > 0);
}

/* MastaMappa -> 'M' 'M' => 0x4d 0x4d -> 0x4d4d -> 19789 */
#define DEFPORT "19789"
int listensock = -1; /* So that we can close sockets on ctrl-c */

int main(argc, argv)
int argc;
char *argv[];
{
int i, n, DSsock, MPsock, svport;
unsigned char *p, *host, *port;
int debug = 0;
struct sigaction act, oldact;
char outbuff[80];

	if (argc < 2)
	{
		fprintf(stderr,"Usage: %s host[:port] [srvrport]\n", argv[0]);
		fprintf(stderr, "\tdefault port = 9867, default srvrport = %s\n", DEFPORT);
		exit(EXIT_FAILURE);
	}

	host = outbuff;
	sprintf(host, "%s:9867:", argv[1]);
	port = strchr(outbuff, ':');
	*port++ = 0;
	p = strchr(port, ':');
	*p = 0;

	ignore_pipe();
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	act.sa_handler = sig_chld;
	sigaction(SIGCHLD, &act, &oldact);

	svport = atoport((argc > 2 ? argv[2] : DEFPORT), "tcp");

	if (svport == -1)
	{
		fprintf(stderr,"Unable to find service: %s\n", (argc > 2 ? argv[2] : DEFPORT));
		exit(EXIT_FAILURE);
	}

	printf("listening on %s\n", (argc > 2 ? argv[2] : DEFPORT));

	MPsock = get_connection(SOCK_STREAM, svport, &listensock);

	/* still compatible with editor version 1 */
	if (write(MPsock,"1\r\n", 3) != 3)
	{
		fprintf(stderr,"Failed to send version compatiblity to Map editor\n");
		close(MPsock);
		return -1;
	}

	MapConnected = 1;

	DSsock = make_connection(port, SOCK_STREAM, host);
	if (DSsock == -1)
	{
		fprintf(stderr,"connection to DStream failed.\n");
		close(MPsock);
		return -1;
	}
	while (DStreamConnected >= 0 && MapConnected > 0)
	{
		if ((n = read_packet(DSsock)) < 1)
		{
			fprintf(stderr, "read_packet from DStream failed.\n", n);
			DStreamConnected = -1;
			break;
		}
		if (debug)
		{
			p = buffer;
			showraw(n);
		}
		parsepacket();
		if (MapConnected && newdata)
		{
			sprintf(outbuff, "%d %d %d\r\n", x, y, z);
			if (write(MPsock, outbuff, strlen(outbuff)) < 0)
				MapConnected = 0;
			newdata = 0;
		}
	}
	printf("<CLOSED>\n");
	close(DSsock);
	close(MPsock);
	return 0;
}
