#ifndef _DSTREAMDEFS_H_
#define _DSTREAMDEFS_H_

#define DPACKET_HELO					0x01
#define DPACKET_LOG						0x02
#define DPACKET_CONNECTION_QUERY		0x03
#define DPACKET_CONNECTION_DETAILS		0x04
#define DPACKET_DAOC_CONNECTION_OPENED	0x05
#define DPACKET_DAOC_CONNECTION_CLOSED	0x06
#define DPACKET_DAOC_DATA				0x07
#define DPACKET_SET_PACKETFILTER		0x08
#define DPACKET_AUTH_CREDENTIALS		0x09
#define DPACKET_AUTH_RESPONSE			0x0a
#define DPACKET_SQUELCH					0x0b
#define DPACKET_RESUME					0x0c
#define DPACKET_KEYS					0x0d
#define DPACKET_MOUSE					0x0e

#pragma pack(push, 1)
struct dstream_header {
	WORD dstream_payload_len;
	BYTE dstream_packet_type;
};

struct dstream_clientdata_header {
	DWORD connection_id;
	BYTE from_client;
	BYTE protocol;
	WORD length;
};

struct dstream_connection_details {
	DWORD connection_id;
	DWORD server_ip;
	WORD server_port;
	DWORD client_ip;
	WORD client_port;
};

struct dstream_clientdata_packet {
	struct dstream_header dsh;
	struct dstream_clientdata_header cdatah;
	char data[1];
};

struct dstream_connect_packet {
	struct dstream_header dsh;
	struct dstream_connection_details c; 
};

#pragma pack(pop)

#endif

