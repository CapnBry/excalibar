#include <winsock2.h>
#include <winbase.h>
#include "detours.h"
#include "dstreamdefs.h"
#include "segmentsearcher.h"

using namespace std;

HMODULE g_hThisModule;
SOCKET hPipe;
struct sockaddr_in saDest;
FILE *pDebugLog;
BOOL g_Loaded = FALSE;
BOOL g_Connected = FALSE;
int g_UDPBytesLeft;
struct dstream_clientdata_packet *p;
struct dstream_connect_packet connect_packet;

#ifdef _DEBUG
void debug_func(char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	vfprintf(pDebugLog, fmt, args);
	va_end(args);
}

#define DEBUGPRINT(arglist) debug_func arglist
#else
#define DEBUGPRINT(arglist) 
#endif

typedef int (*SYMCRYPTPACKET)(char *, int, char *);
typedef int (*UDPREAD)(char *, int);
typedef void (*NULLSUB)(void);
typedef int (WINAPI *CONNECT)(int s, const struct sockaddr* sa, int namelen);
typedef SIZE_T (WINAPI *VIRTUALQUERY)(void *lpAddress, PMEMORY_BASIC_INFORMATION lpBuffer, SIZE_T dwLength); 
typedef int (WINAPI *RECVFROM)(SOCKET s,char *buf,int len,int flags,struct sockaddr *from,int *fromlen);

SYMCRYPTPACKET SymDecPacketTrampoline = NULL;
SYMCRYPTPACKET SymEncPacketTrampoline = NULL;
UDPREAD UDPReadTrampoline = NULL;
CONNECT ConnectTrampoline = NULL;
NULLSUB inmpkfunc = NULL;
VIRTUALQUERY VirtualQueryTrampoline = NULL;
RECVFROM recvfromTrampoline = NULL;

char *DeROT(char *s)
{
	static char buff[80], *i, *o;
	i = s;
	o = buff;
	while (*i)
		*o++ = *i++ ^ 0x73;
	*o = 0;
	return buff;
}

int SendPipeData(void *data, int len)
{
	int retVal = -1;
	if (hPipe != INVALID_SOCKET)
	{
		int iErr = WSAGetLastError();
		retVal = sendto(hPipe, (const char *)data, len, 0, (const sockaddr *)&saDest, sizeof(saDest));
		WSASetLastError(iErr);
	}

	return retVal;
}

int SymEncPacketDetour(char *data, int len, char *sbox)
{
	DEBUGPRINT(("SymEncPacket(%p,%d,%p)\n", data, len, sbox));

	if (len && hPipe != INVALID_SOCKET) {
		p->dsh.dstream_payload_len = (WORD)(len + sizeof(dstream_clientdata_header));
		p->cdatah.from_client = 1;
		p->cdatah.protocol = 0;  // TCP
		p->cdatah.length = len;
		memcpy(p->data, data, len);
		SendPipeData(p, p->dsh.dstream_payload_len + sizeof(dstream_header));
	}

	return SymEncPacketTrampoline(data, len, sbox);
}

int SymDecPacketDetour(char *data, int len, char *sbox)
{
	int retval = SymDecPacketTrampoline(data, len, sbox);

	DEBUGPRINT(("SymDecPacket(%p,%d,%p) udpleft=%d\n", data, len, sbox, g_UDPBytesLeft));

	if (len && hPipe != INVALID_SOCKET) {
		p->dsh.dstream_payload_len = (WORD)(len + sizeof(dstream_clientdata_header));
		p->cdatah.from_client = 0;
		p->cdatah.protocol = (g_UDPBytesLeft > 0) ? 1 : 0;
		p->cdatah.length = len;
		memcpy(p->data, data, len);
		SendPipeData(p, p->dsh.dstream_payload_len + sizeof(dstream_header));
	}

	if (g_UDPBytesLeft > 0)
		g_UDPBytesLeft -= len + 2;
	return retval;
}

int UDPReadDetour(char *buff, int size)
{
	int retval = UDPReadTrampoline(buff, size);
	
	DEBUGPRINT(("CallRecvFrom(%p,%x)=%d\n", buff, size, retval));

	if (retval > 0)  
		g_UDPBytesLeft += retval;
	else
		g_UDPBytesLeft = 0;

	return retval;
}

int WINAPI recvfromDetour(SOCKET s,char *buf,int len,int flags,struct sockaddr *from,int *fromlen)
{
  int retval = recvfromTrampoline(s, buf, len, flags, from, fromlen);
  
	DEBUGPRINT(("recvfrom(%x,%p)=%d\n", buf, len, retval));

	if (retval > 0)  
		g_UDPBytesLeft += retval;
	else
		g_UDPBytesLeft = 0;

	return retval;
}

int WINAPI ConnectDetour(int s, const struct sockaddr* sa, int namelen)
{
	int retval = ConnectTrampoline(s, sa, namelen);

	if ((retval != SOCKET_ERROR || WSAGetLastError() == WSAEWOULDBLOCK)
		&& hPipe != INVALID_SOCKET) {
		struct sockaddr client_addr;
		int addr_size = sizeof(client_addr);

		DEBUGPRINT(("Connect to server started\n", NULL));

		g_Connected = TRUE;

		connect_packet.dsh.dstream_packet_type = 1;  // CONNECTED
		connect_packet.dsh.dstream_payload_len = sizeof(dstream_connection_details);
		connect_packet.c.connection_id = p->cdatah.connection_id;
		connect_packet.c.server_ip = ((struct sockaddr_in *)sa)->sin_addr.S_un.S_addr;
		connect_packet.c.server_port = ((struct sockaddr_in *)sa)->sin_port;

		if (getsockname(s, &client_addr, &addr_size) != SOCKET_ERROR) {
			connect_packet.c.client_ip = ((struct sockaddr_in *)&client_addr)->sin_addr.S_un.S_addr;
			connect_packet.c.client_port = ((struct sockaddr_in *)&client_addr)->sin_port;
		} else {
			connect_packet.c.client_ip = 0;
			connect_packet.c.client_port = 0;
		}
		SendPipeData(&connect_packet, sizeof(connect_packet));

		/* the getsockname call resets the WOULDBLOCK error code.  Fix it back
		   to WOULDBLOCK for the DAOC Client to read */
		WSASetLastError(WSAEWOULDBLOCK);
	}  /* if connect worked */

	return retval;
}

SIZE_T WINAPI VirtualQueryDetour(void *lpAddress, PMEMORY_BASIC_INFORMATION lpBuffer, SIZE_T dwLength)
{
	SIZE_T retval = VirtualQueryTrampoline(lpAddress, lpBuffer, dwLength);

	DEBUGPRINT(("VirtualQuery(%x)\n", lpAddress));

	if (retval > 0 && lpBuffer->AllocationBase == g_hThisModule)
		lpBuffer->State &= ~MEM_COMMIT;
    return retval;
}

void SetupDestAddr(void)
{
	unsigned short port;
	char *envData;

	port = 9024;
	envData = (char *)malloc(32767);
	if (envData) {
		DWORD dwBytes, i;
		dwBytes = GetEnvironmentVariable(DeROT("\x23\x32\x27\x3b"), envData, 32767);  // "PATH"
		if (dwBytes && dwBytes < 32767)
		{
			DEBUGPRINT(("Environment PATH is %s\n", envData));
			for (i = 0; i<dwBytes; i++)
				port += (unsigned char)envData[i];
		}
		else
			DEBUGPRINT(("Could not get environment PATH\n", NULL));
		free(envData);

		while (port < 5000)
			port += 5000;
	}  /* if envData */

	DEBUGPRINT(("Chose port number %u\n", port));

	memset(&saDest, 0, sizeof(saDest));
	saDest.sin_family = AF_INET;
	saDest.sin_port = (u_short)((port & 0xff) << 8 | port >> 8);
	saDest.sin_addr.S_un.S_addr = (u_long)0x0100007f;
}

#define TEXTSEG "\x5d\x07\x16\x0b\x07"

void SetupCamp(const char *modulename, char *parentexename)
{
	if (g_Loaded) 
		return;

#ifdef _DEBUG
	pDebugLog = fopen("\\debug.log", "w");
#else
	pDebugLog = NULL;
#endif _DEBUG
	DEBUGPRINT(("DLL loaded into game: %s\n", parentexename));

	/* set up our "pipe" out of here.  this is just a non-blocking UDP socket to 127.0.0.1 */
	WSADATA wsa;
	unsigned long nonblock;
	WSAStartup(0x200, &wsa);
	hPipe = socket(AF_INET, SOCK_DGRAM, 0);
	if (hPipe == INVALID_SOCKET) {
		DEBUGPRINT(("Could not open pipe: %d\n", WSAGetLastError()));
		return;
	}
	nonblock = 1;
	ioctlsocket(hPipe, FIONBIO, &nonblock); 
	SetupDestAddr();
		
	CSegmentSearch segsrch(parentexename);
	PBYTE pFunc;

	pFunc = segsrch.FindFirst(DeROT(TEXTSEG),  // ".text"
		"\x8b\x75\x0c\x83\xc4\x0c\xd1\xee\x3b\x75\x0c\x8b\xfe\x73", 14);
	DEBUGPRINT(("SymEncPacket=%p\n", pFunc));
	if (pFunc) SymEncPacketTrampoline = (SYMCRYPTPACKET)DetourFunction((PBYTE)pFunc-71, (PBYTE)SymEncPacketDetour);

	pFunc = segsrch.FindFirst(DeROT(TEXTSEG),  // ".text"
		"\xd1\xee\x83\xc4\x0c\x3b\x75\x0c\x8b\xfe\x73", 11);
	DEBUGPRINT(("SymDecPacket=%p\n", pFunc));
	if (pFunc) SymDecPacketTrampoline = (SYMCRYPTPACKET)DetourFunction ((PBYTE)pFunc-73, (PBYTE)SymDecPacketDetour);
			
//	pFunc = segsrch.FindFirst(DeROT(TEXTSEG),  // ".text"
//		"\xff\x74\x24\x08\xb9", 5);
//	DEBUGPRINT(("UDPRead=%p\n", pFunc));
//	if (pFunc) UDPReadTrampoline = (UDPREAD)DetourFunction((PBYTE)pFunc, (PBYTE)UDPReadDetour);

	pFunc = DetourFindFunction("WSOCK32.DLL", DeROT("\x10\x1c\x1d\x1d\x16\x10\x07")); // "connect"
	DEBUGPRINT(("connect=%p\n", pFunc));
	if (pFunc) ConnectTrampoline = (CONNECT)DetourFunction(pFunc, (PBYTE)ConnectDetour);

    pFunc = DetourFindFunction("WSOCK32.DLL", DeROT("\x01\x16\x10\x05\x15\x01\x1c\x1e")); // "recvfrom"
	DEBUGPRINT(("recvfrom=%p\n", pFunc));
	if (pFunc) recvfromTrampoline = (RECVFROM)DetourFunction(pFunc, (PBYTE)recvfromDetour);

	pFunc = DetourFindFunction("KERNEL32.DLL", DeROT("\x25\x1a\x01\x07\x06\x12\x1f\x22\x06\x16\x01\x0a")); // "VirtualQuery"
	DEBUGPRINT(("VirtualQuery=%p\n", pFunc));
	if (pFunc) VirtualQueryTrampoline = (VIRTUALQUERY)DetourFunction(pFunc, (PBYTE)VirtualQueryDetour);

	p = (dstream_clientdata_packet *)malloc(4096);
	g_UDPBytesLeft = 0;
	p->dsh.dstream_packet_type = 0;
	p->cdatah.connection_id = GetCurrentProcessId();

	g_Loaded = TRUE;
}

void SendDisconnectMessage(void)
{
	if (hPipe != INVALID_SOCKET) {
		unsigned long nonblock = 0;
		ioctlsocket(hPipe, FIONBIO, &nonblock); 

		connect_packet.dsh.dstream_packet_type = 2;  // DISCONNECTED
		SendPipeData(&connect_packet, sizeof(connect_packet));
		g_Connected = FALSE;
	}
}
void CleanupCamp(void)
{
	if (g_Connected)
		SendDisconnectMessage();

	if (hPipe != INVALID_SOCKET) {
		SOCKET s = hPipe;
		hPipe = INVALID_SOCKET;

		shutdown(s, SD_SEND);
		closesocket(s);
	}
	WSACleanup();

	if (SymEncPacketTrampoline) DetourRemove((PBYTE)SymEncPacketTrampoline, (PBYTE)SymEncPacketDetour);
	if (SymDecPacketTrampoline) DetourRemove((PBYTE)SymDecPacketTrampoline, (PBYTE)SymDecPacketDetour);
	if (UDPReadTrampoline) DetourRemove((PBYTE)UDPReadTrampoline, (PBYTE)UDPReadDetour);
	if (ConnectTrampoline) DetourRemove((PBYTE)ConnectTrampoline, (PBYTE)ConnectDetour);
	if (VirtualQueryTrampoline) DetourRemove((PBYTE)VirtualQueryTrampoline, (PBYTE)VirtualQueryDetour);

	free(p);

	if (pDebugLog) {
		fflush(pDebugLog);
		fclose(pDebugLog);
		pDebugLog = NULL;
	}

	g_Loaded = FALSE;
}

extern "C" __declspec(dllexport) BOOL __stdcall D387C0A5DC36(int nCode, WPARAM w, LPARAM l)
{
	return 0;
}

BOOL APIENTRY DllMain(HINSTANCE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
{
    char szParentEXEName[MAX_PATH] = {0};
    char szModuleName[MAX_PATH] = {0};
	char *pEXEName;
	bool should_attach;

	g_hThisModule = (HMODULE)hModule;
	GetModuleFileName((HMODULE)hModule, szModuleName, MAX_PATH);
	GetModuleFileName(0, szParentEXEName, MAX_PATH);
	pEXEName = strrchr(szParentEXEName, '\\');
	should_attach = stricmp(pEXEName, DeROT("\x2f\x14\x12\x1e\x16\x5d\x17\x1f\x1f")) == 0; // "\\game.dll"
	should_attach = should_attach || stricmp(pEXEName, DeROT("\x2f\x07\x14\x12\x1e\x16\x5d\x17\x1f\x1f")) == 0;  // \\tgame.dll
	// should_attach = should_attach || stricmp(pEXEName, "\\Project1.exe") == 0;
	if (should_attach)
		if (ul_reason_for_call == DLL_PROCESS_ATTACH) 
			SetupCamp(szModuleName, szParentEXEName);
		else if (g_Loaded && ul_reason_for_call == DLL_PROCESS_DETACH) 
			CleanupCamp();

    return TRUE;
}
