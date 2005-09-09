#include <winsock2.h>
#include <winbase.h>
#include "detours.h"
#include "dstreamdefs.h"
#include "segmentsearcher.h"
#include "injectntapi.h"

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
  #define DEBUG_LOG
#endif

#ifdef DEBUG_LOG
void debug_func(char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	vfprintf(pDebugLog, fmt, args);
	va_end(args);
	fflush(pDebugLog);
}

#define DEBUGPRINT(arglist) debug_func arglist
#else
#define DEBUGPRINT(arglist) 
#endif

//typedef int (*SYMCRYPTPACKET)(char *, int, char *);  // 1.70
typedef int (*SYMCRYPTPACKET2)(char *, int);  
typedef int (*SYMCRYPTPACKET3)(char *, int, char *);  
typedef void (*NULLSUB)(void);
typedef int (WINAPI *CONNECT)(int s, const struct sockaddr* sa, int namelen);
typedef SIZE_T (WINAPI *VIRTUALQUERY)(void *lpAddress, PMEMORY_BASIC_INFORMATION lpBuffer, SIZE_T dwLength); 
typedef int (WINAPI *RECVFROM)(SOCKET s,char *buf,int len,int flags,struct sockaddr *from,int *fromlen);

SYMCRYPTPACKET3 SymDecPacketTrampoline = NULL;
SYMCRYPTPACKET3 SymEncPacketTrampoline = NULL;
CONNECT ConnectTrampoline = NULL;
NULLSUB inmpkfunc = NULL;
VIRTUALQUERY VirtualQueryTrampoline = NULL;
RECVFROM recvfromTrampoline = NULL;
PBYTE SymEncPacketDetour = NULL;
PBYTE SymDecPacketDetour = NULL;

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

int SymEncPacketDetourSTACK(char *data, int len, char *sbox)
{
	DEBUGPRINT(("SymEncPacket(%p,%d,%x)\n", data, len, sbox));

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

int SymEncPacketDetourEAX(char *data, int len)
{
	DWORD saveEAX;
	__asm mov saveEAX, eax;
	DEBUGPRINT(("SymEncPacket(%p,%d,%x)\n", data, len, saveEAX));

	if (len && hPipe != INVALID_SOCKET) {
		p->dsh.dstream_payload_len = (WORD)(len + sizeof(dstream_clientdata_header));
		p->cdatah.from_client = 1;
		p->cdatah.protocol = 0;  // TCP
		p->cdatah.length = len;
		memcpy(p->data, data, len);
		SendPipeData(p, p->dsh.dstream_payload_len + sizeof(dstream_header));
	}

	__asm {
		push len;
		push data;
		mov eax, saveEAX;
		call SymEncPacketTrampoline;
		mov saveEAX, eax;
		add esp, 8;
	}
	return saveEAX;
}

int SymDecPacketDetourEAX(char *data, int len)
{
	int retval;
	// SymDecPacketTrampoline(data, len);
	__asm{
		push len;
		push data;
		//mov eax, 1;
		call SymDecPacketTrampoline;
		mov retval, eax;
		add esp, 8;
	}

	DEBUGPRINT(("SymDecPacket(%p,%d) udpleft=%d\n", data, len, g_UDPBytesLeft));

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

int SymDecPacketDetourSTACK(char *data, int len, char *sbox)
{
	int retval;
	retval = SymDecPacketTrampoline(data, len, sbox);

	DEBUGPRINT(("SymDecPacket(%p,%d) udpleft=%d\n", data, len, g_UDPBytesLeft));

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

	//DEBUGPRINT(("VirtualQuery(%x)\n", lpAddress));

	if (retval > 0 && lpBuffer->AllocationBase == g_hThisModule)
	{
		DEBUGPRINT(("That is us, removing COMMIT flag\n"));
		lpBuffer->State &= ~MEM_COMMIT;
	}
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

int SuspendAllThreadsCommon(bool suspend)
{
	HMODULE hNTDll = GetModuleHandle("NTDLL.DLL");
	PFZwQuerySystemInformation QuerySystemInformation = NULL;
	void *pBuffer = NULL;
	DWORD bufsize = 0x8000;
	NTSTATUS status;

	if (hNTDll == 0)
	{
		DEBUGPRINT(("Can not open ntdll\n"));
		return -1;
	}

	QuerySystemInformation = (PFZwQuerySystemInformation)GetProcAddress(hNTDll, "ZwQuerySystemInformation");  
	if (!QuerySystemInformation)
	{
		DEBUGPRINT(("Can not find ZwQuerySystemInformation\n"));
		FreeLibrary(hNTDll);
		return -1;
	}

	/* Just keep increasing the buffer size until we get a big enough buffer to hold the 
	   whole processandthreads info */
	do
    {
        pBuffer = malloc(bufsize);
        if (pBuffer == NULL)
		{
			FreeLibrary(hNTDll);
			return -1;
		}

        status = QuerySystemInformation(SystemProcessesAndThreadsInformation, pBuffer, bufsize, NULL);
        if (status == STATUS_INFO_LENGTH_MISMATCH)
        {
			DEBUGPRINT(("QuerySystemInformation buffer too small (%d)\n", bufsize));
            free(pBuffer);
			bufsize *= 2;
        }
        else if (status < 0)
		{
			DEBUGPRINT(("QuerySystemInformation unexpected error (%l)\n", status));
            free(pBuffer);
			FreeLibrary(hNTDll);
			return -1;
		}
	} while (status == STATUS_INFO_LENGTH_MISMATCH);

	FreeLibrary(hNTDll);

	PSYSTEM_PROCESSES pProcesses = (PSYSTEM_PROCESSES)pBuffer;
	DWORD thisPID = GetCurrentProcessId();
	for (;;)
	{
		//DEBUGPRINT(("Process ID: %d\n", pProcesses->ProcessId));
		if (pProcesses->ProcessId == thisPID)
		{
			DWORD myThreadId = GetCurrentThreadId();
			int threadCount = pProcesses->ThreadCount;
			PSYSTEM_THREADS pThreads = (PSYSTEM_THREADS)pProcesses->Threads;
			DEBUGPRINT(("Target found.  Has %d threads\n", threadCount));

			for (int iThread=0; iThread<threadCount; iThread++)
			{
				if (pThreads->ClientId.UniqueThread == myThreadId)
					DEBUGPRINT(("One of the threads is us\n"));  // and it should be!
				else {  
					HANDLE hThread = OpenThread(THREAD_SUSPEND_RESUME, false, pThreads->ClientId.UniqueThread);
					if (hThread)
					{
						DEBUGPRINT(("Thread 0x%x opened successfully.\n", pThreads->ClientId.UniqueThread));
						if (suspend)
							SuspendThread(hThread);
						else
							ResumeThread(hThread);
						CloseHandle(hThread);
					}
					else
						DEBUGPRINT(("Thread 0x%x could not be opened %d.\n", 
							pThreads->ClientId.UniqueThread, GetLastError()));
				}  /* if not our thread */
				pThreads++;
			}  /* for each thread */

			break;
		}  /* if this is our process */

		if (pProcesses->NextEntryDelta == 0)
			break;

		pProcesses = (PSYSTEM_PROCESSES)((LPBYTE)pProcesses + pProcesses->NextEntryDelta);
	}

	free(pBuffer);

	return 0;
}


int SuspendAllThreads()
{
	DEBUGPRINT(("Suspending all threads\n"));
	return SuspendAllThreadsCommon(true);
}

int ResumeAllThreads()
{
	DEBUGPRINT(("Resuming all threads\n"));
	return SuspendAllThreadsCommon(false);
}

void DetourProperSymEnc(PBYTE pFunc, CSegmentFileSearch &segsrch)
{
	if (pFunc) 
	{
		PBYTE pFuncSearch, pLastFunc;
		pFuncSearch = segsrch.FindFirst(DeROT(TEXTSEG),  // ".text"
			"\xc9\xc3\x55\x8b\xec");  // leave retn, push ebp mov ebp,esp
		do
		{
			pLastFunc = pFuncSearch;
			pFuncSearch = segsrch.FindNext();
		} while (pFuncSearch && pFuncSearch < pFunc);

		if (pFuncSearch)
		{
			DEBUGPRINT(("Function begins at (+2)=%p\n", pLastFunc));
			/* 0x37 bytes is 3rd param passed in EAX */
			if (pFunc - pLastFunc == 0x37)
			{
				DEBUGPRINT(("Using EAX for third parameter\n"));
				SymEncPacketDetour = (PBYTE)SymEncPacketDetourEAX;
				SymEncPacketTrampoline = (SYMCRYPTPACKET3)DetourFunction((PBYTE)pFunc - 0x35, SymEncPacketDetour);
			}
			else if (pFunc - pLastFunc == 0x38)
			{
				DEBUGPRINT(("Using STACK for third parameter\n"));
				SymEncPacketDetour = (PBYTE)SymEncPacketDetourSTACK;
				SymEncPacketTrampoline = (SYMCRYPTPACKET3)DetourFunction((PBYTE)pFunc - 0x36, SymEncPacketDetour);
			}
			else
				DEBUGPRINT(("No calling signature found=%p\n", pFunc - pLastFunc));
		}
	}
}

void DetourProperSymDec(PBYTE pFunc, CSegmentFileSearch &segsrch)
{
	if (pFunc) 
	{
		PBYTE pFuncSearch, pLastFunc;
		pFuncSearch = segsrch.FindFirst(DeROT(TEXTSEG),  // ".text"
			"\xc9\xc3\x55\x8b\xec");  // leave retn, push ebp mov ebp,esp
		do
		{
			pLastFunc = pFuncSearch;
			pFuncSearch = segsrch.FindNext();
		} while (pFuncSearch && pFuncSearch < pFunc);

		if (pFuncSearch)
		{
			DEBUGPRINT(("Function begins at (+2)=%p\n", pLastFunc));
			/* 0x37 bytes is 3rd param passed in EAX */
			if (pFunc - pLastFunc == 0x37)
			{
				DEBUGPRINT(("Using EAX for third parameter\n"));
				SymDecPacketDetour = (PBYTE)SymDecPacketDetourEAX;
				SymDecPacketTrampoline = (SYMCRYPTPACKET3)DetourFunction((PBYTE)pFunc - 0x35, SymDecPacketDetour);
			}
			else if (pFunc - pLastFunc == 0x38)
			{
				DEBUGPRINT(("Using STACK for third parameter\n"));
				SymDecPacketDetour = (PBYTE)SymDecPacketDetourSTACK;
				SymDecPacketTrampoline = (SYMCRYPTPACKET3)DetourFunction((PBYTE)pFunc - 0x36, SymDecPacketDetour);
			}
			else
				DEBUGPRINT(("No calling signature found=%p\n", pFunc - pLastFunc));
		}
	}
}

void SetupCamp(const char *modulename, char *parentexename)
{
	if (g_Loaded) 
		return;

#ifdef DEBUG_LOG
	pDebugLog = fopen("\\debug.log", "w");
#else
	pDebugLog = NULL;
#endif 
	DEBUGPRINT(("DLL loaded into game: %s\n", parentexename));

	/* set up our "pipe" out of here.  this is just a non-blocking UDP socket to 127.0.0.1 */
	WSADATA wsa;
	unsigned long nonblock;
	int wsaRetVal = WSAStartup(0x101, &wsa);
	if (wsaRetVal != 0)
	{
		DEBUGPRINT(("WSAStartup failed: %d\n", wsaRetVal));
		return;
	}
	hPipe = socket(AF_INET, SOCK_DGRAM, 0);
	if (hPipe == INVALID_SOCKET) {
		DEBUGPRINT(("Could not open pipe: %d\n", WSAGetLastError()));
		return;
	}
	nonblock = 1;
	ioctlsocket(hPipe, FIONBIO, &nonblock); 
	SetupDestAddr();

	SuspendAllThreads();

	CSegmentFileSearch segsrch(parentexename);
	PBYTE pFunc;

	pFunc = segsrch.FindFirst(DeROT(TEXTSEG),  // ".text"
		"\x8b\x75\x0c\xd1\xee\x3b\x75\x0c\x88\x45\xfe\x88\x45\xff\x8b\xfe\x73", 17);
	DEBUGPRINT(("SymEncPacket=%p\n", pFunc));
	DetourProperSymEnc(pFunc, segsrch);

	pFunc = segsrch.FindFirst(DeROT(TEXTSEG),  // ".text"
	  "\x8b\x75\x0c\xd1\xee\x3b\x75\x0c\x88\x5d\xff\x8b\xfe\x73", 14);
	DEBUGPRINT(("SymDecPacket=%p\n", pFunc));
	DetourProperSymDec(pFunc, segsrch);
			
	pFunc = DetourFindFunction("WSOCK32.DLL", DeROT("\x10\x1c\x1d\x1d\x16\x10\x07")); // "connect"
	DEBUGPRINT(("connect=%p\n", pFunc));
	if (pFunc) ConnectTrampoline = (CONNECT)DetourFunction(pFunc, (PBYTE)ConnectDetour);

    pFunc = DetourFindFunction("WSOCK32.DLL", DeROT("\x01\x16\x10\x05\x15\x01\x1c\x1e")); // "recvfrom"
	DEBUGPRINT(("recvfrom=%p\n", pFunc));
	if (pFunc) recvfromTrampoline = (RECVFROM)DetourFunction(pFunc, (PBYTE)recvfromDetour);

	pFunc = DetourFindFunction("KERNEL32.DLL", DeROT("\x25\x1a\x01\x07\x06\x12\x1f\x22\x06\x16\x01\x0a")); // "VirtualQuery"
	DEBUGPRINT(("VirtualQuery=%p\n", pFunc));
	if (pFunc) VirtualQueryTrampoline = (VIRTUALQUERY)DetourFunction(pFunc, (PBYTE)VirtualQueryDetour);
	ResumeAllThreads();

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

	SuspendAllThreads();
	if (SymEncPacketTrampoline) DetourRemove((PBYTE)SymEncPacketTrampoline, (PBYTE)SymEncPacketDetour);
	if (SymDecPacketTrampoline) DetourRemove((PBYTE)SymDecPacketTrampoline, (PBYTE)SymDecPacketDetour);
	if (ConnectTrampoline) DetourRemove((PBYTE)ConnectTrampoline, (PBYTE)ConnectDetour);
	if (VirtualQueryTrampoline) DetourRemove((PBYTE)VirtualQueryTrampoline, (PBYTE)VirtualQueryDetour);
	ResumeAllThreads();

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
	if (pEXEName)
	{
		should_attach = stricmp(pEXEName, DeROT("\x2f\x14\x12\x1e\x16\x5d\x17\x1f\x1f")) == 0; // "\\game.dll"
		should_attach = should_attach || stricmp(pEXEName, DeROT("\x2f\x07\x14\x12\x1e\x16\x5d\x17\x1f\x1f")) == 0;  // \\tgame.dll
//		should_attach = should_attach || stricmp(pEXEName, "\\dstreamservertest.exe") == 0;
		if (should_attach)
			if (ul_reason_for_call == DLL_PROCESS_ATTACH) 
				SetupCamp(szModuleName, szParentEXEName);
			else if (g_Loaded && ul_reason_for_call == DLL_PROCESS_DETACH) 
				CleanupCamp();
	}

    return TRUE;
}
