{
********************************************************************************
--------------------------------------------------------------------------------
                         Plibcap Highlevel function calls

                for Packet Capture Driver by Politecnico di Torino

     Code converted and modified from C to Pascal by Lars Peter Christiansen
--------------------------------------------------------------------------------
 TERMS AND CONDITIONS OF USE.
 some parts of this software is Copyright(C) 2000 Lars Peter Christiansen.

 The author of this software assumes no liability for damages caused under
 any circumstances whatsoever, and is under no obligation. Use of the software
 indicates acceptance of all conditions contained in this document. If you do
 not agree to these terms, you must delete this software immediately.

 You may distribute the archive in which this software is distributed, but
 under no circumstances must this archive be changed. Distributing a modified
 archive is a violation of the software license.

 If you do redistribute this software, please let me know at the email address
 given below.

 If you have any questions, requests, bug reports, etc., please contact me at
 the address given below.

Lars Peter Christiansen
Email  : bakkevej@stofanet.dk
Website: http://home1.stofanet.dk/nitezhifter

Plibcap.c author:
webpsite: http://netgroup-serv.polito.it/windump
--------------------------------------------------------------------------------

                          [ user application ]
                          [       PCAP       ] <- you are here!
                          [    PacketAPI     ]
            ------------------------------------------------
            [ Windows 95/98 |  Windows NT  |  Windows2000  ]
            [  Packet.dll   |  Packet.dll  |  packet.dll   ]
            [  Packet.vxd   |  Packet.sys  |  packet.sys   ]
            ------------------------------------------------
                          [    Netadapter    ]


   Original Filename : Pcap.pas

   Implemented Original Functions :
      Function pcap_open_live() : PPcap
      function pcap_read()      : integer
      function pcap_stats()     : integer
      function pcap_setbuff()   : integer
      function pcap_loop()      : integer
      function pcap_datalink    : integer

   Modified/added/removed :
    17-1-2002:
      Pcap_loop() : Fixed silly bug not exiting endless loop
      pcap_getwinversion(): updated to include WinXP and WinME

    28-1-2001:
      Function Pcap_GetAdapternames() : bug found&fixed [ by _blade_ ]
      Function Pcap_Read() : fixed wrong headerpos in packets. [Jody Dawkins]

     4-2-2000:
      Function Pcap_close  : now removing pcap.buffer from memory
    Older:
      function wsockinit()      : integer; // Removed. obsolete in Delphi
      function  pcap_lookupdev(): pchar;   // Modified to Delphi's advantage
      function pcap_getwinversion: Twinversion // added. Much easier OS handling
      function Pcap_getAdapternames() : String // Hand in hand with Tlist.commatext


   Wanted :
      function pcap_GetMACAddr()  : boolean;
      function pcap_open_offline  : PPcap; + offline functions

********************************************************************************
}

unit Pcap;

interface
uses windows,
     Ndis_def,
     bpf,
     sysutils,  // formatting tools. Could use FormatMessage,but is more complex
     Packet32;  // This is what we wrap

const
  PCAP_ERRBUF_SIZE = 256;              //String size of error descriptions
  PcapBufSize      = 256000;           //Dimension of the buffer in TPcap


// [taken from interface.h]

  DEFAULT_SNAPLEN = 68;                //The default snapshot length.
                                       //This value allows most printers to
                                       //print useful information while keeping
                                       //the amount of unwanted data down.In
                                       //particular, it allows for an ethernet
                                       //header, tcp/ip header, and 14 bytes of
                                       //data (assuming no ip options).


type
  TWinVersion = (wv_WinS,
                 wv_Win9x,              //Added by Lars Peter Christiansen.
                 wv_WinNT,              //Eases the process of determing the
                 wv_Win2000,             //platform and do proper instructions
                 wv_WinXP,               //I.e : Char vs. WideChar issue
                 wv_Unknown );



  PPcap_Stat = ^TPcap_stat;
  Tpcap_stat = record
    ps_recv,	                 	 //* number of packets received */
    ps_drop,	                	 //* number of packets dropped */
    ps_ifdrop : LongWord;                //* drops by interface not supported */
  end;

  TPcap_sf = record                      // Save file for offline reading.
    rfile : HFILE;
    swapped:integer;
    version_major : integer;
    Version_Minor : integer;
    base : Pointer;
  end;

  TPcap_md = record
    Stat : TPcap_stat;
    use_bpf : integer;
    TotPkts  : LongWord;               // Can owerflow after 79hours on ethernet
    TotAccepted:LongWord;              // accepted by filter/sniffer
    TotDrops : LongWord;               // dropped packets
    TotMissed: Longword;               // missed by i/f during this run
    OrigMissed:LongWord;               // missed by i/f before this run
  end;

  PPcap_PktHdr = ^Tpcap_pkthdr;        // Wrapped Drivers packetHeader
  TPcap_pkthdr = record
    ts     : TUnixTimeVal;             // Time of capture
    CapLen,                            // captured length
    Len    : Integer;                  // actual length of packet
  end;

  PPcap = ^TPcap;                      // THE MAIN INTERFACE HANDLE
  TPcap = record                       // used with allmost all Pcap calls.
    Adapter:Padapter;
    Packet :PPacket;                   // Global Driver packet. kind of a buffer
    snapshot:integer;
    linktype:integer;                  // Type and speed of net
    tzoff   :integer;	               // timezone offset
    offset  :integer;
    sf      :Tpcap_sf;                 // Save file
    md      :Tpcap_md;                 // Diagnostics
    //READ BUFFER
    bufsize :integer;
    buffer  :Pointer; //*u_char
    bp      :Pointer; //*u_char
    cc      :integer;
    //Place holder for pcap_next().
    pkt     :Pointer; //*U_char
    //Placeholder for filter code if bpf not in kernel.
    fcode   :Tbpf_program;
    errbuf  : array [0..PCAP_ERRBUF_SIZE-1] of char;  //Last error message
  end;


  // Callback procedure
  Ppcap_handler =^Tpcap_handler;
  Tpcap_handler = procedure(User:pointer;const Header:Ppcap_pkthdr;const Data:pchar);


function  pcap_open_live(Device:String;SnapLen:LongWord;Promisc:boolean;
          To_ms:integer;var errstr:String) : ppcap;
function  pcap_read(p:PPcap;cnt:integer;CallBack:Tpcap_handler;User:pointer) :integer;
function  pcap_stats    (P: pPcap;ps:PPcap_stat) : integer;
function  pcap_setbuff  (p : Ppcap;dim:integer) : integer;
procedure pcap_close    (var p : ppcap);
function  pcap_lookupdev(var ErrStr:string) : pchar;
function pcap_loop(P:Ppcap;cnt:integer;Callback:Tpcap_handler;user:pointer):integer;
function pcap_datalink(P:PPcap) : integer;
function pcap_getwinversion(var verstr:string) : Twinversion;
function Pcap_getAdapternames(Delimiter:char;var ErrStr:string):string;
implementation


//------------------------------------------------------------------------------
//  pcap_t *pcap_open_live(char *device, int snaplen, int promisc,
//                         int to_ms, char *ebuf)
//------------------------------------------------------------------------------
function pcap_open_live(Device:String;SnapLen:LongWord;Promisc:boolean;
         To_ms:integer;var errstr:String) : ppcap;
  var
     P : Ppcap;
     NetType : Tnet_type;
     S : Pchar;

     procedure CleanUp;
     begin
       if P.adapter<>nil then PacketCloseAdapter(P.adapter);
       if P.buffer<>nil then FreeMem(P.buffer,PcapBufSize);
       Freemem(P,SizeOf(Tpcap));

     end;
begin
    result :=nil;

    // CREATE PCAP OBJECT

    GetMem(P,SizeOf(Tpcap));
    if P=nil then
      begin
        ErrStr := 'Cannot allocate pcap object';
        exit;
      end;
    FillChar(p^,sizeof(Tpcap),0);
    P.Adapter := nil;

    // CREATE ADAPTER OBJECT
    GetMem(S,2048);                       // Making temporary pchar
    StrPCopy(S,Device);
    P.Adapter := PacketOpenAdapter(S);
    FreeMem(S,2048);
    if P.Adapter = nil then
      begin
        ErrStr := 'Cannot Open Adapter "'+Device+'"';
        CleanUp;
        exit;
      end;


   // SET FILTER MODE
    if Promisc then
      begin
        if not PacketSetHWFilter(P.adapter,NDIS_PACKET_TYPE_PROMISCUOUS) then
          Begin
            ErrStr:= 'Cannot set Device Filter to Promiscuous mode';
            cleanup;
            exit;
          end;
      end else if not PacketSetHWFilter(P.adapter,NDIS_PACKET_TYPE_DIRECTED) then
          begin
            ErrStr:= 'Cannot set Device Filter to Directed mode';
            cleanup;
            exit;
          end;

    // GET NETCARD SPEED AND TYPE
    if not PacketGetNetType(P.Adapter,@Nettype) then
       Begin
         ErrStr := 'Cannot determine network type and speed';
         Cleanup;
         exit;
       end;

    Case TNDIS_MEDIUM(nettype.LinkType) of

       NdisMediumWan   : P.linktype := DLT_PPP_WIN32;

       NdisMedium802_3 : begin
                           if nettype.LinkSpeed = 100000000 then
                              p.linktype := DLT_EN100MB
                           else if nettype.LinkSpeed=10000000 then
                              p.linktype := DLT_EN10MB
                           else p.linktype:=DLT_PPP_WIN32;
                         end;
       else p.linktype := DLT_EN10MB;
    end;

    // Allocate room for Link header

    p.bufsize := PcapBufSize;
    GetMem(p.buffer,PcapBufSize);
    if P.buffer = nil then
      begin
        ErrStr := 'Cannot allocate Link Header space';
        cleanup;
        exit;
      end;

    p.snapshot := Snaplen;

    // Allocate Global Packet for capturing

    p.packet := PacketAllocatePacket;
    if p.packet = nil then
      begin
        ErrStr := 'Cannot allocate Global Packet Object';
        cleanup;
        exit;
      end;
    PacketInitPacket(p.Packet,p.buffer,p.bufsize);

    // Allocate Driver Buffer
    if not PacketSetBuff(p.adapter,DEFAULT_DRIVERBUFFER) then
      begin
        ErrStr := 'Not enough memory to allocate Driver buffer';
        CleanUp;
        exit;
      end;

    result := p;

end;


//------------------------------------------------------------------------------
//int pcap_read(pcap_t *p, int cnt, pcap_handler callback, u_char *user)
//
//------------------------------------------------------------------------------
function pcap_read( p:PPcap;cnt:integer;CallBack:Tpcap_handler;User:pointer)
         : integer;
var cc   : Longword;//Counter ?
    n    : integer;
    bp,ep: pointer; //Begin and End Point ?
    //bhp  : Pbpf_hdr;//pointer to BPF header struct - removed by Lars Peter
    hdrlen,         //Length of Header
    caplen: LongWord;//Length of captured
begin
  cc := p.cc;
  n  := 0;

  if p.cc = 0 then
    begin

       // *Capture the Packets*
         if PacketReceivePacket(p.adapter,p.packet,TRUE)=false then
         begin
           // ERROR!
           p.errbuf :='Read Error: PacketRecievePacket failed';
           result:=-1;
           exit;
         end;
         cc := p.packet.ulBytesReceived;

         bp := p.buffer;

    end else bp := p.bp;


    // Loop through each packet.

    ep := ptr(longword(bp)+cc); //move end pointer
    while (longword(bp) < longword(ep) ) do
      begin
        caplen := Pbpf_hdr(bp).bh_caplen;
        hdrlen := Pbpf_hdr(bp).bh_hdrlen;

        // XXX A bpf_hdr matches apcap_pkthdr.

        callback(user,
                 Ppcap_pkthdr(bp),
                 ptr(longword(bp)+HdrLen));

        LongWord(bp) := LongWord(bp) + BPF_WORDALIGN(caplen + hdrlen);
        inc(n);
        if (n >= cnt)and(cnt>0) then
          begin
            p.bp := bp;
            p.cc := longword(ep)-longword(bp);
            result := n;
            exit;
          end;
      end;

   p.cc := 0;
   result:=n;
end;


//------------------------------------------------------------------------------
// int pcap_stats(pcap_t *p, struct pcap_stat *ps)
//
//------------------------------------------------------------------------------
function pcap_stats(P: pPcap;ps:PPcap_stat) : integer;
var s:Tbpf_stat;
begin

    if PacketGetStats(
                      P.Adapter,
                      @s) = false then
    begin
      P.errbuf := 'PacketGetStats error';
      result := -1;
      exit;
    end;

    ps.ps_recv := s.bs_recv;
    ps.ps_drop := s.bs_drop;
    result:= 0;
end;

//------------------------------------------------------------------------------
// int pcap_setbuff(pcap_t *p, int dim)
//
//------------------------------------------------------------------------------
function pcap_setbuff(p : Ppcap;dim:integer) : integer;
begin

    if p=nil then
    begin
      result:=-2;
      P.errbuf := 'invalid pcap handle';
      exit;
    end;

    if PacketSetBuff(p.adapter,dim)=false then
    begin
      P.Errbuf := 'Driver error : Not enough memory to allocate buffer';
      result := -1;
      exit;
    end;
    result := 0;
end;


//------------------------------------------------------------------------------
//  void pcap_close(pcap_t *p)
//
// Very simplified from the original
//------------------------------------------------------------------------------
procedure pcap_close(var p : ppcap);
begin

  if p=nil then exit;
  if p.Adapter<>nil then
    begin
      PacketCloseAdapter(p.adapter);
      p.adapter:=nil;
    end;

  if p.buffer<>nil then
    begin
      FreeMem(P.buffer,p.bufsize);
      p.buffer := nil;
    end;
  FreeMem(p,sizeof(Tpcap));
  p:=nil;
end;



//------------------------------------------------------------------------------
//
//     Following procedures is taken from inet.c part of Pcap
//
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//int pcap_loop(pcap_t *p, int cnt, pcap_handler callback, u_char *user)
//------------------------------------------------------------------------------
{pcap_loop() is similar to pcap_dispatch() except it keeps reading
packets until cnt packets are processed or an error occurs. It does
not return when live read timeouts occur. Rather, specifying a
non-zero read timeout to pcap_open_live() and then calling
pcap_dispatch() allows the reception and processing of any
packets that arrive when the timeout occurs. A negative cnt
causes pcap_loop() to loop forever (or at least until an error
occurs).
}
function pcap_loop(P:Ppcap;cnt:integer;Callback:Tpcap_handler;user:pointer):integer;
begin
  if p=nil then begin
    result:= -1;
    exit;
  end;

  while true do begin

      if p.sf.rfile<>0 then
        begin
          result:= -1; //pcap_offline_read(p,cnt,callback,user);
          exit;
        end
      else Repeat
          // Keep reading until we get something(or get an error)
             result := pcap_read(p,cnt,callback,user);
           until result<>0;

      if result<=0 then exit;

      if cnt>0 then
        begin
          cnt:=cnt-result;
          if cnt<=0 then
            begin
              result:=0;
              exit;
            end;
        end;
  end;
end;



//------------------------------------------------------------------------------
{int pcap_dispatch(pcap_t *p, int cnt, pcap_handler callback, u_char *user)}
//------------------------------------------------------------------------------
{pcap_dispatch() is used to collect and process packets. cnt
specifies the maximum number of packets to process before returning.
A cnt of -1 processes all the packets received in one buffer.
A cnt of 0 processes all packets until an error occurs, EOF is
reached, or the read times out (when doing live reads and a
non-zero read timeout is specified). callback specifies a routine
to be called with three arguments: a u_char pointer which is
passed in from pcap_dispatch(), a pointer to the pcap_pkthdr
struct (which precede the actual network headers and data),
and a u_char pointer to the packet data. The number of packets read
is returned. Zero is returned when EOF is reached in a
``savefile.'' A return of -1 indicates an error in which
case pcap_perror() or pcap_geterr() may be used to display the
error text.}

function pcap_dispatch(P :pPcap;cnt:integer;CallBack:Tpcap_handler;User:pointer)
         :integer;
begin
  if P.sf.rfile<>0 Then
      result := -1//pcap_offline_read(p,cnt,callback,user)
  else
      result := pcap_read(p,cnt,callback,user)
end;


//------------------------------------------------------------------------------
//char * pcap_lookupdev(errbuf)
//------------------------------------------------------------------------------
//*
// * Return the name of a network interface attached to the system, or NULL
// * if none can be found.  The interface must be configured up; the
// * lowest unit number is preferred; loopback is ignored.
//
function pcap_lookupdev(var ErrStr:string) : pchar;
var   NameLength   : integer;
      AdapterNames : array[0..1024-1] of char;
      WadapterNames: array[0..1024-1] of widechar;
      i            : integer;
      AdapterName1 : Pchar;
      S            : string;
      Ver          : Twinversion;
begin
   Result := nil;
   NameLength := 1024;
   Ver := pcap_GetwinVersion(S);

   // WINDOWS 95,98 or ME
   if (Ver=wv_Win9x) then
   begin
     GetMem(AdapterName1,NameLength);
     PacketGetAdapterNames(AdapterNames,@NameLength);
     i:=0;
     While i<NameLength do
     begin
       if AdapterNames[i]=' ' then AdapterName1[i]:=#0
                             else AdapterName1[i]:= AdapterNames[i];
       if AdapterNames[i]=#0 then break else inc(i);
     end;

     AdapterName1[i-1] := #0;
     AdapterName1[i+1] := #0;
     AdapterName1[i]   := #0;

     result := Adaptername1;
   end

   // WINDOWS NT,2000 or XP
   Else if (ver=wv_winNT) or (ver=wv_win2000) or (ver=wv_winXP) then
   begin
     Getmem(AdapterName1,1024*Sizeof(char));
     PacketGetAdapterNames(Pchar(@wAdapterNames),@NameLength);

     for i:=0 to NameLength-1 do
     begin
       if (Wadapternames[i]=#0)and(wadapternames[i+1]=#0) then break;
       AdapterName1[i] := char(wAdapterNames[i]);
     end;

     result := adaptername1;
   end;
end;

//------------------------------------------------------------------------------
// int pcap_datalink(pcap_t *p)
//------------------------------------------------------------------------------
// Returns the link type of the device
function pcap_datalink(P:PPcap) : integer;
begin
  result := p.linktype;
end;


//------------------------------------------------------------------------------
// Get OS version // Added By Lars Peter
//------------------------------------------------------------------------------
function pcap_GetWinVersion(var VerStr:string) : TWinVersion;
var
OSversion:OSVERSIONINFO;
begin
  OSversion.dwOSVersionInfoSize:=sizeof(OSVERSIONINFO);
  result := wv_unknown;
  if not GetVersionEx(osversion) then exit;

  with OSversion do begin
  Case dwPlatformId of
    VER_PLATFORM_WIN32s:
      begin
        verStr:=Format('Windows %d.%d',[dwMajorVersion,dwMinorVersion]);
        result:=Wv_wins;
      end;
    VER_PLATFORM_WIN32_WINDOWS:
      begin
  	case dwMinorVersion of
          0 : verstr := 'Windows 95';
         10 : verstr := 'Windows 98';
         90 : verstr := 'Windows Me';
        end;
        Result := Wv_win9x;
      end;
   VER_PLATFORM_WIN32_NT:
     begin
  	 if (dwMajorVersion=5)and (dwMinorVersion=0) then
           begin
              verstr:='Windows 2000';
              if szCSDVersion<>'' then Verstr:=verstr+' with '+szCSDVersion;
              result := wv_win2000;
           end
         else if (dwMajorVersion=5)and(dwMinorVersion=1) then
           begin
             verstr:=Format('Windows XP %s',[szCSDVersion]);
             if szCSDVersion<>'' then Verstr:=verstr+' with '+szCSDVersion;
             result := wv_winxp;
           end
         else if(dwMajorVersion<=4) then
	    begin
              verstr:=Format('Windows NT %d.%d',[dwMajorVersion,dwMinorVersion]);
              if szCSDVersion<>'' then Verstr:=verstr+' with '+szCSDVersion;
              result:=wv_winNT;
            end
         else
             //for newest windows version
	    verstr:=format('Windows %d.%d ',[dwMajorVersion,dwMinorVersion]);
     end;
   end;
  end;
end;

//------------------------------------------------------------------------------
// Get All AdapterNames seperated with chosen delimiter // Added By Lars Peter
//------------------------------------------------------------------------------
function Pcap_GetAdapternames(Delimiter:char;var ErrStr:string):string;
var NameList : Array [0..(4096*2)-1] of char;
    NameLength,
    i        :Longword;
    Ver      :Twinversion;
    s : string; // dummy;
begin
   NameLength := 4096;
   Ver := pcap_GetwinVersion(S);
   FillChar(NameList, sizeof(NameList), 0);
   PacketGetAdapterNames(@NameList, @NameLength);

   Result := '';

   I := 0;
   while I < NameLength do begin
     if Ver = wv_Win9x then
       s := string(PChar(@NameList[I*sizeof(Char)]))
     else
       s := string(PWideChar(@NameList[I*sizeof(WideChar)]));

     if s = '' then
       break;

     if Result <> '' then
       Result := Result + Delimiter;

     Result := Result + s;
     inc(I, Length(s) + 1);
   end;  { while I < NameLength }
end;

end.
