{
********************************************************************************
--------------------------------------------------------------------------------
                              Conversion of BPF.H
                             From C to ObjectPascal

                              - By Lars Peter Christiansen
--------------------------------------------------------------------------------
 If you have any questions, requests, bug reports, etc., please contact me at
 the address given below.

Lars Peter Christiansen
Email  : bakkevej@stofanet.dk
Website: http://home1.stofanet.dk/nitezhifter
--------------------------------------------------------------------------------


********************************************************************************
}
unit bpf;

interface

Type
  // For future combatibility
  Tbpf_u_int32 = LongWord;
  Tbpf_int32   = Integer;

  //[ taken from DRIVER\packet.h ]
  // Unix's way of timestamping.
  PunixTimeVal = ^TunixTimeVal;
  TunixTimeVal = record
    tv_Sec,            // Secs since 1/1/1970
    tv_uSec: LongWord; // microseconds
  end;

  // [ Gotten the following structs from LIBPCAP\Packet32.h]
  Tbpf_insn = record
    code : Word;
    jt   : Byte;
    jf   : Byte;
    k    : Tbpf_u_int32;
  end;

  Pbpf_program = ^Tbpf_program;
  Tbpf_program = record
    bf_len  : LongWord;
    bf_insns: ^Tbpf_insn;
  end;

  Pbpf_stat = ^Tbpf_stat;
  Tbpf_stat = record
    bs_recv,
    bs_drop : LongWord;
  end;

  Pbpf_hdr = ^Tbpf_hdr;        //Structure prepended to each packet.
  Tbpf_hdr =record
    bh_tstamp :TunixTimeval;	//* time stamp */
    bh_caplen,            	//* length of captured portion */
    bh_datalen: Tbpf_u_int32;	//* original length of packet */
    bh_hdrlen : Word ;        	//* length of bpf header (this struct plus
                                //alignment padding) */
  end;

const
  BPF_ALIGNMENT = sizeof(Tbpf_int32);

  DLT_NULL	 =0;	//* no link-layer encapsulation */
  DLT_EN10MB     =1;	//* Ethernet (10Mb) */
  DLT_EN3MB      =2;	//* Experimental Ethernet (3Mb) */
  DLT_AX25       =3;	//* Amateur Radio AX.25 */
  DLT_PRONET     =4;	//* Proteon ProNET Token Ring */
  DLT_CHAOS      =5;	//* Chaos */
  DLT_IEEE802	 =6;	//* IEEE 802 Networks */
  DLT_ARCNET	 =7;	//* ARCNET */
  DLT_SLIP	 =8;	//* Serial Line IP */
  DLT_PPP	 =9;	//* Point-to-point Protocol */
  DLT_FDDI	 =10;	//* FDDI */
  DLT_ATM_RFC1483=11;	//* LLC/SNAP encapsulated atm */
  DLT_RAW	 =12;	//* raw IP */
  DLT_SLIP_BSDOS =13;	//* BSD/OS Serial Line IP */
  DLT_PPP_BSDOS	 =14;	//* BSD/OS Point-to-point Protocol */

  //New types for Win32
  DLT_EN100MB	 =100;	//* Ethernet (100Mb) */
  DLT_PPP_WIN32	 =101;	//* Win32 dial up connection */

(*
 * The instruction encodings.
 *)
(* instruction classes *)
// #define BPF_CLASS(code) ((code) & 0x07)
  BPF_LD		= $00;
  BPF_LDX		= $01;
  BPF_ST		= $02;
  BPF_STX		= $03;
  BPF_ALU		= $04;
  BPF_JMP		= $05;
  BPF_RET		= $06;
  BPF_MISC	= $07;

(* ld/ldx fields *)
// #define BPF_SIZE(code)	((code) & 0x18)
  BPF_W		= $00;
  BPF_H		= $08;
  BPF_B		= $10;
// #define BPF_MODE(code)	((code) & 0xe0)
  BPF_IMM 	= $00;
  BPF_ABS		= $20;
  BPF_IND		= $40;
  BPF_MEM		= $60;
  BPF_LEN		= $80;
  BPF_MSH		= $a0;

(* alu/jmp fields *)
// #define BPF_OP(code)	((code) & 0xf0)
  BPF_ADD		= $00;
  BPF_SUB		= $10;
  BPF_MUL		= $20;
  BPF_DIV		= $30;
  BPF_OR		= $40;
  BPF_AND		= $50;
  BPF_LSH		= $60;
  BPF_RSH		= $70;
  BPF_NEG		= $80;
  BPF_JA		= $00;
  BPF_JEQ		= $10;
  BPF_JGT		= $20;
  BPF_JGE		= $30;
  BPF_JSET	= $40;
// #define BPF_SRC(code)	((code) & 0x08)
  BPF_K		= $00;
  BPF_X		= $08;

(* ret - BPF_K and BPF_X also apply *)
// #define BPF_RVAL(code)	((code) & 0x18)
  BPF_A		= $10;

(* misc *)
//#define BPF_MISCOP(code) ((code) & 0xf8)
  BPF_TAX		= $00;
  BPF_TXA		= $80;


Function BPF_WORDALIGN(X:LongWord) : LongWord;  //Force data to be aligned

implementation

//------------------------------------------------------------------------------
// This was originally a C macro :
//
// #define BPF_WORDALIGN(x) (((x)+(BPF_ALIGNMENT-1))&~(BPF_ALIGNMENT-1))
//
//------------------------------------------------------------------------------
function BPF_WORDALIGN(X:LongWord) : LongWord;
begin
  result := (((X)+(BPF_ALIGNMENT-1))and not(BPF_ALIGNMENT-1));
end;


end.
