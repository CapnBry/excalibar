{$IFDEF PROFILE} {$O-} {$WARNINGS OFF} {$ENDIF }
unit DAOCSkilla_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision$
// File generated on 5/4/2003 11:46:29 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\utils\excalibar\win32\DAOCSkilla\DAOCSkilla.tlb (1)
// LIBID: {E66F5430-EDF3-4527-A88E-8B326F061A6D}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\stdvcl40.dll)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

interface

{$IFNDEF PROFILE}uses ActiveX, Classes, Graphics, StdVCL, Variants, Windows;{$ENDIF}
{$IFDEF PROFILE}uses ActiveX, Classes, Graphics, StdVCL, Variants, Windows ,Profint;{$ENDIF}
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  DAOCSkillaMajorVersion = 1;
  DAOCSkillaMinorVersion = 0;

  LIBID_DAOCSkilla: TGUID = '{E66F5430-EDF3-4527-A88E-8B326F061A6D}';

  IID_IDAOCControl: TGUID = '{56DC5EA8-7D92-4BC7-9C4C-F057190BB814}';
  DIID_IDAOCControlEvents: TGUID = '{663E1317-40D6-40A0-8FEC-58DA85310BA7}';
  CLASS_CDAOCControl: TGUID = '{278CBADD-459D-4C6E-8B1D-378A9977C062}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IDAOCControl = interface;
  IDAOCControlDisp = dispinterface;
  IDAOCControlEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CDAOCControl = IDAOCControl;


// *********************************************************************//
// Interface: IDAOCControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {56DC5EA8-7D92-4BC7-9C4C-F057190BB814}
// *********************************************************************//
  IDAOCControl = interface(IDispatch)
    ['{56DC5EA8-7D92-4BC7-9C4C-F057190BB814}']
    procedure QuitDAOC; safecall;
    procedure SetQuickbarPage(dwPage: Integer); safecall;
    procedure Jump; safecall;
    procedure SendKeys(const bsKeys: WideString); safecall;
    procedure LeftClick(X: Integer; Y: Integer); safecall;
    procedure RightClick(X: Integer; Y: Integer); safecall;
    procedure StopAllActions; safecall;
    procedure Sleep(dwTime: Integer); safecall;
    procedure SelectGroupMember(dwIndex: Integer); safecall;
    procedure Log(const bsMessage: WideString); safecall;
    procedure Face; safecall;
    procedure Follow; safecall;
    procedure Stick; safecall;
    procedure ChatSend(const bsWho: WideString; const bsMessage: WideString); safecall;
    procedure ChatSay(const bsMessage: WideString); safecall;
    procedure ChatGuild(const bsMessage: WideString); safecall;
    procedure ChatAlliance(const bsMessage: WideString); safecall;
    procedure ChatChat(const bsMessage: WideString); safecall;
    procedure ChatGroup(const bsMessage: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IDAOCControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {56DC5EA8-7D92-4BC7-9C4C-F057190BB814}
// *********************************************************************//
  IDAOCControlDisp = dispinterface
    ['{56DC5EA8-7D92-4BC7-9C4C-F057190BB814}']
    procedure QuitDAOC; dispid 1;
    procedure SetQuickbarPage(dwPage: Integer); dispid 2;
    procedure Jump; dispid 3;
    procedure SendKeys(const bsKeys: WideString); dispid 4;
    procedure LeftClick(X: Integer; Y: Integer); dispid 5;
    procedure RightClick(X: Integer; Y: Integer); dispid 6;
    procedure StopAllActions; dispid 7;
    procedure Sleep(dwTime: Integer); dispid 8;
    procedure SelectGroupMember(dwIndex: Integer); dispid 9;
    procedure Log(const bsMessage: WideString); dispid 10;
    procedure Face; dispid 11;
    procedure Follow; dispid 12;
    procedure Stick; dispid 13;
    procedure ChatSend(const bsWho: WideString; const bsMessage: WideString); dispid 14;
    procedure ChatSay(const bsMessage: WideString); dispid 15;
    procedure ChatGuild(const bsMessage: WideString); dispid 16;
    procedure ChatAlliance(const bsMessage: WideString); dispid 17;
    procedure ChatChat(const bsMessage: WideString); dispid 18;
    procedure ChatGroup(const bsMessage: WideString); dispid 19;
  end;

// *********************************************************************//
// DispIntf:  IDAOCControlEvents
// Flags:     (4096) Dispatchable
// GUID:      {663E1317-40D6-40A0-8FEC-58DA85310BA7}
// *********************************************************************//
  IDAOCControlEvents = dispinterface
    ['{663E1317-40D6-40A0-8FEC-58DA85310BA7}']
    procedure OnConnect; dispid 1;
    procedure OnDisconnect; dispid 2;
    procedure OnChatSendIncoming(const bsWho: WideString; const bwMessage: WideString); dispid 3;
    procedure OnPopupMessage(const bsMessage: WideString); dispid 4;
    procedure OnTradeskillCommissionAssigned; dispid 5;
    procedure OnTradeskillTaskCompleted; dispid 6;
    procedure OnTradeskillSuccess(iQuality: Integer); dispid 7;
    procedure OnTradeskillFailure; dispid 8;
    procedure OnTradeskillCapped; dispid 9;
    procedure OnPlayerPosUpdate; dispid 10;
    procedure OnCharacterLogin; dispid 11;
    procedure OnArriveAtGotoDest; dispid 12;
    procedure OnPathChanged; dispid 13;
    procedure OnSelectedObjectChanged; dispid 14;
    procedure OnCombatStyleSuccess(const sStyleName: WideString); dispid 15;
    procedure OnCombatStyleFailure; dispid 16;
  end;

// *********************************************************************//
// The Class CoCDAOCControl provides a Create and CreateRemote method to          
// create instances of the default interface IDAOCControl exposed by              
// the CoClass CDAOCControl. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCDAOCControl = class
    class function Create: IDAOCControl;
    class function CreateRemote(const MachineName: string): IDAOCControl;
  end;

implementation

uses ComObj;

class function CoCDAOCControl.Create: IDAOCControl;
begin
{$IFDEF PROFILE}asm DW 310FH; call Profint.ProfStop; end; Try; asm mov edx,43; mov eax,self; call Profint.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
  Result := CreateComObject(CLASS_CDAOCControl) as IDAOCControl;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,43; call Profint.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

class function CoCDAOCControl.CreateRemote(const MachineName: string): IDAOCControl;
begin
{$IFDEF PROFILE}asm DW 310FH; call Profint.ProfStop; end; Try; asm mov edx,44; mov eax,self; call Profint.ProfEnter; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; {$ENDIF}
  Result := CreateRemoteComObject(MachineName, CLASS_CDAOCControl) as IDAOCControl;
{$IFDEF PROFILE}finally; asm DW 310FH; mov ecx,44; call Profint.ProfExit; mov ecx,eax; DW 310FH; add[ecx].0,eax; adc[ecx].4,edx; end; end; {$ENDIF}
end;

end.
