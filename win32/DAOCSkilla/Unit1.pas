unit Unit1;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, WinSock,
  DAOCControl, DAOCConnection, ExtCtrls, StdCtrls, INIFiles,
  Buttons, DAOCSkilla_TLB, DAOCObjs, Dialogs, GameNetPackets, DAOCPlayerAttributes,
  DAOCInventory, Recipes, IdBaseComponent, QuickLaunchChars, IdHTTP, ShellAPI,
  MapNavigator, IdComponent, IdException, IdTCPConnection, IdTCPClient, DStreamClient,
  DAOCControlList, DStrmServerListFrame, ActnList, Menus;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    lblUpdates: TLabel;
    tmrUpdateCheck: TTimer;
    httpUpdateChecker: TIdHTTP;
    frmDStrmServerList1: TfrmDStrmServerList;
    mnuMain: TMainMenu;
    atlMain: TActionList;
    atnRadar: TAction;
    atnAutoLaunchRadar: TAction;
    atnChatLog: TAction;
    atnChangeChatLogFile: TAction;
    atnSetDaocPath: TAction;
    atnRemoteAdminEnable: TAction;
    atnDumpMobList: TAction;
    atnDumpPacketDataToLog: TAction;
    atnProcessPackets: TAction;
    atnTrackLogins: TAction;
    atnCaptureMobseen: TAction;
    atnCaptureDelveseen: TAction;
    Network1: TMenuItem;
    Logging1: TMenuItem;
    Windows1: TMenuItem;
    atnRemoteTelnetServer1: TMenuItem;
    Processpackets1: TMenuItem;
    atnRealTimeChatLog1: TMenuItem;
    Setrealtimechatlogfile1: TMenuItem;
    Capturemobupdatestomobseen1: TMenuItem;
    Capturedelveinfotodelveseen1: TMenuItem;
    Dumppacketdatatolog1: TMenuItem;
    atnDumpMobList1: TMenuItem;
    Viewradar1: TMenuItem;
    Autolaunchradarwindow1: TMenuItem;
    SetDAoCpath1: TMenuItem;
    Quicklaunch1: TMenuItem;
    rackcharacterlogins1: TMenuItem;
    lblChatLogFile: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnDebuggingClick(Sender: TObject);
    procedure chkChatLogClick(Sender: TObject);
    procedure btnMacroingClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnDeleteCharClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure cbxAutoLoginKeyPress(Sender: TObject; var Key: Char);
    procedure tmrUpdateCheckTimer(Sender: TObject);
    procedure lblUpdatesClick(Sender: TObject);
    procedure chkTrackLoginsClick(Sender: TObject);
    procedure lblRemoteAdminEnableClick(Sender: TObject);
    procedure atnSetDaocPathExecute(Sender: TObject);
    procedure atnRadarExecute(Sender: TObject);
  private
{$IFDEF DAOC_AUTO_SERVER}
    FIConnection: IDAOCControl;
{$ENDIF DAOC_AUTO_SERVER}
    FChatLog:     TFileStream;
    FClosing:     boolean;
    FCheckForUpdates:   boolean;
    FLastUpdateCheck:   TDateTime;
    FProcessPackets:    boolean;
    FChatLogXIEnabled:  boolean;
    FRemoteAdminEnabled:  boolean;
    FDAOCPath:          string;
    FDStreamClients:    TDStreamClientList;
    FDControlList:      TDAOCControlList;

    procedure LoadSettings;
    procedure LoadSettingsForConnection(AConn: TDAOCConnection);
    procedure SaveSettings;
    procedure SaveSettingsForConnection(AConn: TDAOCConnection);
    procedure SetupDAOCConnectionObj(AConn: TDAOCConnection);
    procedure SetupDStreamObj;
    function GetConfigFileName : string;
    procedure ShowGLRenderer;
    procedure CreateChatLog;
    procedure CloseChatLog;
    procedure OpenRemoteAdmin;
    procedure CloseRemoteAdmin;
    procedure UpdateQuickLaunchList;
    procedure UpdateQuickLaunchProfileList;
    procedure ChatLogXI(const s: string);
    procedure LogLocalPlayerXI;
    procedure SetRemoteAdminEnabled(const Value: boolean);
  protected
    procedure CONNLISTNewConnection(Sender: TObject; AConn: TDAOCConnection);
    procedure CONNLISTDeleteConnection(Sender: TObject; AConn: TDAOCConnection);
    procedure DAOCRegionChanged(Sender: TObject);
    procedure DAOCPlayerPosUpdate(Sender: TObject);
    procedure DAOCConnect(Sender: TObject);
    procedure DAOCDisconnect(Sender: TObject);
    procedure DAOCLog(Sender: TObject; const s: string);
    procedure DAOCZoneChange(Sender: TObject);
    procedure DAOCPacket(Sender: TObject; APacket: TGameNetPacket);
    procedure DAOCAfterPacket(Sender: TObject; APacket: TGameNetPacket);
    procedure DAOCInventoryChanged(Sender: TObject);
    procedure DAOCVendorWindow(Sender: TObject);
    procedure DAOCPathChanged(Sender: TObject);
    procedure DAOCStopAllActions(Sender: TObject);
    procedure DAOCNewObject(Sender: TObject; ADAOCObject: TDAOCObject);
    procedure DAOCDeleteObject(Sender: TObject; ADAOCObject: TDAOCObject);
    procedure DAOCObjectMoved(Sender: TObject; ADAOCObject: TDAOCObject);
    procedure DAOCSkillLevelChanged(Sender: TObject; AItem: TDAOCNameValuePair);
    procedure DAOCSelectedObjectChanged(Sender: TObject; ADAOCObject: TDAOCObject);
    procedure DAOCSetGroundTarget(Sender: TObject);
    procedure DAOCChatLog(Sender: TObject; const s: string);
    procedure DAOCPingReply(Sender: TObject; ATime: integer);
    procedure DAOCCharacterLogin(Sender: TObject);
    procedure DAOCUnknownStealther(Sender: TObject; AUnk: TDAOCObject);
    procedure DAOCDelveItem(Sender: TObject; AItem: TDAOCInventoryItem);
    procedure DAOCArriveAtGotoDest(Sender: TObject; ANode: TMapNode);
    procedure DAOCSelectNPCSuccess(Sender: TObject);
    procedure DAOCSelectNPCFailed(Sender: TObject);
    procedure DAOCAttemptNPCRightClickFailed(Sender: TObject);
    procedure DAOCLocalHealthUpdate(Sender: TObject);
    procedure DAOCDoorPositionUpdate(Sender: TObject; ADoor: TDAOCObject);

    procedure DSTREAMDAOCConnect(Sender: TObject;
      AConnectionID: Cardinal; AServerIP: Cardinal; AServerPort: WORD;
      AClientIP: Cardinal; AClientPort: WORD);
    procedure DSTREAMDAOCData(Sender: TObject;
      AConnectionID: Cardinal; AIsFromClient: boolean; AIsTCP: boolean;
      AData: Pointer; ADataLen: integer);
    procedure DSTREAMDAOCDisconnect(Sender: TObject;
      AConnectionID: Cardinal; AServerIP: Cardinal; AServerPort: WORD;
      AClientIP: Cardinal; AClientPort: WORD);
    procedure DSTREAMStatusChange(Sender: TObject);
  public
    procedure Log(const s: string);
    procedure InjectPacket(ASource: TObject; APacket: TGameNetPacket);

    property ProcessPackets: boolean read FProcessPackets write FProcessPackets;
    property RemoteAdminEnabled: boolean read FRemoteAdminEnabled write SetRemoteAdminEnabled;
  end;

var
  frmMain: TfrmMain;

procedure CreateOptionalForms;

implementation

uses
  PowerSkillSetup, ShowMapNodes, MacroTradeSkill, AFKMessage,
  SpellcraftHelp, Macroing, LowOnStat, VCLMemStrms, RemoteAdmin,
  StringParseHlprs, DebugAndTracing
{$IFDEF OPENGL_RENDERER}
  ,GLRender
{$ENDIF OPENGL_RENDERER}
{$IFDEF DAOC_AUTO_SERVER}
  ,TellMacro
{$ENDIF DAOC_AUTO_SERVER}
  ;

{$R *.dfm}

const
  CHAT_XI_PREFIX = 'XI: ';

procedure CreateOptionalForms;
begin
{$IFDEF OPENGL_RENDERER}
  Application.CreateForm(TfrmGLRender, frmGLRender);
  // frmGLRender.DAOCControl := frmMain.FConnection;
{$ENDIF OPENGL_RENDERER}

{$IFDEF DAOC_AUTO_SERVER}
  Application.CreateForm(TfrmTellMacro, frmTellMacro);
{$ENDIF DAOC_AUTO_SERVER}
end;

function GetVersionString : string;
var
  InfoSize:     DWORD;
  Wnd:          DWORD;
  VerBuf:       Pointer;
  VerSize:      DWORD;
  FileInfo:     PVSFixedFileInfo;
begin
    { Get version information to show }
  InfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Wnd);
  if InfoSize <> 0 then begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(ParamStr(0)), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FileInfo), VerSize) then begin
          Result := Format('%d.%d', [
            FileInfo.dwFileVersionMS shr 16,
            FileInfo.dwFileVersionMS and $FFFF]);
            // FileInfo.dwFileVersionLS shr 16,
            // FileInfo.dwFileVersionLS and $FFFF]);
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;

  Log('--==========================================================--');
  Log('  DaocSkilla is provided at no cost and without warranty');
  Log('    under the General Public License (GPL) Version 2.');
  Log('  See LICENSE.TXT for more information regarding licensing.');
  Log('--==========================================================--');

  FDControlList := TDAOCControlList.Create;
  FDControlList.OnNewConnection := CONNLISTNewConnection;
  FDControlList.OnDeleteConnection := CONNLISTDeleteConnection;
  SetupDStreamObj;

  FProcessPackets := true;

{$IFNDEF OPENGL_RENDERER}
  btnGLRender.Visible := false;
{$ENDIF}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  CloseChatLog
end;

procedure TfrmMain.DAOCConnect(Sender: TObject);
var
  pConn:  TDAOCControl;
begin
  pConn := TDAOCControl(Sender);
  // lblServerPing.Caption := 'Connected';
  Log('New connection: ' + pConn.ClientIP + '->' + pConn.ServerIP);

{$IFDEF OPENGL_RENDERER}
  if atnAutoLaunchRadar.Checked then
    ShowGLRenderer;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCDisconnect(Sender: TObject);
begin
  CloseChatLog;
  // lblServerPing.Caption := 'Disconnected';

{$IFDEF OPENGL_RENDERER}
  if atnAutoLaunchRadar.Checked then
    frmGLRender.Close;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCPlayerPosUpdate(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCPlayerPosUpdate(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.SetupDAOCConnectionObj(AConn: TDAOCConnection);
begin
  with TDAOCControl(AConn) do begin
    OnPlayerPosUpdate := DAOCPlayerPosUpdate;
    OnLog := DAOCLog;
    OnConnect := DAOCConnect;
    OnDisconnect := DAOCDisconnect;
    OnZoneChange := DAOCZoneChange;
    OnAfterPacket := DAOCAfterPacket;
//  OnPacket := DAOCPacket;
    OnInventoryChanged := DAOCInventoryChanged;
    OnVendorWindow := DAOCVendorWindow;
    OnPathChanged := DAOCPathChanged;
    OnStopAllActions := DAOCStopAllActions;
    OnNewDAOCObject := DAOCNewObject;
    OnDeleteDAOCObject := DAOCDeleteObject;
    OnDAOCObjectMoved := DAOCObjectMoved;
    OnSkillLevelChanged := DAOCSkillLevelChanged;
    OnSelectedObjectChange := DAOCSelectedObjectChanged;
    OnRegionChanged := DAOCRegionChanged;
    OnSetGroundTarget := DAOCSetGroundTarget;
    OnChatLog := DAOCChatLog;
    OnPingReply := DAOCPingReply;
    OnCharacterLogin := DAOCCharacterLogin;
    OnUnknownStealther := DAOCUnknownStealther;
    OnDelveItem := DAOCDelveItem;
    OnArriveAtGotoDest := DAOCArriveAtGotoDest;
    OnSelectNPCSuccess := DAOCSelectNPCSuccess;
    OnSelectNPCFailed := DAOCSelectNPCFailed;
    OnAttemptNPCRightClickFailed := DAOCAttemptNPCRightClickFailed;
    OnLocalHealthUpdate := DAOCLocalHealthUpdate;

    DAOCPath := FDAOCPath;
    MainHWND := Handle;
  end;
  
  LoadSettingsForConnection(AConn);

{$IFDEF DAOC_AUTO_SERVER}
  FIConnection := FConnection as IDAOCControl;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
    { setup the adapterlist first, so the load settings can set the active
      adapter properly }
  LoadSettings;
  // dmdRemoteAdmin.DAOCControl := FConnection;
  SetRemoteAdminEnabled(FRemoteAdminEnabled);

  UpdateQuickLaunchList;
  UpdateQuickLaunchProfileList;

  FDStreamClients.OpenAll;
end;

procedure TfrmMain.DAOCLog(Sender: TObject; const s: string);
begin
  Log(s);
end;

procedure TfrmMain.DAOCZoneChange(Sender: TObject);
begin
  frmDebugging.DAOCZoneChange;
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCZoneChanged(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.LoadSettings;
begin
  with TINIFile.Create(GetConfigFileName) do begin
    FDAOCPath := ReadString('Main', 'DAOCPath', 'C:\Mythic\Isles\');;
    Left := ReadInteger('Main', 'Left', Left);
    Top := ReadInteger('Main', 'Top', Top);
    FRemoteAdminEnabled := ReadBool('Main', 'RemoteAdminEnabled', true);
    atnAutoLaunchRadar.Checked := ReadBool('Main', 'AutolaunchExcal', true);
    atnChatLog.Checked := ReadBool('Main', 'RealtimeChatLog', false);
    lblChatLogFile.Caption := ReadString('Main', 'ChatLogFile', ExtractFilePath(ParamStr(0)) + 'realchat.log');
    //btnMacroing.Visible := ReadBool('Main', 'EnableMacroing', false);
    FCheckForUpdates := ReadBool('Main', 'CheckForUpdates', true);
    FLastUpdateCheck := ReadDateTime('Main', 'LastUpdateCheck', 0);
    FChatLogXIEnabled := ReadBool('Main', 'ChatLogXIEnabled', true);

    frmPowerskill.Profile := ReadString('PowerskillBuy', 'Profile', 'spellcrafting-example');
    frmPowerskill.AutoAdvance := ReadBool('PowerskillBuy', 'AutoAdvance', true);
    frmPowerskill.Left := ReadInteger('PowerskillBuy', 'Left', frmPowerskill.Left);
    frmPowerskill.Top := ReadInteger('PowerskillBuy', 'Top', frmPowerskill.Top);
    frmPowerskill.LastQuickSlot := ReadString('PowerskillBuy', 'LastQuickSlot', frmPowerskill.LastQuickSlot);
    frmPowerskill.UngroupRecipes := ReadBool('PowerskillBuy', 'UngroupRecipes', frmPowerskill.UngroupRecipes);

    frmMacroTradeSkills.Progression := ReadString('MacroTradeSkills', 'Progression', '0');
    frmMacroTradeSkills.TargetQuality := ReadInteger('MacroTradeSkills', 'TargetQuality', 0);
    frmMacroTradeSkills.TargetSound := ReadString('MacroTradeSkills', 'TargetSound', 'beep');
    frmMacroTradeSkills.StopIfFull := ReadBool('MacroTradeSkills', 'StopIfFull', true);
    frmMacroTradeSkills.OddsLoadKey := ReadString('MacroTradeSkills', 'OddsLoadKey', '');
    frmMacroTradeSkills.OddsLoadCount := ReadInteger('MacroTradeSkills', 'OddsLoadCount', 50);
    frmMacroTradeSkills.OddsLoadPct := ReadFloat('MacroTradeSkills', 'OddsLoadPct', 2.0);
    frmMacroTradeSkills.Left := ReadInteger('MacroTradeSkills', 'Left', frmMacroTradeSkills.Left);
    frmMacroTradeSkills.Top := ReadInteger('MacroTradeSkills', 'Top', frmMacroTradeSkills.Top);

    frmAFK.AFKMessage := ReadString('AFKMessage', 'Message', 'I am AFK!');
    frmAFK.Left := ReadInteger('AFKMessage', 'Left', frmAFK.Left);
    frmAFK.Top := ReadInteger('AFKMessage', 'Top', frmAFK.Top);

{$IFDEF DAOC_AUTO_SERVER}
    frmTellMacro.FileName := ReadString('MacroFile', 'Filename', frmTellMacro.FileName);
    frmTellMacro.Left := ReadInteger('MacroFile', 'Left', frmTellMacro.Left);
    frmTellMacro.Top := ReadInteger('MacroFile', 'Top', frmTellMacro.Top);
{$ENDIF DAOC_AUTO_SERVER}

    frmSpellcraftHelp.Left := ReadInteger('SpellcraftHelp', 'Left', frmSpellcraftHelp.Left);
    frmSpellcraftHelp.Top := ReadInteger('SpellcraftHelp', 'Top', frmSpellcraftHelp.Top);
    frmSpellcraftHelp.Height := ReadInteger('SpellcraftHelp', 'Height', frmSpellcraftHelp.Height);
    frmSpellcraftHelp.CraftRealm := TCraftRealm(ReadInteger('SpellcraftHelp', 'CraftRealm', ord(frmSpellcraftHelp.CraftRealm)));

    frmDebugging.CaptureFile := ReadString('Debugging', 'CaptureFile', 'c:\savedcap.cap');
    frmDebugging.Left := ReadInteger('Debugging', 'Left', frmDebugging.Left);
    frmDebugging.Top := ReadInteger('Debugging', 'Top', frmDebugging.Top);

    frmMacroing.Left := ReadInteger('Macroing', 'Left', frmMacroing.Left);
    frmMacroing.Top := ReadInteger('Macroing', 'Top', frmMacroing.Top);
    frmMacroing.TrinketList := ReadString('Macroing', 'TrinketList', frmMacroing.TrinketList);

    frmLowOnStat.Left := ReadInteger('LowOnStat', 'Left', frmLowOnStat.Left);
    frmLowOnStat.Top := ReadInteger('LowOnStat', 'Top', frmLowOnStat.Top);
    frmLowOnStat.LowHealthEnabled := ReadBool('LowOnStat', 'LowHealthEnabled', frmLowOnStat.LowHealthEnabled);
    frmLowOnStat.LowHealthPct := ReadInteger('LowOnStat', 'LowHealthPct', frmLowOnStat.LowHealthPct);
    frmLowOnStat.LowHealthMessage := ReadString('LowOnStat', 'LowHealthMessage', frmLowOnStat.LowHealthMessage);
    frmLowOnStat.LowEnduranceEnabled := ReadBool('LowOnStat', 'LowEnduranceEnabled', frmLowOnStat.LowEnduranceEnabled);
    frmLowOnStat.LowEndurancePct := ReadInteger('LowOnStat', 'LowEndurancePct', frmLowOnStat.LowEndurancePct);
    frmLowOnStat.LowEnduranceMessage := ReadString('LowOnStat', 'LowEnduranceMessage', frmLowOnStat.LowEnduranceMessage);
    frmLowOnStat.LowManaEnabled := ReadBool('LowOnStat', 'LowManaEnabled', frmLowOnStat.LowManaEnabled);
    frmLowOnStat.LowManaPct := ReadInteger('LowOnStat', 'LowManaPct', frmLowOnStat.LowManaPct);
    frmLowOnStat.LowManaMessage := ReadString('LowOnStat', 'LowManaMessage', frmLowOnStat.LowManaMessage);

    dmdRemoteAdmin.PS1 := Dequote(ReadString('RemoteAdmin', 'PS1', dmdRemoteAdmin.PS1));
    Free;
  end;  { with INI }

  Self.Caption := 'DAOCSkilla ' + GetVersionString + ' - ' + FDAOCPath;
//  chkTrackLogins.Checked := FConnection.TrackCharacterLogins;
    { set up chat log if applicable }
  chkChatLogClick(nil);
end;

procedure TfrmMain.SaveSettings;
begin
  with TINIFile.Create(GetConfigFileName) do begin
    WriteInteger('Main', 'Left', Left);
    WriteInteger('Main', 'Top', Top);
    WriteBool('Main', 'AutolaunchExcal', atnAutoLaunchRadar.Checked);
    WriteBool('Main', 'RealtimeChatLog', atnChatLog.Checked);
    WriteString('Main', 'ChatLogFile', lblChatLogFile.Caption);
    WriteBool('Main', 'TrackLogins', atnTrackLogins.Checked);
    WriteDateTime('Main', 'LastUpdateCheck', FLastUpdateCheck);
    WriteBool('Main', 'ChatLogXIEnabled', FChatLogXIEnabled);

    WriteBool('Main', 'RemoteAdminEnabled', FRemoteAdminEnabled);

    WriteString('PowerskillBuy', 'Profile', frmPowerskill.Profile);
    WriteBool('PowerskillBuy', 'AutoAdvance', frmPowerskill.AutoAdvance);
    WriteString('PowerskillBuy', 'LastQuickSlot', frmPowerskill.LastQuickSlot);
    WriteInteger('PowerskillBuy', 'Left', frmPowerskill.Left);
    WriteInteger('PowerskillBuy', 'Top', frmPowerskill.Top);
    WriteBool('PowerskillBuy', 'UngroupRecipes', frmPowerskill.UngroupRecipes);

    WriteString('MacroTradeSkills', 'Progression', frmMacroTradeSkills.Progression);
    WriteInteger('MacroTradeSkills', 'TargetQuality', frmMacroTradeSkills.TargetQuality);
    WriteString('MacroTradeSkills', 'TargetSound', frmMacroTradeSkills.TargetSound);
    WriteBool('MacroTradeSkills', 'StopIfFull', frmMacroTradeSkills.StopIfFull);
    WriteString('MacroTradeSkills', 'OddsLoadKey', frmMacroTradeSkills.OddsLoadKey);
    WriteInteger('MacroTradeSkills', 'OddsLoadCount', frmMacroTradeSkills.OddsLoadCount);
    WriteFloat('MacroTradeSkills', 'OddsLoadPct', frmMacroTradeSkills.OddsLoadPct);
    WriteInteger('MacroTradeSkills', 'Left', frmMacroTradeSkills.Left);
    WriteInteger('MacroTradeSkills', 'Top', frmMacroTradeSkills.Top);

    WriteString('AFKMessage', 'Message', frmAFK.AFKMessage);
    WriteInteger('AFKMessage', 'Left', frmAFK.Left);
    WriteInteger('AFKMessage', 'Top', frmAFK.Top);

{$IFDEF DAOC_AUTO_SERVER}
    WriteString('MacroFile', 'Filename', frmTellMacro.FileName);
    WriteInteger('MacroFile', 'Left', frmTellMacro.Left);
    WriteInteger('MacroFile', 'Top', frmTellMacro.Top);
{$ENDIF DAOC_AUTO_SERVER}

    WriteInteger('SpellcraftHelp', 'Left', frmSpellcraftHelp.Left);
    WriteInteger('SpellcraftHelp', 'Top', frmSpellcraftHelp.Top);
    WriteInteger('SpellcraftHelp', 'Height', frmSpellcraftHelp.Height);
    WriteInteger('SpellcraftHelp', 'CraftRealm', ord(frmSpellcraftHelp.CraftRealm));

    WriteString('Debugging', 'CaptureFile', frmDebugging.CaptureFile);
    WriteInteger('Debugging', 'Left', frmDebugging.Left);
    WriteInteger('Debugging', 'Top', frmDebugging.Top);

    WriteInteger('Macroing', 'Left', frmMacroing.Left);
    WriteInteger('Macroing', 'Top', frmMacroing.Top);
    WriteString('Macroing', 'TrinketList', frmMacroing.TrinketList);

    WriteString('RemoteAdmin', 'PS1', Quote(dmdRemoteAdmin.PS1));
    Free;
  end;  { with INI }
end;

procedure TfrmMain.DAOCAfterPacket(Sender: TObject; APacket: TGameNetPacket);
begin
  frmDebugging.DAOCAfterPacket(APacket);
end;

procedure TfrmMain.DAOCInventoryChanged(Sender: TObject);
begin
  frmMacroing.DAOCInventoryChanged;
end;

procedure TfrmMain.DAOCVendorWindow(Sender: TObject);
begin
  frmMacroing.DAOCVendorWindow;
end;

procedure TfrmMain.Log(const s: string);
begin
  Memo1.Lines.Add(s);
end;

procedure TfrmMain.DAOCPathChanged(Sender: TObject);
begin
  frmMacroing.DAOCPathChanged;
end;

procedure TfrmMain.DAOCStopAllActions(Sender: TObject);
begin
  frmMacroing.DAOCStopAllActions;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
{$IFDEF OPENGL_RENDERER}
  if frmGLRender.Visible then
    frmGLRender.Close;
{$ENDIF OPENGL_RENDERER}

  FDStreamClients.Clear;

    { we want to free the connection before our destroy because the connection
      might fire callbacks as it closes.  Firing a callback to a sub-form
      which is already destroyed is a bad thing }
{$IFDEF DAOC_AUTO_SERVER}
  FConnection := nil;
  FIConnection := nil;  // interface release frees obj
{$ELSE}
//  FConnection.Free;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMain.DAOCDeleteObject(Sender: TObject; ADAOCObject: TDAOCObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCDeleteObject(Sender, ADAOCObject);
{$ENDIF OPENGL_RENDERER}
//  Log('Deleteing: ' + ADaocObject.Name + ' longest update delta ' + IntToStr(ADAOCObject.LongestUpdateTime));
end;

procedure TfrmMain.DAOCNewObject(Sender: TObject; ADAOCObject: TDAOCObject);
begin
  frmDebugging.DAOCNewObject(ADAOCObject);
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCAddObject(Sender, ADAOCObject);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCObjectMoved(Sender: TObject; ADAOCObject: TDAOCObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCUpdateObject(Sender, ADAOCObject);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCSkillLevelChanged(Sender: TObject; AItem: TDAOCNameValuePair);
begin
  frmMacroing.DAOCSkillLevelChanged(AItem);
end;

function TfrmMain.GetConfigFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TfrmMain.DAOCSelectedObjectChanged(Sender: TObject; ADAOCObject: TDAOCObject);
begin
  if Assigned(ADAOCObject) and (ADAOCObject.ObjectClass = ocMob) then begin
    ChatLogXI(Format('New Target: "%s" Level: %d Health: %d%%',
      [ADAOCObject.Name, ADAOCObject.Level, ADAOCObject.HitPoints]));
    LogLocalPlayerXI;
  end;
  
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCSelectedObjectChanged(Sender, ADAOCObject);
{$ENDIF OPENGL_RENDERER}
//  if Assigned(ADAOCObject) then
//    Log('Largest update delta: ' + IntToStr(ADAOCObject.LongestUpdateTime));
end;

procedure TfrmMain.DAOCRegionChanged(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCRegionChanged(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCSetGroundTarget(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCSetGroundTarget(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.btnDebuggingClick(Sender: TObject);
begin
  // frmDebugging.DAOCControl := FConnection;

  if frmDebugging.Visible then
    frmDebugging.Close
  else
    frmDebugging.Show;
end;

procedure TfrmMain.ShowGLRenderer;
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCConnectionList := FDControlList;
  frmGLRender.PrefsFile := GetConfigFileName;
  frmGLRender.Show;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.chkChatLogClick(Sender: TObject);
begin
  atnChangeChatLogFile.Enabled := not atnChatLog.Checked;

  if not atnChatLog.Checked then
    CloseChatLog;
end;

procedure TfrmMain.CreateChatLog;
var
  sOpenLine:    string;
  sDirectory:   string;
begin
  if Assigned(FChatLog) then
    CloseChatLog;

    { make sure we have a file, Delphi 6 will not respect the share mode on an fmCreate }
  if not FileExists(lblChatLogFile.Caption) then begin
    sDirectory := ExtractFilePath(lblChatLogFile.Caption);
      { make sure the directory exists }
    if sDirectory <> '' then
      ForceDirectories(sDirectory);
    try
      FChatLog := TFileStream.Create(lblChatLogFile.Caption, fmCreate);
    except
      on E: Exception do begin
          { if we get an exception, log it and turn off the chat file }
        atnChatLog.Checked := false;
        atnChatLog.Execute;
        Log(e.Message);
        exit;
      end;
    end;
    FreeAndNil(FChatLog);
  end;  { if creating a new file }

  FChatLog := TFileStream.Create(lblChatLogFile.Caption, fmOpenWrite or fmShareDenyNone);
  FChatLog.Seek(0, soFromEnd);

  sOpenLine := #13#10'*** Chat Log Opened: ' +
    FormatDateTime('ddd mmm dd hh:nn:ss yyyy', Now) + // Tue Jan 08 08:09:33 2002
    #13#10#13#10;
  FChatLog.Write(sOpenLine[1], Length(sOpenLine));
end;

procedure TfrmMain.CloseChatLog;
var
  sCloseLine: string;
begin
  if Assigned(FChatLog) then begin
    sCloseLine := #13#10'*** Chat Log Closed: ' +
      FormatDateTime('ddd mmm dd hh:nn:ss yyyy', Now) + // Tue Jan 08 08:09:33 2002
      #13#10#13#10#13#10;
    FChatLog.Write(sCloseLine[1], Length(sCloseLine));

    FChatLog.Free;
    FChatLog := nil;
  end;
end;

procedure TfrmMain.DAOCChatLog(Sender: TObject; const s: string);
var
  sChatLogLine:   string;
begin
  sChatLogLine := FormatDateTime('[hh:nn:ss] ', Now) + s + #13#10;

  if atnChatLog.Checked then begin
    if not Assigned(FChatLog) then
      CreateChatLog;

    if Assigned(FChatLog) then
      FChatLog.Write(sChatLogLine[1], Length(sChatLogLine))
  end;

  dmdRemoteAdmin.DAOCChatLog(sChatLogLine);
end;

procedure TfrmMain.btnMacroingClick(Sender: TObject);
begin
  // frmMacroing.DAOCControl := FConnection;
  if frmMacroing.Visible then
    frmMacroing.Close
  else
    frmMacroing.Show;
end;

procedure TfrmMain.DAOCPingReply(Sender: TObject; ATime: integer);
begin
//  lblServerPing.Caption := 'Server ping ' + IntToStr(ATime) + 'ms';
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FClosing := true;
end;

procedure TfrmMain.DAOCPacket(Sender: TObject; APacket: TGameNetPacket);
begin
  frmDebugging.DAOCPacket(APacket);
end;

procedure TfrmMain.DAOCCharacterLogin(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCCharacterLogin(Sender);
{$ENDIF OPENGL_RENDERER}

  if atnTrackLogins.Checked then
    UpdateQuickLaunchList;
end;

procedure TfrmMain.UpdateQuickLaunchList;
//var
//  I:    integer;
begin

exit;
(***
  cbxAutoLogin.Clear;
  for I := 0 to FConnection.QuickLaunchChars.Count - 1 do
    with FConnection do
      cbxAutoLogin.Items.AddObject(QuickLaunchChars[I].DisplayName, QuickLaunchChars[I]);

  if cbxAutoLogin.Items.Count > 0 then begin
    cbxAutoLogin.ItemIndex := 0;
    btnLogin.Enabled := true;
    btnDeleteChar.Enabled := true;
  end
  else begin
    btnLogin.Enabled := false;
    btnDeleteChar.Enabled := false;
  end;
  **)
end;

procedure TfrmMain.btnDeleteCharClick(Sender: TObject);
begin
(**
  if cbxAutoLogin.ItemIndex < FConnection.QuickLaunchChars.Count then begin
    FConnection.QuickLaunchChars.Delete(cbxAutoLogin.ItemIndex);
    UpdateQuickLaunchList;
  end;
  ***)
end;

procedure TfrmMain.btnLoginClick(Sender: TObject);
begin
(****
  if cbxAutoLoginProfile.ItemIndex > 0 then
    TQuickLaunchProfile(
      cbxAutoLoginProfile.Items.Objects[
        cbxAutoLoginProfile.ItemIndex
      ]).Activate(FConnection.DAOCPath);

  FConnection.LaunchCharacterIdx(cbxAutoLogin.ItemIndex);
***)
end;

procedure TfrmMain.cbxAutoLoginKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    Key := #0;
    btnLoginClick(nil);
  end;
end;

procedure TfrmMain.tmrUpdateCheckTimer(Sender: TObject);
var
  sVer:     string;
  FS:       TFileStream;
  sVerFile: string;
begin
  tmrUpdateCheck.Enabled := false;
  if not FCheckForUpdates or ((Now - FLastUpdateCheck) < 7) then
    exit;

  FLastUpdateCheck := Now;

  lblUpdates.Caption := 'Checking for updates...';
  lblUpdates.Visible := true;
  lblUpdates.Update;

  sVerFile := ExtractFilePath(ParamStr(0)) + 'versions.ini';
  FS := TFileStream.Create(sVerFile, fmCreate);
  try
    httpUpdateChecker.Get('http://capnbry.net/daoc/versions.php', FS);
  except
  end;
  FS.Free;

  with TINIFile.Create(sVerFile) do begin
    sVer := ReadString('main', 'daocskilla.ver', '');
    Free;
  end;

  if sVer <> GetVersionString then
    lblUpdates.Caption := 'Latest version is ' + sVer
  else
    lblUpdates.Visible := false;
end;

procedure TfrmMain.lblUpdatesClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://capnbry.net/daoc/daocskilla.php', nil, nil, SW_SHOWNORMAL);
  lblUpdates.Visible := false;
end;

procedure TfrmMain.chkTrackLoginsClick(Sender: TObject);
begin
  // FConnection.TrackCharacterLogins := atnTrackLogins.Checked;
end;

procedure TfrmMain.DAOCUnknownStealther(Sender: TObject; AUnk: TDAOCObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCUnknownStealther(Sender, AUnk);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCDelveItem(Sender: TObject; AItem: TDAOCInventoryItem);
begin
  ChatLogXI('DELVE,' + IntToStr(ord(TDAOCConnection(Sender).LocalPlayer.Realm)) +
    ',' + AItem.SummaryLine);
  frmDebugging.DAOCDelveItem(Sender, AItem);
end;

procedure TfrmMain.CloseRemoteAdmin;
begin
  dmdRemoteAdmin.Enabled := false;
  Log('Remote control telnet server disabled');
end;

procedure TfrmMain.OpenRemoteAdmin;
begin
  dmdRemoteAdmin.Enabled := true;
  Log('Remote control telnet server open on port ' +
    IntToStr(dmdRemoteAdmin.tcpRemoteAdmin.DefaultPort));
end;

procedure TfrmMain.DAOCArriveAtGotoDest(Sender: TObject; ANode: TMapNode);
begin
  frmMacroing.DAOCArriveAtGotoDest(ANode);
end;

procedure TfrmMain.DAOCSelectNPCSuccess(Sender: TObject);
begin
  frmMacroing.DAOCSelectNPCSuccess;
end;

procedure TfrmMain.DAOCSelectNPCFailed(Sender: TObject);
begin
  frmMacroing.DAOCSelectNPCFailed;
end;

procedure TfrmMain.DAOCAttemptNPCRightClickFailed(Sender: TObject);
begin
  frmMacroing.DAOCAttemptNPCRightClickFailed;
end;

procedure TfrmMain.ChatLogXI(const s: string);
begin
  if FChatLogXIEnabled then
    DAOCChatLog(nil, CHAT_XI_PREFIX + s);
end;

procedure TfrmMain.LogLocalPlayerXI;
begin
(***
  with FConnection.LocalPlayer do
    ChatLogXI(Format(
      'Local Player: "%s" Level: %d Health: %d%% Endurance: %d%% Mana: %d%%',
      [Name, Level, HitPoints, EndurancePct, ManaPct]));
***)
end;

procedure TfrmMain.DAOCLocalHealthUpdate(Sender: TObject);
begin
  frmMacroing.DAOCLocalHealthUpdate;
end;

procedure TfrmMain.UpdateQuickLaunchProfileList;
//var
//  I:    integer;
begin

exit;
(***
  cbxAutoLoginProfile.Clear;
  cbxAutoLoginProfile.Items.Add('Normal Profile (none)');
  cbxAutoLoginProfile.ItemIndex := 0;

  for I := 0 to FConnection.QuickLaunchProfiles.Count - 1 do
    cbxAutoLoginProfile.Items.AddObject(
      FConnection.QuickLaunchProfiles[I].ProfileName, FConnection.QuickLaunchProfiles[I]);
***)
end;

procedure TfrmMain.DAOCDoorPositionUpdate(Sender: TObject; ADoor: TDAOCObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCDoorPositionUpdate(Sender, ADoor);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.InjectPacket(ASource: TObject; APacket: TGameNetPacket);
begin
  if FProcessPackets then
    FDControlList.ProcessDAOCPacket(ASource, APacket)
  else
    DAOCPacket(Self, APacket);
end;

procedure TfrmMain.SetRemoteAdminEnabled(const Value: boolean);
begin
  atnRemoteAdminEnable.Checked := Value;
  if atnRemoteAdminEnable.Checked then
    OpenRemoteAdmin
  else 
    CloseRemoteAdmin;
end;

procedure TfrmMain.lblRemoteAdminEnableClick(Sender: TObject);
begin
  RemoteAdminEnabled := not RemoteAdminEnabled;
end;

procedure TfrmMain.DSTREAMDAOCData(Sender: TObject;
  AConnectionID: Cardinal; AIsFromClient, AIsTCP: boolean; AData: Pointer;
  ADataLen: integer);
var
  pPacket:  TGameNetPacket;
begin
  pPacket := TGameNetPacket.Create;
  try
    pPacket.ConnectionID := AConnectionID;
    pPacket.IsFromClient := AIsFromClient;
    if AIsTCP then
      pPacket.IPProtocol := gnppTCP
    else
      pPacket.IPProtocol := gnppUDP;
    pPacket.LinkDataToPacket(AData, ADataLen);

    InjectPacket(Sender, pPacket);
  finally
    pPacket.Free;
  end;
end;

procedure TfrmMain.DSTREAMDAOCConnect(Sender: TObject; AConnectionID,
  AServerIP: Cardinal; AServerPort: WORD; AClientIP: Cardinal;
  AClientPort: WORD);
begin
  FDControlList.NewDAOCConnection(Sender, AConnectionID, AServerIP, AClientIP);
end;

procedure TfrmMain.DSTREAMDAOCDisconnect(Sender: TObject; AConnectionID,
  AServerIP: Cardinal; AServerPort: WORD; AClientIP: Cardinal;
  AClientPort: WORD);
begin
  FDControlList.CloseDAOCConnection(Sender, AConnectionID);
end;

procedure TfrmMain.SetupDStreamObj;
begin
  FDStreamClients := TDStreamClientList.Create;
  FDStreamClients.OnDAOCConnect := DSTREAMDAOCConnect;
  FDStreamClients.OnDAOCData := DSTREAMDAOCData;
  FDStreamClients.OnDAOCDisconnect := DSTREAMDAOCDisconnect;
//  FDStreamClients.OnError := DSTREAMError;
  FDStreamClients.OnStatusChange := DSTREAMStatusChange;

  FDStreamClients.LoadFromINI(GetConfigFileName);

  frmDStrmServerList1.DStrmList := FDStreamClients;
end;

procedure TfrmMain.DSTREAMStatusChange(Sender: TObject);
begin
  frmDStrmServerList1.RefreshStatuses;
end;

procedure TfrmMain.LoadSettingsForConnection(AConn: TDAOCConnection);
begin
  with TINIFile.Create(GetConfigFileName) do begin
    with TDAOCControl(AConn) do begin
      QuickLaunchChars.LoadFromFile(GetConfigFileName);
      QuickLaunchProfiles.LoadFromFile(GetConfigFileName);

      WindowManager.UIStyle := ReadString('Main', 'UIStyle', WindowManager.UIStyle);
      MaxObjectDistance := ReadFloat('Main', 'MaxObjectDistance', MaxObjectDistance);
      MaxObjectStaleTime := ReadInteger('Main', 'MaxObjectStaleTime', 300) * 1000;
      TrackCharacterLogins := ReadBool('Main', 'TrackLogins', true);

      DAOCWindowClass := ReadString('Main', 'DAOCWindowClass', DAOCWindowClass);
      SendKeysSlashDelay := ReadInteger('Main', 'SendKeysSlashDelay', SendKeysSlashDelay);
      TurnUsingFaceLoc := ReadBool('Main', 'TurnUsingFaceLoc', TurnUsingFaceLoc);
      InventoryLookupEnabled := ReadBool('Main', 'InventoryLookupEnabled', InventoryLookupEnabled);
      KeyQuickSell := ReadString('Keys', 'QuickSell', KeyQuickSell);
      KeySelectFriendly := ReadString('Keys', 'SelectFriendly', KeySelectFriendly);
      KeyStrafeLeft := ReadString('Keys', 'StrafeLeft', KeyStrafeLeft);
      KeyStrafeRight := ReadString('Keys', 'StrafeRight', KeyStrafeRight);
    end;  { with connection }

    Free;
  end;  { with INI }
end;

procedure TfrmMain.SaveSettingsForConnection(AConn: TDAOCConnection);
begin
  with TINIFile.Create(GetConfigFileName) do begin
    with TDAOCControl(AConn) do begin
      QuickLaunchChars.SaveToFile(GetConfigFileName);

      WriteString('Main', 'UIStyle', WindowManager.UIStyle);
      WriteInteger('Main', 'SendKeysSlashDelay', SendKeysSlashDelay);
      WriteBool('Main', 'TurnUsingFaceLoc', TurnUsingFaceLoc);
      WriteBool('Main', 'InventoryLookupEnabled', InventoryLookupEnabled);
      WriteString('Keys', 'QuickSell', KeyQuickSell);
      WriteString('Keys', 'SelectFriendly', KeySelectFriendly);
      WriteString('Keys', 'StrafeLeft', KeyStrafeLeft);
      WriteString('Keys', 'StrafeRight', KeyStrafeRight);
    end;  { with connection }

    Free;
  end;  { with INI }
end;

procedure TfrmMain.atnSetDaocPathExecute(Sender: TObject);
var
  s:  string;
begin
  s := FDAOCPath;
  if InputQuery('Set DAoC Path', 'Enter the path to your DAoC installation', s) then
    FDAOCPath := s;
end;

procedure TfrmMain.CONNLISTNewConnection(Sender: TObject; AConn: TDAOCConnection);
begin
  SetupDAOCConnectionObj(AConn);
end;

procedure TfrmMain.CONNLISTDeleteConnection(Sender: TObject; AConn: TDAOCConnection);
begin
  SaveSettingsForConnection(AConn);
end;

procedure TfrmMain.atnRadarExecute(Sender: TObject);
begin
  ShowGLRenderer;
end;

end.
