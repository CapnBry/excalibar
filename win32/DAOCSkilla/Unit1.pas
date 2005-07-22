unit Unit1;

(****************************************************************************
**
** Copyright (C) 2004 Bryan Mayland.  All rights reserved.
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
  DAOCControlList, DStrmServerListFrame, ActnList, Menus, DebugAndLoggingFns;

type
  TfrmMain = class(TForm)
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
    atnSkillaLog: TAction;
    ViewDaocSkillainternallog1: TMenuItem;
    atnForceVersionsUpdate: TAction;
    Forceversioncheck1: TMenuItem;
    atnAppendMobseen: TAction;
    atnAppendDelveseen: TAction;
    Appendmobseenfile1: TMenuItem;
    Appenddelveseenfile1: TMenuItem;
    N1: TMenuItem;
    atnMacroing: TAction;
    Macroing1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnDeleteCharClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure cbxAutoLoginKeyPress(Sender: TObject; var Key: Char);
    procedure tmrUpdateCheckTimer(Sender: TObject);
    procedure lblUpdatesClick(Sender: TObject);
    procedure chkTrackLoginsClick(Sender: TObject);
    procedure atnSetDaocPathExecute(Sender: TObject);
    procedure atnRadarExecute(Sender: TObject);
    procedure atnChatLogExecute(Sender: TObject);
    procedure atnChangeChatLogFileExecute(Sender: TObject);
    procedure atnDumpPacketDataToLogExecute(Sender: TObject);
    procedure atnCaptureMobseenExecute(Sender: TObject);
    procedure atnCaptureDelveseenExecute(Sender: TObject);
    procedure atnAutoLaunchRadarExecute(Sender: TObject);
    procedure atnProcessPacketsExecute(Sender: TObject);
    procedure atnRemoteAdminEnableExecute(Sender: TObject);
    procedure atnSkillaLogExecute(Sender: TObject);
    procedure atnForceVersionsUpdateExecute(Sender: TObject);
    procedure atnAppendDelveseenExecute(Sender: TObject);
    procedure atnAppendMobseenExecute(Sender: TObject);
    procedure atnDumpMobListExecute(Sender: TObject);
    procedure atnMacroingExecute(Sender: TObject);
  private
{$IFDEF DAOC_AUTO_SERVER}
    FIConnection: IDAOCControl;
{$ENDIF DAOC_AUTO_SERVER}
    FClosing:     boolean;
    FCheckForUpdates:   boolean;
    FLastUpdateCheck:   TDateTime;
    FProcessPackets:    boolean;
    FDAOCPath:          string;
    FDStreamClients:    TDStreamClientList;
    FDControlList:      TDAOCControlList;
    FDebugLogState:     TDebugLoggingState;
    FRunCount:          integer;

    procedure LoadSettings;
    procedure LoadSettingsForConnection(AConn: TDAOCConnection);
    procedure SaveSettings;
    procedure SaveSettingsForConnection(AConn: TDAOCConnection);
    procedure SetupDAOCConnectionObj(AConn: TDAOCConnection);
    procedure SetupDStreamObj;
    function GetConfigFileName : string;
    procedure ShowGLRenderer;
    procedure UpdateQuickLaunchList;
    procedure UpdateQuickLaunchProfileList;
    procedure CheckForUpdates(AForce: boolean);
    function GetVersion: string;
  protected
    procedure CONNLISTNewConnection(Sender: TObject; AConn: TDAOCConnection);
    procedure CONNLISTDeleteConnection(Sender: TObject; AConn: TDAOCConnection);
    procedure DAOCRegionChanged(Sender: TObject);
    procedure DAOCPlayerPosUpdate(Sender: TObject);
    procedure DAOCConnect(Sender: TObject);
    procedure DAOCDisconnect(Sender: TObject);
    procedure DAOCLog(Sender: TObject; const s: string);
    procedure DAOCZoneChange(Sender: TObject);
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
    procedure DAOCMobInventoryChanged(Sender: TObject; AObj: TDAOCMovingObject);
    procedure DAOCTradeskillCapped(Sender: TObject);
    procedure DAOCKillTaskChanged(Sender: TObject);

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
    procedure InjectPacket(Source: TObject; APacket: TGameNetPacket);

    property Version: string read GetVersion;
  end;

var
  frmMain: TfrmMain;

procedure CreateOptionalForms;

implementation

uses
  DAOCRegion, PowerSkillSetup, ShowMapNodes, MacroTradeSkill, AFKMessage,
  SpellcraftHelp, Macroing, LowOnStat, VCLMemStrms, RemoteAdmin,
  StringParseHlprs, SkillaLog, DisplayLicense
{$IFDEF OPENGL_RENDERER}
  ,GLRender
{$ENDIF OPENGL_RENDERER}
{$IFDEF DAOC_AUTO_SERVER}
  ,TellMacro
{$ENDIF DAOC_AUTO_SERVER}
  ;

{$R *.dfm}

procedure CreateOptionalForms;
begin
{$IFDEF OPENGL_RENDERER}
  Application.CreateForm(TfrmGLRender, frmGLRender);
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
            // FileInfo.dwFileVersionLS and $FFFF]);
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FDebugLogState := TDebugLoggingState.Create;
  FDebugLogState.LogSink := Log;

  FDControlList := TDAOCControlList.Create;
  FDControlList.OnNewConnection := CONNLISTNewConnection;
  FDControlList.OnDeleteConnection := CONNLISTDeleteConnection;
  SetupDStreamObj;

  FProcessPackets := true;
  atnProcessPackets.Checked := FProcessPackets;

{$IFNDEF OPENGL_RENDERER}
  btnGLRender.Visible := false;
{$ENDIF}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDStreamClients);
  FreeAndNil(FDebugLogState);
end;

procedure TfrmMain.DAOCConnect(Sender: TObject);
var
  pConn:  TDAOCControl;
begin
  atnDumpMobList.Enabled := true;

  pConn := TDAOCControl(Sender);
  Log('New connection: ' + pConn.ClientIP + '->' + pConn.ServerIP);

  frmMacroing.DAOCConnect(Sender);
{$IFDEF OPENGL_RENDERER}
  if atnAutoLaunchRadar.Checked then
    ShowGLRenderer;
  frmGLRender.DAOCConnect(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCDisconnect(Sender: TObject);
begin
  // BRY2: Hmmm need to figure out if we're tracking this connection first. Needs work
  // CloseChatLog;
  atnDumpMobList.Enabled := FDControlList.Count > 0;

  frmMacroing.DAOCDisconnect(Sender);
{$IFDEF OPENGL_RENDERER}
  if atnAutoLaunchRadar.Checked then
    frmGLRender.Close;
  frmGLRender.DAOCDisconnect(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCPlayerPosUpdate(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCPlayerPosUpdate(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.SetupDAOCConnectionObj(AConn: TDAOCConnection);
var
  sAppPath: string;
begin
  sAppPath := ExtractFilePath(ParamStr(0));

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
    OnDoorPositionUpdate := DAOCDoorPositionUpdate;
    OnMobInventoryChanged := DAOCMobInventoryChanged;
    OnTradeSkillCapped := DAOCTradeskillCapped;
    OnKillTaskChanged := DAOCKillTaskChanged;

    DAOCPath := FDAOCPath;
    MainHWND := Handle;
    ZoneList.LoadFromMPKFile(FDAOCPath + 'zones' + PathDelim + 'zones.mpk');
    PacketHandlerDefFile := sAppPath + 'packethandlers.ini';
    LoadRealmRanks(sAppPath + 'realmranks.dat');
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

  UpdateQuickLaunchList;
  UpdateQuickLaunchProfileList;

  FDStreamClients.OpenAll;

  dmdRemoteAdmin.DAOCControlList := FDControlList;
  dmdRemoteAdmin.Enabled := atnRemoteAdminEnable.Checked;
  frmMacroing.DAOCControlList := FDControlList;

{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCConnectionList := FDControlList;
  frmGLRender.PrefsFile := GetConfigFileName;
{$ENDIF OPENGL_RENDERER}

  if not FileExists(FDAOCPath + 'gamedata.mpk') then
    if MessageDlg('Your DAoC path is not set correctly.'#13 +
      'DaocSkilla needs this set in order to function properly.'#13 +
      'Currently set to: ' + FDAOCPath + #13#13 +
      'Would you like to set it now?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      atnSetDaocPath.Execute;
end;

procedure TfrmMain.DAOCLog(Sender: TObject; const s: string);
begin
  Log(s);
end;

procedure TfrmMain.DAOCZoneChange(Sender: TObject);
begin
  FDebugLogState.DAOCZoneChanged(Sender);
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCZoneChanged(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.LoadSettings;
begin
  with TINIFile.Create(GetConfigFileName) do begin
    FRunCount := ReadInteger('Main', 'RunCount', 0);
    if FRunCount = 0 then
      TfrmDisplayLicense.Execute;

    FDAOCPath := ReadString('Main', 'DAOCPath', 'C:\Mythic\Isles\');;
    Left := ReadInteger('Main', 'Left', Left);
    Top := ReadInteger('Main', 'Top', Top);
    atnRemoteAdminEnable.Checked := ReadBool('Main', 'RemoteAdminEnabled', false);
    atnAutoLaunchRadar.Checked := ReadBool('Main', 'AutoLaunchExcal', true);
    // atnMacroing.Visible := ReadBool('Main', 'EnableMacroing', true);
    FCheckForUpdates := ReadBool('Main', 'CheckForUpdates', true);
    FLastUpdateCheck := ReadDateTime('Main', 'LastUpdateCheck', 0);

    FDebugLogState.ChatLogEnabled := ReadBool('Logging', 'RealtimeChatLog', false);
    FDebugLogState.ChatLogXIEnabled := ReadBool('Logging', 'ChatLogXIEnabled', true);
    FDebugLogState.ChatLogFileName := ReadString('Logging', 'ChatLogFile', ExtractFilePath(ParamStr(0)) + 'realchat.log');
      { make sure the Append*s precede the Record*s because the Records open the file }
    FDebugLogState.AppendMobseen := ReadBool('Logging', 'AppendMobseen', true);
    FDebugLogState.AppendDelveseen := ReadBool('Logging', 'AppendDelveseen', true);
    FDebugLogState.RecordMobseen := ReadBool('Logging', 'RecordMobseen', false);
    FDebugLogState.RecordDelveseen := ReadBool('Logging', 'RecordDelveseen', false);
    atnAppendMobseen.Checked := FDebugLogState.AppendMobseen;
    atnAppendDelveseen.Checked := FDebugLogState.AppendDelveseen;
    atnCaptureMobseen.Checked := FDebugLogState.RecordMobseen;
    atnCaptureDelveseen.Checked := FDebugLogState.RecordDelveseen;

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
    frmMacroTradeSkills.QuitOnCapped := ReadBool('MacroTradeSkills', 'QuitOnCapped', false);

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

    frmSkillaLog.Left := ReadInteger('SkillaLog', 'Left', frmSkillaLog.Left);
    frmSkillaLog.Top := ReadInteger('SkillaLog', 'Top', frmSkillaLog.Top);
    frmSkillaLog.Height := ReadInteger('SkillaLog', 'Height', frmSkillaLog.Height);
    frmSkillaLog.Width := ReadInteger('SkillaLog', 'Width', frmSkillaLog.Width);
    Free;
  end;  { with INI }

  Self.Caption := 'DaocSkilla ' + GetVersionString + ' - ' + FDAOCPath;
//  chkTrackLogins.Checked := FConnection.TrackCharacterLogins;
  atnChatLog.Checked := FDebugLogState.ChatLogEnabled;
end;

procedure TfrmMain.SaveSettings;
begin
  with TINIFile.Create(GetConfigFileName) do begin
    WriteInteger('Main', 'RunCount', FRunCount + 1);
    WriteString('Main', 'DAOCPath', FDAOCPath);
    WriteInteger('Main', 'Left', Left);
    WriteInteger('Main', 'Top', Top);
    WriteBool('Main', 'AutoLaunchExcal', atnAutoLaunchRadar.Checked);
    WriteDateTime('Main', 'LastUpdateCheck', FLastUpdateCheck);

    WriteString('Logging', 'ChatLogFile', FDebugLogState.ChatLogFileName);
    WriteBool('Logging', 'RealtimeChatLog', FDebugLogState.ChatLogEnabled);
    WriteBool('Logging', 'ChatLogXIEnabled', FDebugLogState.ChatLogXIEnabled);
    WriteBool('Logging', 'AppendMobseen', FDebugLogState.AppendMobseen);
    WriteBool('Logging', 'AppendDelveseen', FDebugLogState.AppendDelveseen);
    WriteBool('Logging', 'RecordMobseen', FDebugLogState.RecordMobseen);
    WriteBool('Logging', 'RecordDelveseen', FDebugLogState.RecordDelveseen);

    WriteBool('Main', 'RemoteAdminEnabled', atnRemoteAdminEnable.Checked);

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
    WriteBool('MacroTradeSkills', 'QuitOnCapped', frmMacroTradeSkills.QuitOnCapped);

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

    WriteInteger('Macroing', 'Left', frmMacroing.Left);
    WriteInteger('Macroing', 'Top', frmMacroing.Top);
    WriteString('Macroing', 'TrinketList', frmMacroing.TrinketList);

    WriteString('RemoteAdmin', 'PS1', Quote(dmdRemoteAdmin.PS1));

    WriteInteger('SkillaLog', 'Left', frmSkillaLog.Left);
    WriteInteger('SkillaLog', 'Top', frmSkillaLog.Top);
    WriteInteger('SkillaLog', 'Height', frmSkillaLog.Height);
    WriteInteger('SkillaLog', 'Width', frmSkillaLog.Width);

    Free;
  end;  { with INI }
end;

procedure TfrmMain.DAOCAfterPacket(Sender: TObject; APacket: TGameNetPacket);
begin
  FDebugLogState.DAOCAfterPacket(Sender, APacket);
end;

procedure TfrmMain.DAOCInventoryChanged(Sender: TObject);
begin
  frmMacroing.DAOCInventoryChanged(Sender);
end;

procedure TfrmMain.DAOCVendorWindow(Sender: TObject);
begin
  frmMacroing.DAOCVendorWindow(Sender);
end;

procedure TfrmMain.Log(const s: string);
begin
  frmSkillaLog.Log(s);
end;

procedure TfrmMain.DAOCPathChanged(Sender: TObject);
begin
  frmMacroing.DAOCPathChanged(Sender);
end;

procedure TfrmMain.DAOCStopAllActions(Sender: TObject);
begin
  frmMacroing.DAOCStopAllActions(Sender);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
{$IFDEF OPENGL_RENDERER}
  if frmGLRender.Visible then
    frmGLRender.Close;
{$ENDIF OPENGL_RENDERER}

  FDStreamClients.SaveToINI(GetConfigFileName);
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

 FDControlList.Clear;
 FreeAndNil(FDControlList);
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
  FDebugLogState.DAOCNewObject(Sender, ADAOCObject);
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
  frmMacroing.DAOCSkillLevelChanged(Sender, AItem);
end;

function TfrmMain.GetConfigFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TfrmMain.DAOCSelectedObjectChanged(Sender: TObject; ADAOCObject: TDAOCObject);
begin
  FDebugLogState.DAOCSelectedObjectChanged(Sender, ADAOCObject);
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCSelectedObjectChanged(Sender, ADAOCObject);
{$ENDIF OPENGL_RENDERER}
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

procedure TfrmMain.ShowGLRenderer;
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.Show;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCChatLog(Sender: TObject; const s: string);
begin
  FDebugLogState.DAOCChatLog(Sender, s);
  dmdRemoteAdmin.DAOCChatLog(Sender, s);
end;

procedure TfrmMain.DAOCPingReply(Sender: TObject; ATime: integer);
begin
//  lblServerPing.Caption := 'Server ping ' + IntToStr(ATime) + 'ms';
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FClosing := true;
end;

procedure TfrmMain.DAOCCharacterLogin(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCCharacterLogin(Sender);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.UpdateQuickLaunchList;
//var
//  I:    integer;
begin
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
begin
  CheckForUpdates(false);
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
  FDebugLogState.DAOCDelveItem(Sender, AItem);
end;

procedure TfrmMain.DAOCArriveAtGotoDest(Sender: TObject; ANode: TMapNode);
begin
  frmMacroing.DAOCArriveAtGotoDest(Sender, ANode);
end;

procedure TfrmMain.DAOCSelectNPCSuccess(Sender: TObject);
begin
  frmMacroing.DAOCSelectNPCSuccess(Sender);
end;

procedure TfrmMain.DAOCSelectNPCFailed(Sender: TObject);
begin
  frmMacroing.DAOCSelectNPCFailed(Sender);
end;

procedure TfrmMain.DAOCAttemptNPCRightClickFailed(Sender: TObject);
begin
  frmMacroing.DAOCAttemptNPCRightClickFailed(Sender);
end;

procedure TfrmMain.DAOCLocalHealthUpdate(Sender: TObject);
begin
  frmMacroing.DAOCLocalHealthUpdate(Sender);
end;

procedure TfrmMain.UpdateQuickLaunchProfileList;
//var
//  I:    integer;
begin
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

procedure TfrmMain.InjectPacket(Source: TObject; APacket: TGameNetPacket);
begin
  if FProcessPackets then
    try
      FDControlList.ProcessDAOCPacket(Source, APacket);
    except
      on E: Exception do
        Log('PACKET FORMAT CHANGED: ' + E.Message);
    end
  else
    FDebugLogState.DAOCAfterPacket(Self, APacket);
end;

procedure TfrmMain.DSTREAMDAOCData(Sender: TObject;
  AConnectionID: Cardinal; AIsFromClient, AIsTCP: boolean; AData: Pointer;
  ADataLen: integer);
var
  pPacket:  TGameNetPacket;
begin
  frmDStrmServerList1.TickListRefresh;

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
var
  pDefaultClient: TDStreamClient;
begin
  FDStreamClients := TDStreamClientList.Create;
  FDStreamClients.OnDAOCConnect := DSTREAMDAOCConnect;
  FDStreamClients.OnDAOCData := DSTREAMDAOCData;
  FDStreamClients.OnDAOCDisconnect := DSTREAMDAOCDisconnect;
//  FDStreamClients.OnError := DSTREAMError;
  FDStreamClients.OnStatusChange := DSTREAMStatusChange;

  FDStreamClients.LoadFromINI(GetConfigFileName);

  // If there no clients in the list, add a default one to localhost
  if FDStreamClients.Count = 0 then begin
    pDefaultClient := TDStreamClient.Create;
    pDefaultClient.Host := 'localhost';
    FDStreamClients.Add(pDefaultClient);
  end;

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
      DefaultLocalPlayerLevel := ReadInteger('ConnectionState', 'Level', DefaultLocalPlayerLevel);
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
      WriteInteger('ConnectionState', 'Level', DefaultLocalPlayerLevel);
    end;  { with connection }

    Free;
  end;  { with INI }
end;

procedure TfrmMain.atnSetDaocPathExecute(Sender: TObject);
var
  s:  string;
  I:  integer;
begin
  s := FDAOCPath;
  if InputQuery('Set DAoC Path', 'Enter the path to your DAoC installation', s) then begin
    s := IncludeTrailingPathDelimiter(s);
    FDAOCPath := s;
    Self.Caption := 'DaocSkilla ' + GetVersionString + ' - ' + FDAOCPath;
    for I := 0 to FDControlList.Count - 1 do begin
      FDControlList[I].DAOCPath := s;
    end;
  end;
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

procedure TfrmMain.atnChatLogExecute(Sender: TObject);
begin
  FDebugLogState.ChatLogEnabled := atnChatLog.Checked;
end;

procedure TfrmMain.atnChangeChatLogFileExecute(Sender: TObject);
var
  s:  string;
begin
  s := FDebugLogState.ChatLogFileName;
  if InputQuery('Realtime Chat Log', 'File name:', s) then
    FDebugLogState.ChatLogFileName := s;
end;

procedure TfrmMain.atnDumpPacketDataToLogExecute(Sender: TObject);
begin
  FDebugLogState.DumpPacketsToLog := atnDumpPacketDataToLog.Checked;
end;

procedure TfrmMain.atnCaptureMobseenExecute(Sender: TObject);
begin
  FDebugLogState.RecordMobseen := atnCaptureMobseen.Checked;
end;

procedure TfrmMain.atnCaptureDelveseenExecute(Sender: TObject);
begin
  FDebugLogState.RecordDelveseen := atnCaptureDelveseen.Checked;
end;

procedure TfrmMain.atnAutoLaunchRadarExecute(Sender: TObject);
begin
  // Have to have code to enable the action, but using AutoCheck
end;

procedure TfrmMain.atnProcessPacketsExecute(Sender: TObject);
begin
  FProcessPackets := atnProcessPackets.Checked;
end;

procedure TfrmMain.atnRemoteAdminEnableExecute(Sender: TObject);
begin
  dmdRemoteAdmin.Enabled := atnRemoteAdminEnable.Checked;
end;

procedure TfrmMain.atnSkillaLogExecute(Sender: TObject);
begin
  frmSkillaLog.Show;
end;

procedure TfrmMain.CheckForUpdates(AForce: boolean);
var
  sVer:     string;
  FS:       TFileStream;
  sVerFile: string;
begin
  tmrUpdateCheck.Enabled := false;
  if not AForce and (not FCheckForUpdates or ((Now - FLastUpdateCheck) < 7)) then
    exit;

  FLastUpdateCheck := Now;

  lblUpdates.Caption := 'Checking for updates...';
  lblUpdates.Visible := true;
  lblUpdates.Update;

  sVerFile := ExtractFilePath(ParamStr(0)) + 'versions.ini';
  FS := TFileStream.Create(sVerFile, fmCreate);
  try
    httpUpdateChecker.Request.UserAgent := 'Mozilla/3.0 (compatible; DaocSkilla ' +
      GetVersionString + ')';
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

procedure TfrmMain.atnForceVersionsUpdateExecute(Sender: TObject);
begin
  CheckForUpdates(true);
end;

function TfrmMain.GetVersion: string;
begin
  Result := GetVersionString;
end;

procedure TfrmMain.atnAppendDelveseenExecute(Sender: TObject);
begin
  FDebugLogState.AppendDelveseen := atnAppendDelveseen.Checked;
end;

procedure TfrmMain.atnAppendMobseenExecute(Sender: TObject);
begin
  FDebugLogState.AppendMobseen := atnAppendMobseen.Checked;
end;

procedure TfrmMain.atnDumpMobListExecute(Sender: TObject);
begin
  if FDControlList.Count > 0 then
    FDebugLogState.DumpMobsToLog(FDControlList[0]);
end;

procedure TfrmMain.DAOCMobInventoryChanged(Sender: TObject; AObj: TDAOCMovingObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCMobInventoryChanged(Sender, AObj);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.atnMacroingExecute(Sender: TObject);
begin
  if frmMacroing.Visible then
    frmMacroing.Close
  else
    frmMacroing.Show;
end;

procedure TfrmMain.DAOCTradeskillCapped(Sender: TObject);
begin
  frmMacroing.DAOCTradeskillCapped(Sender);
end;

procedure TfrmMain.DAOCKillTaskChanged(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCKillTaskChanged(Sender);
{$ENDIF OPENGL_RENDERER}
end;

end.
