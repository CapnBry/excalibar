unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, WinSock,
  PReader2, DAOCControl, DAOCConnection, ExtCtrls, StdCtrls, bpf, INIFiles,
  Buttons, DAOCSkilla_TLB, DAOCObjs, Dialogs, DAOCPackets, DAOCPlayerAttributes,
  Recipes, IdTCPServer, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, QuickLaunchChars, IdHTTP, ShellAPI;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    btnDebugging: TButton;
    chkAutolaunchExcal: TCheckBox;
    chkChatLog: TCheckBox;
    edtChatLogFile: TEdit;
    lblServerPing: TLabel;
    btnResume: TButton;
    tcpCollectorClient: TIdTCPClient;
    tcpCollectorServer: TIdTCPServer;
    tmrReconnect: TTimer;
    btnConnectionOpts: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    btnGLRender: TBitBtn;
    btnMacroing: TBitBtn;
    cbxAutoLogin: TComboBox;
    btnLogin: TBitBtn;
    chkTrackLogins: TCheckBox;
    btnDeleteChar: TBitBtn;
    lblUpdates: TLabel;
    tmrUpdateCheck: TTimer;
    httpUpdateChecker: TIdHTTP;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnGLRenderClick(Sender: TObject);
    procedure btnDebuggingClick(Sender: TObject);
    procedure chkChatLogClick(Sender: TObject);
    procedure btnMacroingClick(Sender: TObject);
    procedure btnResumeClick(Sender: TObject);
    procedure tcpCollectorServerExecute(AThread: TIdPeerThread);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tcpCollectorServerStatus(ASender: TObject;
      const AStatus: TIdStatus; const AStatusText: String);
    procedure tcpCollectorClientStatus(ASender: TObject;
      const AStatus: TIdStatus; const AStatusText: String);
    procedure btnConnectionOptsClick(Sender: TObject);
    procedure tmrReconnectTimer(Sender: TObject);
    procedure tcpCollectorClientDisconnected(Sender: TObject);
    procedure btnDeleteCharClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure cbxAutoLoginKeyPress(Sender: TObject; var Key: Char);
    procedure tcpCollectorServerConnect(AThread: TIdPeerThread);
    procedure tcpCollectorServerDisconnect(AThread: TIdPeerThread);
    procedure tmrUpdateCheckTimer(Sender: TObject);
    procedure lblUpdatesClick(Sender: TObject);
    procedure chkTrackLoginsClick(Sender: TObject);
  private
    FPReader:   TPacketReader2;
    FConnection:  TDAOCControl;
    FIConnection: IDAOCControl;
    FChatLog:       TFileStream;
    FClosing:       boolean;
    FCheckForUpdates:   boolean;
    FLastUpdateCheck:   TDateTime;
    FProcessPackets:    boolean;
    FSegmentFromCollector:    TEthernetSegment;

    procedure LoadSettings;
    procedure SaveSettings;
    function GetConfigFileName : string;
    procedure SetupDAOCConnectionObj;
    procedure ShowGLRenderer(AConnection: TDAOCConnection);
    procedure CreateChatLog;
    procedure CloseChatLog;
    procedure SendSegmentToCollector(ASegment: TEthernetSegment);
    procedure OpenAdapter(const AAdapterName: string);
    procedure CloseAdapter;
    procedure CloseCollectionServer;
    procedure OpenCollectionServer;
    procedure OpenCollectionClient;
    procedure CloseCollectionClient;
    procedure NewSegmentFromCollector;
    function UseCollectionClient : boolean;
    function SetServerNet : boolean;
    procedure UpdateQuickLaunchList;
  protected
    procedure DAOCRegionChanged(Sender: TObject);
    procedure DAOCPlayerPosUpdate(Sender: TObject);
    procedure DAOCConnect(Sender: TObject);
    procedure DAOCDisconnect(Sender: TObject);
    procedure DAOCLog(Sender: TObject; const s: string);
    procedure DAOCZoneChange(Sender: TObject);
    procedure DAOCPacket(Sender: TObject; APacket: TDAOCPacket);
    procedure DAOCInventoryChanged(Sender: TObject);
    procedure DAOCVendorWindow(Sender: TObject);
    procedure DAOCPathChanged(Sender: TObject);
    procedure DAOCStopAllActions(Sender: TObject);
    procedure DAOCNewObject(ASender: TObject; ADAOCObject: TDAOCObject);
    procedure DAOCDeleteObject(ASender: TObject; ADAOCObject: TDAOCObject);
    procedure DAOCObjectMoved(ASender: TObject; ADAOCObject: TDAOCObject);
    procedure DAOCSkillLevelChanged(ASender: TObject; AItem: TDAOCNameValuePair);
    procedure DAOCSelectedObjectChanged(ASender: TObject; ADAOCObject: TDAOCObject);
    procedure DAOCSetGroundTarget(ASender: TObject);
    procedure DAOCChatLog(ASender: TObject; const s: string);
    procedure DAOCPingReply(ASender: TObject; ATime: integer);
    procedure DAOCCharacterLogin(ASender: TObject);
  public
    procedure Log(const s: string);
    procedure EthernetSegment(Sender: TObject; ASegment: TEthernetSegment);

    property ProcessPackets: boolean read FProcessPackets write FProcessPackets;
  end;

var
  frmMain: TfrmMain;

procedure CreateOptionalForms;

implementation

uses
  PowerSkillSetup, ShowMapNodes, MacroTradeSkill, AFKMessage,
  TellMacro, SpellcraftHelp, FrameFns, DebugAndTracing, Macroing,
  ConnectionConfig, VCLMemStrms, RemoteAdmin
{$IFDEF OPENGL_RENDERER}
  ,GLRender
{$ENDIF OPENGL_RENDERER}
  ;

{$R *.dfm}

var
    { ip and net 208.254.16.0/24 and ((tcp and port 10622) or udp) }
  BP_Instns: array[0..16] of Tbpf_insn = (
    (code: BPF_LD + BPF_H + BPF_ABS; jt: 0; jf: 0; k: 12),  // load the ethernet protocol word (offset 12)
    (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 0; jf: 14; k: $0800),  // see if it is IP ($8000)

      { source net 208.254.16.0/24 }
    (code: BPF_LD + BPF_W + BPF_ABS; jt: 0; jf: 0; k: 26),  // load the source DWORD
    (code: BPF_ALU + BPF_AND + BPF_K; jt: 0; jf: 0; k: $ffffff00),  // AND the netmask
{4} (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 3; jf: 0; k: $D0FE1000),  // is it 208.254.16.0

      { dest net 208.254.16.0/24 }
    (code: BPF_LD + BPF_W + BPF_ABS; jt: 0; jf: 0; k: 30),  // load the dest DWORD
    (code: BPF_ALU + BPF_AND + BPF_K; jt: 0; jf: 0; k: $ffffff00),  // AND the netmask
{7} (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 0; jf: 8; k: $D0FE1000),  // is it 208.254.16.0?

      { udp }
    (code: BPF_LD + BPF_B + BPF_ABS; jt: 0; jf: 0; k: 23),  // load the IP proto byte (23)
    (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 5; jf: 0; k: $11),  // is it 17?  match!

      { tcp }
    (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 0; jf: 5; k: $6),  // is it 6?

      { src port 10622 }
    (code: BPF_LD + BPF_H + BPF_ABS; jt: 0; jf: 0; k: 34),  // load src port
    (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 2; jf: 0; k: $297E),  // is it 10622?

      { dest port 10622 }
    (code: BPF_LD + BPF_H + BPF_ABS; jt: 0; jf: 0; k: 36),  // load dest port
    (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 0; jf: 1; k: $297E),  // is it 10622?

    (code: BPF_RET + BPF_K; jt: 0; jf: 0; k: $ffffffff),   // return true
    (code: BPF_RET + BPF_K; jt: 0; jf: 0; k: $0)   // return false
    // (code: ; jt: ; jf: ; k: )
  );

  BPProgram: Tbpf_program = (bf_len: 17; bf_insns: @BP_Instns);

procedure CreateOptionalForms;
begin
{$IFDEF OPENGL_RENDERER}
  Application.CreateForm(TfrmGLRender, frmGLRender);
  frmGLRender.DAOCControl := frmMain.FConnection;
{$ENDIF OPENGL_RENDERER}
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

function StrToHNet(const AIP: string) : Cardinal;
begin
  Result := ntohl(inet_addr(PChar(AIP)));
end;

function HNetToStr(AIP: Cardinal) : string;
begin
  Result := string(my_inet_ntoa(ntohl(AIP)));
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  SetupDAOCConnectionObj;

  FPReader := TPacketReader2.CreateInst;
  FPReader.OnEthernetSegment := EthernetSegment;
  Log(IntToStr(FPReader.AdapterList.Count) + ' network adapters found');

  FProcessPackets := true;

{$IFNDEF OPENGL_RENDERER}
  btnGLRender.Visible := false;
{$ENDIF}
{$IFNDEF DAOC_AUTO_SERVER}
  btnTellMacro.Visible := false;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FPReader.Free;
  CloseChatLog;
end;

procedure TfrmMain.EthernetSegment(Sender: TObject; ASegment: TEthernetSegment);
begin
  frmDebugging.EthernetSegment(Sender, ASegment);

  if not FProcessPackets then
    exit;

  if UseCollectionClient then
    SendSegmentToCollector(ASegment)
  else
    FConnection.ProcessEthernetSegment(ASegment);
end;

procedure TfrmMain.DAOCConnect(Sender: TObject);
begin
  lblServerPing.Caption := 'Connected';
  Log('New connection: ' + FConnection.ClientIP + '->' +
    FConnection.ServerIP);

{$IFDEF OPENGL_RENDERER}
  if chkAutolaunchExcal.Checked then
    ShowGLRenderer(FConnection);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCDisconnect(Sender: TObject);
begin
  // Log('Connection closed.  Largest packet was: ' + IntToStr(FConnection.LargestDAOCPacketSeen));
  CloseChatLog;
  lblServerPing.Caption := 'Disconnected';

{$IFDEF OPENGL_RENDERER}
  if chkAutolaunchExcal.Checked then
    frmGLRender.Close;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCPlayerPosUpdate(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCPlayerPosUpdate;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.SetupDAOCConnectionObj;
begin
  FConnection := TDAOCControl.Create;
  FIConnection := FConnection as IDAOCControl;
  FConnection.MainHWND := Handle;

  FConnection.OnPlayerPosUpdate := DAOCPlayerPosUpdate;
  FConnection.OnLog := DAOCLog;
  FConnection.OnConnect := DAOCConnect;
  FConnection.OnDisconnect := DAOCDisconnect;
  FConnection.OnZoneChange := DAOCZoneChange;
  FConnection.OnAfterPacket := DAOCPacket;
  FConnection.OnInventoryChanged := DAOCInventoryChanged;
  FConnection.OnVendorWindow := DAOCVendorWindow;
  FConnection.OnPathChanged := DAOCPathChanged;
  FConnection.OnStopAllActions := DAOCStopAllActions;
  FConnection.OnNewDAOCObject := DAOCNewObject;
  FConnection.OnDeleteDAOCObject := DAOCDeleteObject;
  FConnection.OnDAOCObjectMoved := DAOCObjectMoved;
  FConnection.OnSkillLevelChanged := DAOCSkillLevelChanged;
  FConnection.OnSelectedObjectChange := DAOCSelectedObjectChanged;
  FConnection.OnRegionChanged := DAOCRegionChanged;
  FConnection.OnSetGroundTarget := DAOCSetGroundTarget;
  FConnection.OnChatLog := DAOCChatLog;
  FConnection.OnPingReply := DAOCPingReply;
  FConnection.OnCharacterLogin := DAOCCharacterLogin;

  Log('Zonelist contains ' + IntToStr(FConnection.ZoneList.Count) + ' zones');
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
    { setup the adapterlist first, so the load settings can set the active
      adapter properly }
  frmConnectionConfig.AssignAdapterList(FPReader.AdapterList);
  LoadSettings;
  dmdRemoteAdmin.DAOCControl := FConnection;
  Log('Remote control telnet server open on port ' +
    IntToStr(dmdRemoteAdmin.tcpRemoteAdmin.DefaultPort));

  FPReader.Promiscuous := frmConnectionConfig.PromiscuousCapture;
  if frmConnectionConfig.SniffPackets then
    OpenAdapter(frmConnectionConfig.AdapterName)
  else
    CloseAdapter;
  OpenCollectionServer;
  if not frmConnectionConfig.ProcessLocally then
    OpenCollectionClient;
  UpdateQuickLaunchList;
end;

procedure TfrmMain.DAOCLog(Sender: TObject; const s: string);
begin
  Log(s);
end;

procedure TfrmMain.DAOCZoneChange(Sender: TObject);
begin
  frmDebugging.DAOCZoneChange;
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCZoneChanged;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.LoadSettings;
begin
  FConnection.QuickLaunchChars.LoadFromFile(GetConfigFileName);
  
  with TINIFile.Create(GetConfigFileName) do begin
    Left := ReadInteger('Main', 'Left', Left);
    Top := ReadInteger('Main', 'Top', Top);
    FConnection.DAOCPath := ReadString('Main', 'DAOCPath', 'C:\Mythic\Isles\');
    FConnection.MaxObjectDistance := ReadFloat('Main', 'MaxObjectDistance', 8500);
    FConnection.MaxObjectStaleTime := ReadInteger('Main', 'MaxObjectStaleTime', 300) * 1000;
    Caption := 'DAOCSkilla ' + GetVersionString + ' - ' + FConnection.DAOCPath;
    chkAutolaunchExcal.Checked := ReadBool('Main', 'AutolaunchExcal', true);
    chkChatLog.Checked := ReadBool('Main', 'RealtimeChatLog', false);
    chkChatLogClick(nil);
    edtChatLogFile.Text := ReadString('Main', 'ChatLogFile', FConnection.DAOCPath + 'realchat.log');
    btnMacroing.Visible := ReadBool('Main', 'EnableMacroing', false);
    FConnection.TrackCharacterLogins := ReadBool('Main', 'TrackLogins', true);
    chkTrackLogins.Checked := FConnection.TrackCharacterLogins;
    FCheckForUpdates := ReadBool('Main', 'CheckForUpdates', true);
    FLastUpdateCheck := ReadDateTime('Main', 'LastUpdateCheck', 0);

    FConnection.DAOCWindowClass := ReadString('Main', 'DAOCWindowClass', FConnection.DAOCWindowClass);

    frmConnectionConfig.AdapterName := ReadString('Main', 'Adapter', '');
    frmConnectionConfig.ProcessLocally := ReadBool('Main', 'ProcessLocally', true);
    frmConnectionConfig.SniffPackets := ReadBool('Main', 'SniffPackets', true);
    frmConnectionConfig.PromiscuousCapture := ReadBool('Main', 'PromiscCapture', false);
    frmConnectionConfig.RemoteCollector := ReadString('Main', 'RemoteCollector', 'localhost');
    frmConnectionConfig.LocalCollectorPort := ReadInteger('Main', 'LocalCollectorPort', DEFAULT_COLLECTOR_PORT);
    frmConnectionConfig.ServerSubnet := TServerSubnet(ReadInteger('Main', 'ServerSubnet', 0));
    frmConnectionConfig.CustomServerSubnet := ReadString('Main', 'CustomServerSubnet', '208.254.16.0');
    SetServerNet;

    frmPowerskill.Profile := ReadString('PowerskillBuy', 'Profile', 'spellcrafting');
    frmPowerskill.AutoAdvance := ReadBool('PowerskillBuy', 'AutoAdvance', true);
    frmPowerskill.Left := ReadInteger('PowerskillBuy', 'Left', frmPowerskill.Left);
    frmPowerskill.Top := ReadInteger('PowerskillBuy', 'Top', frmPowerskill.Top);
    frmPowerskill.LastQuickSlot := ReadString('PowerskillBuy', 'LastQuickSlot', frmPowerskill.LastQuickSlot);

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

    frmTellMacro.FileName := ReadString('MacroFile', 'Filename', frmTellMacro.FileName);
    frmTellMacro.Left := ReadInteger('MacroFile', 'Left', frmTellMacro.Left);
    frmTellMacro.Top := ReadInteger('MacroFile', 'Top', frmTellMacro.Top);

    frmSpellcraftHelp.Left := ReadInteger('SpellcraftHelp', 'Left', frmSpellcraftHelp.Left);
    frmSpellcraftHelp.Top := ReadInteger('SpellcraftHelp', 'Top', frmSpellcraftHelp.Top);
    frmSpellcraftHelp.Height := ReadInteger('SpellcraftHelp', 'Height', frmSpellcraftHelp.Height);
    frmSpellcraftHelp.CraftRealm := TCraftRealm(ReadInteger('SpellcraftHelp', 'CraftRealm', ord(frmSpellcraftHelp.CraftRealm)));

    frmDebugging.CaptureFile := ReadString('Debugging', 'CaptureFile', 'c:\savedcap.cap');
    frmDebugging.Left := ReadInteger('Debugging', 'Left', frmDebugging.Left);
    frmDebugging.Top := ReadInteger('Debugging', 'Top', frmDebugging.Top);

    frmMacroing.Left := ReadInteger('Macroing', 'Left', frmMacroing.Left);
    frmMacroing.Top := ReadInteger('Macroing', 'Top', frmMacroing.Top);
    Free;
  end;  { with INI }
end;

procedure TfrmMain.SaveSettings;
begin
  FConnection.QuickLaunchChars.SaveToFile(GetConfigFileName);

  with TINIFile.Create(GetConfigFileName) do begin
    WriteInteger('Main', 'Left', Left);
    WriteInteger('Main', 'Top', Top);
    WriteBool('Main', 'AutolaunchExcal', chkAutolaunchExcal.Checked);
    WriteBool('Main', 'RealtimeChatLog', chkChatLog.Checked);
    WriteString('Main', 'ChatLogFile', edtChatLogFile.Text);
    WriteBool('Main', 'TrackLogins', chkTrackLogins.Checked);
    WriteDateTime('Main', 'LastUpdateCheck', FLastUpdateCheck);

    WriteString('Main', 'DAOCPath', FConnection.DAOCPath);

    WriteString('Main', 'Adapter', frmConnectionConfig.AdapterName);
    WriteBool('Main', 'ProcessLocally', frmConnectionConfig.ProcessLocally);
    WriteBool('Main', 'SniffPackets', frmConnectionConfig.SniffPackets);
    WriteBool('Main', 'PromiscCapture', frmConnectionConfig.PromiscuousCapture);
    WriteString('Main', 'RemoteCollector', frmConnectionConfig.RemoteCollector);
    WriteInteger('Main', 'LocalCollectorPort', frmConnectionConfig.LocalCollectorPort);
    WriteInteger('Main', 'ServerSubnet', Ord(frmConnectionConfig.ServerSubnet));
    WriteString('Main', 'CustomServerSubnet', frmConnectionConfig.CustomServerSubnet);

    WriteString('PowerskillBuy', 'Profile', frmPowerskill.Profile);
    WriteBool('PowerskillBuy', 'AutoAdvance', frmPowerskill.AutoAdvance);
    WriteString('PowerskillBuy', 'LastQuickSlot', frmPowerskill.LastQuickSlot);
    WriteInteger('PowerskillBuy', 'Left', frmPowerskill.Left);
    WriteInteger('PowerskillBuy', 'Top', frmPowerskill.Top);

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

    WriteString('MacroFile', 'Filename', frmTellMacro.FileName);
    WriteInteger('MacroFile', 'Left', frmTellMacro.Left);
    WriteInteger('MacroFile', 'Top', frmTellMacro.Top);

    WriteInteger('SpellcraftHelp', 'Left', frmSpellcraftHelp.Left);
    WriteInteger('SpellcraftHelp', 'Top', frmSpellcraftHelp.Top);
    WriteInteger('SpellcraftHelp', 'Height', frmSpellcraftHelp.Height);
    WriteInteger('SpellcraftHelp', 'CraftRealm', ord(frmSpellcraftHelp.CraftRealm));

    WriteString('Debugging', 'CaptureFile', frmDebugging.CaptureFile);
    WriteInteger('Debugging', 'Left', frmDebugging.Left);
    WriteInteger('Debugging', 'Top', frmDebugging.Top);

    WriteInteger('Macroing', 'Left', frmMacroing.Left);
    WriteInteger('Macroing', 'Top', frmMacroing.Top);
    Free;
  end;  { with INI }
end;

procedure TfrmMain.DAOCPacket(Sender: TObject; APacket: TDAOCPacket);
begin
  frmDebugging.DAOCPacket(Sender, APacket);
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
  CloseAdapter;
  Application.ProcessMessages;  // get any packets pending out of the message q
  
  SaveSettings;
{$IFDEF OPENGL_RENDERER}
  if frmGLRender.Visible then
    frmGLRender.Close;
{$ENDIF OPENGL_RENDERER}

    { we want to free the connection before our destroy because the connection
      might fire callbacks as it closes.  Firing a callback to a sub-form
      which is already destroyed is a bad thing }
  FConnection := nil;
  FIConnection := nil;  // interface release frees obj
end;

procedure TfrmMain.btnGLRenderClick(Sender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  if frmGLRender.Visible then
    frmGLRender.Close
  else
    ShowGLRenderer(FConnection);
{$ENDIF}
end;

procedure TfrmMain.DAOCDeleteObject(ASender: TObject;
  ADAOCObject: TDAOCObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCDeleteObject(ADAOCObject);
{$ENDIF OPENGL_RENDERER}
//  Log('Deleteing: ' + ADaocObject.Name + ' longest update delta ' + IntToStr(ADAOCObject.LongestUpdateTime));
end;

procedure TfrmMain.DAOCNewObject(ASender: TObject;
  ADAOCObject: TDAOCObject);
begin
  frmDebugging.DAOCNewObject(ADAOCObject);
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCAddObject(ADAOCObject);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCObjectMoved(ASender: TObject;
  ADAOCObject: TDAOCObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCUpdateObject(ADAOCObject);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCSkillLevelChanged(ASender: TObject;
  AItem: TDAOCNameValuePair);
begin
  frmMacroing.DAOCSkillLevelChanged(AItem);
end;

function TfrmMain.GetConfigFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TfrmMain.DAOCSelectedObjectChanged(ASender: TObject;
  ADAOCObject: TDAOCObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCSelectedObjectChanged(ADAOCObject);
{$ENDIF OPENGL_RENDERER}
//  if Assigned(ADAOCObject) then
//    Log('Largest update delta: ' + IntToStr(ADAOCObject.LongestUpdateTime));
end;

procedure TfrmMain.DAOCRegionChanged(Sender: TObject);
begin
  FConnection.SaveConnectionState(GetConfigFileName);
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCRegionChanged;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCSetGroundTarget(ASender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCSetGroundTarget;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.btnDebuggingClick(Sender: TObject);
begin
  frmDebugging.DAOCControl := FConnection;
  
  if frmDebugging.Visible then
    frmDebugging.Close
  else
    frmDebugging.Show;
end;

procedure TfrmMain.ShowGLRenderer(AConnection: TDAOCConnection);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCControl := AConnection;
  frmGLRender.PrefsFile := GetConfigFileName;
  frmGLRender.Show;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.chkChatLogClick(Sender: TObject);
begin
  edtChatLogFile.Enabled := not chkChatLog.Checked;

  if not chkChatLog.Checked then
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
  if not FileExists(edtChatLogFile.Text) then begin
    sDirectory := ExtractFilePath(edtChatLogFile.Text);
      { make sure the directory exists }
    if sDirectory <> '' then
      ForceDirectories(sDirectory);
    try
      FChatLog := TFileStream.Create(edtChatLogFile.Text, fmCreate);
    except
      on E: Exception do begin
          { if we get an exception, log it and turn off the chat file }
        chkChatLog.Checked := false;
        chkChatLogClick(nil);
        Log(e.Message);
        exit;
      end;
    end;
    FreeAndNil(FChatLog);
  end;  { if creating a new file }

  FChatLog := TFileStream.Create(edtChatLogFile.Text, fmOpenWrite or fmShareDenyNone);
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

procedure TfrmMain.DAOCChatLog(ASender: TObject; const s: string);
var
  sChatLogLine:   string;
begin
  if chkChatLog.Checked then begin
    if not Assigned(FChatLog) then
      CreateChatLog;
    if Assigned(FChatLog) then begin
      sChatLogLine := FormatDateTime('[hh:nn:ss] ', Now) + s + #13#10;
      FChatLog.Write(sChatLogLine[1], Length(sChatLogLine))
    end;
  end;
end;

procedure TfrmMain.btnMacroingClick(Sender: TObject);
begin
  frmMacroing.DAOCControl := FConnection;
  if frmMacroing.Visible then
    frmMacroing.Close
  else
    frmMacroing.Show;
end;

procedure TfrmMain.DAOCPingReply(ASender: TObject; ATime: integer);
begin
  lblServerPing.Caption := 'Server ping ' + IntToStr(ATime) + 'ms';
end;

procedure TfrmMain.btnResumeClick(Sender: TObject);
begin
  FConnection.ResumeConnection(GetConfigFileName);
end;

procedure TfrmMain.tcpCollectorServerExecute(AThread: TIdPeerThread);
var
  ms:   TMinSizeVCLMemStream;
begin
  ms := TMinSizeVCLMemStream.Create;
  ms.MinCapacity := 2048;  // should be large enough to hold 1500 MTU
  try
    while not FClosing and AThread.Connection.Connected do begin
      AThread.Connection.ReadStream(ms);
      ms.Seek(0, soFromBeginning);

      FSegmentFromCollector := TEthernetSegment.Create;
      FSegmentFromCollector.LoadFromStream(ms, 0);
      AThread.Synchronize(NewSegmentFromCollector);
      FSegmentFromCollector.Free;
      FSegmentFromCollector := nil;

      ms.Size := 0;
    end;
  finally
    ms.Free;
  end;
end;

procedure TfrmMain.SendSegmentToCollector(ASegment: TEthernetSegment);
var
  ms:   TVCLMemoryStream;
begin
  if not tcpCollectorClient.Connected then
    exit;

  ms := TVCLMemoryStream.Create;
  try
    ASegment.SaveToStream(ms);
    ms.Seek(0, soFromBeginning);
    
    tcpCollectorClient.WriteStream(ms, true, true);
  finally
    ms.Free;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FClosing := true;
  tcpCollectorClient.Disconnect;
  tcpCollectorServer.Active := false;
end;

procedure TfrmMain.tcpCollectorServerStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: String);
begin
  Log('Collection Server: ' + AStatusText);
end;

procedure TfrmMain.tcpCollectorClientStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: String);
begin
  Log('Collection client: ' + AStatusText);
end;

procedure TfrmMain.btnConnectionOptsClick(Sender: TObject);
var
  bNeedAdapterRestart:    boolean;
begin
  frmConnectionConfig.ShowModal;

  bNeedAdapterRestart := SetServerNet;
  bNeedAdapterRestart := bNeedAdapterRestart or (
    frmConnectionConfig.SniffPackets <> FPReader.Active);
  bNeedAdapterRestart := bNeedAdapterRestart or (
    FPReader.Promiscuous <> frmConnectionConfig.PromiscuousCapture);
  bNeedAdapterRestart := bNeedAdapterRestart or (
    FPReader.DeviceNameForAdapter(frmConnectionConfig.AdapterName) <> FPReader.DeviceName);

  if bNeedAdapterRestart then begin
    CloseAdapter;
    FPReader.Promiscuous := frmConnectionConfig.PromiscuousCapture;
    if frmConnectionConfig.SniffPackets then
      OpenAdapter(frmConnectionConfig.AdapterName);
  end;

  if frmConnectionConfig.LocalCollectorPort <> tcpCollectorServer.DefaultPort then begin
    CloseCollectionServer;
    OpenCollectionServer;
  end;

    { if sniff and not process locally, start a connect }
  if UseCollectionClient then
    OpenCollectionClient
  else
    CloseCollectionClient;
end;

procedure TfrmMain.OpenAdapter(const AAdapterName: string);
var
  iIdx:   integer;
begin
  CloseAdapter;
  
  if AAdapterName <> '' then begin
    iIdx := FPReader.AdapterList.IndexOf(AAdapterName);
    if iIdx <> -1 then begin
      FPReader.DeviceName := FPReader.DeviceList[iIdx];
      FPReader.BPFilter := @BPProgram;
      FPReader.Open;
      Log('Adapter opened:');
      Log('  ' + AAdapterName);
    end;
  end;  { if adapter name <> '' }
end;

procedure TfrmMain.CloseAdapter;
begin
  if FPReader.Active then begin
    FPReader.Close;
    FPReader.WaitForClose;
    Log('Adapter closed: ' + FPReader.DeviceName);
  end;
end;

procedure TfrmMain.CloseCollectionServer;
begin
  if tcpCollectorServer.Active then begin
    tcpCollectorServer.Active := false;
    Log('Collection server deactivated');
  end;
end;

procedure TfrmMain.OpenCollectionServer;
begin
  CloseCollectionServer;

  tcpCollectorServer.DefaultPort := frmConnectionConfig.LocalCollectorPort;
  tcpCollectorServer.Active := true;
  Log('Collection server activated on port ' + IntToStr(tcpCollectorServer.DefaultPort));
end;

procedure TfrmMain.CloseCollectionClient;
begin
  tcpCollectorClient.Disconnect;
end;

procedure TfrmMain.OpenCollectionClient;
var
  iPos:   integer;
  s:      string;
begin
  try
    CloseCollectionClient;

    s := frmConnectionConfig.RemoteCollector;
    iPos := Pos(':', s);
    if iPos <> 0 then begin
      tcpCollectorClient.Host := copy(s, 1, iPos - 1);
      tcpCollectorClient.Port := StrToIntDef(copy(s, iPos + 1, Length(s)), DEFAULT_COLLECTOR_PORT);
    end
    else begin
      tcpCollectorClient.Host := s;
      tcpCollectorClient.Port := DEFAULT_COLLECTOR_PORT;
    end;

    tcpCollectorClient.Connect;
  except
    on E: Exception do
      Log(E.Message);
  end;

  if not tcpCollectorClient.Connected then
    tmrReconnect.Enabled := true;
end;

procedure TfrmMain.tmrReconnectTimer(Sender: TObject);
begin
  tmrReconnect.Enabled := false;
  OpenCollectionClient;
end;

procedure TfrmMain.NewSegmentFromCollector;
begin
  EthernetSegment(nil, FSegmentFromCollector);
end;

procedure TfrmMain.tcpCollectorClientDisconnected(Sender: TObject);
begin
  if UseCollectionClient then
    OpenCollectionClient;
end;

function TfrmMain.UseCollectionClient: boolean;
begin
  Result := not FClosing and frmConnectionConfig.SniffPackets and
    not frmConnectionConfig.ProcessLocally
end;

function TfrmMain.SetServerNet : boolean;
(*** Returns true if subnet changed ***)
var
  dwNet:  DWORD;
begin
    { The ServerNet is stored in host order like 1.2.3.4 = $01020304,
      also remember it is a NET not an IP so the last number should be 00 }
  case frmConnectionConfig.ServerSubnet of
    ssUS:  dwNet := $D0FE1000;  // 208.254.16.0
    ssEU:  dwNet := $C1FC7B00;  // 193.252.123.0
    else
      dwNet := StrToHNet(frmConnectionConfig.CustomServerSubnet);
  end;

  Result := dwNet <> BP_Instns[4].k;

  BP_Instns[4].k := dwNet;
  BP_Instns[7].k := dwNet;
  Log('ServerNet set to ' + my_inet_htoa(dwNet));
end;

procedure TfrmMain.DAOCCharacterLogin(ASender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCCharacterLogin;
{$ENDIF OPENGL_RENDERER}

  if chkTrackLogins.Checked then 
    UpdateQuickLaunchList;
end;

procedure TfrmMain.UpdateQuickLaunchList;
var
  I:    integer;
begin
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
end;

procedure TfrmMain.btnDeleteCharClick(Sender: TObject);
begin
  if cbxAutoLogin.ItemIndex < FConnection.QuickLaunchChars.Count then begin
    FConnection.QuickLaunchChars.Delete(cbxAutoLogin.ItemIndex);
    UpdateQuickLaunchList;
  end;
end;

procedure TfrmMain.btnLoginClick(Sender: TObject);
begin
  FConnection.LaunchCharacterIdx(cbxAutoLogin.ItemIndex);
end;

procedure TfrmMain.cbxAutoLoginKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    Key := #0;
    btnLoginClick(nil);
  end;
end;

procedure TfrmMain.tcpCollectorServerConnect(AThread: TIdPeerThread);
begin
  Log('Remote sniffer connected');
end;

procedure TfrmMain.tcpCollectorServerDisconnect(AThread: TIdPeerThread);
begin
  Log('Remote sniffer disconnected');
end;

procedure TfrmMain.tmrUpdateCheckTimer(Sender: TObject);
var
  sVer:   string;
begin
  tmrUpdateCheck.Enabled := false;
  if not FCheckForUpdates or ((Now - FLastUpdateCheck) < 7) then  
    exit;

  FLastUpdateCheck := Now;
  
  lblUpdates.Caption := 'Checking for updates...';
  lblUpdates.Visible := true;
  lblUpdates.Update;

  try
    sVer := httpUpdateChecker.Get('http://capnbry.net/daoc/daocskilla.ver');
    if sVer <> GetVersionString then
      lblUpdates.Caption := 'Latest version is ' + sVer
    else
      lblUpdates.Visible := false;
  except
    lblUpdates.Visible := false;
  end;
end;

procedure TfrmMain.lblUpdatesClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://capnbry.net/daoc/daocskilla.php', nil, nil, SW_SHOWNORMAL);
  lblUpdates.Visible := false;
end;

procedure TfrmMain.chkTrackLoginsClick(Sender: TObject);
begin
  FConnection.TrackCharacterLogins := chkTrackLogins.Checked;
end;

end.
