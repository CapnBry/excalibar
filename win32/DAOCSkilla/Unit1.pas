unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, WinSock,
  PReader2, DAOCControl, DAOCConnection, ExtCtrls, StdCtrls, bpf, INIFiles,
  DAOCWindows, DAOCInventory, Buttons, PowerSkill, DAOCSkilla_TLB,
  DAOCObjs, DAOCPlayerAttributes, Recipes, Dialogs, DAOCPackets;

type
  TSavedCaptureState = record
    CryptKey:   string;
    ServerIP:   string;
    ClientIP:   string;
    RegionID:   integer;
  end;

  TfrmMain = class(TForm)
    lstAdapters: TListBox;
    Memo1: TMemo;
    lblPlayerPos: TLabel;
    lblPlayerHeadSpeed: TLabel;
    tmrTimeoutDelay: TTimer;
    chkAutosell: TCheckBox;
    btnPowerskillBuy: TSpeedButton;
    imgAdapter: TImage;
    Bevel1: TBevel;    btnMacroTradeskill: TButton;
    btnAFK: TButton;
    btnGLRender: TButton;
    lblZone: TLabel;
    btnTellMacro: TButton;
    btnSpellcraftHlp: TButton;
    btnDebugging: TButton;
    chkAutolaunchExcal: TCheckBox;
    btnShowMapModes: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrTimeoutDelayTimer(Sender: TObject);
    procedure btnPowerskillBuyClick(Sender: TObject);
    procedure btnShowMapModesClick(Sender: TObject);
    procedure chkAutosellClick(Sender: TObject);
    procedure lstAdaptersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnMacroTradeskillClick(Sender: TObject);
    procedure btnAFKClick(Sender: TObject);
    procedure btnGLRenderClick(Sender: TObject);
    procedure lstAdaptersClick(Sender: TObject);
    procedure btnTellMacroClick(Sender: TObject);
    procedure btnSpellcraftHlpClick(Sender: TObject);
    procedure btnDebuggingClick(Sender: TObject);
  private
    FPReader:   TPacketReader2;
    FConnection:  TDAOCControl;
    FIConnection: IDAOCControl;
    FAutoSell:      boolean;
    FInSellOff:     boolean;
    FPSItemList:    TPowerSkillItemList;

    FLastConnection:  TSavedCaptureState;

    FProcessPackets:  boolean;
    procedure LoadSettings;
    procedure SaveSettings;
    function GetConfigFileName : string;
    procedure SetupDAOCConnectionObj;
    procedure UpdatePlayer;
    procedure DoAutoSell;
    procedure DoAutoBuy;
    procedure CheckNeedMorePSMaterials;
    function CheckConflictingWindows(AWindowList: array of const) : boolean;
    procedure SaveConnectionState;
    procedure LoadConnectionState;
    procedure RestoreConnectionState;
    procedure ShowGLRenderer(AConnection: TDAOCConnection);
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
  TellMacro, SpellcraftHelp, FrameFns, DebugAndTracing
{$IFDEF OPENGL_RENDERER}
  ,GLRender
{$ENDIF OPENGL_RENDERER}
{$IFDEF REMOTE_ADMIN}
  ,RemoteAdmin
{$ENDIF REMOTE_ADMIN}
  ;

{$R *.dfm}

const
  TIMEOUT_AUTOSELL = 1;
  TIMEOUT_AUTOBUY = 2;

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
{$IFDEF REMOTE_ADMIN}
  Application.CreateForm(TfrmRemoteAdmin, frmRemoteAdmin);
{$ENDIF REMOTE_ADMIN}
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
  SetupDAOCConnectionObj;

  FPReader := TPacketReader2.CreateInst;
  FPReader.OnEthernetSegment := EthernetSegment;

  FPSItemList := TPowerSkillItemList.Create;

  lstAdapters.Items.Assign(FPReader.AdapterList);
  Memo1.Lines.Clear;
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
  FreeAndNil(FPSItemList);

  FIConnection := nil;
  FConnection := nil; // interface release frees obj
  FPReader.Free;
end;

procedure TfrmMain.EthernetSegment(Sender: TObject; ASegment: TEthernetSegment);
begin
  frmDebugging.EthernetSegment(Sender, ASegment);

  if not FProcessPackets then
    exit;

  FConnection.ProcessEthernetSegment(ASegment);
end;

procedure TfrmMain.UpdatePlayer;
begin
  with FConnection do begin
    lblPlayerPos.Caption := Format('(%d,%d,%d)', [PlayerZoneX, PlayerZoneY, PlayerZoneZ]);
    lblPlayerHeadSpeed.Caption := 'Heading: ' + IntToStr(PlayerZoneHead) +
      ' Speed: ' + IntToStr(LocalPlayer.Speed);
  end;
end;

procedure TfrmMain.DAOCConnect(Sender: TObject);
begin
  Log('New connection: ' + FConnection.ClientIP + '->' +
    FConnection.ServerIP);

{$IFDEF OPENGL_RENDERER}
  if chkAutolaunchExcal.Checked then
    ShowGLRenderer(FConnection);
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCDisconnect(Sender: TObject);
begin
  Log('Connection closed.  Largest packet was: ' + IntToStr(FConnection.LargestDAOCPacketSeen));

{$IFDEF OPENGL_RENDERER}
  if chkAutolaunchExcal.Checked then
    frmGLRender.Hide;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.DAOCPlayerPosUpdate(Sender: TObject);
begin
  UpdatePlayer;
{$IFDEF OPENGL_RENDERER}
  frmGLRender.Dirty;
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

  Log('Zonelist contains ' + IntToStr(FConnection.ZoneList.Count));
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LoadSettings;
{$IFDEF REMOTE_ADMIN}
  dmdRemoteAdmin.DAOCControl := FConnection;
{$ENDIF REMOTE_ADMIN}
  Log('ServerNet set to ' + my_inet_htoa(BP_Instns[4].k));
  RestoreConnectionState;
end;

procedure TfrmMain.DAOCLog(Sender: TObject; const s: string);
begin
  Log(s);
end;

procedure TfrmMain.DAOCZoneChange(Sender: TObject);
begin
  if Assigned(FConnection.Zone) then begin
    lblZone.Caption := FConnection.Zone.Name;
    frmDebugging.DAOCZoneChange;
  end
  else
    lblZone.Caption := 'Region ' + IntToStr(FConnection.RegionID);

{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCZoneChanged;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.LoadSettings;
var
  s:    string;
begin
  with TINIFile.Create(GetConfigFileName) do begin
    Left := ReadInteger('Main', 'Left', Left);
    Top := ReadInteger('Main', 'Top', Top);
    FConnection.DAOCPath := ReadString('Main', 'DAOCPath', 'C:\Mythic\Isles\');
    FConnection.MaxObjectDistance := ReadFloat('Main', 'MaxObjectDistance', 6000);
    Caption := 'DAOCSkilla ' + GetVersionString + ' - ' + FConnection.DAOCPath;
    chkAutolaunchExcal.Checked := ReadBool('Main', 'AutolaunchExcal', true);

    FConnection.DAOCWindowClass := ReadString('Main', 'DAOCWindowClass', FConnection.DAOCWindowClass);

      { The ServerNet is stored in host order like 1.2.3.4 = $01020304,
        also remember it is a NET not an IP so the last number should be 00 }
    BP_Instns[4].k := StrToHNet(ReadString('Main', 'ServerNet', HNetToStr(BP_Instns[4].k)));
    BP_Instns[7].k := BP_Instns[4].k;

    s := ReadString('Main', 'Adapter', '');
    if (s <> '') and (lstAdapters.Items.IndexOf(s) <> -1) then begin
      lstAdapters.ItemIndex := lstAdapters.Items.IndexOf(s);
      lstAdaptersClick(nil);
    end;

    LoadConnectionState;

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
    Free;
  end;  { with INI }
end;

procedure TfrmMain.SaveSettings;
begin
  with TINIFile.Create(GetConfigFileName) do begin
    WriteInteger('Main', 'Left', Left);
    WriteInteger('Main', 'Top', Top);
    WriteBool('Main', 'AutolaunchExcal', chkAutolaunchExcal.Checked);

    WriteString('Main', 'DAOCPath', FConnection.DAOCPath);
    if lstAdapters.ItemIndex <> -1 then
      WriteString('Main', 'Adapter', lstAdapters.Items[lstAdapters.ItemIndex]);

    SaveConnectionState;

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

    Free;
  end;  { with INI }
end;

procedure TfrmMain.DAOCPacket(Sender: TObject; APacket: TDAOCPacket);
begin
  frmDebugging.DAOCPacket(Sender, APacket);
end;

procedure TfrmMain.DAOCInventoryChanged(Sender: TObject);
begin
  if not FAutoSell then
    exit;

  if FConnection.SelectedID = 0 then
    exit;

  tmrTimeoutDelay.Tag := TIMEOUT_AUTOSELL;
  // Log('Setting timer to TIMEOUT_AUTOSELL');
  tmrTimeoutDelay.Enabled := false;
  tmrTimeoutDelay.Enabled := true;
end;

procedure TfrmMain.tmrTimeoutDelayTimer(Sender: TObject);
begin
  tmrTimeoutDelay.Enabled := false;

  case tmrTimeoutDelay.Tag of
    TIMEOUT_AUTOSELL:
      DoAutoSell;
    TIMEOUT_AUTOBUY:
      DoAutoBuy;
  end;
end;

procedure TfrmMain.btnPowerskillBuyClick(Sender: TObject);
begin
  frmPowerSkill.DAOCControl := FConnection;
  frmPowerSkill.PSItemList := FPSItemList;

  if not CheckConflictingWindows([frmSpellcraftHelp]) then
    exit;

  if frmPowerSkill.Visible then
    frmPowerSkill.Close
  else
    frmPowerSkill.Show;
end;

procedure TfrmMain.DAOCVendorWindow(Sender: TObject);
begin
  if frmPowerskill.Visible or frmSpellcraftHelp.Visible then begin
    // Log('Setting timer to TIMEOUT_AUTOBUY');
    tmrTimeoutDelay.Tag := TIMEOUT_AUTOBUY;
    tmrTimeoutDelay.Enabled := false;
    tmrTimeoutDelay.Enabled := true;
  end;
end;

procedure TfrmMain.DoAutoSell;
var
  I:    integer;
  pWnd: TStatsWindow;
  iCnt: integer;
  pFirstItem:   TDAOCInventoryItem;
begin
  if FConnection.SelectedID = 0 then begin
//    Log('InvChg: Timer popped with noone selected');
    exit;
  end;

  iCnt := 0;
  pFirstItem := nil;
  with FConnection.LocalPlayer.Inventory do
    for I := 0 to Count - 1 do
      if Items[I].IsInBag and (Items[I].Quality <> 100) and
        (Assigned(FPSItemList.Find(Items[I].CountlessDescription)) or
        (Pos('cloak', Items[I].Description) > 0) or
        (Pos('hinge', Items[I].Description) > 0)) then begin
        inc(iCnt);
        if not Assigned(pFirstItem) then
          pFirstItem := Items[I];
      end;  { If match }

  if (FInSellOff or (iCnt >= Random(7) + 1)) and Assigned(pFirstItem) then begin
    // emo1.Lines.Add('InvChg: Found item - ' + Items[I].Description);
    pWnd := TStatsWindow.Create(FConnection.WindowManager);
    pWnd.SelectInventoryBag(pFirstItem.BagPage);
    sleep(200);
    pWnd.SelectInventoryItem(pFirstItem.BagItemIndex);
    pWnd.Free;
    sleep(500);
    FConnection.DoSendKeys('[f2]');
    FInSellOff := iCnt > 0;
    exit;
  end;

  FInSellOff := false;
//  Log('InvChg: could not find N items');
  CheckNeedMorePSMaterials;
end;

procedure TfrmMain.DoAutoBuy;
var
  bWasAutoSell:   boolean;
begin
  bWasAutoSell := FAutoSell;
  FAutoSell := false;

  if frmPowerskill.Visible then begin
    frmPowerskill.ExecutePurchases;

    if frmPowerskill.KeepBuying and
      AnsiSameText(FConnection.NodeClosestToPlayerPos.Name, FPSItemList.MerchantNodeName) then
      FConnection.PathToNodeName(FPSItemList.ForgeNodeName);
  end

  else if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.ExecutePurchases;

  FAutoSell := bWasAutoSell;
  Log('Purchase complete');
end;

procedure TfrmMain.btnShowMapModesClick(Sender: TObject);
begin
  if frmShowMapNodes.Visible then
    frmShowMapNodes.Close
  else
    frmShowMapNodes.Show;
  frmShowMapNodes.ShowMapNodes(FConnection.MapNodes);
end;

procedure TfrmMain.Log(const s: string);
begin
  Memo1.Lines.Add(s);
end;

procedure TfrmMain.chkAutosellClick(Sender: TObject);
begin
  FAutoSell := chkAutoSell.Checked;
end;

procedure TfrmMain.CheckNeedMorePSMaterials;
begin
  if frmPowerskill.Visible then begin
    if not AnsiSameText(FConnection.NodeClosestToPlayerPos.Name, FPSItemList.ForgeNodeName) then
      exit;

    if not frmPowerskill.HasMaterialsForItem then
      FConnection.PathToNodeName(FPSItemList.MerchantNodeName)
  end;
end;

procedure TfrmMain.DAOCPathChanged(Sender: TObject);
begin
  if frmShowMapNodes.Visible then
    frmShowMapNodes.PathChanged(FConnection.CurrentPath);
end;

procedure TfrmMain.lstAdaptersDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with lstAdapters.Canvas do begin
    if odSelected in State then begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end
    else begin
      Brush.Color := lstAdapters.Color;
      Font.Color := clGrayText;
    end;

    FillRect(Rect);
    Draw(Rect.Left + 2, Rect.Top + 2, imgAdapter.Picture.Graphic);
    TextOut(Rect.Left + 20, Rect.Top + 1, lstAdapters.Items[Index]);
  end;  { with Canvas }
end;

procedure TfrmMain.DAOCStopAllActions(Sender: TObject);
begin
  if frmPowerskill.Visible then
    frmPowerskill.KeepBuying := false;
  if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.KeepBuying := false;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
{$IFDEF OPENGL_RENDERER}
  if frmGLRender.Visible then
    frmGLRender.Close;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.btnMacroTradeskillClick(Sender: TObject);
begin
  frmMacroTradeSkills.DAOCControl := FConnection;

//  if not CheckConflictingWindows([frmSpellcraftHelp]) then
//    exit;

  if frmMacroTradeSkills.Visible then
    frmMacroTradeSkills.Close
  else
    frmMacroTradeSkills.Show;
end;

procedure TfrmMain.btnAFKClick(Sender: TObject);
begin
  frmAFK.DAOCControl := FConnection;
  if frmAFK.Visible then
    frmAFK.Close
  else
    frmAFK.Show;
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

procedure TfrmMain.lstAdaptersClick(Sender: TObject);
begin
  if FPReader.Active then begin
    FPReader.Close;
    Log('Adapter closed: ' + FPReader.DeviceName);
  end;

  FPReader.DeviceName := FPReader.DeviceList[lstAdapters.ItemIndex];
  FPReader.BPFilter := @BPProgram;
  FPReader.Open;
  Log('Adapter opened: ');
  Log('  ' + lstAdapters.Items[lstAdapters.ItemIndex]);
  Log('  ' + FPReader.DeviceName);
end;

procedure TfrmMain.btnTellMacroClick(Sender: TObject);
begin
{$IFDEF DAOC_AUTO_SERVER}
  frmTellMacro.DAOCControl := FConnection;
  if frmTellMacro.Visible then
    frmTellMacro.Close
  else
    frmTellMacro.Show;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMain.DAOCDeleteObject(ASender: TObject;
  ADAOCObject: TDAOCObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCDeleteObject(ADAOCObject);
{$ENDIF OPENGL_RENDERER}
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
  if frmPowerskill.Visible then
    frmPowerskill.SkillLevelChanged(AItem);
end;

procedure TfrmMain.btnSpellcraftHlpClick(Sender: TObject);
begin
  frmSpellcraftHelp.DAOCControl := FConnection;

  if not CheckConflictingWindows([frmPowerskill]) then
    exit;

  if frmSpellcraftHelp.Visible then
    frmSpellcraftHelp.Close
  else
    frmSpellcraftHelp.Show;
end;

function TfrmMain.CheckConflictingWindows(AWindowList: array of const): boolean;
var
  I:    integer;
  sVisWindows:  string;
begin
  sVisWindows := '';
  for I := low(AWindowList) to high(AWindowList) do
    if TForm(AWindowList[I].VObject).Visible then
      sVisWindows := sVisWindows + #9 + TForm(AWindowList[I].VObject).Caption + #13;

  if sVisWindows = '' then begin
    Result := true;
    exit;
  end;

  Result := MessageDlg('That function conflicts with the operation of the following windows:'#13 +
    sVisWindows +
    'If you continue, those windows will be closed.',
    mtWarning, [mbOk, mbCancel], 0) = mrOK;

  if Result then
    for I := low(AWindowList) to high(AWindowList) do
      if TForm(AWindowList[I].VObject).Visible then
        TForm(AWindowList[I].VObject).Close;
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
end;

procedure TfrmMain.DAOCRegionChanged(Sender: TObject);
begin
  FLastConnection.CryptKey := FConnection.CryptKey;
  FLastConnection.ServerIP := FConnection.ServerIP;
  FLastConnection.ClientIP := FConnection.ClientIP;
  FLastConnection.RegionID := FConnection.RegionID;

  SaveConnectionState;
  
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCRegionChanged;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.SaveConnectionState;
begin
  with TINIFile.Create(GetConfigFileName) do begin
    WriteString('ConnectionState', 'CryptKey', FLastConnection.CryptKey);
    WriteString('ConnectionState', 'ServerIP', FLastConnection.ServerIP);
    WriteString('ConnectionState', 'ClientIP', FLastConnection.ClientIP);
    WriteInteger('ConnectionState', 'RegionID', FLastConnection.RegionID);
    Free;
  end;
end;

procedure TfrmMain.LoadConnectionState;
begin
  with TINIFile.Create(GetConfigFileName) do begin
    FLastConnection.CryptKey := ReadString('ConnectionState', 'CryptKey', '');
    FLastConnection.ServerIP := ReadString('ConnectionState', 'ServerIP', '');
    FLastConnection.ClientIP := ReadString('ConnectionState', 'ClientIP', '');
    FLastConnection.RegionID := ReadInteger('ConnectionState', 'RegionID', 0);
    Free;
  end;
end;

procedure TfrmMain.RestoreConnectionState;
begin
(*** NOT WORKIMG YET
  if FLastConnection.CryptKey <> '' then begin
    FConnection.CryptKey := FLastConnection.CryptKey;
    FConnection.ServerIP := FLastConnection.ServerIP;
    FConnection.ClientIP := FLastConnection.ClientIP;
    FConnection.RegionID := FLastConnection.RegionID;
  end;
***)
end;

procedure TfrmMain.DAOCSetGroundTarget(ASender: TObject);
begin
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCSetGroundTarget;
{$ENDIF OPENGL_RENDERER}
end;

procedure TfrmMain.btnDebuggingClick(Sender: TObject);
begin
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

end.
