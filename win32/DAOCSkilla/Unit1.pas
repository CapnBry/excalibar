unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  PReader2, DAOCControl, DAOCConnection, ExtCtrls, StdCtrls, bpf, INIFiles,
  DAOCWindows, DAOCInventory, Buttons, PowerSkill, DAOCSkilla_TLB,
  DAOCObjs, DAOCPlayerAttributes, Recipes, Dialogs, DAOCPackets;

type
  TSavedCaptureState = record
    CryptKey:   string;
    ServerIP:   string;
    ClientIP:   string;
  end;

  TfrmMain = class(TForm)
    lstAdapters: TListBox;
    Memo1: TMemo;
    lblPlayerPos: TLabel;
    lblPlayerHead: TLabel;
    lblPlayerSpeed: TLabel;
    chkCapture: TCheckBox;
    btnOpenPlayback: TButton;
    edtPlayback: TEdit;
    btnPlayPacket: TButton;
    tmrPlayback: TTimer;
    btnPlayTimer: TButton;
    lblFilePos: TLabel;
    chkDumpPackets: TCheckBox;
    chkProcessPackets: TCheckBox;
    tmrTimeoutDelay: TTimer;
    chkAutosell: TCheckBox;
    btnPowerskillBuy: TSpeedButton;
    Button3: TButton;
    chkPCAPFile: TCheckBox;
    imgAdapter: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;    btnMacroTradeskill: TButton;
    btnAFK: TButton;
    btnGLRender: TButton;
    lblZone: TLabel;
    btnTellMacro: TButton;
    btnSpellcraftHlp: TButton;
    Button1: TButton;
    chkRecordMobseen: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkCaptureClick(Sender: TObject);
    procedure btnOpenPlaybackClick(Sender: TObject);
    procedure btnPlayPacketClick(Sender: TObject);
    procedure tmrPlaybackTimer(Sender: TObject);
    procedure btnPlayTimerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrTimeoutDelayTimer(Sender: TObject);
    procedure btnPowerskillBuyClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
    procedure Button1Click(Sender: TObject);
    procedure chkRecordMobseenClick(Sender: TObject);
  private
    FPReader:   TPacketReader2;
    FConnection:  TDAOCControl;
    FIConnection: IDAOCControl;
    FCaptureStream: TFileStream;
    FMobseenFile:   TFileStream;
    FInPlayback:    boolean;
    FAutoSell:      boolean;
    FInSellOff:     boolean;
    FPSItemList:    TPowerSkillItemList;

    FLastConnection:  TSavedCaptureState;

    procedure LoadSettings;
    procedure SaveSettings;
    function GetConfigFileName : string;
    procedure SetupDAOCConnectionObj;
    procedure UpdatePlayer;
    procedure EthernetSegment(Sender: TObject; ASegment: TEthernetSegment);
    procedure DoAutoSell;
    procedure DoAutoBuy;
    procedure CheckNeedMorePSMaterials;
    function CheckConflictingWindows(AWindowList: array of const) : boolean;
    procedure CheckWriteMobseen(ADAOCObject: TDAOCObject);
    procedure CheckWriteAllMobseen;
  protected
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
  public
    procedure UpdateGLRender;
    procedure Log(const s: string);
  end;

var
  frmMain: TfrmMain;

procedure CreateOptionalForms;

implementation

uses
  PowerSkillSetup, ShowMapNodes, MacroTradeSkill, AFKMessage,
  TellMacro, SpellcraftHelp, FrameFns, RemoteAdmin
{$IFDEF OPENGL_RENDERER}
  ,GLRender
{$ENDIF OPENGL_RENDERER}
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
end;

function IPAddrToString(dwIP: DWORD) : string;
begin

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetupDAOCConnectionObj;

  FPReader := TPacketReader2.CreateInst;
  FPReader.OnEthernetSegment := EthernetSegment;

  FPSItemList := TPowerSkillItemList.Create;

  lstAdapters.Items.Assign(FPReader.AdapterList);
  Log(IntToStr(FPReader.AdapterList.Count) + ' network adapters found');

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

  if Assigned(FCaptureStream) then
    FreeAndNil(FCaptureStream);
end;

procedure TfrmMain.EthernetSegment(Sender: TObject;
  ASegment: TEthernetSegment);
begin
  if not FInPlayback and Assigned(FCaptureStream) then
    ASegment.SaveToStream(FCaptureStream);

  if not chkProcessPackets.Checked then
    exit;
    
  FConnection.ProcessEthernetSegment(ASegment);
  // Log(ASegment.AsString);
end;

procedure TfrmMain.UpdatePlayer;
begin
  with FConnection do begin
    lblPlayerPos.Caption := Format('(%d,%d,%d)', [PlayerZoneX, PlayerZoneY, PlayerZoneZ]);
    lblPlayerHead.Caption := 'Heading: ' + IntToStr(PlayerZoneHead);
    lblPlayerSpeed.Caption := 'Speed: ' + IntToStr(LocalPlayer.Speed);
  end;
end;

procedure TfrmMain.chkCaptureClick(Sender: TObject);
begin
  if chkCapture.Checked then
    FCaptureStream := TFileStream.Create('c:\mycap.cap', fmCreate or fmShareDenyWrite)
  else
    FreeAndNil(FCaptureStream);
end;

procedure TfrmMain.btnOpenPlaybackClick(Sender: TObject);
begin
  if chkCapture.Checked then begin
    chkCapture.Checked := false;
    FCaptureStream.Free;
  end;

  FCaptureStream := TFileStream.Create(edtPlayback.Text, fmOpenRead or fmShareDenyNone);
  FInPlayback := true;

    { 24 bytes of header before packets start }
  if chkPCAPFile.Checked then
    FCaptureStream.seek(24, soFromCurrent);

  lblFilePos.Caption := IntToStr(FCaptureStream.Position);
end;

procedure TfrmMain.btnPlayPacketClick(Sender: TObject);
var
  tmpSegment:   TEthernetSegment;
  iCapLen:      integer;
begin
  if not Assigned(FCaptureStream) then
    btnOpenPlaybackClick(Self);

  if not Assigned(FCaptureStream) then
    exit;

  if FCaptureStream.Position >= FCaptureStream.Size then
    exit;

    { 8 header + 4 frame size gets us to caplen }
  if chkPCAPFile.Checked then begin
    FCaptureStream.seek(8, soFromCurrent);
    FCaptureStream.Read(iCapLen, sizeof(iCapLen));
  end
  else
    iCapLen := 0;

  tmpSegment := TEthernetSegment.Create;
  tmpSegment.LoadFromStream(FCaptureStream, iCapLen);
  EthernetSegment(nil, tmpSegment);
  tmpSegment.Free;

  lblFilePos.Caption := IntToStr(FCaptureStream.Position);
end;

procedure TfrmMain.tmrPlaybackTimer(Sender: TObject);
begin
  tmrPlayback.Enabled := false;
  btnPlayPacketClick(nil);
  tmrPlayback.Enabled := FCaptureStream.Position < FCaptureStream.Size;
end;

procedure TfrmMain.btnPlayTimerClick(Sender: TObject);
begin
  tmrPlayback.Enabled := not tmrPlayback.Enabled;
end;

procedure TfrmMain.DAOCConnect(Sender: TObject);
begin
  Log('New connection: ' + FConnection.ClientIP + '->' +
    FConnection.ServerIP);
end;

procedure TfrmMain.DAOCDisconnect(Sender: TObject);
begin
  Log('Connection closed.  Largest packet was: ' + IntToStr(FConnection.LargestDAOCPacketSeen));
end;

procedure TfrmMain.DAOCPlayerPosUpdate(Sender: TObject);
begin
  UpdatePlayer;
  UpdateGLRender;
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

  Log('Zonelist contains ' + IntToStr(FConnection.ZoneList.Count));
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LoadSettings;
  dmdRemoteAdmin.DAOCControl := FConnection;
  Log('ServerNet set to ' + my_inet_htoa(BP_Instns[4].k));
end;

procedure TfrmMain.DAOCLog(Sender: TObject; const s: string);
begin
  Log(s);
end;

procedure TfrmMain.DAOCZoneChange(Sender: TObject);
begin
  if Assigned(FConnection.Zone) then begin
    lblZone.Caption := FConnection.Zone.Name;
    CheckWriteAllMobseen;
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
    FConnection.DAOCPath := ReadString('Main', 'DAOCPath', '');
    FConnection.MaxObjectDistance := ReadFloat('Main', 'MaxObjectDistance', 7500);
    Caption := 'DAOC Skilla - ' + FConnection.DAOCPath;

    FConnection.DAOCWindowClass := ReadString('Main', 'DAOCWindowClass', FConnection.DAOCWindowClass);
    edtPlayback.Text := ReadString('Main', 'CaptureFile', 'c:\savedcap.cap');

      { The ServerNet is stored in host order like 1.2.3.4 = $01020304,
        also remember it is a NET not an IP so the last number should be 00 }
    BP_Instns[4].k := ReadInteger('Main', 'ServerNet', BP_Instns[4].k);
    BP_Instns[7].k := BP_Instns[4].k;

    s := ReadString('Main', 'Adapter', '');
    if (s <> '') and (lstAdapters.Items.IndexOf(s) <> -1) then begin
      lstAdapters.ItemIndex := lstAdapters.Items.IndexOf(s);
      lstAdaptersClick(nil);
    end;

    FLastConnection.CryptKey := ReadString('ConnectionState', 'CrytKey', '');
    FLastConnection.ServerIP := ReadString('ConnectionState', 'ServerIP', '');
    FLastConnection.ClientIP := ReadString('ConnectionState', 'ClientIP', '');

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
    Free;
  end;  { with INI }
end;

procedure TfrmMain.SaveSettings;
begin
  with TINIFile.Create('.\DaocSkilla.ini') do begin
    WriteInteger('Main', 'Left', Left);
    WriteInteger('Main', 'Top', Top);

    WriteString('Main', 'DAOCPath', FConnection.DAOCPath);
    if lstAdapters.ItemIndex <> -1 then
      WriteString('Main', 'Adapter', lstAdapters.Items[lstAdapters.ItemIndex]);
    WriteString('Main', 'CaptureFile', edtPlayback.Text);

    WriteString('ConnectionState', 'CrytKey', FLastConnection.CryptKey);
    WriteString('ConnectionState', 'ServerIP', FLastConnection.ServerIP);
    WriteString('ConnectionState', 'ClientIP', FLastConnection.ClientIP);

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
    Free;
  end;  { with INI }
end;

procedure TfrmMain.DAOCPacket(Sender: TObject; APacket: TDAOCPacket);
var
  sProto:   string;
  sDirec:   string;
//  kk:       TDAOCCryptKey;
begin
  if chkDumpPackets.Checked then begin
    if APacket.IPProtocol = daocpTCP then
      sProto := 'TCP'
    else
      sProto := 'UDP';

    if APacket.IsFromServer then
      sDirec := 'FROM'
    else
      sDirec := ' TO ';

    Log('---- ' + sProto + ' packet ' + sDirec + ' server ---- ' + APacket.HandlerName);
    Log(APacket.AsString);

//    if FConnection.CryptKey <> '000000000000000000000000' then begin
//      Log('++++ ' + sProto + ' packet ' + sDirec + ' server ++++ ' + APacket.HandlerName);
//      StringToDAOCCryptKey(FConnection.CryptKey, kk);
//      APacket.Decrypt(kk);
//      Log(APacket.AsString);
//    end;
  end;
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

procedure TfrmMain.Button3Click(Sender: TObject);
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
  FreeAndNil(FMobseenFile);
  SaveSettings;
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
  frmGLRender.DAOCControl := FConnection;
  frmGLRender.PrefsFile := GetConfigFileName;
  if frmGLRender.Visible then
    frmGLRender.Close
  else
    frmGLRender.Show;
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
{$IFNDEF DAOC_AUTO_SERVER}
  frmTellMacro.DAOCControl := FConnection;
  if frmTellMacro.Visible then
    frmTellMacro.Close
  else
    frmTellMacro.Show;
{$ENDIF DAOC_AUTO_SERVER}
end;

procedure TfrmMain.UpdateGLRender;
begin
{$IFDEF OPENGL_RENDERER}
  if frmGLRender.Visible then
    frmGLRender.Dirty;
{$ENDIF OPENGL_RENDERER}
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
{$IFDEF OPENGL_RENDERER}
  frmGLRender.DAOCAddObject(ADAOCObject);
{$ENDIF OPENGL_RENDERER}
  CheckWriteMobseen(ADAOCObject);
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

procedure TfrmMain.Button1Click(Sender: TObject);
var
  I:    integer;
begin
  for I := 0 to FConnection.DAOCObjects.Count - 1 do
    Log(FConnection.DAOCObjects[I].AsString);
end;

procedure TfrmMain.CheckWriteMobseen(ADAOCObject: TDAOCObject);
var
  s:         string;
  X, Y, Z:   integer;
begin
  if (ADAOCObject.ObjectClass = ocMob) and Assigned(FMobseenFile) and
    Assigned(FConnection.Zone) then begin
    X := FConnection.Zone.ZoneConvertX(ADAOCObject.X);
    Y := FConnection.Zone.ZoneConvertY(ADAOCObject.Y);
    Z := FConnection.Zone.ZoneConvertZ(ADAOCObject.Z);
    if (X >= 65535) or (Y > 65535) then
      exit;
    s := Format('MOBseen,%d,%d,%d,%d,%d,%s,%s'#13#10, [
      FConnection.Zone.ZoneNum, X, Y, Z,
      ADAOCObject.Level, ADAOCObject.Name, TDAOCMob(ADAOCObject).TypeTag
      ]);
    FMobseenFile.Write(s[1], Length(s));
  end;
end;

procedure TfrmMain.chkRecordMobseenClick(Sender: TObject);
begin
  if chkRecordMobseen.Checked then begin
    FMobseenFile := TFileStream.Create('mobseen', fmCreate or fmShareDenyWrite);
    CheckWriteAllMobseen;
  end
  else
    FreeAndNil(FMobseenFile);
end;

procedure TfrmMain.CheckWriteAllMobseen;
var
  I:  integer;
begin
  for I := 0 to FConnection.DAOCObjects.Count - 1 do
    CheckWriteMobseen(FConnection.DAOCObjects[I]);
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

end.
