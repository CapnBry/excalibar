unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, PReader2, DAOCConnection, bpf, INIFiles;

type
  TCCRTLogMode = (lmOff, lmFile, lmScreen);

  TfrmMain = class(TForm)
    lstAdapters: TListBox;
    edtFileName: TEdit;
    Label2: TLabel;
    lblAdapterStatus: TLabel;
    lblConnectionStatus: TLabel;
    imgAdapter: TImage;
    Label3: TLabel;
    rbnToFile: TRadioButton;
    rbnToScreen: TRadioButton;
    rbnOff: TRadioButton;
    memLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure lstAdaptersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstAdaptersClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rbnToFileClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FPReader:     TPacketReader2;
    FConnection:  TDAOCConnection;
    FLogMode:     TCCRTLogMode;
    FChatLog:     TFileStream;
    FLogMemoHeight: integer;

    procedure SetupDAOCConnectionObj;
    procedure UpdateAdapterStatus;
    procedure Log(const s: string);
    procedure CreateChatLog;
    procedure CloseChatLog;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure HideScreenLog;
    procedure ShowScreenLog;
  protected
    procedure EthernetSegment(Sender: TObject; ASegment: TEthernetSegment);

    procedure DAOCConnect(Sender: TObject);
    procedure DAOCDisconnect(Sender: TObject);
    procedure DAOCPlayerPosUpdate(Sender: TObject);
    procedure DAOCChatLog(Sender: TObject; const s: string);
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

const
    { ip and net 208.254.16.0/24 and ((tcp and port 10622) or udp) }
  BP_Instns: array[0..16] of Tbpf_insn = (
    (code: BPF_LD + BPF_H + BPF_ABS; jt: 0; jf: 0; k: 12),  // load the ethernet protocol word (offset 12)
    (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 0; jf: 14; k: $0800),  // see if it is IP ($8000)

      { source net 208.254.16.0/24 }
    (code: BPF_LD + BPF_W + BPF_ABS; jt: 0; jf: 0; k: 26),  // load the source DWORD
    (code: BPF_ALU + BPF_AND + BPF_K; jt: 0; jf: 0; k: $ffffff00),  // AND the netmask
    (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 3; jf: 0; k: $D0FE1000),  // is it 208.254.16.0

      { dest net 208.254.16.0/24 }
    (code: BPF_LD + BPF_W + BPF_ABS; jt: 0; jf: 0; k: 30),  // load the dest DWORD
    (code: BPF_ALU + BPF_AND + BPF_K; jt: 0; jf: 0; k: $ffffff00),  // AND the netmask
    (code: BPF_JMP + BPF_JEQ + BPF_K; jt: 0; jf: 8; k: $D0FE1000),  // is it 208.254.16.0?

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

{ TfrmMain }

procedure TfrmMain.UpdateAdapterStatus;
begin
  if FPReader.Active then begin
    lblAdapterStatus.Caption := 'Open';
    lblAdapterStatus.Font.Color := clGreen;
  end
  else begin
    lblAdapterStatus.Caption := 'Closed';
    lblAdapterStatus.Font.Color := clMaroon;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetupDAOCConnectionObj;

  FPReader := TPacketReader2.CreateInst;
  FPReader.OnEthernetSegment := EthernetSegment;

  lstAdapters.Items.Assign(FPReader.AdapterList);
  Log(IntToStr(FPReader.DeviceList.Count) + ' network adapters found');
  Caption := 'Real-time DAoC Chat logger (' + IntToStr(FPReader.DeviceList.Count) +
    ' network adapters)';

  UpdateAdapterStatus;
  DAOCDisconnect(nil);
  LoadSettings;
end;

procedure TfrmMain.SetupDAOCConnectionObj;
begin
  FConnection := TDAOCConnection.Create;
  FConnection.OnConnect := DAOCConnect;
  FConnection.OnDisconnect := DAOCDisconnect;
  FConnection.OnPlayerPosUpdate := DAOCPlayerPosUpdate;
  FConnection.OnChatLog := DAOCChatLog;
end;

procedure TfrmMain.DAOCConnect(Sender: TObject);
begin
  lblConnectionStatus.Caption := 'Connected';
  lblConnectionStatus.Font.Color := clNavy;
end;

procedure TfrmMain.DAOCDisconnect(Sender: TObject);
begin
  lblConnectionStatus.Caption := 'Disconnected';
  lblConnectionStatus.Font.Color := clMaroon;
  
  CloseChatLog;
end;

procedure TfrmMain.DAOCPlayerPosUpdate(Sender: TObject);
begin
  lblConnectionStatus.Caption := 'Active';
  lblConnectionStatus.Font.Color := clGreen;
end;

procedure TfrmMain.EthernetSegment(Sender: TObject;
  ASegment: TEthernetSegment);
begin
  FConnection.ProcessEthernetSegment(ASegment);
end;

procedure TfrmMain.lstAdaptersDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
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

procedure TfrmMain.Log(const s: string);
begin
;
end;

procedure TfrmMain.DAOCChatLog(Sender: TObject; const s: string);
var
  sChatLogLine:   string;
begin
  if FLogMode = lmOff then
    exit;

  sChatLogLine := FormatDateTime('[hh:nn:ss] ', Now) + s;

  if FLogMode = lmFile then begin
    if not Assigned(FChatLog) then
      CreateChatLog;
    if Assigned(FChatLog) then begin
      sChatLogLine := sChatLogLine + #13#10;
      FChatLog.Write(sChatLogLine[1], Length(sChatLogLine))
    end;
  end  // file

  else if FLogMode = lmScreen then begin
    memLog.Lines.Add(sChatLogLine);
  end
end;

procedure TfrmMain.CreateChatLog;
var
  sOpenLine:    string;
begin
  if Assigned(FChatLog) then
    CloseChatLog;

    { make sure we have a file, Delphi 6 will not respect the share mode on an fmCreate }
  if not FileExists(edtFileName.Text) then begin
    FChatLog := TFileStream.Create(edtFileName.Text, fmCreate);
    FreeAndNil(FChatLog);
  end;

  FChatLog := TFileStream.Create(edtFileName.Text, fmOpenWrite or fmShareDenyNone);
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

  UpdateAdapterStatus;
end;

procedure TfrmMain.LoadSettings;
var
  s:   string;
begin
  with TINIFile.Create('CamelotChatRT.ini') do begin
    Left := ReadInteger('Main', 'Left', Left);
    Top := ReadInteger('Main', 'Top', Top);
    Width := ReadInteger('Main', 'Width', Width);
    FLogMemoHeight := ReadInteger('Main', 'LogMemoHeight', 100);

    s := ReadString('Main', 'Adapter', '');
    if (s <> '') and (lstAdapters.Items.IndexOf(s) <> -1) then begin
      lstAdapters.ItemIndex := lstAdapters.Items.IndexOf(s);
      lstAdaptersClick(nil);
    end;

    edtFileName.Text := ReadString('Main', 'FileName', edtFileName.Text);

    FLogMode := TCCRTLogMode(ReadInteger('Main', 'LogMode', ord(lmFile)));
    case FLogMode of
      lmOff:    rbnOff.Checked := true;
      lmFile:   rbnToFile.Checked := true;
      lmScreen: rbnToScreen.Checked := true;
    end;  { case FLogMode }

    Free;
  end;
end;

procedure TfrmMain.SaveSettings;
begin
  with TINIFile.Create('CamelotChatRT.ini') do begin
    WriteInteger('Main', 'Left', Left);
    WriteInteger('Main', 'Top', Top);
    WriteInteger('Main', 'Width', Width);
    WriteInteger('Main', 'LogMemoHeight', FLogMemoHeight);

    if lstAdapters.ItemIndex <> -1 then
      WriteString('Main', 'Adapter', lstAdapters.Items[lstAdapters.ItemIndex]);
    WriteString('Main', 'FileName', edtFileName.Text);
    WriteInteger('Main', 'LogMode', ord(FLogMode));
    Free;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  CloseChatLog;
end;

procedure TfrmMain.rbnToFileClick(Sender: TObject);
begin
  FLogMode := TCCRTLogMode(TRadioButton(Sender).Tag);

  if FLogMode = lmFile then begin
    HideScreenLog;
  end
  else begin
    CloseChatLog;
    ShowScreenLog;
  end;
end;

procedure TfrmMain.HideScreenLog;
begin
  ClientHeight := rbnToScreen.Top + rbnToScreen.Height;
end;

procedure TfrmMain.ShowScreenLog;
begin
  ClientHeight := rbnToScreen.Top + rbnToScreen.Height + FLogMemoHeight + 3;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if memLog.Height > 20 then
    FLogMemoHeight := memLog.Height;
end;

end.
