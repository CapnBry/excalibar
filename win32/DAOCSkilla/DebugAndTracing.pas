unit DebugAndTracing;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PReader2, ExtCtrls, DAOCConnection, DAOCPackets, DAOCObjs;

type
  TfrmDebugging = class(TForm)
    edtPlayback: TEdit;
    btnOpenPlayback: TButton;
    chkPCAPFile: TCheckBox;
    btnPlayTimer: TButton;
    btnPlayPacket: TButton;
    lblFilePos: TLabel;
    chkRecordMobseen: TCheckBox;
    chkDumpPackets: TCheckBox;
    chkCapture: TCheckBox;
    chkProcessPackets: TCheckBox;
    tmrPlayback: TTimer;
    Bevel1: TBevel;
    btnDumpMobs: TButton;
    procedure chkProcessPacketsClick(Sender: TObject);
    procedure chkCaptureClick(Sender: TObject);
    procedure btnOpenPlaybackClick(Sender: TObject);
    procedure btnPlayPacketClick(Sender: TObject);
    procedure tmrPlaybackTimer(Sender: TObject);
    procedure btnPlayTimerClick(Sender: TObject);
    procedure chkRecordMobseenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDumpMobsClick(Sender: TObject);
  private
    FCaptureStream:   TFileStream;
    FMobseenFile:   TFileStream;
    FInPlayback:    boolean;
    FDControl:      TDAOCConnection;
    
    function GetCaptureFile: string;
    procedure SetCaptureFile(const Value: string);
    procedure Log(const s: string);
    procedure CheckWriteMobseen(ADAOCObject: TDAOCObject);
    procedure CheckWriteAllMobseen;
    function GetMobseenFileName : string;
    function GetMyCapFileName : string;
  public
    procedure EthernetSegment(Sender: TObject; ASegment: TEthernetSegment);
    procedure DAOCPacket(Sender: TObject; APacket: TDAOCPacket);
    procedure DAOCZoneChange;
    procedure DAOCNewObject(AObj: TDAOCObject);

    property DAOCControl: TDAOCConnection read FDControl write FDControl;
    property CaptureFile: string read GetCaptureFile write SetCaptureFile;
  end;

var
  frmDebugging: TfrmDebugging;

implementation

uses
  Unit1, DAOCRegion;
  
{$R *.dfm}

procedure TfrmDebugging.chkProcessPacketsClick(Sender: TObject);
begin
  frmMain.ProcessPackets := chkProcessPackets.Checked;
end;

procedure TfrmDebugging.chkCaptureClick(Sender: TObject);
begin
  if chkCapture.Checked then
    FCaptureStream := TFileStream.Create(GetMyCapFileName, fmCreate or fmShareDenyWrite)
  else
    FreeAndNil(FCaptureStream);
end;

procedure TfrmDebugging.btnOpenPlaybackClick(Sender: TObject);
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

procedure TfrmDebugging.EthernetSegment(Sender: TObject; ASegment: TEthernetSegment);
begin
  if not FInPlayback and Assigned(FCaptureStream) then
    ASegment.SaveToStream(FCaptureStream);
end;

procedure TfrmDebugging.btnPlayPacketClick(Sender: TObject);
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
  frmMain.EthernetSegment(nil, tmpSegment);
  tmpSegment.Free;

  lblFilePos.Caption := IntToStr(FCaptureStream.Position);
end;

procedure TfrmDebugging.tmrPlaybackTimer(Sender: TObject);
begin
  tmrPlayback.Enabled := false;
  btnPlayPacketClick(nil);
  tmrPlayback.Enabled := FCaptureStream.Position < FCaptureStream.Size;
end;

procedure TfrmDebugging.btnPlayTimerClick(Sender: TObject);
begin
  tmrPlayback.Enabled := not tmrPlayback.Enabled;
end;

function TfrmDebugging.GetCaptureFile: string;
begin
  Result := edtPlayback.Text;
end;

procedure TfrmDebugging.SetCaptureFile(const Value: string);
begin
  edtPlayback.Text := Value;
end;

procedure TfrmDebugging.DAOCPacket(Sender: TObject; APacket: TDAOCPacket);
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

procedure TfrmDebugging.Log(const s: string);
begin
  frmMain.Log(s);
end;

procedure TfrmDebugging.chkRecordMobseenClick(Sender: TObject);
begin
  if chkRecordMobseen.Checked then begin
      { make sure the file exists and is blank before we start, we do this
        because Delphi doesn't respect the ShareMode on fmCreate }
    FMobseenFile := TFileStream.Create(GetMobseenFileName, fmCreate);
    FMobseenFile.Free;
    FMobseenFile := TFileStream.Create(GetMobseenFileName, fmOpenWrite or fmShareDenyWrite);
    CheckWriteAllMobseen;
  end
  else
    FreeAndNil(FMobseenFile);
end;

procedure TfrmDebugging.CheckWriteAllMobseen;
var
  pObj:   TDAOCObject;
begin
  if Assigned(FDControl) then begin
    pObj := FDControl.DAOCObjects.Head;
    while Assigned(pObj) do begin
      CheckWriteMobseen(pObj);
      pObj := pObj.Next;
    end;
  end;
end;

procedure TfrmDebugging.CheckWriteMobseen(ADAOCObject: TDAOCObject);
var
  s:         string;
  X, Y, Z:   integer;
begin
  if Assigned(FMobseenFile) and (ADAOCObject.ObjectClass in [ocMob, ocVehicle]) and
    Assigned(FDControl.Zone) then begin
    X := FDControl.Zone.WorldToZoneX(ADAOCObject.X);
    Y := FDControl.Zone.WorldToZoneY(ADAOCObject.Y);
    Z := ADAOCObject.Z;
    if (X >= 65535) or (Y > 65535) then
      exit;
    s := Format('MOBseen,%d,%d,%d,%d,%d,%s,%s'#13#10, [
      FDControl.Zone.ZoneNum, X, Y, Z,
      ADAOCObject.Level, ADAOCObject.Name, TDAOCMob(ADAOCObject).TypeTag
      ]);
    FMobseenFile.Write(s[1], Length(s));
  end;
end;

procedure TfrmDebugging.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCaptureStream);
  FreeAndNil(FMobseenFile);
end;

procedure TfrmDebugging.DAOCZoneChange;
begin
  CheckWriteAllMobseen;
end;

procedure TfrmDebugging.DAOCNewObject(AObj: TDAOCObject);
begin
  CheckWriteMobseen(AObj);
end;

function TfrmDebugging.GetMobseenFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'mobseen';
end;

function TfrmDebugging.GetMyCapFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'mycap.cap';
end;

procedure TfrmDebugging.btnDumpMobsClick(Sender: TObject);
var
  pObj:   TDAOCObject;
begin
  pObj := FDControl.DAOCObjects.Head;
  while Assigned(pObj) do begin
    Log(pObj.AsString);
    pObj := pObj.Next;
  end;
end;

end.
