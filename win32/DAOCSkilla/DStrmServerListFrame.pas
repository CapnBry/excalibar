unit DStrmServerListFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, DStreamClient, GlobalTickCounter;

type
  TfrmDStrmServerList = class(TFrame)
    pnlBottom: TPanel;
    lblAdd: TLabel;
    lblEdit: TLabel;
    lblRemove: TLabel;
    lstServers: TListBox;
    Panel1: TPanel;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    procedure lstServersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstServersClick(Sender: TObject);
    procedure lblRemoveClick(Sender: TObject);
    procedure lblAddClick(Sender: TObject);
    procedure lblEditClick(Sender: TObject);
  private
    FDStrmList:   TDStreamClientList;
    FLastRefresh: Cardinal;

    function SelectedConnection : TDStreamClient;
    procedure SetDStrmList(const Value: TDStreamClientList);
    procedure ReloadServerList;
  public
    procedure RefreshStatuses;
    procedure TickListRefresh;
    
    property DStrmList: TDStreamClientList read FDStrmList write SetDStrmList;
  end;

implementation

{$R *.dfm}

const
  COL_HOST = 0;
  COL_STATUS = 1;
  COL_BYTES_RECV = 2;
  COL_BYTES_SEND = 3;

function UIntToSize(ASize: Cardinal) : string;
begin
  case ASize of
    0:
        Result := '-';
    1..999:
        Result := IntToStr(ASize) + 'B';
    1000..1000000:
        Result := FormatFloat('0.0', ASize / 1024) + 'KB';
    else
        Result := FormatFloat('0.00', ASize / (1024 * 1024)) + 'MB';
  end;
end;

{ TfrmDStrmServerList }

procedure TfrmDStrmServerList.SetDStrmList(const Value: TDStreamClientList);
begin
  FDStrmList := Value;
  ReloadServerList;
end;

procedure TfrmDStrmServerList.lstServersDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if not Assigned(FDStrmList) or (Index >= FDStrmList.Count) then
    exit;

  with lstServers.Canvas do begin
    Brush.Color := lstServers.Color;
    FillRect(Rect);

    Font := lstServers.Font;
    TextOut(Rect.Left + 6, Rect.Top + 2, FDStrmList[Index].HostPretty);
    TextOut(Rect.Left + 108, Rect.Top + 2, FDStrmList[Index].Status);
    TextOut(Rect.Left + 240, Rect.Top + 2, UIntToSize(FDStrmList[Index].BytesRecv));
    TextOut(Rect.Left + 290, Rect.Top + 2, UIntToSize(FDStrmList[Index].BytesSend));
  end;  { with Canvas }
end;

procedure TfrmDStrmServerList.RefreshStatuses;
begin
  lstServers.Invalidate;
  FLastRefresh := GlobalTickCount;
end;

procedure TfrmDStrmServerList.lstServersClick(Sender: TObject);
begin
  lblEdit.Enabled := lstServers.ItemIndex <> -1;
  lblRemove.Enabled := lblEdit.Enabled;
end;

procedure TfrmDStrmServerList.lblRemoveClick(Sender: TObject);
var
  pDSClient:  TDStreamClient;
begin
  pDSClient := SelectedConnection;
  if not Assigned(pDSClient) then
    exit;

  if MessageDlg('Are you sure you wish to delete the DStream connection to:'#13#9 +
    pDSClient.HostPretty, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    FDStrmList.Delete(lstServers.ItemIndex);
    ReloadServerList;
  end;
end;

function TfrmDStrmServerList.SelectedConnection: TDStreamClient;
begin
  if Assigned(FDStrmList) and (lstServers.ItemIndex <> -1) and
    (lstServers.ItemIndex < FDStrmList.Count) then
    Result := FDStrmList[lstServers.ItemIndex]
  else
    Result := nil;
end;

procedure TfrmDStrmServerList.ReloadServerList;
begin
  lstServers.Count := FDStrmList.Count;
  lstServers.Height := lstServers.ItemHeight * lstServers.Count + 2;
  lstServersClick(nil);
end;

procedure TfrmDStrmServerList.TickListRefresh;
begin
  if GlobalTickCount - FLastRefresh > 500 then
    RefreshStatuses;
end;

procedure TfrmDStrmServerList.lblAddClick(Sender: TObject);
var
  s:    string;
  pDS:  TDStreamClient;
begin
  s := 'localhost';
  if InputQuery('Add DStream connection', 'Hostname or IP:', s) then begin
    pDS := TDStreamClient.Create;
    pDS.SetConnectString(s);
    FDStrmList.Add(pDS);
    ReloadServerList;
    pDS.Open;
  end;
end;

procedure TfrmDStrmServerList.lblEditClick(Sender: TObject);
var
  s:    string;
  pDS:  TDStreamClient;
begin
  pDS := SelectedConnection;
  if not Assigned(pDS) then
    exit;

  s := pDS.HostPretty;
  if InputQuery('Add DStream connection', 'Hostname or IP:', s) then begin
    pDS.Close;
    pDS.SetConnectString(s);
    ReloadServerList;
    pDS.Open;
  end;
end;

end.
