unit ConnectionConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

const
  DEFAULT_COLLECTOR_PORT = 9867;

type
  TServerSubnet = (ssUS, ssEU, ssCustom);
  
  TfrmConnectionConfig = class(TForm)
    lstAdapters: TListBox;
    chkPromiscuous: TCheckBox;
    lblPromisc: TLabel;
    imgAdapter: TImage;
    rbnProcessPackets: TRadioButton;
    rbnProcessRemotely: TRadioButton;
    edtRemoteMachine: TEdit;
    grpSniff: TGroupBox;
    grpRecv: TGroupBox;
    Label2: TLabel;
    edtLocalPort: TEdit;
    chkSniffPackets: TCheckBox;
    btnOK: TBitBtn;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    rbnUSServers: TRadioButton;
    rbnEUServers: TRadioButton;
    rbnCustomServers: TRadioButton;
    edtServerSubnet: TEdit;
    chkRemoteAdmin: TCheckBox;
    procedure lstAdaptersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure edtLocalPortKeyPress(Sender: TObject; var Key: Char);
    procedure chkSniffPacketsClick(Sender: TObject);
    procedure rbnCustomServersClick(Sender: TObject);
    procedure edtServerSubnetKeyPress(Sender: TObject; var Key: Char);
  private
    function GetAdapterName: string;
    function GetLocalCollectorPort: integer;
    function GetPromiscuousCapture: boolean;
    function GetRemoteCollector: string;
    function GetSniffPackets: boolean;
    procedure SetAdapterName(const Value: string);
    procedure SetLocalCollectorPort(const Value: integer);
    procedure SetPromiscuousCapture(const Value: boolean);
    procedure SetRemoteCollector(const Value: string);
    procedure SetSniffPackets(const Value: boolean);
    function GetProcessLocally: boolean;
    procedure SetProcessLocally(const Value: boolean);
    function GetServerSubnet: TServerSubnet;
    procedure SetServerSubnet(const Value: TServerSubnet);
    function GetCustomServerSubnet: string;
    procedure SetCustomServerSubnet(const Value: string);
    function GetRemoteAdminEnabled: boolean;
    procedure SetRemoteAdminEnabled(const Value: boolean);
  public
    procedure AssignAdapterList(ASrc: TStrings);

    property SniffPackets: boolean read GetSniffPackets write SetSniffPackets;
    property AdapterName: string read GetAdapterName write SetAdapterName;
    property PromiscuousCapture: boolean read GetPromiscuousCapture write SetPromiscuousCapture;
    property RemoteCollector: string read GetRemoteCollector write SetRemoteCollector;
    property LocalCollectorPort: integer read GetLocalCollectorPort write SetLocalCollectorPort;
    property ProcessLocally: boolean read GetProcessLocally write SetProcessLocally;
    property ServerSubnet: TServerSubnet read GetServerSubnet write SetServerSubnet;
    property CustomServerSubnet: string read GetCustomServerSubnet write SetCustomServerSubnet;
    property RemoteAdminEnabled: boolean read GetRemoteAdminEnabled write SetRemoteAdminEnabled; 
  end;

var
  frmConnectionConfig: TfrmConnectionConfig;

implementation

{$R *.dfm}

procedure TfrmConnectionConfig.AssignAdapterList(ASrc: TStrings);
begin
  lstAdapters.Items.Assign(ASrc);
end;

function TfrmConnectionConfig.GetAdapterName: string;
begin
  if lstAdapters.ItemIndex = -1 then
    Result := ''
  else
    Result := lstAdapters.Items[lstAdapters.ItemIndex];
end;

function TfrmConnectionConfig.GetLocalCollectorPort: integer;
begin
  Result := StrToInt(edtLocalPort.Text);
end;

function TfrmConnectionConfig.GetPromiscuousCapture: boolean;
begin
  Result := chkPromiscuous.Checked;
end;

function TfrmConnectionConfig.GetRemoteCollector: string;
begin
  Result := edtRemoteMachine.Text;
end;

function TfrmConnectionConfig.GetSniffPackets: boolean;
begin
  Result := chkSniffPackets.Checked;
end;

procedure TfrmConnectionConfig.lstAdaptersDrawItem(Control: TWinControl;
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

procedure TfrmConnectionConfig.SetAdapterName(const Value: string);
var
  I:    integer;
begin
  I := lstAdapters.Items.IndexOf(Value);
  if I <> -1 then begin
    lstAdapters.ItemIndex := I;
    // lstAdaptersClick(nil);
  end;
end;

procedure TfrmConnectionConfig.SetLocalCollectorPort(const Value: integer);
begin
  edtLocalPort.Text := IntToStr(Value);
end;

procedure TfrmConnectionConfig.SetPromiscuousCapture(const Value: boolean);
begin
  chkPromiscuous.Checked := Value;
end;

procedure TfrmConnectionConfig.SetRemoteCollector(const Value: string);
begin
  edtRemoteMachine.Text := Value;
end;

procedure TfrmConnectionConfig.SetSniffPackets(const Value: boolean);
begin
  chkSniffPackets.Checked := Value;
  chkSniffPacketsClick(nil);
end;

procedure TfrmConnectionConfig.edtLocalPortKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
    ModalResult := mrOK;
  if not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

procedure TfrmConnectionConfig.chkSniffPacketsClick(Sender: TObject);
var
  bEnabled:   boolean;
begin
  bEnabled := chkSniffPackets.Checked;
  lstAdapters.Enabled := bEnabled;
  chkPromiscuous.Enabled := bEnabled;
  lblPromisc.Enabled := bEnabled;
  rbnProcessPackets.Enabled := bEnabled;
  rbnProcessRemotely.Enabled := bEnabled;
  edtRemoteMachine.Enabled := bEnabled;
end;

function TfrmConnectionConfig.GetProcessLocally: boolean;
begin
  Result := rbnProcessPackets.Checked;
end;

procedure TfrmConnectionConfig.SetProcessLocally(const Value: boolean);
begin
  if Value then
    rbnProcessPackets.Checked := true
  else
    rbnProcessRemotely.Checked := true;
end;

function TfrmConnectionConfig.GetServerSubnet: TServerSubnet;
begin
  if rbnUSServers.Checked then
    Result := ssUS
  else if rbnEUServers.Checked then
    Result := ssEU
  else
    Result := ssCustom;
end;

procedure TfrmConnectionConfig.SetServerSubnet(const Value: TServerSubnet);
begin
  case Value of
    ssUS:  rbnUSServers.Checked := true;
    ssEU:  rbnEUServers.Checked := true;
    else
      rbnCustomServers.Checked := true;
  end;
  
  rbnCustomServersClick(nil);
end;

procedure TfrmConnectionConfig.rbnCustomServersClick(Sender: TObject);
begin
  edtServerSubnet.Enabled := rbnCustomServers.Checked;
end;

procedure TfrmConnectionConfig.edtServerSubnetKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '.', '0'..'9']) then
    Key := #0;
end;

function TfrmConnectionConfig.GetCustomServerSubnet: string;
begin
  Result := edtServerSubnet.Text;
end;

procedure TfrmConnectionConfig.SetCustomServerSubnet(const Value: string);
begin
  edtServerSubnet.Text := Value;
end;

function TfrmConnectionConfig.GetRemoteAdminEnabled: boolean;
begin
  Result := chkRemoteAdmin.Checked;
end;

procedure TfrmConnectionConfig.SetRemoteAdminEnabled(const Value: boolean);
begin
  chkRemoteAdmin.Checked := Value;
end;

end.
