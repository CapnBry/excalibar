unit MobFilterEntry;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMobFilterEntry = class(TForm)
    Label1: TLabel;
    edtMobname: TEdit;
    Label2: TLabel;
    edtMinLevel: TEdit;
    Label3: TLabel;
    edtMaxLevel: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    Label4: TLabel;
    btnBrowse: TButton;
    OpenDialog1: TOpenDialog;
    edtAlert: TEdit;
    procedure btnOkClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure edtMinLevelKeyPress(Sender: TObject; var Key: Char);
  private
    function GetMaxLevel: integer;
    function GetMinLevel: integer;
    function GetMobName: string;
    function GetOKText: string;
    procedure SetMaxLevel(const Value: integer);
    procedure SetMinLevel(const Value: integer);
    procedure SetMobName(const Value: string);
    procedure SetOKText(const Value: string);
    function GetAlertSound: string;
    procedure SetAlertSound(const Value: string);
  public
    property MobName: string read GetMobName write SetMobName;
    property MinLevel: integer read GetMinLevel write SetMinLevel;
    property MaxLevel: integer read GetMaxLevel write SetMaxLevel;
    property AlertSound: string read GetAlertSound write SetAlertSound;
    property OKText: string read GetOKText write SetOKText;
  end;

var
  frmMobFilterEntry: TfrmMobFilterEntry;

implementation

{$R *.dfm}

procedure TfrmMobFilterEntry.btnOkClick(Sender: TObject);
begin
  if Trim(edtMobname.Text) = '' then
    ModalResult := mrCancel
  else
    ModalResult := mrOk;
end;

procedure TfrmMobFilterEntry.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtAlert.Text := OpenDialog1.FileName;
end;

function TfrmMobFilterEntry.GetMaxLevel: integer;
begin
  Result := StrToInt(edtMaxLevel.Text);
end;

function TfrmMobFilterEntry.GetMinLevel: integer;
begin
  Result := StrToInt(edtMinLevel.Text);
end;

function TfrmMobFilterEntry.GetMobName: string;
begin
  Result := edtMobname.Text;
end;

function TfrmMobFilterEntry.GetOKText: string;
begin
  Result := btnOk.Caption;
end;

procedure TfrmMobFilterEntry.SetAlertSound(const Value: string);
begin
  edtAlert.Text := Value;
end;

procedure TfrmMobFilterEntry.SetMaxLevel(const Value: integer);
begin
  edtMaxLevel.Text := IntToStr(Value);
end;

procedure TfrmMobFilterEntry.SetMinLevel(const Value: integer);
begin
  edtMinLevel.Text := IntToStr(Value);
end;

procedure TfrmMobFilterEntry.SetMobName(const Value: string);
begin
  edtMobname.Text := Value;
end;

procedure TfrmMobFilterEntry.SetOKText(const Value: string);
begin
  btnOk.Caption := Value;
end;

function TfrmMobFilterEntry.GetAlertSound: string;
begin
  Result := edtAlert.Text;
end;

procedure TfrmMobFilterEntry.edtMinLevelKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

end.
