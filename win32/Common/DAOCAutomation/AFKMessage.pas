unit AFKMessage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAOCControl;

type
  TfrmAFK = class(TForm)
    edtAFKMessage: TEdit;
    lblAFKMessage: TLabel;
    procedure edtAFKMessageKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDControl: TDAOCControl;
    function GetAFKMessage: string;
    procedure SetAFKMessage(const Value: string);
    procedure SetDControl(const Value: TDAOCControl);
  public
    property DAOCControl: TDAOCControl read FDControl write SetDControl;
    property AFKMessage: string read GetAFKMessage write SetAFKMessage;
  end;

var
  frmAFK: TfrmAFK;

implementation

{$R *.dfm}

{ TfrmAFK }

function TfrmAFK.GetAFKMessage: string;
begin
  if Assigned(FDControl) then
    Result := FDControl.AFKMessage
  else
    Result := edtAFKMessage.Text;
end;

procedure TfrmAFK.SetAFKMessage(const Value: string);
begin
  edtAFKMessage.Text := Value;
  lblAFKMessage.ParentFont := true;

  if Assigned(FDControl) then
    FDControl.AFKMessage := Value;
end;

procedure TfrmAFK.SetDControl(const Value: TDAOCControl);
begin
  if Assigned(Value) then
    Value.AFKMessage := AFKMessage
  else
    FDControl.AFKMessage := '';
    
  FDControl := Value;
end;

procedure TfrmAFK.edtAFKMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    SetAFKMessage(edtAFKMessage.Text);
    Key := #0;
  end
  else
    lblAFKMessage.Font.Color := clMaroon;
end;

procedure TfrmAFK.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetDControl(nil);
end;

end.
