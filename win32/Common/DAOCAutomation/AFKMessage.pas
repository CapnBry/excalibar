unit AFKMessage;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

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
    procedure FormShow(Sender: TObject);
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
  if Assigned(FDControl) then
    FDControl.AFKMessage := '';

  FDControl := Value;

  if Visible and Assigned(FDControl) then
    FDControl.AFKMessage := AFKMessage;
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
  if Assigned(FDControl) then
    FDControl.AFKMessage := '';
end;

procedure TfrmAFK.FormShow(Sender: TObject);
begin
  SetDControl(FDControl);
end;

end.
