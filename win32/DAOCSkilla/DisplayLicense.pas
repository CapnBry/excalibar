unit DisplayLicense;

(****************************************************************************
**
** Copyright (C) 2004 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfrmDisplayLicense = class(TForm)
    Label1: TLabel;
    memLicense: TMemo;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
    class procedure Execute;
  end;

var
  frmDisplayLicense: TfrmDisplayLicense;

implementation

{$R *.dfm}

class procedure TfrmDisplayLicense.Execute;
begin
  with Self.Create(nil) do begin
    ShowModal;
    Free;
  end;
end;

procedure TfrmDisplayLicense.FormCreate(Sender: TObject);
begin
  try
    memLicense.Lines.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'LICENSE.txt');
  except
    memLicense.Clear;
    memLicense.Lines.Add('Missing LICENSE file.');
    memLicense.Lines.Add('');
    memLicense.Lines.Add('Refer to the GNU General Public License (GPL)');
    memLicense.Lines.Add('    http://www.gnu.org/licenses/gpl.txt');
  end;
end;

end.
