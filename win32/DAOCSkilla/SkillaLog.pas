unit SkillaLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmSkillaLog = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure Clear;
    procedure Log(const s: string);
  end;

var
  frmSkillaLog: TfrmSkillaLog;

implementation

{$R *.dfm}

procedure TfrmSkillaLog.Clear;
begin
  Memo1.Lines.Clear;
end;

procedure TfrmSkillaLog.FormCreate(Sender: TObject);
begin
  Clear;
  Log('--==========================================================--');
  Log('  DaocSkilla is provided at no cost and without warranty');
  Log('    under the General Public License (GPL) Version 2.');
  Log('  See LICENSE.TXT for more information regarding licensing.');
  Log('--==========================================================--');
end;

procedure TfrmSkillaLog.Log(const s: string);
begin
  Memo1.Lines.Add(s);
end;

end.
