unit AddPushPin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, GLRenderObjects;

type
  TfrmAddPushpin = class(TForm)
    edtLabel: TEdit;
    cbxColor: TColorBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  private
  public
    class function Execute(X, Y, Z: DWORD; ALabel: string) : TMapElementPoint;
  end;

implementation

{$R *.dfm}

{ TfrmAddPushpin }

class function TfrmAddPushpin.Execute(X, Y, Z: DWORD;
  ALabel: string): TMapElementPoint;
begin
  with Self.Create(Application) do
  try
    Result := TMapElementPoint.Create;
    Result.X := X;
    Result.Y := Y;
    Result.Z := Z;

    edtLabel.Text := ALabel;

    if ShowModal = mrOK then begin
      Result.Name := edtLabel.Text;
      Result.Color := cbxColor.Selected;
    end
    else
      FreeAndNil(Result);
  finally
    Free;
  end;
end;

end.
