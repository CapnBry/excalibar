unit AddPushPin;

interface

uses
{$IFDEF LINUX}
  QForms, QControls,
{$ELSE}
  Windows, Messages, Forms, Buttons, ExtCtrls, 
{$ENDIF !LINUX}
  SysUtils, Classes, Controls, StdCtrls, GLRenderObjects;

type
  TfrmAddPushpin = class(TForm)
    edtLabel: TEdit;
    cbxColor: TColorBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
  public
    class function Execute(X, Y, Z: Cardinal; const ALabel: string) : TMapElementPoint;
  end;

implementation

{$R *.dfm}

{ TfrmAddPushpin }

class function TfrmAddPushpin.Execute(X, Y, Z: Cardinal; const ALabel: string): TMapElementPoint;
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
