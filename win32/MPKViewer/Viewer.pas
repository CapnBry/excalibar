unit Viewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmViewer = class(TForm)
    memLines: TMemo;
  private
  public
    procedure DisplayAndFreeStream(AStrm: TStream);
  end;

implementation

{$R *.dfm}

{ TfrmViewer }

procedure TfrmViewer.DisplayAndFreeStream(AStrm: TStream);
begin
  memLines.Lines.LoadFromStream(AStrm);
  AStrm.Free;
end;

end.
