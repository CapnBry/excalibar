unit ShowMapNodes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MapNavigator, ExtCtrls;

type
  TfrmShowMapNodes = class(TForm)
    pbxMap: TPaintBox;
    procedure pbxMapPaint(Sender: TObject);
  private
    FMapNodes:    TMapNodeList;
    FCurrentPath: TMapNodeList;
    function NodeIsInPath(ANode: TMapNode) : boolean;
  public
    procedure ShowMapNodes(ANodeList: TMapNodeList);
    procedure PathChanged(APath: TMapNodeList);
  end;

var
  frmShowMapNodes: TfrmShowMapNodes;

implementation

{$R *.dfm}

{ TfrmShowMapNodes }

procedure TfrmShowMapNodes.ShowMapNodes(ANodeList: TMapNodeList);
begin
  FMapNodes := ANodeList;

  Invalidate;
end;

procedure TfrmShowMapNodes.pbxMapPaint(Sender: TObject);
var
  rBounding:  TRect;
  iScaleDownByX: integer;
  iScaleDownByY: integer;
  I:    integer;
  J:    integer;
  X:    integer;
  Y:    integer;
begin
  pbxMap.Canvas.Brush.Color := clNavy;
  pbxMap.Canvas.Pen.Color := clSilver;
  pbxMap.Canvas.Font := Font;
  pbxMap.Canvas.FillRect(Rect(0, 0, pbxMap.Width, pbxMap.Height));

  if Assigned(FMapNodes) and (FMapNodes.Count > 0) then begin
    FMapNodes.FindBoundingRect(rBounding);
(*
    X := (rBounding.Right - rBounding.Left);
    Y := (rBounding.Bottom - rBounding.Top);
    rBounding.Left := rBounding.Left - trunc(X * 0.05);
    rBounding.Right := rBounding.Right + trunc(X * 0.05);
    rBounding.Top := rBounding.Top - trunc(Y * 0.05);
    rBounding.Bottom := rBounding.Bottom + trunc(Y * 0.05);
*)

    iScaleDownByX := ((rBounding.Right - rBounding.Left) div ClientWidth) + 1;
    iScaleDownByY := ((rBounding.Bottom - rBounding.Top) div ClientHeight) + 1;

    for I := 0 to FMapNodes.Count - 1 do
      with pbxMap.Canvas do begin
        if NodeIsInPath(FMapNodes[I]) then begin
          Pen.Color := clRed;
          Pen.Width := 2;
        end
        else begin
          Pen.Color := clSilver;
          Pen.Width := 1;
        end;

        X := (FMapNodes[I].X - rBounding.Left) div iScaleDownByX;
        Y := (FMapNodes[I].Y - rBounding.Top) div iScaleDownByY;
        Rect(X, Y, X + 2, Y + 2);

        for J := 0 to FMapNodes[I].ConnectionCount - 1 do begin
          if NodeIsInPath(FMapNodes[I]) and NodeIsInPath(FMapNodes[I].Connections[J]) then begin
            Pen.Color := clRed;
            Pen.Width := 2;
          end
          else begin
            Pen.Color := clSilver;
            Pen.Width := 1;
          end;
          MoveTo(X, Y);
          LineTo((FMapNodes[I].Connections[J].X - rBounding.Left) div iScaleDownByX,
            (FMapNodes[I].Connections[J].Y - rBounding.Top) div iScaleDownByY);
        end;  { for each connection }

        TextOut(X + 4, Y - 4, FMapNodes[I].Name);
      end;  { for each node }
  end;  { if nodelist.count }
end;

function TfrmShowMapNodes.NodeIsInPath(ANode: TMapNode): boolean;
begin
  Result := Assigned(FCurrentPath) and FCurrentPath.NodeInList(ANode);
end;

procedure TfrmShowMapNodes.PathChanged(APath: TMapNodeList);
begin
  FCurrentPath := APath;
  Invalidate;
end;

end.
