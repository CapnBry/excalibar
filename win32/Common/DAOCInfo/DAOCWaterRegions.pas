unit DAOCWaterRegions;

interface

uses
  Types, Graphics, SysUtils, Contnrs, CSVLineParser, StreamINI;

type
  TWaterRegion = class(TObject)
  private
    FZ:       integer;
    FBounds:  TRect;
    FColor:   TColor;
  public
    property Bounds: TRect read FBounds;
    property Z: integer read FZ;
    property Color: TColor read FColor;
  end;

  TWaterRegionList = class(TObjectList)
  private
    function GetItems(Index: integer): TWaterRegion;
  public
    procedure LoadFromINI(ASectorDatINI: TStreamINIFile);

    property Items[Index: integer]: TWaterRegion read GetItems; default;
  end;

implementation

{ TWaterRegionList }

function TWaterRegionList.GetItems(Index: integer): TWaterRegion;
begin
  Result := TWaterRegion(inherited Items[Index]);
end;

procedure TWaterRegionList.LoadFromINI(ASectorDatINI: TStreamINIFile);
var
  iRegions:   integer;
  sSection: string;
  I:      integer;
  B:      integer;
  rectBounds:   TRect;
  tmpWReg:  TWaterRegion;
  CSV:      TCSVLineParser;
begin
  Clear;

  CSV := TCSVLineParser.Create;
  with ASectorDatINI do begin
    iRegions := ReadInteger('waterdefs', 'num', 0);

    for I := 0 to iRegions - 1 do begin
      sSection := Format('river%2.2d', [I]);
        { find the bounding box incompassing the bankpoints }
      rectBounds := Rect(0, 0, 0, 0);
      for B := 0 to ReadInteger(sSection, 'bankpoints', 0) - 1 do begin
        CSV.DataString := ReadString(sSection, Format('left%2.2d', [B]), '');
        if (B = 0) or (rectBounds.Left > CSV.FieldAsInt(0, 0)) then
          rectBounds.Left := CSV.FieldAsInt(0, 0);
        if (B = 0) or (rectBounds.Right < CSV.FieldAsInt(0, 0)) then
          rectBounds.Right := CSV.FieldAsInt(0, 0);
        if (B = 0) or (rectBounds.Top > CSV.FieldAsInt(1, 0)) then
          rectBounds.Top := CSV.FieldAsInt(1, 0);
        if (B = 0) or (rectBounds.Bottom < CSV.FieldAsInt(1, 0)) then
          rectBounds.Bottom := CSV.FieldAsInt(1, 0);

        CSV.DataString := ReadString(sSection, Format('right%2.2d', [B]), '');
        if (B = 0) or (rectBounds.Left > CSV.FieldAsInt(0, 0)) then
          rectBounds.Left := CSV.FieldAsInt(0, 0);
        if (B = 0) or (rectBounds.Right < CSV.FieldAsInt(0, 0)) then
          rectBounds.Right := CSV.FieldAsInt(0, 0);
        if (B = 0) or (rectBounds.Top > CSV.FieldAsInt(1, 0)) then
          rectBounds.Top := CSV.FieldAsInt(1, 0);
        if (B = 0) or (rectBounds.Bottom < CSV.FieldAsInt(1, 0)) then
          rectBounds.Bottom := CSV.FieldAsInt(1, 0);
      end;  { for each bankpoint }

      tmpWReg := TWaterRegion.Create;
      tmpWReg.FBounds := rectBounds;
      tmpWReg.FZ := ReadInteger(sSection, 'height', 0);
      tmpWReg.FColor := TColor(ReadInteger(sSection, 'color', $ffff00));
      Add(tmpWReg);
    end;  { for each water region }
  end;  { with Sector.DAT }
  
  CSV.Free;
end;

end.
