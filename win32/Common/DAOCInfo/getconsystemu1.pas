unit getconsystemu1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  con_range_def = record
    gray_max:   integer;
    green_max:  integer;
    blue_max:   integer;
    yellow_max: integer;
    orange_max: integer;
    red_max:    integer;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private

  public
    defs:   array[0..99] of con_range_def;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TDAOCConRangeDefinition = packed record
    GrayMax:    Shortint;
    GreenMax:   Shortint;
    BlueMax:    Shortint;
    YellowMax:  Shortint;
    OrangeMax:  Shortint;
    RedMax:     Shortint;
  end;

const
  SEEK_OFFSET = (35270 * 64) + 24;

procedure TForm1.Button1Click(Sender: TObject);
var
  I:    integer;
  FS:   TFileStream;
begin
  FS := TFileStream.Create('c:\mythic\isles\game.dll', fmOpenRead or fmShareDenyNone);
  FS.Seek(SEEK_OFFSET, soFromBeginning);
  for I := 0 to 50 do begin
    FS.Read(defs[I], sizeof(con_range_def));
    Memo1.Lines.Add(Format(
      'LEVEL %2.2d = Gray:00-%02.2d Green:%02.2d-%02.2d Blue:%02.2d-%02.2d ' +
      'Yellow: %02.2d-%02.2d Orange: %02.2d-%02.2d Red:%02.2d-%02.2d Purple:%02.2d',
      [I, defs[I].gray_max,
      defs[I].gray_max+1, defs[I].green_max,
      defs[I].green_max+1, defs[I].blue_max,
      defs[I].blue_max+1, defs[I].yellow_max,
      defs[I].yellow_max+1, defs[I].orange_max,
      defs[I].orange_max+1, defs[I].red_max,
      defs[I].red_max+1
      ]));
  end;
  FS.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  I:    integer;
  FS:   TFileStream;
  s:    string;
begin
  Memo1.Lines.Add(': array[0..50] of TDAOCConRangeDefinition = (');
  FS := TFileStream.Create('c:\mythic\isles\game.dll', fmOpenRead or fmShareDenyNone);
  FS.Seek(SEEK_OFFSET, soFromBeginning);
  s := '';
  for I := 0 to 50 do begin
    FS.Read(defs[I], sizeof(con_range_def));
    s := Format(
      '  (GrayMax: %2d; GreenMax: %2d; BlueMax: %2d; YellowMax: %2d; OrangeMax: %2d; RedMax: %2d),  { Level %d }',
      [defs[I].gray_max, defs[I].green_max, defs[I].blue_max,
       defs[I].yellow_max, defs[I].orange_max, defs[I].red_max, I]);
    Memo1.Lines.Add(s);
  end;
  FS.Free;
  Memo1.Lines.Add(');');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  I:    integer;
  iBlockLow:  integer;
  iBlockHigh: integer;

  FS:   TFileStream;
  procedure WriteLevelRange(AMin, AMax: integer);
  var
    L:    integer;
  begin
    Memo1.Lines.Add('  <tr><td>&nbsp;</td>');
    for L := AMin to AMax do
      Memo1.Lines.Add('    <td>' + IntToStr(L) + '</td>');
    Memo1.Lines.Add('  </tr>');
  end;
begin
  Memo1.Lines.Add('<table border=0 cellspacing=0>');

  FS := TFileStream.Create('c:\mythic\isles\game.dll', fmOpenRead or fmShareDenyNone);
  FS.Seek(SEEK_OFFSET, soFromBeginning);
  for I := 0 to 50 do
    FS.Read(defs[I], sizeof(con_range_def));

  for I := 1 to 50 do begin
    if ((I mod 10) = 1) then begin
      iBlockLow := 1; // defs[I].gray_max;
      iBlockHigh := 61; // defs[I+9].red_max + 1;
      if iBlockLow < 1 then
        iBlockLow := 1;
      WriteLevelRange(iBlockLow, iBlockHigh);
    end;
    Memo1.Lines.Add('  <tr><td>' + IntToStr(I) + '</td>');
//    if defs[I].gray_max - iBlockLow > 0 then
      Memo1.Lines.Add(Format('    <td colspan=%-2d bgcolor="#999999">&nbsp;</td>', [defs[I].gray_max - iBlockLow]));
//    if defs[I].green_max - defs[I].gray_max > 0 then
      Memo1.Lines.Add(Format('    <td colspan=%-2d bgcolor="#00ff00">&nbsp;</td>', [defs[I].green_max - defs[I].gray_max]));
    Memo1.Lines.Add(Format('    <td colspan=%-2d bgcolor="#0033ff">&nbsp;</td>', [defs[I].blue_max - defs[I].green_max]));
    Memo1.Lines.Add(Format('    <td colspan=%-2d bgcolor="#ffff00">&nbsp;</td>', [defs[I].yellow_max - defs[I].blue_max]));
    Memo1.Lines.Add(Format('    <td colspan=%-2d bgcolor="#ff9900">&nbsp;</td>', [defs[I].orange_max - defs[I].yellow_max]));
    Memo1.Lines.Add(Format('    <td colspan=%-2d bgcolor="#ff0000">&nbsp;</td>', [defs[I].red_max - defs[I].orange_max]));
    Memo1.Lines.Add(Format('    <td colspan=%-2d bgcolor="#cc00ff">&nbsp;</td>', [iBlockHigh - defs[I].red_max]));
    Memo1.Lines.Add('  </tr>');
  end;
//  WriteLevelRange;

  FS.Free;
  Memo1.Lines.Add('</table>');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I:    integer;
  FS:   TFileStream;
begin
  Memo1.Lines.Add('const CON_RANGE_DEFINITION CON_RANGES[] = (');

  FS := TFileStream.Create('c:\mythic\isles\game.dll', fmOpenRead or fmShareDenyNone);
  FS.Seek(SEEK_OFFSET, soFromBeginning);
  for I := 0 to 50 do begin
    FS.Read(defs[I], sizeof(con_range_def));
    Memo1.Lines.Add(Format(
      '    {%2d, %2d, %2d, %2d, %2d, %2d},   /* Level %2d */',
      [defs[I].gray_max, defs[I].green_max, defs[I].blue_max,
       defs[I].yellow_max, defs[I].orange_max, defs[I].red_max, I]));
  end;
  FS.Free;
  Memo1.Lines.Add('};');
end;

end.
