unit PEFuncs;

interface

uses
  Windows, SysUtils, Classes, Contnrs;

type
  TObjectTableEntry = class(TObject)
  private
    FPhysicalOffset: DWORD;
    FRVA: DWORD;
    FVirtualSize: DWORD;
    FPhysicalSize: DWORD;
    FObjectType: string;
  public
    property ObjectType: string read FObjectType write FObjectType;
    property VirtualSize: DWORD read FVirtualSize write FVirtualSize;
    property RVA: DWORD read FRVA write FRVA;
    property PhysicalSize: DWORD read FPhysicalSize write FPhysicalSize;
    property PhysicalOffset: DWORD read FPhysicalOffset write FPhysicalOffset;
  end;

  TObjectTable = class(TObjectList)
  private
    function GetItems(I: integer): TObjectTableEntry;
  public
    function FindSegment(const AName: string) : TObjectTableEntry;
    property Items[I: integer]: TObjectTableEntry read GetItems; default;
  end;

  TPortableExecutable = class(TObject)
  private
    FObjectTable: TObjectTable;
    FImageBase: DWORD;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const AFName: string);

    property ObjectTable: TObjectTable read FObjectTable;
    property ImageBase: DWORD read FImageBase;
  end;

implementation

{ TObjectTable }

function TObjectTable.FindSegment(const AName: string): TObjectTableEntry;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].ObjectType, AName) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TObjectTable.GetItems(I: integer): TObjectTableEntry;
begin
  Result := TObjectTableEntry(inherited Items[I]);
end;

{ TPortableExecutable }

constructor TPortableExecutable.Create;
begin
  inherited Create;
  FObjectTable := TObjectTable.Create;
end;

destructor TPortableExecutable.Destroy;
begin
  FObjectTable.Free;
  inherited Destroy;
end;

procedure TPortableExecutable.LoadFromFile(const AFName: string);
var
  fs: TFilestream;
  I:    integer;
  signature: DWORD;
  dos_header: IMAGE_DOS_HEADER;
  pe_header:  IMAGE_FILE_HEADER;
  opt_header: IMAGE_OPTIONAL_HEADER;
  objtbl_entry:  IMAGE_SECTION_HEADER;
  pObjEnt:    TObjectTableEntry;
begin
  FObjectTable.Clear;
  FImageBase := 0;

  fs := TFilestream.Create(AFName, fmOpenread or fmShareDenyNone);
  try
    fs.read(dos_header, sizeof(dos_header));
    if dos_header.e_magic <> IMAGE_DOS_SIGNATURE then
      raise Exception.Create('Invalid DOS file header');

    fs.seek(dos_header._lfanew, soFromBeginning);
    fs.read(signature, SizeOf(signature));
    if signature <> IMAGE_NT_SIGNATURE then
      raise Exception.Create('Invalid PE header');

    fs.read(pe_header, sizeof(pe_header));
    if pe_header.SizeOfOptionalHeader > 0 then begin
      fs.read(opt_header, sizeof(opt_header));
      FImageBase := opt_header.ImageBase;

      for I := 0 to pe_header.NumberOfSections - 1 do begin
        fs.Read(objtbl_entry, sizeof(IMAGE_SECTION_HEADER));

        pObjEnt := TObjectTableEntry.Create;
        FObjectTable.Add(pObjEnt);
        pObjEnt.PhysicalOffset := objtbl_entry.Misc.PhysicalAddress;
        pObjEnt.RVA := objtbl_entry.VirtualAddress;
        pObjEnt.VirtualSize := objtbl_entry.Misc.VirtualSize;
        pObjEnt.ObjectType := PChar(@objtbl_entry.name);
        pObjEnt.PhysicalSize := objtbl_entry.SizeOfRawData;
      end;  { for each object }
    end;  { if opt header }
  finally
    fs.Free;
  end; { finally }
end;

end.
 