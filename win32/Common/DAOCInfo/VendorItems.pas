unit VendorItems;

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
  Classes, SysUtils, Contnrs, DAOCObjs;

type
  TDAOCVendorItem = class(TObject)
  private
    FQuantity: integer;
    FPosition: integer;
    FCost:  integer;
    FName:  string;
    FPage: integer;
  public
    procedure Assign(AItem: TDAOCVendorItem);
    function AsString: string;

    procedure LoadFromReader(AReader: TReader);
    procedure SaveToWriter(AWriter: TWriter);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    property Page: integer read FPage write FPage;
    property Position: integer read FPosition write FPosition;
    property Quantity: integer read FQuantity write FQuantity;
    property Cost: integer read FCost write FCost;
    property Name: string read FName write FName;
  end;

  TDAOCVendorItemList = class(TObjectList)
  private
    FVendor:    TDAOCMob;
    function GetItems(Index: integer): TDAOCVendorItem;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(const AName: string) : TDAOCVendorItem;
    function FindOrAdd(const AName: string) : TDAOCVendorItem;
    function PageOfItem(const AName: string) : integer;
    function PositionOfItem(const AName: string) : integer;
    function CostOfItem(const AName: string): integer;
    function CostOfItemQ1(const AName: string): integer;

    procedure LoadFromReader(AReader: TReader);
    procedure SaveToWriter(AWriter: TWriter);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    function AsString: string;
    procedure Clear; override;

    property Items[Index: integer]: TDAOCVendorItem read GetItems; default;
    property Vendor: TDAOCMob read FVendor;
  end;

  TDAOCMasterVendorList = class(TObjectList)
  private
    function GetItems(Index: integer): TDAOCVendorItemList;
  public
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromFile(AFName: string);
    procedure SaveToFile(AFName: string);

    function FindVendor(AVendorName: string) : TDAOCVendorItemList;
    procedure AddOrUpdate(AVendorItemList: TDAOCVendorItemList);
    function CostOfItem(const AName: string) : integer;
    function CostOfItemQ1(const AName: string): integer;
    function FindItemVendor(const AItemName: string) : TDAOCVendorItemList;
    function FindNearestItemVendor(const AItemName: string; AFrom: TDAOCObject) : TDAOCVendorItemList;

    property Items[Index: integer]: TDAOCVendorItemList read GetItems; default;
  end;

implementation

{ TDAOCVendorItemList }

function TDAOCVendorItemList.AsString: string;
var
  I:    integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].AsString + #13#10;
end;

procedure TDAOCVendorItemList.Clear;
begin
  inherited Clear;
  if Assigned(FVendor) then
    FVendor.Clear;
end;

function TDAOCVendorItemList.CostOfItem(const AName: string): integer;
var
  pItem:  TDAOCVendorItem;
begin
  pItem := Find(AName);
  if Assigned(pItem) then
    Result := pItem.Cost
  else
    Result := 0;
end;

function TDAOCVendorItemList.CostOfItemQ1(const AName: string): integer;
var
  pItem:  TDAOCVendorItem;
begin
  pItem := Find(AName);
  if Assigned(pItem) then
    Result := pItem.Cost div pItem.Quantity 
  else
    Result := 0;
end;

constructor TDAOCVendorItemList.Create;
begin
  inherited Create;
  FVendor := TDAOCMob.Create;
end;

destructor TDAOCVendorItemList.Destroy;
begin
    { we have to freeandnil because the destructor calls Clear and we try to
      clear the vendor there too }
  FreeAndNil(FVendor);
  inherited Destroy;
end;

function TDAOCVendorItemList.Find(const AName: string): TDAOCVendorItem;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(AName, Items[I].Name) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TDAOCVendorItemList.FindOrAdd(const AName: string): TDAOCVendorItem;
begin
  Result := Find(AName);
  if not Assigned(Result) then begin
    Result := TDAOCVendorItem.Create;
    Add(Result);
    Result.Name := AName;
  end;
end;

function TDAOCVendorItemList.GetItems(Index: integer): TDAOCVendorItem;
begin
  Result := TDAOCVendorItem(inherited Items[Index]);
end;

procedure TDAOCVendorItemList.LoadFromReader(AReader: TReader);
var
  pItem:    TDAOCVendorItem;
begin
  Clear;
  Vendor.LoadFromReader(AReader);
  
  AReader.ReadListBegin;
  while not AReader.EndOfList do begin
    pItem := TDAOCVendorItem.Create;
    Add(pItem);
    pItem.LoadFromReader(AReader);
  end;
  AReader.ReadListEnd;
end;

procedure TDAOCVendorItemList.LoadFromStream(AStream: TStream);
var
  R:  TReader;
begin
  R := TReader.Create(AStream, 512);
  try
    LoadFromReader(R);
  finally
    R.Free;
  end;
end;

function TDAOCVendorItemList.PageOfItem(const AName: string): integer;
var
  pItem:  TDAOCVendorItem;
begin
  pItem := Find(AName);
  if Assigned(pItem) then
    Result := pItem.Page
  else
    Result := -1;
end;

function TDAOCVendorItemList.PositionOfItem(const AName: string): integer;
var
  pItem:  TDAOCVendorItem;
begin
  pItem := Find(AName);
  if Assigned(pItem) then
    Result := pItem.Position
  else
    Result := -1;
end;

procedure TDAOCVendorItemList.SaveToStream(AStream: TStream);
var
  W:  TWriter;
begin
  W := TWriter.Create(AStream, 512);
  try
    SaveToWriter(W);
  finally
    W.Free;
  end;
end;

procedure TDAOCVendorItemList.SaveToWriter(AWriter: TWriter);
var
  I:  integer;
begin
  Vendor.SaveToWriter(AWriter);
  
  AWriter.WriteListBegin;
  for I := 0 to Count - 1 do
    Items[I].SaveToWriter(AWriter);
  AWriter.WriteListEnd;
end;

{ TDAOCVendorItem }

procedure TDAOCVendorItem.Assign(AItem: TDAOCVendorItem);
begin
  FQuantity := AItem.Quantity;
  FPosition := AItem.Position;
  FCost := AItem.Cost;
  FName := AItem.Name;
  FPage := AItem.Page;
end;

function TDAOCVendorItem.AsString: string;
begin
  Result := Format('(%d,%d) %dx %s (%d c)', [FPage, FPosition, FQuantity, FName, FCost]);
end;

procedure TDAOCVendorItem.LoadFromReader(AReader: TReader);
begin
  FQuantity := AReader.ReadInteger;
  FPosition := AReader.ReadInteger;
  FCost := AReader.ReadInteger;
  FName := AReader.ReadString;
  FPage := AReader.ReadInteger;
end;

procedure TDAOCVendorItem.LoadFromStream(AStream: TStream);
var
  R:  TReader;
begin
  R := TReader.Create(AStream, 512);
  try
    LoadFromReader(R);
  finally
    R.Free;
  end;
end;

procedure TDAOCVendorItem.SaveToStream(AStream: TStream);
var
  W:  TWriter;
begin
  W := TWriter.Create(AStream, 512);
  try
    SaveToWriter(W);
  finally
    W.Free;
  end;
end;

procedure TDAOCVendorItem.SaveToWriter(AWriter: TWriter);
begin
  AWriter.WriteInteger(FQuantity);
  AWriter.WriteInteger(FPosition);
  AWriter.WriteInteger(FCost);
  AWriter.WriteString(FName);
  AWriter.WriteInteger(FPage);
end;

{ TDAOCMasterVendorList }

procedure TDAOCMasterVendorList.AddOrUpdate(AVendorItemList: TDAOCVendorItemList);
var
  pVendorInList:  TDAOCVendorItemList;
  pItem:  TDAOCVendorItem;
  I:    integer;
begin
  if AVendorItemList.Vendor.Name = '' then
    exit;

  pVendorInList := FindVendor(AVendorItemList.Vendor.Name);
  if not Assigned(pVendorInList) then begin
    pVendorInList := TDAOCVendorItemList.Create;
    Add(pVendorInList);
    pVendorInList.Vendor.Assign(AVendorItemList.Vendor);
  end;

  for I := 0 to AVendorItemList.Count - 1 do begin
    pItem := pVendorInList.FindOrAdd(AVendorItemList[I].Name);
    pItem.Assign(AVendorItemList[I]);
  end;
end;

function TDAOCMasterVendorList.CostOfItem(const AName: string): integer;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do begin
    Result := Items[I].CostOfItem(AName);
    if Result <> 0 then
      exit;
  end;

  Result := 0;
end;

function TDAOCMasterVendorList.CostOfItemQ1(const AName: string): integer;
var
  I:    integer;
begin
  for I := 0 to Count - 1 do begin
    Result := Items[I].CostOfItemQ1(AName);
    if Result <> 0 then
      exit;
  end;

  Result := 0;
end;

function TDAOCMasterVendorList.FindItemVendor(const AItemName: string): TDAOCVendorItemList;
var
  I:  integer;
begin
  for I := 0 to Count - 1 do
    if Assigned(Items[I].Find(AItemName)) then begin
      Result := Items[I];
      exit;
    end;

  Result := nil;
end;

function TDAOCMasterVendorList.FindNearestItemVendor(
  const AItemName: string; AFrom: TDAOCObject): TDAOCVendorItemList;
var
  iMinIdx:  integer;
  dDist:    double;
  dMinDist: double;
  I:  integer;
begin
  iMinIdx := -1;
  dMinDist := 0;

  for I := 0 to Count - 1 do
    if Assigned(Items[I].Find(AItemName)) then begin
      dDist := Items[I].Vendor.Distance2D(AFrom);
      if (iMinIdx = -1) or (dMinDist > dDist) then begin
        dMinDist := dDist;
        iMinIdx := I;
      end;  { if new low }
    end;  { if vendor has item }

  if iMinIdx <> -1 then
    Result := Items[iMinIdx]
  else
    Result := nil;
end;

function TDAOCMasterVendorList.FindVendor(AVendorName: string): TDAOCVendorItemList;
var
  I:  integer;
begin
  if AVendorName <> '' then
    for I := 0 to Count - 1 do
      if AnsiSameText(Items[I].Vendor.Name, AVendorName) then begin
        Result := Items[I];
        exit;
      end;

  Result := nil;
end;

function TDAOCMasterVendorList.GetItems(Index: integer): TDAOCVendorItemList;
begin
  Result := TDAOCVendorItemList(inherited Items[Index]);
end;

procedure TDAOCMasterVendorList.LoadFromFile(AFName: string);
var
  FS:   TFileStream;
begin
  Clear;

  FS := TFileStream.Create(AFName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TDAOCMasterVendorList.LoadFromStream(AStream: TStream);
var
  Reader:   TReader;
  pVendorList:  TDAOCVendorItemList;
begin
  Reader := TReader.Create(AStream, 512);
  try
    Reader.ReadListBegin;
    while not Reader.EndOfList do begin
      pVendorList := TDAOCVendorItemList.Create;
      Add(pVendorList);
      pVendorList.LoadFromReader(Reader);
    end;
    Reader.ReadListEnd;
  finally
    Reader.Free;
  end;
end;

procedure TDAOCMasterVendorList.SaveToFile(AFName: string);
var
  FS:   TFileStream;
begin
  FS := TFileStream.Create(AFName, fmCreate or fmShareDenyWrite);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TDAOCMasterVendorList.SaveToStream(AStream: TStream);
var
  Writer:   TWriter;
  I:        integer;
begin
  Writer := TWriter.Create(AStream, 512);
  try
    Writer.WriteListBegin;
    for I := 0 to Count - 1 do
        Items[I].SaveToWriter(Writer);
    Writer.WriteListEnd;
  finally
    Writer.Free;
  end;
end;

end.
