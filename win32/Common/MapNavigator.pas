unit MapNavigator;

interface

(****
  Map nodes are layed out in a top-down coordinate system (y-axis reversed):
  (0,0) --------> +x
        |
        |
        V
        +y
  Map nodes work on the REGIONAL coordinate system, not the ZONE coordinate
  system (zones are subregions).  This means that LOCs run from
  0 -> 2^32, not 0 -> 65535.  The reason being that if you want to go from
  Gotar (32k, 19k) to W_Svea (9k, 45k), there'd have to be calculations to figure
  out if W_Svea was reachable from Gotar, then translate Gotar coords to regional
  then W_Svea to regional, then find the heading.  Since we don't have ZoneInfo,
  that would be impossible.

  Headings are also different from the normal coordinate system.  0 is up, 180 is
  down, and the value increases in a clockwise direction.  This means that all
  trigonometric functions should have their X and Y swapped, and the Y should be
  negated.
****)

uses
  Types, SysUtils, Classes, Math, Contnrs;

type
  TMapNode = class;
  TMapNodeList = class;

  TMapVector = class(TObject)
  private
    FFromNode:  TMapNode;
    FToNode:    TMapNode;
  public
    property FromNode: TMapNode read FFromNode;
    property ToNode: TMapNode read FToNode;
  end;

  TMapVectorList = class(TObjectList)
  private
    function GetItems(iIndex: integer): TMapVector;
  public
    property Items[iIndex: integer]: TMapVector read GetItems; default;
  end;

  TMapNode = class(TObject)
  private
    FX: integer;
    FY: integer;
    FZ: integer;
    FHead:  integer;
    FName:  string;
    FConnections: TList;
    FRegion:  integer;
    FNodeTraversalCost: integer;
    FRadius: integer;
    function GetConnections(iIndex: integer): TMapNode;
    function GetConnectionCount: integer;
  protected
    function IsConnectedToRecur(ANode: TMapNode; AHopsLeft: integer;
      AVisitedNodes: TMapNodeList) : boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddConnection(ANode: TMapNode);
    function ConnectionByName(const AName: string) : TMapNode;

    function Distance2D(ANode: TMapNode) : integer; overload;
    function Distance2D(AX, AY: integer) : integer; overload;
    function Distance3D(ANode: TMapNode) : integer; overload;
    function Distance3D(AX, AY, AZ: integer) : integer; overload;
    function BearingTo(ANode: TMapNode) : integer; overload;
    function BearingTo(AX, AY: integer) : integer; overload;
    function BearingFrom(ANode: TMapNode) : integer; overload;
    function BearingFrom(AX, AY: integer) : integer; overload;
    function IsConnectedTo(ANode: TMapNode; AMaxHops: integer) : boolean;
    function FindPathTo(ANode: TMapNode) : TMapNodeList;

    function AsText : string;

    property Region: integer read FRegion write FRegion;
    property X: integer read FX write FX;
    property Y: integer read FY write FY;
    property Z: integer read FZ write FZ;
    property Head: integer read FHead write FHead;
    property Name: string read FName write FName;
    property Radius: integer read FRadius write FRadius;

    property NodeTraversalCost: integer read FNodeTraversalCost write FNodeTraversalCost; 

    property ConnectionCount: integer read GetConnectionCount;
    property Connections[iIndex: integer]: TMapNode read GetConnections;
  end;

  TMapNodeList = class(TObject)
  private
    FList:    TList;
    FOwnsNodes: boolean;
    FDefaultRadius: integer;
    function GetCount: integer;
    function GetNodes(iIndex: integer): TMapNode;
  protected
    procedure CheckNodeForAddition(ANode: TMapNode);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Sort;
    procedure Clear;
    procedure Add(ANode: TMapNode);
    procedure Insert(APos: integer; ANode: TMapNode);
    function FindOrAddNode(const AName: string) : TMapNode;
    function NodeByName(const AName: string) : TMapNode;
    function NodeInList(ANode: TMapNode) : boolean;
    function NodesAsText : string;
    function FindNearestNode(X, Y: integer; ConnectedOnly: boolean) : TMapNode; overload;
    function FindNearestNode(ANode: TMapNode; ConnectedOnly: boolean) : TMapNode; overload;
    function FindNearestNode3D(X, Y, Z: integer; ConnectedOnly: boolean): TMapNode; overload;
    function FindNearestNode3D(ANode: TMapNode; ConnectedOnly: boolean) : TMapNode; overload;
    procedure SaveToFile(const AFName: string);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromFile(const AFName: string);
    procedure FindBoundingRect(var R: TRect);

    property DefaultRadius: integer read FDefaultRadius write FDefaultRadius;
    property OwnsNodes: boolean read FOwnsNodes write FOwnsNodes;
    property Count: integer read GetCount;
    property Nodes[iIndex: integer]: TMapNode read GetNodes; default;
  end;

implementation

{ TMapNode }

procedure TMapNode.AddConnection(ANode: TMapNode);
begin
  if not Assigned(ANode) then
    raise Exception.Create(Name + ': Cannot add a connection to the NULL node.');

  if ANode = Self then
    raise Exception.Create(Name + ': Cannot add a connection to self.');

  if IsConnectedTo(ANode, 1) then
    raise Exception.Create(Name + ': Already connected to node ' + ANode.Name);

  FConnections.Add(ANode);
end;

function TMapNode.AsText: string;
var
  I:        Integer;
  pRNode:   TMapNode;
begin
  Result := Format('Map Node [%s]'#13#10 +
    '  (X,Y,Z): (%d, %d, %d) Region %d'#13#10 +
    '  Head: %d degrees  Radius: %d'#13#10,
    [Name, X, Y, Z, Region, Head, Radius]);

  if FConnections.Count > 0 then
    Result := Result + '  Connected to:'#13#10;

  for I := 0 to FConnections.Count - 1 do begin
    pRNode := GetConnections(I);
    Result := Result + Format('    [%s] bearing %d (back %d) dist %d'#13#10,
      [pRNode.Name, BearingTo(pRNode), BearingFrom(pRNode), Distance2D(pRNode)]);
  end;  { for }

//  AppendStr(Result, #13#10);
end;

function TMapNode.BearingFrom(ANode: TMapNode): integer;
begin
  Result := BearingFrom(ANode.X, ANode.Y);
end;

function TMapNode.BearingTo(ANode: TMapNode): integer;
begin
  Result := BearingTo(ANode.X, ANode.Y);
end;

function TMapNode.BearingTo(AX, AY: integer): integer;
(*** Returns the bearing to in DOAC heading [0..359] ***)
var
  dx:   integer;
begin
  dx := (X - AX);
  if dx = 0 then
    if Y < AY then
      Result := 180
    else
      Result := 0
  else
    Result := round(RadToDeg(ArcTan2(dx, AY - Y)));

  if Result < 0 then
    inc(Result, 360);
end;

function TMapNode.ConnectionByName(const AName: string): TMapNode;
var
  I: Integer;
begin
   for I := 0 to FConnections.Count - 1 do
    if AnsiSameText(AName, GetConnections(I).Name) then begin
      Result := GetConnections(I);
      exit;
    end;

  Result := nil;
end;

constructor TMapNode.Create;
begin
  inherited Create;
  FConnections := TList.Create;
end;

destructor TMapNode.Destroy;
begin
  FreeAndNil(FConnections);
  inherited Destroy;
end;

function TMapNode.Distance2D(ANode: TMapNode): integer;
begin
  Result := Distance2D(ANode.X, ANode.Y);
end;

function TMapNode.Distance3D(ANode: TMapNode): integer;
begin
  Result := Distance3D(ANode.X, ANode.Y, ANode.Z);
end;

function TMapNode.FindPathTo(ANode: TMapNode): TMapNodeList;
type
  Pdijkstra_info = ^dijkstra_info;
  dijkstra_info = record
    node:     TMapNode;
    weight:   integer;
    parent:   TMapNode;
  end;
var
  I:          integer;
  lstFringe:  TList;
  lstSPaths:  TList;
  pNode:      TMapNode;
  uNodeIdx:   integer;
  uNode:      Pdijkstra_info;
  vNodeIdx:   integer;
  vNode:      Pdijkstra_info;
  iUVWeight:  integer;

  procedure AddNodeToFringe(pNode, pParent: TMapNode; iWeight: integer);
  var
    pdi:        Pdijkstra_info;
  begin
    New(pdi);
    pdi^.node := pNode;
    pdi^.weight := iWeight;
    pdi^.parent := pParent;
    lstFringe.Add(pdi);
  end;

  procedure AddSelfToSPaths;
  var
    pdi:        Pdijkstra_info;
  begin
    New(pdi);
    pdi^.node := Self;
    pdi^.weight := 0;
    pdi^.parent := nil;
    lstSPaths.Add(pdi);
  end;

  function MinWeightNodeIdx : integer;
  var
    I:          integer;
    iMinIdx:    integer;
    iMinWeight: integer;
  begin
    iMinIdx := -1;
    iMinWeight := 0;
    for I := 0 to lstFringe.Count - 1 do
      if (iMinIdx = -1) or (Pdijkstra_info(lstFringe[I])^.weight < iMinWeight) then begin
        iMinIdx := I;
        iMinWeight := Pdijkstra_info(lstFringe[I])^.weight;
      end;
    Result := iMinIdx;
  end;

  function FindNodeIdxInFringe(pNode: TMapNode) : integer;
  begin
    for Result := 0 to lstFringe.Count - 1 do
      if Pdijkstra_info(lstFringe[Result])^.node = pNode then
        exit;
    Result := -1;
  end;

  function FindNodeIdxInSPaths(pNode: TMapNode) : integer;
  begin
    for Result := 0 to lstSPaths.Count - 1 do
      if Pdijkstra_info(lstSPaths[Result])^.node = pNode then
        exit;
    Result := -1;
  end;
begin
(***
  Create a list FRINGE_SET with all nodes connected from SRC
  for each E in FRINGE_SET
    E.parent = SRC
    E.distance = distance S -> E
  Add SRC to SPaths with distance 0
  while FRINGE_SET.count > 0
    u = node with min distance from FRINGE_SET
    SP.add(u)
    for each node v connected to u
      if v not in SP
        if v not in FRINGE_SET
          v.distance = (u.distance + distance u -> v)
          v.parent = u
        else if (u.distance + distance u -> v) < v.distance
          v.distance = (u.distance + distance u -> v)
          v.parent = u
    end for
  end while
***)
  lstFringe := TList.Create;
  lstSPaths := TList.Create;

  AddSelfToSPaths;

  for I := 0 to FConnections.Count - 1 do begin
    pNode := GetConnections(I);
    AddNodeToFringe(pNode, Self, Distance2D(pNode));
  end;

  while lstFringe.Count > 0 do begin
      { find the node with the lowest weight from lstFringe }
    uNodeIdx := MinWeightNodeIdx;
    uNode := Pdijkstra_info(lstFringe[uNodeIdx]);
      { remove it from the fringe and add it to the shortest paths }
    lstFringe.Delete(uNodeIdx);
    lstSPaths.Add(uNode);

    for I := 0 to uNode^.node.ConnectionCount - 1 do begin
      pNode := uNode^.node.Connections[I];
        { if we already have a shortest path, skip it }
      if FindNodeIdxInSPaths(pNode) <> -1 then
        continue;
      vNodeIdx := FindNodeIdxInFringe(pNode);
      iUVWeight := uNode^.weight + uNode^.node.Distance2D(pNode) + FNodeTraversalCost;
        { if vNode isn't in Fringe add it to Fringe }
      if vNodeIdx = -1 then
        AddNodeToFringe(pNode, uNode^.node, iUVWeight)
      else begin
        vNode := Pdijkstra_info(lstFringe[vNodeIdx]);
        if iUVWeight < vNode^.weight then begin
          vNode^.weight := iUVWeight;
          vNode^.parent := uNode^.node;
        end;
      end;
    end;  { for each node connected to uNode }
  end;  { while Fringe }

    { fringe list should be empty at this point }
  lstFringe.Free;

    { ok now we have a list of destnode, parent, metric for every possible
      dest.  See if our dest is in there }
  vNodeIdx := FindNodeIdxInSPaths(ANode);
  if vNodeIdx = -1 then
    Result := nil
  else begin
    //ODS(PChar('Shortest path is distance: ' +
    //  IntToStr(Pdijkstra_info(lstSPaths[vNodeIdx])^.weight)));
    Result := TMapNodeList.Create;
    Result.OwnsNodes := false;
    repeat
      Result.Insert(0, Pdijkstra_info(lstSPaths[vNodeIdx])^.node);
      vNodeIdx := FindNodeIdxInSPaths(Pdijkstra_info(lstSPaths[vNodeIdx])^.Parent);
    until vNodeIdx = -1; // not Assigned(Pdijkstra_info(lstSPaths[vNodeIdx])^.parent);
  end;  { if Dest node is in SPaths }

    { free all the info structures and the Shortest Paths list }
  for I := 0 to lstSPaths.Count - 1 do
    Dispose(Pdijkstra_info(lstSPaths[I]));
  lstSPaths.Free;
end;

function TMapNode.GetConnectionCount: integer;
begin
  Result := FConnections.Count;
end;

function TMapNode.GetConnections(iIndex: integer): TMapNode;
begin
  Result := TMapNode(FConnections[iIndex]);
end;

function TMapNode.IsConnectedTo(ANode: TMapNode;
  AMaxHops: integer): boolean;
var
  pVisitList: TMapNodeList;
begin
  pVisitList := TMapNodeList.Create;
  pVisitList.OwnsNodes := false;
  Result := IsConnectedToRecur(ANode, AMaxHops, pVisitList);
  pVisitList.Free;
end;

function TMapNode.IsConnectedToRecur(ANode: TMapNode; AHopsLeft: integer;
  AVisitedNodes: TMapNodeList): boolean;
var
  I:  Integer;
begin
  Result := Assigned(ConnectionByName(ANode.Name));

  if not Result then begin
    dec(AHopsLeft);
    if AHopsLeft <= 0 then
      exit;

    AVisitedNodes.Add(Self);
    for I := 0 to FConnections.Count - 1 do
      if not AVisitedNodes.NodeInList(GetConnections(I)) then begin
        Result := GetConnections(I).IsConnectedToRecur(ANode, AHopsLeft, AVisitedNodes);
        if Result then
          break;
      end;  { if node not visited }
  end;  { if not found }
end;

function TMapNode.BearingFrom(AX, AY: integer): integer;
begin
  Result := (BearingTo(AX, AY) + 180) mod 360;
end;

function TMapNode.Distance2D(AX, AY: integer): integer;
var
  dx:   double;
  dy:   double;
begin
  dx := (AX - X);
  dy := (AY - Y);
  Result := round(sqrt((dx * dx) + (dy * dy)));
end;

function TMapNode.Distance3D(AX, AY, AZ: integer): integer;
var
  dx:   double;
  dy:   double;
  dz:   double;
begin
  dx := (AX - X);
  dy := (AY - Y);
  dz := (AZ - Z);
  Result := round(sqrt((dx * dx) + (dy * dy) + (dz * dz)));
end;

procedure TMapNode.Clear;
begin
  FX := 0;
  FY := 0;
  FZ := 0;
  FHead := 0;
  FName := '';
  FConnections.Clear;
  FRegion := 0;
  FRadius := 0;
    { TODO: Make NodeTraversalCost be dynamic, based on how much we have to turn }
  FNodeTraversalCost := 0; // 50;
end;

{ TMapNodeList }

procedure TMapNodeList.Add(ANode: TMapNode);
begin
  CheckNodeForAddition(ANode);
  FList.Add(ANode);
end;

procedure TMapNodeList.CheckNodeForAddition(ANode: TMapNode);
begin
  if ANode.Name = '' then
    raise Exception.Create('Node must have a name.');

  if Assigned(NodeByName(ANode.Name)) then
    raise Exception.Create('There is already a node named ' + ANode.Name +
      ' in the list.');
end;

procedure TMapNodeList.Clear;
var
  I: Integer;
begin
  if FOwnsNodes then
    for I := 0 to FList.Count - 1 do
      TMapNode(FList[I]).Free;
  FList.Clear;
end;

constructor TMapNodeList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FDefaultRadius := 75;
end;

destructor TMapNodeList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TMapNodeList.FindNearestNode(X, Y: integer; ConnectedOnly: boolean): TMapNode;
var
  pTmpNode: TMapNode;
begin
  pTmpNode := TMapNode.Create;
  pTmpNode.X := X;
  pTmpNode.Y := Y;

  Result := FindNearestNode(pTmpNode, ConnectedOnly);

  pTmpNode.Free;
end;

procedure TMapNodeList.FindBoundingRect(var R: TRect);
var
  pNode:  TMapNode;
  I:      integer;
begin
  if FList.Count = 0 then
    exit;

  for I := 0 to FList.Count - 1 do begin
    pNode := GetNodes(I);
    if (I = 0) or (R.Left > pNode.FX) then
      R.Left := pNode.FX;
    if (I = 0) or (R.Right < pNode.FX) then
      R.Right := pNode.FX;
    if (I = 0) or (R.Top > pNode.FY) then
      R.Top := pNode.FY;
    if (I = 0) or (R.Bottom < pNode.FY) then
      R.Bottom := pNode.FY;
  end;
end;

function TMapNodeList.FindNearestNode(ANode: TMapNode; ConnectedOnly: boolean): TMapNode;
var
  I:        Integer;
  iMinIdx:  integer;
  iMinDist: integer;
  iDist:    integer;
  pNode:    TMapNode;
begin
  iMinIdx := -1;
  iMinDist := -1;

  for I := 0 to FList.Count - 1 do begin
    pNode := GetNodes(I);
    if ConnectedOnly and (pNode.ConnectionCount = 0) then
      continue;
    iDist := pNode.Distance2D(ANode);
    if (iMinIdx = -1) or (iDist < iMinDist) then begin
      iMinIdx := I;
      iMinDist := iDist;
    end;
  end;  { for }

  if iMinIdx = -1 then
    Result := nil
  else
    Result := GetNodes(iMinIdx);
end;

function TMapNodeList.FindNearestNode3D(X, Y, Z: integer; ConnectedOnly: boolean): TMapNode;
var
  pTmpNode: TMapNode;
begin
  pTmpNode := TMapNode.Create;
  pTmpNode.X := X;
  pTmpNode.Y := Y;
  pTmpNode.Z := Z;

  Result := FindNearestNode(pTmpNode, ConnectedOnly);

  pTmpNode.Free;
end;

function TMapNodeList.FindNearestNode3D(ANode: TMapNode; ConnectedOnly: boolean): TMapNode;
var
  I:        Integer;
  iMinIdx:  integer;
  iMinDist: integer;
  iDist:    integer;
  pNode:    TMapNode;
begin
  iMinIdx := -1;
  iMinDist := -1;

  for I := 0 to FList.Count - 1 do begin
    pNode := GetNodes(I);
    if ConnectedOnly and (pNode.ConnectionCount = 0) then
      continue;
    iDist := pNode.Distance3D(ANode);
    if (iMinIdx = -1) or (iDist < iMinDist) then begin
      iMinIdx := I;
      iMinDist := iDist;
    end;
  end;  { for }

  if iMinIdx = -1 then
    Result := nil
  else
    Result := GetNodes(iMinIdx);
end;

function TMapNodeList.FindOrAddNode(const AName: string): TMapNode;
begin
  Result := NodeByName(AName);
  if not Assigned(Result) then begin
    Result := TMapNode.Create;
    Result.Name := ANAme;
    Add(Result);
  end;
end;

function TMapNodeList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TMapNodeList.GetNodes(iIndex: integer): TMapNode;
begin
  Result := TMapNode(FList[iIndex]);
end;

procedure TMapNodeList.Insert(APos: integer; ANode: TMapNode);
begin
  CheckNodeForAddition(ANode);
  FList.Insert(APos, ANode);
end;

procedure TMapNodeList.LoadFromFile(const AFName: string);
var
  s:    string;
  sl:   TStringList;
  f:    TextFile;
  pNode:  TMapNode;
  pConNode: TMapNode;
begin
  Clear;
  AssignFile(f, AFName);
  Reset(f);

  pNode := nil;
  try
    sl := TStringList.Create;
    while not EOF(f) do begin
      ReadLn(f, s);

        { if there's no commas in the thing, assume it's a connection }
      if Assigned(pNode) and (Pos(',', s) = 0) then begin
        pConNode := FindOrAddNode(Trim(s));
        pNode.AddConnection(pConNode);
      end
      else begin
        sl.CommaText := s;
        if sl.Count >= 6 then begin
          pNode := FindOrAddNode(sl[0]);
          pNode.Region := StrToIntDef(sl[1], 0);
          pNode.X := StrToIntDef(sl[2], 0);
          pNode.Y := StrToIntDef(sl[3], 0);
          pNode.Z := StrToIntDef(sl[4], 0);
          pNode.Head := StrToIntDef(sl[5], 0);
          if sl.Count > 6 then 
            pNode.Radius := StrToIntDef(sl[6], FDefaultRadius);
          if pNode.Radius = 0 then
            pNode.Radius := FDefaultRadius;
        end;  { if sl.Count = 6 }
      end;  { if contains commas }
    end;    { while !EOF }

    sl.Free;
  finally
    CloseFile(f);
  end;
  
  Sort;
end;

function TMapNodeList.NodeByName(const AName: string): TMapNode;
var
  I:    integer;
begin
  for I := 0 to FList.Count - 1 do
    if AnsiSameText(AName, GetNodes(I).Name) then begin
      Result := GetNodes(I);
      exit;
    end;
    
  Result := nil;
end;

function TMapNodeList.NodeInList(ANode: TMapNode): boolean;
var
  I:  integer;
begin
  for I := 0 to FList.Count - 1 do
    if ANode = FList[I] then begin
      Result := true;
      exit;
    end;
    
  Result := false;
end;

function TMapNodeList.NodesAsText: string;
var
  I:    integer;
begin
  Result := '';
  for I := 0 to FList.Count - 1 do 
    Result := Result + GetNodes(I).AsText;
end;

procedure TMapNodeList.SaveToFile(const AFName: string);
var
  FS:   TFileStream;
begin
  fs := TFileStream.Create(AFName, fmCreate or fmShareDenyWrite);
  SaveToStream(fs);
  fs.Free;
end;

procedure TMapNodeList.SaveToStream(AStream: TStream);
var
  I:  Integer;
  J:  integer;
  procedure DoWrite(const S: string); begin AStream.Write(s[1], Length(s)); end;
begin
  for I := 0 to Count - 1 do
    with Nodes[I] do begin
      DoWrite(Format('"%s",%d,%d,%d,%d,%d,%d'#13#10, [Name, Region, X, Y,
        Z, Head, Radius]));
      for J := 0 to ConnectionCount - 1 do
        DoWrite('    ' + Connections[J].Name + #13#10);
    end;  { with Node / for i to count}
end;

procedure TMapNodeList.Sort;
var
  I:  integer;
  J:  integer;
  iMinIdx:  integer;
begin
  for I := 0 to Count - 2 do begin
    iMinIdx := I;
    for J := I + 1 to Count - 1 do
      if AnsiCompareText(Nodes[J].Name, Nodes[iMinIdx].Name) < 0 then
        iMinIdx := J;
    if iMinIdx <> I then
      FList.Exchange(iMinIdx, I);
  end;  { for I }
end;

{ TMapVectorList }

function TMapVectorList.GetItems(iIndex: integer): TMapVector;
begin
  Result := TMapVector(inherited Items[iIndex]);
end;

end.
