unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, MPKFile, Buttons, Menus,
  ActnList, ShellAPI;

type
  TfrmMain = class(TForm)
    treeDirectory: TVirtualStringTree;
    pnlTop: TPanel;
    Label1: TLabel;
    edtFileName: TEdit;
    btnBrowse: TSpeedButton;
    OpenDialog: TOpenDialog;
    ActionList: TActionList;
    atnViewWithNotepad: TAction;
    pumListActions: TPopupMenu;
    Viewwithinternalviewer1: TMenuItem;
    atnExtractToDir: TAction;
    Extract1: TMenuItem;
    atnView: TAction;
    Viewwithassociatedviewer1: TMenuItem;
    procedure edtFileNameKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure treeDirectoryGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure btnBrowseClick(Sender: TObject);
    procedure atnViewWithNotepadExecute(Sender: TObject);
    procedure atnViewExecute(Sender: TObject);
    procedure treeDirectoryDblClick(Sender: TObject);
    procedure atnExtractToDirExecute(Sender: TObject);
  private
    FMPK:   TMPKFile;
    FViewProcess: THANDLE;
    procedure LoadMPKFile(const AFName: string);
    function SelectedDirEntry : TMPKDirectoryEntry;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  Viewer;

function IntToSize(ASize: integer) : string;
var
  sSize:  string;
  modulus: integer;
begin
  Result := '';
  while ASize > 0 do begin
    modulus := ASize mod 1000;
    ASize := ASize div 1000;

      { if we are going to put something before this, make sure we have all 3 numbers }
    if ASize > 0 then
      sSize := Format('%3.3d', [modulus])
    else
      sSize := IntToStr(modulus);

    if Result = '' then
      Result := sSize
    else
      Result := sSize + ',' + Result;
  end;
end;

procedure TfrmMain.edtFileNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    LoadMPKFile(edtFileName.Text);
    Key := #0;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMPK := TMPKFile.Create('');
  treeDirectory.RootNodeCount := 0;

  if ParamCount > 0 then begin
    edtFileName.Text := ParamStr(1);
    LoadMPKFile(edtFileName.Text);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMPK);
end;

procedure TfrmMain.LoadMPKFile(const AFName: string);
var
  I:    integer;
//  Node: PVirtualNode;
begin
  treeDirectory.Clear;
  FMPK.FileName := AFName;

  Caption := Format('(%s) %s', [FMPK.InternalName, FMPK.FileName]);
  for I := 0 to FMPK.Directory.Count - 1 do
    treeDirectory.AddChild(nil, FMPK.Directory[I]);
end;

procedure TfrmMain.treeDirectoryGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  de:   TMPKDirectoryEntry;
begin
  de := TMPKDirectoryEntry(treeDirectory.GetNodeData(Node)^);
  if not Assigned(de) then
    exit;

  case Column of
    0:  CellText := de.Name;
    1:  CellText := FormatDateTime('ddddd h:nn AMPM', de.LastModified);
    2:  CellText := IntToSize(de.UncompressedSize);
    3:  CellText := IntToStr(de.CompressRatio) + '%';
    4:  CellText := IntToSize(de.CompressedSize);
    5:  CellText := LowerCase(IntToHex(de.CRC32, 8));
  end;
end;

procedure TfrmMain.btnBrowseClick(Sender: TObject);
begin
  OpenDialog.FileName := edtFileName.Text;
  if OpenDialog.Execute then begin
    edtFileName.Text := OpenDialog.FileName;
    LoadMPKFile(edtFileName.Text);
  end;
end;

procedure TfrmMain.atnViewWithNotepadExecute(Sender: TObject);
var
  de:   TMPKDirectoryEntry;
begin
  de := SelectedDirEntry;
  if not Assigned(de) then
    exit;

  with TfrmViewer.Create(Self) do begin
    Caption := 'Viewer - ' + de.Name;
    DisplayAndFreeStream(FMPK.ExtractStream(de.Name));
    ShowModal;
    Free;
  end;
end;

function TfrmMain.SelectedDirEntry: TMPKDirectoryEntry;
begin
  if Assigned(treeDirectory.FocusedNode) then
    Result := TMPKDirectoryEntry(treeDirectory.GetNodeData(treeDirectory.FocusedNode)^)
  else
    Result := nil;
end;

procedure TfrmMain.atnViewExecute(Sender: TObject);
var
  de:   TMPKDirectoryEntry;
  sTmp: string;
begin
  de := SelectedDirEntry;
  if not Assigned(de) then
    exit;

  SetLength(sTmp, 1024);
  GetTempPath(1024, PChar(sTmp));
  SetLength(sTmp, StrLen(PChar(sTmp)));
  sTmp := IncludeTrailingPathDelimiter(sTmp);
  FMPK.ExtractToDirectory(de.Name, sTmp);
  FViewProcess := ShellExecute(0, nil, PChar(sTmp + de.Name), nil, PChar(sTmp), SW_SHOW);
  if FViewProcess = SE_ERR_NOASSOC then begin
    DeleteFile(sTmp + de.Name);
    ShowMessage('There is no application associated with this type of file.');
    FViewProcess := 0;
  end;
end;

procedure TfrmMain.treeDirectoryDblClick(Sender: TObject);
begin
  atnView.Execute;
end;

procedure TfrmMain.atnExtractToDirExecute(Sender: TObject);
var
  de:   TMPKDirectoryEntry;
  sTmp: string;
begin
  de := SelectedDirEntry;
  if not Assigned(de) then
    exit;

  SetLength(sTmp, 1024);
  GetTempPath(1024, PChar(sTmp));
  SetLength(sTmp, StrLen(PChar(sTmp)));
  sTmp := IncludeTrailingPathDelimiter(sTmp);
  FMPK.ExtractToDirectory(de.Name, sTmp);
end;

end.

