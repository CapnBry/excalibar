program MPKViewer;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  MPKFile in '..\Common\MPKFile.pas',
  zlib2 in '..\Components\ZLib\zlib2.pas',
  Viewer in 'Viewer.pas' {frmViewer};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Mythic MPK file viewer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
