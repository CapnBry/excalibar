program DAOCSkilla;

uses
  Forms,
  SysUtils,
  Unit1 in 'Unit1.pas' {frmMain},
  DAOCConnection in '..\Common\PacketSniff\DAOCConnection.pas',
  DAOCInventory in '..\Common\DAOCInfo\daocinventory.pas',
  DAOCObjs in '..\Common\DAOCInfo\DAOCObjs.pas',
  DAOCPlayerAttributes in '..\Common\DAOCInfo\DAOCPlayerAttributes.pas',
  DAOCRegion in '..\Common\DAOCInfo\DAOCRegion.pas',
  VendorItems in '..\Common\DAOCInfo\VendorItems.pas',
  MapNavigator in '..\Common\MapNavigator.pas',
  StringParseHlprs in '..\Common\StringParseHlprs.pas',
  ChatParse in '..\Common\ChatParse\ChatParse.pas',
  LinedFileStream in '..\Common\LinedFileStream.pas',
  DAOCSkilla_TLB in 'DAOCSkilla_TLB.pas',
  MPKFile in '..\Common\MPKFile.pas',
  CSVLineParser in '..\Common\CSVLineParser.pas',
  Recipes in '..\Common\DAOCInfo\Recipes.pas',
  DAOCControl in '..\Common\DAOCAutomation\DAOCControl.pas',
  sndkey32 in '..\Common\DAOCAutomation\SNDKEY32.pas',
  DAOCWindows in '..\Common\DAOCInfo\DAOCWindows.pas',
  PowerSkill in '..\Common\DAOCAutomation\PowerSkill.pas',
  PowerSkillSetup in '..\Common\DAOCAutomation\PowerSkillSetup.pas' {frmPowerskill},
  ShowMapNodes in '..\Common\ShowMapNodes.pas' {frmShowMapNodes},
  MacroTradeSkill in '..\Common\DAOCAutomation\MacroTradeSkill.pas' {frmMacroTradeSkills},
  AFKMessage in '..\Common\DAOCAutomation\AFKMessage.pas' {frmAFK},
  AXScript in '..\Common\DAOCAutomation\AXScript.pas',
  ScriptSiteImpl in '..\Common\DAOCAutomation\ScriptSiteImpl.pas',
  SpellcraftHelp in '..\Common\DAOCAutomation\SpellcraftHelp.pas' {frmSpellcraftHelp},
  DDSImage in 'DDSImage.pas',
  VCLMemStrms in '..\Common\VCLMemStrms.pas',
  DAOCClasses in '..\Common\DAOCInfo\DAOCClasses.pas',
  DAOCConSystem in '..\Common\DAOCInfo\DAOCConSystem.pas',
  GlobalTickCounter in '..\Common\GlobalTickCounter.pas',
  QuickSinCos in '..\Common\QuickSinCos.pas',
  Macroing in 'Macroing.pas' {frmMacroing},
  Intersections in '..\Common\Intersections.pas',
  BackgroundHTTP in 'BackgroundHTTP.pas',
  RemoteAdmin in '..\Common\DAOCAutomation\RemoteAdmin.pas' {dmdRemoteAdmin: TDataModule},
  zlib2 in '..\Components\ZLib\zlib2.pas',
  QuickLaunchChars in 'QuickLaunchChars.pas',
  LowOnStat in '..\Common\DAOCAutomation\LowOnStat.pas' {frmLowOnStat},
  geScale in 'geScale.pas',
  GameNetPackets in '..\Common\PacketSniff\GameNetPackets.pas',
  DAOCAccountCharInfo in '..\Common\DAOCInfo\DAOCAccountCharInfo.pas',
  NamedPacketHandler in '..\Common\PacketSniff\NamedPacketHandler.pas',
  FrameFns in '..\Common\PacketSniff\FrameFns.pas',
  DStreamClient in '..\Common\DStream\DStreamClient.pas',
  DStreamDefs in '..\Common\DStream\DStreamDefs.pas',
  DAOCConnectionList in '..\Common\PacketSniff\DAOCConnectionList.pas',
  DAOCControlList in '..\Common\DAOCAutomation\DAOCControlList.pas',
  DStrmServerListFrame in 'DStrmServerListFrame.pas' {frmDStrmServerList: TFrame},
  DebugAndLoggingFns in 'DebugAndLoggingFns.pas',
  SkillaLog in 'SkillaLog.pas' {frmSkillaLog},
  StreamINI in '..\Common\StreamINI.pas',
  DateTimeFormats in '..\Common\DateTimeFormats.pas',
  MobFilterListFrame in 'MobFilterListFrame.pas' {frmMobFilerList: TFrame},
  MobFilterEntry in 'MobFilterEntry.pas' {frmMobFilterEntry},
  DisplayLicense in 'DisplayLicense.pas' {frmDisplayLicense};

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DAOCSkilla for Dark Age of Camelot';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPowerskill, frmPowerskill);
  Application.CreateForm(TfrmShowMapNodes, frmShowMapNodes);
  Application.CreateForm(TfrmMacroTradeSkills, frmMacroTradeSkills);
  Application.CreateForm(TfrmAFK, frmAFK);
  Application.CreateForm(TfrmSpellcraftHelp, frmSpellcraftHelp);
  Application.CreateForm(TfrmMacroing, frmMacroing);
  Application.CreateForm(TdmdRemoteAdmin, dmdRemoteAdmin);
  Application.CreateForm(TfrmLowOnStat, frmLowOnStat);
  Application.CreateForm(TfrmSkillaLog, frmSkillaLog);
  CreateOptionalForms;
  if Screen.PixelsPerInch <> 96 then begin
    frmMain.Log('Font not at normal (96) dpi, attempting to resize for font DPI ' + IntToStr(Screen.PixelsPerInch));
    geAutoScale(frmMain);
    geAutoScale(frmPowerskill);
    geAutoScale(frmShowMapNodes);
    geAutoScale(frmMacroTradeSkills);
    geAutoScale(frmAFK);
    geAutoScale(frmSpellcraftHelp);
    geAutoScale(frmMacroing);
    geAutoScale(frmLowOnStat);
    geAutoScale(frmSkillaLog);
  end;
  Application.Run;
end.
