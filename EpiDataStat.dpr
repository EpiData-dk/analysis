program EpiDataStat;

uses
  Forms,
  Windows,
  SysUtils,
  Dialogs,
  Umain in 'Umain.pas' {aMainForm},
  UVectors in 'UVectors.pas',
  UVectorop in 'UVectorOp.pas',
  UFrames in 'UFrames.pas',
  UAnaToken in 'UAnaToken.pas',
  UanaParser in 'UanaParser.pas',
  UCmdProcessor in 'UCmdProcessor.pas' {DM: TDataModule},
  UVectorVar in 'UVectorVar.pas',
  Uoutput in 'Uoutput.pas',
  UCheckParser in 'UCheckParser.pas',
  UTableDlg in 'UTableDlg.pas' {TableDlg},
  UEpidlg in 'UEpidlg.pas' {EpiDlg},
  UFRGODlg in 'EpiEditor\UFRGODlg.pas' {FRGODlg},
  UGraphDlg in 'UGraphDlg.pas' {UGraphDlg},
  uepifile in 'fileIO\uepifile.PAS',
  uDateUtils in 'utils\uDateUtils.pas',
  UInifile in 'utils\UInifile.pas',
  PasswordUnit in 'utils\PasswordUnit.pas' {PasswordForm},
  Ustack in 'globals\Ustack.pas',
  Uformats in 'globals\Uformats.pas',
  UstringConst in 'globals\UstringConst.pas',
  ansDatatypes in 'globals\ansDatatypes.pas',
  Editor in 'epieditor\Editor.pas' {MDIChild},
  UContinous in 'AnalysisUnits\UContinous.pas',
  Regcomp in 'analysisunits\regress\regcomp.pas' {regression},
  Ustatfunctions in 'AnalysisUnits\Ustatfunctions.pas',
  Udocument in 'AnalysisUnits\Udocument.pas',
  StyleUn in 'Thtml\StyleUn.pas',
  URLSubs in 'Thtml\URLSubs.pas',
  vwPrint in 'Thtml\vwPrint.pas',
  DitherUnit in 'Thtml\DitherUnit.pas',
  HtmlGif1 in 'Thtml\htmlgif1.pas',
  HTMLGif2 in 'Thtml\HTMLGif2.pas',
  HTMLGif in 'Thtml\HTMLGif.pas',
  Htmlsbs1 in 'Thtml\Htmlsbs1.pas',
  Htmlsubs in 'Thtml\HTMLSubs.pas',
  HTMLUn2 in 'Thtml\HTMLUn2.pas',
  Htmlview in 'Thtml\Htmlview.pas',
  MetaFilePrinter in 'Thtml\MetaFilePrinter.pas',
  Readhtml in 'Thtml\Readhtml.pas',
  StylePars in 'Thtml\StylePars.pas',
  ULinearRegression in 'AnalysisUnits\ULinearRegression.pas',
  epdservices in 'utils\epdservices.pas',
  prExpr in 'parser\prExpr.pas',
  UDos in 'AnalysisUnits\UDos.pas',
  AAPasTok in 'parser\AAPasTok.pas',
  UExpToken in 'parser\UExpToken.pas',
  UVariables in 'globals\UVariables.pas',
  SMUtils in 'utils\smUtils.pas',
  UOSUtils in 'utils\UOSUtils.pas',
  UcmdlineEdit in 'UcmdlineEdit.pas',
  UAggregate in 'UAggregate.pas',
  SkStat in 'AnalysisUnits\skunits\skstat.pas',
  SkMatrix in 'AnalysisUnits\skunits\skmatrix.pas',
  SkTypes in 'AnalysisUnits\skunits\sktypes.pas',
  USKtables in 'AnalysisUnits\USKtables.pas',
  UCommands in 'globals\UCommands.pas',
  UEpidataTypes in 'globals\UEpiDataTypes.pas',
  UHelp in 'UHelp.pas' {HelpForm},
  SKxyz in 'AnalysisUnits\skunits\SKxyz.PAS',
  skEXA in 'AnalysisUnits\skunits\skEXA.pas',
  SKrandom in 'AnalysisUnits\skunits\SKrandom.pas',
  Distr in 'AnalysisUnits\skunits\distr.pas',
  UTables in 'AnalysisUnits\UTables.pas',
  UDebug in 'UDebug.pas',
  UTableStat in 'AnalysisUnits\UTableStat.pas',
  UGraph in 'AnalysisUnits\UGraph.pas' {GraphForm},
  pngimage in 'png\pngimage.pas',
  EpiInfoSTATS in 'analysisunits\EpiInfoSTATS.PAS',
  EpiDataFile in 'fileIO\EpiDataFile.pas',
  UCheckProps in 'UCheckProps.pas',
  CheckObjUnit in 'fileIO\CheckObjUnit.pas',
  UCmdTypes in 'globals\UCmdTypes.pas',
  Base64 in 'fileIO\Base64.pas',
  BaseDataset in 'fileIO\BaseDataset.pas',
  BufferedStreams in 'fileIO\BufferedStreams.pas',
  DCPcrypt in 'fileIO\DCPcrypt.pas',
  EpiDataUtils in 'fileIO\EpiDataUtils.pas',
  epiUDFTypes in 'fileIO\epiUDFTypes.pas',
  Rijndael in 'fileIO\Rijndael.pas',
  SHA1 in 'fileIO\SHA1.pas',
  UExtUDF in 'fileIO\UExtUDF.pas',
  AARecFil in 'fileIO\AARecFil.pas',
  GeneralUtils in 'utils\GeneralUtils.pas',
  UTranslation in 'LanguageFiles\UTranslation.pas',
  UMerge in 'AnalysisUnits\UMerge.pas',
  MRUCombo in 'Package\MRUCombo.pas',
  uhtmlutils in 'uhtmlutils.pas',
  Ubrowse2 in 'DataBrowser\Ubrowse2.pas' {FBrowse2},
  UEpiGrid in 'DataBrowser\UEpiGrid.pas',
  UpdateBrowse in 'DataBrowser\UpdateBrowse.pas' {UpdateForm},
  ULifeTables in 'AnalysisUnits\ULifeTables.pas';

{$R *.RES}

var
  MainWindow: HWND;
begin

  MainWindow := FindWindow('TaMainform',nil);

  if (MainWindow <> 0 ) and (FileExists(extractfilepath(application.exename) + 'preventdouble.ea'))  // then do not start
  then
  begin
    MessageDlg('EpiData Analysis already running' +#13 + #13 +
             'To run several instances, delete the file:      ' +#13
             +#13+  extractfilepath(application.exename) + 'preventdouble.ea'
             +#13 +#13 + ' Unfortunately it might be necessary to reboot - if the computer hangs'
             ,mtError,[mbOk], 0);
    exit;
  end
  else
  begin
	  Application.Initialize;
	  Application.Title := 'EpiData Analysis';
	  Application.HelpFile := 'START.HTM';
	  Application.CreateForm(TaMainForm, aMainForm);
  Application.CreateForm(TPasswordForm, PasswordForm);
  Application.Run;
  end;
end.


