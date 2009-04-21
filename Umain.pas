unit Umain;
//Developer: Finn L. N. Boelsmand, 31st of March 2004 - June 2004.
//           JM.Lauritsen Apr 2004 and onwards
//           Torsten Christiansen, Feb. 2004 - ? (general coding)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UVectors, UvectorOp, ansDataTypes,Uframes,  ExtCtrls, ComCtrls ,
  ActnList,UcmdlineEdit, StdActns, Menus, ImgList, Htmlview, ToolWin,SMStatusbar,
  Tbuttonled, shellapi, Math, UDebug, UEpiDatatypes, UGraphDialog,
  Buttons, XPMan, UFRGODlg, HTMLSubs, UTranslation, UMerge;

type
  TaMainForm = class(TForm)
    StatusBar: TSMStatusBar;
    Splitter2: TSplitter;
    ImageList1: TImageList;
    ActionList2: TActionList;
    AcFileOpenREC: TAction;
    AcFileClose: TWindowClose;
    AcFileSave: TAction;
    AcExit: TAction;
    HelpAbout1: TAction;
    AcResetMenu: TAction;
    AcRunMeu: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    AcWindowvars: TAction;
    AcWindowBrowse: TAction;
    AcWindowCommands: TAction;
    ToolButton8: TToolButton;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditCut1: TEditCut;
    EditRedo: TAction;
    Panel: TPanel;
    AcCancel: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    CmdPanel: TPanel;
    Splitter1: TSplitter;
    AcWindowEdit: TAction;
    Viewerpopup: TPopupMenu;
    Copy1: TMenuItem;
    Aclogopen: TAction;
    AcCd: TAction;
    AcPrintViewer: TAction;
    AcPrinterSetup: TAction;
    Acoptions: TAction;
    Acfileproperties: TAction;
    AcViewhtmlFile: TAction;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrintDialog: TPrintDialog;
    AcPrintPreview: TAction;
    EditCopyHTML: TEditCopy;
    EditClearScreen: TAction;
    AcFilePageSetup: TAction;
    AcViewHTMLSource: TAction;
    ViewHTMLsource1: TMenuItem;
    AcSelectAll: TAction;
    Selectall1: TMenuItem;
    ToolButton13: TToolButton;
    PageSetupDialog: TPageSetupDialog;
    indicator: TmButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    AcWebBack: TAction;
    AcWebforward: TAction;
    AcWebReload: TAction;
    N10: TMenuItem;
    Reload1: TMenuItem;
    AcShowStartPage: TAction;
	    AcBrowseFlds: TAction;
  //JL  Browsefields1: TMenuItem;
    AcList: TAction;
    Panel1: TPanel;
    Lvars: TListBox;
    ShowVarLeft: TAction;
    AcShowComandsLeft: TAction;
    Cmdtree: TTreeView;
    wkToolBar: TToolBar;
    ReadDatabtn: TSpeedButton;
    Arrow1: TImage;
    BrowseDataBtn: TSpeedButton;
    GraphBtn: TSpeedButton;
    AcRunDescribe: TAction;
    AcRunFreq: TAction;
    AcRunScatter: TAction;
    ViewerSubMenu: TPopupMenu;
    DESCRIBESUBMENU: TPopupMenu;
    MenuItem1: TMenuItem;
    tables: TMenuItem;
    DescribeData1: TMenuItem;
    Means2: TMenuItem;
    EditorBtn: TSpeedButton;
    AcRunHistogram: TAction;
    AcRunTable: TAction;
    AcRunLine: TAction;
    AcRunMeans: TAction;
    Togglemenu1: TMenuItem;
    XPManifest1: TXPManifest;
    DescribeBtn: TSpeedButton;
    Image1: TImage;
    GraphSubMenu: TPopupMenu;
    AcRunScatter1: TMenuItem;
    AcRunLine1: TMenuItem;
    AcRunHistogram1: TMenuItem;
    BoxPlot2: TMenuItem;
    Bar2: TMenuItem;
    IChart4: TMenuItem;
    PChart4: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    BoxPlot4: TMenuItem;
    AcRunBar: TAction;
    AcRunBox: TAction;
    AcRunUpDate: TAction;
    AcRunIChart: TAction;
    AcRunPChart: TAction;
    AcRunRunChart: TAction;
    RunChart4: TMenuItem;
    AcRunPie: TAction;
    Regress: TMenuItem;
    AcRunRegress: TAction;
    AcHelpCmdAndFct: TAction;
    AcHelpHowto: TAction;
    AcShowOutput: TAction;
    ExitBtn: TSpeedButton;
    Image4: TImage;
    Image6: TImage;
    Image7: TImage;
    AcBrowse1: TAction;
    EditFind: TAction;
    EditRepeatFind: TAction;
    Kwallis1: TMenuItem;
    Correlate1: TMenuItem;
    AcRunKwallis: TAction;
    AcRunCorrelate: TAction;
    AcCommandPrompt: TAction;
    Historylist: TListBox;
    Acshowhist: TAction;
    ClearCommandHistory1: TMenuItem;
    AcGetVersion: TAction;
    HistoryPopup: TPopupMenu;
    HistoryActionList: TActionList;
    HistAcSave: TAction;
    HistAcEdit: TAction;
    HistAcClear: TAction;
    Save1: TMenuItem;
    Clear1: TMenuItem;
    Clear2: TMenuItem;
    AcEditSetup: TAction;
    AcSearchEpiDataList: TAction;
    AcHelpWindow: TAction;
    AcFontSelect: TAction;
    HistAcClipboard: TAction;
    Clipboard1: TMenuItem;
    N34: TMenuItem;
    N35: TMenuItem;
    N36: TMenuItem;
    AcTableDlg: TAction;
    ViewerImagePopup: TPopupMenu;
    CopyToClipboard: TMenuItem;
    N37: TMenuItem;
    CopyasHTML3: TMenuItem;
    AcCopyGraph: TAction;
    AcCopyTableToClipboard: TAction;
    N29: TMenuItem;
    CumulativePlot1: TMenuItem;
    Dotplot1: TMenuItem;
    EpiCurve1: TMenuItem;
    Pareto1: TMenuItem;
    OrgMainMenu: TMainMenu;
    MmenuFile: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    CD1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    MmenuEdit: TMenuItem;
    Redo1: TMenuItem;
    CopyasHTML2: TMenuItem;
    Selectall2: TMenuItem;
    N28: TMenuItem;
    F1: TMenuItem;
    FindAgain1: TMenuItem;
    MmenuHelp: TMenuItem;
    CmdAndFct: TMenuItem;
    N33: TMenuItem;
    PDFintroduction: TMenuItem;
    SearchEpiDataList1: TMenuItem;
    N30: TMenuItem;
    CheckVersion1: TMenuItem;
    HelpAboutItem: TMenuItem;
    AcShowresultvariables: TAction;
    N40: TMenuItem;
    MmenuData: TMenuItem;
    Browsefields1: TMenuItem;
    N8: TMenuItem;
    ResultVariables1: TMenuItem;
    Variables1: TMenuItem;
    N13: TMenuItem;
    Update1: TMenuItem;
    N14: TMenuItem;
    N2: TMenuItem;
    ScreenFont1: TMenuItem;
    AcFileStructure: TAction;
    AcDefaultWindowing: TAction;
    MmenuViewer: TMenuItem;
    Clearscreen2: TMenuItem;
    Closedatafile1: TMenuItem;
    N16: TMenuItem;
    N18: TMenuItem;
    ReloadShownFile1: TMenuItem;
    NextOutputWindow1: TMenuItem;
    PreviousOutputWindow1: TMenuItem;
    StopSavingOutput1: TMenuItem;
    StartSavingOutput1: TMenuItem;
    N19: TMenuItem;
    Properties1: TMenuItem;
    N3: TMenuItem;
    Closedatafile2: TMenuItem;
    Aclogclose: TAction;
    N4: TMenuItem;
    N11: TMenuItem;
    List1: TMenuItem;
    AcClearResultVariables: TAction;
    Clearresult: TMenuItem;
    cancelbtn: TToolButton;
    CloseBtn: TSpeedButton;
    AcEditor: TAction;
    ToolButton12: TToolButton;
    StartSavingOutput2: TMenuItem;
    logclose1: TMenuItem;
    N43: TMenuItem;
    Showstartpage3: TMenuItem;
    Viewlogfile1: TMenuItem;
    acshowcommands: TAction;
    Acshowvariables: TAction;
    Accmdpromptsetfocus: TAction;
    AcRunEditor: TAction;
    Printpreview1: TMenuItem;
    Print1: TMenuItem;
    N5: TMenuItem;
    Pagesetup2: TMenuItem;
    ToolButton9: TToolButton;
    AcViewhelp: TAction;
    achelpkeyboard: TAction;
    Windows2: TMenuItem;
    DefaultWindowPosition1: TMenuItem;
    SaveWindowPosition1: TMenuItem;
    N12: TMenuItem;
    N15: TMenuItem;
    ProcessToolbaronoff1: TMenuItem;
    browserhide: TMenuItem;
    N25: TMenuItem;
    Options1: TMenuItem;
    N6: TMenuItem;
    N27: TMenuItem;
    EditSetupEpiDatastatini2: TMenuItem;
    History1: TMenuItem;
    Acshowvariables1: TMenuItem;
    Saveoutput1: TMenuItem;
    N26: TMenuItem;
    Showcommands1: TMenuItem;
    Accmdpromptsetfocus1: TMenuItem;
    EditPgm1: TMenuItem;
    Browse1: TMenuItem;
    EditSetupEpiDatastatini1: TMenuItem;
    quickguide1: TMenuItem;
    Acbrowserhide: TAction;
    ToolButton18: TToolButton;
    acwktoolbaract: TAction;
    ViewerBtn: TToolButton;
    Clearscreen1: TMenuItem;
    ScreenFont3: TMenuItem;
    N9: TMenuItem;
    logclose2: TMenuItem;
    StartSavingOutput3: TMenuItem;
    N17: TMenuItem;
    Showstartpage1: TMenuItem;
    Viewlogfile2: TMenuItem;
    N20: TMenuItem;
    ReloadShownFile2: TMenuItem;
    NextOutputWindow2: TMenuItem;
    PreviousOutputWindow2: TMenuItem;
    StartSavingOutput4: TMenuItem;
    NextOutputWindow3: TMenuItem;
    PreviousOutputWindow3: TMenuItem;
    N7: TMenuItem;
    Ciplot1: TMenuItem;
    Help1: TMenuItem;
    ScreenFont2: TMenuItem;
    N31: TMenuItem;
    ImpClipBrd: TMenuItem;
    AcImportFromClipboard: TAction;
    AcRunKMPlot: TAction;
    AcRunLifeTable: TAction;
    KMPlot1: TMenuItem;
    LifeTable1: TMenuItem;
    CumulativePlot2: TMenuItem;
    N38: TMenuItem;
    SPCCharts1: TMenuItem;
    SPCMenu1: TMenuItem;
    N32: TMenuItem;
    AcRunCChart: TAction;
    AcRunUChart: TAction;
    AcRunGChart: TAction;
    UChart1: TMenuItem;
    CChart1: TMenuItem;
    GChart1: TMenuItem;
    AcShowSPCMenu: TAction;
    AcRunPareto: TAction;
    AcRunProbitPlot: TAction;
    AcRunCumulativePlot: TAction;
    AcRunDotPlot: TAction;
    AcRunCIPlot: TAction;
    AcRunEpicurve: TAction;
    IChart1: TMenuItem;
    AcRunXbarChart: TAction;
    N41: TMenuItem;
    N42: TMenuItem;
    N44: TMenuItem;
    N39: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure CmdEditCommand(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure WindowClose1Execute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LVarsDblClick(Sender: TObject);
//    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure A(Sender: TObject);
    procedure LVarsKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure CmdTreeKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure AcBrowseExecute(Sender: TObject);
    procedure AcExitExecute(Sender: TObject);
    procedure AcFileOpenRECExecute(Sender: TObject);
    //procedure FileSaveAs1Execute(Sender: TObject);
    procedure AcCancelExecute(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditCopy1Update(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
    procedure CmdTreeDblClick(Sender: TObject);
    procedure AcShowWindowCommandsLeft(Sender: TObject);
    procedure Regress1Click(Sender: TObject);
    procedure AcWindowEditExecute(Sender: TObject);
    procedure AcCdExecute(Sender: TObject);
    procedure StatusBarDblClick(Sender: TObject);
    procedure AcPrinterSetupExecute(Sender: TObject);
    procedure AcPrintViewerExecute(Sender: TObject);
    procedure AcPrintPreviewExecute(Sender: TObject);
    procedure AclogopenExecute(Sender: TObject);
    procedure AclogcloseExecute(Sender: TObject);
    procedure AcViewhtmlFileExecute(Sender: TObject);
    procedure EditCopyHTMLExecute(Sender: TObject);
    procedure EditClearScreenExecute(Sender: TObject);
    procedure AcEditorExecute(Sender: TObject);
    procedure AcfilepropertiesExecute(Sender: TObject);
    procedure AcViewHTMLSourceExecute(Sender: TObject);
    procedure AcSelectAllExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AcFilePageSetupExecute(Sender: TObject);
    procedure HotSpotClick(Sender: TObject; const URL: string;
      var Handled: boolean);
    procedure ImageClick(Sender, Obj: TObject; Button: TMouseButton;
                         Shift: TShiftState; X, Y: Integer);
    procedure ViewerRightClick(Sender: TObject; Parameters: TRightClickParameters);
    procedure FwdBackClick(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure AcShowStartPageExecute(Sender: TObject);
    procedure AcEditSetupFile(Sender: TObject);
    procedure AcBrowseFldsExecute(Sender: TObject);
    procedure AcDoDescribeExecute(Sender: TObject);
    procedure AcListExecute(Sender: TObject);
//JL    procedure fileImportExecute(Sender: TObject);
    procedure Toolbar1Click(Sender: TObject);
    procedure Wktoolbar1Click(Sender: TObject);
//JL add begin
    procedure CreateNewVariables1Click(Sender: TObject);
    procedure Recode1Click(Sender: TObject);
    procedure Scatter1Click(Sender: TObject);
    procedure Line1Click(Sender: TObject);
    procedure Histogram1Click(Sender: TObject);
    procedure Bar1Click(Sender: TObject);
    procedure IChart1Click(Sender: TObject);
    procedure PChart1Click(Sender: TObject);
    procedure Means1Click(Sender: TObject);
    procedure count1Click(Sender: TObject);
    procedure KruskalWallisTest1Click(Sender: TObject);
    procedure AcShowVariablesLeft(Sender: TObject);
    procedure AcShowHistory(Sender: TObject);
    procedure CommandPrompt1Click(Sender: TObject);
    procedure AcRunRegressExecute(Sender: TObject);
    procedure AcRunDescribeExecute(Sender: TObject);
    procedure AcRunFreqExecute(Sender: TObject);
    procedure AcRunScatterExecute(Sender: TObject);
    procedure WKTOOOLBARClick(Sender: TObject);
    procedure LvarsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LvarsEnter(Sender: TObject);
    procedure AcRunHistogramExecute(Sender: TObject);
    procedure AcRunTableExecute(Sender: TObject);
    procedure AcRunLineExecute(Sender: TObject);
    procedure AcRunMeansExecute(Sender: TObject);
    procedure Togglemenu1Click(Sender: TObject);
    procedure AcRunIChartExecute(Sender: TObject);
    procedure AcRunPChartExecute(Sender: TObject);
    procedure AcRunRunChartExecute(Sender: TObject);
    procedure AcRunBarExecute(Sender: TObject);
    procedure AcRunBoxExecute(Sender: TObject);
    procedure AcRunPieExecute(Sender: TObject);
    procedure AcFileSaveExecute(Sender: TObject);
    procedure LvarsExit(Sender: TObject);
    procedure SaveWindowsPosition1Click(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure AcShowOutputExecute(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure AcBrowse1Execute(Sender: TObject);
    procedure LvarsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ToolButton11MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ToolBar1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditFindExecute(Sender: TObject);
    procedure SpecialSearch(FindStr: string; SearchOpt: TFrSearchOptions);
    procedure EditRepeatFindExecute(Sender: TObject);
    procedure AcRunKwallisExecute(Sender: TObject);
    procedure AcRunCorrelateExecute(Sender: TObject);
    procedure AcCommandPromptExecute(Sender: TObject);
    procedure HistorylistKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ClearCommandHistory1Click(Sender: TObject);
    procedure AcGetVersionExecute(Sender: TObject);
    procedure HistorylistDblClick(Sender: TObject);
    procedure HistAcSaveExecute(Sender: TObject);
    procedure HistAcEditExecute(Sender: TObject);
    procedure HistAcClearExecute(Sender: TObject);
    procedure AcSearchEpiDataListExecute(Sender: TObject);
    procedure PDFintroductionClick(Sender: TObject);
    procedure AcHelpWindowExecute(Sender: TObject);
    procedure AcHelpWindowKeyboard(Sender: TObject);
    procedure AcFontSelectExecute(Sender: TObject);
    procedure HistAcClipboardExecute(Sender: TObject);
    procedure wkToolBarClick(Sender: TObject);
    procedure AcTableDlgExecute(Sender: TObject);
    procedure CopyImageToClipboardClick(Sender: TObject);
    procedure CopyTableToClipboardClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AcShowresultvariablesExecute(Sender: TObject);
    procedure AcOptionExecute(Sender: TObject);
    procedure AcFileStructureExecute(Sender: TObject);
    procedure AcDefaultWindowingExecute(Sender: TObject);
    procedure AcClearResultVariablesExecute(Sender: TObject);
    procedure AcFileCloseExecute(Sender: TObject);
    procedure AcbrowserhideExecute(Sender: TObject);
    procedure acwktoolbaractExecute(Sender: TObject);
    procedure HistorylistMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HistorylistExit(Sender: TObject);
    procedure AcImportFromClipboardExecute(Sender: TObject);
    procedure AcRunKMPlotExecute(Sender: TObject);
    procedure AcRunLifeTableExecute(Sender: TObject);
    procedure AcRunCChartExecute(Sender: TObject);
    procedure AcRunUChartExecute(Sender: TObject);
    procedure AcRunGChartExecute(Sender: TObject);
    procedure AcShowSPCMenuExecute(Sender: TObject);
    procedure AcRunParetoExecute(Sender: TObject);
    procedure AcRunProbitPlotExecute(Sender: TObject);
    procedure AcRunCumulativePlotExecute(Sender: TObject);
    procedure AcRunDotPlotExecute(Sender: TObject);
    procedure AcRunCIPlotExecute(Sender: TObject);
    procedure AcRunEpicurveExecute(Sender: TObject);
    procedure AcRunXbarChartExecute(Sender: TObject);
    //JL add end
  private
    dbfdata: TEpiDBFdataset;
    FoundObject: TObject;
    Viewer: THTMLViewer;
    SelectedHTML : string;
    CmdBuffer: string;
    FCurrentErrorLine: integer;
    oldPrecisionMode: TFPUPrecisionMode;
//    FTranslator: TTranslator;
    procedure ShowVarByRightClick(x, y: Integer);
    procedure AppException(Sender: TObject; E: Exception);
    procedure SetupOutput;
    procedure SetupBasicFiles();
    procedure InitializeUnits();
    procedure DeInitializeUnits();
    procedure HAlignPanel1();
    procedure EngineHook(MSg:TMessage);
    procedure RefreshOuput(MSg: TMessage);
    procedure UpdateVisual(Msg: TMessage);
    procedure ReloadLanguage(Msg: TMessage);
    procedure InternalRunCommand(cmd: string; AddToHist: boolean = true);
    procedure FindFocus(PrefControl: TWinControl);
    procedure Output(const s: string);
    procedure OutputError(const s: string);
    procedure InstallShortCut(psc: pchar);
    procedure ExecuteAction(sender: TObject);
    procedure RefreshVarList(MSg: TMessage);

    procedure Closefile(MSg: TMessage);
    procedure UpdateCommandTree;
//    procedure ProcessCommandLine;
    procedure PrintFooter(Sender: TObject; Canvas: TCanvas; NumPage, W,
      H: Integer; var StopPrinting: Boolean);
    procedure PrintHeader(Sender: TObject; Canvas: TCanvas; NumPage, W,
      H: Integer; var StopPrinting: Boolean);
 //   procedure ViewerProgress(Sender: TObject; Stage: TProgressStage;PercentDone: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowErrorLine(ErrLine: integer);
    procedure SetCurrentErrorLine(const Value: integer);
    function GetCurrentErrorLine: integer;
    function ViewFile(const fn: string): boolean;
    procedure DoDlg(const cmdstring: string);
    procedure DoGraphDlg(const cmdstring:string;Xvars, Yvars: Integer;Xlegal, Ylegal: Array of integer;
           AdvType: TEpiAdvOptions = []; By:Boolean=False; Weigth: boolean=false;
           t1:string='X Variable';
           t2:string='Y Variable 1';
           t3:string='Y Variable 2';
           t4:string='Y Variable 3';
           tb:string='Y Variable 4'); overload;
    procedure DoGraphDlg(const DialogOptions: TGraphDlgOptions); overload;
    procedure DoTableDlg(const cmdstring: string);
    procedure LVarsInsert(delete: boolean = true);
    procedure HistoryInsert(delete: boolean = true);
    procedure HandleShutdown();
  public
    CmdEdit:TCommandLineEditor;
    booUMainEditorFocused: Boolean;
    forcequit: boolean;
    MaxVarLength: Integer;
    procedure doCommand(const cmd: string);
    property  CurrentErrorLine: integer read GetCurrentErrorLine write SetCurrentErrorLine;
    procedure AddCommand(const s:string; const eg:string);       // TODO when do we use this ??

//    procedure s(mess: string); //FB: help procedure.
    procedure UpdateWindowFont(fontsize: EpiInt);
    procedure PrintViewer();
//    property  Translator: TTranslator read FTranslator write FTranslator;
//    procedure SaveForm(Form: TCustomForm; Section: string);
//    procedure LoadForm(Form: TCustomForm; Section: string);
  end;

var
  aMainForm: TaMainForm;

implementation

{$R *.DFM}
uses SMUtils,UcmdProcessor, uabout, {UToolWin, UVarWin,UCmdLine,} ShellBrowser,
PreviewForm, uhtmlutils,Clipbrd, Editor, UEpiDlg, UtableDlg, UGraphDlg, cFileUtils, UCommands,
UanaToken, OPConvert, UHelp, UGraph,
// Units to initialize
UIniFile, UDocument, ULinearRegression, UTables, UDos, UAggregate, UpdateBrowse, ULifeTables;

var
  SearchOptions: TFrSearchOptions;
  FindString: String;

const
  UnitName = 'UMain';


procedure TaMainForm.FormCreate(Sender: TObject);
const
  def: TFormDefaults = (Section: 'Main';
                        Top: 1; Left: 1;
                        Width: 800; Height: 600;
                        Maximize: false);
var
  msg: TMessage;
begin
  oldPrecisionMode := SetPrecisionMode(pmExtended);
  forcequit := false;
  CmdEdit:=TCommandLineEditor.create(self);
  CmdEdit.Parent:=CmdPanel;
  CmdEdit.Width:=553;
  CmdEdit.Height:=34;
  CmdEdit.Align:=alClient;
  CmdEdit.Text:=' ';

  CmdEdit.SelStart:=Length(CmdEdit.Text);
  CmdEdit.SelLength:=0;
  CmdEdit.TabOrder:=0;
  CmdEdit.TabStop:=True;
  CmdEdit.OnCommand:=CmdEditCommand;
  Application.OnException:=AppException;
  InitializeUnits;
  SetupBasicFiles;
  SetupOutput;
  UpdateCommandTree;
  closefile(msg);
  caption:=GetBuildInfoAsString;
  booUMainEditorFocused:=True; //FB 21st of April 2004: flag to show whether UMain or Editor is focused.
  Statusbar.Panels[0].Text:=GetCurrentdir;
  OInifile.Initialize(extractfilepath(application.exename)+'epidatastatsetup.ini');
  OInifile.LoadForm(self, def);
  Panel1.Width := OInifile.LoadSplitter();
  SearchOptions := [];
  OTranslator.TranslateForm(self);
end;

function GetSysErrorString(E: TObject):string;
begin
if E is EAccessViolation then
   Result:= 'Internal error, report to info@Epidata.dk ('+ Exception(E).Message+ ')'
else if E is   EWin32Error then
    Result:= GetLastErrorMsg
else
   Result:=  Exception(E).Message;
{  EOutOfMemory = class(EHeapException);
  EInOutError = class(Exception)
  EExternal = class(Exception)
  EExternalException = class(EExternal);
  EIntError = class(EExternal);
  EDivByZero = class(EIntError);
  ERangeError = class(EIntError);
  EIntOverflow = class(EIntError);
  EMathError = class(EExternal);
  EInvalidOp = class(EMathError);
  EZeroDivide = class(EMathError);
  EOverflow = class(EMathError);
  EUnderflow = class(EMathError);
  EInvalidPointer = class(EHeapException);
  EInvalidCast = class(Exception);
  EConvertError = class(Exception);
  EPrivilege = class(EExternal);
  EStackOverflow = class(EExternal);
  EControlC = class(EExternal);
  EVariantError = class(Exception);
  EPropReadOnly = class(Exception);
  EPropWriteOnly = class(Exception);
  EAssertionFailed = class(Exception);
  EAbstractError = class(Exception);
  EIntfCastError = class(Exception);
  EInvalidContainer = class(Exception);
  EInvalidInsert = class(Exception);
  EPackageError = class(Exception);
  public
    ErrorCode: DWORD;
  end;
}
end;

procedure TaMainForm.AppException(Sender: TObject; E: Exception);
var
  opt: TEpiOption;
begin
  if dm.Executor = nil then
  begin
    Application.ShowException(E);
    abort;
  end
  else
  begin
   if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
   if ExceptObject is Exception then
   begin
    if not (ExceptObject is EAbort) then
    begin
      dm.GetOptionValue('DEBUG FILENAME', opt);
      ODebug.SaveToDisk(opt.Value);
      OutputError(GetSysErrorString(ExceptObject));
      Messagebeep(0);
    end;
   end else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
  end;
end;

procedure TaMainForm.CmdEditCommand(Sender: TObject);
var
  cmd: string;
  t: DWord;
begin
  // TODO -OTorsten : When new parser working, delete cmd[1] = '*'....
  t := GetTickCount;
  cmd := Trim(CmdEdit.Text);
  if (cmd<>'') then
  begin
    if (cmd[1] = '*') then
      dm.Info(cmd)
    else begin
      CmdBuffer := CmdBuffer + cmd;
      try
        InternalRunCommand(CmdBuffer, false);
      finally
        CmdBuffer := '';
      end;
    end;
  end;
  t := GetTickCount - t;
  dm.Sysinfo('Command completed in: '+ floattostr(t/1000)+' seconds');
end;

procedure TaMainForm.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept:= NewSize >0;
end;


{
procedure TaMainForm.CmdEditKeyPress(Sender: TObject; var Key: Char);
begin
if key=#13 then key:=#0;
end;
}

procedure TaMainForm.WindowClose1Execute(Sender: TObject);
begin
  close;
end;

procedure TaMainForm.FormShow(Sender: TObject);
var
  opt : TEpiOption;
  NoStartView: boolean;
  fn, ext: string;
  // cmd: TCommand;
begin
  dm := StartEngine;
  Dm.InterfaceHook:=EngineHook;
  Splitter2.Visible := false;
  aMainform.Panel1.Visible  := false;
  dm.initShortCuts;
  NoStartView := dm.runInitFile;
  ODos.cd('');
  if (DM.GetOptionValue('DISPLAY COMMAND HISTORY', Opt) and (Opt.Value = 'ON')) then
      AcShowHistory(Self);
  if (DM.GetOptionValue('DISPLAY COMMANDTREE', Opt) and (Opt.Value = 'ON')) then
      AcShowWindowCommandsLeft(Self);
  if (DM.GetOptionValue('DISPLAY WORKTOOLBAR', Opt) and (Opt.Value = 'OFF')) then
      begin
        wktoolbar.Visible := False;
        ProcessToolbaronoff1.ImageIndex := -1;
      end
      else begin
        AmainForm.Viewerbtn.Hint := OTranslator.Translate(2001,'Show Viewer Menu');
        AmainForm.Cancelbtn.Hint := OTranslator.Translate(2002,'Stop Current Action');
        ProcessToolbaronoff1.ImageIndex := 109;
        end;
  if (DM.GetOptionValue('DISPLAY MAINMENU', Opt) and (Opt.Value = 'OFF')) then
      self.Menu:=NIL;
  if (DM.GetOptionValue('DISPLAY COMMAND PROMPT', opt) and (opt.Value = 'OFF')) then
  begin
    self.CmdPanel.Height := 0;
    Splitter1.Visible := false;
    ViewHTMLsource1.Visible := false;
    Togglemenu1.Visible := false;
  end;
  if trim(ParamStr(1)) <> '' then
  begin
    ODos.CD(ExtractFilePath(ParamStr(1)));
    if ExtractFileExt(ParamStr(1)) = 'REC'
      then dm.info('Cannot yet open rec files directly', 26002) // TODO : OpenFile.OpenFile(ParamStr(1),'READ');
      else dm.RunPGMFile(ParamStr(1));
  end
  else
  begin
    if not NoStartView then
      if (DM.GetOptionValue('START PAGE', opt) and (fileexists(opt.Value))) then
        InternalRunCommand('view "' + opt.value + '"')
      else
        InternalRunCommand('view "' + extractfilepath(application.exename)+ 'docs\' +
               OTranslator.Translate(105,'en') + '\start.htm"');

    if (CmdPanel.visible) then
    begin
      if (not NoStartView) then
      begin
        CmdEdit.History.add('Read ');
        CmdEdit.CurrentCmd := CmdEdit.History.Count;
      end;
      CmdEdit.SelStart:=Length(CmdEdit.Text);
      CmdEdit.SelLength:=0;
    end;
    FindFocus(cmdedit);
  end;
  dm.SetOptionValue('OUTPUT FOLDER', StatusBar.Panels[0].Text, true);
  if dm.GetOptionValue('OUTPUT OPEN', opt) and (opt.Value = 'ON') then
  begin
    aMainForm.Aclogopen.Enabled := false;
    aMainForm.Aclogclose.Enabled := true;
    fn := 'EAoutput';
    ext := 'htm';

    if dm.GetOptionValue('OUTPUT NAME', opt) then
    begin
      fn := ExtractFileNameNoExt(opt.Value);
      ext := ExtractFileExt(opt.Value);
    end;
    fn := StatusBar.Panels[0].Text + PathDelim + fn;
    BackupFile(fn, ext, 3);
    dm.LogOpen(fn + ext, nil);
  end;
  // show toogle menu icons:
  if ((DM.GetOptionValue('DISPLAY WORKTOOLBAR', Opt)) and (Opt.Value = 'OFF')) then
        ProcessToolbaronoff1.ImageIndex := -1
        else ProcessToolbaronoff1.ImageIndex := 109;

 if ((dm.GetOptionValue('DISPLAY DATABROWSER', opt)) and (opt.VALUE = 'OFF')) then
        browserhide.ImageIndex := -1
        else browserhide.ImageIndex := 109;

  if dm.GetOptionValue('DEBUG FILENAME', opt) then
  begin
    fn := ExtractFileNameNoExt(opt.Value);
    ext := ExtractFileExt(opt.Value);
  end;
  fn := StatusBar.Panels[0].Text + PathDelim + fn;
  BackupFile(fn, ext, 3);

  //enable and disable various parts of the mainform:
  dm.enable(false);
end;

procedure TaMainForm.Output(const s :string);
begin
  dm.Writeln(s);
end;

procedure TaMainForm.OutputError(const s :string);
begin
  dm.Writeln2(s, 'error');
end;

Procedure TaMainform.SetupOutput;
//var
  //opt: TEpiOption;
begin
    Viewer:= THTMLViewer.Create(self);
    Viewer.parent := Panel;
    Viewer.Align := alClient;
    Viewer.DefBackground := clBlack;
    Viewer.DefFontSize:= 11;
    Viewer.DefFontName := 'Fixedsys';
//    Viewer.DefFontColor := clYellow;
//    Viewer.OnMouseDown := OnMouseDown;
//    TControl(Viewer).OnClick := nil;


    //Viewer.PopupMenu := Viewerpopup;
    Viewer.OnRightClick := ViewerRightClick;


    Viewer.vscrollbar.visible := True;
{    HistoryMaxCount = 0}
    Viewer.NoSelect := false;
//    Viewer.OnProgress := ViewerProgress; // No progressbar desired
    Viewer.OnMouseUp := ViewerMouseUp;
    Viewer.OnMouseMove := ToolBar1MouseMove;
    Viewer.OnPrintHeader := PrintHeader;
    Viewer.OnPrintFooter := PrintFooter;
    Viewer.HistoryMaxCount := 25;
    Viewer.OnHistoryChange :=HistoryChange;
    Viewer.OnHotSpotClick := HotSpotClick;
    Viewer.OnImageClick := ImageClick;
end;

procedure TaMainForm.SetupBasicFiles();
{$I BasicFiles.inc}
var
  sysdir, startfont: string;
  FHandle: HWND;
  Output: TStringList;

  function WriteToFile(path, content: string):Boolean;
  begin
     if FileExists(path) then exit;
     Output.Text := Content;
     Output.SaveToFile(path);
  end;

  function WriteToINIFile(path, size: string):Boolean;
  begin
     if FileExists(path) then exit;
       Output.Text := EpiDataStat + #13
        + '* Viewer font and size: (plus editor and help windows)' + #13
        + 'set browser font size =' + size + #13
        + 'set graph font size =' + size + #13
        + 'set window font size =' + size + #13
        + 'set window font size =' + size + #13
        + 'set editor font size =' + size + #13
        + 'set language=' +  OTranslator.Translate(100,'en') + #13;
     // if Pos('EPIDATASTAT.INI', AnsiUpperCase(path)) > 0 then
      Output.Text := Output.Text + #13 + 'Set echo=on' + #13;
      Output.SaveToFile(path);
  end;

  // help routines for small repeating texts
  function StartView(): string;
  begin
       result := 'view "@sysdir/docs/' + OTranslator.Translate(105,'en') + '/start.htm"';
  end;

begin
  // create the basic files and folders:
  Output := TStringList.Create;
  sysdir := ExtractFilePath(Application.ExeName);
  WriteToIniFile(sysdir + 'epidatastat.ini','10');
  WriteToIniFile(sysdir + 'epidatastat.10','10');
  WriteToIniFile(sysdir + 'epidatastat.12','12');
  WriteToIniFile(sysdir + 'epidatastat.15','15');
  WriteToIniFile(sysdir + 'epidatastat.20','20');

  // stylesheets:
  WriteToFile(sysdir + 'epiout_w.css', EpiOutCSSblack);
  WriteToFile(sysdir + 'epiout_b.css', EpiOutCSSwhite);
  WriteToFile(sysdir + 'epiout.css', EpiOutCSSblack);
  try
    begin     // attemp create - exception avoided if existing.
      sysdir := ExtractFilePath(Application.ExeName)   + 'docs' + PathDelim ;
      if not DirectoryExists(sysdir) then MkDir(sysdir);
      sysdir := ExtractFilePath(Application.ExeName)   + 'docs'
                + PathDelim + OTranslator.Translate(105,'en') + PathDelim;
      if not DirectoryExists(sysdir) then MkDir(sysdir);
    end
  except
    sysdir := ExtractFilePath(Application.ExeName);
  end;

    // standard font and start files in docs/{language}/
  WriteToFile(sysdir + 'epiout.css',EpiOutCSS);

   // default start file:
  WriteToFile(sysdir +  'start0.htm',
    StartHTM + '<p><ul>'
    + '<li>' + OTranslator.Translate(125,'Introduction to EpiData Analysis, see Help menu') + #13
    + '<li>' + OTranslator.Translate(126,'Check regularly for updated versions and documentation')
    + ' : <a href="' + OTranslator.Translate(120,'Http://www.epidata.dk')+'">' + OTranslator.Translate(120,'Http://www.epidata.dk') + '</a>' + #13
    + '</ul></p></body><html>');

   // setup start file:
  startfont := startfonthtml
    + '<h3>'+ OTranslator.Translate(130,'Basic setup of the EpiData Analysis programme:') +'</h3>' + #13
    + '<ol>' + #13 + '<li>'
    + OTranslator.Translate(131,'Change Position and size of EpiData Analysis') + #13
    + '<ul><li> <A HREF=''epi:set echo=off;cls;saveiniscreen;set echo=on;' + StartView()
    + '''>' + OTranslator.Translate(132,'Save Screen Position') +'</a></ul><br>' + #13
    + '<li>' + OTranslator.Translate(133,'Select background and text colour') + #13
    + '<ul><li><A HREF=''epi:cls;set echo=off;define fn __________________________ glob;fn= "@sysdir"; '
    + 'cd "@fn";copyfile "epiout_w.css" "epiout.css" /replace;'
    + 'copyfile "epiout.css" "docs/'+ OTranslator.Translate(105,'en') + '/epiout.css" /replace;' + StartView() + '''>'
    + OTranslator.Translate(134,'White background') + '</a></li>' + #13
    + '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<A HREF=''epi:cls;set echo=off;define fn __________________________ glob;fn= "@sysdir"; '
    + 'cd "@fn";copyfile "epiout_b.css" "epiout.css"  /replace;'
    + 'copyfile "epiout.css" "docs/'+ OTranslator.Translate(105,'en') + '/epiout.css" /replace;' + StartView() + '''>'
    + OTranslator.Translate(135,'Black background')+ '</a></li></ul><br>' + #13
    + '<li>' + OTranslator.Translate(136,'Choose text font size') + '<ul>' + #13
    + '<li>&nbsp;&nbsp;' + font10 + StartView() +  '''><font style="Font-size=14px ; font-weight : bold; color: blue">10</font></a>&nbsp;&nbsp;&nbsp;&nbsp;'
    +  font12 + StartView() +  '''><font style="Font-size=17px ; font-weight : bold;color: blue">12</font></a>&nbsp;&nbsp;&nbsp;&nbsp;'
    +  font15 + StartView() +  '''><font style="Font-size=21px ; font-weight : bold;color: blue">15</font></a>&nbsp;&nbsp;&nbsp;&nbsp;'
    +  font20 + StartView() +  '''><font style="Font-size=30px ; font-weight : bold;color: blue">20</font></a>' + #13
    + '</ul><br>' + #13
    + '<li> ' + final + StartView() + '''>' + Otranslator.translate(137,'Save changes and Start') + '</a>' + #13
    + '</ol>' + #13 + '<hr>' + #13 + '</body></html>' + #13 ;

  // modify to accomplish translation:
  WriteToFile(sysdir + 'epidatahelp.css',EpiOutCSS);
  WriteToFile(sysdir + 'startfont.htm',Startfont);
  WriteToFile(sysdir + 'start.htm',StartFont);
{  WriteToFile(sysdir + 'docs' + PathSeperator +
              OTranslator.Translate(105,'en') + PathSeperator + 'SPC.htm', SpcHTML);}

   // redo if commands.htm was not found, then we should make simplified startfiles:
  if not FileExists(sysdir + 'commands.htm') then
  begin
    WriteToFile(sysdir + 'commands.htm',
        StartHTM + '<p><ul>'
        + '<li><b>' + OTranslator.Translate(127,'Documentation files missing - see ')
        + '<a href="' + OTranslator.Translate(120,'Http://www.epidata.dk')+'">' + OTranslator.Translate(120,'Http://www.epidata.dk') + '</a>' + #13
        + '</b><br></ul>' + #13 + '</p></body><html>');

    WriteToFile(sysdir + 'start.htm',
        StartHTM + '<p><ul>'
        + '<li><b>' + OTranslator.Translate(127,'Documentation files missing - see ')
        + '<a href="' + OTranslator.Translate(120,'Http://www.epidata.dk')+'">' + OTranslator.Translate(120,'Http://www.epidata.dk') + '</a>' + #13
        + '</b><br></ul>' + #13 + '</p></body><html>');
    {WriteToFile(sysdir + 'startfont.htm',
        StartHTM + '<p><ul>'
        + '<li><b>' + OTranslator.Translate(127,'Documentation files missing - see ')
        + '<a href="' + OTranslator.Translate(120,'Http://www.epidata.dk')+'">' + OTranslator.Translate(120,'Http://www.epidata.dk') + '</a>' + #13
        + '</b><br></ul>' + #13 + '</p></body><html>');}
  end;
  sysdir := ExtractFilePath(Application.ExeName);
  FreeAndNil(Output);
end;

procedure TaMainForm.InitializeUnits();
begin
  OTranslator := TTranslator.Create();
  // Default true given by option DISPLAY TRANSLATE CODE, but options are NOT
  // initialized at this point.
  OTranslator.ReturnOrigtext := true;

  ODocument := TDocument.Create();
  OInifile := TIniFile.Create();
  OLinearRegression := TLinearRegression.Create();
  OTables := TTables.Create();
  OLifeTables := TLifeTables.Create();
  ODos := TDos.Create();
  OAggregate := TAggregate.Create();
  //OBrowse := TBrowse.create();
  OUpdate := TUpdate.Create();
  ODebug := TDebug.Create(0);
  OGraph := TGraph.Create;
  OMerge := TMerge.Create;
end;

procedure TaMainForm.DeInitializeUnits();
begin
  if Assigned(OMerge) then FreeAndNil(OMerge);
  if Assigned(OGraph) then FreeAndNil(OGraph);
  if Assigned(ODebug) then FreeAndNil(ODebug);
//  if Assigned(OBrowse) then FreeAndNil(OBrowse);
  if Assigned(OAggregate) then FreeAndNil(OAggregate);
  if Assigned(ODos) then FreeAndNil(ODos);
  if Assigned(OTables) then FreeAndNil(OTables);
  if Assigned(OLinearRegression) then FreeAndNil(OLinearRegression);
  if Assigned(OInifile) then FreeAndNil(OInifile);
  if Assigned(ODocument) then FreeAndNil(ODocument);
  if Assigned(OTranslator) then FreeAndNil(OTranslator);
end;


procedure TaMainForm.LVarsDblClick(Sender: TObject);
var
 i:integer;
 s: string;
begin
  for i:= 0 to lvars.Items.count-1 do
  begin
    if Lvars.selected[i] then s := trim(copy(Lvars.Items[i],1,MaxVarLength))+' ';
    if pos(s,cmdedit.text) = 0 then LVarsInsert(false);
    LVars.Selected[i] := false;
  end;
  FindFocus(cmdedit);
end;

procedure TaMainform.LVarsInsert(delete: boolean = true);
var
    s : string;
    key: word;
    opt: TEpiOption;
begin
  if (dm.GetOptionValue('DISPLAY COMMAND PROMPT', opt) and (opt.Value = 'OFF')) then exit;
  if LVars.Count = 0 then exit;
  s := trim(copy(Lvars.Items[Lvars.ItemIndex],1,MaxVarLength))+' ';
  cmdedit.SelStart := length(cmdedit.text);
  if pos(' '+s+' ',cmdedit.text+' ') = 0  then  cmdedit.insert(' ' + s)
  else if delete then begin
    cmdedit.SelStart := pos(s,cmdedit.text)-1;
    cmdedit.SelLength := Length(s);
    cmdedit.ClearSelection;
  end;
end;

procedure TaMainForm.LVarsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
// dm.info(inttostr(key));
case key of
  VK_RETURN :     begin
                    LVarsInsert();
                    LvarsDblClick(self);
                    cmdedit.SelStart := length(cmdedit.text);
                  end;
  VK_SPACE,VK_INSERT   :     LVarsInsert();
  VK_TAB,VK_F4: FindFocus(cmdedit);
  81,88,114,67,VK_ESCAPE:
      begin
        FindFocus(cmdedit);
        Lvars.Visible := False;
        HalignPanel1();
{        if not CmdTree.visible then
        begin
          panel1.Visible := False;
          splitter2.Visible := false;
        end
        else CmdTree.Height := Panel1.Height; }
      end;
  else
    if Assigned(aMainForm.onKeyDown) then
      aMainForm.KeyDown(key, shift);
end;
end;

procedure TaMainForm.CmdTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Node: TTreeNode;
  i: integer;
begin
  //if node = Nil then exit;
  Node:=CmdTree.Selected;

  case Key of
     VK_RETURN:
      begin
        if (Node.Level=0) then
          Node.Expand(True)
        else
          begin
            CmdEdit.Text:=Node.Text+' '+CmdEdit.Text;
            //aMainForm.cmdeditCommand(Node);
            FindFocus(cmdedit);//CmdEdit.SetFocus;
            CmdEdit.SelStart:=Length(CmdEdit.Text);
            CmdEdit.SelLength:=0;
            end;
      end;
    88,VK_ESCAPE,VK_F2:
      begin
           FindFocus(cmdedit);//cmdedit.SetFocus;
           CMDTREE.Visible:= False; //x, Esc: close form.
           HAlignPanel1();
      end;
    VK_F4: FindFocus(cmdedit);//CmdEdit.setfocus;
    VK_ADD, 187,      {// +: Expand }
    VK_SUBTRACT, 189: {// -: Collapse}
           begin
             for i := 0 to CmdTree.Items.Count -1 do
               if (key = VK_ADD) or (key = 187) then
                 CmdTree.Items[i].Expand(true)
               else
                 CmdTree.Items[i].Collapse(true);
           end;
  else
    if Assigned(aMainForm.onKeyDown) then
      aMainForm.keyDown(key, shift);
  end;
end;


procedure TaMainForm.AcBrowseExecute(Sender: TObject);
begin
  InternalRunCommand('browse');
end;

procedure TaMainForm.AcExitExecute(Sender: TObject);
begin
  Close();
//  FormCloseQuery(nil, CanClose);
  {if MessageDlg('Quit ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    cmdedit.doCommand('quit');}
end;

procedure TaMainForm.AcFileOpenRECExecute(Sender: TObject);
begin
  CmdEdit.Text := '      ';
  InternalRunCommand('read');
  if dm.filename <> '' then aMainForm.CmdEdit.HackHistory('Read', 'Read "' +   dm.FileName +'"') ;
  //mmenu.
end;

{
procedure TaMainForm.FileSaveAs1Execute(Sender: TObject);
begin
  InternalRunCommand('route ?<SAVEDATAFILE>?');
  application.processmessages;
  InternalRunCommand('write recfile');
end;
}

function TaMainForm.ViewFile(const fn:string):boolean;
var
  path: string;
begin
  path := fn;
  if not fileexists(path) then
  begin
     if extractfileext(path)='' then
         path :=path+'.htm';
     if not fileexists(path) then
     begin
       path := extractfilename(path);
       path := GetCurrentDir+'\'+path;
     end;
     if not fileexists(path) then
     begin
       path := extractfilename(path);
       path :=extractfilepath(application.exename)+'docs\'+path;
     end;
  end;
  if not fileexists(path) then path:=fn;
  Viewer.LoadFromFile(path);
end;


procedure TaMainForm.RefreshOuput(MSg: TMessage);
var
  Stream :TMemoryStream;
begin
  Stream :=TMemoryStream(Msg.wParam);
  Viewer.LoadFromStream(Stream);
  Viewer.VScrollBarPosition := Viewer.MaxVertical;
end;

procedure TaMainForm.InternalRunCommand(cmd: string; AddToHist: boolean = true);
var
  backCursor: TCursor;
  i : integer;
  opt: TEpiOption;
const
  procname = 'InternalRunCommand';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  try
    if AddToHist then
      cmdedit.History.Add(cmd);
    indicator.State :=true;
    backCursor := Cursor;
    Screen.Cursor := crHourGlass;
    try
      dm.RunCommandLine(cmd);
    except
      raise;
    end;
  finally
    if (not EditorHasFocus) and (not HelpWindowHasFocus) and (not OUpdate.BrowseHasFocus) then
      FindFocus(cmdedit);
    indicator.State :=false;
    Screen.Cursor := backCursor;
    if (historylist.visible) and (cmdedit.Visible) then
      for i := Historylist.Count to CmdEdit.History.Count -1 do
        Historylist.Items.Add(CmdEdit.History.Strings[i]);
    CmdEdit.CurrentCmd := CmdEdit.History.Count;
    ODebug.DecIndent;
  end;
end;

procedure TaMainForm.ReloadLanguage(Msg: TMessage);
var
  opt: TEpiOption;
begin
  // Apply selected language for Translor unit.
  OTranslator.Language := pchar(msg.WParam);

  // Translate main form.
  OTranslator.TranslateForm(self);

  // Translate Editor form if present.
  if Assigned(GetEditorHandle()) then
    OTranslator.TranslateForm(GetEditorHandle());

{  // Translate Browse form.
  if Assigned(OBrowse.GetBrowserHandle()) then
    OTranslator.TranslateForm(OBrowse.GetBrowserHandle());    }

  // Translate Browse form.
  if Assigned(OUpdate.GetBrowserHandle()) then
    OTranslator.TranslateForm(OUpdate.GetBrowserHandle());

  // Translate help form.
  if Assigned(GetHelpformHandle()) then
    OTranslator.TranslateForm(GetHelpformHandle());

  // Set Charset and Font Name
//  OTranslator.ReturnOrigtext := true;
  dm.CodeMaker.Charset := OTranslator.Translate(106, 'ISO-8859-1');
  dm.SetOptionValue('VIEWER FONT CHARSET', dm.CodeMaker.Charset, true);
  Viewer.DefFontName := OTranslator.Translate(107, 'VERDANA,COURIER');
  dm.SetOptionValue('VIEWER FONT NAME', Viewer.DefFontName, true);
  Dm.GetOptionValue('DISPLAY TRANSLATE CODE', opt);
  OTranslator.ReturnOrigtext := AnsiUpperCAse(opt.Value) = 'OFF';
//  dm.Sendoutput();
end;

procedure TaMainForm.UpdateVisual(Msg: TMessage);
var
  cmd, val: string;
begin
  cmd := pchar(msg.WParam);
  val := pchar(msg.LParam);
  msg.Result := 0;
  if cmd = 'DATABROWSER' then
  begin
    if val = 'OFF' then
      OUpdate.CloseBrowse()
    else begin
      if dm.dataframe <> nil then
        OUpdate.CreateBrowse(dm.dataframe);
    end;
  end;
  if cmd = 'COMMAND PROMPT' then
  begin
    FindFocus(cmdedit);
    cmdedit.Visible := val = 'ON';
    CmdPanel.Visible := val = 'ON';
    Splitter1.Visible := val = 'ON';
    ViewHTMLsource1.Visible := val = 'ON';
    Togglemenu1.Visible := not (CmdPanel.Visible and (self.Menu = nil));
    if (CmdPanel.Visible) and (CmdPanel.Height=0) then self.CmdPanel.Height := 22;
  end;
  if cmd = 'TRANSLATE CODE' then
  begin
    OTranslator.ReturnOrigtext := val = 'OFF';
    OTranslator.TranslateForm(self);
    if Assigned(GetEditorHandle()) then
      OTranslator.TranslateForm(GetEditorHandle());
    if Assigned(GetHelpformHandle()) then
      OTranslator.TranslateForm(GetHelpformHandle());
{    if Assigned(OBrowse.GetBrowserHandle()) then
      OTranslator.TranslateForm(OBrowse.GetBrowserHandle());  }
    if Assigned(OUpdate.GetBrowserHandle()) then
      OTranslator.TranslateForm(OUpdate.GetBrowserHandle());
  end;
  if cmd = 'COMMANDTREE' then
  begin
    FindFocus(cmdtree);//Viewer.SetFocus;
    if not panel1.Visible then panel1.Visible := val = 'ON';
    cmdtree.Visible := val = 'ON';
  end;
  if cmd = 'COMMAND HISTORY' then
  begin
    FindFocus(Historylist);//Viewer.SetFocus;
    if not panel1.Visible then panel1.Visible := val = 'ON';
    Historylist.Visible := val = 'ON';
  end;
  if cmd = 'MAINMENU' then
    if val = 'ON' then
      aMainForm.Menu := OrgMainMenu
    else
      aMainForm.Menu := nil;
  if cmd = 'TOOLBAR' then
    ToolBar1.Visible := val = 'ON';
  if cmd = 'WORKTOOLBAR' then
    begin
      ProcessToolbaronoff1.ImageIndex := -1;
      wkToolBar.Visible := val = 'ON';
      wkToolBar.Hint := OTranslator.Translate(2002,'Stop Current Action');
      if val = 'ON' then ProcessToolbaronoff1.ImageIndex := 109;
    end;

  if cmd = 'VARIABLES' then
  begin
    FindFocus(Lvars);//Viewer.SetFocus;
    if not panel1.Visible then panel1.Visible := val = 'ON';
    Lvars.Visible := val = 'ON';
  end;
  HAlignPanel1();
  msg.Result := 1; // success
end;

procedure TaMainForm.FindFocus(PrefControl: TWinControl);
begin
  if PrefControl.Visible then
    PrefControl.setfocus
  else if cmdedit.Visible then
    cmdedit.SetFocus
  else
    viewer.SetFocus;
end;

procedure TaMainForm.ExecuteAction(sender:TObject);
begin
  dm.ExecShortCut(TACtion(sender).shortcut);
end;

procedure TaMainForm.InstallShortCut(psc :pchar);
var
 shc : TShortCut;
 act : TACtion;
begin
 shc :=strtoint(psc);
 act := TACtion.Create(self);
 act.ActionList:= ActionList2;
 act.ShortCut :=shc;
 act.OnExecute:=ExecuteAction;
end;


procedure TaMainForm.EngineHook(MSg: TMessage);
var
  dataframe : TEpiDataFrame;
  s:string;
begin
case Msg.Msg of
  EpiCloseFile: closeFile(Msg);
  epiCls:;
  epiEditfile:
    if Sysutils.Trim(pchar(msg.wparam))<> '' then
      ShowEditor(pchar(msg.wparam),1,false)
    else
      ShowEditor('',1,false)    ;
  epiLoadTextfile: Viewer.LoadTextFile(pchar(msg.wparam));
  epiLoadHTMLfile: ViewFile(pchar(msg.wparam));
  EpiRefreshOutput: RefreshOuput(Msg);
  EpiRefreshSelect:
   begin
     dataframe:= TEpiDataFrame(msg.LParam);
     s := Statusbar.Panels[3].text;
     if copy(s,1,4) = ' All' then s:= '';
     Statusbar.Panels[3].text:= pchar(msg.wparam);
     if Statusbar.Panels[3].text='' then
        s:= ' All Selected'
     else
       if length(s) > 0 then
         s:= '(('+Statusbar.Panels[3].text+') and ' + s + ')'
         else
         s:= '('+Statusbar.Panels[3].text+')';
     Statusbar.Panels[2].text:= format('%d ',[dataframe.selectedrowcount]);
     Statusbar.Panels[3].text:= s;
     if (copy(s,1,2) = '((') then dm.info(' '+s,0);
   end;
   EpiOpenFile:
   begin
     dataframe:= TEpiDataFrame(msg.WParam);
     Statusbar.Panels[2].text := format('%d ',[dataframe.SelectedRowCount]);
     if dataframe.RowCount = dataframe.SelectedRowCount then
     Statusbar.Panels[3].text := ' All Selected' else
     Statusbar.Panels[3].text := format('%d records excluded',
          [dataframe.rowcount - dataframe.SelectedRowCount]);
     dm.AddResult('$FileName',EpiTyString,dm.dataframe.filename, 0, 0)
   end;
   EpiVectorListChanged: RefreshVarList(msg);
   EpiRefreshDir: Statusbar.Panels[0].text:= pchar(msg.wparam);
   EpiLoadPGM:CmdEdit.LoadHistoryFromFile(pchar(msg.wparam));
   EpiSavePGM:CmdEdit.SaveHistoryToFile(pchar(msg.wparam));
   EpiAddShortCut: InstallShortCut(pchar(msg.wparam));
   EpiViewerFontName: Viewer.DefFontName :=pchar(msg.wparam);
   EpiViewerFontSize: Viewer.DefFontSize :=msg.wparam;
   EpiViewerFontCharset: begin
                           dm.CodeMaker.Charset := pchar(msg.WParam);
                           dm.Sendoutput;
                         end;
   EpiUpdateVisual: UpdateVisual(Msg);
   EpiEngineError : ShowErrorLine(msg.wparam);
   EpiLanguage: begin
                  ReloadLanguage(Msg);
                  setupbasicfiles;
                end;
  end;//case
end;

procedure TaMainForm.ShowErrorLine(ErrLine: integer);
begin
   CurrentErrorLine:=ErrLine;
end;

procedure TaMainForm.RefreshVarList(MSg: TMessage);
var
  i,j, co : integer;
  dataframe : TEpiDataFrame;
  s : string;
begin
  dataframe := TEpiDataFrame(msg.WParam);
  caption := GetBuildInfoAsString+' '+  dataframe.FileName ;
  Statusbar.Panels[0].text := GetCurrentdir;
  co := dataframe.VectorCount;
  LVars.clear;
  j := 0;
  for i:= 0 to co-1 do j := max(j, length(dataframe.Vectors[i].Name));
  for i:= 0 to co-1 do
  begin
    s := ' '+ EpiTypeShortNameArray[dataframe.vectors[i].FieldDataType] + ' ';
    LVars.items.AddObject(AnsilowerCase(dataframe.Vectors[i].Name),dataframe.Vectors[i]);
    LVars.items[i] := Padright(LVars.items[i],' ',j) + s +  dataframe.vectors[i].GetVariableLabel;
    MaxVarLength := j;
  end;
  if co > 0 then
    dm.AddResult('$VarCount', EpiTyInteger, dataframe.VectorCount, 0, 0)
end;

procedure TaMainForm.Closefile(MSg: TMessage);
begin
  Lvars.clear;
  caption :=GetBuildInfoAsString;
  Statusbar.Panels[0].text:= GetCurrentDir;
  Statusbar.Panels[2].text:='';
  Statusbar.Panels[3].text:='';
  //  Statusbar.Panels[4].text:='';
end;


procedure TaMainForm.AcCancelExecute(Sender: TObject);
begin
 DM.cancel;
 if ActiveControl=CmdEdit then
    cmdedit.Cancel;
 if ActiveControl=Viewer then
   DM.Sendoutput();
end;

procedure TaMainForm.EditCopy1Execute(Sender: TObject);
begin
  if activecontrol= Viewer then
    Viewer.copytoclipboard
  else if ActiveControl=CmdEdit then
    CmdEdit.CopyToClipboard;
end;

procedure TaMainForm.EditCopy1Update(Sender: TObject);
begin
  Editcopy1.Enabled :=(activecontrol= Viewer) or  (ActiveControl=CmdEdit)
end;

procedure TaMainForm.HelpAboutItemClick(Sender: TObject);
begin
 showabout(false,'Data management and statistical analysis package',
  'Epidata Association',
  'Coordinator. Design, implementation, documentation, programming, installation: JM.Lauritsen (Since 2001)',
  'Programming - T.Christiansen, M.Bruus, JM.Lauritsen, S.Kreiner',
  'Contributions - specification: (SPC: V.Høgli, B. Nyen), J. Hockin, P. Arias, G.Desve',
  'Core parser commands, data management etc: S.Mahmud (2002-2003), Installation Routines : G.Desve',
  'Http://www.epidata.dk',
  'Copyright: EpiData Association, Odense Denmark 2001-2008');
end;


procedure TaMainForm.UpdateCommandTree;
var
  GrpNode: TTreeNode;
begin
  if cmdtree = Nil then
     cmdtree := Ttreeview.create(self);
  with cmdTree.Items do
  begin
    BeginUpdate;
    GrpNode:=Add(nil,'Read & start');
    AddChild(GrpNode,'Read');
    AddChild(GrpNode,'Select');
    AddChild(GrpNode,'Run');
    AddChild(GrpNode,'Close');

    GrpNode:=Add(nil,'View Data');
    AddChild(GrpNode,'Browse');
    AddChild(GrpNode,'List');

    GrpNode:=Add(nil,'Analysis');
    AddChild(GrpNode,'Describe');
    AddChild(GrpNode,'Freq');
    AddChild(GrpNode,'Tables');
    AddChild(GrpNode,'Means');
    AddChild(GrpNode,'Regress');
    AddChild(GrpNode,'Kwallis');
    AddChild(GrpNode,'Correlate');
    AddChild(GrpNode,'StatTables');
    AddChild(GrpNode,'Lifetable');

    GrpNode:=Add(nil,'Graphs');
    AddChild(GrpNode,'Bar');
    AddChild(GrpNode,'Histogram');
    AddChild(GrpNode,'BoxPlot');
    AddChild(GrpNode,'Scatter');
    AddChild(GrpNode,'Line');
    AddChild(GrpNode,'Dotplot');
    AddChild(GrpNode,'CDFplot');
    AddChild(GrpNode,'CIPlot');
    AddChild(GrpNode,'EpiCurve');
    AddChild(GrpNode,'Pie');

    GrpNode:=Add(nil,'Save Output, Clear Screen');
    AddChild(GrpNode,'Logopen');
    AddChild(GrpNode,'Logclose');
    AddChild(GrpNode,'Cls');
    AddChild(GrpNode,'View');

    GrpNode:=Add(nil,'Generate/Change Variables');
    AddChild(GrpNode,'Define');
    AddChild(GrpNode,'Let');
    AddChild(GrpNode,'Gen');
    AddChild(GrpNode,'If (       )  then  x=');
    AddChild(GrpNode,'Recode .. to .. by ..');
    AddChild(GrpNode,'Drop');
    AddChild(GrpNode,'Keep');

 //  GrpNode:=Add(nil,'Further Analysis');
//    AddChild(GrpNode,'Match');

    GrpNode:=Add(nil,'SPC graphs');
    AddChild(GrpNode,'PChart');
    AddChild(GrpNode,'Ichart');
    AddChild(GrpNode,'RunChart');
    AddChild(GrpNode,'Pareto');

    GrpNode:=Add(nil,'Save-Sort-Edit Data');
    AddChild(GrpNode,'Sort');
    AddChild(GrpNode,'Merge');
    AddChild(GrpNode,'SaveData');
    AddChild(GrpNode,'Aggregate');
    AddChild(GrpNode,'Update/Edit Data');

    GrpNode:=Add(nil,'Clean Up and Stop');
    AddChild(GrpNode,'Close');
    AddChild(GrpNode,'SavePgm ');
    AddChild(GrpNode,'Clh');
    AddChild(GrpNode,'Var Temp Clear');
    AddChild(GrpNode,'Quit');

    GrpNode:=Add(nil,'Information');
    AddChild(GrpNode,'Variables');
    AddChild(GrpNode,'Type');
    AddChild(GrpNode,'result');
    AddChild(GrpNode,'Echo');
    AddChild(GrpNode,'Show');
    AddChild(GrpNode,'Set');
    AddChild(GrpNode,'?');


    GrpNode:=Add(nil,'Disk Commands');
    AddChild(GrpNode,'CD');
    AddChild(GrpNode,'Dir');
    AddChild(GrpNode,'DOS');
    AddChild(GrpNode,'!');
    AddChild(GrpNode,'Copyfile');
    AddChild(GrpNode,'Erase');
    EndUpdate;
  end;
end;

procedure TaMainForm.CmdTreeDblClick(Sender: TObject);
var
  State: word; // cmdNode: TTreeNode;
begin
  State := VK_RETURN;
  CmdTreeKeyDown(Sender, State, []);
{ cmdNode:= cmdtree.selected;
 if cmdNode=nil then exit;
 if (GetKeyState(VK_CONTROL) and not $7FFF) <> 0 then
 begin
   cmdedit.text:= cmdNode.Text+' ';
   cmdedit.SelStart:=length(cmdedit.text);
 end
 else
   cmdedit.doCommand(cmdNode.Text);}
end;


procedure TaMainForm.AcShowWindowCommandsLeft(Sender: TObject);
var
  s:string;
  sst: word;
  opt: TEpiOption;
begin
//  if (dm.GetOptionValue('DISPLAY COMMAND PROMPT', opt) and (opt.Value = 'OFF')) then exit;
{  if Lvars.Visible then
  begin
    CmdTree.Align := alTop;
    Cmdtree.Height := round(panel1.Height/2)-1;
  end else
    CmdTree.Align := alClient;        }
  if (CmdTree.Visible) and (CmdTree.Focused) then
  begin
    sst := 88;
    CmdTreeKeyDown(self, sst , []);
    HAlignPanel1();
    exit;
  end;
  if not panel1.Visible then panel1.visible := True;
  if not Splitter2.Visible then Splitter2.Visible := true;
  CmdTree.visible := True;
  HalignPanel1();
  CmdTree.TabStop := True;
  FindFocus(CmdTree);//.SetFocus;
  //cmdedit.SelStart:=length(cmdedit.text);
end;

procedure TaMainForm.AcShowVariablesLeft(Sender: TObject);
var
  s1:string;
  sst: word;
begin
  // if dm.FileName = '' then begin dm.info('No file open'); exit; end;
    dm.checkdataopen();

  {  if CmdTree.Visible then
  begin
    CmdTree.Align := alTop;
    Cmdtree.Height := round(panel1.Height/2)-1;
  end; }
  if (LVars.Visible) and (LVars.Focused) then
  begin
    sst := VK_ESCAPE;
    LVarsKeyDown(self, sst , []);
    HAlignPanel1(); //CmdTree.Align := alClient;
    exit;
  end;
  if not panel1.Visible then panel1.visible := True;
  if not Splitter2.Visible then splitter2.Visible := true;
  Lvars.visible := True;
  HAlignPanel1();
  Lvars.TabStop := True;
  FindFocus(Lvars); //.SetFocus;
  cmdedit.SelStart:=length(cmdedit.text);
end;

procedure TaMainForm.AcShowHistory(Sender: TObject);
var
  s1:string;
  sst: word;
  i: integer;
begin
  if (HistoryList.Visible) and (HistoryList.Focused) then
  begin
    sst := 88;
    HistoryListKeyDown(self, sst , []);
    HAlignPanel1();
    exit;
  end;
  if not panel1.Visible then panel1.visible := True;
  if not Splitter2.Visible then splitter2.Visible := true;
  Historylist.Clear;
  for i := 0 to CmdEdit.History.Count -1 do
       Historylist.Items.Add(CmdEdit.History.Strings[i]);
  Historylist.visible := True;
  HAlignPanel1();
  Historylist.TabStop := True;
  FindFocus(Historylist);//.SetFocus;
end;

procedure TaMainForm.AddCommand(const s:string;const eg:string);
var s1: string;
begin
// cmdedit.add('* e.g.: ' + s + ' ' +  eg);
 cmdedit.insert(s+' '+s1) ;
 cmdedit.SelStart:=length(cmdedit.text);
 FindFocus(cmdedit);//cmdedit.SetFocus;
end;


procedure TaMainForm.A(Sender: TObject);
var
 s : string;
begin
 cmdedit.SelStart:=length(cmdedit.text);
 FindFocus(cmdedit);
end;

procedure TaMainForm.Regress1Click(Sender: TObject);
begin
  InternalRunCommand('Regress // max 5000 observations');
end;

procedure TaMainForm.AcWindowEditExecute(Sender: TObject);
begin
  InternalRunCommand('update');
end;

procedure TaMainForm.AcCdExecute(Sender: TObject);
var
 foldername: string;
begin
  foldername := GetCurrentdir;
  if foldername = '..' then foldername := '..\';
  if BrowseFolder(foldername,'Select current folder') then
    InternalRunCommand('cd '+ AnsiQuotedStr(foldername,'"'));
    Statusbar.Panels[0].text:= GetCurrentdir;
end;

procedure TaMainForm.StatusBarDblClick(Sender: TObject);
begin
  if StatusBar.MousePanel =  StatusBar.panels[0] then
    AcCd.Execute;
end;

procedure TaMainForm.AcPrinterSetupExecute(Sender: TObject);
begin
    PrinterSetupDialog.Execute;
end;


procedure TaMainForm.PrintHeader(Sender: TObject; Canvas: TCanvas;
  NumPage, W, H: Integer; var StopPrinting: Boolean);
var
  AFont: TFont;
begin
AFont := TFont.Create;
AFont.Name := 'Arial';
AFont.Size := 8;
with Canvas do
  begin
  Font.Assign(AFont);
  SetTextAlign(Handle, TA_Top or TA_Left);
  TextOut(50, 40, Viewer.DocumentTitle);
  SetTextAlign(Handle, TA_Top or TA_Right);
  TextOut(W-50, 40, Viewer.CurrentFile);
  end;
AFont.Free;
end;

procedure TaMainForm.PrintFooter(Sender: TObject; Canvas: TCanvas;
  NumPage, W, H: Integer; var StopPrinting: Boolean);
var
  AFont: TFont;
begin
AFont := TFont.Create;
AFont.Name := 'Arial';
AFont.Size := 8;
with Canvas do
  begin
  Font.Assign(AFont);
  SetTextAlign(Handle, TA_Bottom or TA_Left);
  TextOut(50, 20, DateToStr(Date));
  SetTextAlign(Handle, TA_Bottom or TA_Right);
  TextOut(W-50, 20, 'Page '+IntToStr(NumPage));
  end;
AFont.Free;
end;

procedure TaMainForm.AcPrintViewerExecute(Sender: TObject);
begin
with PrintDialog do
  if Execute then
    if PrintRange = prAllPages then
      viewer.Print(1, 9999)
    else
      Viewer.Print(FromPage, ToPage);
end;

procedure TaMainform.PrintViewer();
begin
  try
    Viewer.Print(1,9999);
  except
    //ShowMessage(GetLastErrorMsg());
  end;
end;


procedure TaMainForm.AcPrintPreviewExecute(Sender: TObject);
var
  pf: TPreviewForm;
  Abort: boolean;
begin
  pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
  try
    if not Abort then
      pf.ShowModal;
  finally
    pf.Free;
  end;
end;

procedure TaMainForm.AclogopenExecute(Sender: TObject);
begin
  InternalRunCommand('logopen');
end;

procedure TaMainForm.AclogcloseExecute(Sender: TObject);
begin
  InternalRunCommand('logclose');
end;

procedure TaMainForm.AcViewhtmlFileExecute(Sender: TObject);
var
 fn, oldcss : string;
begin
  if GetOpenfilename(fn,EpiLogFileFilter) then
  begin
    Viewer.LoadFromFile(fn)
  end;
end;

procedure TaMainForm.EditCopyHTMLExecute(Sender: TObject);
var
 html: string;
// buf : array[0..4000] of char;
begin
//  Lvars.items.add('-----------');
//  enumclipformat;
//  html:= '<H1> test</H1>';
  CopyHTMLToClipBoard(SelectedHTML);
//  Lvars.items.add('-----------');
//  enumclipformat;
end;


procedure TaMainForm.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  BUFSIZE = 1000000;
var
  Pos, Pos2, VSelLength: integer;
  st   : TStream;
  buf : array[0..BUFSIZE] of char;
begin
  VSelLength := Viewer.SelLength;
  if VSelLength >= 0 then
    Pos := Viewer.FindSourcePos(Viewer.SelStart)
  else Pos := Viewer.FindSourcePos(Viewer.SelStart+VSelLength);
  st := dm.OutputStream;
  if VSelLength = 0 then
  //      RichEdit.SelLength := 0
  else
  if VSelLength > 0 then
    begin
    Pos2 := Viewer.FindSourcePos(Viewer.SelStart+VSelLength-1)+1;
    VSelLength := Pos2-Pos;
    end
  else
    begin
    Pos2 := Viewer.FindSourcePos(Viewer.SelStart-1)+1;
    VSelLength := Pos2-Pos;
    end;
  if (Pos >= 0) and (VSelLength > 0) then
  begin
   st.Position :=pos;
   if VSelLength > BUFSIZE then
   begin
     dm.Sendoutput;
     dm.Error('Exceeded maximum selection size', [], 26001);
   end;
   st.read(buf,VSelLength-1);
   SelectedHTML:=buf;
  end;
end;

procedure TaMainForm.EditClearScreenExecute(Sender: TObject);
begin
   dm.cls;
   // InternalRunCommand('cls');
end;

procedure TaMainForm.AcEditorExecute(Sender: TObject);
begin
  ShowEditor('',1,false);
  booUMainEditorFocused:=False;
end;

procedure TaMainForm.doCommand(const cmd: string);
begin
  try
    indicator.State :=true;
    try
      InternalRunCommand(cmd);
    except
      raise;
    end;
  finally
     indicator.State :=false;
  end;
end;


procedure TaMainForm.AcfilepropertiesExecute(Sender: TObject);
begin
  if dm.dataframe=nil then exit;
  showFileProp(dm.dataframe.FileName, handle);
end;

procedure TaMainForm.SetCurrentErrorLine(const Value: integer);
begin
  FCurrentErrorLine := Value;
end;

procedure TaMainForm.AcViewHTMLSourceExecute(Sender: TObject);
var
 stream : TStream;
 s: string;
 p: pchar;
begin
  begin
   Stream := dm.OutputStream;
   Stream.position:=0;
   ShowEditorUsingStream(Stream ,'',false);
  end;

// Not yes working...
{  Viewer.SelectAll;
  p := pchar(Viewer.SelText);
  Stream := TMemoryStream.Create();
  Stream.WriteBuffer(p, length(Viewer.SelText));
  Stream.position:=0;
  ShowEditorUsingStream(Stream, '', false);
  FreeAndNil(Stream);}

  //InternalRunCommand('logopen'); // not sure this is right?

{  Stream := dm.OutputStream;     //todo: change to current view instead of current latest output
  Stream.position:=0;
  ShowEditorUsingStream(Stream ,'',false);}
end;

function TaMainForm.GetCurrentErrorLine: integer;
begin
 result := dm.Executor.CurrentExecLine;
end;

procedure TaMainForm.AcSelectAllExecute(Sender: TObject);
begin
  Viewer.SelectAll;
end;

procedure TaMainForm.FormCloseQuery(Sender: TObject;  var CanClose: Boolean);
begin
  CanClose := false;
  if not forcequit then
  begin
    if MessageDlg(#13#13 + 'Quit ?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      exit;
    if not ShutdownEditor then
      exit;
  end;
  // Shutdown must be in the order - Mainform, then dm. Else history will be corrupted.
  HandleShutdown();
  dm.HandleShutdown();
  CanClose := True;
end;

procedure TaMainForm.HandleShutdown();
var
  s: string;
begin
  CmdEdit.History.Insert(0, '// ' + GetBuildInfoAsString + ' ' + DateToStr(Date) + ' ' + TimeTostr(Time));
  S := AnsiUpperCase(Trim(CmdEdit.History[CmdEdit.History.Count-1]));
  if (s = 'QUIT') or (s = 'EXIT') then
    CmdEdit.History.Delete(CmdEdit.History.Count - 1);
  SetPrecisionMode(oldPrecisionMode);
end;

procedure TaMainForm.AcFilePageSetupExecute(Sender: TObject);
begin
  PageSetupDialog.execute
end;

procedure TaMainForm.HotSpotClick(Sender: TObject; const URL: string;
          var Handled: boolean);
{This routine handles what happens when a hot spot is clicked.  The assumption
 is made that DOS filenames are being used. .EXE, .WAV, .MID, and .AVI files are
 handled here, but other file types could be easily added.

 If the URL is handled here, set Handled to True.  If not handled here, set it
 to False and ThtmlViewer will handle it.}
const
  snd_Async = $0001;  { play asynchronously }
var
  PC: array[0..255] of char;
  S: string;
  Params: string[255];
  Ext: string[5];
  ID: string;
  I, J, K: integer;
begin
Handled := False;
{The following looks for a link of the form, "IDExpand_XXX".  This is interpreted
 as meaning a block with an ID="XXXPlus" or ID="XXXMinus" attribute should
 have its Display property toggled.
}
J := Pos('EPI:', AnsiUpperCase(URL));
if (J > 0) then
begin
  ID :=copy(URL,j+4,length(URL));
  dm.RunCommndsList(Id);
  Handled := True;
  Exit;
end;

I := Pos('IDEXPAND_', AnsiUpperCase(URL));
if I=1 then
  begin
  ID := Copy(URL, 10, Length(URL)-9);
  Viewer.IDDisplay[ID+'Plus'] := not Viewer.IDDisplay[ID+'Plus'];
  Viewer.IDDisplay[ID+'Minus'] := not Viewer.IDDisplay[ID+'Minus'];
  Viewer.Reformat;
  Handled := True;
  Exit;
  end;

I := Pos(':', URL);
J := Pos('FILE:', AnsiUpperCase(URL));
K := Pos('.PDF', AnsiUpperCase(URL));
if (I <= 2) or (J > 0) or (K > 0) then
  begin                      {apparently the URL is a filename}
  S := URL;
  K := Pos(' ', S);     {look for parameters}
  if K = 0 then K := Pos('?', S);  {could be '?x,y' , etc}
  if K > 0 then
    begin
    Params := Copy(S, K+1, 255); {save any parameters}
    setlength(S, K-1);            {truncate S}
    end
  else Params := '';
  S := Viewer.HTMLExpandFileName(S);
  Ext := AnsiUpperCase(ExtractFileExt(S));
  if Ext = '.WAV' then
    begin
    Handled := True;
//    sndPlaySound(StrPCopy(PC, S), snd_ASync);
    end
  else if Ext = '.EXE' then
    begin
    Handled := True;
    WinExec(StrPCopy(PC, S+' '+Params), sw_Show);
    end
  else if (Ext = '.MID') or (Ext = '.AVI')  then
    begin
    Handled := True;
    WinExec(StrPCopy(PC, 'MPlayer.exe /play /close '+S), sw_Show);
    end
    else if (Ext = '.PDF') then
    begin
      Handled := True;
      ShellExecute(0, 'open', pchar(s{url}), nil, nil, SW_SHOWNORMAL);
    end;
  {else ignore other extensions}
//  Edit1.Text := URL;
  Exit;
  end;
  I := Pos('MAILTO:', AnsiUpperCase(URL));
  J := Pos('HTTP:', AnsiUpperCase(URL));

if (I > 0) or (J > 0) or (K > 0)then
  begin
  ShellExecute(0, nil, pchar(URL), nil, nil, SW_SHOWNORMAL);
  Handled := True;
  Exit;
  end;
end;

procedure TaMainForm.ImageClick(Sender, Obj: TObject; Button: TMouseButton;
                         Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
begin
  if Button = mbRight then
    if (Obj is TImageObj) then
    begin
      FoundObject := TImageObj(Obj);
      GetCursorPos(Pt);
      CopyToClipboard.OnClick := CopyImageToClipboardClick;
      //Viewerpopup.Popup(Pt.X, Pt.Y);
      ViewerImagePopup.Popup(Pt.X+20, Pt.Y+50);
    end;
end;

procedure TaMainForm.ViewerRightClick(Sender: TObject; Parameters: TRightClickParameters);
var
  pt: TPoint;

  function InTable():boolean;
  const
    buffer_feed: integer = 200;
  var
    buffer: string;
    copybuf: pchar;
    bpos, epos, sourcepos,
    pos: integer;
  begin
    result := false;
    sourcepos := Viewer.FindSourcePos(Viewer.SelStart)-(buffer_feed div 2);
    buffer := Copy(Viewer.DocumentSource, sourcepos, buffer_feed);
    pos := System.Pos('<TD', buffer);
    if pos > 0 then // Locate '<TABLE'...
    begin
      result := true;
      pos := System.Pos('<TABLE', buffer);
      while (pos = 0) do
      begin
        sourcepos := max(sourcepos-buffer_feed,0);
        buffer := Copy(Viewer.DocumentSource, sourcepos, buffer_feed);
        pos := System.Pos('<TABLE', buffer);
      end;
      inc(sourcepos, pos);
      bpos := Sourcepos-1;
      pos := System.Pos('</TABLE>', buffer);
      while (pos = 0) do
      begin
        sourcepos := sourcepos+buffer_feed;
        buffer := Copy(Viewer.DocumentSource, sourcepos, buffer_feed);
        pos := System.Pos('</TABLE>', buffer);
      end;
      epos := sourcepos + pos + 6;
      copybuf := StrAlloc(epos-bpos);
      StrCopy(copybuf, pchar(Copy(Viewer.DocumentSource, bpos, epos-bpos)));
      FoundObject := TObject(copybuf);
    end;
  end;

begin
  // Do NOT popup when rightclicking image - handled above.
  if Parameters.Image <> nil then exit;
  GetCursorPos(Pt);
  {
  if InTable() then
  begin
    // if select length = 0 then
    CopyToClipboard.OnClick := CopyTableToClipboardClick;
    ViewerImagePopup.Popup(Pt.X, Pt.Y);
  end else       }
    Viewerpopup.Popup(Pt.X, Pt.Y)
end;

procedure TaMainForm.ReloadButtonClick(Sender: TObject);
{the Reload button was clicked}
begin
with Viewer do
  begin
  AcWebReLoad.Enabled := False;
  ReLoad;
  AcWebReLoad.Enabled := CurrentFile <> '';
  Viewer.SetFocus;
  end;
end;

procedure TaMainForm.FwdBackClick(Sender: TObject);
{Either the Forward or Back button was clicked}
begin
with Viewer do
  begin
  if Sender = AcWebBack then
    HistoryIndex := HistoryIndex +1
  else
    HistoryIndex := HistoryIndex -1;
  Self.Caption := DocumentTitle;
  end;
end;

procedure TaMainForm.HistoryChange(Sender: TObject);
{This event occurs when something changes history list}
var
  I: integer;
  Cap: string[80];
begin
with Sender as ThtmlViewer do
  begin
  {check to see which buttons are to be enabled}
  AcWebForward.Enabled := HistoryIndex > 0;
  AcWebBack.Enabled := HistoryIndex < History.Count-1;
(*
  {Enable and caption the appropriate history menuitems}
  HistoryMenuItem.Visible := History.Count > 0;
  for I := 0 to MaxHistories-1 do
    with Histories[I] do
      if I < History.Count then
        Begin
        Cap := History.Strings[I];
        if TitleHistory[I] <> '' then
          Cap := Cap + '--' + TitleHistory[I];
        Caption := Cap;    {Cap limits string to 80 char}
        Visible := True;
        Checked := I = HistoryIndex;
        end
      else Histories[I].Visible := False;
  Caption := DocumentTitle;    {keep the caption updated}
  Viewer.SetFocus;*)
  end;
end;

procedure TaMainForm.AcShowStartPageExecute(Sender: TObject);
var
opt :Tepioption;

begin
  if (DM.GetOptionValue('START PAGE', opt) and (fileexists(opt.Value))) then
    InternalRunCommand('view "' + opt.value + '"')
  else
    InternalRunCommand('view "' + extractfilepath(application.exename)
        + 'docs\' + OTranslator.Translate(105,'en') + '\start.htm"');
end;

procedure TaMainForm.AcHelpWindowExecute(Sender: TObject);
begin
  dm.help(cmdedit.Text);
end;

procedure TaMainForm.AcHelpWindowKeyboard(Sender: TObject);
begin
 dm.ViewHelpfile(extractfilepath(application.exename)+'docs\' + OTranslator.Translate(105,'en') + '\keyboard.htm');
end;

procedure TaMainForm.AcFontSelectExecute(Sender: TObject);
begin
  dm.ViewHelpfile(extractfilepath(application.exename)+'docs\' + OTranslator.Translate(105,'en') + '\startfont.htm');
end;

procedure TaMainForm.AcEditSetupFile(Sender: TObject);
begin
  InternalRunCommand('edit "' + extractfilepath(application.exename)+ 'epidatastat.ini"')
end;

procedure TaMainForm.DoGraphDlg(const cmdstring:string;Xvars, Yvars: Integer;Xlegal, Ylegal: Array of integer;
           AdvType: TEpiAdvOptions = [];By:Boolean=False; Weigth: boolean=false;
           t1:string='X Variable';
           t2:string='Y Variable 1';
           t3:string='Y Variable 2';
           t4:string='Y Variable 3';
           tb:string='Y Variable 4');
var
 cmd: string;
 Res: integer;
begin
  Res:= showGraphdlg(self, CmdString, cmd, Xvars, Yvars,xlegal,ylegal, AdvType, By, Weigth,
            t1,t2,t3,t4,tb);
  if Res <> DlgResCancel then cmdedit.Clear;
  case Res of
    DlgResRun: InternalRunCommand(cmd);
    DlgResPaste: cmdedit.insert(cmd)
  end;
end;

procedure TaMainForm.DoGraphDlg(const DialogOptions: TGraphDlgOptions);
var
  GraphDlg: TGraphDialog;
  Res: integer;
begin
  if not dm.CheckDataOpen() then exit;
  GraphDlg := TGraphDialog.Create(self);
  GraphDlg.Initialize(DialogOptions);
  Res := GraphDlg.ShowModal;
  if Res = mrOk then
    InternalRunCommand(GraphDlg.CmdString);
  FreeAndNil(GraphDlg);
end;

procedure TaMainForm.DoTableDlg(const cmdstring: string);
var
 cmd: string;
 Res: integer;
begin
  Res := showTabledlg(self, CmdString, cmd);
  if Res <> DlgResCancel then cmdedit.Clear;
  case Res of
    DlgResRun: InternalRunCommand(cmd);
    DlgResPaste: cmdedit.insert(cmd)
  end;
end;

procedure TaMainForm.DoDlg(const cmdstring:string);
var
 cmd: string;
 Res: integer;
begin
  Res:= showdlg(self, CmdString, cmd);
  case Res of
    DlgResRun: InternalRunCommand(cmd);//cmdedit.doCommand(cmd);
    DlgResPaste: cmdedit.insert(cmd)
  end;
end;

procedure TaMainForm.AcBrowseFldsExecute(Sender: TObject);
begin
 DoDlg('browse varlist');
end;

procedure TaMainForm.AcDoDescribeExecute(Sender: TObject);
begin
 DoDlg('describe varlist');
end;

procedure TaMainForm.AcListExecute(Sender: TObject);
begin
  DoDlg('list varlist');
end;

procedure TaMainForm.toolbar1Click(Sender: TObject);
begin
 if toolbar1.Visible then toolbar1.Visible := False
  else toolbar1.Visible := True;

 if lvars.visible and cmdtree.visible then
  begin
  Lvars.height := round(Panel1.height/2);
  CmdTree.height := round(Panel1.height/2);
  end;
  if lvars.visible and not cmdtree.visible then
    Lvars.height := Panel1.height;
  if not lvars.visible and cmdtree.visible then
    CmdTree.height := Panel1.height;
end;

procedure TaMainForm.wktoolbar1Click(Sender: TObject);
var
  s: string;
begin

 if wktoolbar.Visible then
   s := 'OFF'
 else
   s := 'ON';
 dm.SetOptionValue('DISPLAY WORKTOOLBAR', s);
end;

procedure TaMainForm.CreateNewVariables1Click(Sender: TObject);
begin
      addcommand('gen',' new = old/10')
end;


procedure TaMainForm.Recode1Click(Sender: TObject);
begin
      addcommand('recode', 'v1 to v1new 1-3=1 4-6=4 or age to agegrp by 10')
end;

procedure TaMainForm.Scatter1Click(Sender: TObject);
begin
       addcommand('scatter','xvar y1 y2 y3 ...')
end;

procedure TaMainForm.Line1Click(Sender: TObject);
begin
             addcommand('line','x-var y1 y2 y3 ...')
end;

procedure TaMainForm.Histogram1Click(Sender: TObject);
begin
                  addcommand('histogram','agegroup')
end;

procedure TaMainForm.Bar1Click(Sender: TObject);
begin
      addcommand('bar','agegroup')
end;

procedure TaMainForm.IChart1Click(Sender: TObject);
begin
       addcommand('ichart','Count mean lcl ucl')
end;

procedure TaMainForm.PChart1Click(Sender: TObject);
begin
       addcommand('pchart','time count total /xlabel=var /break=value /text="x,y,text to show,box"')
end;

procedure TaMainForm.Means1Click(Sender: TObject);
begin
        dodlg('means varlist')
end;

procedure TaMainForm.count1Click(Sender: TObject);
begin
          addcommand('count ','if age < 10')
end;

procedure TaMainForm.KruskalWallisTest1Click(Sender: TObject);
begin
       addcommand('kwallis','age sex')
end;

{procedure TaMainForm.s(mess: string); //FB: help procedure.
begin
  MessageDlg(mess,mtInformation,[mbOK],0);
end;
}
procedure TaMainForm.UpdateWindowFont(fontsize: EpiInt);
var
  opt: TEpiOption;
  size: integer;
begin
  cmdtree.Font.Size := fontsize;
  lvars.Font.Size := fontsize;
  historylist.Font.Size := fontsize;
  cmdpanel.Height := cmdpanel.Height + round(2*(fontsize-cmdedit.Font.Size));
  cmdedit.Font.Size := fontsize;
end;

procedure TaMainForm.CommandPrompt1Click(Sender: TObject);
var
 i : integer;
begin
  for i:= 0 to lvars.Items.count-1 do Lvars.selected[i]:=false;
  FindFocus(cmdedit);//CmdEdit.SetFocus;
  CmdEdit.SelStart:=Length(CmdEdit.Text);
  CmdEdit.SelLength:=0;
  if CmdEdit.Height < 22 then  CmdEdit.Height := 22;
end;

procedure TaMainForm.AcRunDescribeExecute(Sender: TObject);
begin
  DoDlg('describe varlist');
end;

procedure TaMainForm.AcRunRegressExecute(Sender: TObject);
begin
  DoDlg('regress varlist');
end;

procedure TaMainForm.AcRunFreqExecute(Sender: TObject);
begin
  DoTableDlg('freq varlist');
end;

procedure TaMainForm.WKTOOOLBARClick(Sender: TObject);
VAR
  p:TPoint;
begin
 if (sender = ViewerBtn) then
    begin
        ViewerBtn.Down:=True;
        P.y:=ViewerBtn.Top + ViewerBtn.Height;
        P.x:=ViewerBtn.Left;
        p:=wkToolBar.ClientToScreen(P);
        ViewerSubMenu.Popup(p.x,p.y);
        ViewerBtn.Down := False;
    end
   else if (sender.ClassType = TSpeedButton) then
    begin
        (Sender As TSpeedButton).Down:=True;
        P.y:=(Sender as TSpeedButton).Top + (Sender as TSpeedButton).Height;
        P.x:=(Sender as TSpeedButton).Left;
          p:=wkToolBar.ClientToScreen(P);
        IF Sender=ReadDataBtn THEN internalruncommand('read');
        IF Sender=GraphBtn THEN GraphSubMenu.PopUp(p.x,p.y);
        IF Sender=BrowseDataBtn THEN internalRunCommand('browse');
        IF Sender=DescribeBtn THEN DescribeSubMenu.PopUp(p.x,p.y);
        IF Sender=EditorBtn THEN AcEditorExecute(Self);
        {  IF Sender=ExportDataBtn THEN ExportDataPopUp.PopUp(p.x,p.y);   }
        (Sender As TSpeedButton).Down:=False;
    end
   else exit;
END;

procedure TaMainForm.ShowVarByRightClick(x, y: Integer);
var
 p,q : TPoint;
 idx: integer;
 varname: string;
 v: TEpiVector;
 s: string;
begin
  p.X := x; p.Y := y;
  idx := lvars.ItemAtPos(p,true);
  if idx = -1 then exit;
  varname := Trim(Copy(LVars.Items[idx], 1, MaxVarLength));
  v := dm.dataframe.VectorByName[varname];
  s := 'Name: ' + v.Name +  '    Type: ' + ODocument.AppendDateType(v, GetFieldTypeName(v.FieldDataType)) + #13;
  s := s + 'Label: ' + v.GetVariableLabel + #13;
  if Assigned(v.CheckProperties.ValueLabelSet) then
  begin
    s := s + 'Value      Label' + #13;
    for idx := 0 to v.CheckProperties.ValueLabelSet.Count-1 do
      s := s + ' ' + v.CheckProperties.ValueLabelSet[idx] + '         ' + PChar(v.CheckProperties.ValueLabelSet.Objects[idx]) + #13;
  end;
  //messagedlg(s,mtInformation,[mbOK],0,);
  ShowMessage(s);
end;

procedure TaMainForm.LvarsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: LVarsInsert();
    mbRight: ShowVarByRightClick(x,y);
  else
    ;
  end;
end;

procedure TaMainForm.LvarsEnter(Sender: TObject);
var
  i: integer;
  s: string;
begin
  for i := 0 to LVars.Items.Count - 1 do
    if pos(trim(copy(LVars.Items[i], 1, 10)) + ' ', cmdedit.Text) > 0 then LVars.Selected[i] := true;
end;

procedure TaMainForm.AcRunTableExecute(Sender: TObject);
begin
  DoTableDlg('Tables varlist');
end;


procedure TaMainForm.AcRunMeansExecute(Sender: TObject);
begin
  DoDlg('means varlist');
end;

procedure TaMainForm.Togglemenu1Click(Sender: TObject);
begin
  if self.Menu <> NIL THEN
    self.Menu:=NIL
  ELSE
    self.Menu:=OrgMainMenu;
end;

procedure TaMainForm.AcFileSaveExecute(Sender: TObject);
begin
  InternalRunCommand('savedata');
end;

procedure TaMainForm.LvarsExit(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to LVars.Items.Count - 1 do
    LVars.Selected[i] := false;
end;

procedure TaMainForm.SaveWindowsPosition1Click(Sender: TObject);
begin
  OIniFile.SaveCurrentForm(Self, 'Main');
end;

procedure TaMainForm.Splitter2Moved(Sender: TObject);
begin
  OInifile.SaveSplitter(Panel1.Width);
end;

procedure TaMainForm.AcShowOutputExecute(Sender: TObject);
begin
  DM.Sendoutput();
end;

procedure TaMainForm.Panel1Resize(Sender: TObject);
begin
  if CmdTree.Visible and LVars.Visible then
    CmdTree.Height := round((Panel1.Height / 2) -1);
end;

procedure TaMainForm.AcBrowse1Execute(Sender: TObject);
begin
  InternalRunCommand('browse');
end;

procedure TaMainForm.LvarsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_INSERT then
    LVars.Selected[LVars.ItemIndex] := true;
  if {(Key = VK_SPACE) or }(Key = VK_INSERT) then
    if LVars.ItemIndex < LVars.Items.Count-1 then
      LVars.ItemIndex := Lvars.ItemIndex + 1;

end;

procedure TaMainForm.ToolButton11MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Screen.Cursor = crHourGlass) and Indicator.State then
    Screen.Cursor := crDefault;

{    (X >= ToolButton11.Left) and( X <= ToolButton11.Left+ToolButton11.Width) and
    (Y >= ToolButton11.Top) and (Y <= ToolButton11.Top+ToolButton11.Height) then
    StatusBar.Panels[0].Text := 'Classname: ' + Sender.ClassName +
                              ' X: ' + IntToStr(X) + ' Y: ' + IntTOStr(Y);
}
end;

procedure TaMainForm.ToolBar1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Screen.Cursor = crDefault) and Indicator.State then
    Screen.Cursor := crHourGlass;
end;

procedure TaMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    // Page_Down, Page_Up:
    VK_PRIOR, VK_NEXT:
       IF (AMAINFORM.Viewer.Focused or aMainform.CmdEdit.Focused) THEN
       Viewer.KeyDown(key, shift);
    VK_F4: FindFocus(cmdedit);//cmdedit.SetFocus;
    VK_F2: AcShowWindowCommandsLeft(Self);
    VK_F3: AcShowVariablesLeft(self);
    VK_F7: acSHowHistory(self);
  end;
  HAlignPanel1();
end;

procedure TaMainForm.EditFindExecute(Sender: TObject);
var
  FindStr, ReplStr: string;
  SearchOpt: TFrSearchOptions;
  ShowOpt: TFrShowOptions;
begin
  SearchOpt := [];
  if (aMainForm.ActiveControl = CmdEdit) then
    FindStr := cmdedit.GetWords(1, 1);
  ShowOpt := [frMatchCase, frBackwards, frLocalWindow..frCommands];
  if ShowSearchDlg(FindStr, REplStr, SearchOpt, 2, ShowOpt) = fraFind then
    SpecialSearch(Findstr, SearchOpt);
end;

procedure TaMainForm.SpecialSearch(FindStr: string; SearchOpt: TFrSearchOptions);
begin
  FindString := findstr;
  SearchOptions := SearchOPt;
  if frStartpage in SearchOpt then
    if pos('start.htm', viewer.CurrentFile) = 0 then
      InternalRunCommand('view "' + extractfilepath(application.exename)+ '\docs\' +
             OTranslator.Translate(105,'en') + '\start.htm"');
  if (frCommands in SearchOpt) or (frHowTo in SearchOpt) then
  begin
    if (frHowTo in SearchOpt) then
      ShowHelpForm(FindStr, 'howto')
    else
      ShowHelpForm(FindStr, 'commands');
  end;
  if (frLocalWindow in SearchOpt) and (Viewer.CurrentFile <> '') then
  begin
    ActiveControl := Viewer;
    AcCancelExecute(nil);
  end;
  Viewer.findex(FindStr, frMatchCase in SearchOpt, frBackwards in SearchOpt);
end;


procedure TaMainForm.EditRepeatFindExecute(Sender: TObject);
begin
  if SearchOptions = [] then exit;
  SpecialSearch(FindString, SearchOptions);
end;

procedure TaMainForm.AcRunKwallisExecute(Sender: TObject);
begin
  DoDlg('kwallis varlist');
end;

procedure TaMainForm.AcRunCorrelateExecute(Sender: TObject);
begin
  DoDlg('correlate varlist');
end;

procedure TaMainForm.AcCommandPromptExecute(Sender: TObject);
var
  visible: boolean;
  s: string;
begin
  // CommandPromptOnOff2.ImageIndex := -1;
  if cmdpanel.visible then s := 'OFF' else  s := 'ON';
    { else
     begin
       s := 'ON';
       CommandPromptOnOff2.ImageIndex := 109;
     end;}
  dm.SetOptionValue('DISPLAY COMMAND PROMPT', s);
end;



procedure TaMainform.HistoryInsert(delete: boolean = true);
var
    //key: word;
    opt: TEpiOption;
    i : integer;
begin
  if (dm.GetOptionValue('DISPLAY COMMAND PROMPT', opt) and (opt.Value = 'OFF')) then exit;
  cmdedit.ClearSelection;
  if HistoryList.ItemIndex > -1  then
    cmdedit.text := HistoryList.Items[HistoryList.ItemIndex];
  FindFocus(cmdedit);
  cmdedit.SelLength := 0;
  cmdedit.SelStart  := Length(Cmdedit.text);
  HalignPanel1();
end;

procedure TaMainForm.HistorylistKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_RETURN,VK_SPACE,VK_INSERT : HistoryInsert();
    VK_TAB,VK_F4: FindFocus(cmdedit);//Cmdedit.setfocus;
    81,88,114,117, 67,VK_ESCAPE:
        begin
          FindFocus(cmdedit);//Cmdedit.setfocus;
          if key = 88 then
             begin
             Historylist.TabStop := False;
             HistoryList.Visible := False;
             end;
          HalignPanel1();
        end;
  end;
end;


procedure TaMainForm.HistorylistDblClick(Sender: TObject);
var
 cmd: string;
begin
  HistoryInsert();
  if Historylist.ItemIndex > -1 then
    cmd := Historylist.Items[Historylist.ItemIndex];
  InternalRunCommand(cmd);
  CmdEdit.Clear;
end;

procedure TaMainForm.HAlignPanel1();
 // Show right side panels correctly
var
 i, offset, h:integer;
begin
  i := 0;
  if Lvars.visible then i:= i + 1;
  if CmdTree.visible then i:= i + 1;
  if Historylist.visible then i:= i + 1;

  if i = 0 then
     begin
          panel1.Visible := False;
          splitter2.Visible := false;
          FindFocus(cmdedit);//cmdedit.setfocus;
          exit;
     end
  else
  begin
    // placement:
    if i = 1 then
      begin
        if CmdTree.visible     then CmdTree.Align := alClient;
        if Lvars.visible       then Lvars.Align := alClient;
        if Historylist.visible then Historylist.Align := alClient;
      end
    else if i = 2 then
          begin
            if not historylist.visible then
               begin Lvars.Align := alBottom; CmdTree.Align := alTop; end
            else if not CmdTree.visible then
               begin Lvars.Align := alTop; HistoryList.Align := alBottom; end
            else if not Lvars.visible then
               begin Historylist.Align := alBottom; CmdTree.Align := alTop; end;
         end
    else     // all three visible
        begin
            CmdTree.Align := alTop;
            Lvars.Align := alClient;
            Historylist.Align := alBottom;
        end;
    // height
    if i > 1 then offset := 1 else offset := 0;
    h := round(Panel1.Height/i)-offset;
    if (lvars.visible) then
                  Lvars.height := h;
    if (CmdTree.visible) then
                  CmdTree.height := h;
    if (Historylist.visible) then
                  Historylist.height := h;
    end;
 end;

procedure TaMainForm.ClearCommandHistory1Click(Sender: TObject);
begin
   historylist.Clear;
   CmdEdit.History.Clear;
   CmdEdit.Text := '      ';
   CmdEdit.History.Clear;
   CmdEdit.CurrentCmd := 1;
   CmdEdit.History.add('Clh');
   CmdEdit.HackHistory('Clh', 'clh    // command history cleared');
end;

procedure TaMainForm.AcGetVersionExecute(Sender: TObject);
var
  s: string;
begin
  s := dm.Version();
  if s <> '' then
    CheckVersion1.Caption := 'Latest public rel.: ' + s;
end;

procedure TaMainForm.HistAcSaveExecute(Sender: TObject);
begin
  InternalRunCommand('savepgm');
end;

procedure TaMainForm.HistAcEditExecute(Sender: TObject);
begin
  ShowEditor('', 1, true);
end;

procedure TaMainForm.HistAcClearExecute(Sender: TObject);
begin
  cmdedit.History.Clear;
  Acshowhist.Execute();
end;


procedure TaMainForm.AcSearchEpiDataListExecute(Sender: TObject);
begin
  dm.info('"Search EpiData-list" attempts to open Internet Connection to EpiData-list Archives', [], 26003);
  ShellExecute(0, nil, pchar('http://lists.umanitoba.ca/pipermail/epidata-list/'), nil, nil, SW_SHOWNORMAL);
end;


procedure TaMainForm.PDFintroductionClick(Sender: TObject);
var
  error: integer;
  fn, lang: string;
  opt: TEpiOption;
begin
  if sender = PDFintroduction then fn := 'EA_Intro_'
     else  fn := 'EA_quick_';

  if DM.GetOptionValue('LANGUAGE', opt) then
    lang := AnsiLowerCase(opt.Value);

  fn := extractfilepath(application.exename) + 'docs' + PathDelim + OTranslator.Translate(105,'en')  + PathDelim + fn +
        OTranslator.Translate(105,'en') + '.pdf' ;

  if not FileExists(fn) then
    dm.Info('Introduction files not found for language: %s', [lang], 26004)
    else error := ShellExecute(0, 'open', pchar(fn), nil, nil, SW_SHOWNORMAL);
end;

procedure TaMainForm.HistAcClipboardExecute(Sender: TObject);
var
  Data: THandle;
  DataPtr: Pointer;
  Buffer: string;
  i, size: Integer;
begin
  size := 0;
  buffer := Historylist.Items[0];
  for i := 1 to Historylist.Items.Count -1 do
    Buffer := Buffer + #13 + Historylist.Items[i];
  size := length(buffer);
  EmptyClipboard;
  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, size+1);
  try
    DataPtr := GlobalLock(Data);
    try
      Move(PChar(Buffer)^, DataPtr^, size+1);
      Clipboard.SetAsHandle(CF_TEXT, Data);
    finally
      GlobalUnlock(Data);
    end;
  except
    GlobalFree(Data);
    raise;
  end;
end;

procedure TaMainForm.wkToolBarClick(Sender: TObject);
begin
  if wktoolbar.Visible then wktoolbar.visible := false
    else wktoolbar.Visible := True;
  //dm.info(OTranslator.Translate(2001,'Turn off in help menu/setup')); //dm.SetOptionValue('DISPLAY WORKTOOLBAR', 'OFF');
end;

procedure TaMainForm.AcTableDlgExecute(Sender: TObject);
begin
      DoTableDlg('Tables varlist');
end;

procedure TaMainForm.CopyImageToClipboardClick(Sender: TObject);
begin
  clipboard.Assign(TImageobj(FoundObject).Bitmap);
end;

procedure TaMainForm.CopyTableToClipboardClick(Sender: TObject);
begin
  CopyHTMLToClipBoard(string(FoundObject));
end;

procedure TaMainForm.FormDestroy(Sender: TObject);
begin
  DeInitializeUnits();
end;

procedure TaMainForm.AcShowresultvariablesExecute(Sender: TObject);
begin
  InternalRunCommand('result')
end;

procedure TaMainForm.AcOptionExecute(Sender: TObject);
begin
  InternalRunCommand('set')
end;

procedure TaMainForm.AcFileStructureExecute(Sender: TObject);
begin
  InternalRunCommand('var')
end;

procedure TaMainForm.AcDefaultWindowingExecute(Sender: TObject);
const
  def: TFormDefaults = (Section: 'Main';
                        Top: 1; Left: 1;
                        Width: 700; Height: 600;
                        Maximize: false);
begin
  OInifile.SaveForm('Main',1,1,700,600,False);
  OInifile.SaveForm('Editor',20,300,500,500,False);
  OInifile.SaveForm('Browse',100,430,500,600,False);
  OInifile.SaveForm('Help',25,430,500,600,False);
  Oinifile.LoadForm(self,def);
end;


procedure TaMainForm.AcClearResultVariablesExecute(Sender: TObject);
begin
  InternalRunCommand('result clear')
end;

procedure TaMainForm.AcFileCloseExecute(Sender: TObject);
begin
  InternalRunCommand('close');
end;


procedure TaMainForm.AcbrowserhideExecute(Sender: TObject);
var s : string;
  opt : TepiOption;
begin
// show toogle menu icons:
   browserhide.ImageIndex := -1;
   if dm.GetOptionValue('DISPLAY DATABROWSER', opt) and (opt.Value = 'ON') then
      dm.SetOptionValue('DISPLAY DATABROWSER', 'OFF')
      else
      begin
        dm.SetOptionValue('DISPLAY DATABROWSER', 'ON');
        if dm.dataframe = Nil then dm.Info('Data Browser will be shown when you read data', [], 26005);
        browserhide.ImageIndex := 109;
      end;
end;

procedure TaMainForm.acwktoolbaractExecute(Sender: TObject);
begin
         WKTOOOLBARClick(amainform.ViewerBtn);
end;

procedure TaMainForm.HistorylistMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    HistoryInsert();
end;

procedure TaMainForm.HistorylistExit(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Historylist.Count - 1 do
    Historylist.Selected[i] := false;
end;

procedure TaMainForm.AcImportFromClipboardExecute(Sender: TObject);
begin
  doCommand('Read /CB');
end;

procedure TaMainForm.AcRunLifeTableExecute(Sender: TObject);
begin
  DoDlg('lifetable varlist');
end;

procedure TaMainForm.AcShowSPCMenuExecute(Sender: TObject);
var
  sysdir, loadfile, lang: string;
  opt: TEpiOption;

  function test(path: string): boolean;
  begin
    result := false;
    if (loadfile = '') and fileexists(path) then
      result := true;
  end;

begin
  sysdir := ExtractFilePath(Application.ExeName) + 'docs' + PathDelim;
  if dm.GetOptionValue('LANGUAGE', opt) then
    if AnsiUpperCase(opt.Value) = 'ENGLISH' then
      lang := 'en'
    else
      lang := OTranslator.Translate(105,'en');

  loadfile := '';

  // notice that as soon as loadfile is <> '', then the test(      )   will fail.
  // 1. search in {sysdir}\docs\{language}\xxxx.htm[l] for the language specific extended file: (e.g. read.htm og read.html)
  if test(sysdir + lang + PathDelim + 'spc.htm') then
    loadfile := sysdir + lang + PathDelim + 'spc.htm';

  if test(sysdir + lang + PathDelim + 'spc.htm') then
    loadfile := sysdir + lang + PathDelim + 'spc.html';

  // 2.search in {sysdir}\docs\en\ xxxx.htm[l]  for the file.
  if test(sysdir + 'en' + PathDelim + 'spc.htm') then
    loadfile := sysdir + 'en' + PathDelim + 'spc.htm';
  if test(sysdir + 'en' + PathDelim + 'spc.htm') then
    loadfile := sysdir + 'en' + PathDelim + 'spc.htm';

  ViewHelpForm(Loadfile);
end;

procedure TaMainForm.AcRunScatterExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'SCATTER';
    Opts.Title := 'Scatter';
    Opts.VarCount := 4;
    Opts.Defaults := [goBY];
    Opts.BoxTypes[0] := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    Opts.BoxTypes[1] := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    Opts.BoxLabels[2] := 'Y Variable (optional)';
    Opts.BoxTypes[2] := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    Opts.BoxLabels[3] := 'Y Variable (optional)';
    Opts.BoxTypes[3] := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    Opts.ByTypes := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('Scatter varlist',1,10, [EpiTyFloat,EpiTyInteger,EpiTyDate],
//             [EpiTyFloat,EpiTyInteger,EpiTyDate], [GrpStdOpt],True);
end;

procedure TaMainForm.AcRunLineExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'LINE';
    Opts.Title := 'Line';
    Opts.VarCount := 4;
    Opts.Defaults := [goBY];
    Opts.BoxTypes[0] := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    Opts.BoxTypes[1] := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    Opts.BoxLabels[2] := 'Y Variable (optional)';
    Opts.BoxTypes[2] := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    Opts.BoxLabels[3] := 'Y Variable (optional)';
    Opts.BoxTypes[3] := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    Opts.ByTypes := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
{  DoGraphDlg('Line varlist', 1, 10, [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyString, EpiTyByte],
              [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyString, EpiTyByte],
              [GrpStdOpt],true);}
end;

procedure TaMainForm.AcRunHistogramExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'HISTOGRAM';
    Opts.Title := 'Histogram';
    Opts.VarCount := 1;
    Opts.Defaults := [goBY];
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyUppercase, EpiTyString, EpiTyByte];
    Opts.ByTypes := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
{  DoGraphDlg('Histogram varlist', 1, 0,
              [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyUppercase, EpiTyString, EpiTyByte],
              [],
              [GrpStdOpt],True);   }
end;

procedure TaMainForm.AcRunBarExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'BAR';
    Opts.Title := 'Bar';
    Opts.VarCount := 1;
    Opts.Defaults := [goBY];
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyUppercase, EpiTyString, EpiTyByte];
    Opts.ByTypes := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
{  DoGraphDlg('Bar varlist', 1, 0, [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyString, EpiTyByte],
              [],
              [GrpStdOpt],True);    }
end;

procedure TaMainForm.AcRunBoxExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'BOXPLOT';
    Opts.Title := 'Box Plot';
    Opts.VarCount := 1;
    Opts.Defaults := [goBY];
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.ByTypes := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
{  DoGraphDlg('Box varlist', 1, 0, [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyString, EpiTyByte],
              [],
              [GrpStdOpt],true); }
end;

procedure TaMainForm.AcRunDotPlotExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'DOTPLOT';
    Opts.Title := 'Dot Plot';
    Opts.VarCount := 1;
    Opts.Defaults := [goBY];
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.ByTypes := [EpiTyDate,EpiTyFloat,EpiTyInteger];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('Dotplot varlist',1,0, [EpiTyFloat,EpiTyInteger],
//             [], [GrpStdOpt],True);
end;

procedure TaMainForm.AcRunCumulativePlotExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'CDFPLOT';
    Opts.Title := 'Cumulative Plot';
    Opts.VarCount := 1;
    Opts.Defaults := [goBY];
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.ByTypes := [EpiTyInteger, EpiTyFloat];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//      DoGraphDlg('CdfPlot varlist',1,0, [EpiTyFloat,EpiTyInteger],
//             [], [GrpStdOpt],True);
end;

procedure TaMainForm.AcRunProbitPlotExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'CDFPLOT';
    OPts.CmdOptions := '/P';
    Opts.Title := 'Probit Plot';
    Opts.VarCount := 1;
    Opts.Defaults := [goBY];
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.ByTypes := [EpiTyInteger, EpiTyFloat];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('ProbitPlot varlist',1,0, [EpiTyFloat,EpiTyInteger],
//             [], [GrpStdOpt],True);
end;

procedure TaMainForm.AcRunCIPlotExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'CIPLOT';
    Opts.Title := 'Proportional Plot (CI Plot)';
    Opts.VarCount := 4;
    Opts.BoxLabels[0] := 'Outcome';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[1] := 'Variable';
    Opts.BoxTypes[1] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[2] := 'Variable (optional)';
    Opts.BoxTypes[2] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[3] := 'Variable (optional)';
    Opts.BoxTypes[3] := [EpiTyInteger, EpiTyFloat];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
{  DoGraphDlg('CIplot varlist',1,4, [EpiTyFloat,EpiTyInteger,EpiTyDate,EpiTyByte],
             [EpiTyFloat,EpiTyInteger,EpiTyBoolean,EpiTyByte], [GrpStdOpt],
             false,False,'Choose Variable','Choose Variable (optional)','Choose Variable (optional)','Choose Variable (optional)',
             'Outcome (Not optional)');}
end;

procedure TaMainForm.AcRunEpicurveExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'EPICURVE';
    Opts.Title := 'Epidemic Curve';
    Opts.VarCount := 2;
    Opts.Defaults := [goBY];
    Opts.BoxLabels[0] := 'Outcome';
    Opts.BoxTypes[0] := [EpiTyFloat,EpiTyInteger,EpiTyBoolean,EpiTyByte];
    Opts.BoxLabels[1] := 'Time variable';
    Opts.BoxTypes[1] := [EpiTyDate,EpiTyFloat,EpiTyInteger,EpiTyBoolean,EpiTyByte];
    Opts.ByTypes := [EpiTyFloat,EpiTyInteger];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('EpiCurve varlist',1,1, [EpiTyFloat,EpiTyInteger,EpiTyDate,EpiTyByte],
//             [EpiTyFloat,EpiTyInteger,EpiTyBoolean,EpiTyDate,EpiTyByte], [GrpStdOpt],True);
end;

procedure TaMainForm.AcRunKMPlotExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'LIFETABLE';
    Opts.CmdOptions := '/NOLT';
    Opts.Title := 'Kaplan-Meier Plot';
    Opts.VarCount := 3;
    Opts.Defaults := [goBY, goWeight];
    Opts.BoxLabels[0] := 'Outcome';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat, EpiTyDate, EpiTyByte, EpiTyBoolean];
    Opts.BoxLabels[1] := 'Time total (or time start)';
    Opts.BoxTypes[1] := [EpiTyInteger, EpiTyFloat, EpiTyDate, EpiTyByte];
    Opts.BoxLabels[2] := 'Time end (optional)';
    Opts.BoxTypes[2] := [EpiTyInteger, EpiTyFloat, EpiTyDate, EpiTyByte];
    Opts.ByTypes := [EpiTyFloat, EpiTyInteger, EpiTyByte, EpiTyBoolean];
    Opts.WeightTypes := [EpiTyFloat, EpiTyInteger, EpiTyByte, EpiTyBoolean];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('LIFETABLE varlist /NOLT', 1, 2, [EpiTyInteger, EpiTyFloat, EpiTyDate, EpiTyByte], [EpiTyInteger, EpiTyFloat, EpiTyDate, EpiTyByte],
//             [GrpStdOpt], true, true);
end;

procedure TaMainForm.AcRunPieExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'PIE';
    Opts.Title := 'Pie';
    Opts.VarCount := 1;
    Opts.Defaults := [];
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyString, EpiTyByte];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('Pie varlist', 1, 0, [EpiTyInteger, EpiTyDate, EpiTyFloat, EpiTyBoolean, EpiTyUppercase, EpiTyString, EpiTyByte],
//              [],
//              [GrpStdOpt]);
end;

procedure TaMainForm.AcRunIChartExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'ICHART';
    Opts.Title := 'IChart';
    Opts.VarCount := 2;
    Opts.Defaults := [goSPC, goTest];
    Opts.BoxLabels[0] := 'Measurement';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[1] := 'X axis (optional)';
    Opts.BoxTypes[1] := [EpiTyInteger, EpiTyFloat, EpiTyString, EpiTyDate];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('IChart varlist', 1, 1, [EpiTyInteger, EpiTyFloat],
//              [EpiTyInteger, EpiTyString, EpiTyDate, EpiTyFloat], [GrpSpcOpt, GrpStdOpt]);
end;

procedure TaMainForm.AcRunPChartExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'PCHART';
    Opts.Title := 'PChart';
    Opts.VarCount := 3;
    Opts.Defaults := [goSPC, goTest];
    Opts.BoxLabels[0] := 'Count';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[1] := 'Total';
    Opts.BoxTypes[1] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[2] := 'X axis (optional)';
    Opts.BoxTypes[2] := [EpiTyInteger, EpiTyFloat, EpiTyString, EpiTyDate];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('PChart varlist', 1, 2, [EpiTyInteger, EpiTyFloat],
//             [EpiTyInteger, EpiTyString, EpiTyDate, EpiTyFloat], [GrpSpcOpt, GrpStdOpt]);
end;

procedure TaMainForm.AcRunRunChartExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'RUNCHART';
    Opts.Title := 'RunChart';
    Opts.VarCount := 2;
    Opts.Defaults := [goSPC, goTest];
    Opts.BoxLabels[0] := 'Measurement';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[1] := 'X axis (optional)';
    Opts.BoxTypes[1] := [EpiTyInteger, EpiTyFloat, EpiTyString, EpiTyDate];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('RunChart varlist', 1, 1, [EpiTyInteger, EpiTyFloat],
//             [EpiTyInteger, EpiTyString, EpiTyDate, EpiTyFloat], [GrpSpcOpt, GrpStdOpt]);
end;

procedure TaMainForm.AcRunUChartExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'UCHART';
    Opts.Title := 'UChart';
    Opts.VarCount := 3;
    Opts.Defaults := [goSPC, goTest];
    Opts.BoxLabels[0] := 'Count';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[1] := 'Volume/Total';
    Opts.BoxTypes[1] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[2] := 'X axis (optional)';
    Opts.BoxTypes[2] := [EpiTyInteger, EpiTyFloat, EpiTyString, EpiTyDate];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('UChart varlist', 1, 2, [EpiTyInteger, EpiTyFloat],
//              [EpiTyInteger, EpiTyString, EpiTyDate, EpiTyFloat], [GrpSpcOpt, GrpStdOpt]);
end;

procedure TaMainForm.AcRunCChartExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'CCHART';
    Opts.Title := 'CChart';
    Opts.VarCount := 2;
    Opts.Defaults := [goSPC, goTest];
    Opts.BoxLabels[0] := 'Count';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[1] := 'X axis (optional)';
    Opts.BoxTypes[1] := [EpiTyInteger, EpiTyFloat, EpiTyString, EpiTyDate];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
//  DoGraphDlg('CChart varlist', 1, 1, [EpiTyInteger, EpiTyFloat],
//              [EpiTyInteger, EpiTyString, EpiTyDate, EpiTyFloat], [GrpSpcOpt, GrpStdOpt]);
end;

procedure TaMainForm.AcRunGChartExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'GCHART';
    Opts.Title := 'GChart';
    Opts.VarCount := 1;
    Opts.Defaults := [goSPC];
    Opts.BoxLabels[0] := 'Date/Sequence #';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat, EpiTyDate];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
end;

procedure TaMainForm.AcRunParetoExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'PARETO';
    Opts.Title := 'Pareto';
    Opts.VarCount := 1;
    Opts.Defaults := [goWeight];
    Opts.BoxLabels[0] := 'Category';
    Opts.BoxTypes[0] := [EpiTyString,EpiTyFloat,EpiTyInteger];
    Opts.WeightTypes := [EpiTyFloat,EpiTyInteger];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
end;

procedure TaMainForm.AcRunXbarChartExecute(Sender: TObject);
var
  Opts: TGraphDlgOptions;
begin
  Opts := nil;
  try
    if Sender is TGraphDlgOptions then
      Opts := TGraphDlgOptions(Sender)
    else
      Opts := TGraphDlgOptions.Create();
    Opts.Cmd := 'Xbar';
    Opts.Title := 'Xbar Chart';
    Opts.VarCount := 3;
    Opts.Defaults := [goSPC, goTest];
    Opts.BoxLabels[0] := 'Measurement';
    Opts.BoxTypes[0] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[1] := 'Time/Sequence';
    Opts.BoxTypes[1] := [EpiTyInteger, EpiTyFloat];
    Opts.BoxLabels[2] := 'X axis (optional)';
    Opts.BoxTypes[2] := [EpiTyFloat, EpiTyInteger, EpiTyString, EpiTyDate];
    DoGraphDlg(Opts);
  finally
    if Assigned(Opts) then FreeAndNil(Opts);
  end;
end;



end.

