unit UCmdProcessor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Uoutput,
  ansDataTypes,Uframes, UVectors, UvectorOp,ActnList,UEpidataTypes,UAnaToken,
  UanaParser, AApasTok, uepifile, Umatrix,UstringConst,UCommands, uformats,
  {AgOpenDialog,} UTranslation, UMerge {,FavOpenDialog, oDialogEx, AgOpenDialog} ;

const
   EpiOpenFile=100;
   EpiCloseFile=101;
   EpiLoadPGM=105;
   EpiSavePGM=106;
   epiEditfile=107;
   epiLoadHTMLfile=108;
   epiLoadTextfile=109;
   EpiVectorListChanged=110;
   EpiRefreshOutput=130;
   EpiRefreshSelect=150;
   EpiRefreshDir=160;
   EpiCls=131;
   EpiAddShortCut=200;
   EpiViewerFontName=300;
   EpiViewerFontSize=301;
   EpiUpdateVisual=302;
   EpiViewerFontCharset=303;
   EpiLanguage=304;
   EpiEngineError=801;
   EpiQuit=999;

type
  TInterfaceHook=procedure(MSg:TMessage) of object;

  TDM = class(TDataModule)
    OD: TOpenDialog;
    SD: TSaveDialog;
    procedure DMCreate(Sender: TObject);
    procedure DMDestroy(Sender: TObject);
  private
    ana       : TAnaExecutor;
    fdataframe : TEpiDataFrame;
    fInterfaceHook: TInterfaceHook;
    fRunHandle: integer;
//    fOutPutStream: TMemoryStream;
    fCancelled: boolean;
    fOptions: TStringList;
    fShortCuts: TStringList;
    fCurrentDir: string;
    fFloatFormat : string;
    fShowError: boolean;
    fShowTiming: Boolean;
    fShowSysInfo: Boolean;
    fShowInfo: boolean;
//    fShowWarning: boolean;
    fShowCommand: boolean;
    fShowResult: boolean;
//    fShowGraph : boolean;
    fSavePng : Boolean;
    foldShowResult: boolean;
    loop,EmitGap :integer;
    FGetOutputStream: TStream;
    Fgettest: string;
    FFileName: string;
    fSaveOuput: boolean;
//    FTranslator: TTranslator;
    function getOpenFileCount: word;
    function SetupOutput{(aForm : TForm)}:boolean;
//    procedure SetOutPutStream(const Value: TMemoryStream);
    procedure OnParseError(const msg: string; Token: TSMToken; var Handled: boolean);
    procedure showExecoutput(const output:string);
    function initGlobalSysVar:boolean;
    function initDataFrameVars(df: TEpiDataFrame): boolean;
    function DeinitDataFrameVars(df: TEpiDataFrame): boolean;
    function InternalOpenFile(var fn: string; var DataFrame: TEpidataFrame;const pw:string=''): boolean;
    function initOptions: boolean;
    function handleSetKey(const op, val: string): boolean;
    function UpdateSettings(const op, val: string): boolean;

    function CloseOut: boolean;
    function GetFloatFormat: string;
    procedure SetFloatFormat(const Value: string);

    function OutCorrelate(Varnames: TStrings; df: TEpiDataFrame; const CorMatrix: XMatrix): boolean;

    function CheckVariableNo(VarNames: TStrings; Min: word; Max: word = 0): boolean;
//    function CheckVariableNo(VarNames: TStrings; No: integer): boolean;
    function SetVarFormatToDefault(pDataFrame: TEpidataFrame;force:boolean): boolean;
    procedure SetOutputStream(const Value: TStream);
    procedure Setgettest(const Value: string);
    function GetOutputStream: TStream;
    function Expandlist(items, expandeditems: TStringlist): boolean;
    function GetFullFilename(const path: string): string;
    procedure PrintHeader(const msg: string);
    procedure SetFileName(const Value: string);
    function RunTestPGMFile(const fn: string; var testrec: TTestRecordArray; cmd: TCommand): boolean;
    procedure Setdataframe(const Value: TEpiDataFrame);
    function CreateNewDataSetFromResults(fn: string; varnames: array of string): TEpiDataFrame;
    function PopulateFldfromVars(const fldname, varname: String): boolean;
    function InternalCloseFile(var aDataFrame: TepiDataframe): boolean;
    function SetAllEchoSwitches(value: boolean): boolean;
    function Lang(code:integer; origtext:string):string;
    procedure InternalInfo(const msg: string);
  public
    OutputList: TStatOutputList;
    CodeMaker:THTMLMaker;
    function IsMissingOption(cmd: TCommand): boolean;
    function NotifyInterface(pMsg: cardinal; pWparam, pLParam: integer): integer;
    function AddResult(const pName: string; pDataType: integer; Val: Variant; pLength, pDecimals: integer): boolean;
    function Sendoutput: boolean;
    function SetOptionValue(const op, val: string; Updating: boolean = false): string;
    function GetOptionValue(const op: string;  var opt: TEpiOption): boolean;
    function CheckDataOpen(attempt:boolean=true;cmdid : integer=1): boolean;
    function runInitFile(const fn: string=''): boolean;
    function initShortCuts: boolean;
    function AddshortCut(const key, cmd: string): boolean;
    function ExecShortCut(sc: TShortCut): boolean;
    function Quit(CommandID: Cardinal): boolean;
    function OpenFile(fn: string; cmd: TCommand): boolean;
    function WriteFile(fn: string;Varnames: TStrings): boolean;
    function SaveDataFile(fn: string; VarNames: Tstrings; df: TEpiDataframe;cmd: TCommand): boolean;
    function DoSaveDataFile(fn: string; VarNames: Tstrings;cmd: TCommand): boolean;
    function CloseFile: boolean;
    function Enable(const off: boolean): boolean;
    function ViewFile(const fn: string): boolean;
    function ViewHelpfile(const fn: string): boolean;
    function EditFile(const fn: string): boolean;
    function LogOpen(const fn: string; cmd: TCommand):boolean;
    function RunCommndsList(const script: string): boolean;
    function RunPGMFile(const fn: string): boolean;
    procedure RunCommandLine(const cmd: string);
    function AppendFile(fn:string; cmd: TCommand): boolean;
    function Merge(Filename: string; Varnames: TStrings; Cmd: TCommand): boolean;
    function ShowTextFile(const fn: string; cmd: TCommand): boolean;
    function doPGMFileAction(fn: string; cmd: TCommand): boolean;
    function copyfile(const old,new:string; cmd: TCommand):boolean;
    function doDOSFileAction(fn: string; action: word): boolean;
    function doLogFileAction(fn:string; cmd: TCommand):boolean;
    function recode(Cmd: TCommand; const op,dstvar, srcvar: string;  RecodeList: Tstrings): boolean;
    function count(cmd: TCommand): boolean;
//    function Browse: boolean;
    procedure UpdateBrowse(CmdID: word);
    function Browse2(varnames:TStrings; cmd: TCommand): boolean;
    function UpdateData(varnames:TStrings; cmd: TCommand): boolean;
    function List(varnames:TStrings; cmd: TCommand) : boolean;
    function DoSet(varnames: TStrings): boolean;
    function variables(cmd:TCommand):boolean;
    function ParseMacroString(const s:String; var align:THAlignment):string;
    function EchoType(fn:string; action: word; cmd: TCommand = nil):boolean;
    function Writeln(const s:string;ForeColor:Tcolor=clyellow;align:THAlignment=caLeftJustify):boolean;
    function Writeln2(const s: string; oClass: String=''): boolean;
    function WriteDirect(const s: string): boolean;
    function Cls: boolean;
    function CLh: Boolean;
    function Version: string;
    function Doif(Exp: IValue; const tstr, fstr: string): boolean;
    function select(Exp:IValue):boolean;
    function Describe(VarNames : TStrings; cmd: TCommand):boolean;
    function Bar(VarNames: TStrings; cmd: TCommand): boolean;
    function Line(VarNames: TStrings; cmd: TCommand): boolean;
    function Pie(VarNames: TStrings; cmd: TCommand): boolean;
    function BoxPlot(VarNames: TStrings; Cmd: TCommand): boolean;
    function Scatter(VarNames: TStrings; cmd: TCommand): boolean;
    function IChart(Varnames: TStrings; cmd: TCommand): boolean;
    function XChart(Varnames: TStrings; cmd: TCommand): boolean;
    function PChart(Varnames: TStrings; cmd: TCommand): boolean;
    function EpiCurve(Varnames: TStrings; cmd: TCommand): boolean;
    function CIPlot(Varnames: TStrings; cmd: TCommand): boolean;
    function CumDistPlot(Varnames: TStrings; cmd: TCommand): boolean;
    function DotPlot(Varnames: TStrings; cmd: TCommand): boolean;
    function ParetoPlot(Varnames: TStrings; cmd: TCommand): boolean;
    function LifeTable(Varnames: TStrings; cmd: TCommand): boolean;
    function DoLabelValue(Varnames: TStrings; cmd: TCommand): boolean;
    function DoLabel(Varname, Varlabel: String): boolean;
    function LabelData(LabelName: string): boolean;
    function Aggregate(ByVars: TStrings;Cmd: TCommand): boolean;
    function Stattables(Varnames: TStrings;Cmd: TCommand): boolean;
    function PreTables(Varnames: TStrings; cmd : Tcommand): boolean;
    function ErasePng(cmd: TCommand): boolean;
    function Means(Varnames: TStrings; cmd: TCommand): boolean;
    function regress(Varnames: TStrings): boolean;
    function Correlate(Varnames: TStrings): boolean;
    function KWallis(Varnames: TStrings; cmd: TCommand): boolean;
    function Sort(VarNames : TStrings):boolean;
    procedure Memory;
    function Eval(Exp: IValue): boolean;
    function Assert(Exp: IValue): boolean;
    function Let(const pVarname:String;Exp: IValue): boolean;
    function Doloop(ploopObj: TObject): boolean;
    function Define(pVarDesc : TAnaVariableDescriptor):boolean;
    function GenField(dataframe: TEpiDataFrame; pVarDesc: TAnaVariableDescriptor; Exp: IValue): boolean;
    function GenVar(pVarDesc: TAnaVariableDescriptor; Exp: IValue): boolean;
    function generate(rows:integer):boolean;
    function help(HelpText: String): boolean;
    procedure CloseHelp();
    procedure PrintResult(const msg: string);
    procedure Cancel;
    procedure CommandReset;
    function EvalExpression(const Expression: String): IValue;
    function EvalAsBoolean(Exp: IValue): boolean;
    function PreCommand(cmd : TCommand): boolean;
    function PostCommand(Exp: IValue): boolean;
    function SetVarFormat(pDataFrame:TEpidataFrame;const VectorName:string;const fmt:string):boolean;
    function CheckforBreak:boolean;
    function SaveResult(fn:string;varnames: array of string):boolean;
    property OutputStream : TStream read GetOutputStream write SetOutputStream;
    property OpenFileCount:word read getOpenFileCount;
    property Dataframe : TEpiDataFrame read fdataframe write Setdataframe;
//    property OutPutStream : TMemoryStream read fOutPutStream write SetOutPutStream;
    property InterfaceHook:TInterfaceHook read fInterfaceHook write fInterfaceHook;
    property Cancelled : boolean read fCancelled write fCancelled;
    property Executor :  TAnaExecutor read ana;
    property Options: TStringList read fOptions;
    property ShortCuts :TStringList read fShortCuts;
    property CurrentDir :string read fCurrentDir write fCurrentDir;
    property FloatFormat : string read GetFloatFormat write SetFloatFormat;

//    property ShowWarning: boolean read fShowWarning write fShowWarning;
//    property Showtiming: boolean read fShowTiming write fShowTiming;
    property ShowError: boolean read fShowError write fShowError;
    property Showsysinfo: boolean read fshowsysinfo write fshowsysinfo;

//    property ShowGraph: boolean read fShowGraph write fShowGraph;

    property ShowInfo: boolean read fShowInfo write fShowInfo;
    property ShowCommand: boolean read fShowCommand write fShowCommand;
    property ShowResult: boolean read fShowResult write fShowResult;

    property SaveOuput : boolean read fSaveOuput write fSaveOuput;
    property OldShowResult: boolean read foldShowResult write foldShowResult;
//    property SavePng: boolean read fSavePng write fSavePng;
    property FileName :string read FFileName write SetFileName;
    procedure Error(const msg: string; const Args: array of const; translate: Integer = 0; severity: integer= 0);
    procedure PrintCommand(const msg: string);
    procedure Info(const msg: string; translate: Integer = 0); overload;
    procedure Info(const msg: string; const Args: array of const; translate: Integer = 0); overload;
    procedure Sysinfo(const msg: string);
    function  outfmt(var size: EpiFloat):string;
//    property  Translator:TTranslator read FTranslator write FTranslator;
  end;

function StartEngine:TDM;

var
  DM: TDM;

implementation

uses menus,SMUtils,{UFrameDS,{ubrowse,}ubrowse2,uDateUtils,UVectorVar,UFileDir,UOSUtils,UStatfunctions,
     EpiInfoSTATS, Cstrings, cFileUtils, EpdServices, InvokeRegistry, Rio,
     SOAPHTTPClient, UDebug, UGraph, UpdateBrowse,
     // Seperated analysis units: (Added june 2004, Torsten Christiansen)
     UTables, UContinous, UAggregate, Umain, uabout, UVariables,
     Udocument, ULinearRegression, UDos, Math, StrUtils, Editor, UHelp,
     DateUtils, GeneralUtils, ULifeTables;

{$R *.DFM}

const
 MAXBUFSIZE=1024*8;
 UnitName = 'UCmdProcessor';




function StartEngine:TDM;
begin
  if dm = nil then
     dm:= TDM.Create(application);
//  dm.Translator:=NIL;
  result:=dm;
end;

function TDM.CheckDataOpen(attempt:boolean=true; cmdID : integer = 1): boolean;
var s : string;
begin
  result:=False  ;
  if (not Assigned(dataframe)) and attempt then
  begin
    Openfile('', nil);
    if not (dataframe=nil) then
      if aMainForm.CmdEdit.History.Strings[aMainForm.CmdEdit.History.Count-2] = 'Read '
        then aMainForm.CmdEdit.History.Strings[aMainForm.CmdEdit.History.Count-2] := 'Read "' + dataframe.FileName +'"'
        else aMainForm.CmdEdit.History.Insert(aMainForm.CmdEdit.History.Count-1,'Read "' + dataframe.FileName + '"');
  end;
  if not Assigned(dataframe) then
    dm.info('No data', [], 103005)
  else
    result:=True
end;

function TDM.CheckVariableNo(VarNames: TStrings; Min: word; Max: word = 0): boolean;
var
 varco : integer;
begin
  if Varnames=nil then
    varco:=0
  else
    varco :=Varnames.count;
  if (Min or Max) = 0 then
    if varco> 0 then error('Variables are not allowed', [], 103001);
  if (Min = Max) then
    if varco <> Min then error('%d variable(s) expected', [Min], 103002);
  if varco < Min then error('%d or more variables are expected',[Min], 103003);
  if Max <> 0 then
    if varco > Max then error('Maximum %d variables are allowed',[max], 103004);
end;

function TDM.AddResult(const pName: string;pDataType:integer; Val:Variant; pLength, pDecimals: integer):boolean;
var
  avar : TAnaVariableDescriptor;
begin
  aVar :=TAnaVariableDescriptor.CreateResult(pName,pDataType, pLength, pDecimals);
  aVar.DefaultValue:= val;
  Executor.newvar(aVar);
  FreeAndNil(AVar);
end;


function  tdm.outfmt(var size: EpiFloat):string;
begin
  result := '%6.2f';
  size := abs(size);
  if (size > 5000.0) and (size < 10000.0)
       then  result := '%8.1f'
     else if (size > 9999.0)
       then  result := '%14.1f'
     else if (size = 0.000000000000) or (size = 1.000000000000)
       then result :='%3.1f'
     else if (size > 0.1) and  (size < 1.0)
       then  result := '%6.3f'
     else if (size > 0.0001) and  (size < 0.11)
       then  result := '%8.4f'
    else if (size < 0.00011)
       then  result := '%16.8f';
end;

function TDM.PreCommand(cmd : TCommand): boolean;
var
 Param   : IValue;
 s: string;
begin
   EmitGap:=-1;
   loop   :=0;
   cancelled :=false;
   if cmd.CommandId in EpiClearResultCmds then
   begin
      Executor.ClearVars(EpiVarResult);
   end;
   Executor.ClearVars(EpiVarSystem);
   SaveOuput := cmd.Output;
   if Assigned(dataframe) then
   begin
     s := aMainForm.StatusBar.Panels[3].Text;
     if (Pos('All', s) > 0) then s := '';

     if (cmd.ParameterCount> 0) then
     begin
       Param :=cmd.ParamByName['TEMPIF'];
       if param<> nil then
       begin
         dataFrame.backupSelector;
         dataframe.ApplySelection(Param.AsIValue,true);
         if (cmd.CommandId in EpiShowSelectCmds) then
           begin
             if length(s) > 0 then s := s + ' and ';
             s := s + '(' + Executor.LastSelect + ')'; // {Info('IF applied (%s)', [Executor.LastSelect], 203047);}
           end
       end;
     end;
     If ((cmd.CommandId in EpiShowSelectCmds) and (length(s) > 0)) then
       Info('Select: %s', [s], 203046);
   end;
end;

function TDM.PostCommand(Exp: IValue): boolean;
begin
 if (dataframe<>nil) then
 begin
  dataFrame.RestoreSelector;
 // browse takes away name of file.
 end;
  sysutils.DecimalSeparator := EpiDecSeparator;
  sysutils.DateSeparator  := EpiDateSeparator;

//  ShowResult:=oldShowResult;
  SaveOuput:=false;
// cancelled :=false;
end;

function TDM.count(cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
  s: Tstrings;
begin
//  s := TStringList.Create;	{ construct the list object }
  s := nil;
  try    { use the string list }
       if not CheckDataOpen() then exit; //CheckDataOpen();
    df := dataframe.prepareDataframe(s, nil);
//    FreeAndNil(df);
    if cmd.ParamByName['Q'] = nil then
      PrintResult('Count :  '+ inttostr(df.SelectedRowCount));
    AddResult('$count',EpiTyInteger,df.SelectedRowCount,0,0);
  finally
    FreeAndNil(s);
    FreeAndNil(df);
  end;
end;

{function TDM.Browse: boolean;
var
  ds : TSMFrameDataSet;
begin
  checkdataopen();
  ds :=nil;
try
  ds := TSMFrameDataSet.create(nil);
  ds.DataFrame  := dataframe;
  ds.readonly:=true;
  ds.Active :=true;
 // showBrowse(ds);
    dataFrame.RestoreSelector;
finally
  ds.free;
end;
end;
}

{**************************************
  Should only be called when refreshing
  the browse window!
  MUST NEVER be called with Update
  command.
  *************************************}
procedure TDM.UpdateBrowse(CmdID: word);
var
  df: TEpiDataframe;
  varnames: TStrings;
  //opt : TEpiOption;
begin
  // Dataframe might NOT be initialized at this point. Just exit
  if dataframe = nil then exit;
  if (cmdid in [opSelect, opLet, opSort]) then
    varnames := OUpdate.CurrentVarnames
  else begin
    varnames := TStringList.Create();
    dataframe.Vectors.GetVectorNames(varnames);
  end;

  df := dataframe.prepareDataframe(varnames, nil);
  if not OUpdate.UpdateBrowseWindow(df)then
    FreeAndNil(df);
end;

function TDM.Browse2(varnames:TStrings; cmd: TCommand): boolean;
var
 Vectorlist :TEpiVectors;
 DF: TEpiDataframe;
 VectorID: string;
 opt: TEpiOption;

begin
  df := nil;
  if not dm.CheckDataOpen(True) then exit;
  result := false;
  try
    if (dataframe.SelectedRowCount = 0) then
    begin
      OUpdate.CloseBrowse();
      dm.error('No data', [], 103005);
    end;
    df := dataframe.prepareDataframe(varnames, nil);
    Vectorlist:= df.GetVectorListByName(varnames);

    if (cmd.CommandID in [opRead, opGenerate]) then
    begin
      if dm.GetOptionValue('DISPLAY DATABROWSER', opt) and (AnsiUpperCAse(opt.Value) = 'ON') then
        OUpdate.CreateBrowse(df)
      else
        // DF is not used so free it otherwise we have a memory leak (it's the
        // prepared Dataframe from above).
        if Assigned(df) then FreeAndNil(df);
    end else begin
      OUpdate.CreateBrowse(df);
    end;

    result := true;
  finally
  end;
end;

function TDM.UpdateData(varnames:TStrings; Cmd: Tcommand): boolean;
var
 Vectorlist :TEpiVectors;
 DF: TEpiDataframe;
 VectorID: string;
 opt: TEpiOption;
 didbrowse: boolean;

begin
  CheckDataOpen();
  try
    VectorList := nil;
    if (DataFrame.SelectorBacked) or (DataFrame.RowCount <> DataFrame.SelectedRowCount) then
      dm.Error('Select/If not allowed with update', [], 103052);
    if Assigned(Cmd.ParamByName['ID']) then
    begin
      VectorID := Cmd.ParamByName['ID'].AsString;
      if not Assigned(Dataframe.FindVector(VectorID)) then
      begin
        dm.info('Field %s not found. Using "recnumber" as ID label.', [VectorID], 203045);
        VectorID := 'recnumber';
      end;
    end else
      VectorID := 'recnumber';
    if VectorID = 'recnumber' then
      dm.Info('Recnumber depends on sorting', [], 203002);
    Vectorlist := dataframe.GetVectorListByName(varnames);
    // Release the form if showing as browse window.

    OUpdate.CreateUpdate(dataframe, vectorlist, vectorid);
    dm.Info('Savedata to keep changes.', [], 203001);
  finally
    if Assigned(VectorList) then FreeAndNil(VectorList);
  end;
end;



function TDM.DoSet(varnames:TStrings): boolean;
var
 i : integer;
 SetTable: TStatTable;

begin
  if varnames.count=0 then
  begin
    SetTable := OutputList.NewTable(2,foptions.Count+1);
    SetTable.TableType := sttSystem;
    SetTable.Cell[1,1] := 'Option';
    SetTable.Cell[2,1] := 'Value';
    for i:=0 to foptions.Count-1 do
      begin
        SetTable.Cell[1,i+2] := foptions[i];
        SetTable.Cell[2,i+2] := AnsiUpperCase(TEpiOption(foptions.objects[i]).value);
      end;
    CodeMaker.OutputTable(SetTable, '');
    Sendoutput;
  end
  else
  for i:=0 to varnames.count-1 do
    SetOptionValue(Varnames.Names[i], Varnames.Values[Varnames.Names[i]]);
end;


function TDM.GetOptionValue(const op: string; var opt : TEpiOption): boolean;
var
 idx : integer;
begin
  result := false;
  opt := nil;
  if foptions <> nil then
  begin
    result := foptions.Find(op, idx);
    if not result then
       error('%s not supported', [op], 103007);
    opt := TEpioption(options.objects[idx]);
  end;
end;


function TDM.handleSetKey(const op,val:string): boolean;
var
 cmd : string;
begin
 result:=false;
 cmd := Sysutils.AnsiUpperCase(getvarname(op,1));
 if cmd <> 'KEY' then exit;
 cmd:= Sysutils.AnsiUpperCase(Sysutils.trim(copy(op,4,Maxint)));
 AddShortcut(cmd,val);
 result:=true;
end;

function TDM.SetAllEchoSwitches(value:boolean): boolean;
var
  val: string;
begin
  fShowCommand := value;
  fShowInfo    := value;
  fShowResult  := value;
  if value then val := 'ON' else val := 'OFF';
  SetOptionValue('SHOW COMMAND', val, true);
  SetOptionValue('SHOW INFO', val, true);
  setoptionValue('SHOW RESULT', val, true);
end;

function TDM.UpdateSettings(const op,val:string): boolean;
var
 cmd, cmd2 : string;
 i : integer;
 p : pchar;
 opt : TEpiOption;
 F: file;

begin
  result:=false;
  cmd := ansiuppercase(getvarname(op,1)); 
  if cmd='ECHO' then
    SetAllEchoSwitches(ansiuppercase(val)='ON')
  else if cmd = 'SHOW' then
  begin
    if ansiuppercase(op) = 'SHOW COMMAND' then
                  fShowCommand:=ansiuppercase(val)='ON'
    else if ansiuppercase(op) = 'SHOW ERROR' then
                  fShowError:=ansiuppercase(val)='ON'
    else if ansiuppercase(op) = 'SHOW INFO' then
                  fShowInfo:=ansiuppercase(val)='ON'
    else if ansiuppercase(op) = 'SHOW RESULT' then
                  ShowResult:=ansiuppercase(val)='ON'
    else if ansiuppercase(op) = 'SHOW SYSTEMINFO' then
                  fShowSysInfo:=ansiuppercase(val)='ON';
    SetOptionValue(op, ansiuppercase(val), true);
  end;

  if cmd='DEBUG' then
  begin
    ODebug.DebugLevel := StrToInt(val);
    result := true;
    exit;
  end;

  if cmd='DISPLAY' then
  begin
    cmd := RightStr(op, length(op)-8);
    NotifyInterface(EpiUpdateVisual,integer(pchar(AnsiUpperCase(cmd))),integer(pchar(AnsiUpperCase(val))));
  end;

  if cmd='WINDOW' then
  begin
    aMainForm.UpdateWindowFont(StrToInt(val));
  end;

  if (cmd='EDITOR') and (AnsiUpperCase(GetVarName(op, 8)) = 'FONT') then
  begin
    if Assigned(GetEditorHandle()) then
      GetEditorHandle().Memo1.Font.Size := StrToInt(val);
  end;

  if cmd='STYLE' then
  begin
  if AnsiUpperCase(GetVarName(op, 13)) = 'EXTERNAL' then
    CodeMaker.StartOutput
  else begin
    cmd :=Sysutils.trim(val);
    if cmd<>'' then
    begin
      cmd:=StrRemoveSurroundingQuotes(cmd);
    end;
    CodeMaker.CSSFileName:=cmd;
  end;
  end;

  if cmd = 'LANGUAGE' then
  begin
    NotifyInterface(EpiLanguage,integer(pchar(AnsiUpperCase(val))),0);
  end; 

  if cmd<> 'VIEWER' then exit;
  i := pos('FONT', AnsiUpperCase(op));
  if i = 0 then exit;
  cmd := AnsiUpperCase(getvarname(op,i+4));
  if cmd='NAME' then
    NotifyInterface(EpiViewerFontName,integer(pchar(AnsiUpperCase(val))),0)
  else if cmd='SIZE' then
    NotifyInterface(EpiViewerFontSize,strtointdef(val,-1),0)
  else if cmd='CHARSET' then
    NotifyInterface(EpiViewerFontCharset, integer(pchar(AnsiUpperCase(val))), 0);
  result:=true;
end;


function TDM.SetOptionValue(const op,val:string; Updating: boolean = false): string;
var
 idx,cod : integer;
 nval: EpiFloat;
 tval: string;
 opt : TEpiOption;
begin
  result:='';
  if handleSetKey(op,val) then exit;
  if not GetOptionValue(op,opt) then exit;
  case opt.oType of
  EpiTyBoolean:
  begin
    if (Sysutils.AnsiUpperCase(val)='OFF') or
       (Sysutils.AnsiUpperCase(val)='ON') then
       opt.Value :=val
    else
       error ('Invalid option value %s', [val], 103008);
  end;
  EpiTyInteger, EpiTyFloat:
  begin
    system.val(val,nval,cod);
    if cod> 0 then
       error ('Invalid option value %s', [val], 103008);
    opt.value:=val;
  end;
  EpiTyString,EpiTyUppercase:
  begin
    tval := StrRemoveSurroundingQuotes(val);
    opt.value := tval;
  end;
  end;//case
  if not updating then
  begin
    UpdateSettings(op,val);
    info('%s = %s', [op,val], 201002);
  end;
end;


//function TDM

function tdm.List(varnames:TStrings; cmd: TCommand) : boolean;
var
 i,j,k, rco, cco,srow,totlen, AppendLevel : integer;
 aLine,s,s1 : string;
 df: TEpiDataFrame;
 Vectorlist :TEpiVectors;
 Wlist : array of integer;
 BufPos: Integer;
 Buffer: array[0..MAXBUFSIZE] of Char;
 opt: TEpiOption;


  procedure AppendChars(Pp: PChar; Count: Integer);
  var
    N, sz: Integer;
  begin
    N := SizeOf(Buffer) - BufPos;
    if N > Count then N := Count;
    sz := strlen(Pp);
    if N <> 0 then Move(Pp[0], Buffer[BufPos], sz);
    Inc(BufPos, N);
  end;

  procedure AppendString(const S: string;aLen: integer);
  begin
   if S='' then
    AppendChars(cFiller, aLen)
   else
    AppendChars(Pointer(S), aLen)
  end;

begin
  if not dm.CheckDataOpen() then exit;  // dm.CheckDataOpen(false);CheckDataOpen();
  Vectorlist:=nil;
  df := nil;

  try
    df := dataframe.prepareDataframe(Varnames, nil);
    BufPos := 0;
    fillchar(buffer,sizeof(buffer),cFiller);
    //  p := nil;
    rco := df.RowCount;
    EmitGap:=2;
    // accomplish showing with national decimal and date separator settings.
    sysutils.DecimalSeparator := EpiDecSeparator;
    sysutils.DateSeparator  := EpiDateSeparator;;

    Vectorlist:= df.GetVectorListByName(varnames);
    cco := Vectorlist.Count;
    SetLength(Wlist,cco);
    totlen:=0;
    k := 0;
    if cmd.ParamByName['NO'] = nil then
    begin
      SetLength(Wlist,cco+1);
      k := 1;
      Wlist[0]:=  7;
      inc(totlen, wlist[0]);
    end;
    for j:= 0 to cco-1 do
    begin
      Wlist[j+k]:=  Vectorlist.Vectors[j].FieldDataSize+2;
      if length(Vectorlist.Vectors[j].Name )>  (Wlist[j+k]-2) then
        Wlist[j+k]:= length(Vectorlist.Vectors[j].Name)+5;
      inc(totlen,wlist[j+k]);
    end;
    if k = 1 then
      AppendString('RecNo',Wlist[0]);
    for j:= 0 to cco-1 do
      AppendString((Vectorlist.Vectors[j].Name),Wlist[j+k]);
    SetString(aline, Buffer, BufPos);

    PrintResult(aline);
    // Now list the actual data:
    for i:=1 to rco do
    begin
      if cancelled then
      begin
        cancelled :=false;
        info('Cancelled by user', [], 203003);
        break;
      end;
      if (not df.Selected[i]) then continue;
      BufPos := 0;
      fillchar(buffer,sizeof(buffer),cFiller);
      if k = 1 then
        AppendString(IntToStr(i),Wlist[0]);
      for j:= 0 to cco-1 do
        AppendString(Sysutils.trim(
                     Vectorlist.Vectors[j].GetValueLabel(
                       Vectorlist.Vectors[j].AsString[i], Cmd.ParameterList)
                     ), Wlist[j+k]);
      SetString(aline, Buffer, BufPos);
      PrintResult(aLine);
      //to coordinate with printResult, see printResult code
      if loop mod EmitGap=0 then
        application.ProcessMessages;
    end;
    Sendoutput;
  finally
    if Assigned(VectorList) then FreeAndNil(VectorList);
    if Assigned(df) then FreeAndNil(df);
    EmitGap := 1;
  end;
end;


function TDM.NotifyInterface(pMsg:cardinal; pWparam,pLParam: integer): integer;
var
  msg : TMessage;
begin
  if assigned(Interfacehook) then
  begin
     Msg.Msg:=pMsg;
     Msg.WParam :=pWparam;
     Msg.LParam :=pLParam;
     Interfacehook(Msg);
     application.ProcessMessages();
     result:=Msg.result;
  end;
end;

function TDM.Enable(const off: boolean): boolean;
begin
  aMainForm.AcFileClose.Enabled := off;    // TODO : add all other related to file open here
  aMainForm.FileSaveItem.Enabled := off;
  aMainform.Properties1.Enabled := off;
  aMainform.CloseBtn.Enabled := off;
  aMainform.acrunupdate.enabled := off;
  aMainform.acShowresultvariables.Enabled := off;
  aMainform.acClearresultvariables.Enabled := off;
end;

function TDM.CloseFile: boolean;
begin
  if (Assigned(fdataframe)) and (fdataframe.Modified) then
   dm.info('Closing data - Save history to recreate data with changes', [], 203004);
  Result:= InternalCloseFile(fDataFrame);
  OBrowse.CloseBrowse();
  OUpdate.CloseBrowse();
  // TODO : should remove resultvariable with name of file created on read of file:
  //  dm.AddResult()
end;

function TDM.InternalCloseFile(var aDataFrame: TepiDataframe): boolean;
var
  msg : TMessage;
begin
  Enable(not true);
  Result:=true;
  if adataframe=nil then
    aDataframe:=dataframe;
  if adataframe=nil then exit;
  DeinitDataFrameVars(adataframe);
  FreeAndNil(aDataFrame);
  NotifyInterface(EpiCloseFile,0,0);
  info('File closed', [], 203005);
end;


function TDM.help(HelpText: String): boolean;
begin
  ShowHelpForm(trim(helptext));
end;

procedure TDM.closehelp();
begin
  CloseHelpForm;
end;

function TDM.ShowTextFile(const fn:string; Cmd: TCommand):boolean;
var
 f        : TextFile;
 s,outs    : string;
begin
  if not fileexists(fn)
    then dm.info('File %s not found', [fn], 103013)
  else
  begin
    AssignFile( f, fn );
    Reset(f);
    readln( f, outs);        (*EpiData *)
    if (pos('<html',outs) > 0) or (pos('<!DOCTYPE',AnsiUppercase(copy(outs,1,20))) > 0) or (pos('<head>',outs) > 0)  then
     error('Complete html pages not allowed with show. Remove HTML header or : <font class=info>view %s </font>', [fn], 103009);
    while not eof(f) do
    begin
      readln( f, s);        (*EpiData *)
      outs := outs + s + '<br>';
      if length(outs) > 3000 then
      begin
        if cmd.ParamByName['CLASS'] <> nil then
          dm.Writeln2(outs, cmd.ParamByName['CLASS'].AsString)
        else
          dm.Writeln2(outs);
        outs:='';
      end;
    end; // while
    if length(outs) > 0 then
      if cmd.ParamByName['CLASS'] <> nil then
        dm.Writeln2(outs, cmd.ParamByName['CLASS'].AsString)
      else
        dm.Writeln2(outs);
    system.closefile(f);
  end;
end;

function TDM.EchoType(fn: string; action: word; cmd: TCommand = nil):boolean;
var
   align:THAlignment;
   s,r :string;
begin
  r := '';
  s := '';
  case action of
    opEcho: Info(fn, [], 0);
    opType:
    begin
      if (length(fn)>1) and (fn[1]='"') then
          fn:=StrRemoveSurroundingQuotes(fn);
      fn := ParseMacroString(fn, align);
      if cmd <> nil then
      begin
        if cmd.ParamByName['H5'] <> nil then r := 'h5';
        if cmd.ParamByName['H4'] <> nil then r := 'h4';
        if cmd.ParamByName['H3'] <> nil then r := 'h3';
        if cmd.ParamByName['H2'] <> nil then r := 'h2';
        if cmd.ParamByName['H1'] <> nil then r := 'h1';
        if cmd.ParamByName['CLASS'] <> nil then
          s := cmd.ParamByName['CLASS'].AsString;
      end;
      if (r <> '') then WriteDirect('<' + r + '>' + trim(fn) + '</' + r + '>')
      else writeln2(trim(fn), s);
    end;
    opTitle:
      WriteDirect('<h1>' + fn + '</h1>');
  end;//case
end;


function TDM.doPGMFileAction(fn:string; cmd: TCommand):boolean;
var
 i,d, co, totp, tota, tote, totl: integer;
 f : TEpiInfoDataset;
 t: DWord;
 currdir : string;
 testrec: TTestRecordArray;
 tab : TStatTable;
 totdp: EpiFloat;
 opt: TEpiOption;
 action: word;
begin
try
 if cmd = nil then
   action := opSavePgm
 else
   action := cmd.CommandID;
 case action of
 opSavePgm:
   begin
    if fn='' then
    begin
      sd.Filter := 'Program file|*.pgm|All|*.*';
      sd.FilterIndex := 1;
      sd.DefaultExt :='.pgm';
      if not sd.Execute then exit;
    end;
    if fn='' then
      fn := sd.filename;
    if fn='' then
     error('Missing file name', [], 101004);
    fn:=AdjustFileExt(fn,'.pgm');
    NotifyInterface(EpiSavePGM,integer(pchar(fn)),0);
    info('File name: %s saved', [fn], 203006);
   end;
 opshow: ShowTextFile(fn, cmd);
 opRun,opRunTest:
   begin
    if fn='' then
    begin
      od.FileName :='';
      od.Filter:= 'Program file|*.pgm|All|*.*';
      od.FilterIndex := 1;
      if not od.Execute then exit;
      fn := od.filename;
      SetCurrentDir(ExtractFileDir(fn)); //added build 118 - when remowing agopendialog      
    end;
    if fn='' then
     error('Missing file name', [], 101004);
    case action of
    opRun:
      begin
        RunPGMFile(fn);
  //      if GetOptionValue('OUTPUT FOLDER',opt) then
  //        SetCurrentDir(opt.Value);          // removed again due to problems  Oct 3rd 05 JL
      end;
    opRunTest:
      begin
        dm.cls;
        dm.UpdateSettings('ECHO', 'OFF');
        tota := 0;
        totp := 0;
        tote := 0;
        totdp := 2000;
        dm.AddResult('$assert_error', epityinteger, 0, 3, 0);
        dm.AddResult('$assert', epityinteger, 0, 3, 0);
        dm.AddResult('$dp', EpiTyFloat, 2000, 3, 9);
        dm.AddResult('$dpdif', epityfloat,1E-15, 3, 9);
        //locate curr dir
        CurrDir := getCurrentDir();
        try
          RunTestPGMFile(fn, testrec, cmd);
        finally
          ODos.CD(CurrDir);
        end;
        //return to previous curr dir
        tab:=OutputList.NewTable(7,high(testrec)+4);
        tab.TableType := sttSystem;
        tab.Caption := '<h3>' + GetBuildInfoAsString + ' ' + DateToStr(Date) + ' ' + TimeTostr(Time) +
                       '</h3><b>Summary table of pgm files:</b> ' + DateToStr(Date) + ' ' + TimeTostr(Time) + '<hr>';
        tab.cell[1,1]:='File' ;
        tab.cell[2,1]:='Asserts<br>Planned';
        tab.cell[3,1]:='<br>Found';
        tab.cell[4,1]:='<br>Errors';
        tab.Cell[5,1]:='Precision<br>digits';
        tab.cell[6,1]:='<br>abs.&nbsp;diff';
        tab.cell[7,1]:='Comments' ;
        for i := 0 to high(testrec) do
        begin
          tab.cell[1,i+2]:= testrec[i].FileName;
          tab.cell[2,i+2]:= trim(format('%d',[testrec[i].Planned]));
          tab.cell[3,i+2]:= trim(format('%d',[testrec[i].Asserts]));
          tab.cell[4,i+2]:= trim(format('%d',[testrec[i].Errors]));
          if (testrec[i].DigitPrecision > 0) and (testrec[i].DigitPrecision < 2000) and (dm.Executor.FindVar('$dp') <> nil)   then
          begin
            if testrec[i].DigitPrecision = 15 then
              begin
                tab.Cell[5,i+2] := '15';
                tab.cell[6,i+2] := '0.0';
              end
            else
              begin
              d := 14;
              while d > -5 do  // find precision
              begin
              if (testrec[i].dif < power(10,-d)) then
                  begin
                    if d > 0
                      then tab.Cell[5,i+2] := format('&lt; 10<sup>-%d</sup>',[d])
                      else tab.Cell[5,i+2] := format('%6.0f',[testrec[i].dif]);
                  d := -6;
                  end
              else
                  dec(d);
              end;
              tab.cell[6,i+2] := tab.Cell[5,i+2];
              tab.Cell[5,i+2] := trim(format('%5.0f',[testrec[i].DigitPrecision]));
             end;
          end else begin
            tab.Cell[5,i+2]:= '';
            tab.Cell[6,i+2]:= '';
          end;
          tab.cell[7,i+2]:=trim(format('%s',[testrec[i].Comment]));
          totp := totp + testrec[i].Planned;
          tota := tota + testrec[i].Asserts;
          if ((testrec[i].Planned - testrec[i].Asserts) <> 0) then
            tab.cell[7,i+2] := tab.cell[7,i+2] + '<br> <b>Failure: Count of asserts</b>';
          tote := tote + testrec[i].Errors;
          if (testrec[i].DigitPrecision <> 0) and (testrec[i].DigitPrecision < totdp) then totdp := testrec[i].DigitPrecision;
          if Testrec[i].Comment = '<b>Exception occurred!</b>' then
            for d := 3 to 6 do tab.cell[d,i+2] := '';
        end; // this pgm
        tab.cell[1,high(testrec)+3]:='Total' ;
        tab.cell[2,high(testrec)+3]:=trim(format('%d',[totp]));
        tab.cell[3,high(testrec)+3]:=trim(format('%d',[tota]));
        tab.cell[4,high(testrec)+3]:=trim(format('%d',[tote]));
        if totdp <> 2000 then tab.cell[5,high(testrec)+3]:=trim(format('%5.0f',[totdp]));
        if (tote > 0) then
          tab.cell[7,high(testrec)+3] := '<b>Assert errors exists!</b>';
        if (totp <> tota) then
          tab.cell[7,high(testrec)+3] := tab.cell[7,high(testrec)+3] + '<br><b>Planned asserts differs from actual assert!</b>';
        dm.cls;
        dm.codemaker.OutputTable(tab,'<hr><small>Abs dif: difference btw. expected and calculated parameter (if &lt; 10<sup>-14</sup> = 0.0)'
                                      + '<br>Digits precision(max: 15)</small>');
        dm.Sendoutput;
        dm.UpdateSettings('ECHO', 'ON');
        dm.UpdateSettings('SHOW SYSTEMINFO', 'OFF');
      end;
   end;
end;
end;  //case

except
  on E: Exception do
    error('PGM Error:<br>%s', [E.message], 103010);
end;
end;

function TDM.RunPGMFile(const fn:string):boolean;
var
 rc : integer;
 fn1,newdir: string;

begin
  //save echo setting
  Result:=false;
  fn1 := fn;
  if not fileexists(fn) then
      fn1 := fn + '.pgm';
  if not fileexists(fn1) then exit;
  aMainForm.CmdEdit.HackHistory('run', 'run ' + fn1);
//  SetCurrentDir(ExtractFilePath(fn));
//  NotifyInterface(EpiRefreshDir, integer(pchar(ExtractFilePath(fn))), 0);
  rc:=Executor.RunScript(fn1);
  case rc of
  //1: info('Script run successfully');
  0: error('script resulted in errors', [], 103011);
  2: info('Script interrupted by user', 203007);
  end;
  //restore echo setting:

  Result:=rc=1;
end;

function TDM.RunTestPGMFile(const fn: string; var testrec: TTestRecordArray; cmd: TCommand): boolean;
var
  rc : integer;
  FileScan: TmFileScan;
  path,ext, currdir, s : string;
  i,co, t : integer;
  Source: TStringList;
begin
  Result:=True;
  if (FileGetAttr(fn) and faDirectory = faDirectory) then
  begin
   if not directoryexists(fn) then
        error('Directory %s not found', [fn], 103012);
   try
     try
       FileScan:= TmFileScan.Create(self) ;
       FileScan.SubDirectories := false;
       FileScan.FilePath := IncludeTrailingPathDelimiter(fn)+'*.*'   ;
       FileScan.Start;
       co := FileScan.SearchResult.count;
       for i:= 0 to co-1 do
       begin
          RunTestPGMFile(IncludeTrailingPathDelimiter(fn)+FileScan.SearchResult[i], testrec, cmd);
          Application.ProcessMessages();
          if dm.Cancelled then
            exit;
       end;
     except
       Result:=false;
       raise;
     end;
   finally
     FileScan.free;
   end;
  end
  else   // run a single pgm file
  begin
    if not fileexists(fn) then
        error('File %s not found', [fn], 103013);
    if AnsiupperCase(extractfileext(fn))<>'.PGM' then exit;
    CurrDir := GetCurrentDir();
    SetCurrentDir(CurrDir + PathDelim + ExtractFilePath(fn));
    writedirect('<br><b>Running</b>: ' + fn );
    try
      Source := TStringList.Create;
      t := high(testrec) + 1;
      setlength(testrec, t + 1);
      testrec[t].FileName := fn;
      Source.LoadFromFile(ExtractFileName(fn));
      s := Source.Strings[0];
      if pos('*#', s) > 0 then
      begin
        testrec[t].Planned := StrToInt(copy(s, pos('*#', s)+2, pos('#*', s) - pos('*#', s)-2));
        delete(s, 1, pos('#*', s)+2);
      end;
      Testrec[t].Comment := s;

      // now run the file:
      Executor.RunScript(ExtractFileName(fn));
      //*sysinfo('Ended on '+ datetostr(now));

      if ana.FindVar('$assert') <> nil then
          testrec[t].Asserts := ana.FindVar('$assert').AsInteger;

     if ana.FindVar('$assert_error') <> nil then
          testrec[t].Errors := ana.FindVar('$assert_error').AsInteger;

     if ana.FindVar('$dp') <> nil then
         testrec[t].DigitPrecision := ana.FindVar('$dp').AsFloat;
    except
      Testrec[t].Comment := '<b>Exception occurred!</b>';
      if Assigned(cmd.ParamByName['HALT']) then
      begin
        Error('Failed in file: %s', [fn], 103014, -1);
        raise;
      end;
    end;
    FreeAndNil(Source);
    // finally - reset parameters for next file:
    dm.AddResult('$dpdif', epityfloat,1E-15, 3, 9);
    dm.AddResult('$dp', epityfloat, 2000, 3, 9);
    dm.AddResult('$assert', epityinteger, 0, 3, 0);
    dm.AddResult('$assert_error', epityinteger, 0, 3, 0);
    // and move back to previous dir - before current pgm file:
    SetCurrentDir(CurrDir);
  end;
  Result:=rc=1;
end;


function TDM.RunCommndsList(const script:string):boolean;
var
 rc : integer;
begin
Result:=false;
if script='' then exit;
rc:=Executor.RunScriptAsString(script);
case rc of
// 1: info('Script run successfully');
0: error('script resulted in errors', [], 103011);
2: info('Script interrupted by user', [], 203007);
end;
Result:=rc=1;
end;


function TDM.generate(rows: integer): boolean;
begin
  if OpenFileCount> 0 then
    error('No, data will be lost: Close first', [], 103015);
  try
    result := false;
    if rows<1 then
      error('Rows count must be at least 1', [], 103016);
    if dataframe <> nil then dataFrame.free;
    fdataframe := TEpiDataFrame.CreateTemp(rows);
    initDataFrameVars(fdataframe);
    dataframe.FileName := 'Generated dataset';
    result := true;
    except
      on E: Exception do
        error('Exception occured: %s', [E.message], 103017);
  end;
end;




function TDM.SetVarFormatToDefault(pDataFrame:TEpidataFrame; force:boolean):boolean;
var
   i, co : integer;
begin
  co := pDataframe.VectorCount;
  for i := 0 to co-1 do
    if force or (pdataframe.vectors[i].FieldFormat='') then
        SetVarFormat(pDataFrame, pdataframe.vectors[i].name, pdataframe.vectors[i].FieldFormat)
end;


function TDM.SetVarFormat(pDataFrame:TEpidataFrame;const VectorName:string;const fmt:string):boolean;
var
 sfmt : string;
 v : TEpiVector;
begin
   v:=pdataframe.VectorByName[VectorName];
   sfmt:=fmt;
//set default
   if sfmt='' then
   begin
   case v.DataType of
     EpiTyBoolean  :;
     EpiTyInteger  :sfmt :='%d';
     EpiTyFloat    :sfmt :=format('%%%d.%df',[v.FieldDataSize+v.FieldDataDecimals,v.FieldDataDecimals]);
     EpiTyDate     :
     begin
{        case v.FieldDataType of
        eftDate,eftTodayType:           fld.fieldformat:='%MDY';
        eftEuroDate,eftEuroToday:       fld.fieldformat:='%DMY';
        eftYMDDate,eftYMDToday:         fld.fieldformat:='%YMD';
        end;}
     end;
     EpiTyString,
     EpiTyUppercase   :sfmt :=format('%%%ds',[v.FieldDataSize]);
   end;
   end;
   v.FieldFormat:=sfmt;
end;


function TDM.InternalOpenFile(var fn:string;var DataFrame:TEpidataFrame;const pw:string=''):boolean;

var
 ext : string;
begin
  if fn='' then
    error('Missing file name', [], 101004);
  fn:= GetFullFilename(fn);
  ext :=Sysutils.trim(AnsiUpperCase(ExtractFileExt(fn)));
  if ext='' then
  begin
    ext:='.REC';
    fn:=fn+ext;
  end;
  if not (fn = '$CB.txt') then
  begin
    if not fileexists(fn) then
      Error('File %s not found', [fn], 103013)
    else
      fn:=Expandfilename(fn);
  end else begin
    fn := 'from clipboard';
  end;
  try
    info('Loading data %s, please wait...', [fn], 203008);
    if ext ='.REC' then
      dataframe := TEpiDataFrame.Create(TEpiInfoDataset,fn,0,pw)
    else if ext ='.DBF' then
      dataframe := TEpiDataFrame.Create(TEpiDBFdataset,fn,0)
    else if ((ext ='.TXT') or (ext ='.CSV')) then
      dataframe := TEpiDataFrame.Create(TEpiTXTdataset,fn,0)
    else
      error('Unknown file extension %s', [ext], 103018);
    if dataframe.Errorcount> 0 then
      info('Errors reading data', [], 203009);
    SetVarFormatToDefault(dataframe,false);
    enable(True);
    result:=true;
  except
//    on E: Exception do
//      error('Exception occured: %s', [E.message], 103017);
  end;
end;


function TDM.OpenFile(fn: string; cmd: TCommand): boolean;
var
 t: DWord;
 i, co, j, ro : integer;
 UserData: TEpiInfoCustomUserData;
 s,pw:string;
 opt: TEpiOption;

 function CF(cmd: tcommand): string;
 begin
   result := '';
   if cmd <> nil then
     if cmd.ParamByName['CLOSE'] <> nil then result := '/CLOSE';
 end;

begin
 result := false;
 pw:='';
 if Assigned(cmd) then
   if (Cmd.ParamByName['CLOSE'] <> nil) then dm.CloseFile();

 if (Assigned(dataframe)) and (not dataframe.Modified) then
   dm.CloseFile();

 if cmd<>nil then
   if cmd.ParamByName['KEY'] <> nil then pw:=cmd.ParamByName['KEY'].AsString;

 if OpenFileCount> 0 then
   error('Data modified - Close first', [], 103019);
 if Assigned(cmd) then
   if Assigned(Cmd.ParamByName['CB']) then
     fn := '$CB.txt';
 if (fn = '') then
 begin
   od.FileName:='';
   od.Filter:= EpiDataFilter;
   od.FilterIndex := 1;
   od.InitialDir := GetCurrentDir ;
   if not od.Execute then exit;
 end;
 if fn='' then
 begin
    fn := od.filename;
    SetCurrentDir(ExtractFileDir(fn));
    aMainForm.CmdEdit.HackHistory('Read', 'Read "' + fn + '" ' + CF(cmd));
 end;
 //aMainForm.Close11.Enabled := true;
 aMainForm.FileSaveItem.Enabled := true;
 try
   if Assigned(fdataframe) then FreeAndNil(fdataframe);
    t := GetTickCount;
    InternalOpenFile(fn, fdataframe, pw);
    t := GetTickCount-t;
    initDataFrameVars(fdataframe);
    NotifyInterface(EpiVectorListChanged,integer(dataframe),0);
    NotifyInterface(EpiOpenFile,integer(dataframe),0);
    s := lang(203010, 'File name :') + fn ;
    if dataframe.DataLAbel<> '' then
         s:= s + '<br>' + dataframe.DataLAbel;
    filename:=fn;
    s:= s + '<br>' + format(lang(203011,'Fields: %d   Total records: %d   Included: %d'),
         [dataframe.vectorcount,dataframe.RowCount,dataframe.SelectedRowCount]);
    info(s, [], 0);
    result := true;
  except
//    on E: Exception do
//      error('Exception occured: %s', [E.message], 103017);
  end;
end;


function TDM.AppendFile(fn:string; cmd: TCommand): boolean;
VAR
  co:Integer;
  AppendFrame :TEpidataFrame;
  opt:TEpiOption;
  OldSetDeleted:String;
  ExcludedFields,LackingFields,InCompatibleFields:string;
  s,PW : string;
begin
 if (OpenFileCount=0) or (dataframe=NIL) then
   error('No data', [], 103005);
 if fn='' then
 begin
   od.FileName:='';
   od.Filter:= EpiDataFilter;
   od.FilterIndex := 1;
   if not od.Execute then exit;
 end;
 if fn='' then
 begin
    fn := od.filename;
    aMainForm.CmdEdit.HackHistory('Append', 'Append "' + fn + '"');
 end;
 try
   AppendFrame:=nil;
   IF GetOptionValue('READ DELETED', opt) THEN OldSetDeleted:=opt.Value;

   if (cmd.ParamByName['KEY'] <> nil) then pw:=cmd.ParamByName['KEY'].AsString;
   //dm.Info(pw);

  InternalOpenFile(fn,AppendFrame,pw);

   co:=dataframe.RowCount;
   dataframe.AppendNewRows(AppendFrame,ExcludedFields,LackingFields,InCompatibleFields);
   s:= format(lang(203012,'Appended file:  Fields: %d   Total records: %d   Valid records: %d'),
       [AppendFrame.VectorCount,AppendFrame.RowCount,AppendFrame.SelectedRowCount]);
   if Appendframe.RowCount <> Appendframe.SelectedRowCount then
       s:= s + '<br>' + format(lang(203013, 'No of deselected records: %d'),[Appendframe.RowCount - Appendframe.SelectedRowCount]);
   if ExcludedFields<>'' THEN
       s:= s + '<br>' + format(lang(203014, 'Unknown fields excluded from append file: %s'),[ExcludedFields]);
   if InCompatibleFields<>'' THEN
       s:= s + '<br>' + format(lang(203015, 'Incompatible fields excluded: %s'),[InCompatibleFields]);
   if LackingFields<>'' THEN
       s:= s + '<br>' + format(lang(203016, 'Fields not found in append file: %s'),[LackingFields]);
     info(s + '<br>' + format(lang(203017, '<br>Combined file:  Fields: %d   Total records: %d'),[dataframe.VectorCount,dataframe.SelectedRowCount]), [], 0);
  finally
    AppendFrame.Free;
    IF GetOptionValue('READ DELETED', opt) THEN
    begin
      if OldSetDeleted<>opt.Value then SetOptionValue('READ DELETED',OldSetDeleted);
    end;
    NotifyInterface(EpiRefreshSelect,integer(pchar(Executor.LastSelect)),integer(dataframe));
  end;
end;

function TDM.Merge(Filename: string; Varnames: TStrings; Cmd: TCommand): boolean;
var
  RelateFrame: TEpiDataframe;
  Pw: string;
  opt: TEpiOption;
begin
   if not dm.CheckDataOpen() then exit; // CheckDataOpen();
  if Cmd.CommandID = opMerge then
    CheckVariableNo(Varnames, 1);
  Relateframe := nil;
  try
    if Trim(Filename) = '' then
      if not OD.Execute then exit
      else Filename := OD.FileName;
    InternalOpenFile(Filename, RelateFrame);
    if Cmd.CommandID = opMerge then
      OMerge.DoMerge(Dataframe, RelateFrame, Varnames, Cmd)
    else
      OMerge.DoAppend(Dataframe, RelateFrame, Varnames, Cmd);
    initDataFrameVars(fdataframe);
    NotifyInterface(EpiVectorListChanged, integer(Dataframe),0);
    NotifyInterface(EpiOpenFile, integer(Dataframe),0);
  finally
    if Assigned(RelateFrame) then FreeAndNil(RelateFrame);
  end;
end;

function TDM.DoSaveDataFile(fn: string; VarNames: Tstrings; cmd: TCommand): boolean;
var
  df: TEpiDataframe;
begin
  df := nil;
  if OpenFileCount= 0 then
    error('No data', [], 103005); //***
  try
    df := dataframe.prepareDataframe(Varnames, nil);
    result := SaveDataFile(fn, varnames, df, cmd);
  finally
    FreeAndNil(df);
  end;
end;

function TDM.SaveDataFile(fn: string;VarNames: Tstrings; df: TEpiDataframe; cmd: TCommand): boolean;
var
   s,pw    : string;
   co   : integer;
   Adataset :TEpiDataset;
   opt : TEpiOption;
   t: DWord;
   i : integer;
begin
  Adataset:=nil;
  Result:=false;
  try
    if df = nil then
      error('SaveDataFile: No dataframe!', [], 103020);
    if fn='' then
    begin
      sd.FileName:='';
      sd.Filter:= EpiDataFilter;
      sd.FilterIndex := 1;
      if not sd.Execute then Error('Missing file name', [], 101004);
    end;
    if fn='' then
    begin
      fn := sd.filename;
      aMainForm.CmdEdit.HackHistory('Savedata', 'Savedata "' + fn + '"');
    end;
    if df.FindVector(fn)<> nil then
      info('Filename %s is similar to a variable name',  [fn], 203018);
    fn:=Expandfilename(GetFullFilename(fn));
    fn:=AdjustFileExt(fn,'.rec');
    if Ansiuppercase(extractfileext(fn)) <>'.REC' then
      error('Must be .REC file', [], 103021);
    s := ChangeFileExt(fn,'.chk');
    if (FileExists(fn) or  FileExists(s)) and (cmd.ParamByName['REPLACE'] = nil) then
      error('DataFile and/or Chk file exists. <br> Add /REPLACE or erase<br> %s <br>%s', [fn,s], 103022);
    pw := '';
    if cmd<>nil then
    if cmd.ParamByName['KEY'] <> nil then pw:=cmd.ParamByName['KEY'].AsString;

    Info('Saving data to: %s', [fn], 203019);
    // t := GetTickcount;
    Adataset := Df.CreateNewDataSet(TEpiInfoDataset,fn,Varnames,pw);
    //Adataset := df.CreateNewDataSet(TEpiInfoDataset,fn,Varnames);
    Result := Df.SaveToDataSet(Adataset{,Varnames},{pw})=0;
    //  t := GetTickCount-t;
    info('%d Fields %d Records', [df.VectorCount,df.RowCount], 203020);  // was VectorCount-1
    //  sysinfo('Saved in : '+ floattostr(t/1000)+' seconds');
    Adataset.free;
    filename:=fn;
  except
    On E : Exception do
    begin
     Error('Exception occured: %s', [E.Message], 103017,-1);
     Error('Failed to save data to %s', [fn], 103023,-1);
     Adataset.free;
    end;
  end;
end;


function TDM.WriteFile(fn: string;Varnames: TStrings): boolean;
var
 i, co : integer;
 f : TEpiInfoDataset;
 Adataset :TEpiDataset;
 t: DWord;
begin
 Adataset:=nil;
 Result:=false;
 if OpenFileCount= 0 then
   error('No data', [], 103005); //***
 if fn='' then
   error('Missing file name', [], 101004);
 fn:=Expandfilename(GetFullFilename(fn));
 fn:=AdjustFileExt(fn,'.rec');
 info('Writing to file: %s, please wait...', [fn], 2030211);
 t := GetTickcount;
try
  if Ansiuppercase(extractfileext(fn))='.DBF' then
  begin
    info('Saving data in DBF format is not possible, data will be saved in REC format', [], 203022);
  end;
  Adataset:= dataframe.CreateNewDataSet(TEpiInfoDataset,fn,Varnames);
  Result:=dataframe.SaveToDataSet(Adataset{,Varnames})=0;
  Adataset.free;
  Adataset:=nil;
  filename:=fn;
  t := GetTickCount-t;
  info('File name : %s', [fn], 203023);
//  sysinfo('Saved in : '+ floattostr(t/1000)+' seconds');
except
  On E : Exception do
  begin
   Error('Exception occured: %s', [E.Message], 103017,-1);
   Error('Failed to save data to %s', [fn], 103023,-1);
   Adataset.free;
  end;
end;
end;


function TDM.ParseMacroString(const s: String;var align: THAlignment): string;
var
  i, len: integer;
  z ,idstr :string;
begin
   Result:='';
   z:= Executor.ExpandMacros(s);
   len := length(z);
   i:=1;
   while i <= len do
   begin
      if (z[i]='\') then
      begin
       if (i<len-1) and (z[i+1]='\') then
       begin
         result:=result+'\';
         inc(i,2);
       end
       else
       begin
         inc(i);
         case upcase(z[i]) of
         'N' : result:= result+ #13#10;
         'C' :align :=caCenter;
         'R' :align :=caRightJustify;
         'L' :align :=caLeftJustify;
         '0'..'9':
         begin
            idstr:='';
            while (i<=len) and IsNumeric(z[i]) do
               begin idstr:=idstr+z[i];inc(i);end;
            result:=result+char(strtointdef(idstr,0));
            dec(i);
         end;
        end;//case
         inc(i);
       end;
      end
      else
      begin
         result:=result+z[i];
         inc(i);
      end;
  end;
  result:=Sysutils.trim(result);
  if length(result)> 0 then
    result:=result+ #13#10;
end;



function TDM.Writeln(const s: string;ForeColor:Tcolor=clyellow;align:THAlignment=caLeftJustify): boolean;
var
  para : TStatPara;
begin
  try
     Para:=nil;
     para := outputList.newPara(s);
     para.options.FFontColor:=forecolor;
     para.options.FAlignment:=align;
     Codemaker.outputPara(Para);
     if loop > EmitGap then
     begin
        loop:=0;
        Sendoutput;
     end
     else inc(loop);
  finally
     Para.free;
  end;
end;

function TDM.Writeln2(const s: string;oClass:String=''): boolean;
var
  para : TStatPara;
begin
  try
     Para:=nil;
     para := outputList.newPara(s);
     para.options.fOClass := oClass;
     Codemaker.outputPara(Para);
     if loop > EmitGap then
     begin
        loop:=0;
        Sendoutput;
     end
     else inc(loop);
  finally
     Para.free;
  end;
end;

function TDM.WriteDirect(const s: string): boolean;
begin
  Codemaker.write(s + CRLF, s + CRLF);
  Sendoutput;
end;

function TDM.Sendoutput: boolean;
var
 st : TStream;
begin
//  if not ShowResult then exit;
  st := CodeMaker.GetStream;
  NotifyInterface(EpiRefreshOutput,integer(st),0);
end;


function TDM.Cls: boolean;
var
 st : TStream;
begin
  Codemaker.Clear;
  st := CodeMaker.GetStream;
  NotifyInterface(EpiRefreshOutput,integer(st),0);
end;

function TDM.Clh: boolean;
begin
 aMainForm.ClearCommandHistory1Click(Self);
end;

function TDM.Version(): string;
var
  version, release, build, infotext: widestring;
  Iepd: IepdservicesPortType;
  w1,w2,w3,w4 : word;
  v1,v2,v3,v4: word;
  new: boolean;
  s: string;
begin
  new := false;
  result := '';
  GetBuildInfo(w1,w2,w3,w4);
  s:= format('<br>Current version: %d.%d Release %d (Build %d)',[w1,w2,w3,w4]);
  try
    Iepd := GetIepdservicesPortType();
    Iepd.getEpiDataStatVersion(version,release,build,infotext);
    version := trim(version);
    v1 := StrToInt(copy(version, 0, pos('.', version)-1));
    v2 := StrToInt(copy(version, pos('.', version)+1, MaxInt));
    release := trim(release);
    v3 := Trunc(StrToFloat(release));
    build := trim(build);
    v4 := StrToInt(build);
    if w1 < v1 then
      new := true;
    if (w1 = v1) and (w2 < v2) then
      new := true;
    if (w1 = v1) and (w2 = v2) and (w3 < v3) then
      new := true;
    if w4 > strToInt(build) then new:= False;
    if new then
      s:= s + '<br>New public release is: ' +  format('%d.%d Release %d (Build %d)',[v1,v2,v3,v4])
            + '<br> Update from Http://www.epidata.dk'
    else begin
      s:= s + '<br>Latest public release '+  format('%d.%d Release %d (Build %d)',[v1,v2,v3,v4]) ;
      result := format('%d.%d Release %d (Build %d)',[v1,v2,v3,v4]);
    end;
    s := s + '<br>' + infotext;
  except
    on Exception do s:= s + '<br>No internet connection found'
  end;
    info(s, [], 0);
end;

function TDM.Quit(CommandID: Cardinal): boolean;
var
  fn: string;
  opt: TEpiOption;
  save: boolean;
begin
  save := false;
  if CodeMaker.LogFileName <> '' then
    CloseOut;
  fn := 'temp';
  if GetOptionValue('HISTORY NAME', opt) then
    fn := opt.Value;
  if GetOptionValue('OUTPUT FOLDER', opt) then
    fn := opt.Value + '\' + fn;
  try
    BackupFile(fn, 'pgm', 1);
  except
  end;
  aMainform.CmdEdit.History.Insert(0, '// ' + GetBuildInfoAsString + ' ' + DateToStr(Date) + ' ' + TimeTostr(Time));
  aMainform.CmdEdit.History.Delete(aMainform.CmdEdit.History.Count - 1); // This should delete the last entry, which would be the "QUIT" command.
  try
    doPGMFileAction(fn, nil);
    NotifyInterface(EpiQuit,0,0);
  except
    error('Click on close icon (X) to inforce quit', [], 103024,-1);
  end;
end;


function TDM.getOpenFileCount: word;
begin
  result:=0;
  if dataframe<> nil then inc(result);
end;



function TDM.Doif(Exp: IValue;const tstr,fstr:string): boolean;

  procedure DOcmd(const cmdline:String);
  var
   cmd :integer;
   i,co : integer;
  begin
    if not executor.Anaparser.IfableCommand(cmdline,cmd) then exit;
    if cmd in [oplet{,opidentifier}] then
      RunCommandLine(cmdline)
    else
    begin
      co :=dataframe.RowCount;
      for i:=1 to co do
      begin
        dataframe.RowNo := i;
        if dataframe.Selected[i] then
           RunCommandLine(cmdline)
      end;
    end;
  end;

begin
  if dataframe=nil then
     error('No data', [], 103005);
  try
    try
      Dataframe.backupSelector;
      Dataframe.ApplySelection(Exp,true);
      // Added Torsten Christiasen 19. june 2006:
      // Otherwise parsing "if (lege[1] = 0) then x = ...." where
      // 'x' is a global var, will result in running the THEN statement unintentionally 
      if dataframe.SelectedRowCount > 0 then
        DOcmd(tstr);
    except
      raise;
    end;
  finally
    Dataframe.RestoreSelector;
  end;

  if fstr<>'' then
  begin
    try
      try
        Dataframe.backupSelector;
        Dataframe.ApplySelection(Exp,false);
        if dataframe.SelectedRowCount > 0 then
          DOcmd(fstr);
      except
        raise;
      end;
    finally
      Dataframe.RestoreSelector;
    end;
  end;
end;


function TDM.select(Exp: IValue): boolean;
begin
  result := false;
  if dataframe=nil then
     error('No data', [], 103005);
  try
    //clear old selection on select ''
    if exp = nil then
     Dataframe.ApplySelection(nil,true)
    else
    //or apply new selection as well:
     Dataframe.ApplySelection(Exp,true);

    dataframe.RebuildRecnumber();
    if Dataframe.SelectedRowCount=0 then
     info('No records available', [], 203024);
  except
    Executor.LastSelect:='';
    raise;
  end;
  NotifyInterface(EpiRefreshSelect,integer(pchar(Executor.LastSelect)),integer(dataframe));
  result := true;
end;

function TDM.recode(Cmd: TCommand; const op,dstvar,srcvar:string;RecodeList:Tstrings): boolean;
var
 cmdline, labelline, sl,sh, intervaltxt:string;
 byval :integer;
 rec: TAnaReCodeData;
 df : TEpiDataframe;
 co,i,j :integer;
 dstV, srcV, tmpV :TEpiVector;
 opt: TEpiOption;
 maxval: epifloat;
 len: integer;
 byvars: TStringList;
 agglist: TAggrList;

  function CleanUpVector(const input: string): string;
  var
    i: integer;
  begin
    result := '';
    if trim(input) = '' then exit;
    i := pos(intervaltxt, trim(input))-2;
    if i < len then
      result := DupChar('0', len-i) + copy(input, 1, + length(intervaltxt)+2)
    else
      result := copy(input, 1, i + length(intervaltxt)+2);
    i := i + length(intervaltxt) + 3;
    if length(trim(input))-i < len then
      result := result + ' ' + DupChar('0', len-(length(trim(input))-i)-1) + copy(input, i, MAXINT)
    else
      result := result + ' ' + copy(input, i, MAXINT);
  end;

begin
  if dataframe=nil then
    error('No data', [], 103005);
  dstV := dataframe.VectorByName[dstvar];
  srcV := dataframe.VectorByName[srcvar];

  Df := nil;
  ByVars := nil;
  AggList := nil;

  try
    if op='RND' then
    begin
      Info('RND not implemented', [], 203025);
      // RND not implemented
    end
    else if op='BY'then
    begin
      if not (dstV.DataType in [EpiTyInteger, EpiTyFloat]) then
        Error('%s <br> Must be Integer: Define %s ##', [dstV.Name, dstV.Name], 103025);
      if GetOptionValue('RECODE INTERVAL TEXT', opt) then
        intervaltxt := opt.Value
      else
        intervaltxt := '-';
      rec := TAnaReCodeData(Recodelist.objects[0]);
      byval := rec.vlow;
      cmdline := format('%s = (%s DIV %d) * %d', [dstvar, srcvar, byval, byval]);
      runcommandline(cmdline);
      Byvars := TStringList.Create();
      Byvars.Add(dstvar);
      AggList := TAggrList.Create();
      AggList.Add(TAggrCount.Create('$N', '', acAll));
      df := OAggregate.AggregateDataframe(dataframe, byvars, AggList, Cmd);
      tmpV := df.FindVector(dstvar);
      cmdline := 'labelvalue ' + dstvar;
      for i := 1 to tmpV.Length do
        if not tmpV.IsMissing[i] then
          cmdline := cmdline + ' ' + format('/%d="%d - %d"',
                                     [tmpV.AsInteger[i], tmpV.AsInteger[i], (tmpV.AsInteger[i]+byval-1)]);
      if Cmd.ParamExists['CLEAR'] then
        cmdline := cmdline + ' /CLEAR';
      runcommandline(cmdline);

      // Now do some cleaning up since the output might be malformatted
      // eg. missing values are formated as "-" since expression-parser doesn't handle
      // this situation
      co := dataframe.RowCount;
      maxval := 0;
      for i := 1 to co do
      begin
        if not srcv.IsMissing[i] then
          maxval := Max(abs(srcv.AsFloat[i]), maxval);
        if srcv.IsMissing[i] then //TC: 22 sept. 2007
   //   if trim(dstv.AsString[i]) = intervaltxt then
          dstv.IsMissing[i] := true;
      end;
      {len := length(inttostr(floor(maxval)));
      for i := 1 to co do
        if not dstv.IsMissing[i] then
          dstv.AsString[i] := CleanUpVector(dstv.AsString[i]);}
    end
    else
    begin
      co:=RecodeList.count;
      labelline := 'labelvalue ' + dstV.Name;
      for i:=0 to co-1 do
      begin
        rec:=TAnaReCodeData(Recodelist.objects[i]);
        if rec.Operation='ELSE' then
        begin
          cmdline := format('%s=%s',[dstvar,rec.vcode]);
          runcommandline(cmdline);
          labelline := labelline + format(' /%s="Other"', [rec.vcode]);
          break;
        end;
      end;
      for i:=0 to co-1 do
      begin
        rec:=TAnaReCodeData(Recodelist.objects[i]);
        if rec.Operation='RANGE' then
        begin
          if (rec.vlow='LO') and (rec.vhigh='HI') then
             cmdline := format('if 1=1 then %s=%s',[dstvar,rec.vcode])
          else if (rec.vlow='LO') then
             cmdline := format('if (%s<=%s) then %s=%s',[srcvar,rec.vhigh,dstvar,rec.vcode])
          else if (rec.vhigh='HI') then
             cmdline := format('if (%s>=%s) then %s=%s', [srcvar, rec.vlow,dstvar,rec.vcode])
          else
            cmdline := format('if (%s>=%s) and (%s<=%s) then %s=%s',
                             [srcvar, rec.vlow, srcvar, rec.vhigh, dstvar, rec.vcode]);
          runcommandline(cmdline);
          labelline := labelline + format(' /%s="%s-%s"', [rec.vcode, rec.vlow, rec.vhigh]);
        end
        else if rec.Operation='EQUAL' then
        begin
          cmdline := format('if (%s=%s) then %s=%s',[srcvar, rec.vlow,dstvar,rec.vcode]);
          runcommandline(cmdline);
          labelline := labelline + format(' /%s=".%s"', [rec.vcode, rec.vlow]);
        end  else if rec.Operation='COMMA' then
        begin
          cmdline:='';
          labelline := labelline + format(' /%s="', [rec.vcode]);
          sl:=rec.vlow;
          j:=pos(',',sl);
          while j <> 0  do
          begin
            sh:=copy(sl,1,j-1);
            cmdline := cmdline + format(' OR (%s=%s) ',[srcvar, sh]);
            labelline := labelline + sh + ',';
            sl:=copy(sl,j+1,Maxint);
            j:=pos(',',sl);
          end;
          cmdline := cmdline + format(' OR (%s=%s) ',[srcvar, sl]);
          labelline := labelline + sl + '"';
          cmdline[pos('O',cmdline)]:='i';
          cmdline[pos('R',cmdline)]:='f';
          cmdline := cmdline + format(' then %s=%s',[dstvar,rec.vcode]);
          runcommandline(cmdline);
        end;
      end;
      if not (dstV.DataType in [EpiTyInteger, EpiTyFloat]) then
      begin
        dm.Info('Valuelabeling not posible. Destination %s must be of type integer.', [dstV.Name], 203026);
      end else begin
        if Cmd.ParamExists['CLEAR'] then
          labelline := labelline + ' /CLEAR';
        aMainForm.doCommand(labelline);
      end;
    end;
  finally
    if Assigned(Df) then FreeAndNil(Df);
    if Assigned(agglist) then FreeAndNil(agglist);
    if Assigned(byvars) then FreeAndNil(byvars);
  end;
end;


function TDM.EvalExpression(const Expression:String):IValue;
begin
  Result := Executor.EvalExpression(Expression);
end;



function TDM.Eval(Exp: IValue): boolean;
var
 t : DWord;
begin
  //t := getTickCount;
  if exp=nil then
  begin
    error('No expression', [], 103026);
  end
  else
  begin
     PrintResult(Exp.AsString);
  end;
  //t := getTickCount-t;
  //writeln('Expression evalution ended in '+ floattostr(t div 1000) +' seconds');
end;

function formatAssert:string;
var
  SL: TStringList;
begin
  sl := nil;
  Result:='';
  try
    try
      if Dm.Executor.fileName='' then exit;
      sl := tstringlist.Create;
      sl.LoadFromFile(dm.Executor.FileName);
      Result:=format(' in file  %s Line %d : %s',
                     [Dm.Executor.fileName,Dm.Executor.CurrentExecLine,
                      sl[Dm.Executor.CurrentExecLine-1]]);
    except
      Result:=format(' in file  %s . Check input lines',[Dm.Executor.fileName]);
    end;
  finally
    sl.Free;
  end;
end;

function TDM.Assert(Exp: IValue): boolean;
var
 t : DWord;
 err, ass, i: integer;
 check: boolean;
begin
  if dm.ana.FindVar('$assert') = nil then
    dm.AddResult('$assert', EpiTyInteger, 0, 3, 0);
  if dm.ana.FindVar('$assert_error') = nil then
    dm.AddResult('$assert_error', EpiTyInteger, 0, 3, 0);
  ass := dm.ana.FindVar('$assert').AsInteger;
  err := dm.ana.FindVar('$assert_error').AsInteger;
  if exp=nil then exit;
  if not exp.CanReadAs(ttBoolean) then
   error('No Boolean expression found in ASSERT command', [], 103027);
  if not Exp.AsBoolean then
  begin
    PrintResult('Assertion failed' + formatAssert);
    dm.AddResult('$assert_error', EpiTyInteger, err+1, 3, 0);
  end;
  dm.AddResult('$assert', EpiTyInteger, ass+1, 3, 0);
end;

function TDM.EvalAsBoolean(Exp: IValue): boolean;
var
 t : DWord;
begin
//writeln('Expression evalution started');
//t := getTickCount;
if exp=nil then exit;
Result:=Exp.AsBoolean;
//t := getTickCount-t;
//writeln('Expression evalution ended in '+ floattostr(t div 1000) +' seconds');
end;



function TDM.Doloop(ploopObj:TObject): boolean;
var
 t : DWord;
 co,i: integer;
 v : TEpiVector;
 gv : Ivalue;
 done : boolean;
 loopobj:TLoopCommand;
 s   : string;
 fexpandlist : TStringList;
begin
fexpandlist :=nil;
done:=false;
if ploopObj=nil then  error('Missing loop data', [], 103028);
loopobj:=TLoopCommand(ploopObj);
if loopobj.loopvarname='' then
   Error('Loop variable is missing', [], 103029);
{#TODO1: design function to return variable and type}
if dataframe <> nil then
begin
  v := DataFrame.FindVector(loopobj.loopvarname);
  if v<> nil then
        error('Data field %s cannot be used as loop variable', [loopobj.loopvarname], 103030);
end;
if not done then
begin
  gv :=Executor.globalvariables.VarbyName[loopobj.loopvarname];
  if gv <> nil then done:=true;
end;
if not done then
   error('Variable not found: %s', [loopobj.loopvarname], 103031);
co :=loopobj.Items.count;
case co of
0 :  error('There are no loop choices', [], 103032);
1 :
begin
   try
    fexpandlist := TStringList.create;
    if not Executor.Expandlist(loopobj.Items,fexpandlist) then
            Expandlist(loopobj.Items, fexpandlist);
    co := fexpandlist.count;
    for i:= 0 to co-1 do
    begin
        gv.Value :=fexpandlist[i] ;
        if gv.AsString='' then error('Item %d is invalid loop choice',[i], 103033);
        Executor.RunBlock(loopobj.CommandLines, nil, nil);
    end;
   finally
      fexpandlist.free;
   end;
end
else
begin
for i:= 0 to co-1 do
begin
    gv.Value :=loopobj.Items[i] ;
    if gv.AsString='' then error('Item %d is invalid loop choice',[i],103033);
    Executor.RunBlock(loopobj.CommandLines, nil, nil);
end;
end;
end;//case
end;

function TDM.Expandlist(items,expandeditems: TStringlist):boolean;
var
 Vectorlist :TEpiVectors;
 i, co : integer;
begin
 Vectorlist:=nil;
  if not dm.CheckDataOpen() then exit; CheckDataOpen();
try
  Vectorlist:= Dataframe.GetVectorListByName(items);
  expandeditems.clear;
  co := Vectorlist.count;
  for i:= 0 to  co-1 do
    expandeditems.Add(Vectorlist[i].name);
  Result:=true;
finally
  Vectorlist.free;
end;
end;



function TDM.Let(const pVarname: String; Exp: IValue): boolean;
var
 v : TEpiVector;
 gv : Ivalue;
 done : boolean;
begin
result := false;
done:=false;
if exp=nil then  error('No expression', [], 103026);
if dataframe <> nil then
begin
  v := DataFrame.FindVector(pVarName);
 if v<> nil then
  begin
   Dataframe.UpdateVector(v,Exp);
   done :=true;
  end
end;
if not done then
begin
  gv :=Executor.findvar(pVarname);
  if gv <> nil then
  begin
     case gv.tag of
     EpiVarGlobal: gv.value:= Exp.Value;
     EpiVarResult, EpiVarSystem: error('Result variables are read-only', [], 103034);
     end;
     done:=true;
  end;
end;
if not done then
   error('Variable not found: %s', [pVarName], 103031);
result := true;
end;


procedure TDM.Error(const msg: string; const Args: array of const; translate: Integer = 0; severity: integer = 0);
var
  nmsg: string;
begin
  if showError then
  begin
    nmsg := format(OTranslator.Translate(translate, msg), args);
    writeln2(nmsg, 'error');
  end;
  NotifyInterface(EpiEngineError,ana.CurrentExecLine,Severity);
  if severity > -1 then abort;
end;

procedure TDM.InternalInfo(const msg: string);
begin
  if not ShowInfo then exit;
  writeln2(msg, 'info');
end;

procedure TDM.Info(const msg: string; translate: Integer = 0);
var
  nmsg: string;
begin
  nmsg := OTranslator.Translate(translate, msg);
  InternalInfo(nmsg);
end;

procedure TDM.Info(const msg: string; const Args: array of const; translate: Integer = 0);
var
  nmsg: string;
begin
  nmsg := format(OTranslator.Translate(translate, msg), args);
  InternalInfo(nmsg);
end;

procedure TDM.SysInfo(const msg: string);
begin
  if showsysinfo then
    writeln2(msg,'sysinfo');
end;

procedure TDM.PrintCommand(const msg: string);
begin
  if showCommand then
    writeln2(msg,'command');
end;


procedure TDM.PrintResult(const msg: string);
begin
  if fShowResult then
    writeln2(msg,'result');
end;

procedure TDM.PrintHeader(const msg: string);
begin
  if fShowResult then
   writeln2(msg,'firstrow');
end;


procedure TDM.Memory;
var
 f,n ,i,w, tot : integer;
 ms :TMemoryStatus;

begin
  if not (dataframe=nil) then
  begin
  n := dataframe.RowCount;
  f := dataframe.VectorCount;
  tot :=0;
  for i := 0 to f-1 do
     tot := tot + dataframe.vectors[i].MemoryUsed;
  printresult(format( 'Memory for data = %d bytes (%f MB)',[tot + 8*n, (tot + 8*n)/(1024*1000)]));
  end;

  ms.dwLength :=sizeof(TMemoryStatus);
  GlobalMemoryStatus(ms);
  dm.info('%d %% of memory is used <br> Physical memory %d MB'
  + '<br> Free physical memory %d MB'
  + '<br> Total Virtual memory %d MB'
  + '<br> Free Virtual memory %d MB',
  [ms.dwMemoryLoad,ms.dwTotalPhys div 1048576,ms.dwAvailPhys div 1048576,
  ms.dwTotalPageFile div 1048576,ms.dwAvailPageFile div 1048576], 203027);

end;

function TDM.variables(cmd:TCommand): boolean;
var
 Param   : IValue;
 Notify  : Boolean;

begin
  if (cmd.ParameterCount > 0) then //got commands
  begin
    Param :=cmd.ParamByName['SUBCOMMAND'];
    if param<> nil then
    begin
      if (Param.AsString='DROP') or
         (Param.AsString='KEEP') or
         (Param.AsString='FORMAT') then
      begin
        Param := cmd.ParamByName['VARLIST'];
        CheckVariableNo(TStringList(Param),1);
      end;
    end;
  end;
  notify := ODocument.DoVariables(cmd);
  If notify then NotifyInterface(EpiVectorListChanged,integer(dataframe),0);
end;

function TDM.IsMissingOption(cmd: TCommand): boolean;
begin
  result := Assigned(cmd.ParamByName['MISSING']) or
            Assigned(cmd.ParamByName['M']);
end;

function TDM.Define(pVarDesc: TAnaVariableDescriptor): boolean;
var
  v          :TEpiVector;
  vvar       :TVectorVar;
  s          :String;
begin
   case pVarDesc.VarScope of
     EpiVarStandard,EpiVarCum:
     begin
        if not dm.CheckDataOpen() then exit; checkdataopen();
       v := dataframe.NewVector(pVarDesc);
       v.VariableLabel := pVarDesc.Name;
       s := ODocument.AppendDateType(v, GetFieldTypeName(pVarDesc.dataType));
       vvar := TVectorVar.create(dataframe.VectorByName[pVarDesc.name],nil);
       executor.AddVar(vvar,EpiVarLocal);
       NotifyInterface(EpiVectorListChanged, integer(dataframe), 0);
     end;
     EpiVarGlobal:
     begin
       Executor.NewVar(pVarDesc);
       s := GetFieldTypeName(pVarDesc.dataType);
     end;
     EpiVarSystem: Error('Defining systemvariables not allowed', [], 103035);
     end;//case
   s := format(lang(203028,'Var Name %s of type %s'), [pVarDesc.Name,s]);
   s := s + '<br>' + format(lang(203029,'Var length: %d'),[pVarDesc.length]);
   if not (pVarDesc.DataType in [EpiTyString,EpiTyUppercase]) then
     s := s + format(lang(203030,'decimals %d'), [pVarDesc.Decimals]);
   info(s, [], 0);
end;

function TDM.GenField(Dataframe: TEpiDataFrame; pVarDesc: TAnaVariableDescriptor; Exp: IValue): boolean;
var
  v          : TEpiVector;
  vvar       : TVectorVar;
  opt        : TEpiOption;
  list       : TStrings;
  vtype      : integer;
begin
  //checkdataopen();
  result := false;
  v := nil;
  if getOpenFileCount = 0 then
    error('No data', [], 103005)
  else
  begin
    //  if exp=nil then  error('No expression');
    try
      if pVarDesc.DataType = EpiTyUnknown then
      begin
        if (GetOptionValue('VAR GENERATE TYPE', opt)) then
        begin
          vtype := StringToEpiType(opt.Value);
          if vtype = EpiTyUnknown then
            vtype := ShortStringToEpiType(opt.Value);
          if vtype = EpiTyUnknown then
            Error('Unknown default value for VAR GENERATE TYPE', [], 103036);
          pVarDesc.DataType := vtype;
          case vType of
             EpiTyBoolean: pVarDesc.Length :=1;
             EpiTyInteger: pVarDesc.Length :=9;  // changed from 4
             EpiTyFloat:
               begin
                 pVarDesc.Length    :=10;
                 pVarDesc.Decimals  :=4;
                 pVarDesc.Format := '%8.4f';
               end;
             EpiTyDate:    pVarDesc.Length :=10;
             EpiTyString,
             EpiTyUppercase:  pVarDesc.Length :=12;
          end;
        end;
      end;
      v := dataframe.NewVector(pVarDesc);
      v.VariableLabel := pVarDesc.Name;
      if dataframe <> nil then
      begin
        v:= DataFrame.FindVector(pVarDesc.name);
        if (v<> nil) and (exp<>nil) then
          Dataframe.UpdateVector(v,Exp);
      end;
    except
      // Could not create var correctly... posibly due to bad dataframe update.
      if Assigned(v) then
      begin
        list := TStringList.Create();
        list.Add(v.Name);
        Executor.DropVars(list, EpiVarlocal);
        dataframe.DropVectors(list);
        FreeAndNil(list);
        FreeAndNil(v);
      end;
      raise;
      exit;
    end;
    NotifyInterface(EpiVectorListChanged,integer(dataframe),0);
    info('Var Name %s of type %s and length: %d  decimals %d',
         [pVarDesc.Name,ODocument.AppendDateType(v, GetFieldTypeName(pVarDesc.dataType)),
         pVarDesc.length,pVarDesc.Decimals], 203031);
    result := true;
  end;
end;


function TDM.GenVar(pVarDesc: TAnaVariableDescriptor;Exp: IValue): boolean;
var
  v       :IValue;
  S : STRING;
begin
//assume it is global
 if pVarDesc.VarScope =0 then
      pVarDesc.VarScope:=EpiVarGlobal;
  v:=Executor.NewVar(pVarDesc);
  if (v<> nil) and (exp<>nil) then
  begin
     v.value:= Exp.Value;
  end;
  s:= format(lang(203031,'Var Name %s of type %s and length: %d  decimals %d'),[pVarDesc.Name,GetFieldTypeName(pVarDesc.dataType),pVarDesc.length,pVarDesc.Decimals]);
  if GetFieldTypeName(PVarDesc.DataType) = 'Integer' then
    s := s + lang(203032, '<br>Notice integers > 1000 are saved as float variables with 0 decimals');
  info(s, [], 0);
end;

function TDM.Describe(VarNames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
  VarN: TStrings;

begin
  df:=nil;
  Try
     if not dm.CheckDataOpen() then exit; //CheckDataOpen();
//    CheckVariableNo(Varnames,0);

    // How to handle missing data in this section???
    // At the moment handled by the DoDescribe() function.
    // Rewritten by Torsten Christiansen 2004-01-07

    Varnames := dataframe.GetVectorNames(Varnames);

    if (cmd.ParamByName['NM'] <> nil) then
      df := dataframe.prepareDataframe(Varnames, Varnames)
    else
      df := dataframe.prepareDataframe(Varnames, Nil);

    if df.SelectedRowCount = 0 then error('No Data', [], 103005);
    if df.SelectedRowCount < 20 then info('Small N, Warning: Check Percentiles', [], 203033);
    DoDescribe(df, varnames, cmd);

  finally
     df.free;
  end;
end;

function TDM.Means(Varnames: TStrings; cmd: TCommand): boolean;
var
  df         : TEpiDataFrame;

begin
  df := nil;
  try
    checkdataopen();
    CheckVariableNo(Varnames, 1, 2);

    if (Cmd.ParamByName['M'] <> nil) then dm.info('Option %s not implemented yet', ['/M'], 203034);

    if  (Cmd.ParamByName['BY'] = nil) and (Varnames.Count = 2) then
            dm.Info('Syntax: Means %s /BY= %s', [dataframe.VectorByName[Varnames[0]].Name,dataframe.VectorByName[Varnames[1]].Name], 203035);

    if (Cmd.ParamByName['BY'] <> nil) then
      Varnames.Add(Cmd.ParamByName['BY'].AsString);

    // How to handle missing data in this section???
    // At the moment handled by the DoMeans() function.
    // Rewritten by Torsten Christiansen 2004-01-07

    df := dataframe.prepareDataframe(Varnames, Varnames);
    if df.SelectedRowCount = 0 then error('No Data', [], 103005);
    if df.SelectedRowCount < 10 then info('Warning: Percentiles imprecise', [], 203036);
    DoMeans(df, varnames, cmd);

  finally
    if df <> nil then df.free;
  end;
end;

function TDM.Sort(Varnames: TStrings): boolean;
var
   Vectorlist :TEpiVectors;
   v          : TepiVector;
   i,co       : integer;
begin
  Vectorlist:=nil;
  try
     if not dm.CheckDataOpen() then exit; //CheckDataOpen();
    Vectorlist:= Dataframe.GetVectorListByName(varnames);
    co := Vectorlist.Count;
    info('Sorting please wait...', [], 203037);
    dataFrame.Sort(Vectorlist);
    info('Sorting complete', [], 203038);
  finally
    Vectorlist.free;
  end;
end;

function TDM.PreTables(Varnames: TStrings; cmd : Tcommand): boolean;
var
  df: TEpiDataframe;
begin
  df :=nil;
  try
    if not dm.CheckDataOpen() then exit; //checkdataopen();
    CheckVariableNo(Varnames,1);
    if (cmd.ParamByName['W'] <> nil) then
      Varnames.Add(cmd.ParamByName['W'].AsString);

    // Option /OA = /CT /AR
    if (Cmd.ParamExists['OA']) then
    begin
      Cmd.ParameterList.RemoveVar(Cmd.ParamByName['OA']);
      Cmd.ParameterList.AddVar('CT', 0);
      Cmd.ParameterList.AddVar('AR', 0);
    end;

    if (Cmd.ParamExists['AR']) and (not Cmd.ParamExists['CT']) then
      dm.Error('/AR only allowed with compact table (/CT)', [], 0, 0);

    if (IsMissingOption(cmd) or (cmd.ParamByName['F'] <> nil) or (cmd.ParamByName['FV'] <> nil)
        or (cmd.ParamByName['CT'] <> nil) or (cmd.CommandID = opFreq)) and
       (not (cmd.ParamByName['NM'] <> nil)) then
      df := dataframe.prepareDataframe(Varnames, nil)
    else
      df := dataframe.prepareDataframe(Varnames, Varnames);


    if df.SelectedRowCount = 0 then
      dm.Error('No Data', [], 103005);
    result := OTables.DoTables(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
  end;
end;


function TDM.Aggregate(ByVars: TStrings; Cmd: TCommand): boolean;
var
  Df, AggDF: TEpiDataframe;
  AggrList: TAggrList;
  Varnames: TStrings;
  dummy: boolean;
const
  procname = 'Aggregate';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if not CheckDataOpen() then exit;
  Df := nil;
  AggDF := nil;
  Varnames := nil;
  AggrList := nil;
  try
    CheckVariableNo(ByVars, 0, 600);
//    if byvars <> nil then
//      Varnames.AddStrings(ByVars);
    if (cmd.ParamByName['M'] <> nil) then
      df := dataframe.prepareDataframe(Varnames, nil)
    else
      df := dataframe.prepareDataframe(Varnames, ByVars);
    AggDF := OAggregate.DoAggregate(df, ByVars, Cmd);

    if cmd.ParamByName['CLOSE'] <> nil then
    begin
      dm.InternalCloseFile(fDataframe);
      fDataframe := AggDF;
      initDataFrameVars(fDataframe);
      NotifyInterface(EpiVectorListChanged,integer(dataframe),0);
      NotifyInterface(EpiOpenFile,integer(dataframe),0);
      OBrowse.UpdateBrowseWindow(fdataframe);
      OUpdate.UpdateBrowseWindow(fdataframe);
    end else
      FreeAndNil(AggDF);
  finally
    if Assigned(Df) then FreeAndNil(Df);
    if Assigned(VarNames) then FreeAndNil(Varnames);
    if Assigned(AggrList) then FreeAndNil(AggrList);
  end;
  ODebug.DecIndent;
end;

function TDM.Stattables(Varnames: TStrings; Cmd: TCommand): boolean;
var
  df, aggdf: TEpiDataframe;
  AllVars, ByVars: TStrings;
const
  procname = 'Stattables';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, Self.ClassName, procname, procversion, 1);

  df := nil;
  ByVars := nil;
  AllVars := nil;
  if not dm.CheckDataOpen() then exit;
  try
    CheckVariableNo(Varnames, 1);
    ByVars := TStringList.Create;
    AllVars := TStringList.Create;
    AllVars.Assign(Varnames);
    if Cmd.ParamExists['BY'] then
    begin
      SplitString(Cmd.ParamByName['BY'].AsString, ByVars, [' ', ',']);
      AllVars.AddStrings(ByVars);
    end;

    if Cmd.ParamExists['STRATA'] then
    begin
      Allvars.Add(Cmd.ParamByName['STRATA'].AsString);
      ByVars.Insert(0, Cmd.ParamByName['STRATA'].AsString);
    end;

    if (cmd.ParamExists['M']) then
      df := dataframe.prepareDataframe(AllVars, nil)
    else
      df := dataframe.prepareDataframe(AllVars, ByVars);

    aggdf := OAggregate.DoStattable(df, VarNames, ByVars, cmd);
    
    if cmd.ParamByName['CLOSE'] <> nil then
    begin
      dm.InternalCloseFile(fDataframe);
      fDataframe := AggDF;
      initDataFrameVars(fDataframe);
      NotifyInterface(EpiVectorListChanged,integer(dataframe),0);
      NotifyInterface(EpiOpenFile,integer(dataframe),0);
      OBrowse.UpdateBrowseWindow(fdataframe);
      OUpdate.UpdateBrowseWindow(fdataframe);
    end else
      FreeAndNil(AggDF);
  finally
    if Assigned(df) then FreeAndNil(df);
    if Assigned(AllVars) then FreeAndNil(AllVars);
    if Assigned(ByVars) then FreeAndNil(ByVars);
  end;
  ODebug.DecIndent;
end;

function TDM.Bar(VarNames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
begin
       if not dm.CheckDataOpen() then exit; //checkdataopen();
  CheckVariableNo(Varnames, 1, 1);
  df := nil;
  try
    if (cmd.ParamByName['W'] <> nil) then
      if not (dataframe.VectorByName[cmd.ParamByName['W'].AsString] is TEpiIntVector) then
        Error('Weight variable must be of type Integer', [], 103037)
      else
        Varnames.Add(cmd.ParamByName['W'].AsString);
    if (cmd.ParamByName['BY'] <> nil) then
    begin
      Varnames.Add(cmd.ParamByName['BY'].AsString);
      cmd.CommandID := opBar;  // Histogram does handle /BY but TAreaSeries cannot show proper side-by-side graphs.
    end;
    if (cmd.ParamByName['M'] <> nil) then
      df := dataframe.prepareDataframe(Varnames, nil)
    else
      df := dataframe.prepareDataframe(Varnames, Varnames);
    if df <> nil then OGraph.DoGraphs(df, varnames, cmd);
  finally
    if df <> nil then FreeAndNil(df);
  end;
end;

function TDM.Line(VarNames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
  Missing: TStrings;
begin
       if not dm.CheckDataOpen() then exit; //checkdataopen();
  CheckVariableNo(Varnames, 2);
  if cmd.ParamByName['YTEXT'] = nil then
    cmd.ParameterList.AddVar('YTEXT', dataframe.VectorByName[varnames[1]].GetVariableLabel(Cmd.ParameterList));
  Missing := TStringList.Create;
  Missing.Add(Varnames[0]);
  df := dataframe.prepareDataframe(Varnames, Missing);
  if df <> nil then OGraph.DoGraphs(df, varnames, cmd);
end;

function TDM.Pie(VarNames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
const
  procname = 'Pie';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  try
         if not dm.CheckDataOpen() then exit; //checkdataopen();
    CheckVariableNo(Varnames,1);
    df := dataframe.prepareDataframe(Varnames, varnames);
    if df.SelectedRowCount = 0 then error('No Data', [], 103005);
    OGraph.DoGraphs(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
  end;
end;

function TDM.BoxPlot(VarNames: TStrings; Cmd: TCommand): boolean;
var
  df: TEpiDataframe;
  missing: TStringList;
const
  procname = 'BoxPlot';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  df := nil;
  try
    if not dm.CheckDataOpen() then exit;
    CheckVariableNo(Varnames,1);
    Missing := TStringList.Create();
    if Cmd.ParamExists['NM'] then
      Missing.AddStrings(Varnames);
    if Cmd.ParamExists['BY'] then
    begin
      varnames.Add(Cmd.ParamByName['BY'].AsString);
      if not Cmd.ParamExists['M'] then
        missing.Add(Cmd.ParamByName['BY'].AsString);
    end;
    df := dataframe.prepareDataframe(Varnames, missing);
    if df.SelectedRowCount = 0 then error('No Data', [], 103005);
    if df.SelectedRowCount < 20 then info('Small N, Warning: Check Percentiles', [], 203033);
    OGraph.DoGraphs(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
  end;
end;



function TDM.Scatter(VarNames: TStrings; cmd: TCommand): boolean;
var
  df    : TEpiDataFrame; 
  Missing: TStringList;
  i: integer;
const
  procname = 'Scatter';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  df := nil;
  missing := nil;
  try
    if not dm.CheckDataOpen() then exit; //checkdataopen();
    CheckVariableNo(Varnames,2);
    missing := TStringList.Create();
    Missing.AddStrings(Varnames);

  // Uncomment when XLAB needed in scatter!
{    if (cmd.ParamByName['XLABEL'] <> nil) then
      Varnames.Add(cmd.ParamByName['XLABEL'].AsString);}
    if (cmd.ParamByName['BY'] <> nil) then
    begin
      Varnames.Add(cmd.ParamByName['BY'].AsString);
      if Cmd.ParamByName['M'] = nil then
        Missing.Add(cmd.ParamByName['BY'].AsString);
    end;
    df := dataframe.prepareDataframe(Varnames, missing);

    if df.SelectedRowCount = 0 then error('No Data', [], 103005);
    OGraph.DoGraphs(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
    if Assigned(missing) then FreeAndNil(missing);
  end;
end;

function TDM.XChart(Varnames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
  Vectorlist: TEpiVectors;
  i : integer;
const
  procname = 'XChart';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  df := nil;
  Vectorlist := nil;
  try
         if not dm.CheckDataOpen() then exit; //checkdataopen();
    CheckVariableNo(Varnames, 2, 2);

    // Do check that first var is countable (integer, datetime)
    // and likewise with sec. var is integer.
    VectorList := dataframe.GetVectorListByName(Varnames);
    if not (VectorList[0].DataType in [EpiTyInteger, EpiTyDate, EpiTyFloat]) then
      error('First variable (%s) must be of type Integer or Date', [varnames[0]], 103038);
    if not (VectorList[1].DataType in [EpiTyInteger, EpiTyFloat]) then
      error('Second variable (%s) must be of type Integer or Float', [varnames[1]], 103039);
    if (cmd.ParamByName['XLABEL'] <> nil) then
      Varnames.Add(cmd.ParamByName['XLABEL'].AsString);
    df := dataframe.prepareDataframe(Varnames, Varnames);

    For i := 0 to 1 do
      if VectorList[i].DataType in [EpiTyFloat] then
        df.ConvertVector(varnames[i], EpiTyInteger,' read as ');

    if df.SelectedRowCount = 0 then error('No Data', [], 103005);

    if cmd.ParamByName['YTEXT'] = nil then
      cmd.ParameterList.AddVar('YTEXT', 'Count');

    OGraph.DoXChart(df, varnames, cmd)

  finally
    if df <> nil then df.Free;
    if Vectorlist <> nil then Vectorlist.Free;
  end;
end;


function TDM.IChart(Varnames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
  Vectorlist: TEpiVectors;
  i : integer;
begin
  df := nil;
  Vectorlist := nil;
  try
         if not dm.CheckDataOpen() then exit; //checkdataopen();
    CheckVariableNo(Varnames, 2, 2);

    // Do check that first var is countable (integer, datetime)
    // and likewise with sec. var is integer.
    VectorList := dataframe.GetVectorListByName(Varnames);
    if not (VectorList[0].DataType in [EpiTyInteger, EpiTyDate, EpiTyFloat]) then
      error('First variable (%s) must be of type Integer or Date', [varnames[0]], 103038);
    if not (VectorList[1].DataType in [EpiTyInteger, EpiTyFloat]) then
      error('Second variable (%s) must be of type Integer or Float', [varnames[1]], 103039);
    if (cmd.ParamByName['XLABEL'] <> nil) then
      Varnames.Add(cmd.ParamByName['XLABEL'].AsString);
    df := dataframe.prepareDataframe(Varnames, Varnames);

    For i := 0 to 0 do
         if VectorList[i].DataType in [EpiTyFloat] then
              df.ConvertVector(varnames[i], EpiTyInteger,' read as ');

    if df.SelectedRowCount = 0 then error('No Data', [], 103005);

    if cmd.ParamByName['YTEXT'] = nil then
      cmd.ParameterList.AddVar('YTEXT', 'Count');

    OGraph.DoGraphs(df, varnames, cmd)
//    DoIChart(df, varnames, cmd);

  finally
    if Assigned(df) then FreeAndNil(df);
    if Vectorlist <> nil then Vectorlist.Free;
  end;
end;

function TDM.PChart(Varnames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
  Vectorlist: TEpiVectors;
  i : integer;

begin
  df := nil;
  Vectorlist := nil;

  try
         if not dm.CheckDataOpen() then exit; //checkdataopen();
    CheckVariableNo(Varnames, 3,3);

    VectorList := dataframe.GetVectorListByName(Varnames);
    if not (VectorList[0].DataType in [EpiTyInteger, EpiTyDate, EpiTyFloat]) then
      error('First variable (%s) must be of type Integer, Float or Date', [varnames[0]], 103040);
    if not (VectorList[1].DataType in [EpiTyInteger, EpiTyFloat]) then
      error('Second variable (%s) must be of type Integer or Float', [varnames[1]], 103039);
    if not (VectorList[2].DataType in [EpiTyInteger, EpiTyFloat]) then
      error('Third variable (%s) must be of type Integer or Float', [varnames[2]], 103041);

    if (cmd.ParamByName['XLABEL'] <> nil) then
      Varnames.Add(cmd.ParamByName['XLABEL'].AsString);

    df := dataframe.prepareDataframe(Varnames, nil);

    For i := 0 to 0 do
         if VectorList[i].DataType in [EpiTyFloat] then
              df.ConvertVector(varnames[i], EpiTyInteger, ' read as ');

    if df.SelectedRowCount = 0 then error('No Data', [], 103005);

    if cmd.ParamByName['YTEXT'] = nil then
      cmd.ParameterList.AddVar('YTEXT', 'Percentage');

    OGraph.DoGraphs(df, varnames, cmd);

  finally
    if Assigned(df) then FreeAndNil(df);
    if Vectorlist <> nil then Vectorlist.Free;
  end;
end;

function TDM.EpiCurve(Varnames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataframe;
  VectorList: TEpiVectors;
  Missing: TStrings;
  i: integer;
const
  procname = 'EpiCurve';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
       if not dm.CheckDataOpen() then exit; //CheckDataOpen();
  if Varnames.Count > 2 then
    Error('Use: EpiCurve %s %s /by=%s', [varnames[0], varnames[1], varnames[2]], 103042);
  df := nil;
  vectorlist := nil;
  Missing := nil;
  try
    CheckVariableNo(Varnames, 2, 2);
    
    Missing := TStringList.Create();
    Missing.Add(Varnames[0]);
    
    if Cmd.ParamByName['BY'] <> nil then
      varnames.Add(Cmd.ParamByName['BY'].AsString);
      
    VectorList := dataframe.GetVectorListByName(Varnames);
    df := dataframe.prepareDataframe(Varnames, Missing);
    for i := 0 to Varnames.count-1 do
    begin
      if (VectorList[i].DataType = EpiTyString) then
        Error('Cannot use string variable: %s', [Vectorlist[i].Name], 103043);
      if not (VectorList[i].DataType in [EpiTyInteger, EpiTyDate]) then
        df.ConvertVector(VectorList[i].Name, EpiTyInteger, ' read as ');
    end;

    OGraph.DoGraphs(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
    if Vectorlist <> nil then FreeAndNil(VectorList);
    ODebug.DecIndent;
  end;
end;

function TDM.DotPlot(Varnames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataframe;
  s: TStrings;
const
  procname = 'DotPlot';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
       if not dm.CheckDataOpen() then exit; //CheckDataOpen();
  CheckVariableNo(Varnames, 1, 1);
  df := nil;
  s := TStringList.Create;	{ construct the list object }
  s.Append(varnames[0]);
  if Cmd.ParamByName['BY'] <> nil then
  begin
    varnames.Add(Cmd.ParamByName['BY'].AsString);
    if Cmd.ParamByName['M'] = nil then
      s.Add(Cmd.ParamByName['BY'].AsString);
  end;
  try
    if (cmd.ParamByName['M'] <> nil) then
      df := dataframe.prepareDataframe(Varnames,s)
    else
      df := dataframe.prepareDataframe(Varnames, Varnames);
//      if not (VectorList[0].DataType in [EpiTyInteger, EpiTyDate]) then
//         df.ConvertVector(VectorList[0].Name, EpiTyInteger, ' read as ');
    OGraph.DoGraphs(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
    if s <> nil then FreeAndNil(s);
    ODebug.DecIndent;
  end;
end;

function TDM.CIPlot(Varnames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataFrame;
  s: TStrings;
  byvar : string;
const
  procname = 'CIPlot';
  procversion = '1.1.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  df := nil;
  s := nil;

  if not dm.CheckDataOpen() then exit; //CheckDataOpen();

  try
    CheckVariableNo(Varnames, 2);
    S := TStringList.Create;
    if cmd.ParamByName['BY'] <> Nil then
      dm.info('Option %s not implemented yet', ['/BY'], 203034);
{    begin
      varnames.Add(Cmd.ParamByName['BY'].AsString);
      S.Add(Cmd.ParamByName['BY'].AsString);
    end else
      dm.Error('/BY=<byvar> cannot be missing', [], 103044);  }

    // Outcome may never contain missing.
    S.Add(Varnames[0]);
    if (cmd.ParamExists['NM']) then
      S.AddStrings(Varnames);
//      dm.info('Option %s not implemented yet', ['/NM'], 203034);

//    S.AddStrings(Varnames);
    df := dataframe.prepareDataframe(Varnames, S);
    OGraph.DoGraphs(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
    if Assigned(s) then FreeAndNil(s);
    ODebug.DecIndent;
  end;
end;

function TDM.CumDistPlot(Varnames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataframe;
  //VectorList: TEpiVectors;
const
  procname = 'CumDistPlot';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
       if not dm.CheckDataOpen() then exit; //CheckDataOpen();
  CheckVariableNo(Varnames, 1, 1);
  df := nil;
  try
    if Assigned(cmd.ParamByName['BY']) then
      Varnames.Add(cmd.ParamByName['BY'].AsString);
    df := dataframe.prepareDataframe(Varnames, Varnames);
    OGraph.DoGraphs(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
    ODebug.DecIndent;
  end;
end;

function TDM.ParetoPlot(Varnames: TStrings; cmd: TCommand): boolean;
var
  df: TEpiDataframe;
const
  procname = 'ParetoPlot';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  CheckDataOpen();
  CheckVariableNo(Varnames, 1, 1);
  df := nil;
  try
    if Cmd.ParamByName['W'] <> nil then
      Varnames.Add(Cmd.ParamByName['W'].AsString);
    if Cmd.ParamByName['XLABEL'] <> nil then
      Varnames.Add(Cmd.ParamByName['XLABEL'].AsString);
    df := dataframe.prepareDataframe(Varnames, Varnames);
    OGraph.DoGraphs(df, varnames, cmd);
  finally
    if Assigned(df) then FreeAndNil(df);
    ODebug.DecIndent;
  end;
end;

function TDM.LifeTable(Varnames: TStrings; cmd: TCommand): boolean;
var
  df, ltdf: TEpiDataFrame;
  MissingList, MaxList: TStrings;
  Exp: IValue;
const
  procname = 'LifeTable';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  df := nil;
  MissingList := nil;
  MaxList := nil;
  try
    CheckVariableNo(Varnames, 2, 3);
    If not CheckDataOpen() then exit;

    // Missing list should contain:
    // - Outcome
    // - Timevar(s) (if two present) if not /MT present
    // - Weigth
    MissingList := TStringList.Create();
    MissingList.AddStrings(Varnames);
    MaxList := TStringList.Create();
    MaxList.AddStrings(Varnames);

    Cmd.ParameterList.AddVar('$TVAR', '');
    if Cmd.ParamExists['EXIT'] then
    begin
      if Varnames.Count = 3 then
        Cmd.ParamByName['$TVAR'].Value := MissingList[2]
      else
        Cmd.ParamByName['$TVAR'].Value := MissingList[1];
    end;

    if Cmd.ParamExists['MT'] then
    begin
      // Always include Timevar1 if two timevars present. Hence delete var2
      if varnames.Count = 3 then
      begin
        Cmd.ParamByName['$TVAR'].Value := MissingList[2];
        MissingList.Delete(2);
        MaxList.Delete(2);
      end else begin
        Cmd.ParamByName['$TVAR'].Value := MissingList[1];
        MissingList.Delete(1);
        MaxList.Delete(1);
      end;
    end;

    if Cmd.ParamExists['EXIT'] then
    begin

    end;

    if Cmd.ParamExists['BY'] then
    begin
      Varnames.Add(Cmd.ParamByName['BY'].AsString);
      MaxList.Add(Cmd.ParamByName['BY'].AsString);
    end;

    if Cmd.ParamExists['W'] then
    begin
      Varnames.Add(Cmd.ParamByName['W'].AsString);
      MissingList.Add(Cmd.ParamByName['W'].AsString);
      MaxList.Add(Cmd.ParamByName['W'].AsString);
    end;

    if IsMissingOption(cmd) then
      df := Dataframe.PrepareDataframe(Varnames, MissingList)
    else if Cmd.ParamExists['MT'] then
      df := Dataframe.PrepareDataframe(Varnames, MaxList)
    else
      df := Dataframe.PrepareDataframe(Varnames, Varnames);

    if df.SelectedRowCount = 0 then error('No Data', [], 103005);

    ltdf := OLifeTables.DoLifeTables(df, varnames, cmd);

    if not Cmd.ParamExists['NG'] then
    begin
      Varnames.Add(df.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList));
      OGraph.DoGraphs(ltdf, varnames, cmd);
    end;

    if cmd.ParamExists['CLOSE'] then
    begin
      ltdf.NormalizeVectorNames(nil);
      dm.InternalCloseFile(fDataframe);
      fdataframe := ltdf;
      initDataFrameVars(fDataframe);
      NotifyInterface(EpiVectorListChanged, integer(dataframe), 0);
      NotifyInterface(EpiOpenFile, integer(dataframe), 0);
      OBrowse.UpdateBrowseWindow(fdataframe);
      OUpdate.UpdateBrowseWindow(fdataframe);
    end else
      FreeAndNil(ltdf);
  finally
    if Assigned(df) then FreeAndNil(df);
    if Assigned(MissingList) then FreeAndNil(MissingList);
    if Assigned(MaxList) then FreeAndNil(MaxList);
    ODebug.DecIndent;
  end;
end;

function TDM.DoLabelValue(Varnames: TStrings; cmd: TCommand): boolean;
const
  procname = 'DoLabelValue';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
       if not dm.CheckDataOpen() then exit; //CheckDataOpen();
  CheckVariableNo(VarNames, 1, 1);
  if cmd.CommandID = opLabelValue then
    result := ODocument.DoValueLabel(Dataframe, Dataframe.GetVectorNames(Varnames), Cmd)
  else
    result := ODocument.DoMissingValue(Dataframe, Dataframe.GetVectorNames(Varnames), Cmd);
  ODebug.DecIndent;
end;

function TDM.DoLabel(Varname, Varlabel: string): boolean;
begin
       if not dm.CheckDataOpen() then exit; //CheckDataOpen();
  result := ODocument.DoLabel(Dataframe, Varname, Varlabel);
end;

function TDM.LabelData(LabelName: string): boolean;
begin
       if not dm.CheckDataOpen() then exit; //CheckDataOpen();
  result := ODocument.DoLabelData(Dataframe, LabelName);
end;

function TDM.SetupOutput{(aForm: TForm)}: boolean;
var
 css :string;
 opt : Tepioption;
begin
    OutputList:= TStatOutputList.Create;
    CodeMaker:=THTMLMaker.Create(OutputList);
    css:=getCurrentdir+'\'+'epiout.css';
    if (not fileexists(css)) then
      css:= extractfilepath(application.exename)+'epiout.css';
    if fileexists(css) then
          CodeMaker.CSSFileName :=css;
    if (GetOptionValue('VIEWER FONT CHARSET', Opt)) then
       CodeMaker.Charset := opt.value;
    CodeMaker.StartOutput;
//    fOutPutStream:= TMemoryStream.Create ;
end;

function TDM.ErasePng(cmd: TCommand): boolean;
var
  SR: TSearchRec;
  k,j: integer;
  del: boolean;
  SearchName, MatchName: string;
  Y, M, D: Word;
  ParamD: boolean;

begin
  j := 0;
  k := 0;
  searchname := '*.png';
  ParamD := Assigned(Cmd.ParamByName['D']);
  if ParamD then
  begin
    DecodeDate(Date(), y, m, d);
    MatchName := 'Graph' + IntToStr(y) + IntToStr(m) + IntToStr(d);
    searchname := 'Graph' + SearchName;
  end else if not Assigned(Cmd.ParamByName['ALL']) then
    searchname := 'Graph' + SearchName;
  if FindFirst(GetCurrentDir + '\' + searchname, faAnyFile, sr) = 0 then
  begin    // count how many
    repeat
      if ParamD and (Pos(AnsiUpperCase(MatchName), AnsiUpperCase(sr.Name)) > 0) then
        continue;
      inc(j);
    until FindNext(sr) <> 0;
    FindClose(sr);
    del := True;
    if Cmd.ParamByName['NOCONFIRM'] = nil then
    begin
      Del := False;
      info('To avoid confirmation: erasepng /noconfirm', [], 203039);
      if MessageDlg('Erase '+searchname +' ? (Files: ' + trim(format('%8d',[j]))+')',mtConfirmation,[mbNo,mbYes],0) = mrYes then
        if MessageDlg('Confirm erase ? (Files: ' + trim(format('%8d',[j])) + ')',mtConfirmation,[mbNo,mbYes],0) = mrYes then
          Del := True;
    end;
    if del = True then
    begin
      FindFirst(GetCurrentDir + '\' + searchname, faAnyFile, sr);
      repeat
        if ParamD and (Pos(AnsiUpperCase(MatchName), AnsiUpperCase(sr.Name)) > 0) then
          continue;
        DeleteFile(sr.Name);
        inc(k);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;
  info('%d files deleted', [k], 203040);
end;

function TDM.runInitFile(const fn:string): boolean;
var
  inif :string;
  source: TStringList;
  i: integer;

  function ContainsWord(str, substr: string): boolean;
  var
    p: integer;
    s: string;
  begin
    result := false;
    s:= AnsiUppercase(trimleft(str));
    p := pos(substr,s);
    if (p = 1) and (s[length(substr)+1] = ' ') then
        result := true;
  end;

begin
  result := false;
  inif :=Sysutils.trim(fn);
  if inif='' then
  begin
    inif:=getCurrentdir+'\'+'epidatastat.ini';
    if not fileexists(inif) then
      inif:= extractfilepath(application.exename)+'epidatastat.ini';
  end;
  RunPGMFile(inif);
  source:= TStringList.create;
  source.LoadFromFile(inif);
  for i := 0 to source.Count-1 do
  begin
    result := result or ContainsWord(source[i], 'VIEW');
    result := result or ContainsWord(source[i], 'RUN');
    result := result or ContainsWord(source[i], 'RUNTEST');
    result := result or ContainsWord(source[i], 'READ');
  end;
end;

procedure TDM.DMCreate(Sender: TObject);
begin
  SysUtils.DecimalSeparator := EpiDecSeparator;
  ana := TAnaExecutor.Create(nil);
  ana.OnError:=OnParseError;
  ana.OnShowOutput :=showExecoutput;
  initGlobalSysVar;
  initOptions;
  SetupOutput;
  randomize;
  initShortCuts;
  DecimalSeparator:='.';
  SetFalseString('N');
  SetTrueString('Y');
end;


procedure TDM.OnParseError(const msg: string; Token: TSMToken;
  var Handled: boolean);
begin
// handled:=true;
 error(msg,[],0,0);
 Messagebeep(0);
 abort;
end;

procedure TDM.Cancel;
begin
  cancelled :=true;
//  info('Command cancelled by user');
end;

procedure TDM.CommandReset;
begin
  cancelled :=false;
end;

procedure TDM.RunCommandLine(const cmd:string);
var
  scmd, srem:string;
  p     : integer;
const
  procname = 'RunCommandLine';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  srem:=cmd;
  while true do
  try
     if srem='' then break;
     p:= pos(';', srem);
     if p> 0 then
     begin
        scmd:=Sysutils.trim(copy(srem,1,p-1));
        srem:=Sysutils.trim(copy(srem,p+1,Maxint));
     end
     else
     begin
       scmd:=srem;
       srem:='';
     end;
     if scmd='' then break;
  //   writeln('. '+scmd,clwhite);
  //   CommandReset;
     ana.RunCommandLine(scmd);
  finally
    ODebug.DecIndent;
  end;
end;



procedure TDM.DMDestroy(Sender: TObject);
begin
  FreeAndNil(Ana);
  FreeAndNil(CodeMaker);
  FreeAndNil(fOptions);
  FreeAndNil(fdataframe);
end;

procedure TDM.showExecoutput(const output: string);
begin
//   if getword(output
   PrintCommand('. '+output);
end;


function TDM.ExecShortCut(sc:TShortCut):boolean;
var
 s:string;
 idx :integer;
begin
 s:=inttostr(sc);
 if not ShortCuts.find(s,idx) then exit;
 s := pchar(ShortCuts.objects[idx]);
 if s='' then exit;
 aMainform.CmdEdit.History.Add(s);
 runcommandline(s);
end;

function TDM.AddshortCut(const key,cmd:string):boolean;
var
 idx,sc :integer;
 lcmd:string;
begin
 result:=false;
 sc:=TextToShortCut(key);
 if sc=0 then exit;
 lcmd :=Sysutils.trim(cmd);
 if lcmd<>'' then
  if lcmd[1]='"' then
  begin
     lcmd:=copy(lcmd,2,Maxint);
     if lcmd[length(lcmd)]='"' then
       lcmd:=copy(lcmd,1,length(lcmd)-1);
 end;
 idx:= ShortCuts.addObject(inttostr(sc), TObject(strnew(pchar(lcmd))));
 NotifyInterface(EpiAddShortCut,integer(pchar(fshortcuts[idx])),0);
end;


function TDM.initShortCuts: boolean;
begin
 if fShortCuts=nil then
     fShortCuts:= TStringList.create;
 fShortCuts.Sorted:=true;
 fShortCuts.Duplicates:=dupIgnore;
 fShortCuts.Clear;
end;


function TDM.initOptions: boolean;
begin
  if foptions=nil then
    fOptions:= TStringList.create;
  foptions.Sorted:=true;
  foptions.Duplicates:=dupIgnore;
  fOptions.Clear;

  foptions.AddObject('RANDOM SEED', TEpiOption.Create('RANDOM SEED', '9', EpiTyInteger));
  foptions.AddObject('RANDOM SIMULATIONS', TEpiOption.Create('RANDOM SIMULATIONS', '500', EpiTyInteger));


  foptions.AddObject('DEBUG LEVEL', TEpiOption.Create('DEBUG LEVEL', '0', EpiTyInteger));
  foptions.AddObject('DEBUG FILENAME', TEpiOption.Create('DEBUG FILENAME', 'moduletest.log', EpiTyString));
  foptions.AddObject('LANGUAGE',TEpiOption.Create('LANGUAGE','English',EpiTyString));

  //lifetable settings
  foptions.AddObject('LIFETABLE INTERVAL',TEpiOption.Create('LIFETABLE INTERVAL', '0,7,15,30,60,90,180,360,540,720,3600,7200,15000', EpiTyString));
  foptions.AddObject('LIFETABLE HEADER',TEpiOption.Create('LIFETABLE HEADER', 'Interval,N<sub>At risk</sub>,Deaths,Lost,Survival,Std. Error', EpiTyString));

  //table design
  foptions.AddObject('TABLE DESIGN',TEpiOption.Create('TABLE DESIGN','LINE',EpiTyString));
  foptions.AddObject('TABLE DESIGN FREQ',TEpiOption.Create('TABLE DESIGN FREQ','LINE',EpiTyString));
  foptions.AddObject('TABLE DESIGN STAT',TEpiOption.Create('TABLE DESIGN STAT','LINE',EpiTyString));
  foptions.AddObject('TABLE DESIGN SYSTEM',TEpiOption.Create('TABLE DESIGN SYSTEM','SYSTEM',EpiTyString));
  foptions.AddObject('TABLE DESIGN GRAPH',TEpiOption.Create('TABLE DESIGN GRAPH','GRAPH',EpiTyString));

  // Table formatting
  // foptions.AddObject('TABLE PERCENT',TEpiOption.Create('TABLE PERCENT','OFF',EpiTyString));
  foptions.AddObject('TABLE PERCENT FORMAT COL',TEpiOption.Create('TABLE PERCENT FORMAT COL','P1{}',EpiTyString));
  foptions.AddObject('TABLE PERCENT FORMAT ROW',TEpiOption.Create('TABLE PERCENT FORMAT ROW','P1()',EpiTyString));
  foptions.AddObject('TABLE PERCENT FORMAT TOTAL',TEpiOption.Create('TABLE PERCENT FORMAT TOTAL','P1[]',EpiTyString));

  // Table headers
  foptions.AddObject('TABLE PERCENT HEADER',TEpiOption.Create('TABLE PERCENT HEADER','%',EpiTyString));
  foptions.AddObject('TABLE PERCENT HEADER ROW',TEpiOption.Create('TABLE PERCENT HEADER ROW','%',EpiTyString));
  foptions.AddObject('TABLE PERCENT HEADER COL',TEpiOption.Create('TABLE PERCENT HEADER COL','%',EpiTyString));
  foptions.AddObject('TABLE PERCENT HEADER TOTAL',TEpiOption.Create('TABLE PERCENT HEADER TOTAL','%',EpiTyString));

  foptions.AddObject('TABLE CT OR HEADER',TEpiOption.Create('TABLE CT OR HEADER','Outcome:,Case,Non Case,N,Exposed,Non exposed',EpiTyString));
  foptions.AddObject('TABLE CT RR HEADER',TEpiOption.Create('TABLE CT RR HEADER','Outcome:,Exposed,Not Exposed,N,n,Ill,RR,AR (%)',EpiTyString));
  foptions.AddObject('TABLE CI HEADER',TEpiOption.Create('TABLE CI HEADER','(95% CI)',EpiTyString));
  foptions.AddObject('TABLE CI FORMAT',TEpiOption.Create('TABLE CI FORMAT','()-',EpiTyString));

  foptions.AddObject('SHOW SYSTEMINFO',TEpiOption.Create('SHOW SYSTEMINFO','OFF',EpiTyBoolean));
  foptions.AddObject('ECHO',TEpiOption.Create('ECHO','ON',EpiTyBoolean));
  foptions.AddObject('SHOW ERROR',TEpiOption.Create('SHOW ERROR','ON',EpiTyBoolean));
  foptions.AddObject('SHOW INFO',TEpiOption.Create('SHOW INFO','ON',EpiTyBoolean));
  foptions.AddObject('SHOW COMMAND',TEpiOption.Create('SHOW COMMAND','ON',EpiTyBoolean));
  foptions.AddObject('SHOW RESULT',TEpiOption.Create('SHOW RESULT','ON',EpiTyBoolean));

  foptions.AddObject('SAVEDATA FIRSTWORD',TEpiOption.Create('FIRSTWORD','ON',EpiTyBoolean));

  foptions.AddObject('START PAGE',TEpiOption.Create('START PAGE','start.htm',EpiTyString));

  foptions.AddObject('HISTORY NAME',TEpiOption.Create('HISTORY NAME','temp',EpiTyString));
  foptions.AddObject('HISTORY COMMENT', TEpiOption.Create('HISTORY COMMENT', 'ON', EpiTyBoolean));
  foptions.AddObject('HISTORY COMMAND PGM',TEpiOption.Create('HISTORY COMMAND PGM','ON',EpiTyBoolean));

  // default behaviour for logfiles
  foptions.AddObject('OUTPUT OPEN', TEpiOption.Create('OUTPUT OPEN', 'ON', EpiTyBoolean));
  foptions.AddObject('OUTPUT NAME', TEpiOption.Create('OUTPUT NAME', 'EAoutput.htm', EpiTyString));
  foptions.AddObject('OUTPUT FOLDER', TEpiOption.Create('OUTPUT FOLDER', '', EpiTyString));

  foptions.AddObject('READ DELETED',TEpiOption.Create('READ DELETED','OFF',EpiTyBoolean));

  // Displayable objects on the main form!
  //foptions.AddObject('DISPLAY TOOLBAR',TEpiOption.Create('DISPLAY TOOLBAR','ON',EpiTyBoolean));
  foptions.AddObject('DISPLAY COMMANDTREE',TEpiOption.Create('DISPLAY COMMANDTREE','OFF',EpiTyBoolean));
  foptions.AddObject('DISPLAY VARIABLES',TEpiOption.Create('DISPLAY VARIABLES','OFF',EpiTyBoolean));
  foptions.AddObject('DISPLAY COMMAND HISTORY',TEpiOption.Create('DISPLAY COMMAND HISTORY','OFF',EpiTyBoolean));
  foptions.AddObject('DISPLAY WORKTOOLBAR', TEpiOption.Create('DISPLAY WORKTOOLBAR', 'ON', EpiTyBoolean));
  foptions.AddObject('DISPLAY MAINMENU',TEpiOption.Create('DISPLAY MAINMENU','ON',EpiTyBoolean));
  foptions.AddObject('DISPLAY COMMAND PROMPT',TEpiOption.Create('DISPLAY COMMAND PROMPT','ON',EpiTyBoolean));
  foptions.AddObject('DISPLAY DATABROWSER',TEpiOption.Create('DISPLAY DATABROWSER','OFF',EpiTyBoolean));

  // show english text instead of number when translation is not available ? off: number on: default english
  foptions.AddObject('DISPLAY TRANSLATE CODE',TEpiOption.Create('DISPLAY TRANSLATE CODE','OFF',EpiTyBoolean));

  foptions.AddObject('GRAPH SAVETYPE',TEpiOption.Create('GRAPH SAVETYPE','png',EpiTyString));
  foptions.AddObject('GRAPH FOOTNOTE', TEpiOption.Create('GRAPH FOOTNOTE','EpiData Analysis Graph',EpiTyString));
  foptions.AddObject('GRAPH FILENAME SHOW', TEpiOption.Create('GRAPH FILENAME SHOW','OFF',EpiTyBoolean));
  foptions.AddObject('GRAPH FILENAME FOLDER', TEpiOption.Create('GRAPH FILENAME FOLDER','OFF',EpiTyBoolean));
  foptions.AddObject('GRAPH CLIPBOARD', TEpiOption.Create('GRAPH CLIPBOARD','ON',EpiTyBoolean));
  foptions.AddObject('GRAPH SIZE X', TEpiOption.Create('GRAPH SIZE X', '400', EpiTyInteger));
  foptions.AddObject('GRAPH SIZE Y', TEpiOption.Create('GRAPH SIZE Y', '300', EpiTyInteger));
  foptions.AddObject('GRAPH FONT SIZE',TEpiOption.Create('GRAPH FONT SIZE','10',EpiTyInteger));
  foptions.AddObject('GRAPH COLOUR',TEpiOption.Create('GRAPH COLOUR','01234567890123456789',EpiTyString));
  foptions.AddObject('GRAPH COLOUR TEXT',TEpiOption.Create('GRAPH COLOUR','213333333',EpiTyString));
  foptions.AddObject('GRAPH SYMBOL',TEpiOption.Create('GRAPH SYMBOL', '01234567890123456789',EpiTyString));
  foptions.AddObject('GRAPH SYMBOL FILLED',TEpiOption.Create('GRAPH SYMBOL FILLED', '01010101010101010101',EpiTyString));

  foptions.AddObject('VAR GENERATE TYPE',TEpiOption.Create('VAR GENERATE TYPE','f',EpiTyString));
  foptions.AddObject('RECODE INTERVAL TEXT',TEpiOption.Create('RECODE INTERVAL TEXT','-',EpiTyString));

  foptions.AddObject('WINDOW FONT SIZE',TEpiOption.Create('WINDOW FONT SIZE','12',EpiTyInteger));
  foptions.AddObject('EDITOR FONT SIZE',TEpiOption.Create('EDITOR FONT SIZE','10',EpiTyInteger));
  foptions.AddObject('BROWSER FONT SIZE',TEpiOption.Create('BROWSER FONT SIZE','10',EpiTyInteger));

  foptions.AddObject('EDITOR PRINT INFO',TEpiOption.Create('EDITOR PRINT INFO','ON',EpiTyBoolean));
  foptions.AddObject('PRINT PREVIEW CM',TEpiOption.Create('PRINT PREVIEW CM','ON',EpiTyBoolean));
  foptions.AddObject('LANGUAGE',TEpiOption.Create('LANGUAGE','English', EpiTyString));

  foldShowResult:=true;

  fShowResult:=true;
  fShowCommand:=true;
  fShowInfo :=true;

  fShowSysInfo :=false;
  fShowError:=true;

  //foptions.AddObject('NUMBER FORMAT',TEpiOption.Create('NUMBER FORMAT','%8.2f',EpiTyString));

  foptions.AddObject('VIEWER FONT NAME',TEpiOption.Create('VIEWER FONT NAME','"Verdana,Arial,Fixedsys"',EpiTyString));
  foptions.AddObject('VIEWER FONT CHARSET',TEpiOption.Create('VIEWER FONT CHARSET','ISO-8859-1',EpiTyString));
  foptions.AddObject('VIEWER FONT SIZE',TEpiOption.Create('VIEWER FONT SIZE','10',EpiTyInteger));
  foptions.AddObject('STYLE SHEET',TEpiOption.Create('STYLE SHEET','"'+ extractfilepath(application.exename)+'epiout.css'+'"',EpiTyString));
  foptions.AddObject('STYLE SHEET EXTERNAL',TEpiOption.Create('STYLE SHEET EXTERNAL','OFF',EpiTyBoolean));

//obsolete
{  foptions.AddObject('PRINTER',TEpiOption.Create('PRINTER','""',EpiTyString));
  foptions.AddObject('PORT',TEpiOption.Create('PORT','ON',EpiTyString));
  foptions.AddObject('PMODE',TEpiOption.Create('PMODE','0',EpiTyInteger));
  foptions.AddObject('PLINES',TEpiOption.Create('PLINES','0',EpiTyInteger));
  foptions.AddObject('PAGE',TEpiOption.Create('PAGE','""',EpiTyString)); }
end;

function TDM.initGlobalSysVar: boolean;
var
  pvar :TAnaVariableDescriptor;
begin
  try
    pvar:= TAnaVariableDescriptor.CreateSystem('SYSTEMDATE',EpiTyString,12,0);
    pvar.DefaultValue:=EpiDateToStr(EpiToday,dfDMY,10);
    Executor.NewVar(pvar);
  finally
    pvar.free;
  end;
  try
    pvar:= TAnaVariableDescriptor.CreateSystem('SYSTEMTIME',EpiTyString,12,0);
    pvar.DefaultValue:=timetostr(time);
    Executor.NewVar(pvar);
  finally
    pvar.free;
  end;
  try
    pvar:= TAnaVariableDescriptor.CreateSystem('SYSDIR',EpiTyString,12,0);
    pvar.DefaultValue:=Extractfilepath(application.ExeName);
    Executor.NewVar(pvar);
  finally
    pvar.free;
  end;
end;


function TDM.initDataFrameVars(df:TEpiDataFrame): boolean;
var
  avar :TFrameVar;
  vvar  :TVectorVar;
  i, co : integer;
begin
  avar:=TFrameVar.Create(df,'RECNUMBER');
  Executor.AddVar(avar,EpiVarLocal);
  avar:=TFrameVar.Create(df,'RECORDCOUNT');
  Executor.AddVar(avar,EpiVarLocal);
  avar:=TFrameVar.Create(df,'RECDELETED');
  Executor.AddVar(avar,EpiVarLocal);
  avar:=TFrameVar.Create(df,'RECVERIFIED');
  Executor.AddVar(avar,EpiVarLocal);
  co:= df.vectorcount;
  for i:= 0 to co-1 do
  begin
     vvar:= TVectorVar.Create(df.vectors[i],nil);
     Executor.AddVar(vvar,EpiVarLocal);
  end;
end;

function TDM.DeinitDataFrameVars(df:TEpiDataFrame): boolean;
begin
  Executor.clearvars(EpiVarLocal);
//  Executor.ClearVars(EpiVarResult);
end;

function TDM.GetFullFilename(const path:string):string;
var
 path1:string;
begin
 path1 :=sysutils.trim(StrRemoveSurroundingQuotes(path));
 if path1='' then exit;
 Result:=PathCanonical(path1,'\');
end;

function TDM.doDOSFileAction(fn:string; action: word):boolean;
begin
  ODos.DOSFileAction(fn, action);
end;

function TDM.copyfile(const old,new:string; cmd: TCommand):boolean;
begin
  ODos.copyfile(old, new, cmd);
end;

function TDM.doLogFileAction(fn:string; cmd: TCommand):boolean;
var
  ext: string;
  opt: TEpiOption;
begin
try
 case cmd.CommandID of
  opLOGOPEN : LogOpen(fn, cmd);
  opLogClose: closeout;
  opView: ViewFile(fn);
  opHelpView: ViewHelpfile(fn);
end;  //case
except
  on E: Exception do
    error('Exception occured: %s', [E.message], 103017);
end;
end;


function TDM.ViewFile(const fn:string):boolean;
var
 fn1: string;
 ext: string;
 opt: TEpiOption;
begin
  fn1:=Sysutils.trim(fn);
  if fn1='' then
    if not GetOpenfilename(fn1,EpiLogFileFilter) then exit;
  try
    NotifyInterface(epiLoadHTMLfile,integer(pchar(fn1)),0);
  except
  on E: Exception do
    error('Exception occured: %s', [E.message], 103017);
  end;
  //if GetOptionValue('OUTPUT FOLDER', opt) then
  //  SetCurrentDir(opt.Value);      //removed again due to problems   Oct 3rd JL
end;


function TDM.ViewHelpFile(const fn:string):boolean;
var
 fn1: string;
 ext: string;
 opt: TEpiOption;
begin
  fn1:=Sysutils.trim(fn);
  if fn1='' then
    if not GetOpenfilename(fn1,EpiLogFileFilter) then exit;
  try
    ViewHelpForm(trim(fn1));
  except
  on E: Exception do
    error('Exception occured: %s', [E.message], 103017);
  end;
end;

function TDM.EditFile(const fn:string):boolean;
var
 fn1: string;
begin
  fn1:=Sysutils.trim(fn);
//  if fn1='' then
//    if not GetOpenfilename(fn1,EpiProgramFilter) then exit;
  try
    NotifyInterface(epiEditfile,integer(pchar(fn1)),0);
  except
  on E: Exception do
    error('Exception occured: %s', [E.message], 103017);
  end;
end;



function TDM.LogOpen(const fn: string; cmd: TCommand):boolean;
var
 fn1: string;
 ext, s: string;
 opt: TEpiOption;
 nofn: boolean;
begin
  aMainForm.Aclogopen.Enabled := true;
  aMainForm.Aclogclose.Enabled := false;
  if cmd <> nil then
    if Cmd.ParamByName['CLOSE'] <> nil then
      closeout();
  if Codemaker.LogFileName <> '' then
    Error('Log open - Add /close', [], 103045);
  nofn := false;
  fn1:=Sysutils.trim(fn);
  if fn1='' then
  begin
    if not Getsavefilename(fn1, EpiLogFileFilter, '.HTM') then exit;
    nofn := true;
  end;

  ext :=copy(ansiuppercase(extractfileext(fn1)),2,3);
  if ext=''then
  begin
     fn1:=fn1+'.HTM';
     ext:='HTM';
  end;

  if nofn then
    if (cmd.CommandID = opLogOpen) then
      aMainForm.CmdEdit.HackHistory('LogOpen', 'LogOpen "' + fn1 +'"') ;

  try
    if fileexists(fn1) then
    begin
      if (cmd.CommandID = opLogOpen) and (cmd.ParamByName['REPLACE'] <> nil) then
        CodeMaker.FileMode := fmCreate
      else if (Cmd.ParamByName['APPEND'] <> nil) then
      begin
        if Cmd.ParamByName['APPEND'] <> nil then
          CodeMaker.FileMode := fmOpenReadWrite
        else
          CodeMaker.FileMode := fmCreate;
      end else
        error('File %s exists. <br> Use /Replace or /Append to replace or append logfile.', [fn1], 103046);
    end else
      CodeMaker.FileMode := fmCreate;
  except
  end;

  try
     if ((ext='TXT') or (ext='LOG')) then
        CodeMaker.LogType:= EpiOTText
     else
        CodeMaker.LogType:=EpiOTHTML;
     CodeMaker.LogFileName :=fn1;
     CodeMaker.StartOutput;
     // Only applicable during start of program.
     if cmd <> nil then
       info('Logopen %s <br> %s %s %s', [fn1, GetBuildInfoAsString, DateToStr(Date), copy(TimeTostr(Time),1,5)], 203041);
     Umain.aMainForm.StatusBar.Panels[4].text:= ExtractFileNameNoPath(fn1);
  except
  on E: Exception do
      error('Exception occured: %s', [E.message], 103017);
  end;
  //if GetOptionValue('OUTPUT FOLDER', opt) then
  //  SetCurrentDir(opt.Value);  //removed again due to problems   Oct 3rd JL
     aMainForm.Aclogopen.Enabled := false;
     aMainForm.Aclogclose.Enabled := true;
end;

function TDM.CloseOut:boolean;
var
 fn1: string;
begin
  try
    fn1 :=CodeMaker.LogFileName;
    if fn1 = '' then
        info('Log file not open', [], 203042)
    else
    begin
     CodeMaker.LogFileName :='';
     info('Log file %s closed', [fn1], 203043);
     Umain.aMainForm.StatusBar.Panels[4].text:= '';
    end;
  except
  on E: Exception do
    error('Exception occured: %s', [E.message], 103017);
  end;
    aMainForm.Aclogopen.Enabled := true;
   aMainForm.Aclogclose.Enabled := false;
end;

function TDM.GetFloatFormat: string;
begin
  result:=fFloatFormat;
  if result='' then result:='%8.3f';
end;

procedure TDM.SetFloatFormat(const Value: string);
var
 s : string;
 opt: TEpioption;
 p : pchar;
begin
  s := Sysutils.trim(value);
  if s='' then exit;
  if s[1]='"' then
    s := StrRemoveSurroundingQuotes(s);
  fFloatFormat:=s;
end;

function TDM.Correlate(Varnames: TStrings): boolean;
var
   CorMatrix : XMatrix;
   df: TEpiDataFrame;
   checknames: TStringList;
begin
  try
         if not dm.CheckDataOpen() then exit; //checkdataopen();
    CheckNames := TStringList.Create();
    dataframe.GetVectorListByName(VarNames).GetVectorNames(CheckNames);
    CheckVariableNo(CheckNames, 2);
    df := dataframe.prepareDataframe(Varnames, Varnames);
    if df.selectedRowCount > MAXOBSERVATIONS THEN
      error('Max records for %s: %d N was: %d', ['Correlate',MAXOBSERVATIONS,df.selectedrowcount], 103047);
    EpiCorrelate(Varnames,df,CorMatrix);
    OutCorrelate(Varnames,df,CorMatrix);
  finally
    CheckNames.Free;
    if assigned(df) then df.free;
  end;
end;

function TDM.regress(Varnames: TStrings): boolean;
var
   RegRec: TMultStatRec;
   df: TEpiDataFrame;
begin
  try
         if not dm.CheckDataOpen() then exit; //checkdataopen();
    CheckVariableNo(Varnames, 2);
    df := dataframe.prepareDataframe(Varnames, Varnames);
    if df.selectedRowCount > MAXOBSERVATIONS THEN
      error('Max records for %s: %d N was: %d', ['Regression',MAXOBSERVATIONS,df.selectedrowcount], 103047);
    OLinearRegression.EpiRegress(Varnames,df,RegRec);
    OLinearRegression.OutRegress(Varnames,df,RegRec,inttostr(dataframe.selectedRowCount));
  finally
    if assigned(df) then df.free;
  end;
end;

function TDM.OutCorrelate(Varnames: TStrings;df: TEpiDataFrame;const CorMatrix : XMatrix):boolean;
var
  i,co,j,cco,varco       : integer;
  v,categ   : TepiVector;
  s,st         :string;
  tab : TStatTable;
  conlim : EpiFloat;
begin
  varco := varnames.count;
  tab :=nil;
try
  tab:=Outputlist.NewTable(varco+1,varco+1);
  tab.TableType := sttNormal;
  for i:= 0 to varco-1 do
  begin
    tab.Cell[i+2,1]:=varnames[i];
    tab.Cell[1,i+2]:=varnames[i];
  end;
   for i:= 1 to varco do
      for j:= 1 to varco do
           if i <= j then                    //col,row
             tab.Cell[i+1,j+1]:= Epiformat(CorMatrix[i,j],'%4.3f')
           else
             tab.Cell[i+1,j+1]:='';
  codemaker.outputtable(tab,'Total N = ' + Inttostr(dataframe.selectedRowCount) + ' Included: N= ' + Inttostr(df.selectedrowcount));
  Sendoutput;
finally
 tab.free;
end;
end;


function TDM.KWallis(Varnames: TStrings; cmd: TCommand):boolean;
var
   df: TEpiDataFrame;
   Byvar : Tstringlist;
begin
  df := nil;
  try
         if not dm.CheckDataOpen() then exit; //CheckDataOpen();

   // if (Cmd.ParamByName['M'] <> nil) then dm.info('Option /M not implemented yet');

    if (Cmd.ParamByName['BY'] = nil) and (Varnames.Count = 2) then
             dm.Info('Syntax: Kwallis %s /BY=%s', [Varnames[0],Varnames[1]], 203044);

    CheckVariableNo(Varnames, 1, 2);

    //check for data types
    if dataframe.VectorByName[Varnames[0]].DataType in [EpiTyDate,EpiTyUppercase,EpiTyString] then
         dm.error('A date/string variable cannot be processed using this command', [], 103048);

    if Varnames.Count = 1 then
      if Assigned(Cmd.ParamByName['BY']) then
        Varnames.add(Cmd.ParamByName['BY'].AsString)
      else
        dm.Error('Syntax: Kwallis %s /BY="variable-name"', [varnames[0]], 103049);


    if dataframe.VectorByName[Varnames[1]].DataType in [EpiTyString,EpiTyUppercase] then
          dm.error('Category variable must be numerical<br>create numerical variable', [], 103050);

    Byvar := TStringList.Create();
    Byvar.Add(Varnames[0]);

    if Assigned(Cmd.ParamByName['BY']) then
      if (cmd.ParamByName['M'] <> nil) then
        df := dataframe.prepareDataframe(Varnames, byvar)
      else
        df := dataframe.prepareDataframe(Varnames, Varnames)
    else
      df := dataframe.prepareDataframe(Varnames, Varnames);

    // TODO : Rewrite to allow only missing in "by variable"
//    if (cmd.ParamByName['M'] <> nil) then
//      df := dataframe.prepareDataframe(Varnames, byvar)
//    else


    DoKwallis(df, varnames, cmd);
  finally
    //if Assigned(ByVar) then FreeAndNil(ByVar);
    if Assigned(df) then FreeAndNil(df);
  end;
end;



procedure TDM.SetOutputStream(const Value: TStream);
begin
  FGetOutputStream := Value;
end;

function TDM.GetOutputStream: TStream;
begin
  Result:= CodeMaker.GetStream;
end;

procedure TDM.Setgettest(const Value: string);
begin
//does nothing
end;


function TDM.CheckforBreak: boolean;
begin
  application.ProcessMessages;
  Result:=Cancelled;
end;

procedure TDM.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TDM.Setdataframe(const Value: TEpiDataFrame);
begin
  fdataframe := Value;
end;

function TDM.PopulateFldfromVars(const fldname,varname: String): boolean;
var
 v : TEpiVector;
 gv : Ivalue;
 i : integer;
begin
Result:=false;
if dataframe <> nil then
begin
  v := DataFrame.FindVector(fldName);
  if v= nil then exit;
  i:=0;
  while true do
  begin
    inc(i);
    gv :=Executor.findvar(Varname+inttostr(i));
    if gv = nil then break;
    if gv.tag <>  EpiVarResult then break;
    dataframe.UpdateVectorRow(v,gv,i);
  end;
end;
end;


function TDM.CreateNewDataSetFromResults(fn: string;varnames: array of string): TEpiDataFrame;
var
  i,co,maxlen,maxrow         : integer;
  List       : TStringList;
  gv    : IValue;
  vname,vtype, s      :string;
  oldframe :  TEpiDataFrame;
  extyp :TExprType;
  oldecho : boolean;
begin
   Result:=nil;
   List :=nil;
   oldframe :=nil;
  maxrow:=0;
try
   List :=TStringList.create;
   for i:= low(varnames) to high(varnames) do
   begin
       vname :=varnames[i];
       gv :=Executor.findvar(vname+'1');
       maxrow:=0;
       if gv <> nil then
       begin
         if gv.tag <> EpiVarResult then continue;
         co:=1;        maxlen:=10;
         extyp:= gv.ExprType;
         vType := GetFieldTypeName(EpiParserDataTypeToFieldType(extyp));
         while true do
         begin
            gv :=Executor.findvar(format('%s%d',[vname,co+1]));
            if gv=nil then break;
            if extyp=ttString then
            begin
               s := gv.AsString;
               maxlen:=max(length(s),maxlen);
            end;
            inc(co);
         end;
         if extyp=ttString then
            vtype:=format('string(%d)',[max(maxlen,10)]);
         maxrow:= max(maxrow,co);
         vname :=copy(vname,2,MAxint);
         list.add(format(vname+'=gen %s %s=%s',[vtype,vname,smif(extyp=ttString,'""','.')]));
       end;
   end;
   if list.Count > 0 then
   begin
     try
     try
       oldframe :=DataFrame;
       SetAllEchoSwitches(false); //stop talking
       Dataframe:=nil;//simulate file closure
       RunCommandline('generate '+ inttostr(maxrow));
       for i := 0 to list.Count -1 do
       begin
          RunCommandline(list.Values[list.Names[i]]);
          PopulateFldfromVars(list.Names[i],'$'+list.NAmes[i]);
       end;
       SetAllEchoSwitches(true);
       WriteFile(fn,nil);
      except
         raise;
      end;
     finally
        closefile;
        Dataframe :=oldframe;
        SetAllEchoSwitches(true);
     end;
   end;
finally
  List.free;
end;
end;


function TDM.SaveResult(fn: string; varnames: array of string): boolean;
var
 t: DWord;
begin
 Result:=false;
 if fn='' then error('Missing file name', [], 101004);
 t := GetTickcount;
try
   CreateNewDataSetFromResults(fn,varnames);
  t := GetTickCount-t;
except
  On E : Exception do
  begin
   Error('Exception occured: %s', [E.Message], 103017, -1);
   Error('Failed to write %s', [fn], 103051, -1);
  end;
end;
end;


function TDM.Lang(code:integer; origtext:string):string;
begin

  result := OTranslator.Translate(code,origtext);
{  if assigned(FTranslator)
  then result:=FTranslator.Translate(code,origtext)
  else result:=origtext;}
end;

end.

