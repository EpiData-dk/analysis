unit UanaParser;

interface
uses windows,sysutils,UEpidataTypes,classes,dialogs, prExpr,UVariables,AAPasTok,
 UAnaToken, UCommands, UVEctors,SMutils, ansDatatypes,UExpToken,variants;

//const
//applies to all built-in commands

// all constant definitions moved to unit: Uanatoken

Type

TEpiParseVarOption=(PVParseAll,PVstar,PVQuery, PVRange,PVCheckValidName,PVAllowQuotes);
TEpiParseVarOptions= set of TEpiParseVarOption;

TEpiParseCmdOption=(PCAllowIf,PCAllowVarlist,PCAllowOptions);
TEpiParseCmdOptions=set of TEpiParseCmdOption;

TLoopCommand=class(TCommandBlock)
  private
    Flevel: integer;
    Floopvar: TVar;
    Floopvarname: string;
    FItems: TStringList;
    procedure Setlevel(const Value: integer);
    procedure Setloopvar(const Value: TVar);
    procedure Setloopvarname(const Value: string);
    procedure SetItems(const Value: TStringList);
public
//  constructor Create(ploopvar:TVar);
  constructor Create(const ploopvarname, pCommandLines: string;pItems :TStringList);
  procedure SetCmdSeparated(const Value: string; Sep: char);
  function Execute:boolean;
  property level : integer read Flevel write Setlevel;
  property loopvar:TVar  read Floopvar write Setloopvar;
  property loopvarname: string read Floopvarname write Setloopvarname;
  property Items :TStringList read FItems write SetItems;
end;



EAnaExecutor=class(Exception);
TAnaParser=class;

TAnaExecutor=class
  private
//    fForm: TForm;
    fAnaParser: TAnaParser;
    fVariables: TVarList;
    fResultVariables: TVarList;
    fGlobalVariables: TVarList;
    fSystemVariables: TVarList;
    fOnShowOutput: TOnShowOutput;
    fOnOpenFile: TOnOpenFile;
    fOnError: TOnParseError;
    fFileName: string;
    fWorkingDir: string;
    fBackGroundImage: string;
    fShowButton: boolean;
    fGlobalcmdlist: TCommandBlockList;
    fLastSelect: string;
    fCurrentExecLine: integer;
    ExePointer        : integer;
//    FIfCount: integer;
    procedure DoOpenFile(const filename: string;CreateNew,ReadOnly:boolean);
    procedure ParserErrHandler(const msg:string;Token:TSMToken;var Handled:boolean);
    procedure DoShowOutput(const output: string);
    function ExecuteCommandList(cmdlst: TCommandList): boolean;
//Main Identifier function (IDF)
    function VarIDF(const Identifier: String;ParameterList: TParameterList): IValue;
    function ProcessLabels(source, Labels: TStrings): boolean;
    function AttemptLabelJump(const cmdline: string; Labels: TStrings): integer;
    function ProcessCMDs(source: Tstrings; cmdlist: TCommandBlockList{;const pFileName:string}): boolean;
    function ProcessQuery(const cmdline: String): string;
    function GetCurrentExecLine: integer;
    function ExecuteImIF(Exp: IValue): boolean;
    function ParseCommandLine(cmdline: string;DoQuery: boolean=true): TCommandList;
    function GetVariableListByName(varnames: Tstrings; VarScope: integer=-1): TVarList;
public
    constructor Create(pOwner:TObject);
    Destructor Destroy;override;
    function FindVar(const vname: string): IValue;
    function RunBlock(lines,labels: TStringList; cmdlist:TCommandBlockList):integer;
    function EvalExpression(const Expression: String): IValue;
    function NewVar(pVarDesc: TAnaVariableDescriptor): IValue;
    function AddVar( aVar :IValue; VarScope:word): IValue;
    function DropVars(varnames: Tstrings; VarScope: word): boolean;
    function RemoveVar(aVar: IValue; VarScope: word): IValue;
    function ClearVars(VarScope: word): IValue;
    function RunCommandLine(cmdline:string;DoQuery:boolean=true;DoRun:boolean=true):boolean;
    function RunScript(const aFileName:string):Integer;
    function RunScriptAsString(const Script: string): integer;
//    function CloseMenuScript(const FileName:string=''):boolean;
    function ExpandMacros(const code:string):string;
    procedure RunTimeError(const msg:string='Unknown Run time error');
    function Expandlist(items, expandeditems: TStringlist): boolean;
    function FindVector(const vname:string):TEpiVector;
//    property Form: TForm read fForm write fForm;
    property Anaparser :TAnaParser read fAnaParser;
    property Variables :TVarList read fVariables write fVariables;
    property GlobalVariables :TVarList read fGlobalVariables write fGlobalVariables;
    property ResultVariables :TVarList read fResultVariables write fResultVariables;
    property SystemVariables :TVarList read fSystemVariables write fSystemVariables;
    property OnShowOutput:TOnShowOutput read fOnShowOutput write fOnShowOutput;
    property OnOpenFile:TOnOpenFile read fOnOpenFile write fOnOpenFile;
    property OnError :TOnParseError read fOnError write fOnError;
    property FileName : string read fFileName;
    property WorkingDir :string read fWorkingDir write fWorkingDir;
    property Globalcmdlist:TCommandBlockList read fGlobalcmdlist write fGlobalcmdlist;
    property LastSelect : string read fLastSelect write fLastSelect;
    property CurrentExecLine: integer read GetCurrentExecLine write fCurrentExecLine;
//    property IfCount : integer read FIfCount write SetIfCount;
end;


TAnaParser=class(TSMParser)
private
    fAnaExecutor: TAnaExecutor;
    function GetUniqueName: string;
    function ParseStringBlock: boolean;
    function ParseCommandBlock: boolean;
    function ParseKeyWordCommand: TCommand;
    function ParseFilenameWithOptionsCommand(pCommandID:integer; const optSyntax: String = GlobaloptSyntax): TCommand;
    function ParseAppendCommand(pCommandID: integer ; const optSyntax: String = GlobaloptSyntax): TCommand;
    function ParseRouteCommand(pCommandID:integer): TCommand;
    function ParseWriteCommand(pCommandID:integer): TCommand;
    function ParseSaveDataCommand(pCommandID: integer; const optSyntax: String = GlobaloptSyntax ): TCommand;
    function ParseSelectCommand(pCommandID:integer): TCommand;
    function ParseCountCommand(pCommandID:integer): TCommand;
    function ParseHelpCommand(pCommandID: integer): TCommand;
    function ParseParamLessCommand(pCommandID: integer): TCommand;
    function ParseEvalCommand(pCommandID: integer): TCommand;
    function ParseDefineCommand(pCommandID: integer): TCommand;
    function ParseLetCommand(pCommandID: integer;VarName:string=''): TCommand;
//  function ParseDescribeCommand(pCommandID: integer): TCommand;
    function ParseMeansCommand(pCommandID: integer): TCommand;
    function ParseVariableNames(VectorList: TStrings;Options: TEpiParseVarOptions): boolean;
    function ParseRestOfLine(pCommandID: integer): TCommand;
    function ParseOutputCommmand(pCommandID: integer): TCommand;
    function ParseTypeCommand(pCommandID: integer; const optSyntax:string=GlobaloptSyntax): TCommand;
    function ParseGenCommands(pCommandID: integer): TCommand;
    function ParseIfCommand(pCommandID: integer): TCommand;
    function ParseRelateCommand(pCommandID: integer): TCommand;
    function ParseRecodeCommand(pCommandID: integer): TCommand;
    function Parse2FileDosCommand(pCommandID: integer; const optSyntax: String = GlobaloptSyntax ): TCommand;
    function ParseandIgnore(pCommandID: integer): TCommand;
    function ParseSetCommand(pCommandID: integer): TCommand;
    function ParseRegressCommand(pCommandID: integer): TCommand;
    function ParseOptions(const cmd, options: string;const Params: TVarList; AllowUnknown: boolean = false): boolean;
    function ParseLateIfCommand(var Ifexp: IValue): boolean;
    function ParseTypicalCommand(pCommandID: integer;pParseoptions:TEpiParseCmdOptions=[];
                                 const optSyntax:string=GlobaloptSyntax): TCommand;
    function ParseValueLabelCommand(pCommandID: integer): TCommand;
    function ParseVariableCommand(pCommandID: integer): TCommand;
    function ParseGenVarCommands(pCommandID: integer): TCommand;
    function ParseloopCommand(pCommandID: integer): TCommand;
    function ParseExpression(Var Exp:Ivalue;til:TSMTokenTypeSet=[opEndofline,opEndofFile]):boolean;
//    function ParseoutputCommand(pCommandID: integer): TCommand;
    function ParseLabelCommand(pCommandID: integer): TCommand;
protected
//  procedure CmdError(var Params: TVarList; const msg: string);
//  function parseCommandLine: boolean;
    function IsPossibleLetCmd(const VarName:string):boolean;
public
  constructor Create(Parent:TAnaExecutor;const pFilename:string);
  constructor CreateString(Parent:TAnaExecutor;const ParseString:string);
  function IfableCommand(const cmdline: string;var cmd:integer): boolean;
  Function ParseFile:TCommandList;
  property AnaExecutor : TAnaExecutor read fAnaExecutor;
end;

implementation

uses UparserErrors,UCmdProcessor,UVectorVar,Ustack,SGrep, CStrings, Umain, uDateUtils, UDebug;

const
  UnitName = 'UAnaParser';


{ TAnaParser }

function TAnaParser.ParseFile:TCommandList ;
var
 cmd : Tcommand;
begin
  result:=TCommandList.Create(nil);
  try
    while true do
    begin
      NextToken;
      case Currenttoken.TokenType of
        opComment:continue ;
        OpEndofFile: break;
        opEndofLine:continue;
        opKeyword,opIdentifier :
        begin
          cmd:=ParseKeyWordCommand;
          if cmd<> nil then
            Result.add(cmd);
        end;
      end;//case
    end;//while
  except
    if result <> nil then result.free;
    result:=nil;
    raise;
  end;
end;



function TAnaParser.ParseKeyWordCommand:TCommand;
begin
  result:=nil;
  if Currenttoken.TokenGroup = opKeyWord then
  begin
  case Currenttoken.TokenSubType of
    opBegin,
    opEnd: begin exit;end;
    opSet:Result:=ParseSetCommand(opSet);
    opRead: Result:=ParseFilenameWithOptionsCommand(opRead, 'CLOSE KEY CB');
    opAppend: Result:=ParseTypicalCommand(Currenttoken.TokenSubType, [PCAllowVarList], ' FILE KEY ');  //ParseAppendCommand(opAppend, 'KEY') ;
    opRelate:result:= ParseRelateCommand(opRelate);
    opMerge: result:=ParseTypicalCommand(Currenttoken.TokenSubType, [PCAllowVarList], ' FILE KEY UPDATE UPDATEALL TABLE ');
    opSelect:Result:=ParseSelectCommand(opSelect);
    opCount :result:=ParseCountCommand(opCount);

    opEval, opQuery, opAssert:Result:=ParseEvalCommand(Currenttoken.TokenSubType);
    opDEFINE : result:=ParseDefineCommand(opDEFINE);
    opif,opimif: result:=ParseIfCommand(Currenttoken.TokenSubType);

    opErasePng: result:=ParseTypicalCommand(Currenttoken.TokenSubType, [], 'NOCONFIRM ALL D ');
    opPie : result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf], Gnrl_options +  GRAPH_OPTIONS);
    opBar : result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf], Gnrl_options +  GRAPH_OPTIONS +' PCT XALL SORT M ');
    opHistogram, opShortHistogram: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  ' XONLY  SORT M PCT ' + Gnrl_options + GRAPH_OPTIONS);
    opLine: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + GRAPH_OPTIONS + ' M BYC BYS ');
    opScatter, opShortScatter: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + GRAPH_OPTIONS + ' M BYC BYS ');
    opPchart: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf], Gnrl_options +  GRAPH_OPTIONS + SPC_OPTIONS + ' TAB NCI ');
    opIchart: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + GRAPH_OPTIONS + SPC_OPTIONS + ' TAB NCI ');
    opRunChart: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + GRAPH_OPTIONS + SPC_OPTIONS + ' TAB NCI ');
    opPareto: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + GRAPH_OPTIONS);
    opXChart: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + GRAPH_OPTIONS + SPC_OPTIONS + ' TAB NCI ');

    opEpiCurve : result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  ' NT '+ ' '  + Gnrl_options + GRAPH_OPTIONS );

    opCDFPlot : result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  ' AGG P ' + ' '  + Gnrl_options + GRAPH_OPTIONS );
    opCIPlot : result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  ' NM O NL NCI NT ' + Gnrl_options + GRAPH_OPTIONS );
    opDotPlot : result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  ' M DI C' + ' ' +  Gnrl_options + GRAPH_OPTIONS);
    opBox,opboxplot: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + GRAPH_OPTIONS + ' M OUT1 OUT2 OUT P1090 R NT ');

    opFreq :result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  ' M NM CI' + Gnrl_options + PCT_OPTIONS);
    opTables, opShortTables: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf], Gnrl_options + SORT_OPTIONS + STAT_OPTIONS +
                                          PCT_OPTIONS + ' NM FV M S F NT NC NCS ');
    opTableDialog: result:=ParseParamLessCommand(opTableDialog);

    opLifeTable: result := ParseTypicalCommand(Currenttoken.TokenSubType, [PCAllowVarList,PCAllowIf], Gnrl_options + GRAPH_OPTIONS + ' BY W END I ');

    opDescribe, opShortDescribe :result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + 'NM');
    opMeans, opShortMeans: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],  Gnrl_options + 'T E0 E1 E2 E3 BY M ');

    opshortSTables, opStables,opShortAggregate, opAggregate: result := ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],
         Gnrl_options + ' REPLACE SAVE CLOSE M MEAN SD SV MEDIAN SUM MIN MAX P5 P10 P25 P50 P75 P90 P95 P99 ISR IQR IDR DES MV D0 D1 D2');

    opKWallis: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf], Gnrl_Options + ' BY M ');

    opRegress,opCorrelate,opShortCorrelate : result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf]);

    opSort   :result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList]);
    opLet,opAssign: Result:=ParseLetCommand(Currenttoken.TokenSubType);
    opRecode: Result:=ParseRecodeCommand(opRecode);
    opView : Result:=ParseRouteCommand(Currenttoken.TokenSubType);
    opHelpView:result:=ParseRouteCommand(Currenttoken.TokenSubType);
    opSaveData : result:=ParseSaveDataCommand(opSaveData, ' KEY REPLACE');

    opBrowse: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf]);
    opUpdate: result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf], 'ID');

    opQuit, opExit :result:=ParseParamLessCommand(Currenttoken.TokenSubType);
    opList :result:=ParseTypicalCommand(Currenttoken.TokenSubType,[PCAllowVarList,PCAllowIf],'NO V VL');
    opTitle, opEcho:result:=ParseOutputCommmand(Currenttoken.TokenSubType);
    opType: result := ParseTypeCommand(opType, 'H1 H2 H3 H4 H5 CLASS');
    opShow: result := ParseTypeCommand(opShow, 'CLASS');
    opdrop, opRename,opvar,opVariables, opKeep, opResultList: result:=ParseVariableCommand(opVariables);
    opCls :result:=ParseParamLessCommand(opCls);
    opClh :result:=ParseParamLessCommand(opClh);
    opClose :result:=ParseParamLessCommand(opClose);
    opMemory :result:=ParseParamLessCommand(opMemory);
    opVersion :result:=ParseParamLessCommand(opVersion);
    opSaveIniScreen: result :=ParseParamLessCommand(opSaveIniScreen);

    opRunTest: result := ParseFilenameWithOptionsCommand(CurrentToken.TokenSubType, ' HALT ');
    opSavepgm,opRun: result:=ParseRestOfLine(Currenttoken.TokenSubType);

    opGen: Result:=ParseGenVarCommands(Currenttoken.TokenSubType);
    opGenerate: result:=ParseGenCommands(opGenerate);

    opCloseHelp, opPrintViewer,
    opNewPage: result := ParseParamLessCommand(Currenttoken.TokenSubType);


    opDir, opERASE, opCD,opDOS, opDoDos,
      opMkDir: result:=ParseRestOfLine(Currenttoken.TokenSubType);
    opCopyFile: result:=Parse2FileDOSCommand(Currenttoken.TokenSubType, ' REPLACE ');

    opLogOpen:result:=ParseFilenameWithOptionsCommand(Currenttoken.TokenSubType, 'REPLACE CLOSE APPEND CONTINUE C');
    opLOGCLOSE:result:=ParseParamLessCommand(opLogClose);

    opIMMEDIATE: ParseandIgnore(Currenttoken.TokenSubType);
    opHelp:result:=ParseRestOfLine(opHelp);
    oploop:result:=ParseloopCommand(oploop);
    opEdit:result:=ParseFilenameWithOptionsCommand(opEdit);
    opLabel: result := ParseLabelCommand(opLabel);
    opLabelData: result := ParseRestOfLine(opLabelData);
    opLabelValue, opMissingValue: result := ParseValueLabelCommand(CurrentToken.TokenSubType)
  else
      //error(Currenttoken,'Command not implemented yet');
    dm.error('%s : Command not implemented yet', [Currenttoken.Token], 101001);
  end;//case
  end else if Currenttoken.TokenGroup = opIdentifier then
  begin
     if IsPossibleLetCmd(Currenttoken.token) then
       Result:=ParseLetCommand(opLet,Currenttoken.token)
     else
       //error(Currenttoken,'Unknown Command');
       dm.error('%s : Unknown Command', [Currenttoken.Token], 101002);
//      dm.error(Currenttoken.Token + ' : Command not implemented yet');
  end
  else
     //error(Currenttoken,'Unknown Command ');
     dm.error('%s : Unknown Command', [Currenttoken.Token], 101002);
end;


function TAnaParser.GetUniqueName:string;
var
 i : integer;
begin
  i:=1;
  result:=format('NI%d',[i]);
end;

function CleanMenuName(const aname:string):String;
var
  p, alen : integer;
begin
  if aname='' then exit;
  alen:=length(aname);
  p:= pos('^',aname);
  if (p < 1) or (p=alen) or not(aname[P+1] in['#','^']) then
  begin
    result:=aname;
    exit;
  end;
  result:=copy(aname,1,p-1);
  while p<alen do
  begin
    if not (aname[p] in ['^','#',#48..#57]) then
     result:=result+ aname[p];
    inc(p);
  end;
end;



function TAnaParser.ParseStringBlock: boolean;
Begin
  NextToken;
  If Currenttoken.TokenSubType = opEnd then exit;
  If Currenttoken.TokenSubType <> opBegin then Error(Currenttoken,begin_expected);
  Accept([opEndofLine]);
//  While parseStringItem  do;
  If Currenttoken.TokenSubType <> opend then
    Error(Currenttoken,end_expected);
  Accept([opEndofLine,opEndofFile]);
End;

function TAnaParser.ParseCommandBlock: boolean;
//var
// cmdblk : TcommandBlock;
Begin
(*  NextToken;
  If token.TokenSubType = opEnd then exit;
  If token.TokenSubType <> opBegin then Error(token,begin_expected);
  Accept([opEndofLine]);
  While parseCommandLine  do;
  If token.TokenSubType <> opend then
    Error(token,end_expected);
//    parser.Error(token,end_expected);
  Accept([opEndofLine,opEndofFile]);
  ThisCmdBlk:=nil;
*)
End;

constructor TAnaParser.Create(Parent:TAnaExecutor;const pFilename: string);
begin
 inherited CreateToken(TSMAnaTokenizer.CreateFile(pFilename),Parent.VarIDF);
 fAnaExecutor:=Parent;
end;

constructor TAnaParser.CreateString(Parent: TAnaExecutor; const ParseString: string);
begin
 inherited CreateToken(TSMAnaTokenizer.CreateString(ParseString),Parent.VarIDF);
 fAnaExecutor:=Parent;
end;

function TAnaParser.ParseFilenameWithOptionsCommand(pCommandID:integer; const optSyntax: String = GlobaloptSyntax):TCommand;
var
  fn : string;
  Params :TVarList;
  Param : IValue;
begin
  Param:=nil;  Result:=nil;
  Params :=TVarList.Create;
  if NextToken.TokenType <> opDivide then
  begin
   fn := CurrentToken.Token;
   Nexttoken;
  end;
  if fn <> '' then
    Params.AddVar(TVar.Create('FILENAME', fn));
  if (CurrentToken.TokenType in [opEndofLine,opEndofFile,opDivide]) then
    ParseOptions('',optSyntax,Params)
  else
    error(CurrentToken, 'Add " " around filenames.');
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseRelateCommand(pCommandID:integer):TCommand;
var
 fn,VarName : string;
 Params :TVarList;
 Param : IValue;
begin
  result:=nil;
  VarName :=NextToken.Token;
  if not IsValidEpiIdent(VarName) then
          error(Currenttoken,' invalid variable name '+Varname);
  fn :=trim(GetRestOfLine);
  if fn='' then error(Currenttoken,'Missing file name');
  Params :=TVarList.Create;
  Params.AddVar(TVar.Create('FILENAME',fn));
  Params.AddVar(TVar.Create('VARNAME',VarName));
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseAppendCommand(pCommandID: integer; const optSyntax: String = GlobaloptSyntax ): TCommand;
var
 fn,VarName : string;
 Params :TVarList;
 Param : IValue;
begin
  Param:=nil;  Result:=nil;
  Params :=TVarList.Create;
  if NextToken.TokenType <> opDivide then
  begin
   fn := CurrentToken.Token;
   Nexttoken;
  end;
  if fn <> '' then
    Params.AddVar(TVar.Create('FILENAME', fn));
  // need to add possible key:
  if (CurrentToken.TokenType in [opEndofLine,opEndofFile,opDivide]) then
    ParseOptions('',optSyntax,Params);

  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseLabelCommand(pCommandID: integer): TCommand;
var
  Params: TVarList;
  Param: IValue;
  varname, text: string;
begin
  Params := TVarList.Create;
  varname := NextToken.Token;
  Params.AddVar(TVar.Create('VARNAME', varname));
  text := NextToken.Token;
  Params.AddVar(TVar.Create('VARLABEL', text));
  text := trim(ReadToEOL());
  if text <> '' then
    dm.Info('add " "', [], 201001);
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseValueLabelCommand(pCommandID: integer): TCommand;
var
  List : TStringList;
  options:TEpiParseVarOptions;
  Params :TVarList;
begin
  Params:=nil;
  List :=TStringList.create;
  try
    Params := TVarList.Create;
    include(options, PVCheckValidName);
    if ParseVariableNames(list, options) then
      Params.AddVar(TVar.Create('VARLIST',integer(list)));
    NextToken;
    if Currenttoken.TokenType=opDivide then
    begin
      ParseOptions('', 'CLEAR', Params, True);
    end;
    result:=TCommand.Create(pCommandID,Params);
  except
    FreeAndNil(list);
    Params:=nil;
    raise;
  end;
end;

function TAnaParser.ParseVariableCommand(pCommandID:integer):TCommand;
var
 CmdName,VarName : string;
 Params :TVarList;
 list : TStringList;
 subcmd : integer;
begin
 CmdName := '';
 result := nil;
 Params := nil; list := nil;
 Params := TVarList.Create;
 if CurrentToken.TokenSubType in [opKeep, opDrop, opResultList, opRename] then
   CmdName := CurrentToken.Token
 else
   CmdName := NextToken.Token;
 if Cmdname<>'' then
 begin
   subcmd := CurrentToken.TokenSubType;
   case subcmd of
     opDrop, opKeep:
         begin
           list :=TStringList.create;
           if ParseVariableNames(list,[PVCheckValidName]) then
             Params.AddVar(TVar.Create('VARLIST',integer(list)));
         end;
     opResultList:
         if not(LookAheadToken.TokenType in [opEndOfFile, opEndOfline]) then
           if AnsiUppercase(NextToken.Token) <> 'CLEAR' then
             error(Currenttoken, ' Unknown subcommand '+Currenttoken.token)
           else
             CmdName := CurrentToken.Token;
     opTemp:
         begin
           CmdName := NextToken.Token;
           if (AnsiUppercase(CmdName) <> 'CLEAR') then
             error(Currenttoken, ' Unknown subcommand '+Currenttoken.token);
         end;
     opRename:
         begin
//           dm.info('rename variables not implemented yet');
           VarName := NextToken.Token;
           if not IsValidEpiIdent(VarName) then
             error(Currenttoken,' invalid variable name '+Varname);
           Params.AddVar(TVar.Create('VARNAME',VarName));
           if uppercase((NextToken.Token)) <> 'TO' then
             error(CurrentToken, 'Keyword TO required between variables');
           VarName :=NextToken.Token;
           if not IsValidEpiIdent(VarName) then
               error(Currenttoken,' invalid variable name '+VarName);
           Params.AddVar(TVar.Create('NEWVARNAME',VarName));
         end
     else
         begin
           error(Currenttoken,' Unknown subcommand '+Currenttoken.token);
         end;
   end;//case
   Params.AddVar(TVar.Create('SUBCOMMAND',AnsiUppercase(CmdName)));
 end;
 result:=TCommand.Create(pCommandID,Params);
end;



function TAnaParser.ParseOptions(const cmd,options:string;const Params :TVarList; AllowUnknown: boolean = false):boolean;
var
 lcmd,opname,tok : string;
 Param : IValue;
 oplist, cmdlist : TStringList;
 i, coOp, CmdLen : integer;
 tokenset: TSMTokenTypeSet;
begin
  oplist :=nil;
try
  result:=true;
  oplist := TStringList.Create;
  SplitString(Ansiuppercase(options),oplist);
  coOp := oplist.count;
  tokenset := [opIdentifier,opString];
  if AllowUnknown then include(tokenset, opNumber);
  if (coOp > 0) then
  begin
    while true do
    begin
        //assume current token is forslash
        if (Currenttoken.TokenType in [opEndofLine,opEndofFile]) then break;
        tok:='';
        opname:= accept(tokenset, 'Invalid syntax').token;

        if not ((LookAheadToken.TokenType in [opEndofLine,opEndofFile,opDivide]) or
                (LookAheadToken.TokenSubType = opIF)) then
        begin
          if nexttoken.token <> '=' then error(Currenttoken,format('= sign expected at %s', [Currenttoken.Token]));
          tok := nexttoken.Token;
          if not (Currenttoken.TokenType in [opNumber,opIdentifier,opString]) then error(Currenttoken,'Invalid option value');
        end;

        if (oplist.IndexOf(opname)> -1) or AllowUnknown then
        begin
          Params.AddVar(TVar.Create(AnsiUppercase(opname),tok), true);
        end
        else
           error(Currenttoken,'Unknown option '+ opname);

        if LookAheadToken.TokenSubType = opIf then
        begin
          nexttoken;
          break;
        end;
        accept([opDivide,opEndofLine,opEndofFile],'Invalid syntax');
    end;//while
  end //coOp> 0
  else
     error(Currenttoken,'Options not allowed');
finally
   oplist.free;
end;
end;



function TAnaParser.ParseSetCommand(pCommandID:integer):TCommand;
var
 Params :TVarList;
 Param : IValue;
 tok,opt,val:string;
 Rnd,isElse, range,equal,comma :boolean;
 SetList:TStringList;
 i, co :integer;
 option : TEpiOption;
begin
  result:=nil;
  try
    SetList:=TStringList.Create;
  //  while true do   // Discarded while loop - why was it here in the first place??? (Torsten. Jan. 2008)
  //  begin
      tok:= accept([opidentifier,opEndofLine,opEndofFile], 'Invalid syntax').token;
      if Currenttoken.TokenType in [opEndofLine,opEndofFile] then
      begin
        Params :=TVarList.Create;
        Params.AddVar(TVar.Create('VARLIST',integer(SetList)));
        result:=TCommand.Create(pCommandID,Params);
        exit;
      end;
      while (nexttoken.Token<>'=') and not (Currenttoken.tokentype in [opEndofLine,opEndofFile]) do
          tok := tok +' '+Currenttoken.Token;
      if Currenttoken.token <> '=' then error(Currenttoken,'= sign expected');
      val := ReadToEOL;
  //    val:= nexttoken.Token;  // (Torsten. Jan. 2008)
      if val = '?' then         // show the value of that one option
      begin
        dm.GetOptionValue(tok,option);
        dm.info('%s = %s', [tok, option.value], 201002);
        FreeAndNil(setlist);
        exit;
      end else begin
//        if not (Currenttoken.TokenType in [opNumber, opIdentifier, opString]) then
//          error(Currenttoken, 'Invalid option value');
//        if Currenttoken.TokenType = opString then
//          val:=Ansiquotedstr(val,'"');
        SetList.Add(tok+'='+ val);
      end;
  //  end;//while
    Params :=TVarList.Create;
    Params.AddVar(TVar.Create('VARLIST',integer(SetList)));
    result:=TCommand.Create(pCommandID,Params);
  except
    SetList.free;
  end;
end;


function TAnaParser.ParseRecodeCommand(pCommandID:integer):TCommand;
var
 Params :TVarList;
 Param : IValue;
 dstvar,srcvar, tok,op:string;
 Rnd,isElse, range,equal,comma, paren:boolean;
 Recode : TAnaReCodeData;
 RecodeList:TStringList;
 vhigh, vcode, vlow: variant;
 i, co :integer;
begin
  result:=nil;
  RecodeList:=nil;
  Accept([opIdentifier]);
  srcvar:=Currenttoken.Token;
  if not IsValidEpiIdent(srcvar) then
          error(Currenttoken,' invalid variable name '+srcvar);
  Rnd:= AnsiUppercase(srcvar)='RND';
  RecodeList:=TStringList.create;
  tok := AnsiUppercase(lookaheadtoken.Token);
  if not (tok = 'TO') then
            dm.info('Note: Recode without "TO" destroys data.', [], 201003);
  //        error(Currenttoken,' TO missing, e.g. <br> Recode '+srcvar+' TO grp'+srcvar +' by 10');
  if (tok='AS') or (tok='TO') then
  begin
    Nexttoken;
    Accept([opIdentifier]);
    dstvar:=Currenttoken.Token;
    if not IsValidEpiIdent(dstvar) then
        error(Currenttoken,' invalid variable name '+dstvar);
    tok := lookaheadtoken.Token;
  end;
  if rnd then
  begin
    op:='RND';
    if (dstvar='') then
       error(Currenttoken,'Random recoding requires destination variable');
    co:=0;
    while true do
    begin
        vlow:= accept([opNumber,opidentifier,opPeriod,opEndofLine,opEndofFile],'Invalid recode options').token;
        if CurrentToken.TokenSubType = opIf then
          Error(CurrentToken, 'If not allowed in RECODE command');
        if Currenttoken.TokenType in [opEndofLine,opEndofFile] then
          break;
        if Currenttoken.TokenType=opNumber then
        begin
          if nexttoken.Token<>'%' then
            error(Currenttoken,'% sign expected')
        end else begin
          if nexttoken.TokenSubType <> opElse then
            error(Currenttoken,'Else or number is expected');
          isElse:=true;
        end;
        if nexttoken.TokenSubType <> opEq then
              error(Currenttoken,'= sign expected');
        vcode:= accept([opNumber,opidentifier,opstring,opPeriod],'Invalid recode options').token;
        Recode := TAnaReCodeData.Create(vlow,vhigh,vcode,op,isElse);
        inc(co);
        RecodeList.AddObject(format('RECODE%d',[i]),Recode);
    end;//whike
  end //rnd
  else
  begin
 // recode must be to new variable:
    if (dstvar='') then  dstvar:=srcvar;
    tok := AnsiUppercase(lookaheadtoken.Token);
    if (tok='BY') then
    begin
       nexttoken;
       op:='BY';
       accept([opNumber,opidentifier],'Invalid recode options');
       vlow:=Currenttoken.Token;// either a varibale name or number
       Recode := TAnaReCodeData.Create(vlow,vhigh,vcode,op,isElse);
       inc(co);
       RecodeList.AddObject(format('RECODE%d',[i]),Recode);
       if NextToken.TokenSubType = opIf then
         Error(CurrentToken, 'If not allowed in RECODE command');
    end
    else
    begin
      while true do
      begin
          range:=false;equal:=false;comma:=false; isElse:=false; paren:=false;
          vlow:= accept([opOpenParen,opNumber,opstring,opidentifier,opPeriod,opEndofLine,opEndofFile],'Invalid recode options').token;
          if CurrentToken.TokenSubType = opIf then
            Error(CurrentToken, 'If not allowed in RECODE command');
          if Currenttoken.TokenType in [opEndofLine,opEndofFile] then break;
          // We are possibly dealing with a negative number...
          if Currenttoken.TokenType=opOpenParen then
          begin
            accept([opMinus], 'Invalid recode option');
            vlow := '-' + accept([opNumber], 'Invalid recode option').Token;
            accept([opCloseParen], 'Invalid recode option');
          end;
          if Currenttoken.TokenType=opidentifier then
          begin
             if AnsiUppercase(vlow)='LO' then range:=true;
             if AnsiUppercase(vlow)='ELSE' then isElse:=true;
          end;
          if Currenttoken.TokenType=opString then
             vlow:=Ansiquotedstr(vlow,'"');
          tok := accept([opNumber,opEq,opComma,opMinus,opPeriod, opstring,opidentifier],'Invalid recode options').token;
          if range and (tok<>'-') then error(Currenttoken,'- sign expected');
          if tok='=' then equal:=true
          else if tok='-' then range:=true
          else if tok=',' then comma:=true;
          if Iselse and not equal then error(Currenttoken,'= sign expected');
          if comma then
          begin
             while true do
             begin
                vhigh := accept([opNumber,opstring,opidentifier,opPeriod],'Invalid recode options').token;
                if Currenttoken.TokenType=opString then
                 vhigh:=Ansiquotedstr(vhigh,'"');
                vlow:=vlow+','+vhigh;
                accept([opEq,opComma]);
                if Currenttoken.TokenType=opEq then break;
             end;//while
             equal:=true;
           end;
          vhigh := accept([opOpenParen,opNumber,opstring,opidentifier,opPeriod],'Invalid recode options').token;
          // We are possibly dealing with a negative number...
          if Currenttoken.TokenType=opOpenParen then
          begin
            accept([opMinus], 'Invalid recode option');
            vhigh := '-' + accept([opNumber], 'Invalid recode option').Token;
            accept([opCloseParen], 'Invalid recode option');
          end;
          if Currenttoken.TokenType=opString then
             vhigh:=Ansiquotedstr(vhigh,'"');
          if equal or isElse then
          begin
             vcode:=vhigh;
             vhigh:='';
          end
          else if range then
          begin
             accept([opEq],'= sign is expected');
             vcode:= accept([opNumber,opidentifier,opstring,opPeriod],'Invalid recode options').token;
             if Currenttoken.TokenType=opString then
                vcode:=Ansiquotedstr(vcode,'"');
          end;
          if range then op:='RANGE'
          else if equal then op:='EQUAL';
          if comma then op:='COMMA';
          if isElse then op:='ELSE';
          Recode := TAnaReCodeData.Create(vlow,vhigh,vcode,op,isElse);
          inc(co);
          RecodeList.AddObject(format('RECODE%d',[i]),Recode);
      end;//whike
   end;//
  end;
  if co=0 then
       error(Currenttoken,'Recoding options are missing');
  Params :=TVarList.Create;
  Params.AddVar(TVar.Create('OPERATION',op));
  Params.AddVar(TVar.Create('SRCVAR',srcvar));
  Params.AddVar(TVar.Create('DSTVAR',dstvar));
  Params.AddVar(TVar.Create('VARLIST',integer(RecodeList)));
  result:=TCommand.Create(pCommandID,Params);
end;

//deprecated command to be replaced by set obs on
function TAnaParser.ParseGenCommands(pCommandID:integer):TCommand;
var
 expStr : string;
 Params :TVarList;
 Param, exp : IValue;
begin
  result:=nil;
  expStr:=trim(GetRestOfLine);
  exp := nil;
  if expstr <> '' then
    exp := GenExpression(expStr,AnaExecutor.VarIDF);
  Params :=TVarList.Create;
  Params.AddVar(TVar.Create('EXP',exp));
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseRestOfLine(pCommandID:integer):TCommand;
var
 s : string;
 Params :TVarList;
 Param : IValue;
begin
  result:=nil;
  s :=trim(GetRestOfLine);
  Params :=TVarList.Create;
  if s<>'' then Params.AddVar(TVar.Create('ROL',s));
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.Parse2FileDOSCommand(pCommandID:integer; const optSyntax: String = GlobaloptSyntax):TCommand;
var
 fn1, fn2 : string;
 Params :TVarList;
 Param : IValue;
begin
  result:=nil;
  fn1 := NextToken.Token;
  if fn1 = '' then
    raise Exception.Create('Source filename missing.');
  if not ValidFileName(fn1) then
    Error(CurrentToken, 'Malformed filename.');
  fn2 := NextToken.Token;
  if fn2 = '' then
    raise Exception.Create('Destination filename missing.');
  if not ValidFileName(fn2) then
    Error(CurrentToken, 'Malformed filename.');
  Params :=TVarList.Create;
  if fn1<>'' then Params.AddVar(TVar.Create('FROMFILENAME',fn1));
  if fn2<>'' then Params.AddVar(TVar.Create('TOFILENAME',fn2));
  NextToken;
  if CurrentToken.TokenSubType = opDivide then
    ParseOptions('', optSyntax, Params)
  else if not (CurrentToken.TokenGroup in [opEndOfFile, opEndOfLine]) then
    Error(currenttoken, 'Incorrect syntax or invalid filname.');
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseandIgnore(pCommandID:integer):TCommand;
begin
  result:=nil;
  ReadtoEOL;
end;

function TAnaParser.ParseRouteCommand(pCommandID: integer): TCommand;
begin
  result:= ParseFilenameWithOptionsCommand(pCommandID);
end;

function TAnaParser.ParseSaveDataCommand(pCommandID: integer; const optSyntax: String = GlobaloptSyntax ): TCommand;
var
 Recfile : string;
 Params :TVarList;
 Param : IValue;
 list : TStringList;
 options:TEpiParseVarOptions;
begin
  Param := nil;
  Result := nil;
  Params := TVarList.Create;
  list := TStringList.create;
  include(options, PVCheckValidName);
  if not (NextToken.TokenType = opDivide) then
  begin
    Recfile := CurrentToken.Token;
    if Recfile <> '' then
    begin
      Params.AddVar(TVar.Create('FILENAME', RECFILE));
      if ParseVariableNames(list,options) then
      begin
        Params.AddVar(TVar.Create('VARLIST',integer(list)));
      end;
    end;
    NextToken;
  end;
  // add other options:
  if (CurrentToken.TokenType in [opEndofLine,opEndofFile,opDivide]) then
    ParseOptions('', optSyntax,Params);

  result:=TCommand.Create(pCommandID,Params);
end;


function TAnaParser.ParseWriteCommand(pCommandID: integer): TCommand;
var
 Recfile : string;
 Params :TVarList;
 Param : IValue;
 list : TStringList;
 options:TEpiParseVarOptions;
begin
 Param:=nil; Params:=nil;
 result:=nil;
 error(Currenttoken,'Write obsolete use: Savedata ' + nexttoken.token);
 {
 list :=TStringList.create;
 include(options,PVCheckValidName);
 Recfile :=NextToken.Token;
 if Recfile='RECFILE' then
  begin
     Params :=TVarList.Create;
     Params.AddVar(TVar.Create('RECFILE',RECFILE));
     if ParseVariableNames(list,options) then
     begin
          Params.AddVar(TVar.Create('VARLIST',integer(list)));
      end;
    result:=TCommand.Create(pCommandID,Params);
  end
  else
  begin
    error(Currenttoken,'Only RECFILE is allowed here');
  end;
  }
end;

function TAnaParser.ParseParamLessCommand(pCommandID:integer):TCommand;
begin
  NextToken;
  if not(Currenttoken.TokenType in [opEndofline, opEndofFile])then
     error(Currenttoken, 'Syntax error');
  result:=TCommand.Create(pCommandID,nil);
end;


function TAnaParser.ParseSelectCommand(pCommandID: integer): TCommand;
var
 expStr : string;
 Params :TVarList;
 Param, exp : IValue;
begin
  result:=nil;
  try
    IgnoreError:=true;
    if LookAheadToken.TokenSubType = opIf then NextToken;
    expStr:=trim(ReadToEOL);
    exp := nil;
    if expstr <> '' then
      exp := GenBooleanExpression(expStr,AnaExecutor.VarIDF);
    AnaExecutor.LastSelect:=expStr;
    IgnoreError:=false;
  except
    exp:=nil;
    IgnoreError:=false;
    raise;
  end;
  Params :=TVarList.Create;
  Params.AddVar(TVar.Create('BOOLEXP',exp));
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseCountCommand(pCommandID: integer): TCommand;
var
 expStr : string;
 Params :TVarList;
 Param, exp : IValue;
 quiet: boolean;
 iPos: integer;
begin
  result:=nil;
  quiet := false;
  Params :=TVarList.Create;
  try
    IgnoreError:=true;
    if LookAheadToken.TokenSubType = opIf then NextToken;
    expStr:=trim(ReadToEOL);
    exp := nil;
    iPos := pos('//', expStr);
    if iPos > 0  then
      expStr := copy(expStr,1,pos('//',expstr)-1);

    iPos := pos('/Q', AnsiUppercase(expStr));
    if iPos > 0  then              // cannot find '/Q'  Why ??
    begin
      quiet := true;
      expStr := Copy(expStr, 1, iPos-1) +
                  Copy(expStr, iPos+2, length(expStr));
      Params.AddVar(TVar.Create('Q','Q'));
    end;

    if expstr <> '' then
      exp := GenBooleanExpression(expStr,AnaExecutor.VarIDF);

    if exp<>nil then
    begin
      exp:= TVar.Create('TEMPIF',exp);
      Params.AddVar(exp);
    end;

    IgnoreError:=false;
  except
    exp:=nil;
    IgnoreError:=false;
    exit;
  end;
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.IfableCommand(const cmdline:string;var cmd:integer): boolean;
var
  s :string;
  aToken: TSMTOken;
begin
  result:=false;
  s:=trim(cmdline);
  if s='' then exit;
  aToken.Token :=GetVarname(s,1);
  Tokenizer.GetTokenClass(aToken);
//  if aToken.TokenGroup=opInvalidToken then exit;
  result:=aToken.TokenSubType in [oplet,opType];
  if result then begin cmd:=aToken.TokenSubType; exit;end;
  if aToken.TokenGroup in [opOperator,opKeyword] then
     begin cmd:=aToken.TokenType; exit;end;
  result:= IsValidEpiIdent(aToken.Token);
  if result then cmd:=opIdentifier;
  //delete cmd
  s:=trim(stringreplace(s,aToken.Token ,'',[rfReplaceAll, rfIgnoreCase]));
  if s='' then exit;
  if s[1]='=' then result:=true;
  if result then begin cmd:=opLet;exit;end;
end;


function TAnaParser.ParseIfCommand(pCommandID: integer): TCommand;
var
 expStr,boolstr,Tstr,Fstr : string;
 Params :TVarList;
 Param, exp : IValue;
 ithen,ielse,acmd:integer;
begin
  result:=nil;
  ParseExpression(exp,[opthen]);
  if exp=nil then
     error(Currenttoken,'Missing boolean expression');
  if  Currenttoken.TokenSubType <> opThen
     then error(Currenttoken,'Then is missing');
  TStr:=trim(ReadToEOL);
  Params :=TVarList.Create;
  Params.AddVar(TVar.Create('BOOLEXP',exp));

//immdediate if nothing is allowed after if
  if pCommandid= opImIf then
  begin
    if (Tstr<>'') and (copy(Tstr,1,2)<>'!!') then
       error(Currenttoken,'Immediate if: commands are not allowed on the same line');
  end
  else
  begin
    if tstr='' then
       error(Currenttoken,'Missing command after then');
    ielse:= pos('ELSE',AnsiUpperCase(Tstr));
    if ielse>0 then
    begin
        fstr:=trim(copy(tstr,ielse+4,MaxInt));
        if fstr='' then
          error(Currenttoken,'Missing command after else');
        if not IfableCommand(fstr,acmd) then
          error(Currenttoken,'Only assign,let,type or assignment are allowed in an if statement');
        Tstr:=trim(copy(tstr,1,ielse-1));
        Params.AddVar(TVar.Create('FSTR',fstr));
    end;
    if not IfableCommand(tstr,acmd) then
       error(Currenttoken,'Only assign,let,type or assignment are allowed in an if statement');
     Params.AddVar(TVar.Create('TSTR',Tstr));
  end;
  result:=TCommand.Create(pCommandID,Params);
end;





function TAnaParser.ParseOutputCommmand(pCommandID: integer): TCommand;
var
 Str : string;
 Params :TVarList;
 Param, exp : IValue;
begin
  result:=nil;
  Str:=trim(GetRestOfLine);
  Params :=TVarList.Create;
  Params.AddVar(TVar.Create('STR',str));
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseTypeCommand(pCommandID: integer; const optSyntax:string=GlobaloptSyntax): TCommand;
var
  fn : string;
  Params :TVarList;
  Param : IValue;
begin
  Param:=nil;  Result:=nil;
  Params :=TVarList.Create;
  if NextToken.TokenType <> opDivide then
  begin
   fn := CurrentToken.Token;
   Nexttoken;
  end;
  if fn <> '' then
    Params.AddVar(TVar.Create('STR', fn));
  if (CurrentToken.TokenType in [opEndofLine,opEndofFile,opDivide]) then
    ParseOptions('',optSyntax,Params)
  else
    error(CurrentToken, 'Add " " around output text.');
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseVariableNames(VectorList: TStrings;Options:TEpiParseVarOptions):boolean;
var
 varname : string;
 Params :TVarList;
 Param, exp : IValue;
 lOptions:TEpiParseVarOptions;
 AllowedToken,TerminatingToken: TSMTokenTypeSet;
begin
  result:=false;
  loptions:=[];
  AllowedToken:=[opIdentifier];
  if PVAllowQuotes in options then
      include(AllowedToken,opString);
  TerminatingToken:=  [opdivide,opif,opDo];
  VectorList.clear;
  if (lookaheadtoken.TokenSubType in TerminatingToken ) then exit;

  varname :=trim(ReadTochar);
  if varname='' then exit;
  if pos('*',varname) > 0 then include(loptions,PVStar);
  if pos('?',varname) > 0 then include(loptions,PVQuery);
  if pos('-',varname) > 0 then include(loptions,PVRange);
  if ([PVStar,PVRange] * loptions)= [PVStar,PVRange] then
      error(Currenttoken,' the star wildcard cannot be mixed with a range '+Varname);
  if ([PVQuery,PVRange] * loptions)= [PVQuery,PVRange] then
      error(Currenttoken,' the query wildcard cannot be mixed with a range '+Varname);
  if loptions <>[] then
  begin
     VectorList.add(varname);
     options:=options*loptions;
     result:=true;
     exit;
  end;
  if PVCheckValidName in options then
      if not IsValidEpiIdent(VarName) then
         error(Currenttoken,' invalid variable name '+Varname);   
  VectorList.add(varname);
  while true do
  begin
    if not (lookaheadtoken.TokenType in AllowedToken) or
       (lookaheadtoken.TokenSubType in TerminatingToken) then break;
    varname:=NextToken.token;
    if PVCheckValidName in options then
        if not IsValidEpiIdent(VarName) then
          error(Currenttoken,' invalid variable name '+Varname);
    VectorList.add(varname);
  end;
  result:=true;
end;



function TAnaParser.ParseLateIfCommand(var Ifexp : IValue):boolean;
var
 expStr : string;
begin
  Result:=false;
  // if Currenttoken.TokenSubType = opIF then   // - This does NOT work along with parsing [let] <exp> IF <exp>...
  if AnsiUppercase(CurrentToken.Token) = 'IF' then
  begin
    expStr:=trim(ReadToChar('/'));
    // Make sure we can parse dates using "/" as sepeartor.
    // Assume if a number is following "/" then we have a date.
    NextToken();
    if not (LookAheadToken.TokenType in [opNumber, opEndOfFile, opEndOfLine]) then
      exit;
    while LookAheadToken.TokenType = opNumber do
    begin
      expStr := expStr + '/' + trim(ReadToChar('/'));
      NextToken();
      if not (LookAheadToken.TokenType in [opNumber, opEndOfFile, opEndOfLine]) then
        exit;
    end;

    Ifexp := nil;
    if expstr <> '' then
      Ifexp := GenBooleanExpression(expStr,AnaExecutor.VarIDF);
    if ifexp<>nil then
    begin
      AnaExecutor.LastSelect := expstr;
      ifexp:= TVar.Create('TEMPIF',Ifexp);
      result:=true;
    end;
  end;
end;


function TAnaParser.ParseLetCommand(pCommandID: integer;VarName:string): TCommand;
var
 expStr : string;
 Params :TVarList;
 Param, exp : IValue;
begin
  result:=nil;
  if VarName='' then
    VarName :=NextToken.Token;
  if not IsValidEpiIdent(VarName) then
          error(Currenttoken,' invalid variable name '+Varname);
  if nextToken.TokenSubType <> opEq then
          error(Currenttoken,'= is expected');
  // Should instead be - parse to IF! But will probably break the GenExpression
  // since the tokenizer is destroyed in the process.... :?
  expStr:=trim(ReadToEOL);
  exp := GenExpression(expStr,AnaExecutor.VarIDF);
  // TODO : Read to EOL makes the following lines make no sense! Torsten (March 2007).
  if CurrentToken.TokenSubType = opThen then
    Error(CurrentToken, 'Incorrect use of keyword: THEN');
  if exp <> nil then
  begin
    ParseLateIfCommand(param);
    Params :=TVarList.Create;
    Params.AddVar(TVar.Create('VARNAME',Varname));
    Params.AddVar(TVar.Create('EXP',exp));
    if assigned(param) then
      Params.AddVar(param);
    result:=TCommand.Create(pCommandID,Params);
  end;
end;



function TAnaParser.ParseEvalCommand(pCommandID: integer): TCommand;
var
 Params :TVarList;
 Param, exp : IValue;
begin
  result:=nil;
  ParseExpression(exp);
  if exp <> nil then
  begin
    Params :=TVarList.Create;
    Params.AddVar(TVar.Create('EXP',exp));
    result:=TCommand.Create(pCommandID,Params);
  end;
end;

function TAnaParser.ParseExpression(var Exp: Ivalue;til: TSMTokenTypeSet): boolean;
begin
   Tokenizer.ExprDelimiters:=til;
   Exp:= Expression(Tokenizer,IdentifierFunction);
end;


function TAnaParser.ParseGenVarCommands(pCommandID: integer): TCommand;
var
 VarName, expStr : string;
 Params :TVarList;
 Param, exp : IValue;
 vType, vLen, Vdec, Vscope : integer;
 vFormat, def      :string;
 vardesc: TAnaVariableDescriptor;
 DummyObj: Pointer;
begin
  result:=nil;
  exp :=nil;
  VarName := NextToken.Token;
  if Currenttoken.TokenType <> opIdentifier then
      error(Currenttoken,' invalid variable name '+Varname);
  vlen:=-1;
  vType :=StringToEpiType(Varname);
  if (vType = EpiTyUnknown) and not (LookAheadToken.TokenType in [opEq, opEndOfline, opEndOfFile]) then
    vType := ShortStringToEpiType(Varname);

  if (vType in [EpiTyString,EpiTyUppercase]) and (LookAheadToken.TokenType=opOpenParen) then
  begin
      NextToken;
      VarName :=NextToken.Token;
      vLen:=strtointdef(Varname,-1);
      if vlen=-1 then
         error(Currenttoken,'variable length is expected');
      if vlen > MaxStringSize then
         error(Currenttoken,'variable length is is more than '+inttostr(MaxStringSize));
      Accept([opCloseParen]);
  end;
  if vType<>EpiTyUnknown then
       VarName :=NextToken.Token;
  if Currenttoken.TokenType <> opIdentifier then
      error(Currenttoken,' invalid variable name '+Varname);
  if not IsValidEpiIdent(VarName) then
      error(Currenttoken,' invalid variable name '+Varname);
  if Tokenizer.Keywords.Find(AnsiUpperCase(VarName), DummyObj) then
    ErrorFmt(CurrentToken, 'Cannot use a reserved word as variable identifier: %s', [VarName]);
  vdec :=0;
  case vType of
     EpiTyBoolean:    vLen  :=1;
     EpiTyInteger:    vLen  :=9;    // changed from 4 todo check if integers should be larger JL Oct 2007
     EpiTyFloat:
     begin
       vLen  :=12;
       vDec  :=4;
     end;
     EpiTyDate:
     begin
       vLen  :=10;
       vFormat := '%DMY';
     end;
     EpiTyString,
     EpiTyUppercase:  if vlen=-1 then  vLen  :=12;
  end;
  if nextToken.TokenSubType = opEq then
  begin
      ParseExpression(exp);
      if exp=nil then
        error(Currenttoken,'Missing or invalid expression');
      ParseLateIfCommand(param)
  end
  else
    accept([opEndofline,opEndoffile]);
  Params :=TVarList.Create;
  varDesc := TAnaVariableDescriptor.create(Varname,Vtype,Vlen, vdec,vFormat);
  Params.AddVar(TVar.Create('VARDESC',integer(varDesc)));
  Params.AddVar(TVar.Create('EXP',exp));
  if assigned(param) then
    Params.AddVar(param);
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseDefineCommand(pCommandID: integer): TCommand;
var
 VarName,VarType, sv : string;
 Params :TVarList;
 Param : IValue;
 vType, vLen, Vdec, Vscope : integer;
 vFormat, def      :string;
 i,co         :integer;
 vardesc: TAnaVariableDescriptor;
 DummyObj: Pointer;
begin
  result:=nil;
  VarName :=NextToken.Token;
  if not IsValidEpiIdent(VarName) then
          error(Currenttoken,' invalid variable name '+Varname);
  if Tokenizer.Keywords.Find(AnsiUpperCase(VarName), DummyObj) then
    ErrorFmt(CurrentToken, 'Cannot use a reserved word as variable identifier: %s', [VarName]);
  VarType:= AnsiUppercase(Trim(ReadToChar));
  co:=length(vartype);
  if co=0 then
          error(Currenttoken,' Missing variable type');
  vdec :=0;
  case vartype[1] of
  '_':
  begin
    vType := EpiTyString;
    vLen  :=1;
    for i:= 2 to co do
      if vartype[i]='_' then inc(Vlen)
    else
      error(Currenttoken,' Invalid variable type');
    vFormat := '%s' + IntToStr(VLen);
  end;
  '#':
  begin
     vType := EpiTyInteger;
     vLen  :=1;
     for i:= 2 to co do
     begin
       inc(vLen);
       case vartype[i] of
       '#': if vType = EpiTyFloat then inc(vdec);
       '.':
       begin
         if vType = EpiTyInteger then
           vType := EpiTyFloat
         else
           error(Currenttoken,' Invalid variable type');
       end
       else
           error(Currenttoken,' Invalid variable type');
       end;//case
     end;//for
     case vType of
       EpiTyInteger : vFormat := '%d';
       EpiTyFloat   : vFormat := '%' + IntToStr(vLen - (vDec+1)) + '.' + IntToStr(vDec) + 'f';
     end;
  end;//begin #
 '<':
  begin
    if co<2 then error(Currenttoken,' Invalid variable type');
    case vartype[2] of
      'A':   // upper case
      begin
        if co > 82 then
          error(Currenttoken,' Max Length: 80');
        if pos('>',vartype) = 0 then
          error(Currenttoken,' Incorrect Define, e.g. &lt;AAAAAAAAA&gt;');
        vType := EpiTyUppercase;
        vLen  := co - 2;
        vFormat := '%s' + IntToStr(VLen);
      end;
    'Y': //Either a Boolean or a dateformat with YY(YY)-MM-DD:
        begin
          if vartype[3] = '>' then
          begin
            vType := EpiTyBoolean;
            vLen  :=1;
          end else begin
            // Not a bool, assumes dateformat is YY(YY)-MM-DD!
          vType := EpiTyDate;
          vlen := pos('>',vartype);
          if not((vlen = 10) or (vlen = 12)) then error(Currenttoken,' Invalid variable type');
          vFormat := '%YMD';
          end;
        end;
    'D','M':
        begin
          vType := EpiTyDate;
          vlen := pos('>',vartype);
          if vlen=12 then vlen:=10;  //MIB rettet 15feb05
          if not((vlen = 10) or (vlen = 12)) then error(Currenttoken,' Invalid variable type');
          vFormat := '%' + AnsiUppercase(copy(vartype,2,1)) + AnsiUppercase(copy(vartype,5,1)) + AnsiUppercase(copy(vartype,8,1));
    //      vlen:=length(vFormat);
        end;
    'E':
        error(currenttoken, 'Encrypted fields must be created before reading data into analysis')
    else
      error(Currenttoken,' Invalid variable type');
    end;//case
  end//begin <
  else
     error(Currenttoken,' Invalid variable type');
  end;//case
  varType:=AnsiUpperCase(nexttoken.token);
  def:='';
  Vscope:=EpiVarStandard;
  if varType<> '' then
  begin
     if copy(varType,1,4)='GLOB' then Vscope:=EpivarGlobal
     else if copy(varType,1,4)='CUMU' then Vscope:=EpivarCum
     else if copy(vartype,1,4)='SYST' then VScope:=EpiVarSystem
     else def:=varType;
  end;
  if def ='' then
     def:=nexttoken.token;
  Params :=TVarList.Create;
  varDesc := TAnaVariableDescriptor.create(Varname,Vtype,Vlen, vdec,vFormat);
  if def <> '' then
    VarDesc.DefaultValue :=def;
  vardesc.VarScope :=Vscope;
  Params.AddVar(TVar.Create('VARDESC',integer(varDesc)));
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseTypicalCommand(pCommandID: integer; pParseoptions: TEpiParseCmdOptions=[];
    const optSyntax: string = GlobaloptSyntax): TCommand;
var
 list : TStringList;
 options:TEpiParseVarOptions;
 Params :TVarList;
 Param : IValue;
 i : integer;
 Tok: TSMToken;

begin
 Param:=nil; Params:=nil;
 list :=TStringList.create;
 try
    Params :=TVarList.Create;
    if PCAllowVarlist in  pParseoptions then
    begin                                          
//      include(options,PVCheckValidName);
      if ParseVariableNames(list,options) then
      begin
          Params.AddVar(TVar.Create('VARLIST',integer(list)));
      end;
    end;
    NextToken;
    if Currenttoken.TokenType=opDivide then
    begin
        ParseOptions('',optSyntax,Params);
    end;

    if (Currenttoken.TokenSubType = opIf) and not (PCAllowIf in pParseoptions) then
    begin
      Tok := CurrentToken;
      ReadToEOL;
      error(Tok, 'If is not allowed with this command');
    end
    else
    If (PCAllowIf in pParseoptions) and ParseLateIfCommand(Param) then
    begin
      Params.AddVar(Param);
      NextToken;
    end;

    if (Currenttoken.TokenType = opDivide) then
    begin
      Tok := CurrentToken;
      ReadToEOL;
      Error(Tok, 'Options after IF not allowed.');
    end;

    // TODO: What if commandline at this point contains strange characters that
    // should NOT be allowed - such as !, ?, etc.???
    if not (CurrentToken.TokenType in [opSemicolon, opEndOfLine, opEndOfFile]) then
    begin
      Tok := CurrentToken;
      ReadToEOL;
      Error(Tok, 'Incorrect formatted line.');
    end;

    result:=TCommand.Create(pCommandID,Params);
 except
  list.free;
  Params:=nil;
  raise;
 end;
end;


function TAnaParser.ParseMeansCommand(pCommandID: integer): TCommand;
var
 list : TStringList;
 options:TEpiParseVarOptions;

 Params :TVarList;
 Param : IValue;
begin
 list :=TStringList.create;
 try
    include(options,PVCheckValidName);
    if parseVariableNames(list,options) then
    begin
        Params :=TVarList.Create;
        Params.AddVar(TVar.Create('VARLIST',integer(list)));
        result:=TCommand.Create(pCommandID,Params);
    end;
    If ParseLateIfCommand(Param) then
    begin
        if Params = nil then
           Params :=TVarList.Create;
        Params.AddVar(Param);
        if result= nil then
          result:=TCommand.Create(pCommandID,Params);
    end;
 except
  list.free;
 end;
end;


function TAnaParser.ParseRegressCommand(pCommandID: integer): TCommand;
var
 list : TStringList;
 options:TEpiParseVarOptions;
 Params :TVarList;
 Param : IValue;
begin
 list :=TStringList.create;
 try
    include(options,PVCheckValidName);
    if parseVariableNames(list,options) then
    begin
        Params :=TVarList.Create;
        Params.AddVar(TVar.Create('VARLIST',integer(list)));
        result:=TCommand.Create(pCommandID,Params);
    end;
 except
  list.free;
 end;
end;


function TAnaParser.IsPossibleLetCmd(const VarName:string): boolean;
var
 expStr : string;
begin
  result:=false;
  if not IsValidEpiIdent(VarName) then exit;
  if LookAheadToken.TokenSubType <> opEq then exit;
  result:=true;
end;




function TAnaParser.ParseHelpCommand(pCommandID: integer): TCommand;
var
 expStr : string;
 Params :TVarList;
begin
  result:=nil;
  expStr:=trim(ReadToEOL);
  Params :=TVarList.Create;
  Params.AddVar(TVar.Create('WORD',expStr));
  result:=TCommand.Create(pCommandID,Params);
end;

function TAnaParser.ParseloopCommand(pCommandID:integer):TCommand;
var
 CmdName,VarName : string;
 Params :TVarList;
 list : TStringList;
 looper : TLoopCommand;
begin
 result:=nil;
 VarName :=NextToken.Token;
 if not IsValidEpiIdent(VarName) then
          error(Currenttoken,' invalid loop variable name '+Varname);
 Params:=nil;list:=nil;
 accept([opEq],'= sign is expected');
 Params :=TVarList.Create;
 list :=TStringList.create;
try
 if not ParseVariableNames(list,[PVAllowQuotes]) then
    error(Currenttoken,'Item list is needed');
// Params.AddVar(TVar.Create('VARNAME',VarName));
 NextToken;
 if Currenttoken.TokenSubType <> opdo then
    error(Currenttoken,' DO is expected but '+VarName+' found');
 cmdname :=trim(ReadToChar(#13));
 if cmdname='' then
    error(Currenttoken,' DO should be followed by a command');

 looper := TLoopCommand.Create(varname,cmdname,list);
 Params.AddVar(TVar.Create('LOOPOBJ',looper));
 result:=TCommand.Create(pCommandID,Params);
finally
 list.free;
end;
end;


{ TAnaExecutor }

constructor TAnaExecutor.Create(pOwner: TObject);
begin
 inherited Create;
 fVariables:= TVarList.Create;
 fGlobalVariables:= TVarList.Create;
 fResultVariables:= TVarList.Create;
 fSystemVariables:= TVarList.Create;
 Globalcmdlist:=TCommandBlockList.Create;
 ExePointer:=-1;
// fForm:=powner;
end;

destructor TAnaExecutor.Destroy;
begin
  Variables.free;
  ResultVariables.free;
  GlobalVariables.free;
  SystemVariables.Free;
  Globalcmdlist.free;
  inherited;
end;

function WriteTextToFile(const filename,txt:string):boolean;
var
  stream : TFileStream;
begin
  stream := TFileStream.create(filename,fmCreate);
try
  stream.size:=0;
  stream.Write(txt[1],length(txt));
finally
  stream.free;
end;
end;



procedure TAnaExecutor.DoShowOutput(const output:string);
Var
  I,len  : integer;
  S: string;
Begin
 len :=Length(output);
 If len > 0 Then
    For I:=1 To len Do
       Case output[I] Of
          '&': S := S + '&amp;';
          '<': S := S + '&lt;';
          '>': S := S + '&gt;';
          #9,#32: S := S + '&nbsp;';
//          VK_SPACE
       Else
          S := S + output[I];
       End;{Case}
  if assigned(OnShowOutput) then
      OnShowOutput(S);
end;



function TAnaExecutor.ProcessCMDs(source:Tstrings;cmdlist:TCommandBlockList{;const pFileName:string}): boolean;
var
 cmd,s :string;
 i,co,LineNo : integer;
 inCmd :boolean;
 cmdblck:TCommandBlock;

begin
//  cmdlist.Clear;
  inCmd :=false;
  i:=-1;
  while i < source.count-1 do
  begin
     inc(i);
     LineNo:=i+1;
     s :=trim(source[i]);
     if s='' then continue;
     cmd:= AnsiUppercase(GetVarName(s,1));
     if cmd='END' then
     begin
        if not incmd then RunTimeError('End without command block');
        if cmdblck <> nil then
           cmdlist.add({pFileName+'.'+}cmdblck.commandID,cmdblck);
        incmd:=false;
        source.Delete(i);
        dec(i);
     end
     else if cmd= 'CMD' then
     begin
      if inCmd then RunTimeError('commands cannot be nested');
      incmd:=true;
      cmd:=trim(GetVarName(s,4));
      if cmd='' then RunTimeError('Missing/invalid CMD name');
      if not IsValidIdent(cmd) then
           RunTimeError(format('Invalid label name %s in line %d',[cmd,i]));
      cmdblck:=TCommandBlock.Create(AnsiUppercase(cmd),'','');
      source.Delete(i);
      dec(i);
     end
     else
     begin
      if not inCmd then continue;
      cmdblck.CommandLines.AddObject(s,Tobject(LineNo));
      source.Delete(i);
      dec(i);
     end;
  end;
  if inCmd then RunTimeError('Command without end');
end;


function TAnaExecutor.ProcessLabels(source:Tstrings;Labels:TStrings): boolean;
var
 labname,s :string;
 i,co : integer;
begin
  Labels.Clear;
  co :=source.count;
  for i :=0 to co-1 do
  begin
     s :=trim(source[i]);
     if s='' then continue;
     if s[1]=':' then
     begin
        labname:=GetVarName(s,2);
        if (labname='') or not IsValidIdent(labname) then
           RunTimeError(format('Invalid label name %s in line %d',[s,i]));
        Labels.AddObject(labname,TObject(i));
     end;
  end;
end;


function TAnaExecutor.AttemptLabelJump(const cmdline:string;Labels:TStrings): integer;
var
 cmd,s :string;
 i,co : integer;
begin
 Result:=0;
 cmd:= AnsiUppercase(GetVarName(cmdline,1));
 if cmd<>'GOTO' then exit;
 s:=trim(GetVarName(cmdline,5));
 if s='' then RunTimeError('Missing/invalid label name');
 result :=labels.IndexOf(s);
 if result=-1 then
     RunTimeError(format('label %s not found',[s]));
 result:=integer(labels.Objects[result]);
end;


function TAnaExecutor.ProcessQuery(const cmdline:String):string;
var
 i,j,co : integer;
 cmd,s,s1 :string;
begin
 result:=cmdline;
 s:=trim(cmdline);
 if length(s)=0 then exit;
 i :=pos('?',s);
 if i=0 then exit;
 s :=copy(s,i+1,maxInt);
 j :=pos('?',s);
 if j=0 then exit;
 s1:=trim(copy(s,1,j-1));
 if s1<>'' then
 begin
   if Ansiuppercase(s1)='<OPENDATAFILE>' then
   begin
    if not GetOpenFileName(cmd,EpiDataFilter) then cmd:='';
    if cmd='' then RunTimeError('Missing file name');
    result:= copy(cmdline,1,i-1) +' "' + cmd + '" '+ copy(s1,j+1,MaxInt);
    exit;
   end
   else
   if Ansiuppercase(s1)='<SAVEDATAFILE>' then
   begin
    if not GetSaveFileName(cmd,EpiDataFilter,'.REC') then cmd:='';
    if cmd='' then RunTimeError('Missing file name');
     result:= copy(cmdline,1,i-1)  +' "' + cmd + '" '+ copy(s1,j+1,MaxInt);
    exit;
   end
 end;
 if inputQuery('',s1,cmd) then
 begin
    result:= copy(cmdline,1,i-1) +' ' + cmd +
     ' '+ copy(s,j+1,MaxInt);
 end;
end;

Type
 TEpiRunStaus=(ElseFound, EndFound,EOFFound,ErrFound);

function TAnaExecutor.RunBlock(lines,labels: TStringList; cmdlist:TCommandBlockList):integer;
var
 lExePointer,j,co : integer;
 ifco,elseco,endifco : integer;
 cmdline,cmdVerb,s :string;
 cmdblk :TCommandBlock;
 stack,ifstack : TaaStack;
 GRS :TEpiRunStaus;

    function IntRunBolock(DoRun:boolean; var RS :TEpiRunStaus):integer;
    var
     cmd : TCommand;
     lcmds : TCommandList;
     cont, Ignorequery :boolean;
     btrue, IgnoreCmd :boolean;
     Param : IValue;
     opt: TEpiOption;


    begin
      cont :=false;
      Ignorequery:=false;
      IgnoreCmd:=false;
      while lExePointer < co-1 do
      begin
         if dm.CheckforBreak then
         begin
           Result:=2;
           break;
         end;
         inc(lExePointer);
         s :=trim(lines[lExePointer]);
         if lines.Objects[lExePointer]<> nil then
            CurrentExecLine:=integer(lines.Objects[lExePointer])
         else
            CurrentExecLine:=lExePointer+1;
         if s='' then continue;
         if (s[1]='*') or (copy(s,1,2)='!!') or (copy(s,1,2)='//') then
         begin
           if dm.GetOptionValue('HISTORY COMMENT', opt) and (opt.Value = 'ON') then
             dm.Info(s);
           result := 1;
           continue;
         end;
         if cont then
         begin
           delete(cmdline,length(cmdline),1);
           cmdline:=cmdline+' '+ s;
         end else
            cmdline:=s;
         cont:= s[length(s)]='\' ;
         if not cont then
         begin
           if cmdline[1]=':' then break;
           cmdVerb:= AnsiUppercase(GetVarName(cmdline,1));
    //all executable commands should go between these two lines
           if DoRun then
           begin
           j:=AttemptLabelJump(cmdline,labels);
           if j> 0 then
           begin
              stack.Push(pointer(lExePointer));
              lExePointer:=j;
              continue;
           end;
           if cmdVerb='SET' then
           begin
             cmdVerb:= AnsiUppercase(GetVarName(cmdline,4));
             if (cmdVerb='KEY') or (cmdVerb='BREAKCMD') then
                Ignorequery:=true;
           end else
           if cmdVerb='RETURN' then
           begin
             if stack.IsEmpty then
               RuntimeError('Return without prior goto');
             lExePointer:= integer(stack.pop)+1;
             continue;
           end
           end; //if doRun
           if cmdVerb='IMIF' then
           begin
             lcmds:=ParseCommandLine(cmdline);
             if (lcmds=nil) or (lcmds.count=0) then break;
             cmd:= lcmds[0];
             Param :=cmd.ParamByName['BOOLEXP'];
             btrue:=dm.EvalAsBoolean(Param.AsIValue);
             ifStack.Push(@btrue);
             Result:=IntRunBolock(bTrue and doRun,RS);
             case RS of
             ElseFound:
             begin
              Result:=IntRunBolock(Not bTrue and doRun,RS);
              case RS of
               ElseFound:RuntimeError('Else without prior imif');
               EOFFound : RuntimeError('if without prior endif');
               ErrFound : break;
               EndFound : continue;
             end;//case
             end;
             EOFFound : RuntimeError('if without prior endif');
             ErrFound : break;
             EndFound : continue;
             end;//case
           end else
           if cmdVerb='ELSE' then
           begin
               if Ifstack.IsEmpty then
                  RuntimeError('Else without prior imif');
               RS:= ElseFound;
               Result:=1;
               exit;
           end else
           if cmdVerb='ENDIF' then
           begin
               if Ifstack.IsEmpty then
                  RuntimeError('Endif without prior imif');
               RS:= EndFound;
               ifStack.Pop;
               Result:=1;
               exit;
           end;
           if DoRun then
           begin
             if dm.GetOptionValue('HISTORY COMMAND PGM', opt) and (AnsiUpperCase(opt.Value) = 'ON') then
              aMainForm.CmdEdit.History.Add(cmdline);
             if RunCommandline(cmdline, not Ignorequery) then result:=1 else result:=0;
             if result<>1 then break;
           end;
         end;
      end; //while
      if lExePointer >= co then
        RS:=EOFFound
      else
        RS :=ErrFound;
    end;

begin
  result:=0;
  co:= lines.count;
  if co=0 then RuntimeError('Nothing to execute');
  stack := TaaStack.create;
  ifstack := TaaStack.create;
try
  lExePointer:=-1;
  Result:=IntRunBolock(True,GRS);
  if not IfStack.ISempty then
    RuntimeError('Imif without endif');
finally
  stack.free;
  ifStack.free;
end;
end;


function TAnaExecutor.RunScript(const aFileName: string): integer;
var
 source,labels: TStringList;
 rc : integer;
begin
  result:=0;
  if not fileexists(afilename) then
     RunTimeError('File not found');
  source:=nil;labels:=nil;
  try
    try
      source:= TStringList.create;
      source.LoadFromFile(aFilename);
      if source.count=0 then  RunTimeError('Empty file');
      ProcessCMDs(source, Globalcmdlist{,FileName});
      if source.count=0 then RunTimeError('No executable commands');
      labels:= TStringList.create;
      ProcessLabels(source,labels);
      fFileName:= aFilename;
      Result:=runblock(source,labels,Globalcmdlist);
      if result=1 then
         fCurrentExecLine:=-1;
    except
      raise
    end;
  finally
    source.free;
    labels.free;
    ExePointer:=-1;
    fFileName:='';
  end;
end;


function TAnaExecutor.RunScriptAsString(const Script: string): integer;
var
 source,labels: TStringList;
 rc : integer;
begin
result:=0;
if script='' then exit;
source:=nil;labels:=nil;
try
try
  source:= TStringList.create;
  source.SetText(pchar(stringreplace(script,';',#10,[rfReplaceAll])));
  if source.count=0 then  RunTimeError('Nothing to execute');
  ProcessCMDs(source, Globalcmdlist{,FileName});
  if source.count=0 then RunTimeError('No executable commands');
  labels:= TStringList.create;
  ProcessLabels(source,labels);
  Result:=runblock(source,labels,Globalcmdlist);
  if result=1 then
     fCurrentExecLine:=-1;
except
  raise
end;
finally
  source.free;
  labels.free;
  ExePointer:=-1;
end;
end;


procedure TAnaExecutor.RunTimeError(const msg: string);
var
 handled: boolean;
 token : TSMTOken;
begin
 handled := false;
 if assigned(fOnError) then
     fOnError(Msg,Token,handled);
 if not handled then
   raise EAnaExecutor.Create(format('%s' + #13#10+ 'At line %d',[Msg,Token.line]));
end;

//TODO optimize this code freq run
function TAnaExecutor.ExpandMacros(const code: string): string;
var
 i, len,id : integer;
 idstr,macval     :string;
 gvar :IValue;
 moreparameters: boolean;
begin
  len := length(code);
  i:=1;
  while i <= len do
  begin
      if (code[i]='@') {and (i<len-1) and (upcase(code[i+1])='G')} then
      begin
      if (i<len-1) and (code[i+1]='@') then
       begin
         result:=result+'@';
         inc(i,2);
       end
       else
       begin
         idstr:='';
         inc(i);
         while (i<=len) and (code[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$' ])  do
         begin
           idstr := idstr + code[i];
           inc(i);
         end;
         macval:='';
         if (code[i]='(') then
         begin
           macval := macval+code[i];
           inc(i);
           while (i<=len) and (code[i] in [',', '0'..'9']) do
           begin
             macval := macval+code[i];
             inc(i);
             moreparameters := true;
           end;
         end;
         if moreparameters then
           if not (code[i] = ')') then
             RunTimeError('Incorrectly formed paramters!')
           else 
             inc(i);
         idstr:=trim(idstr);
         if idstr='' then
           runtimeerror('Invalid use of @, missing variable name');
         gvar := Findvar(idstr);
         if gvar <> nil then
           if moreparameters then
             result:=result + IntToStr(gvar.Value[strtoint(macval[2]), strtoint(macval[4])])
           else
             result:=result + gvar.AsString
         else
          runtimeerror(format('Variable %s not found',[idstr]))
        end;
      end
      else
      begin
         result:=result+code[i];
         inc(i);
      end;                          
  end;
end;

procedure TAnaExecutor.DoOpenFile(const filename: string;CreateNew,ReadOnly:boolean);
begin
  if assigned(OnOpenFile) then
     OnOpenFile(filename,CreateNew,ReadOnly);
end;



procedure TAnaExecutor.ParserErrHandler(const msg: string; Token: TSMToken;
  var Handled: boolean);
begin
 handled := false;
 if assigned(fOnError) then
     fOnError(Msg,Token,handled);
 if not handled then
   raise EAnaExecutor.Create(format('%s' + #13#10+ 'At line %d',[Msg,Token.line]));
 handled:=true;
end;


function TAnaExecutor.RunCommandLine(cmdline: string;DoQuery:boolean;DoRun:boolean): boolean;
var
 cmdlst :TCommandList;
 cmdblk : TCommandblock;
const
  procname = 'RunCommandLine';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  cmdlst :=nil;
  try
    if DoQuery then
      cmdline:=ExpandMAcros(ProcessQuery(cmdline));
    Dm.Sendoutput;
    DoShowOutput(cmdline);
    //this is CMD call
    if IsValidIdent(trim(cmdline)) then
    begin
      cmdblk:=Globalcmdlist.Find(trim(cmdline));
      if cmdblk <> nil then
      begin
        result:=RunBlock(cmdblk.CommandLines,nil,Globalcmdlist)=1;
        exit;
      end;
    end;
    if fAnaParser <> nil then
        fAnaParser.free;
    fAnaparser := TAnaParser.CreateString(self,cmdLine);
    fAnaParser.onError :=self.ParserErrHandler;
    cmdlst:=fAnaParser.ParseFile;
if cmdlst<> nil then
        result:=ExecuteCommandList(cmdLst);
  finally
    cmdlst.free;
    ODebug.DecIndent;
  end;
end;

function TAnaExecutor.ParseCommandLine(cmdline: string;DoQuery:boolean): TCommandList;
begin
  Result :=nil;
try
 if DoQuery then
    cmdline:=ExpandMAcros(ProcessQuery(cmdline));
  if fAnaParser <> nil then
      fAnaParser.free;
  fAnaparser := TAnaParser.CreateString(self,cmdLine);
  fAnaParser.onError :=self.ParserErrHandler;
  Result:=fAnaParser.ParseFile;
except
   Result.free;
   Result:=nil;
end;
end;

function TAnaExecutor.ExecuteImIF(Exp: IValue): boolean;
var
 t : DWord;
begin
   if ExePointer=-1 then
       RuntimeError('Immediate if is not allowed here');
   Result:=dm.EvalAsBoolean(Exp);
   while true do
   begin
//      runcommandline(
   end;
end;

function TAnaExecutor.ExecuteCommandList(cmdlst :TCommandList): boolean;
var
 i, co ,n: integer;
 cmd   : TCommand;
 Param   : IValue;
 fn,fn1,op      : string;
 VarDesc : TAnaVariableDescriptor;
 list    : TStringList;
 opt : TEpiOption;
 success: boolean;

  function GetFileName:string;
  begin
     Result:='';
     Param := cmd.ParamByName['FILENAME'];
     if not Assigned(param) then
       Param := cmd.ParamByName['FILE'];
     if param<> nil then
        Result :=StrUnquote(trim(Param.Value));
  end;

  function GetVarList:TStringList;
  begin
      Result:=nil;
      Param := cmd.ParamByName['VARLIST'];
      if Param <> nil then
        Result := TStringList(Param.AsInteger);
  end;

  function GetRestOfLine:string;
  begin
     Result:='';
     Param :=cmd.ParamByName['ROL'];
     if param<> nil then
        Result :=StrUnquote(trim(Param.Value));
  end;
  
const
  procname = 'ExecuteCommandList';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  VarDesc:=nil;
  list :=nil;
  result:=true;

  try
    try
      co :=cmdlst.count;
      for i:= 0 to co-1 do
      begin
        cmd := cmdlst[i];
    //change this if more than one command may be executed
        Dm.preCommand(cmd);
        case cmd.CommandID  of
         opRead:
          begin
            success := dm.OpenFile(GetFileName, Cmd);
            if (DM.GetOptionValue('DISPLAY VARIABLES', Opt) and (Opt.Value = 'ON')) then
              amainform.AcShowVariablesLeft(Self);
            if success then
              begin
                dm.Browse2(nil, cmd);
              end;
          end;
         opAppend,
         opMerge:
           begin
             dm.Merge(GetFileName, GetVarList, Cmd);
           end;
         opRelate:
           dm.error('RELATE replaced by MERGE command - see help file', [], 101003);
         opImIf:
           begin
              Param :=cmd.ParamByName['BOOLEXP'];
              ExecuteImif(Param.AsIValue);
           end;
         opif:
         begin
             Param :=cmd.ParamByName['TSTR'];
             fn:= Param.AsString;
             fn1 :='';
             Param :=cmd.ParamByName['FSTR'];
             if Param<> nil then
                fn1:= Param.AsString;
             Param :=cmd.ParamByName['BOOLEXP'];
             dm.Doif(Param.AsIValue,fn,fn1);
         end;
         opLet:
         begin
           Param :=cmd.ParamByName['VARNAME'];
           if Param <> Nil then
             begin
               fn    := Param.AsString;
               Param :=cmd.ParamByName['EXP'];
               if (dm.Let(fn, Param.AsIValue))and ((dm.GetOptionValue('DISPLAY DATABROWSER', opt)) and (opt.Value = 'ON')) then
               begin
                 dm.PostCommand(cmd);
                 dm.UpdateBrowse(opLet);
               end;
    //           dm.Let(fn, Param.AsIValue) ;
    //           dm.PostCommand(cmd);
    //           dm.UpdateBrowse(opLet);
           end;
         end;
         opGen:
           begin
             Param := cmd.ParamByName['VARDESC'];
             VarDesc := TAnaVariableDescriptor(Param.AsInteger);
             Param :=cmd.ParamByName['EXP'];
             if Param <> nil then
             begin
               if cmd.CommandID = opGen  then
                 dm.GenField(dm.dataframe, varDesc, Param.AsIValue)
               else
                 dm.GenVar(varDesc, Param.AsIValue);
             end;
             dm.UpdateBrowse(opGen);
           end;
         opGenerate:
         begin
             n:=0;
             Param :=cmd.ParamByName['EXP'];
             if (Param<> nil) and (Param.AsIValue<> nil) then
                n:=Param.AsIValue.AsInteger;
             if dm.generate(n) then
               dm.Browse2(nil, cmd);
         end;
         opEval, opQuery:
         begin
             Param :=cmd.ParamByName['EXP'];
             dm.Eval(Param.AsIValue);
         end;
         opAssert:
         begin
             Param :=cmd.ParamByName['EXP'];
             dm.Assert(Param.AsIValue);
         end;
         opSaveData:
         begin
            dm.DoSaveDataFile(GetFileName,GetVarList,Cmd) ;
         end;
         opDEFINE:
         begin
            Param := cmd.ParamByName['VARDESC'];
            if Param <> nil then
            begin
                VarDesc := TAnaVariableDescriptor(Param.AsInteger);
                dm.define(varDesc);
            end;
         end;
         opDescribe, opShortDescribe:
         begin
                dm.Describe(GetVarList, cmd);
         end;
         opPie:
         begin
                  dm.Pie(GetVarList,cmd);
         end;
         opHistogram, opbar,
         opShortHistogram:
         begin
           dm.Bar(GetVarList,cmd);
         end;
         opBox, opBoxplot:
         begin
                dm.BoxPlot(GetVarList,cmd);
         end;
         opLine, opScatter, opShortScatter:
         begin
                dm.Scatter(GetVarList, cmd);
         end;
         opXChart:
                dm.XChart(GetVarList, cmd);
         OpIChart, opRunChart:
         begin
                dm.IChart(GetVarList, cmd);
         end;
         OpPchart:
         begin
                dm.PChart(GetVarList, cmd);
         end;
         opEpiCurve:
         begin
                dm.EpiCurve(GetVarList, cmd);
         end;
         opCIPlot:
         begin
                dm.CIPlot(GetVarList, cmd);
         end;
         opCDFPlot:
         begin
                dm.CumDistPlot(GetVarList, cmd);
         end;
         opDotPlot:
         begin
                dm.DotPlot(GetVarList, cmd);
         end;
         opPareto:
         begin
                dm.ParetoPlot(GetVarList, cmd);
         end;
         opLifeTable:
         begin
                dm.LifeTable(GetVarList, cmd);
         end;
         opFreq, opTables, opShortTables:
         begin
                dm.PreTables(GetVarList, cmd);
         end;
         opLabelValue, opMissingValue:
         begin
                dm.DoLabelValue(GetVarList, cmd);
         end;
         opLabel:
         begin
                Param := cmd.ParamByName['VARNAME'];
                if Param <> nil then
                  fn := Param.AsString;
                Param := cmd.ParamByName['VARLABEL'];
                if Param <> nil then
                  fn1 := Param.AsString;
                dm.DoLabel(fn, fn1);
         end;
         opLabelData:
                dm.LabelData(GetRestOfLine);
         opSort:
         begin
                dm.Sort(GetVarList);
                dm.UpdateBrowse(opSort);
         end;
         opCount: dm.count(cmd);
         opSelect:
         begin
             Param :=cmd.ParamByName['BOOLEXP'];
             if Param <> Nil then
               begin
                 dm.select(Param.AsIValue);
                 dm.UpdateBrowse(opSelect);
               end;
         end;
         opBrowse: dm.Browse2(GetVarList,cmd);
         opUpdate: dm.UpdateData(GetVarList,cmd);
         opList :
         begin
                dm.list(GetVarList,Cmd);
                dm.Info('Note browse is faster than list', [], 201004);
         end;
         opMeans, opShortMeans:
         begin
                dm.Means(GetVarList, cmd);
         end;
         opRegress:
         begin
              dm.Regress(GetVarList);
         end;
         opCorrelate, opShortCorrelate:
         begin
              dm.Correlate(GetVarList);
         end;
         opKWallis:
         begin
              dm.KWallis(GetVarList, cmd);
         end;
         opTitle, opEcho, opType:
        begin
             fn:='';
             Param :=cmd.ParamByName['STR'];
             if param<> nil then
                fn :=trim(Param.Value);
             dm.EchoType(fn, cmd.CommandID, cmd);
        end;
        opEdit:
        begin
             dm.EditFile(GetFileName);
        end;
        opSavePgm,opRun:
        begin
             dm.doPGMFileAction(GetRestOfLine, cmd);
        end;
        opRunTest:
        begin
             dm.doPGMFileAction(GetFileName, cmd);
        end;
        opShow:
        begin
           fn:='';
           Param :=cmd.ParamByName['STR'];
           if param<> nil then
              fn :=trim(Param.Value);
           dm.doPGMFileAction(fn, cmd);
        end;
        opLOGClose:
        begin
          if cmd.CommandID in [opLOGClose] then
              dm.doLogFileAction('', cmd)
          else
              dm.doLogFileAction(GetRestOfLine, cmd)
        end;
        opLOGOPEN, opView, opHelpView: dm.doLogFileAction(GetFileName, cmd);
        opDIR,opERASE,opCD,opDOS, opDoDos, opMkDir:
        begin
             dm.doDOSFileAction(GetRestOfLine,cmd.CommandID);
        end;
        opCOPYFILE:
          begin
            fn:=Cmd.ParamByName['FROMFILENAME'].AsString;
            fn1:=Cmd.ParamByName['TOFILENAME'].AsString;
            dm.copyfile(fn,fn1, cmd) ;
          end;
        opCloseHelp: dm.CloseHelp;
        opPrintViewer: aMainForm.PrintViewer;
        opNewPage: dm.WriteDirect('<p style="page-break-after : always ; visibility : hidden ">&nbsp;</p>');
        opRecode :
        begin
           Param :=cmd.ParamByName['OPERATION'];
           op :=trim(Param.Value);
           Param :=cmd.ParamByName['SRCVAR'];
           fn :=trim(Param.Value);
           Param :=cmd.ParamByName['DSTVAR'];
           fn1 :=trim(Param.Value);
           dm.Recode(op,fn1,fn,GetVarList);
           dm.UpdateBrowse(opRecode);
        end;
        opErasePng:
        begin
          dm.ErasePng(cmd);
        end;
        opSet:
        begin
           dm.DoSet(GetVarList);
        end;
        opVariables:
        begin
            dm.variables(cmd);
        end;
        opMemory: dm.memory;
        opCls : dm.cls;
        opCLh : dm.clh;
        opVersion : dm.version;
        opSaveIniScreen : amainform.SaveWindowsPosition1Click(Self);
        opHelp:
        begin
          dm.help(GetRestOfLine);
        end;
        opLoop:
        begin
           Param :=cmd.ParamByName['LOOPOBJ'];
           dm.Doloop(param.AsObject);
        end;
        opAggregate, opShortAggregate
        ,opStables,opShortStables:
        begin
          dm.Aggregate(GetVarList, Cmd);
        end;
    {    opStables,opShortStables:
        begin
          dm.Stattables(GetVarList, Cmd);
        end;  }
        opTableDialog: AMainForm.AcRunTableExecute(Self);
        opClose: dm.closefile;
        opQuit, opExit: dm.quit(cmd.CommandID);
        end;//case
      end;//for
    except
      result:=false;
      raise;
    end;
  finally
    Dm.postCommand(cmd);
    VarDesc.free;
    list.free;
    ODebug.DecIndent;
  end;
end;


function TAnaExecutor.VarIDF(const Identifier: String;
                              ParameterList: TParameterList): IValue;
var
  v : TEpiVector;
  avar,Param :IValue;
  varname : string;
begin
  Result:= nil;
//handle result function
  if Identifier[1]='$' then
  begin
    Result := ResultVariables.CloneByName[Identifier];
    if Result = nil then
      raise EExpression.CreateFmt('Result %s is not found', [Identifier]);
    if (not TVar(Result.GetObject).AcceptParameters) and Assigned(ParameterList) then
      raise EExpression.CreateFmt('Identifier %s does not accept parameters', [Identifier]);
    TVar(Result.GetObject).SetParameterList(ParameterList);
    exit;
  end;
  v := FindVector(Identifier);
  if v <> nil then
  begin
{   if Assigned(ParameterList) then
      raise EExpression.CreateFmt('Identifier %s does not require parameters', [Identifier]);}
    Result:= TVectorVar.Create(v,ParameterList);
  end
  else begin
    varname := Identifier;
    if AnsiUppercase(varname) = '_N' then varname := 'RECNUMBER';
    avar:=FindVar(varname);
  end;
  if avar <> nil then
  begin
   case aVar.Tag of
   EpiVarLocal:
   begin
   if Assigned(ParameterList) then
      raise EExpression.CreateFmt('Identifier %s does not require parameters', [Identifier]);
//add checkdataopen(attmept:boolean)
    if (dm=nil) then
      dm.error('Missing file name', [] , 101004, 0);
      if not dm.CheckDataOpen(False) then exit;  // dm.CheckDataOpen(false);
    Result:= TFrameVar.Create(dm.dataframe,varname);
   end;
   EpiVarGlobal:
   begin
   if Assigned(ParameterList) then
      raise EExpression.CreateFmt('Identifier %s does not require parameters', [Identifier]);
      Result:= aVar;
   end;
  end;//case
 end;
end;

function TAnaExecutor.FindVector(const vname: string): TEpiVector;
begin
  result:=nil;
  if (dm=nil) or (dm.dataframe=nil) then exit;
  result:=dm.dataframe.FindVector(vname);
end;

function TAnaExecutor.FindVar(const vname: string): IValue;
begin
  result:=nil;
  result:=Variables.VarbyName[vname];
  if result=nil then
    result:=GlobalVariables.VarbyName[vname];
  if result=nil then
    result:=ResultVariables.VarbyName[vname];
  if result=nil then
    result:=SystemVariables.VarbyName[vname];
end;


function TAnaExecutor.NewVar(pVarDesc: TAnaVariableDescriptor): IValue;
var
  i          : integer;
  v        :IValue;
  FExprType : TExprType;
begin
    if findVector(pVarDesc.Name) <> nil then
        runtimeerror('Variable exists : '+ pVarDesc.Name);
//initialize var to missing if no default value
    FExprType:= EpiFieldTypeToParserDataType(pVarDesc.DataType);
    if VarIsEmpty(pVarDesc.DefaultValue) then
    begin
    case FExprType of
      ttFloat          : pVarDesc.DefaultValue := NA_FLOAT;
      ttInteger,ttDate : pVarDesc.DefaultValue := NA_INT;
      ttstring         : pVarDesc.DefaultValue := NA_STR;
      ttBoolean        : pVarDesc.DefaultValue := NA_BYTE;
    end;
    end;
   v := TVar.Create(pVarDesc.Name, pVarDesc.DefaultValue, FExprType);
   Result := AddVar(v,pVarDesc.VarScope);
end;


function TAnaExecutor.AddVar(aVar :IValue; VarScope:word): IValue;
begin
  case  VarScope of
  EpiVarGlobal:
  begin
    if findVector(aVar.VarName) <> nil then
        runtimeerror('Variable exists : '+ aVar.VarName);
    aVar.Tag:=VarScope;
    GlobalVariables.AddVar(aVar);
  end;
  EpiVarlocal:
  begin
    aVar.Tag:=VarScope;
    Variables.AddVar(aVar);
  end;
  EpiVarResult:
  begin
    aVar.Tag:=VarScope;
    ResultVariables.AddVar(aVar);
  end;
  EpiVarSystem:
  begin
    aVar.Tag:=VarScope;
    SystemVariables.AddVar(aVar);
  end;
  end;//case
  Result:=aVar;
end;

function TAnaExecutor.DropVars(varnames: Tstrings; VarScope:word): boolean;
var
 i, co : integer;
 v     : Ivalue;
begin
  Result:=False;
  co :=varnames.count;
  for i:= 0 to co-1 do
  begin
     v :=FindVar(varnames[i]);
     if v<> nil then
     begin
         if v.Tag= VarScope then
            RemoveVar(v,v.tag);
     end;
  end;
  Result:=true;
end;


function TAnaExecutor.RemoveVar(aVar :IValue; VarScope:word): IValue;
begin
{  if findVector(aVar.VarName) = nil then
        runtimeerror('Variable not found '+ aVar.VarName); } //Removed by TC
  case  VarScope of
  EpiVarGlobal:
  begin
    GlobalVariables.RemoveVar(aVar);
  end;
  EpiVarlocal:
  begin
    Variables.RemoveVar(aVar);
  end;
  EpiVarResult:
  begin
    aVar.Tag:=VarScope;
    ResultVariables.RemoveVar(aVar);
  end;
  EpiVarSystem: RunTimeError('Removing Systemvariables not allowed');
  end;//case
end;


function TAnaExecutor.ClearVars(VarScope:word): IValue;
var
  assert, assert_error,
  dp, dpdif: iValue;
  v: TVar;
  s: string;
begin
  case VarScope of
  EpiVarGlobal:
  begin
    GlobalVariables.clear;
  end;
  EpiVarlocal:
  begin
    Variables.clear;
  end;
  EpiVarResult:
  begin
    assert := ResultVariables.VarbyName['$assert'];
    assert_error := ResultVariables.VarbyName['$assert_error'];
    dp := ResultVariables.VarbyName['$dp'];
    dpdif := ResultVariables.VarbyName['$dpdif'];
    ResultVariables.clear;
    if Assigned(assert) then ResultVariables.AddVar(assert);
    if Assigned(assert_error) then ResultVariables.AddVar(assert_error);
    if Assigned(dp) then ResultVariables.AddVar(dp);
    if Assigned(dpdif) then ResultVariables.AddVar(dpdif);
  end;
  EpiVarSystem:
  begin
    TVar(SystemVariables.VarbyName['SYSTEMDATE'].GetObject).SetValue(EpiDateToStr(EpiToday,dfDMY,10));
    TVar(SystemVariables.VarbyName['SYSTEMTIME'].GetObject).SetValue(timetostr(Now));
  end;
  // RunTimeError('Clearing Systemvariables not allowed');
  end;//case
end;


function TAnaExecutor.GetVariableListByName(varnames: Tstrings;VarScope:integer=-1): TVarList;
var
 i, co, vbidx, veidx : integer;
 varname, vb,ve :string;
 Ivar :IValue;
procedure GetAllVariables(VarList:TVarList;VarScope:integer);
var
 j : integer;
begin
   if (varscope=-1) or (varscope=EpiVarGlobal) then
     for j:=0 to GlobalVariables.Count-1 do
     begin
        Ivar :=GlobalVariables[j];
        Tvar(Ivar).clone;
        VarList.AddVar(Ivar);
     end;
   if (varscope=-1) or (varscope=EpiVarlocal) then
     for j:=0 to Variables.Count-1 do
     begin
        Ivar :=Variables[j];
        Tvar(Ivar).clone;
        VarList.AddVar(Ivar);
     end;
   if (varscope=-1) or (varscope=EpiVarResult) then
     for j:=0 to ResultVariables.Count-1 do
     begin
        Ivar :=ResultVariables[j];
        Tvar(Ivar).clone;
        VarList.AddVar(Ivar);
     end;
   if (varscope=-1) or (varscope=EpiVarSystem) then
     for j:=0 to SystemVariables.Count-1 do
     begin
        Ivar :=SystemVariables[j];
        Tvar(Ivar).clone;
        VarList.AddVar(Ivar);
     end;
end;

procedure GetMatchingVariables(VarList:TVarList;const mask:string;VarScope:integer);
var
 j : integer;
begin
   if (varscope=-1) or (varscope=EpiVarGlobal) then
     for j:=0 to GlobalVariables.Count-1 do
     if MatchString(mask,GlobalVariables[j].Varname,0).BPos > -1 then
     begin
        Ivar :=GlobalVariables[j];
        Tvar(Ivar).clone;
        VarList.AddVar(Ivar);
     end;
   if (varscope=-1) or (varscope=EpiVarlocal) then
     for j:=0 to Variables.Count-1 do
     if MatchString(mask,Variables[j].Varname,0).BPos > -1 then
     begin
        Ivar :=Variables[j];
        Tvar(Ivar).clone;
        VarList.AddVar(Ivar);
     end;
   if (varscope=-1) or (varscope=EpiVarResult) then
     for j:=0 to ResultVariables.Count-1 do
     if MatchString(mask,ResultVariables[j].Varname,0).BPos > -1 then
     begin
        Ivar :=ResultVariables[j];
        VarList.AddVar(Ivar.Varname, Ivar.value);
     end;
   if (varscope=-1) or (varscope=EpiVarSystem) then
     for j:=0 to SystemVariables.Count-1 do
     if MatchString(mask,SystemVariables[j].Varname,0).BPos > -1 then
     begin
        Ivar :=SystemVariables[j];
        VarList.AddVar(Ivar.Varname, Ivar.value);
     end;
end;

begin
 result:= TVarList.Create;;
 if VarNames =nil then
     co := 0
 else
    co := VarNames.count;
 if co=0 then
 begin
    GetAllVariables(result,VarScope);
    exit;
 end;
 if co=1 then
 begin
    varname:=trim(Varnames[0]);
    if varname='*' then
    begin
        GetAllVariables(result,VarScope);
        exit;
    end;
    if (pos('*',varname) > 0) or (pos('?',varname) > 0) or (pos('[',varname) > 0)then
    begin
       GetMatchingVariables(result,varname,VarScope);
       exit;
    end;
 end;
//add support to varscope
 for i:= 0 to co-1 do
 begin
      Ivar:= findVar(varNames[i]);
      if Ivar= nil then
         RunTimeError('Variable not found '+varNames[i]);
            Result.AddVar(Ivar);
 end;
end;


function TAnaExecutor.Expandlist(items,expandeditems: TStringlist):boolean;
var
 Vectorlist :TEpiVectors;
 VarList    : TVarList;
 s : string;
 i, co : integer;
begin
 Result:=false;
 if items.count<1 then exit;
 s:=trim(items[0]);
 if s='' then exit;
 if s[1]='$'then //Result variable
 begin
   VarList:=GetVariableListByName(items,EpiVarResult );
   co :=Varlist.count;
   expandeditems.clear;
   for i:= 0 to co-1 do
      expandeditems.Add(Varlist[i].Varname);
   Result:=True;
   exit;
 end;
{ Vectorlist:=nil;
 CheckDataOpen();
try
  Vectorlist:= Dataframe.GetVectorListByName(varnames);
finally
  Vectorlist.free;
end;}
end;


function TAnaExecutor.EvalExpression(const Expression:String):IValue;
begin
  Result := Anaparser.GenExpression(Expression,VarIDF);
end;


function TAnaExecutor.GetCurrentExecLine: integer;
begin
  Result := fCurrentExecLine;
end;


{ TLoopCommand }

constructor TLoopCommand.Create(const ploopvarname, pCommandLines: string;pItems :TStringList);
var
  s1,s2 : string;
begin
  inherited create('LOOP','','');
  loopvarname := ploopvarname;
  SetCmdSeparated(pCommandLines,';');
  fItems :=TStringList.create;
  if pItems <> nil then
     Items.assign(pitems);
end;

procedure TLoopCommand.SetCmdSeparated(const Value: string; Sep:char);
var
  P, P1: PChar;
  S: string;
begin
  CommandLines.BeginUpdate;
  try
    CommandLines.Clear;
    P := PChar(Value);
    while P^ in [#1..#31] do P := CharNext(P);
    while P^ <> #0 do
    begin
      if P^ = '"' then
        S := AnsiExtractQuotedStr(P, '"')
      else
      begin
        P1 := P;
        while (P^ > #31) and (P^ <> Sep) do P := CharNext(P);
        SetString(S, P1, P - P1);
      end;
      CommandLines.Add(S);
      while P^ in [#1..#31] do P := CharNext(P);
      if P^ = Sep then
        repeat
          P := CharNext(P);
        until not (P^ in [#1..#31]);
    end;
  finally
    CommandLines.EndUpdate;
  end;
end;



function TLoopCommand.Execute: boolean;
begin

end;

procedure TLoopCommand.SetItems(const Value: TStringList);
begin
  FItems := Value;
end;

procedure TLoopCommand.Setlevel(const Value: integer);
begin
  Flevel := Value;
end;

procedure TLoopCommand.Setloopvar(const Value: TVar);
begin
  Floopvar := Value;
end;

procedure TLoopCommand.Setloopvarname(const Value: string);
begin
  Floopvarname := Value;
end;


end.
