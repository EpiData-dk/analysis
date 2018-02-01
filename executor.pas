unit executor;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ast, ast_types, outputcreator, epidatafiles,
  epidatafilestypes, epidatafilerelations, epivaluelabels, options_hashmap,
  contnrs, select_stack, epiopenfile, epimiscutils, result_variables,
  epidocument, LazMethodList, parser_types, epicustombase, epiranges;

type
  TExecutor = class;

  TExecutorStatementEvent = procedure ( Statement: TCustomStatement ) of object;

  TGUIAction = (
    gaClearScreen,   // Clear output window
    gaClearHistory,  // Delete all of history
    gaProjectTree,   // Reload project tree
    gaVariableList,  // Reload variable list
    gaTitle          // Reload title
  );
  TGUIInteraction = procedure( Sender: TExecutor; GUIAction: TGUIAction) of object;


  TPrepareDatasetOption = (
    // Ignores the INCLUDE DELETED set opiont
    pdoIgnoreDeleteSetOption,

    // Invert DELETED selection (this also includes already "marked for deletetion")
    pdoInvertDelete,

    // Adds an interger field '$obsno' that contains the original observation numbers
    pdoAddOrgObsNo
  );

  TPrepareDatasetOptions = set of TPrepareDatasetOption;

  { TExecutor }

  TExecutor = class({IEpiTypeChecker, }IEpiScriptExecutor)
  private
    FOutputCreator: TOutputCreator;
    FDocFile: TEpiDocumentFile;
    FDataFile: TEpiDataFile;
    FSelectStack: TSelectStack;
    procedure RebuildSelectStack;
    procedure ClearSelectStack;
    procedure ClearFieldVars;
    procedure ClearVLSetVars;
    procedure ClearDatasetVars;
    procedure ClearGlobals;
    procedure CreateDatasetVars(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    procedure DoUseDatafile(ADataFile: TEpiDataFile);
    procedure DoUpdateFieldResultVar;
    procedure DoUpdateDatasetResultVar;
    procedure DoUpdateValuelabelsResultVar;
    procedure DoUpdateProjectresultVar;
    function  GetDocument: TEpiDocument;
    procedure OpenFileError(const Msg: string);
  private
    // Save handling - for handling .lock files diffently than in Manager/EntryClient
    FOldWarning: TOpenEpiWarningEvent;
    FSaveOptions: TOptionList;
    procedure DMDocFileCreated(Sender: TObject; DocFile: TEpiDocumentFile);
    function SaveWarning(WarningType: TOpenEpiWarningType; const Msg: string
      ): TOpenEpiWarningResult;

  // PrepareDataFile
  private
    FInverseSelectVector: TEpiField;
    FMissingFields: TStrings;
    FDropDeleted: Boolean;
    FPrepareDFOptions: TPrepareDatasetOptions;
    function GetDataFile: TEpiDataFile;
    function PrepareDatafilePack(Sender: TEpiDataFile; Index: Integer): boolean;
  protected
    function DoPrepareDatafile(SelectField: TStrings; MissingFields: TStrings; Options: TPrepareDatasetOptions = []): TEpiDataFile; virtual;

  protected
    procedure DoError(Const Msg: UTF8String); virtual;
    // Lvl:  0 = short, 1 = All
    procedure DoInfo(Const Msg: UTF8String; Lvl: Word = 1); virtual;
    procedure DoWarning(Const Msg: UTF8String); virtual;
  public
    procedure Error(Const Msg: UTF8String);
    function  EpiVarsFromVarlist(Const VariableList: TVariableList): TEpiFields;
    function  VariableNamesFromPrefix(const Prefix: UTF8String): TStrings;
    procedure UpdateFieldResultVar;
    procedure UpdateDatasetResultVar;
    procedure UpdateValuelabelsResultVar;
    function  PrepareDatafile(SelectField: TStrings; MissingFields: TStrings; Options: TPrepareDatasetOptions = []): TEpiDataFile;
    property  DataFile: TEpiDataFile read GetDataFile;
    property  Document: TEpiDocument read GetDocument;
    property  DocFile: TEpiDocumentFile read FDocFile;

  // Internal Vars
  private
//    FSortedFields: TEpiFields;
    FOptions: TSetOptionsMap;
    FDataSets: TExecutorDatasetVariables;
    FVLSets:  TExecutorValuelabelsets;
    FFields:  TExecutorDataVariables;
    FConsts:  TExecutorDataVariables;
    FResults: TExecutorDataVariables;
    procedure DeleteExecVar(Const Ident: UTF8String);
    function  GetModified: boolean;
    procedure InitSetOptions;
    function  GetSetOptionValue(const Key: UTF8String): UTF8String;
    procedure SetSetOptionValue(const Key: UTF8String; AValue: UTF8String);
    function  GetSortedFields: TEpiFields;
  public
    function  AddResultConst(Const Ident: UTF8String; DataType: TEpiFieldType): TExecVarGlobal; virtual;
    function  AddResultVector(Const Ident: UTF8String; DataType: TEpiFieldType;
      Length: Integer): TExecVarVector; virtual;
    function  AddResultMatrix(Const Ident: UTF8String; DataType: TEpiFieldType;
      Cols, Rows: Integer): TExecVarMatrix; virtual;
    function  GetExecVariable(Const Ident: UTF8String): TCustomExecutorVariable;
    function  GetExecDataVariable(Const Ident: UTF8String): TCustomExecutorDataVariable;
    procedure ClearResults;
    property  Valuelabels: TExecutorValuelabelsets read FVLSets;
    property  Datasets: TExecutorDatasetVariables read FDataSets;
    property  SortedFields: TEpiFields read GetSortedFields;
    property  Fields: TExecutorDataVariables read FFields;
    property  Consts: TExecutorDataVariables read FConsts;
    property  Results: TExecutorDataVariables read FResults;
    property  SetOptions: TSetOptionsMap read FOptions;
    property  SetOptionValue[Const Key: UTF8String]: UTF8String read GetSetOptionValue write SetSetOptionValue;
    property  Modified: boolean read GetModified;

  private
    FAssignmentChanges: Integer;
    procedure SetCurrentRecNo(AValue: integer);
    procedure AssignmentDataChangeHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);

  // Various outputs
  private
    procedure OutputFileInformation(Doc: TEpiDocument);

  // New subcommands
  private
    // Sanity Checks (for new and edit)
    function DoCrudCommonSanityCheck(ST: TCustomCrudCommand; Item: TEpiCustomItem): boolean;
    function DoCrudDataformSanityCheck(ST: TCustomCrudCommand; DF: TEpiDataFile): boolean;
    function DoCrudVariableSanityCheck(ST: TCustomCrudCommand; F: TEpiField): boolean;
    function DoCrudValuelabelSanityCheck(ST: TCustomCrudCommand; VLs: TEpiValueLabelSet): boolean;

    // Subcommand execution
    procedure DoNewProject(St: TNewProject);
    procedure DoNewDataform(St: TNewDataset);
    procedure DoNewValueLabel(St: TNewValuelabel);
    procedure DoCrudVariableOptions(St: TCustomCrudCommand; F: TEpiField);
    procedure DoNewVariable(St: TNewVariable);
    procedure DoNewGlobal(St: TCustomNewValued);

  { Loop Accounting }
  private
    FPostLoopNotification: TMethodList;
    FNestedLoopCounter: Integer;


  // EXEC's
  protected
    // FCurrentRecNo is NO offset into current SelectVector. However it will never exceed
    // the boundries of SelectVector... ie.  FCurrentRecNo = [0 .. SelectVector.Size - 1]
    FCurrentRecNo: Integer;

    // Atypical
    procedure ExecAssignment(ST: TAssignment); virtual;
    procedure ExecIfThen(ST: TIfThen); virtual;
    procedure ExecFor(ST: TFor); virtual;
    procedure ExecSelect(ST: TSelect); virtual;
    procedure ExecEval(ST: TEvalExpression); virtual;
    procedure ExecAssert(ST: TAssertCommand); virtual;
    procedure ExecSet(ST: TSetCommand); virtual;
    procedure ExecUse(ST: TUse); virtual;

    // Crud Commands
    procedure ExecNew(ST: TCustomNew); virtual;
    procedure ExecEdit(ST: TCustomCrudCommand); virtual;
    procedure ExecDrop(ST: TDropCommand); virtual;
    procedure ExecList(ST: TCustomCrudCommand); virtual;

    // Variable Commands
    procedure ExecBrowse(ST: TCustomVariableCommand); virtual;
    procedure ExecMean(ST: TCustomVariableCommand); virtual;
    procedure ExecFreq(ST: TCustomVariableCommand); virtual;
    procedure ExecSort(ST: TCustomVariableCommand); virtual;
    procedure ExecAppend(ST: TAppendCommand); virtual;
    procedure ExecMerge(ST: TMergeCommand); virtual;
    procedure ExecCheck(ST: TCustomCheckCommand); virtual;
    procedure ExecReport(ST: TCustomReportCommand); virtual;
    procedure ExecReorder(ST: TReorderCommand); virtual;

    // String commands
    procedure ExecRead(ST: TCustomStringCommand); virtual;
    procedure ExecSave(ST: TCustomStringCommand); virtual;
    procedure ExecRun(ST: TCustomStringCommand); virtual;
    procedure ExecRuntest(ST: TCustomStringCommand); virtual;
    procedure ExecSystemCmd(ST: TCustomStringCommand); virtual;

    // Empty commands
    procedure ExecReset(ST: TCustomEmptyCommand); virtual;
    procedure ExecClose(ST: TCustomEmptyCommand); virtual;
    procedure ExecCls(ST: TCustomEmptyCommand); virtual;
    procedure ExecClh(ST: TCustomEmptyCommand); virtual;
    procedure ExecCount(ST: TCustomEmptyCommand); virtual;
    procedure ExecQuit(ST: TCustomEmptyCommand); virtual;
  protected
    procedure DoStatement(St: TCustomStatement); virtual;
    procedure DoStatementList(L: TStatementList); virtual;
  public
    constructor Create(OutputCreator: TOutputCreator); virtual;
    destructor Destroy; override;
    procedure Execute(TheProgram: TStatementList);  virtual;
    procedure ExecStatement(St: TCustomStatement);

  { IEpiTypeChecker }
  public
    procedure TypeCheckError(Const Msg: string; Const LineNo, ColNo, BytePos: integer);  virtual;
    property CurrentRecNo: integer read FCurrentRecNo write SetCurrentRecNo;

  { IEpiScriptExecutor }
  private
    FTypeCheckErrorOutput: boolean;
    function CheckVariableIndex(EV: TCustomExecutorVariable; CV: TCustomVariable; OutputError: boolean = true): boolean;
  public
    function GetVariableExecType(const Ident: UTF8String): TExecutorVariableType;
    function GetVariableValueBool(Const Sender: TCustomVariable): Boolean; virtual;
    function GetVariableValueInt(Const Sender: TCustomVariable): EpiInteger; virtual;
    function GetVariableValueFloat(Const Sender: TCustomVariable): EpiFloat; virtual;
    function GetVariableValueDate(const Sender: TCustomVariable): EpiDate; virtual;
    function GetVariableValueTime(const Sender: TCustomVariable): EpiDateTime; virtual;
    function GetVariableValueString(Const Sender: TCustomVariable): EpiString; virtual;
    function GetVariableValueMissing(const Sender: TCustomVariable): boolean; virtual;
    function GetVariableValueUserMissing(Const Sender: TCustomVariable): boolean; virtual;
    function GetVariableType(const Sender: TCustomVariable): TEpiFieldType; virtual;
    function GetCurrentRecordNo: Integer; virtual;
    function CreateFunction(const FunctionName: string;
      const ParamList: TParamList): TFunctionCall; virtual;
    function TypeCheckVariable(const Sender: TCustomVariable; TypesAndFlags: TTypesAndFlagsRec): boolean;
    function ExpandVariableList(Const Sender: TCustomVariable; VariableChecker: IVariableCheck;
      Index: Integer; out AVariableList: TVariableList): boolean;
    procedure SetTypeCheckErrorOutput(Active: boolean);

  protected
    FCancelled: Boolean;
    FExecuting: boolean;
    function  GetCancelled: Boolean; virtual;
    function  GetSelectVector: TEpiIntField; virtual;
  public
    property Executing: Boolean read FExecuting;
    property Cancelled: Boolean read GetCancelled write FCancelled;
    property SelectVector: TEpiIntField read GetSelectVector;

  { Events }
  private
    FOnAfterStatements: TMethodList;
    FOnBeforeStatements: TMethodList;
    FOnStartExecuting: TMethodList;
    FOnEndExecuting: TMethodList;
    FIgnoreRedrawRequest: Boolean;
    FOldRedrawRequest: TNotifyEvent;
    FOnRunPgmLexError: TLexError;
    FOnRunPgmSyntaxError: TSyntaxError;
    FOnRunPgmCommentError: TCommentError;
    FOnGUIInteraction: TGUIInteraction;
    procedure InternalRedrawRequest(Sender: TObject);
  protected
    procedure DoBeforeStatement(Statement: TCustomStatement);
    procedure DoAfterStatement(Statement: TCustomStatement);
    procedure DoStartExecuting;
    procedure DoEndExecuting;
    procedure DoGUIInteraction(GUIAction: TGUIAction);
  public
    procedure AddOnAfterStatementHandler(Event: TExecutorStatementEvent);
    procedure AddOnBeforeStatementHandler(Event: TExecutorStatementEvent);
    procedure AddOnStartExecutingHandler(Event: TNotifyEvent);
    procedure AddOnEndExecutingHandler(Event: TNotifyEvent);
    procedure RemoveOnAfterStatementHandler(Event: TExecutorStatementEvent);
    procedure RemoveOnBeforeStatementHandler(Event: TExecutorStatementEvent);
    procedure RemoveOnStartExecutingHandler(Event: TNotifyEvent);
    procedure RemoveOnEndExecutingHandler(Event: TNotifyEvent);
    property  OnRunPgmLexError: TLexError read FOnRunPgmLexError write FOnRunPgmLexError;
    property  OnRunPgmSyntaxError: TSyntaxError read FOnRunPgmSyntaxError write FOnRunPgmSyntaxError;
    property  OnRunPgmCommentError: TCommentError read FOnRunPgmCommentError write FOnRunPgmCommentError;
    property  OnGUIInteraction: TGUIInteraction read FOnGUIInteraction write FOnGUIInteraction;
  end;

implementation

uses
  LazUTF8, LazFileUtils, datamodule, epidatafilerelations_helper,
  Controls, runtest, Forms, parser, LazUTF8Classes, math,
  epiexport, epiexportsettings, epieximtypes, episervice_asynchandler,
  token, ana_procs, epitools_statusbarparser, epifields_helper, typinfo,
  RegExpr, ana_globals, browse4, strutils, options_fontoptions,
  ana_documentfile,

  // STATEMENTS
  list, edit, drop, systemcmd, merge, integrity_tests, report, save_output,

  // STATISTICS
  means, freq;

type
  EExecutorException = class(Exception);
  EExecutorIndexException = class(EExecutorException);
  EExecutorZeroDataException = class(EExecutorException);

{ TExecutor }

procedure TExecutor.RebuildSelectStack;
var
  TmpSelectStack: TSelectStack;
  F, TmpSelectVector: TEpiIntField;
  Runner, i: Integer;
  Expr: TExpr;
begin
  // only a single selectvector => no datafile has been loaded yet
  if FSelectStack.Count <= 1 then Exit;

  // Reverse the stack to redo the calculations from the bottom up
  TmpSelectStack := TSelectStack.Create;
  while FSelectStack.Count > 1 do
    TmpSelectStack.Push(FSelectStack.Pop);


  // First the selectvector that covers the entire datafile, free the previous one
  TmpSelectStack.Pop.Free;
  F := TEpiIntField(TEpiField.CreateField(nil, ftInteger));
  F.Size := FDataFile.Size;
  for i := 0 to F.Size - 1 do
    F.AsInteger[i] := i;

  FSelectStack.Push(F);

  TmpSelectVector := TmpSelectStack.Pop;
  while Assigned(TmpSelectVector) do
    begin
      F := TEpiIntField(TEpiField.CreateField(nil, ftInteger));
      F.Size := SelectVector.Size;

      Expr := TExpr(TmpSelectVector.RemoveCustomData(SELECTION_EXPR));

      Runner := 0;
      for i := 0 to SelectVector.Size - 1 do
        begin
          FCurrentRecNo := i;
          if Expr.AsBoolean then
            begin
              F.AsInteger[Runner] := SelectVector.AsInteger[FCurrentRecNo];
              Inc(Runner);
            end;
        end;
      FCurrentRecNo := 0;
      F.Size := Runner;

      FSelectStack.Push(F);
      TmpSelectVector.Free;

      TmpSelectVector := TmpSelectStack.Pop;
    end;
end;
procedure TExecutor.ClearSelectStack;
var
  F: TEpiField;
begin
  F := FSelectStack.Pop;
  while Assigned(F) do
    begin
      F.Free;
      F := FSelectStack.Pop;
    end;

  F := TEpiField.CreateField(nil, ftInteger);
  F.Size := 0;
  FSelectStack.Push(TEpiIntField(F));
end;

procedure TExecutor.ClearFieldVars;
var
  i: Integer;
begin
  for i := 0 to FFields.count -1 do
    FFields.Data[i].Free;

  FFields.Clear;
end;

procedure TExecutor.ClearVLSetVars;
var
  i: Integer;
begin
  for i := 0 to FVLSets.count -1 do
    FVLSets.Data[i].Free;

  FVLSets.Clear;
end;

procedure TExecutor.ClearDatasetVars;
var
  i: Integer;
begin
  for i := 0 to FDataSets.count -1 do
    FDataSets.Data[i].Free;

  FDataSets.Clear;
end;

procedure TExecutor.ClearGlobals;
var
  i: Integer;
begin
  for i := 0 to FConsts.count -1 do
    FConsts.Data[i].Free;

  FConsts.Clear;
end;

procedure TExecutor.CreateDatasetVars(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer = nil);
begin
  TExecVarVector(Data).AsStringVector[FDataSets.Count] := Relation.Datafile.Name;
  FDataSets.Add(Relation.Datafile.Name, TExecutorDatasetVariable.Create(Relation.Datafile.Name, Relation));
end;

procedure TExecutor.DoError(const Msg: UTF8String);
begin
  FOutputCreator.DoError(Msg);
  FCancelled := true;
end;

procedure TExecutor.DoInfo(const Msg: UTF8String; Lvl: Word);
begin
  if Lvl = 0 then
    FOutputCreator.DoInfoShort(Msg);

  if Lvl = 1 then
    FOutputCreator.DoInfoAll(Msg);
end;

procedure TExecutor.DoWarning(const Msg: UTF8String);
begin
  FOutputCreator.DoWarning(Msg);
end;

procedure TExecutor.Error(const Msg: UTF8String);
begin
  DoError(Msg);
end;

function TExecutor.EpiVarsFromVarlist(const VariableList: TVariableList
  ): TEpiFields;
var
  i: Integer;
begin
  result := TEpiFields.Create(nil);
  result.Sorted := false;

  for i := 0 to VariableList.Count - 1 do
    Result.AddItem(FDataFile.Fields.FieldByName[VariableList[i].Ident]);
end;

function TExecutor.VariableNamesFromPrefix(const Prefix: UTF8String): TStrings;
var
  i: Integer;
  S: UTF8String;
begin
  result := TStringList.Create;

  S := UTF8UpperString(Prefix);

  for i := 0 to FDataSets.Count - 1 do
    if (UTF8Pos(S, UTF8UpperString(FDataSets.Keys[i])) = 1) then
      result.AddObject(FDataSets.Keys[i], FDataSets.Data[i]);

  for i := 0 to FVLSets.Count - 1 do
    if (UTF8Pos(S, UTF8UpperString(FVLSets.Keys[i])) = 1) then
      result.AddObject(FVLSets.Keys[i], FVLSets.Data[i]);

  for i := 0 to FFields.Count - 1 do
    if (UTF8Pos(S, UTF8UpperString(FFields.Keys[i])) = 1) then
      result.AddObject(FFields.Keys[i], FFields.Data[i]);

  for i := 0 to FConsts.Count - 1 do
    if (UTF8Pos(S, UTF8UpperString(FConsts.Keys[i])) = 1) then
      result.AddObject(FConsts.Keys[i], FConsts.Data[i]);

  // check for both pos 1 and 2 since the prefix may contain the
  // result specifier "$"
  for i := 0 to FResults.Count - 1 do
    if (UTF8Pos(S, UTF8UpperString(FResults.Keys[i])) in [1,2]) then
      result.AddObject(FResults.Keys[i], FResults.Data[i]);
end;

procedure TExecutor.UpdateFieldResultVar;
begin
  DoUpdateFieldResultVar;
end;

procedure TExecutor.UpdateDatasetResultVar;
begin
  DoUpdateDatasetResultVar;
end;

procedure TExecutor.UpdateValuelabelsResultVar;
begin
  DoUpdateValuelabelsResultVar;
end;

function TExecutor.PrepareDatafile(SelectField: TStrings;
  MissingFields: TStrings; Options: TPrepareDatasetOptions): TEpiDataFile;
begin
  result := DoPrepareDatafile(SelectField, MissingFields, Options);
end;

procedure TExecutor.DoUseDatafile(ADataFile: TEpiDataFile);
var
  F: TEpiField;
  i: Integer;
begin
  ClearSelectStack;

  FDataFile := ADataFile;
  DoUpdateFieldResultVar;

  if (not Assigned(DataFile)) then
    Exit;

  F := TEpiIntField(TEpiField.CreateField(nil, ftInteger));
  F.Size := FDataFile.Size;
  for i := 0 to FDataFile.Size -1 do
    F.AsInteger[i] := i;
  // trick the select stack on recalculations to always select all!
  F.AddCustomData(SELECTION_EXPR, TBooleanLiteral.Create(true));
  FSelectStack.Push(TEpiIntField(F));
end;

procedure TExecutor.DoUpdateFieldResultVar;
var
  RV: TExecVarVector;
  i: Integer;
  F: TEpiField;
begin
  ClearFieldVars;

  if (not Assigned(DataFile)) then
    begin
      RV := AddResultVector('$variable', ftString, 0);
      Exit;
    end
  else
    RV := AddResultVector('$variable', ftString, FDataFile.Fields.Count);

  i := 0;
  for F in FDataFile.Fields do
    begin
      FFields.add(F.Name, TExecVarField.Create(F));
      RV.AsStringVector[i] := F.Name;
      inc(i);
    end;
end;

procedure TExecutor.DoUpdateDatasetResultVar;
begin
  ClearDatasetVars;
  Document.Relations.OrderedWalk(@CreateDatasetVars, AddResultVector('$dataset', ftString, Document.DataFiles.Count));
  DoGUIInteraction(gaProjectTree);
end;

procedure TExecutor.DoUpdateValuelabelsResultVar;
var
  i: Integer;
  VLresult: TExecVarVector;
  VL: TEpiValueLabelSet;
begin
  ClearVLSetVars;

  i := 0;

  VLresult := AddResultVector('$valuelabelset', ftString, Document.ValueLabelSets.Count);
  for VL in Document.ValueLabelSets do
    begin
      FVLSets.Add(VL.Name, TExecutorValuelabelsetVariable.Create(VL.Name, VL));
      VLresult.AsStringVector[i] := VL.Name;
      Inc(i);
    end;
end;

procedure TExecutor.DoUpdateProjectresultVar;
begin
  if Assigned(FDocFile) then
    begin
      AddResultConst('$filename', ftString).AsStringVector[0] := ExtractFileName(FDocFile.FileName);
      AddResultConst('$filepath', ftString).AsStringVector[0] := ExtractFilePath(FDocFile.FileName);
    end
  else
    begin
      DeleteExecVar('$filename');
      DeleteExecVar('$filepath');
    end;
end;

function TExecutor.GetDocument: TEpiDocument;
begin
  result := nil;

  if Assigned(FDocFile) then
    result := FDocFile.Document;
end;

procedure TExecutor.OpenFileError(const Msg: string);
begin
  DoError(Msg);
end;

procedure TExecutor.DMDocFileCreated(Sender: TObject; DocFile: TEpiDocumentFile
  );
begin
  FOldWarning := DocFile.OnWarning;
  DocFile.OnWarning := @SaveWarning;
end;

function TExecutor.SaveWarning(WarningType: TOpenEpiWarningType;
  const Msg: string): TOpenEpiWarningResult;
var
  S: UTF8String;
begin
  case WarningType of
    wtLockFile:
      S := 'This file is locked by another program!' + LineEnding +
           'You can force a read/save by using the option !force';

    wtLockFileMissing:
      S := 'The lock file is missing!' + LineEnding +
           'The project file may have been edited by another EpiData program!' + LineEnding +
           'You can force a read/save by using the option !force'
  else
    begin
      if Assigned(FOldWarning) then
        result := FOldWarning(WarningType, Msg);
      Exit;
    end;
  end;

  if (Assigned(FSaveOptions)) and
     (FSaveOptions.HasOption('force'))
  then
    result := wrYes
  else
    begin
      DoError(S);
      Result := wrNo;
    end;
end;

function TExecutor.PrepareDatafilePack(Sender: TEpiDataFile; Index: Integer
  ): boolean;
var
  F: TEpiField;
  S: String;
begin
  // ALWAYS delete obs. if not in current select;
  if (FInverseSelectVector.IsMissing[Index]) then
    Exit(true);

  // Delete this obs. if it is marked for deletion
  result := (Sender.Deleted[Index] and FDropDeleted);

  // Delete this obs. if it is part of the fields that must be checked for missing
  if Assigned(FMissingFields) then
    for S in FMissingFields do
      begin
        F := Sender.Fields.FieldByName[S];
        Result := Result or (F.IsMissing[Index] or F.IsMissingValue[Index]);
      end;

  // Invert the selection if requestion
  if (pdoInvertDelete in FPrepareDFOptions) then
    Result := not Result;
end;

function TExecutor.DoPrepareDatafile(SelectField: TStrings;
  MissingFields: TStrings; Options: TPrepareDatasetOptions): TEpiDataFile;
var
  i: Integer;
  F, DeleteVec, ObsNoVec: TEpiField;
  S: String;
  CloneDoc: TEpiDocument;
  Idx: Int64;
begin
  CloneDoc := TEpiDocument(FDocFile.Document.Clone);
  Result := CloneDoc.DataFiles.GetDataFileByName(FDataFile.Name);

  if (not Assigned(SelectField)) or (SelectField.Count = 0) then
    begin
      if (not Assigned(SelectField)) then
        SelectField := TStringListUTF8.Create;

      for F in SortedFields do
        SelectField.Add(F.Name);
    end;

  for i := CloneDoc.DataFiles.Count - 1 downto 0 do
    if Result <> CloneDoc.DataFiles[i] then
      CloneDoc.DataFiles.DeleteItem(I).Free;

  if (pdoAddOrgObsNo in Options) then
    begin
      // Add a vector with the original record numbers!
      ObsNoVec := Result.NewField(ftInteger);
      ObsNoVec.Name := ANA_EXEC_PREPAREDS_OBSNO_FIELD;
      for i := 0 to Result.Size - 1 do
        ObsNoVec.AsInteger[i] := i;

      SelectField.Add(ANA_EXEC_PREPAREDS_OBSNO_FIELD);
    end;

  if (pdoIgnoreDeleteSetOption in Options) then
    FDropDeleted := false
  else
    FDropDeleted := (UTF8UpperString(FOptions[ANA_SO_INCLUDE_DELETED].Value) = 'OFF');

  FMissingFields := MissingFields;
  FPrepareDFOptions := Options;

  // Build a vector that is non-missing on entries where the select vector is active.
  FInverseSelectVector := TEpiField.CreateField(nil, ftInteger);
  FInverseSelectVector.Size := Result.Size;
  FInverseSelectVector.ResetData;
  for i := 0 to SelectVector.Size - 1 do
    FInverseSelectVector.AsInteger[SelectVector.AsInteger[i]] := 1;

  Result.Pack(@PrepareDatafilePack);

  FInverseSelectVector.Free;

  // This also removed the DeleteVec
  for i := Result.Fields.Count -1 downto 0 do
    begin
      F := Result.Field[i];
      if SelectField.IndexOf(F.Name) < 0 then
        F.Free;
    end;

  if (pdoAddOrgObsNo in Options) then
    SelectField.Delete(SelectField.IndexOf(ANA_EXEC_PREPAREDS_OBSNO_FIELD));
end;

procedure TExecutor.DeleteExecVar(const Ident: UTF8String);
var
  Idx: Integer;
begin
  if FConsts.Find(Ident, Idx) then
    begin
      FConsts.Data[Idx].Free;
      FConsts.Remove(Ident);
    end;

  if FResults.Find(Ident, Idx) then
    begin
      FResults.Data[Idx].Free;
      FResults.Remove(Ident);
    end;
end;

function TExecutor.GetModified: boolean;
begin
  result := false;

  if Assigned(FDocFile) then
    result := FDocFile.Document.Modified;
end;

function TExecutor.GetExecDataVariable(const Ident: UTF8String
  ): TCustomExecutorDataVariable;
var
  Idx: Integer;
begin
  Result := nil;

  Idx := FFields.IndexOf(Ident);
  if (Idx >= 0) then
    result := FFields.Data[Idx];

  if FConsts.find(Ident, Idx) then
    result := FConsts.Data[Idx];

  if FResults.find(Ident, Idx) then
    result := FResults.Data[Idx];
end;

procedure TExecutor.ClearResults;
var
  V: TCustomExecutorVariable;
  i: LongInt;
begin
  for i := FResults.Count - 1 downto 0 do
    begin
      V := FResults.Data[i];

      if (V.InheritsFrom(TExecVarSystem)) then
        Continue;

      V.Free;
      FResults.Delete(i);
    end;
end;

procedure TExecutor.SetCurrentRecNo(AValue: integer);

  procedure RaiseError(Index: Integer);
  begin
    raise Exception.CreateFmt('Internal Error: Executor.SetCurrentRecNo - AValue out of bounds %i', [AValue]);
  end;

begin
  if (FCurrentRecNo = AValue) then exit;

  if (AValue < 0) or (AValue > SelectVector.Size - 1) then
    RaiseError(AValue);

  FCurrentRecNo := AValue;
end;

procedure TExecutor.AssignmentDataChangeHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  DataRec: PEpiFieldDataEventRecord;
begin
  if (EventGroup <> eegFields) then exit;
  if (TEpiFieldsChangeEventType(EventType) <> efceData) then exit;

  Inc(FAssignmentChanges);
end;

procedure TExecutor.OutputFileInformation(Doc: TEpiDocument);
var
  DF: TEpiDataFile;
  LastModified: TDateTime;
  T: TOutputTable;
begin
  LastModified := TDateTime(0);

  for DF in Doc.DataFiles do
    begin
      LastModified := Max(LastModified, DF.RecModifiedDate);
      LastModified := Max(LastModified, DF.StructureModifiedDate);
    end;

  FOutputCreator.DoInfoAll(
    'Cycle: ' + IntToStr(Doc.CycleNo) +
    ' Datasets: ' + IntToStr(Doc.DataFiles.Count) +
    ' Modified: ' + DateTimeToStr(LastModified)
  );
end;

function TExecutor.DoCrudCommonSanityCheck(ST: TCustomCrudCommand;
  Item: TEpiCustomItem): boolean;
var
  Opt: TOption;
  Evt: TExecutorVariableType;
  S: UTF8String;
  Ev: TCustomExecutorVariable;
begin
  result := false;

  if (ST.HasOption('r', Opt)) then
    begin
      S := Opt.Expr.AsIdent;
      Ev := GetExecVariable(S);

      if (Item is TEpiField)         then Evt := evtField;
      if (Item is TEpiValueLabelSet) then Evt := evtValuelabel;
      if (Item is TEpiDataFile)      then Evt := evtDataset;

      if (not Item.ValidateRename(S, false)) or
         (Assigned(EV) and (EV.VarType <> Evt))
      then
        begin
          DoError('Cannot rename to "' + S + '", name is already in use!');
          Exit;
        end;
    end;

  result := true;
end;

function TExecutor.DoCrudDataformSanityCheck(ST: TCustomCrudCommand;
  DF: TEpiDataFile): boolean;
var
  Opt, AfterOpt: TOption;
  Sz: ASTInteger;
  Ars: TEpiDataFileAfterRecordState;
  StatusbarParser: TEpiStatusbarStringParser;
  MR: TEpiMasterRelation;
  aDF: TEpiDataFile;
begin
  result := DoCrudCommonSanityCheck(ST, DF);
  if (not result) then exit;

  Result := False;

  if (St.HasOption('parent', Opt)) then
    begin
      MR := Document.Relations.MasterRelationFromDatafileName(Opt.Expr.AsIdent);
      if (not Assigned(MR)) then
      begin
        DoError('Parent dataset does not exist: ' + Opt.Expr.AsIdent);
        Exit;
      end;

      aDF := MR.Datafile;
      if aDF.KeyFields.Count = 0 then
      begin
        DoError('Parent dataset must define a key with at least 1 variable' + LineEnding +
                'before you can create a related dataset');
        Exit;
      end;

      if (MR is TEpiDetailRelation) then
      begin
        MR := TEpiDetailRelation(MR).MasterRelation;
        if MR.Datafile.KeyFields.Count = aDf.KeyFields.Count then
        begin
          DoError('Parent dataset must define a key with at least 1 variable more than' + LineEnding +
                  'in the grand-parent dataset!');
          Exit;
        end;
      end;
    end;

  // A dataform cannot have "childobs" or "afterobs" options if
  // not a child record.
  //  -  Document.Relations.MasterRelationFromDatafileName(DF.Name).InheritsFrom(TEpiDetailRelation))
  if (ST.HasOption('childobs') or ST.HasOption('afterobs')) then
    begin
      if (not Assigned(DF)) and (not ST.HasOption('parent')) then
        begin
          DoError('Options !childobs and !afterobs are only allowed if used with !parent');
          Exit;
        end;

      if (Assigned(DF)) and (not Document.Relations.MasterRelationFromDatafileName(DF.Name).InheritsFrom(TEpiDetailRelation)) then
        begin
          DoError('Options !childobs and !afterobs can only be used on a related dataset');
          Exit;
        end;
    end;

  if ST.HasOption('childobs', Opt) and
     ST.HasOption('afterobs', AfterOpt)
  then
    begin
      Sz := Opt.Expr.AsInteger;
      Ars := TEpiDataFileAfterRecordState(AfterOPt.Expr.AsInteger);

      if (Sz = 0) and
         (not (Ars in [arsNewRecord, arsReturnToParent]))
      then
        begin
          DoError('If using !childobs := 0 then !afterobs must be either 0 or 1. See help for more information.');
          Exit;
        end;

      if (Sz = 1) and
         (not (Ars in [arsReturnToParent, arsStayOnRecord]))
      then
        begin
          DoError('If using !childobs := 1 then !afterobs must be either 1 or 3. See help for more information.');
          Exit;
        end;

      if (Sz > 1) and
         (not (Ars in [arsReturnToParent, arsReturnToParentOnMax]))
      then
        begin
          DoError('If using !childobs > 1 then !afterobs must be either 1 or 2. See help for more information.');
          Exit;
        end;
    end;

  if ST.HasOption('statusbar', Opt) then
    begin
      StatusbarParser := TEpiStatusbarStringParser.Create;
      if (not StatusbarParser.ParseString(Opt.Expr.AsString)) then
        begin
          DoError('Incorrect statusbar content string: ' + Opt.Expr.AsString);
          StatusbarParser.Free;
          Exit;
        end;
      StatusbarParser.Free;
    end;

  if (ST.HasOption('r', Opt)) and
     (not DF.ValidateRename(Opt.Expr.AsIdent, false))
  then
    begin
      DoError('Cannot rename dataset, name is already in use!');
      Exit;
    end;

  if (ST.HasOption('size', Opt)) and
     (Opt.Expr.AsInteger < 0)
  then
    begin
      DoError('Invalid size for dataset: ' + Opt.Expr.AsString);
      Exit;
    end;

  result := true;
end;

function TExecutor.DoCrudVariableSanityCheck(ST: TCustomCrudCommand;
  F: TEpiField): boolean;
var
  Opt, OptLow, OptHigh: TOption;
  Idx: Integer;
  Len, Dec: ASTInteger;
  VL: TEpiValueLabelSet;
  MR: TEpiMasterRelation;
  S: UTF8String;
  Ev: TCustomExecutorVariable;
begin
  result := DoCrudCommonSanityCheck(ST, F);
  if (not result) then exit;

  Result := False;

  Len := F.Length;
  Dec := F.Decimals;

  if (ST.HasOption('l', Opt)) then
  begin
    Len := Opt.Expr.AsInteger;
    if (Len < 1) or ((F.FieldType = ftInteger) and (Len > 19)) then
    begin
      DoError('Invalid length! Must be 1 or larger');
      Exit;
    end;
  end;

  if ST.OptionList.HasOption('d', Opt) then
    begin
      if not (F.FieldType in FloatFieldTypes) then
        DoWarning('Setting decimals on non-float variables have no effect');

      Dec := Opt.Expr.AsInteger;
      if (Dec < 1) then
        begin
          DoError('Invalid decimal: ' + IntToStr(Dec));
          ST.ExecResult := csrFailed;
          Exit;
        end;
    end;

  if (ST.HasOption('vl', Opt)) then
    begin
      if (not FVLSets.Find(Opt.Expr.AsIdent, Idx)) then
         begin
           DoError('Value label set not found: ' + Opt.Expr.AsIdent);
           Exit;
         end;

      VL := FVLSets.Data[Idx].Valuelabelset;
      if (not F.AcceptsValuelabelSet(VL, Len, Dec)) then
        begin
          DoError('Value label set does not match variable size/type');
          Exit;
        end;
    end;

  if (ST.HasOption('min', OptLow)) or
     (ST.HasOption('max', OptHigh))
  then
    begin
      if (not ST.HasOption('min', OptLow)) then
        begin
          DoError('Missing !min option!');
          Exit;
        end;

      if (not ST.HasOption('max', OptHigh)) then
        begin
          DoError('Missing !max option!');
          Exit;
        end;

      if (OptLow.Expr.ResultType <> FieldTypeToASTTypeTable[F.FieldType]) then
        begin
          DoError('!min value does not have the same datatype as the variable!');
          Exit;
        end;

      if (OptHigh.Expr.ResultType <> FieldTypeToASTTypeTable[F.FieldType]) then
        begin
          DoError('!max value does not have the same datatype as the variable!');
          Exit;
        end;
    end;

  if (ST.HasOption('entry', Opt)) then
    begin
      if ((Opt.Expr.AsInteger > 2) or
          (Opt.Expr.AsInteger < 0))
      then
        begin
          DoError('Invalid entrymode value. Must be 0, 1 or 2!');
          Exit;
        end;

      if (F.FieldType in AutoFieldTypes) then
        begin
          DoError('Entrymode cannot be set on automatic variables!');
          Exit;
        end;
    end;

  if (ST.HasOption('auto', Opt)) and
     (Assigned(Opt.Expr)) and
     ((Opt.Expr.AsInteger < 0) or (Opt.Expr.AsInteger > 2))
  then
    begin
      DoError('Invalid auto mode value. Must be 0, 1 or 2!');
      Exit;
    end;

  if (ST.HasOption('key')) or
     (ST.HasOption('nokey'))
  then
    begin
      MR := Document.Relations.MasterRelationFromDatafileName(FDataFile.Name);
      if MR.DetailRelations.Count > 0 then
        begin
          DoError('The current key cannot be changed because child datasets exists!');
          Exit;
        end;

      if (ST.HasOption('key')) and
         (F.IsKeyfield)
      then
        DoWarning('Variable is already part of the key! Ignoring !key option');

      if (ST.HasOption('nokey')) and
         (not F.IsKeyfield)
      then
        DoWarning('Variable is not part of the key! Ignoring !nokey option');
    end;

  result := true;
end;

function TExecutor.DoCrudValuelabelSanityCheck(ST: TCustomCrudCommand;
  VLs: TEpiValueLabelSet): boolean;
var
  VLPairs: TValueLabelPairs;
  i, Idx: Integer;
  Values: TStringListUTF8;
  S: EpiString;
  Opt: TOption;
begin
  result := DoCrudCommonSanityCheck(ST, VLs);
  if (not result) then exit;

  Result := False;
  VLPairs := TValueLabelPairs(GetObjectProp(ST, 'ValueLabelPairs'));

  for i := 0 to ST.OptionList.Count - 1 do
    begin
      Opt := ST.OptionList.Options[i];
      if (Assigned(Opt.Expr)) and
         (Opt.Ident <> 'r')
      then
        S := Opt.Expr.AsString;

      case Opt.Ident of
        'm':
          begin
            if (not (VLs.ValueLabelExists[S] or VLPairs.Find(S, Idx))) then
              begin
                DoError('Cannot assign missing - value does not exist!');
                Exit;
              end;
          end;

        'd':
          begin
            if (not VLs.ValueLabelExists[S]) then
              begin
                DoError('Cannot delete - value does not exist!');
                Exit;
              end;
          end;

        'nom':
          begin
            if (not VLs.ValueLabelExists[S]) then
              begin
                DoError('Cannot remove missing - value does not exist!');
                Exit;
              end;
          end;
      end;
    end;

//  Values.Free;
  result := true;
end;

procedure TExecutor.DoNewProject(St: TNewProject);
var
  Doc: TEpiDocument;
  Opt: TOption;
  TmpST:  TCustomStatement;
  OptList: TOptionList;
begin
  ST.ExecResult := csrFailed;

  if (Assigned(DocFile)) then
  begin
    if (Document.Modified) and
       (not ST.HasOption('c'))
    then
      begin
        DoError('Close project before creating new!');
        Exit;
      end;

    TmpST := TCustomEmptyCommand.Create(nil, 'close');
    ExecClose(TCustomEmptyCommand(TmpST));
    TmpST.Free;
  end;

  FDocFile  := TEpiDocumentFile.Create;
  Doc := FDocFile.CreateNewDocument('en');
  EpiAsyncHandlerGlobal.AddDocument(Doc);

  if ST.HasOption('title', Opt) then
    Doc.Study.Title.Text := Opt.Expr.AsString
  else
    Doc.Study.Title.Text := 'Untitled';

  DoInfo('New project created: ' + Doc.Study.Title.Text);

  if ST.HasOption('size', Opt) then
  begin
    OptList := TOptionList.Create;
    OptList.Add(Opt);
    OptList.Add(TOption.Create(TVariable.Create('label', Self), TStringLiteral.Create('Dataform 1')));
    TmpST := TNewDataset.Create(TVariable.Create('ds1', Self), OptList);
    DoNewDataform(TNewDataset(TmpST));
  end else
    DoUseDatafile(nil);

  DoUpdateProjectresultVar;
  DoUpdateValuelabelsResultVar;
  St.ExecResult := csrSuccess;
end;

procedure TExecutor.DoNewDataform(St: TNewDataset);
var
  MR: TEpiMasterRelation;
  DR: TEpiDetailRelation absolute MR;
  Opt: TOption;
  Rel: TEpiDatafileRelationList;
  DF: TEpiDataFile;
  ParentKeyField, NewKeyField: TEpiField;
  Ft: TEpiFieldType;
begin
  // Complete sanity check for all co-dependant options (like afterrec,...)
  // before applying options

  if not DoCrudDataformSanityCheck(ST, nil) then
  begin
    ST.ExecResult := csrFailed;
    Exit;
  end;

  // ======================
  // Sanity check done!
  // ======================
  if ST.HasOption('parent', Opt) then
    Rel := Document.Relations.MasterRelationFromDatafileName(Opt.Expr.AsIdent).DetailRelations
  else
    Rel := Document.Relations;

  MR := Rel.NewMasterRelation;
  DF := Document.DataFiles.NewDataFile;
  DF.Name := ST.Variable.Ident;
  MR.Datafile := DF;

  if ST.HasOption('parent') then
    for ParentKeyField in Datafile.KeyFields do
    begin
      // In a related datafile, the "primary" key cannot contain autoinc - it would
      // screw up the numbering.
      Ft := ParentKeyField.FieldType;
      if Ft = ftAutoInc then Ft := ftInteger;

      NewKeyField := DF.NewField(Ft);
      NewKeyField.Assign(ParentKeyField);
      NewKeyField.EntryMode := emNoEnter;
      DF.KeyFields.AddItem(NewKeyField);
    end;

  if (ST.HasOption('size', Opt)) then
    DF.Size := Opt.Expr.AsInteger;

  if (ST.HasOption('label', Opt)) then
    DF.Caption.Text := Opt.Expr.AsString;

  if (St.HasOption('childrec', Opt)) then
    DR.MaxRecordCount := Opt.Expr.AsInteger;

  if (St.HasOption('afterrec', Opt)) then
    DF.AfterRecordState := TEpiDataFileAfterRecordState(Opt.Expr.AsInteger);

  if (St.HasOption('statusbar', Opt)) then
    DF.StatusbarContentString := Opt.Expr.AsString;


  DoInfo('New dataform created: ' + DF.Caption.Text + LineEnding +
         '(' + IntToStr(DF.Size) + ' observations)');

  DoUseDatafile(DF);
  DoUpdateDatasetResultVar;
end;

procedure TExecutor.DoNewValueLabel(St: TNewValuelabel);
var
  VLSet: TEpiValueLabelSet;
  VL: TEpiCustomValueLabel;
  VLPairs: TValueLabelPairs;
  Opt: TOption;
  i: Integer;
begin
  ST.ExecResult := csrFailed;

  VLSet := Document.ValueLabelSets.NewValueLabelSet(ST.NewType);
  VLSet.Name := ST.Variable.Ident;

  if (not DoCrudValuelabelSanityCheck(ST, VLSet)) then
    begin
      VLSet.Free;
      Exit;
    end;

  VLPairs := ST.ValueLabelPairs;
  if Assigned(VLPairs) then
    for i := 0 to VLPairs.Count -1 do
      begin
        if VLSet.ValueLabelExists[VLPairs.Values[i].AsString] then
          VL := VLSet.ValueLabel[VLPairs.Values[i].AsString]
        else
          VL := VLSet.NewValueLabel;

        VL.TheLabel.Text := VLPairs.LabelText[i];
        case VLSet.LabelType of
          ftInteger: TEpiIntValueLabel(VL).Value    := VLPairs.Values[i].AsInteger;
          ftFloat:   TEpiFloatValueLabel(VL).Value  := VLPairs.Values[i].AsFloat;
          ftString:  TEpiStringValueLabel(VL).Value := VLPairs.Values[i].AsString;
          ftDMYDate,
          ftMDYDate,
          ftYMDDate: TEpiDateValueLabel(VL).Value   := VLPairs.Values[i].AsDate;
          ftTime:    TEpiTimeValueLabel(VL).Value   := VLPairs.Values[i].AsTime;
        end;
      end;


  for i := 0 to ST.OptionList.Count - 1 do
    begin
      Opt := ST.OptionList.Options[i];
      if Opt.Ident <> 'm' then continue;

      VLSet.ValueLabel[Opt.Expr.AsString].IsMissingValue := true;
    end;

  DoInfo('New valuelabel created:' + LineEnding +
         Format('(%d (value, label) pairs, %d missing)', [VLSet.Count, VLSet.MissingCount])
  );

  DoUpdateValuelabelsResultVar;
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.DoCrudVariableOptions(St: TCustomCrudCommand; F: TEpiField);
var
  Opt: TOption;
  R: TEpiRange;

  procedure AddCompare(TargetName: UTF8String; CompareType: TEpiComparisonType);
  begin
    if (not Assigned(F.Comparison)) then
      F.Comparison := TEpiComparison.Create(F);

    F.Comparison.CompareField := FDataFile.Fields.FieldByName[TargetName];
    F.Comparison.CompareType := CompareType;
  end;

begin
  if ST.HasOption('r', Opt) then
    F.Name := Opt.Expr.AsIdent;

  if ST.HasOption('label', Opt) then
    F.Question.Text := Opt.Expr.AsString;

  if ST.HasOption('l', Opt) then
    F.Length := Opt.Expr.AsInteger;

  if ST.HasOption('d', Opt) then
    F.Decimals := Opt.Expr.AsInteger;

  if ST.HasOption('vl', Opt) then
    F.ValueLabelSet := TEpiValueLabelSet(Document.ValueLabelSets.GetItemByName(Opt.Expr.AsIdent));

  if ST.HasOption('novl') then
    F.ValueLabelSet := nil;

  if ST.HasOption('min', Opt) then
    begin
      if (Assigned(F.Ranges)) then
        F.Ranges.Free;

      F.Ranges := TEpiRanges.Create(F);
      R := F.Ranges.NewRange;
      case F.FieldType of
        ftInteger:
          R.AsInteger[true] := Opt.Expr.AsInteger;

        ftFloat:
          R.AsFloat[true] := Opt.Expr.AsFloat;

        ftDMYDate,
        ftMDYDate,
        ftYMDDate:
          R.AsDate[true] := Opt.Expr.AsDate;

        ftTime:
          R.AsTime[true] := Opt.Expr.AsTime;
      end;

      ST.HasOption('max', Opt);
      case F.FieldType of
        ftInteger:
          R.AsInteger[false] := Opt.Expr.AsInteger;

        ftFloat:
          R.AsFloat[false] := Opt.Expr.AsFloat;

        ftDMYDate,
        ftMDYDate,
        ftYMDDate:
          R.AsDate[false] := Opt.Expr.AsDate;

        ftTime:
          R.AsTime[false] := Opt.Expr.AsTime;
      end;
    end;

  if ST.HasOption('norange') then
    begin
      F.Ranges.Free;
      F.Ranges := nil;
    end;

  if ST.HasOption('entry', Opt) then
    F.EntryMode := TEpiEntryMode(Opt.Expr.AsInteger);

  if ST.HasOption('confirm') then
    F.ConfirmEntry := true;

  if ST.HasOption('noconfirm') then
    F.ConfirmEntry := False;

  if ST.HasOption('key') and
     (not F.IsKeyfield)
  then
    FDataFile.KeyFields.AddItem(F);

  if ST.HasOption('nokey') and
     (F.IsKeyfield)
  then
    FDataFile.KeyFields.RemoveItem(F);

  if ST.HasOption('auto', Opt) then
    if Assigned(Opt.Expr) then
      TEpiCustomAutoField(F).AutoMode := TEpiAutoUpdateMode(Opt.Expr.AsInteger)
    else
      TEpiCustomAutoField(F).AutoMode := umCreated;

  if ST.HasOption('cmpEQ', Opt) then AddCompare(Opt.Expr.AsIdent, fcEq);
  if ST.HasOption('cmpNE', Opt) then AddCompare(Opt.Expr.AsIdent, fcNEq);
  if ST.HasOption('cmpGT', Opt) then AddCompare(Opt.Expr.AsIdent, fcGT);
  if ST.HasOption('cmpLT', Opt) then AddCompare(Opt.Expr.AsIdent, fcLT);
  if ST.HasOption('cmpGE', Opt) then AddCompare(Opt.Expr.AsIdent, fcGEq);
  if ST.HasOption('cmpLE', Opt) then AddCompare(Opt.Expr.AsIdent, fcLEq);

  if St.HasOption('nocmp') then
    if (Assigned(F.Comparison)) then
      begin
        F.Comparison.Free;
        F.Comparison := nil;
      end;

  DoUpdateFieldResultVar;
end;

procedure TExecutor.DoNewVariable(St: TNewVariable);
var
  Section: TEpiSection;
  OldV, F: TEpiField;
  lTop, lLeft: Integer;
  Opt: TOption;
  R: TEpiRange;
  ft: TEpiFieldType;
  Auto: Boolean;
begin
  Section := FDataFile.MainSection;

  if Section.Fields.Count > 0 then
    begin
      OldV  := Section.Fields[Section.Fields.Count - 1];
      lTop  := OldV.Top + 35;
      lLeft := OldV.Left;
    end
  else
    begin
      lTop  := 20;
      lLeft := 80;
    end;

  ft := ST.NewType;
  Case ft of
    ftInteger:
      if ST.HasOption('auto') then ft := ftAutoInc;

    ftDMYDate:
      begin
        if ST.HasOption('dmy')  then ft := ftDMYDate;
        if ST.HasOption('mdy')  then ft := ftMDYDate;
        if ST.HasOption('ymd')  then ft := ftYMDDate;
        if ST.HasOption('auto') then ft := Succ(Succ(Succ(ft)));
      end;

    ftTime:
      if ST.HasOption('auto') then ft := ftTimeAuto;

    ftString:
      begin
        if ST.HasOption('u')    then ft := ftUpperString;
        if ST.HasOption('memo') then ft := ftMemo;
      end
  end;

  F := Section.NewField(ft);
  F.Name := ST.Variable.Ident;
  F.Top  := lTop;
  F.Left := lLeft;

  // Complete sanity check for all co-dependant options (like afterrec,...)
  // before applying options
  if not DoCrudVariableSanityCheck(ST, F) then
  begin
    F.Free;
    ST.ExecResult := csrFailed;
    Exit;
  end;

  DoCrudVariableOptions(St, F);

  try
    if Assigned(ST.ValueExpr) then
      ExecAssignment(TAssignment.Create(TVariable.Create(F.Name, Self), ST.ValueExpr));
  except
  end;

  DoInfo('New variable created: ' + F.Name + LineEnding +
         ' type     = ' + EpiTypeNames[F.FieldType] + LineEnding +
         ' length   = ' + IntToStr(F.Length) + LineEnding +
         ' decimals = ' + IntToStr(F.Decimals)
        );
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.DoNewGlobal(St: TCustomNewValued);
var
  GV: TNewGlobalVector;
  NG: TNewGlobal;
  i, Idx: Integer;
  P: TParamList;
  IV: TIndexVariable;
  EV: TCustomExecutorDataVariable;
begin
  if (FConsts.Find(ST.Variable.Ident, i)) and
     (ST.HasOption('replace'))
  then
    begin
      FConsts.Data[i].Free;
      FConsts.Delete(i);
    end;

  if ST is TNewGlobalVector then
    begin
      GV := TNewGlobalVector(ST);

      if (not FConsts.Find(ST.Variable.Ident, Idx)) then
        Idx := FConsts.Add(ST.Variable.Ident, TExecVarGlobalVector.Create(ST.Variable.Ident, GV.NewType, GV.VectorExpr.AsInteger));

      EV := FConsts.Data[Idx];

      if Assigned(GV.ValueExpr) then
        for i := 1 to GV.VectorExpr.AsInteger do
      {    case GV.Variable.ResultType of
            rtBoolean:
              EV.AsBooleanVector[i - 1] := GV.ValueExpr.AsBoolean;

            rtInteger:
              EV.AsIntegerVector[i - 1] := GV.ValueExpr.AsInteger;

            rtDate:
              EV.AsDateVector[i - 1]    := GV.ValueExpr.AsDate;

            rtFloat:
              EV.AsFloatVector[i - 1]   := GV.ValueExpr.AsFloat;

            rtTime:
              EV.AsTimeVector[i - 1]    := GV.ValueExpr.AsTime;

            rtString:
              EV.AsStringVector[i - 1]  := GV.ValueExpr.AsString;
          end;  }

          begin
            P := TParamList.Create;
            P.Add(TIntegerLiteral.Create(i));
            // TODO: Is this really nessesary?
            IV := TIndexVariable.Create(TVariable.Create(GV.Variable.Ident, Self), Self, P);
            IV.TypeCheck(Self, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData, [evfInternal, evfAsValue]));
            ExecAssignment(TAssignment.Create(IV, GV.ValueExpr));
            P.Free;
          end;
    end
  else
    begin
      NG := TNewGlobal(ST);

      if (not FConsts.Find(ST.Variable.Ident, i)) then
        FConsts.Add(NG.Variable.Ident, TExecVarGlobal.Create(NG.Variable.Ident, NG.NewType));

      if Assigned(NG.ValueExpr) then
        ExecAssignment(TAssignment.Create(TVariable.Create(NG.Variable.Ident, Self), NG.ValueExpr));
    end;

  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.InitSetOptions;
var
  SOpt: TSetOption;
begin
  FOptions := TSetOptionsMap.create;

  FOptions.Insert(ANA_SO_EXITSAVE,                       TSetOption.Create('NO', rtBoolean));

  FOptions.Insert(ANA_SO_COMMANDLOG,                     TSetOption.Create('ON', rtBoolean));
  FOptions.Insert(ANA_SO_COMMANDLOGFILE,                 TSetOption.Create('commandlog.pgm', rtString));
  FOptions.Insert(ANA_SO_COMMANDLOGLINES,                TSetOption.Create('1000', rtInteger));

  FOptions.Insert(ANA_SO_CLIPBOARD_DELIMITER,            TSetOption.Create(',',  rtString));

  FOptions.Insert(ANA_SO_ECHO,                           TSetOption.Create('ON', rtBoolean));
  FOptions.Insert(ANA_SO_SHOW_COMMAND,                   TSetOption.Create('ON', rtBoolean));
  FOptions.Insert(ANA_SO_SHOW_DEBUG,                     TSetOption.Create('ON', rtBoolean));
  FOptions.Insert(ANA_SO_SHOW_ERROR,                     TSetOption.Create('ON', rtBoolean));

  SOpt := TSetOption.Create('ON', rtString);
  SOpt.LegalValues.Add('ON');
  SOpt.LegalValues.Add('SHORT');
  SOpt.LegalValues.Add('OFF');
  FOptions.Insert(ANA_SO_SHOW_INFO,                      SOpt);
  FOptions.Insert(ANA_SO_SHOW_WARNING,                   TSetOption.Create('ON', rtBoolean));

  FOptions.Insert(ANA_SO_EDITOR_FONT_SIZE,               TSetOption.Create('10',      rtInteger));
  FOptions.Insert(ANA_SO_EDITOR_FONT_NAME,               TSetOption.Create({$IFDEF MSWINDOWS}'Courier New'{$ENDIF}{$IFDEF LINUX}'DejaVu Sans Mono'{$ENDIF}{$IFDEF DARWIN}'Courier New'{$ENDIF}, rtString));

  FOptions.Insert(ANA_SO_OUTPUT_FONT_SIZE,               TSetOption.Create('10',      rtInteger));
  FOptions.Insert(ANA_SO_OUTPUT_FONT_NAME,               TSetOption.Create({$IFDEF MSWINDOWS}'Courier New'{$ENDIF}{$IFDEF LINUX}'DejaVu Sans Mono'{$ENDIF}{$IFDEF DARWIN}'Courier New'{$ENDIF}, rtString));
  FOptions.Insert(ANA_SO_OUTPUT_FONT_COLOR,              TFontColorOption.Create('#000000', rtString));
  FOptions.Insert(ANA_SO_OUTPUT_FONT_STYLE,              TFontStyleOption.Create('',        rtString));
  FOptions.Insert(ANA_SO_OUTPUT_BG_COLOR,                TFontColorOption.Create('#FFFFFF', rtString));

  FOptions.Insert(ANA_SO_CMDEDIT_FONT_SIZE,              TSetOption.Create('10',      rtInteger));
  FOptions.Insert(ANA_SO_CMDEDIT_FONT_NAME,              TSetOption.Create({$IFDEF MSWINDOWS}'Courier New'{$ENDIF}{$IFDEF LINUX}'DejaVu Sans Mono'{$ENDIF}{$IFDEF DARWIN}'Courier New'{$ENDIF}, rtString));
  FOptions.Insert(ANA_SO_CMDEDIT_FONT_COLOR,             TFontColorOption.Create('#000000', rtString));
  FOptions.Insert(ANA_SO_CMDEDIT_FONT_STYLE,             TFontStyleOption.Create('',        rtString));
  FOptions.Insert(ANA_SO_CMDEDIT_BG_COLOR,               TFontColorOption.Create('#FFFFFF', rtString));

  FOptions.Insert(ANA_SO_BROWSER_FONT_SIZE,              TSetOption.Create('10',      rtInteger));
  FOptions.Insert(ANA_SO_BROWSER_FONT_NAME,              TSetOption.Create({$IFDEF MSWINDOWS}'Courier New'{$ENDIF}{$IFDEF LINUX}'DejaVu Sans Mono'{$ENDIF}{$IFDEF DARWIN}'Courier New'{$ENDIF}, rtString));
  FOptions.Insert(ANA_SO_BROWSER_FONT_COLOR,             TFontColorOption.Create('#000000', rtString));
  FOptions.Insert(ANA_SO_BROWSER_FONT_STYLE,             TFontStyleOption.Create('',        rtString));
  FOptions.Insert(ANA_SO_BROWSER_BG_COLOR,               TFontColorOption.Create('#FFFFFF', rtString));
  FOptions.Insert(ANA_SO_BROWSER_OBS_DEFAULT_COLOR,      TFontColorOption.Create('#F0F0F0', rtString));
  FOptions.Insert(ANA_SO_BROWSER_OBS_DELETED_COLOR,      TFontColorOption.Create('#FF0000', rtString));
  FOptions.Insert(ANA_SO_BROWSER_OBS_VERIFIED_COLOR,     TFontColorOption.Create('#008080', rtString));

  // OUTPUT VIEWER
  SOpt := TSetOption.Create('TEXT', rtString);
  SOpt.LegalValues.Add('HTML');
  SOpt.LegalValues.Add('TEXT');
  {$IFDEF EPI_CHROMIUM_HTML}
  SOpt.LegalValues.Add('OLDHTML');
  SOpt.LegalValues.Add('OSR');
  {$ENDIF}
  FOptions.Insert(ANA_SO_OUTPUT_FORMAT, SOpt);

  SOpt := TSetOption.Create('L', rtString);
  SOpt.LegalValues.Add('L');
  SOpt.LegalValues.Add('V');
  SOpt.LegalValues.Add('VL');
  SOpt.LegalValues.Add('LV');
  FOptions.Insert(ANA_SO_STATISTICS_VALUE_LABEL, SOpt);

  SOpt := TSetOption.Create('VLA', rtString);
  SOpt.LegalValues.Add('VN');
  SOpt.LegalValues.Add('VNL');
  SOpt.LegalValues.Add('VLN');
  SOpt.LegalValues.Add('VLA');
  FOptions.Insert(ANA_SO_STATISTICS_VARIABLE_LABEL, SOpt);

  SOpt := TSetOption.Create('V', rtString);
  SOpt.LegalValues.Add('L');
  SOpt.LegalValues.Add('V');
  SOpt.LegalValues.Add('VL');
  SOpt.LegalValues.Add('LV');
  FOptions.Insert(ANA_SO_BROWSE_VALUE_LABEL, SOpt);

  SOpt := TSetOption.Create('VN', rtString);
  SOpt.LegalValues.Add('VN');
  SOpt.LegalValues.Add('VNL');
  SOpt.LegalValues.Add('VLN');
  SOpt.LegalValues.Add('VLA');
  FOptions.Insert(ANA_SO_BROWSE_VARIABLE_LABEL, SOpt);

  FOptions.Insert(ANA_SO_INCLUDE_DELETED, TSetOption.Create('OFF', rtBoolean));
end;

function TExecutor.GetSetOptionValue(const Key: UTF8String): UTF8String;
begin
  result := SetOptions.GetValue(Key).Value;
end;

procedure TExecutor.SetSetOptionValue(const Key: UTF8String; AValue: UTF8String
  );
begin
  SetOptions.GetValue(Key).Value := AValue;
end;

function TExecutor.GetSortedFields: TEpiFields;
begin
  result := FDataFile.Fields;
end;

function TExecutor.AddResultConst(const Ident: UTF8String;
  DataType: TEpiFieldType): TExecVarGlobal;
begin
  DeleteExecVar(Ident);

  Result := TExecVarResultConst.Create(Ident, DataType);
  FResults.Add(Ident, Result);
end;

function TExecutor.AddResultVector(const Ident: UTF8String;
  DataType: TEpiFieldType; Length: Integer): TExecVarVector;
begin
  DeleteExecVar(Ident);

  Result := TExecVarVector.Create(Ident, DataType, Length);
  FResults.Add(Ident, Result);
end;

function TExecutor.AddResultMatrix(const Ident: UTF8String;
  DataType: TEpiFieldType; Cols, Rows: Integer): TExecVarMatrix;
begin
  DeleteExecVar(Ident);

  Result := TExecVarMatrix.Create(Ident, DataType, Cols, Rows);
  FResults.Add(Ident, Result);
end;

function TExecutor.GetExecVariable(const Ident: UTF8String
  ): TCustomExecutorVariable;
var
  Idx: LongInt;
begin
  result := nil;

  Idx := FDataSets.IndexOf(Ident);
  if (Idx >= 0) then
    result := FDataSets.Data[Idx];

  if FVLSets.Find(Ident, Idx) then
    result := FVLSets.Data[Idx];

  if not Assigned(result) then
    Result := GetExecDataVariable(Ident);
end;

procedure TExecutor.ExecRead(ST: TCustomStringCommand);
var
  FN: UTF8String;
  i: Integer;
  VL: TEpiValueLabelSet;
  TmpDocFile: TEpiDocumentFile;
  EC: TCustomEmptyCommand;
  Rel: TEpiMasterRelation;
  VLresult: TExecVarVector;
begin
  FN := '';

  if Assigned(St.StringExpr) then
    FN := ST.StringExpr.AsString;

  if Modified and
     (not ST.HasOption('c'))
  then
    begin
      DoError('Project is modified. Close project first!');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  if Assigned(FDocFile) then
    begin
      EC := TCustomEmptyCommand.Create(nil, 'close');
      ExecStatement(EC);
      EC.Free;
    end;

  if (FN <> '') then
    begin
      if (not FileExistsUTF8(FN)) then
      begin
        DoError('File does not exist: ' + FN);
        ST.ExecResult := csrFailed;
        Exit;
      end;
    end;

  try
    aDM.OnOpenFileError := @OpenFileError;
    aDm.OnDocFileCreated := @DMDocFileCreated;
    FSaveOptions := ST.Options;
    case aDM.OpenFile(ST, TmpDocFile) of
      dfrCanceled:
        begin
          DoInfo('Read cancelled!');
          FCancelled := true;
          ST.ExecResult := csrFailed;
          Exit;
        end;
      dfrError:
        begin
          DoError('Error loading file: ' + FN);
          ST.ExecResult := csrFailed;
          Exit;
        end;
      dfrSuccess:
        FN := TmpDocFile.FileName;
    end;
  except
    on E: Exception do
      begin
        ST.ExecResult := csrFailed;
        DoError(E.Message);
        Exit;
      end;
  end;

  FDocFile := TmpDocFile;
  FDocFile.OnWarning := FOldWarning;
  FSaveOptions := nil;
  DoUpdateDatasetResultVar;
  DoUpdateValuelabelsResultVar;

  for Rel in Document.Relations do
    if (not Rel.ProtectedItem) then
      break;

  DoUseDatafile(Rel.Datafile);

  FDocFile.Document.Modified := false;
  DoInfo('Loaded file: ' + ExpandFileNameUTF8(FN));

  DoUpdateProjectresultVar;
  OutputFileInformation(FDocFile.Document);
  DoGUIInteraction(gaTitle);

  AddToRecent(FN, GetRecentDataIniFileName, RecentDataFiles);
end;

procedure TExecutor.ExecSave(ST: TCustomStringCommand);
var
  FN: UTF8String;
  Setting: TEpiExportSettingClass;
  ExportSetting: TEpiExportSetting;
  DFSetting: TEpiExportDatafileSettings;
  F: TEpiField;
  Exporter: TEpiExport;
  Ft: TEpiDialogFilter;
  opt: TOption;
  Ver: ASTInteger;
  SaveOutput: TSaveOutput;
  DF: TEpiDataFile;
begin
  FN := '';

  if (ST.HasOption('output', Opt)) then
    begin
      SaveOutput := TSaveOutput.Create(Self, FOutputCreator);
      SaveOutput.ExecSaveOutput(ST);
      SaveOutput.Free;

      Exit;
    end;

  if (Assigned(DataFile)) and
     (SelectVector.Size <> DataFile.Size)
  then
    begin
      DoError('do NOT combine SAVE with SELECT - Cannot save a partial dataset!');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  if Assigned(ST.StringExpr) then
    FN := ST.StringExpr.AsString;

  if (FN = '') then
    begin
      if (not aDM.SaveDialog1.Execute) then
        begin
          DoInfo('Save cancled');
          ST.ExecResult := csrFailed;
          Exit;
        end;

      FN := aDM.SaveDialog1.FileName;

      // TODO: Really bad use of aDM...
      if assigned(aDM.OnDialogFilename) then
        aDM.OnDialogFilename(FN);

      if FileExistsUTF8(FN) then
        ST.Options.Add(TOption.Create(TVariable.Create('replace', Self), nil));
    end;

  if (FileExistsUTF8(FN)) and
     (not ST.HasOption('replace'))
  then
    begin
      DoError('File exists.' + LineEnding + 'Add !REPLACE or erase file:' + LineEnding + FN);
      ST.ExecResult := csrFailed;
      Exit;
    end;

  if (not FilenameToFileType(FN, Ft)) then
    begin
      DoError('Unknown filetype "' + ExtractFileExt(FN) + '" not supported!');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  case ft of
    dfText:
      Setting := TEpiCSVExportSetting;

    dfDTA:
      Setting := TEpiStataExportSetting;

    dfEPX,
    dfEPZ:
      Setting := TEpiEPXExportSetting;
  else
    if (not ST.HasOption('format')) then
      begin
        DoError('Saving to "' + ExtractFileExt(FN) + '" not supported!');
        ST.ExecResult := csrFailed;
        Exit;
      end;
  end;

  if ST.HasOption('format', opt) then
    Case opt.Expr.AsString of
      'stata':
        Setting := TEpiStataExportSetting;

      'epidata':
        Setting := TEpiEPXExportSetting;

      'csv':
        Setting := TEpiCSVExportSetting;
    else
      DoError('Unknown save format: ' + opt.Expr.AsString);
      ST.ExecResult := csrFailed;
      Exit;
    end;

  ExportSetting := Setting.Create;
  ExportSetting.Doc := FDocFile.Document;
  ExportSetting.Encoding := eeUTF8;
  ExportSetting.ExportDeleted := true;

  if (ExportSetting.InheritsFrom(TEpiCSVExportSetting)) then
    with TEpiCSVExportSetting(ExportSetting) do
      begin
        FieldSeparator := SetOptions[ANA_SO_CLIPBOARD_DELIMITER].Value;
        if ST.HasOption('d', Opt)      then FieldSeparator   := Opt.Expr.AsString;
        if ST.HasOption('varn', Opt)   then ExportFieldNames := Opt.Expr.AsBoolean else ExportFieldNames := true;
        if ST.HasOption('q', Opt)      then QuoteChar        := Opt.Expr.AsString;
        if ST.HasOption('dated', Opt)  then DateSeparator    := Opt.Expr.AsString;
        if ST.HasOption('timed', Opt)  then TimeSeparator    := Opt.Expr.AsString;
        if ST.HasOption('decd', Opt)   then DecimalSeparator := Opt.Expr.AsString;
        if ST.HasOption('nl', Opt)     then NewLine          := Opt.Expr.AsString;
        if ST.HasOption('memonl', Opt) then MemoNewLine      := Opt.Expr.AsString;
        if ST.HasOption('fixed')       then FixedFormat      := True;
        if ST.HasOption('bom')         then ByteOrderMark    := True;
      end;

  if (ExportSetting.InheritsFrom(TEpiStataExportSetting)) then
    with TEpiStataExportSetting(ExportSetting) do
      begin
        if ST.HasOption('version', Opt) then
          begin
            Ver := Opt.Expr.AsInteger;
            case Ver of
              4, 5:   Version := dta4;
              6:      Version := dta6;
              7:      Version := dta7;
              8, 9:   Version := dta8;
              10, 11: Version := dta10;
              12:     Version := dta12;
              13:     Version := dta13;
              14:     Version := dta14;
            else
              if Ver < 4 then  Version := dta4;
              if Ver > 14 then Version := dta14;
            end;
          end
        else
          Version := dta14;
      end;

  if (ExportSetting.InheritsFrom(TEpiEPXExportSetting)) then
    with TEpiEPXExportSetting(ExportSetting) do
      begin
        DocumentClass := TAnaDocumentFile;
        ExportValueLabels := true;
      end;

  if (Setting <> TEpiEPXExportSetting) or
     ((Setting = TEpiEPXExportSetting) and
      (ST.HasOption('ds'))
     )
  then
    begin
      if (not Assigned(DataFile)) then
        begin
          DoError('Project contains no datasets and cannot be exported!');
          ST.ExecResult := csrFailed;
          Exit;
        end;

      DFSetting := TEpiExportDatafileSettings.Create;

      if (St.HasOption('ds', opt)) then
        DF := Document.DataFiles.GetDataFileByName(Opt.Expr.AsIdent)
      else
        DF := DataFile;

      DFSetting.DatafileName := DF.Name;
      DFSetting.ExportFileName := FN;
      DFSetting.FromRecord := 0;
      DFSetting.ToRecord   := FDataFile.Size - 1;

      for F in DF.Fields do
        with DFSetting.ExportItems do
          begin
            if IndexOf(TEpiCustomItem(F.Owner.Owner).Name) < 0 then
              Add(TEpiCustomItem(F.Owner.Owner).Name);
            Add(F.Name);
          end;

      ExportSetting.DatafileSettings.Add(DFSetting);

      Exporter := TEpiExport.Create;
      Exporter.Export(ExportSetting);
      Exporter.Free;
    end
  else
    begin
      FOldWarning := FDocFile.OnWarning;
      FDocFile.OnWarning := @SaveWarning;

      FSaveOptions := ST.Options;
      Document.IncCycleNo;

      if (not FDocFile.SaveFile(FN)) then
      begin
        DoError('Project not saved!');
        ExportSetting.Free;
        ST.ExecResult := csrFailed;
        Exit;
      end;

      FSaveOptions := nil;
      FDocFile.OnWarning := FOldWarning;
    end;
  ExportSetting.Free;

  DoInfo('Saved file: ' + ExpandFileNameUTF8(FN));
  OutputFileInformation(FDocFile.Document);
  DoUpdateProjectresultVar;

  DoGUIInteraction(gaTitle);

  AddToRecent(FN, GetRecentDataIniFileName, RecentDataFiles);
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecRun(ST: TCustomStringCommand);
var
  FN: UTF8String;
  P: TParser;
  TheProgram: TStatementList;
  OldCurrentDir: String;
begin
  FN := '';

  if Assigned(ST.StringExpr) then
    FN := ST.StringExpr.AsString;

  if (FN = '') then
    begin
      if aDM.OpenPgmFile(FN) = dfrCanceled then
        begin
          DoInfo('run canceled');
          ST.ExecResult := csrFailed;
          Exit;
        end;
    end;

  if (not FileExistsUTF8(FN)) then
  begin
    DoError('File "' + FN + '" does not exits');
    Exit;
  end;

  try
    P := TParser.Create(Self);
    P.OnCommentError := OnRunPgmCommentError;
    P.OnLexError     := OnRunPgmLexError;
    P.OnSyntaxError  := OnRunPgmSyntaxError;

    if P.ParseFile(FN, TheProgram) then
    begin
      OldCurrentDir := GetCurrentDirUTF8;
      SetCurrentDirUTF8(ExtractFilePath(FN));
      Execute(TheProgram);
      SetCurrentDirUTF8(OldCurrentDir);
    end;

    P.Free;
  except
    on E: Exception do
      begin
        DoError('Run error: ' + E.Message);
        Exit;
      end;
  end;
end;

procedure TExecutor.ExecRuntest(ST: TCustomStringCommand);
var
  FN: UTF8String;
  RT: TRunTest;

begin
  FN := '';

  if Assigned(ST.StringExpr) then
    FN := ST.StringExpr.AsString;

  if (FN = '') then
    begin
      if aDM.OpenDirectory(FN) = dfrCanceled then
        begin
          DoInfo('Runtest canceled');
          ST.ExecResult := csrFailed;
          Exit;
        end;
    end;

  RT := TRunTest.Create(Self, FOutputCreator);
  RT.RunTest(ST, FN);
  RT.Free;
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecSystemCmd(ST: TCustomStringCommand);
var
  SCmd: TSystemCmd;
begin
  SCmd := TSystemCmd.Create(Self, FOutputCreator);
  SCmd.ExecSystemCmd(ST);
  SCmd.Free;
end;

procedure TExecutor.ExecReset(ST: TCustomEmptyCommand);
begin
  ClearFieldVars;
  ClearDatasetVars;
  ClearSelectStack;
  ClearResults;
  ClearVLSetVars;
  ClearGlobals;

  EpiAsyncHandlerGlobal.RemoveDocument(Document);
  FreeAndNil(FDocFile);
  FDataFile := nil;

  DoGUIInteraction(gaClearHistory);
  DoGUIInteraction(gaClearScreen);
  DoGUIInteraction(gaTitle);

  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecClose(ST: TCustomEmptyCommand);
begin
  ClearFieldVars;
  ClearDatasetVars;
  ClearSelectStack;
  ClearResults;
  ClearVLSetVars;

  EpiAsyncHandlerGlobal.RemoveDocument(Document);
  FreeAndNil(FDocFile);
  FDataFile := nil;
  ST.ExecResult := csrSuccess;

  DoUpdateProjectresultVar;
  DoGUIInteraction(gaTitle);

  DoInfo('Project Closed!');
end;

procedure TExecutor.ExecCls(ST: TCustomEmptyCommand);
begin
{  FOutputCreator.Clear;
  DoWarning('WARNING - Testversion. Confirm results with Public release!');
  FOutputCreator.RequestRedraw;}
  DoGUIInteraction(gaClearScreen);
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecClh(ST: TCustomEmptyCommand);
begin
  DoGUIInteraction(gaClearHistory);
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecCount(ST: TCustomEmptyCommand);
var
  I: Integer;
begin
  I := SelectVector.Size;
  DoInfo('Count: ' + IntToStr(I));
  AddResultConst('$count', ftInteger).AsFloatVector[0] := I;
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecQuit(ST: TCustomEmptyCommand);
begin
  Application.MainForm.Close;
end;

function TExecutor.GetCancelled: Boolean;
begin
  result := FCancelled;
end;

function TExecutor.GetSelectVector: TEpiIntField;
begin
  result := FSelectStack.Peek;
end;

procedure TExecutor.InternalRedrawRequest(Sender: TObject);
begin
  if Assigned(FOldRedrawRequest) and
     (not FIgnoreRedrawRequest)
  then
    FOldRedrawRequest(Sender);
end;

procedure TExecutor.DoBeforeStatement(Statement: TCustomStatement);
var
  Event: TMethod;
begin
  for Event in FOnBeforeStatements do
    TExecutorStatementEvent(Event)(Statement);
end;

procedure TExecutor.DoAfterStatement(Statement: TCustomStatement);
var
  Event: TMethod;
begin
  for Event in FOnAfterStatements do
    TExecutorStatementEvent(Event)(Statement);
end;

procedure TExecutor.DoStartExecuting;
var
  Event: TMethod;
begin
  for Event in FOnStartExecuting do
    TNotifyEvent(Event)(Self);
end;

procedure TExecutor.DoEndExecuting;
var
  Event: TMethod;
begin
  for Event in FOnEndExecuting do
    TNotifyEvent(Event)(Self);
end;

procedure TExecutor.DoGUIInteraction(GUIAction: TGUIAction);
begin
  if Assigned(OnGUIInteraction) then
    OnGUIInteraction(Self, GUIAction);
end;

procedure TExecutor.AddOnAfterStatementHandler(Event: TExecutorStatementEvent);
begin
  FOnAfterStatements.Add(TMethod(Event));
end;

procedure TExecutor.AddOnBeforeStatementHandler(Event: TExecutorStatementEvent);
begin
  FOnBeforeStatements.Add(TMethod(Event));
end;

procedure TExecutor.AddOnStartExecutingHandler(Event: TNotifyEvent);
begin
  FOnStartExecuting.Add(TMethod(Event));
end;

procedure TExecutor.AddOnEndExecutingHandler(Event: TNotifyEvent);
begin
  FOnEndExecuting.Add(TMethod(Event));
end;

procedure TExecutor.RemoveOnAfterStatementHandler(Event: TExecutorStatementEvent
  );
begin
  FOnAfterStatements.Remove(TMethod(Event));
end;

procedure TExecutor.RemoveOnBeforeStatementHandler(
  Event: TExecutorStatementEvent);
begin
  FOnBeforeStatements.Remove(TMethod(Event));
end;

procedure TExecutor.RemoveOnStartExecutingHandler(Event: TNotifyEvent);
begin
  FOnStartExecuting.Remove(TMethod(Event));
end;

procedure TExecutor.RemoveOnEndExecutingHandler(Event: TNotifyEvent);
begin
  FOnEndExecuting.Remove(TMethod(Event));
end;

procedure TExecutor.ExecAssignment(ST: TAssignment);
var
  V: TCustomVariable;
  i, Changes: Integer;
  EV: TCustomExecutorDataVariable;
  SelectRecNo: Int64;
  OldErrorVal: UTF8String;
  OldCancelled: Boolean;
begin
  // All calls to ExecAssignment MUST have passed typecheck, to make sure
  // than it is not possible to assign to evtResultVector, evtResultMatrix.

  V := ST.Variable;
  EV := GetExecDataVariable(ST.Variable.Ident);

  ST.ExecResult := csrFailed;

  try
    OldErrorVal := FOptions['SHOW ERROR'].Value;
    OldCancelled := FCancelled;
    FOptions['SHOW ERROR'].Value := 'OFF';
    CheckVariableIndex(EV, V);
  except
    on E: EExecutorZeroDataException do
      begin
        FOptions['SHOW ERROR'].Value := OldErrorVal;
        FCancelled := OldCancelled;
        DoInfo('No Data!');
        ST.ExecResult := csrSuccess;
        Exit;
      end;

    on E: EExecutorException do
      begin
        FOptions['SHOW ERROR'].Value := OldErrorVal;
        raise;
      end;
  end;
  FOptions['SHOW ERROR'].Value := OldErrorVal;

  Changes := 0;

  case EV.VarType of
    evtGlobal, evtGlobalVector:
      begin
        FCurrentRecNo := 0;
        I := 0;

        if (EV.VarType = evtGlobalVector) then
          I:= TIndexVariable(V).Expr[0].AsInteger - 1;

        if ST.Expr.IsMissing then
          EV.IsMissing[I] := true
        else
          case V.ResultType of
            rtBoolean:
              EV.AsBooleanVector[I] := ST.Expr.AsBoolean;

            rtDate:
              EV.AsDateVector[I]    := ST.Expr.AsDate;

            rtInteger:
              EV.AsIntegerVector[I] := ST.Expr.AsInteger;

            rtTime:
              EV.AsTimeVector[I]    := ST.Expr.AsTime;

            rtFloat:
              EV.AsFloatVector[I]   := ST.Expr.AsFloat;

            rtString:
              EV.AsStringVector[I]  := ST.Expr.AsString;
          end;

        Changes := 1;
      end;

    evtField:
      begin
        if (V.VarType = vtIndexed) then
          begin
            FCurrentRecNo := TIndexVariable(V).Expr[0].AsInteger - 1;
            SelectRecNo := SelectVector.AsInteger[FCurrentRecNo];

            if ST.Expr.IsMissing then
              EV.IsMissing[SelectRecNo] := true
            else
              case V.ResultType of
                rtBoolean:
                  EV.AsBooleanVector[SelectRecNo] := ST.Expr.AsBoolean;

                rtDate:
                  EV.AsDateVector[SelectRecNo]    := ST.Expr.AsDate;

                rtInteger:
                  EV.AsIntegerVector[SelectRecNo] := ST.Expr.AsInteger;

                rtTime:
                  EV.AsTimeVector[SelectRecNo]    := ST.Expr.AsTime;

                rtFloat:
                  EV.AsFloatVector[SelectRecNo]   := ST.Expr.AsFloat;

                rtString:
                  EV.AsStringVector[SelectRecNo]  := ST.Expr.AsString;
              end;

            Changes := 1;
          end
        else
          begin
            FAssignmentChanges := 0;
            TExecVarField(EV).Field.RegisterOnChangeHook(@AssignmentDataChangeHook, true);
            for i := 0 to SelectVector.Size - 1 do
              begin
                FCurrentRecNo := i;
                SelectRecNo := SelectVector.AsInteger[FCurrentRecNo];

                if ST.Expr.IsMissing then
                  EV.IsMissing[SelectRecNo] := true
                else
                  case V.ResultType of
                    rtBoolean:
                      EV.AsBooleanVector[SelectRecNo] := ST.Expr.AsBoolean;

                    rtDate:
                      EV.AsDateVector[SelectRecNo]    := ST.Expr.AsDate;

                    rtInteger:
                      EV.AsIntegerVector[SelectRecNo] := ST.Expr.AsInteger;

                    rtTime:
                      EV.AsTimeVector[SelectRecNo]    := ST.Expr.AsTime;

                    rtFloat:
                      EV.AsFloatVector[SelectRecNo]   := ST.Expr.AsFloat;

                    rtString:
                      EV.AsStringVector[SelectRecNo]  := ST.Expr.AsString;
                  end;
              end;
            TExecVarField(EV).Field.UnRegisterOnChangeHook(@AssignmentDataChangeHook);
            Changes := FAssignmentChanges;
          end;
      end
  else
    // Should not be posible - must be caught in TypeChecker!
    DoError('TExecutor.ExecAssignment: EV.VarType');
    ST.ExecResult := csrFailed;
    Exit;
  end;

  if Changes > 1 then
    DoInfo(Format('(%d real changes)', [Changes]))
  else
    DoInfo(Format('(%d real change)', [Changes]));

  ST.ExecResult := csrSuccess;
  FCurrentRecNo := 0;
end;

procedure TExecutor.ExecIfThen(ST: TIfThen);
begin
  if ST.Expr.AsBoolean
  then
    DoStatement(ST.ThenStatement)
  else
    if Assigned(ST.ElseStatement)
    then
      DoStatement(ST.ElseStatement);
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecFor(ST: TFor);
var
  SVal, EVal, Idx: ASTInteger;
  I: Integer;
  AVal: TArray;
  V: TCustomExecutorDataVariable;
begin
  V := GetExecDataVariable(ST.Variable.Ident);

// Used when assigning index value to a global vector.... disabled for now.
//  if (ST.Variable.ParamCount > 0) then
//    Idx := 0 // TODO: (ST.Variable.Expr[0].AsInteger - 1)
//  else
  Idx := 0;

  Inc(FNestedLoopCounter);

  case ST.ForType of
    ftRange:
      begin
        SVal := ST.StartExpr.AsInteger;
        EVal := ST.EndExpr.AsInteger;

        case ST.Direction of
          fdTo:
            begin
              for i := SVal to EVal do
                begin
                  V.AsIntegerVector[Idx] := I;
                  DoStatement(ST.Statement);
                  if Cancelled then break;
                end;
            end;

          fdDownTo:
            begin
              for i := SVal downto EVal do
                begin
                  V.AsIntegerVector[Idx] := I;
                  DoStatement(ST.Statement);
                  if Cancelled then break;
                end;
            end;
        end;
      end;


    ftArray:
      begin
        AVal := ST.ArrayVal;

        for i := 0 to AVal.Count - 1 do
          begin
            case AVal.ResultSubType of
              rtBoolean: V.AsBooleanVector[Idx] := AVal.ExprList[i].AsBoolean;
              rtInteger: V.AsIntegerVector[Idx] := AVal.ExprList[i].AsInteger;
              rtDate:    V.AsDateVector[Idx]    := AVal.ExprList[i].AsDate;
              rtFloat:   V.AsFloatVector[Idx]   := AVal.ExprList[i].AsFloat;
              rtTime:    V.AsTimeVector[Idx]    := AVal.ExprList[i].AsTime;
              rtString:  V.AsStringVector[Idx]  := AVal.ExprList[i].AsString;
            else
              DoError('Datatype "' + ASTResultTypeString[AVal.ResultSubType] + '" not implemented in "for <variable> in <array>"..."');
              ST.ExecResult := csrFailed;
              Exit;
            end;
            DoStatement(ST.Statement);
            if Cancelled then break;
          end;
      end;
  end;

  Dec(FNestedLoopCounter);
  if (FNestedLoopCounter = 0) then
    FPostLoopNotification.CallNotifyEvents(Self);

  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecNew(ST: TCustomNew);
var
  S: UTF8String;
  V: TCustomExecutorVariable;
  Res: Boolean;
begin
  if (ST.SubCommand <> ccProject)
  then
    begin
      S := TCustomNewNamed(ST).Variable.Ident;
      V := GetExecVariable(S);

      if S[1] = '$' then
        begin
          DoError('Creating result variables is not allowed!');
          ST.ExecResult := csrFailed;
          Exit;
        end;

      if Assigned(V) or (
           Assigned(DataFile) and
           (not DataFile.ValidateRename(S))
         )
      then
        begin
          res := false;

          if (ST.SubCommand = ccGlobal) and
             (V.InheritsFrom(TExecVarGlobal) or V.InheritsFrom(TExecVarGlobalVector))
          then
            res := ((TCustomExecutorDataVariable(V).DataType = TCustomNewGlobal(ST).NewType) and
                    (
                     ((ST is TNewGlobal)       and (V is TExecVarGlobal)) or
                     ((ST is TNewGlobalVector) and (V is TExecVarGlobalVector))
                    )
                   ) or
                   ST.HasOption('replace');

          if (not res) then
            begin
              if Assigned(V) then
                DoError('Cannot create "' + S + '", name already used as a ' + ExecutorVariableTypeString[V.VarType] + '!')
              else
                DoError('Cannot create "' + S + '", name already used as section or heading!');
              ST.ExecResult := csrFailed;
              Exit;
            end;
        end;
    end;

  case ST.SubCommand of
    ccProject:
      DoNewProject(TNewProject(ST));

    ccDataset:
      DoNewDataform(TNewDataset(ST));

    ccValuelabel:
      DoNewValueLabel(TNewValuelabel(ST));

    ccVariable:
      DoNewVariable(TNewVariable(ST));

    ccGlobal:
      DoNewGlobal(TCustomNewValued(ST));
  end;
end;

procedure TExecutor.ExecSelect(ST: TSelect);
var
  F: TEpiIntField;
  Runner, i: Integer;
begin
  DoInfo('Selecting, please wait...');
  FOutputCreator.RequestRedraw;
  F := TEpiIntField(TEpiField.CreateField(nil, ftInteger));
  F.Size := SelectVector.Size;

  Runner := 0;
  for i := 0 to SelectVector.Size - 1 do
    begin
      FCurrentRecNo := i;
      if ST.Expr.AsBoolean then
        begin
          F.AsInteger[Runner] := SelectVector.AsInteger[FCurrentRecNo];
          Inc(Runner);
        end;
    end;
  FCurrentRecNo := 0;
  F.Size := Runner;

  DoInfo('Selecting complete!');
  DoInfo(Format('(%d of %d selected)', [F.Size, SelectVector.Size]), 0);

  FOutputCreator.DoNormal('');
  FOutputCreator.RequestRedraw;

  F.AddCustomData(SELECTION_EXPR, ST.Expr);
  FSelectStack.Push(F);

  ST.ExecResult := csrFailed;
  try
    DoStatement(ST.Statement);
    ST.ExecResult := csrSuccess;
  finally
    F := FSelectStack.Pop;
    F.RemoveCustomData(SELECTION_EXPR);
    F.Free;
  end;
end;

procedure TExecutor.ExecEval(ST: TEvalExpression);
begin
  ST.ExecResult := csrFailed;
  DoInfo(StringsReplace(ST.Expr.AsString, ['{','}'], ['{{','}}'], [rfReplaceAll]), 0);
  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecAssert(ST: TAssertCommand);
var
  Opt: TOption;
  Res, ShowOutput: Boolean;
  Expr: TExpr;
begin
  // Assert is NOT run through normal typechecking, because we wish
  // to catch typecheck errors too.
  Res := ST.Statement.TypeCheck(Self);
  ShowOutput := (not ST.HasOption('q'));

  if (Res) then
    if (ST.Statement.InheritsFrom(TExpr)) then
      Res := TExpr(ST.Statement).AsBoolean
    else
      begin
        DoStatement(ST.Statement);
        FCancelled := false;
        Res := (ST.Statement.ExecResult = csrSuccess)
      end;

  if ST.HasOption('fail', Opt) then
    Res := (not Res);

  if (not Res) then
    begin
      if ShowOutput then
        FOutputCreator.DoNormal('assert failed');

      if ST.HasOption('halt', Opt) then
        FCancelled := true;
    end
  else
    begin
      if ShowOutput then
        FOutputCreator.DoNormal('assert success');

      // IF execution was canceled, then restore back to "normal"
      FCancelled := false;
    end;

  ST.ExecResult := csrSuccess;
end;

procedure TExecutor.ExecSet(ST: TSetCommand);
var
  Iter: TSetOptionsMap.TIterator;
  Key, OldVal: UTF8String;
  Data: TSetOption;
  Tab: TOutputTable;
  Idx: Integer;
  L: TOutputLine;
  S: EpiString;
begin
  // Bad combination:  SET := <Expression>
  // since there is no key to assign the EXP to.
  if (Assigned(ST.AssignmentExpr)) and
     (not Assigned(ST.OptionExpr))
  then
    begin
      DoError('Missing SET option name!');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  // "Set" alone lists all Key, Value pair.
  if (not Assigned(ST.OptionExpr)) and
     (not Assigned(ST.AssignmentExpr))
  then
    begin
      Tab := FOutputCreator.AddTable;
      Tab.ColCount := 3;
      Tab.RowCount := FOptions.size + 1;

      Tab.Cell[0, 0].Text := 'Option';
      Tab.Cell[1, 0].Text := 'Datatype';
      Tab.Cell[2, 0].Text := 'Value';

      Idx := 1;
      Iter := FOptions.Min;
      repeat
        Key  := Iter.GetKey;
        Data := Iter.GetValue;

        Tab.Cell[0, Idx].Text := Key;
        Tab.Cell[1, Idx].Text := ASTResultTypeString[Data.ASTType];
        Tab.Cell[2, Idx].Text := Data.Value;
        Inc(Idx);
      until (not Iter.Next);

      Tab.SetColAlignment(0, taLeftJustify);
      Tab.SetColAlignment(1, taLeftJustify);
      Tab.SetColAlignment(2, taLeftJustify);

      ST.ExecResult := csrSuccess;
      Exit;
    end;

  S := UTF8UpperString(ST.OptionExpr.AsString);

  // Set <option name>, list only that single key,value pair.
  if (not Assigned(ST.AssignmentExpr)) and
     (Assigned(ST.OptionExpr))
  then
    begin
      if (not FOptions.TryGetValue(S, Data))
      then
        begin
          DoError('Unknown option: ' + S);
          ST.ExecResult := csrFailed;
          Exit;
        end;

      Tab := FOutputCreator.AddTable;
      Tab.ColCount := 2;
      Tab.RowCount := 1;

      Tab.Cell[0, 0].Text := S;
      Tab.Cell[1, 0].Text := Data.Value;

      ST.ExecResult := csrSuccess;
      Exit;
    end;

  // Set <option name> := <value>
  if (Assigned(ST.OptionExpr)) and
     (Assigned(ST.AssignmentExpr))
  then
    begin
      if (not FOptions.TryGetValue(S, Data))
      then
        begin
          DoError('Unknown option: ' + S);
          ST.ExecResult := csrFailed;
          Exit;
        end;

      try
        OldVal := Data.Value;
        Data.Value := ST.AssignmentExpr.AsString;
      except
        on E: ESetOption do
          begin
            DoError(E.Message);
            ST.ExecResult := csrFailed;
            Exit;
          end;
      end;

      DoInfo(S + ' = ' + Data.Value + '  (was: ' + OldVal + ' )');

      ST.ExecResult := csrSuccess;
      Exit;
    end;
end;

procedure TExecutor.ExecList(ST: TCustomCrudCommand);
var
  EL: TExecList;
begin
  EL := TExecList.Create(Self, FOutputCreator);
  EL.ExecList(ST);
  EL.Free;
end;

procedure TExecutor.ExecEdit(ST: TCustomCrudCommand);
var
  EE: TExecEdit;
  F: TEpiField;
  VLs: TEpiValueLabelSet;
  DF: TEpiDataFile;
begin
  ST.ExecResult := csrFailed;

  case ST.SubCommand of
    ccVariable:
      begin
        F := DataFile.Fields.FieldByName[TEditVariable(ST).Variable.Ident];
        if (not DoCrudVariableSanityCheck(ST, F)) then
          Exit;

        DoCrudVariableOptions(ST, F);
        ST.ExecResult := csrSuccess;

        DoUpdateFieldResultVar;
        Exit;
      end;

    ccDataset:
      begin
        DF := Document.DataFiles.GetDataFileByName(TEditDataset(ST).Variable.Ident);
        if (not DoCrudDataformSanityCheck(ST, DF)) then
          Exit;
      end;

    ccProject,
    ccData:
      ; // so far no sanity check

    ccValuelabel:
      begin
        VLs := Document.ValueLabelSets.GetValueLabelSetByName(TEditValueLabel(ST).Variable.Ident);
        if (not DoCrudValuelabelSanityCheck(ST, VLs)) then
          Exit;
      end;
  end;

  EE := TExecEdit.Create(Self, FOutputCreator);
  EE.ExecEdit(ST);
  EE.Free;

  case ST.SubCommand of
    ccDataset:
      begin
        DoUpdateDatasetResultVar;

        // If the size was changed, the select stack must be updated.
        if (ST.HasOption('size')) then
          RebuildSelectStack;
      end;

    ccValuelabel:
      DoUpdateValuelabelsResultVar;
  end;

  DoGUIInteraction(gaProjectTree);
end;

procedure TExecutor.ExecDrop(ST: TDropCommand);
var
  ED: TExecDrop;
  FCurrentDf: String;
begin
  if Assigned(FDataFile) then
    FCurrentDf := FDataFile.Name
  else
    FCurrentDf := '';

  ED := TExecDrop.Create(Self, FOutputCreator);
  ED.ExecDrop(ST);
  ED.Free;

  // Depending on what type of drop it was, do something to restore sanity:
  case ST.SubCommand of
    // Since data has been removed, rebuild the select stack to get things in order.
    ccData:
      RebuildSelectStack;

    // If currently used dataset was dropped, then reload a new one...
    ccDataset:
      if (not Document.DataFiles.ItemExistsByName(FCurrentDf))
      then
        begin
          if Document.DataFiles.Count > 0 then
            DoUseDatafile(Document.DataFiles[0])
          else
            DoUseDatafile(nil);
        end;

    ccVariable: ;
    ccProject: ;
    ccValuelabel: ;
    ccGlobal: ;
    ccResult: ;
  end;
end;

procedure TExecutor.ExecBrowse(ST: TCustomVariableCommand);
var
  L: TStrings;
  DF: TEpiDataFile;
  BrowseForm4: TBrowseForm4;
  Options: TPrepareDatasetOptions;
begin
  ST.ExecResult := csrSuccess;

  if (St.HasOption('a')) then
    begin
      CasecadeBrowsers;
      Exit;
    end;

  if (St.HasOption('c')) then
    begin
      CloseBrowsers;
      Exit;
    end;

  L := ST.VariableList.GetIdentsAsList;

  Options := [pdoIgnoreDeleteSetOption];
  if (ST.HasOption('del')) then
    Options := [pdoInvertDelete];
  DF := DoPrepareDatafile(L, nil, Options);

  BrowseForm4 := CreateBrowser(Self);
  BrowseForm4.Browse(DF, L, TBrowseCommand(ST));
  BrowseForm4.Show;
end;

procedure TExecutor.ExecMean(ST: TCustomVariableCommand);
var
  M: TIntervalDescriptives;
  L: TStrings;
  DF: TEpiDataFile;
  opt: TOption;
begin
  L := ST.VariableList.GetIdentsAsList;

  if ST.HasOption('by', opt) then
    L.Add(Opt.Expr.AsIdent);

  M := nil;
  DF := DoPrepareDatafile(L, L);

  try
    if DF.Size = 0 then
      begin
        DoError('No data!');
        ST.ExecResult := csrFailed;
        Exit;
      end;

    M := TIntervalDescriptives.Create(Self, FOutputCreator);
    M.DoMeans(DF, ST);
  finally
    DF.Free;
    M.Free;
    L.Free;
  end;
end;

procedure TExecutor.ExecFreq(ST: TCustomVariableCommand);
var
  L: TStrings;
  DF: TEpiDataFile;
  F: TFreqCommand;
begin
  L := ST.VariableList.GetIdentsAsList;
  F := nil;
  DF := nil;

  if ST.HasOption('m') then
    DF := DoPrepareDatafile(L, nil)
  else
    DF := DoPrepareDatafile(L, L);

  try
    if DF.Size = 0 then
      begin
        DoError('No data!');
        ST.ExecResult := csrFailed;
        Exit;
      end;

    F := TFreqCommand.Create(Self, FOutputCreator);
    F.ExecFreq(DF, ST);
  finally
    DF.Free;
    F.Free;
    L.Free;
  end;
end;

procedure TExecutor.ExecSort(ST: TCustomVariableCommand);
var
  lFields: TEpiFields;
begin
  lFields := EpiVarsFromVarlist(st.VariableList);

  DoInfo('Sorting, please wait....');
  FOutputCreator.RequestRedraw;

  DataFile.SortRecords(
    lFields,
    ST.HasOption('d') or ST.HasOption('descending'),
    SelectVector
  );
  DoInfo('Sorting complete!');
  DoInfo(Format('(%d observations sorted)', [SelectVector.Size]), 0);

  lFields.Free;
end;

procedure TExecutor.ExecAppend(ST: TAppendCommand);
var
  AppendModule: TMerge;
begin
  if (ST.VariableList.Count > 0) and
     (not ST.HasOption('ds'))
  then
    begin
      DoError('The use of variables without !ds option is not allowed');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  AppendModule := TMerge.Create(Self, FOutputCreator);
  AppendModule.DoAppend(ST);
  AppendModule.Free;

  // Update select stack since data was added
  RebuildSelectStack;
end;

procedure TExecutor.ExecMerge(ST: TMergeCommand);
var
  MergeModule: TMerge;
begin
  // Sanity checks:
  if (SelectVector.Size <> DataFile.Size) then
    begin
      DoError('do NOT combine MERGE with SELECT - Cannot perform merge on a reduces dataset!');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  if (ST.HasOption('combine') and
      (ST.HasOption('update') or ST.HasOption('replace'))
     ) or
     (ST.HasOption('update') and (ST.HasOption('replace')))
  then
    begin
      DoError('!combine, !update, and !replace are mutually exclusive!');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  MergeModule := TMerge.Create(Self, FOutputCreator);
  MergeModule.DoMerge(ST);
  MergeModule.Free;

  // Update select stack since data was added
  RebuildSelectStack;
  DoUpdateFieldResultVar;
  DoUpdateDatasetResultVar;
  DoUpdateValuelabelsResultVar;
end;

procedure TExecutor.ExecCheck(ST: TCustomCheckCommand);
var
  PC: TProjectChecker;
begin
  PC := TProjectChecker.Create(Self, FOutputCreator);
  PC.Check(ST);
  PC.Free;
end;

procedure TExecutor.ExecReport(ST: TCustomReportCommand);
var
  Reporter: TReports;
begin
  Reporter := TReports.Create(Self, FOutputCreator);
  Reporter.Report(ST);
  Reporter.Free;
end;

procedure TExecutor.ExecReorder(ST: TReorderCommand);
var
  AList: TStrings;
  Opt: TOption;
  PivotIdx, FIdx, i, Top, Left, Runner: Integer;
  PivotField, FirstField, F: TEpiField;
  Reverse: Boolean;
  RV: TCustomExecutorDataVariable;
  Item: TEpiCustomControlItem;
  Section: TEpiSection;
  Heading: TEpiHeading;
  TmpItems: TEpiCustomControlItemList;
  S: String;
begin
  // Before making the re-order, check that no sections exists - if they do:
  // 1: extract all controls to the main form.
  // 2: place them left aligned, top-to-bottom
  // 3: Delete all sections
  if (FDataFile.Sections.Count > 1) then
    begin
      // Create a tmp list to loop over - if working on the "live" list,
      // then any re-order using top/left may disrupt the list.
      // This will in the end make the list fail!
      TmpItems := TEpiCustomControlItemList.Create(nil);
      TmpItems.Sorted := false;

      for Item in FDataFile.ControlItems do
        if (Item = FDataFile.MainSection) then
          Continue
        else
          TmpItems.AddItem(Item);

      // Now process the list
      Top := 25;
      Left := 200;
      for Item in TmpItems do
        begin
          if (Item is TEpiSection) then
            begin
              Section := TEpiSection(Item);

              Heading := FDataFile.NewHeading;
              Heading.Name := 'SectionHeading_' + Section.Name;
              Heading.Caption.Text := Section.Caption.Text;
              Heading.Top := Top;
              Heading.Left := Left;

              Inc(Top, 30);

              Continue;
            end;

          if (Item is TEpiHeading) then
            begin
              Section := TEpiHeading(Item).Section;

              if (Section <> FDataFile.MainSection) then
                begin
                  Section.Headings.RemoveItem(Item);
                  FDataFile.MainSection.Headings.AddItem(Item);
                end;
            end;

          if (Item is TEpiField) then
            begin
              Section := TEpiField(Item).Section;

              if (Section <> FDataFile.MainSection) then
                begin
                  Section.Fields.RemoveItem(Item);
                  FDataFile.MainSection.Fields.AddItem(Item);
                end;
            end;

          Item.Top := Top;
          Item.Left := Left;

          Inc(Top, 30);
        end;

      for i := FDataFile.Sections.Count -1 downto 0 do
        if FDataFile.Section[i] <> FDataFile.MainSection then
          FDataFile.Section[i].Free;

      TmpItems.Free;
    end;
  // Done "deflating" the content of the dataset.

  // Find the spot to place the of selected varialbes - the pivot field.
  // the variables will be placed BEFORE the pivot field.
  PivotField := SortedFields[0];
  if ST.HasOption('before', Opt) then
    PivotField := SortedFields.FieldByName[Opt.Expr.AsIdent];

  if ST.HasOption('after', Opt) then
    begin
      FIdx := SortedFields.IndexOf(SortedFields.FieldByName[Opt.Expr.AsIdent]) + 1;
      if (FIdx >= SortedFields.Count) then
        PivotField := nil
      else
        PivotField := SortedFields[FIdx];
    end;

  if ST.HasOption('last') then
    PivotField := nil;

  // Now work on the tmp list again.
  TmpItems := TEpiCustomControlItemList.Create(nil);
  TmpItems.Sorted := false;
  for Item in FDataFile.ControlItems do
    if (Item = FDataFile.MainSection) then
      Continue
    else
      TmpItems.AddItem(Item);

  // At this point there are no more sections (besides the main section)
  Runner := 0;
  Top := 25;
  Left := 200;
  AList := ST.VariableList.GetIdentsAsList;

  while (Runner < TmpItems.Count) do
    begin
      Item := TEpiCustomControlItem(TmpItems[Runner]);
      Inc(Runner);

      if (Item = PivotField) then
        begin
          for S in AList do
            begin
              F := SortedFields.FieldByName[S];

              F.Top := Top;
              F.Left := Left;

              Inc(Top, 30);
            end;
        end;

      if AList.IndexOf(Item.Name) >= 0 then
        Continue;

      Item.Top := Top;
      Item.Left := Left;

      Inc(Top, 30);
    end;

  if (not Assigned(PivotField)) then
    for S in AList do
      begin
        F := SortedFields.FieldByName[S];

        F.Top := Top;
        F.Left := Left;

        Inc(Top, 30);
      end;

  TmpItems.Clear;
  TmpItems.Free;

  RV := FResults.KeyData['$variable'];
  for i := 0 to SortedFields.Count - 1 do
    RV.AsStringVector[i] := SortedFields.Field[i].Name;

  DoGUIInteraction(gaProjectTree);
end;

procedure TExecutor.ExecUse(ST: TUse);
var
  Idx: LongInt;
  F: TEpiField;
begin
  Idx := Datasets.IndexOf(ST.Variable.Ident);
  DoUseDatafile(Datasets.Data[Idx].DataFile);
  DoInfo('Using ' + DataFile.Caption.Text + ' (' + DataFile.Name + ')');

  for F in DataFile.Fields do
    begin
      if Valuelabels.Find(F.Name, Idx) then
        begin
          FOutputCreator.DoWarning(
            'Variable "' + F.Name + '" has the same name as a valuelabel. Use this command to change the name of the valuelabel: ' + LineEnding +
            'edit valuelabel ' + F.Name + ' !r := <new name>;'
          );
        end;

      if Consts.Find(F.Name, Idx) then
        begin
          FOutputCreator.DoWarning(
            'Variable "' + F.Name + '" has the same name as a global. Use this command to drop the global, then create a new:' + LineEnding +
            'drop global ' + F.Name + ';'
          );
        end;
    end;

  DoGUIInteraction(gaProjectTree);
end;

procedure TExecutor.DoStatement(St: TCustomStatement);
var
  Cmd: TCustomStatement;
begin
  DoBeforeStatement(St);

  try
    try
      if (not Assigned(FDocFile)) and
         (ST.RequireOpenProject)
      then
        begin
          Cmd := TCustomStringCommand.Create(nil, TOptionList.Create, 'read');
          Cmd.AssignToken(TToken.Create(ST.LineNo, ST.ColNo, -1));
          DoStatement(Cmd);

          if Cancelled then
            begin
              ST.ExecResult := csrFailed;
              Exit;
            end;
        end;

      if (not St.TypeCheck(Self)) then
        begin
          FCancelled := true;
          ST.ExecResult := csrFailed;
          Exit;
        end;

      case ST.StatementType of
      // Atypical statements
        stStatementList:
          DoStatementList(TStatementList(ST));

        stAssignment:
          ExecAssignment(TAssignment(ST));

        stIfThen:
          ExecIfThen(TIfThen(ST));

        stFor:
          ExecFor(TFor(ST));

        stNew:
          ExecNew(TCustomNew(ST));

        stSelect:
          ExecSelect(TSelect(ST));

        stEval:
          ExecEval(TEvalExpression(ST));

        stAssert:
          ExecAssert(TAssertCommand(ST));

        stSet:
          ExecSet(TSetCommand(ST));

      // Crud Commands

        stList:
          ExecList(TCustomCrudCommand(ST));

        stEdit:
          ExecEdit(TCustomCrudCommand(ST));

        stDrop:
          ExecDrop(TDropCommand(ST));

      // Variable Commands;

        stBrowse:
          ExecBrowse(TCustomVariableCommand(ST));

        stMeans:
          ExecMean(TCustomVariableCommand(ST));

        stUse:
          ExecUse(TUse(ST));

        stFreq:
          ExecFreq(TCustomVariableCommand(ST));

        stSort:
          ExecSort(TCustomVariableCommand(ST));

        stAppend:
          ExecAppend(TAppendCommand(ST));

        stMerge:
          ExecMerge(TMergeCommand(ST));

        stCheck:
          ExecCheck(TCustomCheckCommand(ST));

        stReport:
          ExecReport(TCustomReportCommand(ST));

        stReorder:
          ExecReorder(TReorderCommand(ST));

      // String Commands
        stRead:
          ExecRead(TCustomStringCommand(ST));

        stRun:
          ExecRun(TCustomStringCommand(ST));

        stRuntest:
          ExecRuntest(TCustomStringCommand(ST));

        stSave:
          ExecSave(TCustomStringCommand(ST));

        stCD, stLS, stTerm, stErase:
          ExecSystemCmd(TCustomStringCommand(ST));

      // Empty
        stReset:
          ExecReset(TCustomEmptyCommand(ST));

        stCls:
          ExecCls(TCustomEmptyCommand(ST));

        stClh:
          ExecClh(TCustomEmptyCommand(ST));

        stClose:
          ExecClose(TCustomEmptyCommand(ST));

        stCount:
          ExecCount(TCustomEmptyCommand(ST));

        stQuit:
          ExecQuit(TCustomEmptyCommand(ST));

        stNone:
          ST.ExecResult := csrSuccess;
      else
        DoError('EXECUTOR: Statement not implemented: ' + ST.ClassName);
      end;

    except
      ST.ExecResult := csrFailed;
      raise
    end;
  finally
    DoAfterStatement(St);
  end;
end;

procedure TExecutor.DoStatementList(L: TStatementList);
var
  i: Integer;
begin
  I := 0;

  while (I < L.Count) and
        (not Cancelled)
  do
    begin
      DoStatement(L.Statements[i]);
      Inc(i);
    end;
end;

constructor TExecutor.Create(OutputCreator: TOutputCreator);
var
  F: TEpiField;
begin
//  FSortedFields  := TEpiFields.Create(nil);
  FOutputCreator := OutputCreator;

  FOldRedrawRequest := FOutputCreator.OnRedrawRequest;
  FOutputCreator.OnRedrawRequest := @InternalRedrawRequest;

  FOnAfterStatements  := TMethodList.Create;
  FOnBeforeStatements := TMethodList.Create;
  FOnStartExecuting   := TMethodList.Create;
  FOnEndExecuting     := TMethodList.Create;

  // Loop setup
  FPostLoopNotification := TMethodList.Create;
  FNestedLoopCounter    := 0;

  FSelectStack := TSelectStack.Create;
  ClearSelectStack;

  FDataSets              := TExecutorDatasetVariables.Create;
  FDataSets.OnKeyCompare := @ExecutorDataVariablesCompare;
  FDataSets.Sorted       := false;

  FVLSets                := TExecutorValuelabelsets.Create;
  FVLSets.OnKeyCompare   := @ExecutorDataVariablesCompare;
  FVLSets.Sorted         := true;

  FFields                := TExecutorDataVariables.Create;
  FFields.OnKeyCompare   := @ExecutorDataVariablesCompare;
  FFields.Sorted         := false;

  FConsts                := TExecutorDataVariables.Create;
  FConsts.OnKeyCompare   := @ExecutorDataVariablesCompare;
  FConsts.Sorted         := true;

  FResults               := TExecutorDataVariables.Create;
  FResults.OnKeyCompare  := @ExecutorDataVariablesCompare;
  FResults.Sorted        := true;

  FResults.Add('$SYSTEMDATETIME', TExecVarSystemDateTime.Create);

  InitSetOptions;

  FExecuting := false;
  FTypeCheckErrorOutput := true;
end;

destructor TExecutor.Destroy;
begin
  FOnAfterStatements.Free;
  FOnBeforeStatements.Free;

  FSelectStack.Free;
  FDocFile.Free;
end;

procedure TExecutor.Execute(TheProgram: TStatementList);
begin
  try
    FExecuting := true;
    FCancelled := false;
    try
      DoStartExecuting;
      DoStatementList(TheProgram);
    except
      on E: EExecutorException do
        ;

      on E: Exception do
        begin
          DoError(
            'EXCEPTION ERROR: ' + E.Message + LineEnding +
            'TODO: Better message output!'
          );
          FOutputCreator.RequestRedraw;
        end;
    end;
  finally
    FExecuting := false;
    DoEndExecuting;
  end;
end;

procedure TExecutor.ExecStatement(St: TCustomStatement);
begin
  DoStatement(St);
end;

procedure TExecutor.TypeCheckError(const Msg: string; const LineNo, ColNo,
  BytePos: integer);
begin
  if FTypeCheckErrorOutput then
    DoError(Msg);
end;

function TExecutor.GetDataFile: TEpiDataFile;
begin
  result := FDataFile;
end;

function TExecutor.CheckVariableIndex(EV: TCustomExecutorVariable;
  CV: TCustomVariable; OutputError: boolean): boolean;
var
  i: Integer;
  Idx: ASTInteger;
  IV: TIndexVariable absolute CV;

  procedure DoLocalError(Size: Integer);
  begin
    if SelectVector.Size = 0 then
      begin
        if OutputError then DoError('No data!');
        raise EExecutorZeroDataException.Create('');
      end
    else
      begin
        if OutputError then
          DoError('Index out of bounds (' + CV.Ident + '): ' + IntToStr(Idx) + LineEnding +
                  'Size = ' + IntToStr(Size));
        raise EExecutorIndexException.Create('');
      end;
  end;

begin
  result := false;

  case EV.VarType of
    evtGlobal,
    evtResultConst:
      result := true;

    evtGlobalVector:
      begin
        Idx := IV.Expr[0].AsInteger;
        if (Idx < 1) or (Idx > TExecVarGlobalVector(EV).Length) then
          DoLocalError(TExecVarVector(EV).Length);
      end;

    evtResultVector:
      begin
        Idx := IV.Expr[0].AsInteger;
        if (Idx < 1) or (Idx > TExecVarVector(EV).Length) then
          DoLocalError(TExecVarVector(EV).Length);
      end;

    evtField:
      begin
        if CV.VarType = vtIndexed then
          Idx := IV.Expr[0].AsInteger
        else
          Idx := FCurrentRecNo + 1;  // +1 becase FCurrentRecNo is 0-indext and user provided Idx is 1-indexed.;

        if (Idx < 1) or (Idx > SelectVector.Size) then
          DoLocalError(SelectVector.Size);
      end;

    evtResultMatrix:
      begin
        Idx := IV.Expr[0].AsInteger;
        if (Idx < 1) or (Idx > TExecVarMatrix(EV).Cols) then
          DoLocalError(TExecVarMatrix(EV).Cols);

        Idx := IV.Expr[1].AsInteger;
        if (Idx < 1) or (Idx > TExecVarMatrix(EV).Rows) then
          DoLocalError(TExecVarMatrix(EV).Rows);
      end;
  end;

  result := true;
end;

function TExecutor.GetVariableExecType(const Ident: UTF8String
  ): TExecutorVariableType;
var
  L: TStrings;
begin
  result := GetExecVariable(Ident).VarType;
end;

function TExecutor.GetVariableValueBool(const Sender: TCustomVariable): Boolean;
var
  V: TCustomExecutorDataVariable;
  IV: TIndexVariable absolute Sender;
begin
  V := GetExecDataVariable(Sender.Ident);

  if (not CheckVariableIndex(V, Sender)) then
    Exit;

  case V.VarType of
    evtGlobal,
    evtResultConst:
      Result := Boolean( V.AsBooleanVector[0] );

    evtGlobalVector,
    evtResultVector:
      begin
        result := Boolean( V.AsBooleanVector[IV.Expr[0].AsInteger - 1] );
      end;

    evtField:
      begin
        if Sender.VarType = vtVariable then
          result := Boolean( V.AsBooleanVector[SelectVector.AsInteger[FCurrentRecNo]] )
        else
          result := Boolean( V.AsBooleanVector[SelectVector.AsInteger[IV.Expr[0].AsInteger - 1]] );
      end;


    evtResultMatrix:
      begin
        result := Boolean( V.AsBooleanMatrix[IV.Expr[0].AsInteger - 1,
                                             IV.Expr[1].AsInteger - 1] );
      end;
  end;
end;

function TExecutor.GetVariableValueInt(const Sender: TCustomVariable
  ): EpiInteger;
var
  V: TCustomExecutorDataVariable;
  IV: TIndexVariable absolute Sender;
begin
  V := GetExecDataVariable(Sender.Ident);

  if (not CheckVariableIndex(V, Sender)) then
    Exit;

  case V.VarType of
    evtGlobal,
    evtResultConst:
      Result := V.AsIntegerVector[0];

    evtGlobalVector,
    evtResultVector:
      begin
        result := V.AsIntegerVector[IV.Expr[0].AsInteger - 1];
      end;

    evtField:
      begin
        if Sender.VarType = vtVariable then
          result := V.AsIntegerVector[SelectVector.AsInteger[FCurrentRecNo]]
        else
          result := V.AsIntegerVector[SelectVector.AsInteger[IV.Expr[0].AsInteger - 1]];
      end;


    evtResultMatrix:
      begin
        result := V.AsIntegerMatrix[IV.Expr[0].AsInteger - 1,
                                    IV.Expr[1].AsInteger - 1];
      end;
  end;
end;

function TExecutor.GetVariableValueFloat(const Sender: TCustomVariable
  ): EpiFloat;
var
  V: TCustomExecutorDataVariable;
  IV: TIndexVariable absolute Sender;
begin
  V := GetExecDataVariable(Sender.Ident);

  if (not CheckVariableIndex(V, Sender)) then
    Exit;

  case V.VarType of
    evtGlobal,
    evtResultConst:
      Result := V.AsFloatVector[0];

    evtGlobalVector,
    evtResultVector:
      begin
        result := V.AsFloatVector[IV.Expr[0].AsInteger - 1];
      end;

    evtField:
      begin
        if Sender.VarType = vtVariable then
          result := V.AsFloatVector[SelectVector.AsInteger[FCurrentRecNo]]
        else
          result := V.AsFloatVector[SelectVector.AsInteger[IV.Expr[0].AsInteger - 1]];
      end;


    evtResultMatrix:
      begin
        result := V.AsFloatMatrix[IV.Expr[0].AsInteger - 1,
                                    IV.Expr[1].AsInteger - 1];
      end;
  end;
end;

function TExecutor.GetVariableValueDate(const Sender: TCustomVariable): EpiDate;
var
  V: TCustomExecutorDataVariable;
  IV: TIndexVariable absolute Sender;
begin
  V := GetExecDataVariable(Sender.Ident);

  if (not CheckVariableIndex(V, Sender)) then
    Exit;

  case V.VarType of
    evtGlobal,
    evtResultConst:
      Result := V.AsDateVector[0];

    evtGlobalVector,
    evtResultVector:
      begin
        result := V.AsDateVector[IV.Expr[0].AsInteger - 1];
      end;

    evtField:
      begin
        if Sender.VarType = vtVariable then
          result := V.AsDateVector[SelectVector.AsInteger[FCurrentRecNo]]
        else
          result := V.AsDateVector[SelectVector.AsInteger[IV.Expr[0].AsInteger - 1]];
      end;


    evtResultMatrix:
      begin
        result := V.AsDateMatrix[IV.Expr[0].AsInteger - 1,
                                 IV.Expr[1].AsInteger - 1];
      end;
  end;
end;

function TExecutor.GetVariableValueTime(const Sender: TCustomVariable
  ): EpiDateTime;
var
  V: TCustomExecutorDataVariable;
  IV: TIndexVariable absolute Sender;
begin
  V := GetExecDataVariable(Sender.Ident);

  if (not CheckVariableIndex(V, Sender)) then
    Exit;

  case V.VarType of
    evtGlobal,
    evtResultConst:
      Result := V.AsTimeVector[0];

    evtGlobalVector,
    evtResultVector:
      begin
        result := V.AsTimeVector[IV.Expr[0].AsInteger - 1];
      end;

    evtField:
      begin
        if Sender.VarType = vtVariable then
          result := V.AsTimeVector[SelectVector.AsInteger[FCurrentRecNo]]
        else
          result := V.AsTimeVector[SelectVector.AsInteger[IV.Expr[0].AsInteger - 1]];
      end;


    evtResultMatrix:
      begin
        result := V.AsTimeMatrix[IV.Expr[0].AsInteger - 1,
                                 IV.Expr[1].AsInteger - 1];
      end;
  end;
end;

function TExecutor.GetVariableValueString(const Sender: TCustomVariable
  ): EpiString;
var
  V: TCustomExecutorDataVariable;
  IV: TIndexVariable absolute Sender;
begin
  V := GetExecDataVariable(Sender.Ident);

  if (not CheckVariableIndex(V, Sender)) then
    Exit;

  case V.VarType of
    evtGlobal,
    evtResultConst:
      Result := V.AsStringVector[0];

    evtGlobalVector,
    evtResultVector:
      begin
        result := V.AsStringVector[IV.Expr[0].AsInteger - 1];
      end;

    evtField:
      begin
        if Sender.VarType = vtVariable then
          result := V.AsStringVector[SelectVector.AsInteger[FCurrentRecNo]]
        else
          result := V.AsStringVector[SelectVector.AsInteger[IV.Expr[0].AsInteger - 1]];
      end;


    evtResultMatrix:
      begin
        result := V.AsStringMatrix[IV.Expr[0].AsInteger - 1,
                                   IV.Expr[1].AsInteger - 1];
      end;
  end;
end;

function TExecutor.GetVariableValueMissing(const Sender: TCustomVariable
  ): boolean;
begin
  try
    CheckVariableIndex(GetExecVariable(Sender.Ident), Sender, False)
  except
    Result := true;
    Exit;
  end;

  Case GetVariableType(Sender) of
    ftBoolean:
      result :=  false;
    ftInteger,
    ftAutoInc:
      result :=  TEpiIntField.CheckMissing(GetVariableValueInt(Sender));
    ftFloat:
      result :=  TEpiFloatField.CheckMissing(GetVariableValueFloat(Sender));
    ftDMYDate,
    ftMDYDate,
    ftYMDDate,
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto:
      result :=  TEpiIntField.CheckMissing(GetVariableValueDate(Sender));
    ftTime,
    ftTimeAuto:
      result :=  TEpiFloatField.CheckMissing(GetVariableValueTime(Sender));
    ftMemo,
    ftString,
    ftUpperString:
      result :=  TEpiStringField.CheckMissing(GetVariableValueString(Sender));
  end;
end;

function TExecutor.GetVariableValueUserMissing(const Sender: TCustomVariable
  ): boolean;
var
  V: TCustomExecutorVariable;
  VF: TExecVarField;
  IV: TIndexVariable absolute Sender;
begin
  Result := false;

  try
    V := GetExecVariable(Sender.Ident);
    CheckVariableIndex(V, Sender, False)
  except
    Exit;
  end;

  case V.VarType of
    evtField:
      begin
        VF := TExecVarField(V);
        if Sender.VarType = vtVariable then
          result := VF.Field.IsMissingValue[SelectVector.AsInteger[FCurrentRecNo]]
        else
          result := VF.Field.IsMissingValue[SelectVector.AsInteger[IV.Expr[0].AsInteger - 1]];
      end;

  else
{    evtGlobal: ;
    evtGlobalVector: ;
    evtDataset: ;
    evtValuelabel: ;
    evtResultConst: ;
    evtResultVector: ;
    evtResultMatrix: ;    }
    result := false;
  end;
end;

function TExecutor.GetVariableType(const Sender: TCustomVariable
  ): TEpiFieldType;
begin
  Result := GetExecDataVariable(Sender.Ident).DataType;
end;

function TExecutor.GetCurrentRecordNo: Integer;
begin
  result := FCurrentRecNo;
end;

function TExecutor.CreateFunction(const FunctionName: string;
  const ParamList: TParamList): TFunctionCall;
begin
  result := nil;
end;

function TExecutor.TypeCheckVariable(const Sender: TCustomVariable;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
var
  VIndexed: TIndexVariable absolute Sender;
  V: TCustomExecutorVariable;
  S: UTF8String;
  E: TExecutorVariableType;
begin
  result := false;

  V := GetExecVariable(Sender.Ident);
  if (not Assigned(V)) then
    begin
      if (not (evfExternal in TypesAndFlags.Flags)) then
        begin
          DoError('Identifier "' + Sender.Ident + '" not found!');
          Exit;
        end
      else
        begin
          Result := true;
          Exit;
        end;
    end;

  if not (V.VarType in TypesAndFlags.ExecutorVariableTypes) then
    begin
      S := '';
      for E in TypesAndFlags.ExecutorVariableTypes do
        S := S + ExecutorVariableTypeString[E] + ', ';
      Delete(S, Length(S)-1, 2);

      DoError('Identifier "' + Sender.Ident + '" is not the correct category!' + LineEnding +
              'Expected: ' + S + LineEnding +
              'Got: ' + ExecutorVariableTypeString[V.VarType]);
      Exit;
    end;

  case V.VarType of
    evtGlobal,
    evtResultConst,
    evtValuelabel,
    evtDataset:
      begin
        if (Sender.VarType = vtIndexed) then
          begin
            DoError('Identifier "' + Sender.Ident + '" does not accept an index');
            Exit;
          end;
      end;

    evtField:
      begin
        if (Sender.VarType = vtIndexed) and
           (VIndexed.ParamCount <> 1)
        then
          begin
            DoError('Identifier "' + Sender.Ident + '" only accepts one index');
            Exit;
          end;

        if (evfAsValue in TypesAndFlags.Flags) and
           (VIndexed.ParamCount = 0)
        then
          begin
            DoError('Identifier "' + Sender.Ident + '" must have one index');
            Exit;
          end;
      end;

    evtGlobalVector,
    evtResultVector:
      begin
        if (
            (Sender.VarType = vtVariable) or
            (
             (Sender.VarType = vtIndexed) and
             (VIndexed.ParamCount <> 1)
            )
           ) and
           ((not (evfAsObject in TypesAndFlags.Flags)) or
            (evfAsValue in TypesAndFlags.Flags)
           )
        then
          begin
            DoError('Identifier "' + Sender.Ident + '" must have one index');
            Exit;
          end;
      end;

    evtResultMatrix:
      begin
        if (
            (Sender.VarType = vtVariable) or
            (
             (Sender.VarType = vtIndexed) and
             (VIndexed.ParamCount <> 2)
            )
           ) and
           ((not (evfAsObject in TypesAndFlags.Flags)) or
            (evfAsValue in TypesAndFlags.Flags)
           )
        then
          begin
            DoError('Identifier "' + Sender.Ident + '" must have two indeces');
            Exit;
          end;
      end;
  end;

  result := true;
end;

function TExecutor.ExpandVariableList(const Sender: TCustomVariable;
  VariableChecker: IVariableCheck; Index: Integer; out
  AVariableList: TVariableList): boolean;
var
  VR: TVariableRange absolute Sender;
  StartIdx, EndIdx, TmpIdx, i: Integer;
  S: UTF8String;
  F: TEpiField;
  R: TRegExpr;
  EVType, EVType2: TExecutorVariableType;
  EV: TCustomExecutorVariable;
  EVTypes: TExecutorVariableTypes;

  function evtCompare(TypeA, TypeB: TExecutorVariableType): boolean;
  begin
    // Global = GlobalVector
    result :=
      (([evtGlobal, evtGlobalVector] * [TypeA]) <> []) and
      (([evtGlobal, evtGlobalVector] * [TypeB]) <> []);

    // ResultConst = ResultVector = ResultMatrix
    result := Result or (
      (([evtResultConst, evtResultVector, evtResultMatrix] * [TypeA]) <> []) and
      (([evtResultConst, evtResultVector, evtResultMatrix] * [TypeB]) <> [])
    );

    Result := Result or (TypeA = TypeB);
  end;

begin
  Result := false;
  AVariableList := TVariableList.Create;

  if (Sender is TVariableRange) then
    begin
      EV := GetExecVariable(VR.StartVariable.Ident);
      if (not Assigned(EV)) then
        begin
          Error(
            '"' + VR.StartVariable.Ident + '" does not exit!'
          );
          Exit;
        end;

      if (not Assigned(GetExecVariable(VR.EndVariable.Ident))) then
        begin
          Error(
            '"' + VR.EndVariable.Ident + '" does not exit!'
          );
          Exit;
        end;

      EVType := EV.VarType;
      EVType2 := GetVariableExecType(VR.EndVariable.Ident);
      if (not evtCompare(EVType, EVType2)) then
        begin
          Error(
            '"' + VR.StartVariable.Ident + '" and "' + VR.EndVariable.Ident + '" are not the same catgory.' + LineEnding +
            '"' + VR.StartVariable.Ident + '" is a ' + ExecutorVariableTypeString[EVType] + LineEnding +
            '"' + VR.EndVariable.Ident   + '" is a ' + ExecutorVariableTypeString[EVType2] + LineEnding +
            'Variable expansion is not possible!'
          );
          Exit;
        end;

      case EVType of
        evtGlobal,
        evtGlobalVector:
          begin
            StartIdx := FConsts.IndexOf(VR.StartVariable.Ident);
            EndIdx   := FConsts.IndexOf(VR.EndVariable.Ident);
          end;

        evtField:
          begin
            StartIdx := SortedFields.IndexOf(SortedFields.FieldByName[VR.StartVariable.Ident]);
            EndIdx   := SortedFields.IndexOf(SortedFields.FieldByName[VR.EndVariable.Ident]);
          end;

        evtDataset:
          begin
            StartIdx := FDataSets.IndexOf(VR.StartVariable.Ident);
            EndIdx   := FDataSets.IndexOf(VR.EndVariable.Ident);
          end;

        evtValuelabel:
          begin
            StartIdx := FVLSets.IndexOf(VR.StartVariable.Ident);
            EndIdx   := FVLSets.IndexOf(VR.EndVariable.Ident);
          end;

        evtResultConst,
        evtResultVector,
        evtResultMatrix:
          begin
            StartIdx := FResults.IndexOf(VR.StartVariable.Ident);
            EndIdx   := FResults.IndexOf(VR.EndVariable.Ident);
          end

      else
        begin
          Error(
            'Expansion of variables with type: ' + ExecutorVariableTypeString[EVType] + ' is currently not possible!'
          );
          Exit;
        end;
      end;

      if (StartIdx > EndIdx) then
        begin
          TmpIdx   := StartIdx;
          StartIdx := EndIdx;
          EndIdx   := TmpIdx;
        end;

      for i := StartIdx to EndIdx do
        case EVType of
          evtGlobal,
          evtGlobalVector:
            AVariableList.Add(TVariable.Create(FConsts.Keys[i], Self));

          evtField:
            AVariableList.Add(TVariable.Create(SortedFields[i].Name, Self));

          evtDataset:
            AVariableList.Add(TVariable.Create(FDataSets.Keys[i], Self));

          evtValuelabel:
            AVariableList.Add(TVariable.Create(FVLSets.Keys[i], Self));

          evtResultConst,
          evtResultVector,
          evtResultMatrix:
            AVariableList.Add(TVariable.Create(FResults.Keys[i], Self));
        end;
    end;

  S := Sender.Ident;
  if (UTF8Pos('?', S) > 0) or
     (UTF8Pos('*', S) > 0)
  then
    begin
      EVTypes := VariableChecker.GetAcceptedVariableTypesAndFlags(Index).ExecutorVariableTypes;

      // Help out user a little by adding a '$' in cast they missed it
      if (([evtResultConst, evtResultVector, evtResultMatrix] * EVTypes) <> []) and
         (S[1] <> '$')
      then
        S := '$' + S;

      S := StringReplace(S, '$', '\$', [rfReplaceAll]);
      S := StringReplace(S, '?', '.',  [rfReplaceAll]);
      S := StringReplace(S, '*', '.*', [rfReplaceAll]);

      R := TRegExpr.Create(S);
      R.ModifierI := true;

      if (evtField in EVTypes) then
        for F in SortedFields do
          if (R.Exec(F.Name)) and
             (R.Match[0] = F.Name)
          then
            AVariableList.Add(TVariable.Create(F.Name, Self));

      if (evtDataset in EVTypes) then
        for i := 0 to FDataSets.Count - 1 do
          if (R.Exec(FDataSets.Keys[i])) and
             (R.Match[0] = FDataSets.Keys[i])
          then
            AVariableList.Add(TVariable.Create(FDataSets.Keys[i], Self));

      if (evtValuelabel in EVTypes) then
        for i := 0 to FVLSets.Count - 1 do
          if (R.Exec(FVLSets.Keys[i])) and
             (R.Match[0] = FVLSets.Keys[i])
          then
            AVariableList.Add(TVariable.Create(FVLSets.Keys[i], Self));

      if (([evtGlobal, evtGlobalVector] * EVTypes) <> []) then
        for i := 0 to FConsts.Count - 1 do
          if (R.Exec(FConsts.Keys[i])) and
             (R.Match[0] = FConsts.Keys[i])
          then
            AVariableList.Add(TVariable.Create(FConsts.Keys[i], Self));

      if (([evtResultConst, evtResultVector, evtResultMatrix] * EVTypes) <> []) then
        for i := 0 to FResults.Count - 1 do
          if (R.Exec(FResults.Keys[i])) and
             (R.Match[0] = FResults.Keys[i])
          then
            AVariableList.Add(TVariable.Create(FResults.Keys[i], Self));

      if (AVariableList.Count = 0) then
        begin
          DoError('No identifiers match "' + Sender.Ident + '"');
          Result := false;
          Exit;
        end;
    end;

  result := true;
end;

procedure TExecutor.SetTypeCheckErrorOutput(Active: boolean);
begin
  FTypeCheckErrorOutput := Active;
end;

end.

