unit runtest;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, GOLDParser, FileUtil, Token, ast, epidatafiles,
  epidatafilestypes, epidocument, outputcreator, parser;

type

  { TRunTest }

  TRunTest = class
  private
    FHaltedLine: Integer;
    FHalted:  boolean;
    FCommand: TCustomStringCommand;
    FDatafile: TEpiDataFile;
    FDatafileIdx: Integer;

    // Fields holding result data
    FFilename: TEpiField;
    FPlanned: TEpiField;
    FFound: TEpiField;
    FError: TEpiField;
    FPrecision: TEpiField;
    FAbsDiff: TEpiField;
    FComment: TEpiField;
    procedure LexError(Sender: TObject; ErrorToken: TToken);
    procedure ParseResponse(Sender: TObject;
      const Response: TGoldParserMessage; var StopParsing, handled: boolean);
  private
    FInternalOutputCreator: TOutputCreator;
    FExternalOutputCreator: TOutputCreator;
    FInternalExecutor: TExecutor;
    FExternalExecutor: TExecutor;
    procedure InternalRunTest(Const RunDir: UTF8String);
    procedure RunFile(Const FN: UTF8String);
    procedure DoAssert(ST: TAssertCommand);
    function  DoLRE(ParamList: TParamList): EpiFloat;
  public
    constructor Create(Executor: TExecutor; Output: TOutputCreator);
    destructor Destroy; override;
    procedure RunTest(ST: TCustomStringCommand; Const StartDir: UTF8String);
    procedure SyntaxError(Sender: TObject; ErrorToken: TToken; TokenTable: TTokenStack);
    property  Halted: boolean read FHalted;
  end;

implementation

uses
  ast_types, datamodule, LazFileUtils, LazUTF8Classes, strutils,
  epi_script_function_mathfunctions, LazUTF8,  math;

type

  { TRuntestExecutor }

  TRuntestExecutor = class(TExecutor)
  private
    FRunTest: TRunTest;
  protected
    function  GetCancelled: Boolean; override;
    procedure DoError(const Msg: UTF8String); override;
    procedure DoStatement(St: TCustomStatement); override;
    procedure ExecAssert(ST: TAssertCommand); override;
  public
    function CreateFunction(const FunctionName: string; const ParamList: TParamList): TFunctionCall; override;
  public
    constructor Create(OutputCreator: TOutputCreator; ARunTest: TRunTest);
  end;


  { TRunTestMathFunction }

  TRunTestMathFunction = class(TEpiScriptFunction_MathFunctions)
  private
    FRunTest: TRunTest;
  public
    constructor Create(ARunTest: TRunTest; const ParamList: TParamList);
    function AsFloat: EpiFloat; override;
  end;

{ TRunTestMathFunction }

constructor TRunTestMathFunction.Create(ARunTest: TRunTest;
  const ParamList: TParamList);
begin
  inherited Create(otFuncLRE, ParamList);
  FRunTest := ARunTest;
end;

function TRunTestMathFunction.AsFloat: EpiFloat;
begin
  Result := FRunTest.DoLRE(FParamList);
end;

{ TRuntestExecutor }

function TRuntestExecutor.GetCancelled: Boolean;
begin
  Result := (inherited GetCancelled) or
            (FRunTest.FExternalExecutor.Cancelled);
end;

procedure TRuntestExecutor.DoError(const Msg: UTF8String);
begin
  inherited DoError(Msg);
  Cancelled := false;
end;

procedure TRuntestExecutor.DoStatement(St: TCustomStatement);
begin
  try
    inherited DoStatement(St);
  except
    FCancelled := true;
  end;
end;

procedure TRuntestExecutor.ExecAssert(ST: TAssertCommand);
begin
  FRunTest.DoAssert(ST);
end;

function TRuntestExecutor.CreateFunction(const FunctionName: string;
  const ParamList: TParamList): TFunctionCall;
begin
  Result := inherited CreateFunction(FunctionName, ParamList);

  if Assigned(result) then exit;

  if UTF8LowerString(FunctionName) = 'lre' then
    result := TRunTestMathFunction.Create(FRunTest, ParamList);
end;

constructor TRuntestExecutor.Create(OutputCreator: TOutputCreator;
  ARunTest: TRunTest);
begin
  inherited Create(OutputCreator);
  FRunTest := ARunTest;
end;

{ TRunTest }

procedure TRunTest.RunFile(const FN: UTF8String);
var
  P: TParser;
  TheProgram: TStatementList;
  ParseOk: Boolean;
begin
  FExternalOutputCreator.DoNormal('Running: ' + FN);
  FExternalOutputCreator.RequestRedraw;

  FDatafileIdx := FDatafile.NewRecords();
  FFilename.AsString[FDatafileIdx] := Fn;
  FPlanned.AsInteger[FDatafileIdx] := 0;
  FFound.AsInteger[FDatafileIdx]   := 0;
  FError.AsInteger[FDatafileIdx]   := 0;
//  FPrecision.AsFloat[FDatafileIdx] := 100;

  P := TParser.Create(FInternalExecutor);
  P.OnSyntaxError   := @SyntaxError;
  P.OnCommentError  := @LexError;
  P.OnLexError      := @LexError;
  P.OnParseResponse := @ParseResponse;

  ParseOk := P.ParseFile(FN, TheProgram);
  P.Free;

  if ParseOk then
    FInternalExecutor.Execute(TheProgram);
end;

procedure TRunTest.DoAssert(ST: TAssertCommand);
var
  Opt: TOption;
  Res: Boolean;
begin
  Res := ST.Statement.TypeCheck(FInternalExecutor);

  if (Res) then
    if (ST.Statement.InheritsFrom(TExpr)) then
      Res := TExpr(ST.Statement).AsBoolean
    else
      begin
        FInternalExecutor.ExecStatement(ST.Statement);
        FInternalExecutor.Cancelled := false;
        Res := (ST.Statement.ExecResult = csrSuccess)
      end;

  if ST.HasOption('fail', Opt) then
    Res := (not Res);

  FFound.AsInteger[FDatafileIdx] := FFound.AsInteger[FDatafileIdx] + 1;
  if (not Res) then
    begin
      FError.AsInteger[FDatafileIdx] := FError.AsInteger[FDatafileIdx] + 1;

      if FCommand.HasOption('halt') then
        begin
          FInternalExecutor.Cancelled := true;
          FHaltedLine := ST.LineNo;
          FHalted := true;
        end;
    end;

  ST.ExecResult := csrSuccess;
end;

function TRunTest.DoLRE(ParamList: TParamList): EpiFloat;
var
  c, t: EpiFloat;
begin
  // See TEpiScriptFunction_MathFunctions for the calculations of LRE and why it's used the way we do.
  c := ParamList[0].AsFloat;
  t := ParamList[1].AsFloat;

  if (t <> 0) then
    Result := min(15, -log10(abs((c - t) / t)))
  else
    Result := min(15, -log10(abs(c - t)));

  FPrecision.AsFloat[FDatafileIdx] := min(FPrecision.AsFloat[FDatafileIdx], Result);
  FAbsDiff.AsFloat[FDatafileIdx]   := min(FAbsDiff.AsFloat[FDatafileIdx], abs(c-t));
end;

procedure TRunTest.LexError(Sender: TObject; ErrorToken: TToken);
begin
  SyntaxError(Sender, ErrorToken, nil);
end;

procedure TRunTest.ParseResponse(Sender: TObject;
  const Response: TGoldParserMessage; var StopParsing, handled: boolean);
var
  CToken: TToken;
  S: String;
  I: Longint;
begin
  if (Response <> gpMsgCommentLine) then
    exit;

  CToken := TParser(Sender).GoldParser.CurrentToken;
  if (CToken.LineNum = 1) then
    begin
      S := UTF8Trim(CToken.DataVar);

      if (S[1] = '#') then
        begin
          Copy2SymbDel(S, '#');
          if TryStrToInt(Copy2SymbDel(S, '#'), I) then
            FPlanned.AsInteger[FDatafileIdx] := I;

          Delete(S, 1, 1);
          FComment.AsString[FDatafileIdx] := UTF8Trim(S);
        end;

      StopParsing := false;
      handled := true;
    end;
end;

procedure TRunTest.InternalRunTest(const RunDir: UTF8String);
var
  lCurrentDir: String;
  Dirs, Files: TStringListUTF8;
  DS: TListDirectoriesSearcher;
  i, j: Integer;
  FS: TListFileSearcher;
begin
  lCurrentDir := GetCurrentDirUTF8;
  SetCurrentDirUTF8(RunDir);

  Dirs := TStringListUTF8.Create;
  Dirs.Add('.');

  DS := TListDirectoriesSearcher.Create(Dirs);
  DS.Search('.');
  DS.Free;
  Dirs.Sort;

  try
    Files := TStringListUTF8.Create;
    FS := TListFileSearcher.Create(Files);
    for i := 0 to Dirs.Count - 1 do
      begin
        Files.Clear;
        FS.Search(Dirs[i], '*.pgm', false, true);
        Files.Sort;

        for j := 0 to Files.Count -1 do
          begin
            RunFile(Files[j]);

            if (FExternalExecutor.Cancelled) or
               (Halted)
            then
              Exit;
          end;
      end;

  finally
    FS.Free;
    SetCurrentDirUTF8(lCurrentDir);
  end;
end;

constructor TRunTest.Create(Executor: TExecutor; Output: TOutputCreator);
begin
  FInternalOutputCreator := TOutputCreator.Create;
  FInternalExecutor      := TRuntestExecutor.Create(FInternalOutputCreator, Self);

  FExternalOutputCreator := Output;
  FExternalExecutor      := Executor;

  FDatafile := TEpiDataFile.Create(nil);
  FHalted   := false;

  FFilename  := FDatafile.NewField(ftString);
  FPlanned   := FDatafile.NewField(ftInteger);
  FFound     := FDatafile.NewField(ftInteger);
  FError     := FDatafile.NewField(ftInteger);
  FPrecision := FDatafile.NewField(ftFloat);
  FAbsDiff   := FDatafile.NewField(ftFloat);
  FComment   := FDatafile.NewField(ftString);
end;

destructor TRunTest.Destroy;
begin
  FInternalExecutor.Free;
  FInternalOutputCreator.Free;
  inherited Destroy;
end;

procedure TRunTest.RunTest(ST: TCustomStringCommand; const StartDir: UTF8String);
var
  i, SumPlanned, SumFound, SumError: Integer;
  T: TOutputTable;
  OldProgress: TEpiProgressEvent;
  OldOutputC: TOutputCreator;
begin
  FCommand := ST;

  OldProgress := aDM.OnProgress;
  OldOutputC  := aDM.OutputCreator;
  aDM.OnProgress := nil;
  aDM.OutputCreator := FInternalOutputCreator;

  InternalRunTest(StartDir);

  aDM.OnProgress := OldProgress;
  aDM.OutputCreator := OldOutputC;


  FExternalOutputCreator.DoNormal('');

  T := FExternalOutputCreator.AddTable;
  T.ColCount := 7;
  T.RowCount := FDatafile.Size + 2;

  T.Cell[0, 0].Text := '{\b Filename}';
  T.Cell[1, 0].Text := '{\b Planned}';
  T.Cell[2, 0].Text := '{\b Found}';
  T.Cell[3, 0].Text := '{\b Error}';
  T.Cell[4, 0].Text := '{\b Precision}';
  T.Cell[5, 0].Text := '{\b Abs. Diff}';
  T.Cell[6, 0].Text := '{\b Comment}';
  T.SetRowAlignment(0, taLeftJustify);

  SumPlanned := 0;
  SumFound   := 0;
  SumError   := 0;
  for i := 0 to FDatafile.Size - 1 do
    begin
      T.Cell[0, i + 1].Text := FFilename.AsString[i];
      T.Cell[1, i + 1].Text := FPlanned.AsString[i];
      SumPlanned += FPlanned.AsInteger[i];
      T.Cell[2, i + 1].Text := FFound.AsString[i];
      SumFound += FFound.AsInteger[i];
      T.Cell[3, i + 1].Text := FError.AsString[i];
      SumError += FError.AsInteger[i];

      if (not FPrecision.IsMissing[i]) then
        T.Cell[4, i + 1].Text := IntToStr(Trunc(FPrecision.AsFloat[i]));

      if (not FAbsDiff.IsMissing[i]) then
        T.Cell[5, i + 1].Text := FAbsDiff.AsString[i];

      T.Cell[6, i + 1].Text := FComment.AsString[i];
    end;

  T.Cell[0, FDatafile.Size + 1].Text := 'Total:';
  T.Cell[1, FDatafile.Size + 1].Text := IntToStr(SumPlanned);
  T.Cell[2, FDatafile.Size + 1].Text := IntToStr(SumFound);
  T.Cell[3, FDatafile.Size + 1].Text := IntToStr(SumError);
  if (SumError > 0) then
    T.Cell[6, FDatafile.Size + 1].Text := '{\b Assert Errors Exits!}';

  T.SetColAlignment(0, taLeftJustify);
  T.SetColAlignment(6, taLeftJustify);

  if Halted then
    T.Footer.Text := '{\b Runtest was halted during execution: LineNo = ' + IntToStr(FHaltedLine) + '}';
end;

procedure TRunTest.SyntaxError(Sender: TObject; ErrorToken: TToken;
  TokenTable: TTokenStack);
begin
  FComment.AsString[FDatafileIdx] :=
    Format('{\b Syntax Error in PGM (line: %d, pos: %d)}',
           [ErrorToken.LineNum, ErrorToken.CaretNum]);
end;


end.

