program validator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  epidatacore, interfaces, LazFileUtils,
  outputcreator, executor, ast, outputgenerator_txt,
  parser, datamodule, outputgenerator_console;

type

  { TAnalysisValidator }

  TAnalysisValidator = class(TCustomApplication)
  private
    FFailed: Boolean;
    procedure HandleRunTestResult(Statement: TCustomStatement);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

const
  RuntestCommand: UTF8String = 'runtest "%s";';

{ TAnalysisValidator }

procedure TAnalysisValidator.HandleRunTestResult(Statement: TCustomStatement);
begin
  FFailed := (Statement.ExecResult = csrFailed);
end;

procedure TAnalysisValidator.DoRun;
var
  ErrorMsg, RuntestPath, Path: String;
  aOutputCreator: TOutputCreator;
  aOutputGenerator: TOutputGeneratorConsole;
  aExecutor: TExecutor;
  aParser: TParser;
  TheProgram: TStatementList;
  OutputText: UTF8String;
  PositionalArguments: TStringArray;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate(1);
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  PositionalArguments := GetNonOptions('h', ['help']);
  if Length(PositionalArguments) <> 1 then
  begin
    WriteHelp;
    Terminate(2);
    Exit;
  end;

  RuntestPath := PositionalArguments[0];

  { add your program here }
  Path := CleanAndExpandDirectory(RuntestPath);

  if not DirectoryExistsUTF8(Path) then
  begin
    WriteLn('Path does not exists or is not a directory: ', RuntestPath);
    Terminate(3);
    Exit;
  end;


  aOutputCreator := TOutputCreator.Create;
  aOutputGenerator := TOutputGeneratorConsole.Create(aOutputCreator);
  aExecutor := TExecutor.Create(aOutputCreator);
  aExecutor.AddOnAfterStatementHandler(@HandleRunTestResult);
  aParser := TParser.Create(aExecutor);

  if not aParser.ParseText(Format(RuntestCommand, [Path]), TheProgram) then
    begin
      WriteLn('Internal Error in Validator Parsing!');
      Terminate(4);
      Exit;
    end;
  aParser.Free;

  aExecutor.Execute(TheProgram);
  aOutputGenerator.GenerateReport(OutputText);
  WriteLn(OutputText);

  if FFailed then
    Terminate(5)
  else
    Terminate;
end;

constructor TAnalysisValidator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  aDM := TaDM.Create(Self);
end;

destructor TAnalysisValidator.Destroy;
begin
  inherited Destroy;
end;

procedure TAnalysisValidator.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TAnalysisValidator;
begin
  Application := TAnalysisValidator.Create(nil);
  Application.Title := 'EpiData Analysis Validator';
  Application.Run;
  Application.Free;
end.

