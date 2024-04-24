unit history;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, ast, ast_types, outputcreator;

type

  { THistory }

  THistory = class(IFPObserver)
  private
    FUpdateCount: Integer;
    FCurrentIdx: Integer;
    FStrings: TStrings;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    procedure ExecutorAfterStatement(Statement: TCustomStatement);
    procedure ExecutorBeforeStatement(Statement: TCustomStatement);
    function GetFailed(const Index: Integer): Boolean;
    procedure SetFailed(const Index: Integer; AValue: Boolean);

  // Command Log:
  private
    FLoadingStartup: boolean;
    FCommandLogActive: boolean;
    FCommandLogFilename: UTF8String;
    FCommandLogLineCount: Integer;
    FCommandLogLines: TStrings;
    FEnabled: Boolean;
    function GetCustom(const Index: Integer): Boolean;
    procedure SetCustom(const Index: Integer; AValue: Boolean);
    procedure StopCurrentLog;
    procedure CommandLogChange(Sender: TObject);
    procedure CommandLogFileChange(Sender: TObject);
    procedure CommandLogLinesChange(Sender: TObject);
    function  CheckCommandLogFile(ReportError: boolean = true): boolean;
    procedure DoSaveCommandLog;
    procedure DoLoadCommandLog(ReportError: boolean = true);
    procedure SetupHooks;
    procedure RemoveHooks;

  // IFPObserver
  private
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);

  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure AddLine(S: UTF8String);
    procedure AddLines(L: TStrings); overload;
    procedure AddLines(S: UTF8String); overload;
    procedure AddCommandLogLine(S: UTF8String);
    property Lines: TStrings read FStrings;
    property Failed[Const Index: Integer]: Boolean read GetFailed write SetFailed;
    property Custom[Const Index: Integer]: Boolean read GetCustom write SetCustom;
    property Enabled: Boolean read FEnabled write FEnabled;
    property LoadingStartup: boolean read FLoadingStartup write FLoadingStartup;
  end;

implementation

uses
  LazUTF8, LazFileUtils, epiversionutils, ana_globals;

{ THistory }

procedure THistory.ExecutorAfterStatement(Statement: TCustomStatement);
var
  Idx: Integer;
begin
  if (not Enabled) then exit;

  Idx := Statement.LineNo - 1;
  Case Statement.ExecResult of
    csrSuccess:
      begin
        if (Statement.StatementType = stCD) then
        begin
          StopCurrentLog;
          DoLoadCommandLog;
        end;

        // Special case where an implicit read is executed from
        // within the TExecutor itself.
        // Since "read" inserted into history in the "wrong" order,
        // we need to rearrange in the StringList.
        if (Statement.StatementType = stRead) and
           (Statement.ByteNo = -1)
        then
          FStrings.Move(FStrings.Count - 1, FStrings.Count - 2);
      end;

    csrFailed:
      begin
        // Special case where an implicit read is executed from
        // within the TExecutor itself.
        if (Statement.StatementType = stRead) and
           (Statement.ByteNo = -1)
        then
          begin
            FStrings.Delete(FStrings.Count - 1);
            FCurrentIdx := FStrings.Count - 1;
            Exit;
          end;
 // Jamie: the FStrings statement may fail with out of bounds Idx when
 //        running a pgm statement that completes with csrFailed.
 //        Same below with csrCustom
        if (not FExecutor.Executing) then
            FStrings.Objects[Idx] := TObject(1);
      end;

    csrCustom:
      if (not FExecutor.Executing) then
        FStrings.Objects[Idx] := TObject(2);
  end;
end;

procedure THistory.ExecutorBeforeStatement(Statement: TCustomStatement);
begin
  if (not Enabled) then exit;

  // Special case where an implicit read is executed from
  // within the TExecutor itself.
  if (Statement.StatementType = stRead) and
     (Statement.ByteNo = -1)
  then
    AddLine('read;');
end;

procedure THistory.CommandLogChange(Sender: TObject);
begin
  FCommandLogActive := FExecutor.SetOptionValue[ANA_SO_COMMANDLOG] = 'ON';
  DoLoadCommandLog;
end;

procedure THistory.CommandLogFileChange(Sender: TObject);
begin
  FCommandLogFilename  := FExecutor.SetOptionValue[ANA_SO_COMMANDLOGFILE];
  DoLoadCommandLog;
end;

procedure THistory.CommandLogLinesChange(Sender: TObject);
begin
  FCommandLogLineCount := StrToInt(FExecutor.SetOptionValue[ANA_SO_COMMANDLOGLINES]);
  DoSaveCommandLog;
end;

function THistory.GetFailed(const Index: Integer): Boolean;
begin
  if (not Enabled) then exit;

  result := false;
  if (Index < 0) or (Index > FStrings.Count) then exit;

  result := (PtrInt(FStrings.Objects[Index]) = 1);
end;

procedure THistory.SetFailed(const Index: Integer; AValue: Boolean);
begin
  if (not Enabled) then exit;

  if (Index < 0) or (Index > FStrings.Count) then exit;

  if AValue then
    FStrings.Objects[Index] := TObject(1)
  else
    FStrings.Objects[Index] := nil;
end;

procedure THistory.StopCurrentLog;
begin
  AddCommandLogLine('// EpiData Analysis v' + GetEpiVersionInfo(HINSTANCE) + ' Stop:  date: ' + DateToStr(Now) + '  time: ' + TimeToStr(Now));
end;

function THistory.GetCustom(const Index: Integer): Boolean;
begin
  if (not Enabled) then exit;

  result := false;
  if (Index < 0) or (Index > FStrings.Count) then exit;

  result := (PtrInt(FStrings.Objects[Index]) = 2);
end;

procedure THistory.SetCustom(const Index: Integer; AValue: Boolean);
begin
  if (not Enabled) then exit;

  if (Index < 0) or (Index > FStrings.Count) then exit;

  if AValue then
    FStrings.Objects[Index] := TObject(2)
  else
    FStrings.Objects[Index] := nil;
end;

function THistory.CheckCommandLogFile(ReportError: boolean): boolean;
begin
  (*Debug
    FOutputCreator.DoInfoAll('Current directory is ' + GetCurrentDirUTF8);
    FOutputCreator.DoInfoAll('Does ' + FCommandLogFilename + ' exist? ' + BoolToStr(FileExistsUTF8(FCommandLogFilename)));
  End Debug*)
  Result :=
          (
            (FileExistsUTF8(FCommandLogFilename)) and
            (FileIsWritable(FCommandLogFilename))
          ) or (
            DirectoryIsWritable(GetCurrentDirUTF8)
          );


  if (not result) and
     (ReportError) and
     (not LoadingStartup)
  then
    begin
      FOutputCreator.DoWarning('Command Log file is not writeable and has been disabled!' + LineEnding +
                               'To re-enable command log:' + LineEnding +
                               ' 1) Change currentdir with write access' + LineEnding +
                               ' 2) type:  SET "COMMANDLOG" := "ON"');
      FOutputCreator.DoInfoAll('');
      FExecutor.SetOptionValue['COMMANDLOG'] := 'OFF';
      Exit;
    end;
end;

procedure THistory.DoSaveCommandLog;
var
  LineCount: LongInt;
begin
  if (not FCommandLogActive) then
    Exit;

  while FCommandLogLines.Count > FCommandLogLineCount do
    FCommandLogLines.Delete(0);

  if (FCommandLogFilename <> '') then
    begin
      if (not CheckCommandLogFile) then
        Exit;

      FCommandLogLines.SaveToFile(FCommandLogFilename);
    end;
end;

procedure THistory.DoLoadCommandLog(ReportError: boolean);
begin
  if (not FCommandLogActive) then
    Exit;

  if (not CheckCommandLogFile(ReportError)) then
    Exit;

  if (FCommandLogFilename <> '') then
    begin
      if (FileExistsUTF8(FCommandLogFilename)) then
        FCommandLogLines.LoadFromFile(FCommandLogFilename)
      else
        FCommandLogLines.Clear;

      FCommandLogLines.Add('// EpiData Analysis v' + GetEpiVersionInfo(HINSTANCE) + ' Start:  date: ' + DateToStr(Now) + '  time: ' + TimeToStr(Now));
    end;
end;

procedure THistory.SetupHooks;
begin
  FExecutor.SetOptions.GetValue('COMMANDLOG').AddOnChangeHandler(@CommandLogChange);
  FExecutor.SetOptions.GetValue('COMMANDLOGFILE').AddOnChangeHandler(@CommandLogFileChange);
  FExecutor.SetOptions.GetValue('COMMANDLOGLINES').AddOnChangeHandler(@CommandLogLinesChange);

  FCommandLogActive    := FExecutor.SetOptionValue['COMMANDLOG'] = 'ON';
  FCommandLogFilename  := FExecutor.SetOptionValue['COMMANDLOGFILE'];
  FCommandLogLineCount := StrToInt(FExecutor.SetOptionValue['COMMANDLOGLINES']);
end;

procedure THistory.RemoveHooks;
begin
  FExecutor.SetOptions.GetValue('COMMANDLOG').RemoveOnChangeHandler(@CommandLogChange);
  FExecutor.SetOptions.GetValue('COMMANDLOGFILE').RemoveOnChangeHandler(@CommandLogFileChange);
  FExecutor.SetOptions.GetValue('COMMANDLOGLINES').RemoveOnChangeHandler(@CommandLogLinesChange);
end;

procedure THistory.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if (Operation <> ooChange) then exit;

  if FStrings.Count = 0 then
    FCurrentIdx := -1;
end;

constructor THistory.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator
  );
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;

  FStrings := TStringList.Create;
  FStrings.FPOAttachObserver(Self);

  FCommandLogLines := TStringList.Create;
  FEnabled := true;
  FLoadingStartup := false;

  FExecutor.AddOnAfterStatementHandler(@ExecutorAfterStatement);
  FExecutor.AddOnBeforeStatementHandler(@ExecutorBeforeStatement);
  SetupHooks;
  DoLoadCommandLog(false);
end;

destructor THistory.Destroy;
begin
  StopCurrentLog;
  RemoveHooks;
  inherited Destroy;
end;

procedure THistory.AddLine(S: UTF8String);
begin
  if (not Enabled) then exit;

  FCurrentIdx := FStrings.Count;
  FStrings.Add(S);
  FCommandLogLines.Add(S);
  DoSaveCommandLog;
end;

procedure THistory.AddLines(L: TStrings);
begin
  if (not Enabled) then exit;

  FCurrentIdx := FStrings.Count;
  FStrings.AddStrings(L);
  FCommandLogLines.AddStrings(L);
  DoSaveCommandLog;
end;

procedure THistory.AddLines(S: UTF8String);
begin
  if (not Enabled) then exit;

  FCurrentIdx := FStrings.Count;
  FStrings.AddText(S);
  FCommandLogLines.AddText(S);
  DoSaveCommandLog;
end;

procedure THistory.AddCommandLogLine(S: UTF8String);
begin
  if (not Enabled) then exit;

  FCommandLogLines.Add(S);
  DoSaveCommandLog;
end;

end.

