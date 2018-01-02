unit report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, executor, epidocument, outputcreator;

type

  { TReports }

  TReports = class
  private
    FSt: TCustomReportCommand;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  protected
    procedure DoReportUsers;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    procedure Report(ST: TCustomReportCommand);
  end;

implementation

uses
  episecuritylog, epilogger, epiglobals, epidatafileutils;


{ TReports }

procedure TReports.DoReportUsers;
var
  Document: TEpiDocument;
  SecurityLog: TEpiSecurityDatafile;
  DataLog: TEpiSecurityDataEventLog;
  KeyLog: TEpiSecurityKeyFieldLog;
  blockedcount, i, RowCount, j: Integer;
  LogTypeCount: array[TEpiLogEntry] of integer;
  LogTypeEnum: TEpiLogEntry;
  T: TOutputTable;
begin
  Document := FExecutor.Document;

  SecurityLog := TEpiSecurityDatafile(Document.DataFiles.GetDataFileByName(EpiSecurityLogDatafileName));
  DataLog     := TEpiSecurityDataEventLog(Document.Datafiles.GetDataFileByName(EpiSecurityLogDataEventName));
  KeyLog      := TEpiSecurityKeyFieldLog(Document.DataFiles.GetDataFileByName(EpiSecurityLogKeyDataName));

  blockedcount := 0;
  for LogTypeEnum := Low(LogTypeCount) to High(LogTypeCount) do
    LogTypeCount[LogTypeEnum] := 0;

  for i := 0 to SecurityLog.Size -1 do
  begin
    LogTypeEnum := TEpiLogEntry(SecurityLog.LogType.AsInteger[i]);
    LogTypeCount[LogTypeEnum] := LogTypeCount[LogTypeEnum] + 1;

    if LogTypeEnum = ltBlockedLogin then
      inc(blockedcount);
  end;

  RowCount :=
   3 +                                    // Header + Lines numbers + Start date + Blocked count
   Integer(High(TEpiLogEntry));           // Count of each log type

  DumpDatafileRecords(SecurityLog);

  T := FOutputCreator.AddTable;
  T.ColCount := 2;
  T.RowCount := RowCount;
  T.Header.Text := 'Log Overview';

  T.Cell[0, 0].Text := 'Task';
  T.Cell[1, 0].Text := 'Content';

  T.Cell[0, 1].Text := 'Log Entries';
  T.Cell[1, 1].Text := IntToStr(SecurityLog.Size);

  T.Cell[0, 2].Text := 'Start Date';
  for i := 0 to SecurityLog.Size - 1 do
    if SecurityLog.Date.AsDateTime[i] <> 0 then
      begin
        T.Cell[1, 2].Text := DateTimeToStr(SecurityLog.Date.AsDateTime[i] +
                                           SecurityLog.Time.AsDateTime[i]);
        break;
      end;

  i := 3;
  for LogTypeEnum := Low(LogTypeCount) to High(LogTypeCount) do
  begin
    if LogTypeEnum = ltNone then continue;

    T.Cell[0, i].Text := EpiLogEntryText[LogTypeEnum];
    T.Cell[1, i].Text := IntToStr(LogTypeCount[LogTypeEnum]);
    Inc(i);
  end;

  FOutputCreator.DoNormal('');

  T := FOutputCreator.AddTable;
  T.ColCount := 3;
  T.RowCount := blockedcount + 1;

  T.Header.Text := 'Blocked Login Attempts';

  T.Cell[0, 0].Text := 'Blocked machine';
  T.Cell[1, 0].Text := 'Latest login';
  T.Cell[2, 0].Text := 'Date / Time';

  i := 1;
  for j := 0 to SecurityLog.Size -1 do
  begin
    LogTypeEnum := TEpiLogEntry(SecurityLog.LogType.AsInteger[j]);

    if (LogTypeEnum = ltBlockedLogin)
    then
      begin
        T.Cell[0, i].Text := SecurityLog.LogContent.AsString[j];
        T.Cell[1, i].Text := SecurityLog.UserName.AsString[j];
        T.Cell[2, i].Text := DateTimeToStr(SecurityLog.Date.AsDateTime[j] +
                                           SecurityLog.Time.AsDateTime[j]);

        Inc(i);
      end;
  end;

  FSt.ExecResult := csrSuccess;
end;

constructor TReports.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator
  );
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

procedure TReports.Report(ST: TCustomReportCommand);
begin
  FSt := ST;

  case FSt.SubCmd of
    rscCountById: ;

    rscUsers:
      DoReportUsers;
  end;
end;

end.

