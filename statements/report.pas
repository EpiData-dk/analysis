unit report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ast, executor, result_variables, epidocument,
  epidatafilestypes, epiopenfile, outputcreator;

type

  { TReports }

  TReports = class
  private
    FSt: TCustomReportCommand;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    procedure FileError(const Msg: string);
  protected
    procedure DoReportUsers;
    procedure DoReportCountById;
    procedure DoReportValidateDoubleEntry;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    procedure Report(ST: TCustomReportCommand);
  end;

implementation

uses
  episecuritylog, epilogger, epiglobals, epidatafileutils, epireport_report_countbyid,
  ast_types, epiopenfile_cache, epidatafiles, LazFileUtils, datamodule, epireport_report_doubleentryvalidate;


{ TReports }

procedure TReports.FileError(const Msg: string);
begin
  FExecutor.Error(Msg);
end;

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

procedure TReports.DoReportCountById;
var
  CoreReportConverter: TCoreReportGeneratorToOutputCreator;
  CBIDReport: TEpiReportCountById;
var
  Opt: TOption;
  FileNames, Datasets: TExecVarGlobalVector;
  FileCache: TEpiDocumentFileCache;
  S, DSName: String;
  i, Idx: Integer;
  DataFiles: TEpiDataFiles;
  Doc: TEpiDocument;
  DF: TEpiDataFile;
  DocFile: TEpiDocumentFile;
  DocFiles: TEpiDocumentFileList;
  ReadCmd: TReadCommand;
begin
  FileNames := nil;
  if FSt.HasOption('fn', Opt) then
    begin
      if (Opt.Expr.ResultType <> rtString) then
        begin
          FExecutor.Error('"' + Opt.Expr.AsIdent + '" must contain string values!');
          FSt.ExecResult := csrFailed;
          Exit;
        end;

      FileNames := TExecVarGlobalVector(FExecutor.GetExecDataVariable(Opt.Expr.AsIdent));
    end
  else
    begin
      if (not Assigned(FExecutor.Document)) then
        begin
          FExecutor.Error('If !fn is not used then a project must be opened for Count-by-Id to work!');
          FSt.ExecResult := csrFailed;
          Exit;
        end;
    end;

  Datasets := nil;
  if FSt.HasOption('ds', Opt) then
    begin
      if (Opt.Expr.ResultType <> rtString) then
        begin
          FExecutor.Error('"' + Opt.Expr.AsIdent + '" must contain string values!');
          FSt.ExecResult := csrFailed;
          Exit;
        end;

      Datasets := TExecVarGlobalVector(FExecutor.GetExecDataVariable(Opt.Expr.AsIdent));
    end;

  if (Assigned(FileNames)) and
     (Assigned(Datasets)) and
     (FileNames.Length <> Datasets.Length)
  then
    begin
      FExecutor.Error(
        '"' + FileNames.Ident + '" and "' + Datasets.Ident + '" must have the same number of elements!' + LineEnding +
        FileNames.Ident + ': size = ' + IntToStr(FileNames.Length) + LineEnding +
        Datasets.Ident + ': size = ' + IntToStr(Datasets.Length)
      );
      FSt.ExecResult := csrFailed;
      Exit;
    end;

  if (not Assigned(FileNames)) and
     (not Assigned(DataFiles))
  then
    begin
      FExecutor.Error('Neither !fs or !ds was used. At least one must be provided!');
      FSt.ExecResult := csrFailed;
      Exit;;
    end;

  DataFiles := TEpiDataFiles.Create(nil);
  DataFiles.UniqueNames := false;
  DataFiles.Sorted := false;
  DataFiles.ItemOwner := false;

  DocFiles := TEpiDocumentFileList.Create;

  if Assigned(FileNames) then
    begin
      FileCache := TEpiDocumentFileCache.Create;

      for i := 0 to FileNames.Length - 1 do
        begin
          S := FileNames.AsStringVector[i];

          if S <> '' then
            begin
              if (not FileExistsUTF8(S)) then
              begin
                FExecutor.Error('File does not exist: ' + S + '  (index: ' + IntToStr(i + 1));
                FSt.ExecResult := csrFailed;
                Exit;
              end;

              ReadCmd := TReadCommand.Create(
                TStringLiteral.Create(S),
                TOptionList.Create
              );

              aDM.OnOpenFileError := @FileError;
              if aDM.OpenFile(ReadCmd, Docfile) <> dfrSuccess then
                begin
                  FExecutor.Error('Error loading file: ' + S);
                  FSt.ExecResult := csrFailed;
                  Exit;
                end;
              Doc := DocFile.Document;

              if Assigned(Datasets) then
                begin
                  DSName := Datasets.AsStringVector[i];
                  if ((DSName = '') or (DSName = TEpiStringField.DefaultMissing)) and
                     (Doc.DataFiles.Count > 1)
                  then
                    begin
                      FExecutor.Error('The project "' + S + '" contains more than one dataset and no dataset name was given on index: ' + IntToStr(i + 1));
                      FSt.ExecResult := csrFailed;
                      Exit;
                    end;

                  if ((DSName = '') or (DSName = TEpiStringField.DefaultMissing)) then
                    DF := Doc.DataFiles[0]
                  else
                    DF := Doc.DataFiles.GetDataFileByName(DSName);

                  if (not Assigned(DF)) then
                    begin
                      FExecutor.Error('The project "' + S + '" contains no dataset with the name "' + DSName + '" (on index: ' + IntToStr(i + 1) + ')');
                      FSt.ExecResult := csrFailed;
                      Exit;
                    end;

                  DocFiles.Add(DocFile);
                  DataFiles.AddItem(DF);
                end
              else
                // NO Assigned Datasets
                begin
                  if (Doc.DataFiles.Count > 1) then
                    begin
                      FExecutor.Error('The project "' + S + '" contains more than one dataset and no dataset name was given on index: ' + IntToStr(i + 1));
                      FSt.ExecResult := csrFailed;
                      Exit;
                    end;

                  DF := Doc.DataFiles[0];
                  DocFiles.Add(DocFile);
                  DataFiles.AddItem(DF);
                end;

              ReadCmd.Free;
            end;
        end;
    end
  else
    // NO Assigned Filenames
    begin
      for i := 0 to Datasets.Length - 1 do
        begin
          DSName := Datasets.AsStringVector[i];

          if ((DSName = '') or (DSName = TEpiStringField.DefaultMissing)) then
            begin
              FExecutor.Error('No filename provided and no dataset name provided! (on index: ' + IntToStr(i + 1) + ')');
              FSt.ExecResult := csrFailed;
              Exit;
            end;

          if (not FExecutor.Datasets.Find(DSName, Idx)) then
            begin
              FExecutor.Error('Current contains no dataset with the name "' + DSName + '" (on index: ' + IntToStr(i + 1) + ')');
              FSt.ExecResult := csrFailed;
              Exit;
            end;

          DF := FExecutor.Datasets.Data[Idx].DataFile;
          DataFiles.AddItem(DF);
          DocFiles.Add(FExecutor.DocFile);
        end;
    end;


  CoreReportConverter := TCoreReportGeneratorToOutputCreator.Create(FOutputCreator);

  CBIDReport := TEpiReportCountById.Create(CoreReportConverter);
  CBIDReport.FieldNames    := FSt.VariableList.GetIdentsAsList;
  CBIDReport.DataFiles     := DataFiles;
  CBIDReport.DocumentFiles := DocFiles;
  CBIDReport.RunReport;
  CBIDReport.Free;

  DataFiles.Free;
  for i := DocFiles.Count - 1 downto 0 do
    DocFiles[i].Free;
  DocFiles.Free;
end;

procedure TReports.DoReportValidateDoubleEntry;
var
  Opt: TOption;
  FN: EpiString;
  ReadCMD: TReadCommand;
  Docfile: TEpiDocumentFile;
begin
  if FSt.HasOption('fn', Opt) then
    begin
      FN := '';
      if (Assigned(Opt.Expr)) then
        begin
          FN := OPt.Expr.AsString

          if (not FileExistsUTF8(FN)) then
            begin
              DoError('File does not exist: ' + FN);
              FSt.ExecResult := csrFailed;
              Exit;
            end;
        end;

      ReadCMD := TReadCommand.Create(TStringLiteral.Create(FN), TOptionList.Create);
      aDM.OnOpenFileError := @FileError;
      if (aDM.OpenFile(ReadCMD, Docfile) <> dfrSuccess) then
        begin
          FExecutor.Error('Error loading file: ' + S);
          FSt.ExecResult := csrFailed;
          Exit;
        end;
    end
  else
    Docfile := FExecutor.DocFile;

  if FSt.HasOption('ds', Opt) then
    begin
    end;
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
    rscCountById:
      DoReportCountById;

    rscUsers:
      DoReportUsers;

    rscValidateDoubleEntry:
      DoReportValidateDoubleEntry;
  end;
end;

end.

