unit report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ast, executor, result_variables, epidocument,
  epidatafilestypes, epiopenfile, outputcreator, epidatafiles;

type

  { TReports }

  TReports = class
  private
    FSt: TCustomReportCommand;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    procedure FileError(const Msg: string);
    procedure CBIDSumStatCallback(UniqueObs, CombinedObs: Integer;
      MissingObs: TBoundArray; ResultDF: TEpiDataFile; CountFields: TEpiFields);

  private
    FCommonRecRes,
    FMissingMainRes,
    FMissingDublRes,
    FNonUniqMainRes,
    FNonUniqDuplRes,
    FErrorRecRes,
    FErrorVarRes:
      TCustomExecutorDataVariable;
    FCurrentDblValDFIndex: Integer;
    procedure DBLValCreateResultVars(Size: Integer);
    procedure DBLValCallAllCallback(CommonRecords, MissingInMain,
      MissingInDupl, NonUniqueMain, NonUniqueDupl, ErrorRec,
      ErrorFields: integer);
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
  ast_types, epiopenfile_cache, LazFileUtils, datamodule, epicustomlist_helper,
  epireport_report_doubleentryvalidate, epidatafilerelations, epitools_val_dbl_entry,
  epireport_report_mainheader;


{ TReports }

procedure TReports.FileError(const Msg: string);
begin
  FExecutor.Error(Msg);
end;

procedure TReports.CBIDSumStatCallback(UniqueObs, CombinedObs: Integer;
  MissingObs: TBoundArray; ResultDF: TEpiDataFile; CountFields: TEpiFields);
var
  RV: TExecVarVector;
  i: Integer;
begin
  FExecutor.AddResultConst('$report_cby_nt', ftInteger).AsIntegerVector[0]       := UniqueObs;
  FExecutor.AddResultConst('$report_cby_complete', ftInteger).AsIntegerVector[0] := CombinedObs;
  RV := FExecutor.AddResultVector('$report_cby_nf', ftInteger, Length(MissingObs));
  for i := Low(MissingObs) to High(MissingObs) do
    RV.AsIntegerVector[i] := MissingObs[i];
end;

procedure TReports.DBLValCreateResultVars(Size: Integer);
begin
  if (Size = 1) then
    begin
      FCommonRecRes   := FExecutor.AddResultConst('$report_val_common', ftInteger);
      FMissingMainRes := FExecutor.AddResultConst('$report_val_mmis', ftInteger);
      FMissingDublRes := FExecutor.AddResultConst('$report_val_dmis', ftInteger);
      FNonUniqMainRes := FExecutor.AddResultConst('$report_val_mnuniq', ftInteger);
      FNonUniqDuplRes := FExecutor.AddResultConst('$report_val_dnuniq', ftInteger);
      FErrorRecRes    := FExecutor.AddResultConst('$report_val_errorrec', ftInteger);
      FErrorVarRes    := FExecutor.AddResultConst('$report_val_errorvar', ftInteger);
    end
  else
    begin
      FCommonRecRes   := FExecutor.AddResultVector('$report_val_common', ftInteger, Size);
      FMissingMainRes := FExecutor.AddResultVector('$report_val_mmis', ftInteger, Size);
      FMissingDublRes := FExecutor.AddResultVector('$report_val_dmis', ftInteger, Size);
      FNonUniqMainRes := FExecutor.AddResultVector('$report_val_mnuniq', ftInteger, Size);
      FNonUniqDuplRes := FExecutor.AddResultVector('$report_val_dnuniq', ftInteger, Size);
      FErrorRecRes    := FExecutor.AddResultVector('$report_val_errorrec', ftInteger, Size);
      FErrorVarRes    := FExecutor.AddResultVector('$report_val_errorvar', ftInteger, Size);
    end;
end;

procedure TReports.DBLValCallAllCallback(CommonRecords, MissingInMain,
  MissingInDupl, NonUniqueMain, NonUniqueDupl, ErrorRec, ErrorFields: integer);
begin
  FCommonRecRes.AsIntegerVector[FCurrentDblValDFIndex]   := CommonRecords;
  FMissingMainRes.AsIntegerVector[FCurrentDblValDFIndex] := MissingInMain;
  FMissingDublRes.AsIntegerVector[FCurrentDblValDFIndex] := MissingInDupl;
  FNonUniqMainRes.AsIntegerVector[FCurrentDblValDFIndex] := NonUniqueMain;
  FNonUniqDuplRes.AsIntegerVector[FCurrentDblValDFIndex] := NonUniqueDupl;
  FErrorRecRes.AsIntegerVector[FCurrentDblValDFIndex]    := ErrorRec;
  FErrorVarRes.AsIntegerVector[FCurrentDblValDFIndex]    := ErrorFields;
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

  if (not Document.Admin.Initialized) then
    begin
      FExecutor.Error('User administration is not used on current project!');
      FSt.ExecResult := csrFailed;
      Exit;
    end;

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
  Options: TEpiReportCBIDOptions;
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
     (not Assigned(Datasets))
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
  Options := [ercoShowSumstats];

  if (not (FST.HasOption('nol'))) then
    Include(Options, ercoShowDetailList);

  CBIDReport := TEpiReportCountById.Create(CoreReportConverter);
  CBIDReport.FieldNames         := FSt.VariableList.GetIdentsAsList;
  CBIDReport.DataFiles          := DataFiles;
  CBIDReport.DocumentFiles      := DocFiles;
  CBIDReport.OnSumStatsComplete := @CBIDSumStatCallback;
  CBIDReport.Options            := Options;
  CBIDReport.RunReport;
  CBIDReport.Free;

  DataFiles.Free;
  for i := DocFiles.Count - 1 downto 0 do
    if DocFiles[i] <> FExecutor.DocFile then
      DocFiles[i].Free;
end;

procedure TReports.DoReportValidateDoubleEntry;
var
  Opt: TOption;
  FN: EpiString;
  ReadCMD: TReadCommand;
  Docfile: TEpiDocumentFile;
  DSName: UTF8String;
  CoreReporter: TCoreReportGeneratorToOutputCreator;
  DblVal: TEpiReportDoubleEntryValidation;
  DF: TEpiDataFile;
  List: TStrings;
  Fields, JoinFields: TEpiFields;
  i: Integer;
  F: TEpiField;
  Options: TEpiReportDEVOptions;
  R: TEpiReportMainHeader;
  DocumentFiles: TEpiDocumentFileList;
  DblOptions: TEpiToolsDblEntryValidateOptions;

  function CompareTreeStructure(Const RelationListA, RelationListB: TEpiDatafileRelationList): boolean;
  var
    i: Integer;
    MRA: TEpiMasterRelation;
    MRB: TEpiMasterRelation;
  begin
    result := (RelationListA.Count = RelationListB.Count);
    if not Result then exit;

    for i := 0 to RelationListA.Count - 1 do
    begin
      MRA := RelationListA.MasterRelation[i];
      MRB := RelationListB.MasterRelation[i];

      Result :=
        (MRA.Datafile.Name = MRB.Datafile.Name) and
        CompareTreeStructure(MRA.DetailRelations, MRB.DetailRelations);

      if not Result then exit;
    end;
  end;

begin
  if FSt.HasOption('fn', Opt) then
    begin
      FN := '';
      if (Assigned(Opt.Expr)) then
        begin
          FN := OPt.Expr.AsString;

          if (not FileExistsUTF8(FN)) then
            begin
              FExecutor.Error('File does not exist: ' + FN);
              FSt.ExecResult := csrFailed;
              Exit;
            end;
        end;

      ReadCMD := TReadCommand.Create(TStringLiteral.Create(FN), TOptionList.Create);
      aDM.OnOpenFileError := @FileError;
      if (aDM.OpenFile(ReadCMD, Docfile) <> dfrSuccess) then
        begin
          FExecutor.Error('Error loading file: ' + FN);
          FSt.ExecResult := csrFailed;
          Exit;
        end;
    end
  else
    Docfile := FExecutor.DocFile;

  DSName := '';
  if FSt.HasOption('ds', Opt) then
    DSName := Opt.Expr.AsIdent;

  if (not FSt.HasOption('ds')) and
     (not FSt.HasOption('fn'))
  then
    begin
      FExecutor.Error('At least one of the options !fn or !ds must be used');
      FSt.ExecResult := csrFailed;
      Exit;
    end;

  if (DSName = '') then
    begin
      if (not CompareTreeStructure(FExecutor.Document.Relations, Docfile.Document.Relations))   then
        begin
          FExecutor.Error('The two projects do not share the same structure of related datasets!');
          FSt.ExecResult := csrFailed;
          Exit;
        end;
    end
  else
    if (Docfile.Document.DataFiles.Count > 1) and
       (DSName = '')
    then
      begin
        FExecutor.Error('The project "' + Docfile.FileName + '" has more than one dataset, and !ds is not specified');
        FSt.ExecResult := csrFailed;
        Exit;
      end;

  if (DSName <> '') and
     (not Docfile.Document.DataFiles.ItemExistsByName(DSName))
  then
    begin
      FExecutor.Error('Dataset "' + DSName + '" not found!');
      FSt.ExecResult := csrFailed;
      Exit;
    end;

  CoreReporter := TCoreReportGeneratorToOutputCreator.Create(FOutputCreator);
  FCurrentDblValDFIndex := 0;

  Options := [erdoShowOverview];
  if (not (FSt.HasOption('nol'))) then Include(Options, erdoShowDetailList);

  DblOptions := [devIgnoreDeleted, devCaseSensitiveText];
  if FSt.HasOption('noc') then Exclude(DblOptions, devCaseSensitiveText);
  if FSt.HasOption('val') then Include(DblOptions, devAddVerifiedFlagToDF);

  DocumentFiles := TEpiDocumentFileList.Create;
  DocumentFiles.Add(FExecutor.DocFile);
  DocumentFiles.Add(Docfile);

  R := TEpiReportMainHeader.Create(CoreReporter);
  R.ProjectList := DocumentFiles;
  R.Title := 'Double Entry Validation Report';
  R.RunReport;
  R.Free;
  DocumentFiles.Free;

  if ((FExecutor.Document.DataFiles.Count > 1) and (Docfile.Document.DataFiles.Count > 1)) and
     (not FSt.HasOption('ds'))
  then
    // The only case where two who projects are being validate against each other.
    begin
      if Assigned(FSt.VariableList) and
         (FSt.VariableList.Count > 0)
      then
        begin
          FExecutor.Error('When comparing whole projects, it is not possible to select individual variables to compare!');
          FSt.ExecResult := csrFailed;
          Exit;
        end;

      Fields := TEpiFields.Create(nil);
      Fields.ItemOwner := false;
      Fields.UniqueNames := false;

      DBLValCreateResultVars(FExecutor.Document.DataFiles.Count);
      for DF in FExecutor.Document.DataFiles do
        begin
          for F in DF.Fields do
            Fields.AddItem(F);

          DblVal := TEpiReportDoubleEntryValidation.Create(CoreReporter);
          DblVal.KeyFields     := DF.KeyFields;
          DblVal.CompareFields := Fields;
          DblVal.MainDF := DF;
          DblVal.DuplDF := Docfile.Document.DataFiles.GetDataFileByName(DF.Name);
          DblVal.DblEntryValidateOptions := DblOptions;
          DblVal.ReportOptions := Options;
          DblVal.RunReport;

          Fields.Clear;
          Inc(FCurrentDblValDFIndex);
        end;
    end
  else
    begin
      if (DSName <> '') then
        DF := Docfile.Document.DataFiles.GetDataFileByName(DSName)
      else
        DF := Docfile.Document.DataFiles[0];

      if Assigned(FSt.VariableList) and
        (FSt.VariableList.Count > 0)
      then
        List := FSt.VariableList.GetIdentsAsList
      else
        List := FExecutor.DataFile.KeyFields.GetItemNames(false);

      JoinFields := TEpiFields.Create(nil);
      JoinFields.UniqueNames := false;
      JoinFields.Sorted := false;
      for i := 0 to List.Count - 1 do
        JoinFields.AddItem(FExecutor.DataFile.Fields.FieldByName[List[i]]);

      Fields := TEpiFields.Create(nil);

      for F in FExecutor.DataFile.Fields do
        begin
          if (JoinFields.ItemExistsByName(F.Name)) then continue;

          if (FSt.HasOption('nos'))    and (F.FieldType in StringFieldTypes) then continue;
          if (FSt.HasOption('nodt'))   and (F.FieldType in DateFieldTypes + TimeFieldTypes) then continue;
          if (FSt.HasOption('noauto')) and (F.FieldType in AutoFieldTypes) then continue;

          Fields.AddItem(F);
        end;

      DBLValCreateResultVars(1);

      DblVal := TEpiReportDoubleEntryValidation.Create(CoreReporter);
      DblVal.KeyFields     := JoinFields;
      DblVal.CompareFields := Fields;
      DblVal.MainDF := FExecutor.DataFile;
      DblVal.DuplDF := DF;
      DblVal.OnCallAllDone := @DBLValCallAllCallback;
      DblVal.DblEntryValidateOptions := DblOptions;
      DblVal.ReportOptions := Options;
      DblVal.RunReport;

      Fields.Free;
      JoinFields.Free;
    end;

  if (Docfile <> FExecutor.DocFile) then
    Docfile.Free;
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

