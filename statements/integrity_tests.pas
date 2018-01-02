unit integrity_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, executor, outputcreator, ast, result_variables,
  epidatafilerelations, epitools_projectvalidate, epifields_helper;

type

  { TProjectChecker }

  TProjectChecker = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FValuelabelFormatType: TEpiGetValueLabelType;
    FVariableLabelFormatType: TEpiGetVariableLabelType;
    procedure CheckDataReport(const Sender: TObject;
      const Relation: TEpiMasterRelation;
      const ResultArray: TEpiProjectResultArray);
  private
    // CheckData things
    FCheckDataST: TCheckDataCommand;
    FCheckDataOptions: TEpiToolsProjectValidateOptions;
    procedure DoRecordsSummarizedResultTable(
      Const Relation: TEpiMasterRelation;
      Const RecordArray: TEpiProjectResultArray);
    procedure DoRecordsReport(
      Const Relation: TEpiMasterRelation;
      Const RecordResult: TEpiProjectResultArray);
  protected
    procedure DoCheckData(ST: TCheckDataCommand); virtual;
    procedure DoCheckRelate(ST: TCheckRelateCommand); virtual;
    procedure DoCheckKey(ST: TCheckKeyCommand); virtual;
    procedure DoCheckStudy(ST: TCheckStudyCommand); virtual;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    procedure Check(ST: TCustomCheckCommand);
  end;

implementation

uses
  epidatafiles, epitools_integritycheck, epimiscutils,
  epidatafilerelations_helper, options_utils,
  epidatafileutils, epidatafilestypes, LazUTF8,
  typinfo, ana_globals, epicustomlist_helper;

const
  CheckDataResultNames: Array[TEpiToolsProjectValidateOption] of string =
    ('del',
     'smis',
     'mustenter',
     'range_VL',
     'compare',
     'length',
     'jumps',
     'keyField',
     'unique',
     'relate',
     'study'
    );


{ TProjectChecker }

procedure TProjectChecker.CheckDataReport(const Sender: TObject;
  const Relation: TEpiMasterRelation; const ResultArray: TEpiProjectResultArray
  );
begin
  DoRecordsSummarizedResultTable(Relation, ResultArray);

  if FCheckDataST.HasOption('lst') then
    DoRecordsReport(Relation, ResultArray);
end;

procedure TProjectChecker.DoRecordsSummarizedResultTable(
  const Relation: TEpiMasterRelation; const RecordArray: TEpiProjectResultArray
  );
var
  RecordErrorCount: Integer;
  i: Integer;
  FieldErrorCount: Integer;
  RecordCount: Integer;
  OptCount: Integer;
  SumTable: array[TEpiToolsProjectValidateOption] of array of cardinal;
  TotalTable: array[TEpiToolsProjectValidateOption] of cardinal;
  Opt: TEpiToolsProjectValidateOption;
  Rec: TEpiProjectValidateResultRecord;
  S: String;
  j: Integer;
  ValidationFields: TStrings;
  DF: TEpiDataFile;
  T: TOutputTable;
  F: TEpiField;
  lValidateOptions: TEpiToolsProjectValidateOptions;
  RV: TCustomExecutorDataVariable;
  HasProperty: Boolean;

  function CalcErrorPct: Extended;
  begin
    Result := RecordErrorCount / RecordCount;
  end;

  function CalcErrorFieldPct: Extended;
  begin
    Result := FieldErrorCount / (RecordCount * ValidationFields.Count);
  end;

begin
  DF := Relation.Datafile;

  ValidationFields := TStringList.Create;
  for F in DF.Fields do
    ValidationFields.AddObject(F.Name, F);

  if (FCheckDataST.HasOption('detail')) then
    begin
      RecordErrorCount := 0;
      FieldErrorCount  := Length(RecordArray);

      RecordCount := DF.Size - DF.DeletedCount;

      if Length(RecordArray) > 0 then
        Inc(RecordErrorCount);
      for i := Low(RecordArray) + 1 to High(RecordArray) do
      begin
        if RecordArray[i-1].RecNo <> RecordArray[i].RecNo then
          Inc(RecordErrorCount);
      end;

      T := FOutputCreator.AddTable;
      T.ColCount := 2;
      T.RowCount := 7;
      T.Cell[0, 0].Text := 'Test';                            T.Cell[1, 0].Text := 'Result';
      T.Cell[0, 1].Text := 'Number of variables checked';     T.Cell[1, 1].Text := IntToStr(ValidationFields.Count);
      T.Cell[0, 2].Text := 'Number of obs. checked';          T.Cell[1, 2].Text := IntToStr(RecordCount);
      T.Cell[0, 3].Text := 'Obs. with errors';                T.Cell[1, 3].Text := IntToStr(RecordErrorCount);
      T.Cell[0, 4].Text := 'Variable entries with errors';    T.Cell[1, 4].Text := IntToStr(FieldErrorCount);
      T.Cell[0, 5].Text := 'Error percentage (#obs)';         T.Cell[1, 5].Text := FormatFloat('##0.00', CalcErrorPct * 100);
      T.Cell[0, 6].Text := 'Error percentage (#vars)';        T.Cell[1, 6].Text := FormatFloat('##0.00', CalcErrorFieldPct * 100);

      FExecutor.AddResultConst('$check_data_detail_fieldschecked', ftInteger).AsIntegerVector[0] := ValidationFields.Count;
      FExecutor.AddResultConst('$check_data_detail_obschecked',    ftInteger).AsIntegerVector[0] := RecordCount;
      FExecutor.AddResultConst('$check_data_detail_obserrors',     ftInteger).AsIntegerVector[0] := RecordErrorCount;
      FExecutor.AddResultConst('$check_data_detail_varerrors',     ftInteger).AsIntegerVector[0] := FieldErrorCount;
      FExecutor.AddResultConst('$check_data_detail_obspct',        ftFloat).AsFloatVector[0]     := CalcErrorPct * 100;
      FExecutor.AddResultConst('$check_data_detail_varpct',        ftFloat).AsFloatVector[0]     := CalcErrorFieldPct * 100;
    end;

  lValidateOptions := FCheckDataOptions;
  Exclude(lValidateOptions, pvIgnoreDeleted);
  Exclude(lValidateOptions, pvCheckStudyInfo);

  OptCount := 0;
  for Opt in lValidateOptions do
  begin
    SetLength(SumTable[Opt], ValidationFields.Count);
    Inc(OptCount);
    TotalTable[Opt] := 0;
  end;

  for i := Low(RecordArray) to High(RecordArray) do
  begin
    Rec := RecordArray[i];
    Inc(SumTable[Rec.FailedCheck, ValidationFields.IndexOf(Rec.Field.Name)]);
    Inc(TotalTable[Rec.FailedCheck]);
  end;

  T := FOutputCreator.AddTable;
  // +2: 1 for variable name and 1 for "n" col
  T.ColCount := OptCount + 2;
  T.RowCount := ValidationFields.Count + 2;
  T.Header.Text := 'Dataset: ' + DF.Name + '  Caption: ' + DF.Caption.Text;

  RV := FExecutor.AddResultVector('$check_data_varnames', ftString, ValidationFields.Count);
  for i := 0 to ValidationFields.Count - 1 do
    begin
      T.Cell[0, i + 1].Text := TEpiField(ValidationFields.Objects[i]).GetVariableLabel(FVariableLabelFormatType);
      RV.AsStringVector[i] := ValidationFields[i];
    end;
  T.SetRowBorders(0, [cbTop, cbBottom]);

  T.Cell[0, ValidationFields.Count + 1].Text := 'Total';
  T.SetColAlignment(0, taLeftJustify);
  T.SetRowBorders(ValidationFields.Count + 1, [cbTop, cbBottom]);

  RV := FExecutor.AddResultVector('$check_data_n', ftInteger, ValidationFields.Count);
  T.Cell[1, 0].Text := 'n';
  for i := 0 to ValidationFields.Count - 1 do
    begin
      T.Cell[1, i + 1].Text := IntToStr(TEpiField(ValidationFields.Objects[i]).NonMissingSize);
      RV.AsIntegerVector[i] := TEpiField(ValidationFields.Objects[i]).NonMissingSize;
    end;

  i := 2;
  for Opt in lValidateOptions do
    T.Cell[PostInc(i), 0].Text := EpiToolProjectValidationOptionTextShort[Opt];

  j := 2;
  for Opt in lValidateOptions do
  begin
    RV := FExecutor.AddResultVector('$check_data_' + CheckDataResultNames[Opt], ftInteger, ValidationFields.Count);
    for i := 0 to ValidationFields.Count - 1 do
      begin
        HasProperty := true;
        case Opt of
          pvCheckMustEnter:
            HasProperty := TEpiField(ValidationFields.Objects[i]).EntryMode = emMustEnter;

          pvCheckDataRange:
            HasProperty := Assigned(TEpiField(ValidationFields.Objects[i]).Ranges) or
                           Assigned(TEpiField(ValidationFields.Objects[i]).ValueLabelSet);

          pvCheckComparison:
            HasProperty := Assigned(TEpiField(ValidationFields.Objects[i]).Comparison);

          pvCheckJumpReset:
            HasProperty := Assigned(TEpiField(ValidationFields.Objects[i]).Jumps);
        end;

        if HasProperty then
          T.Cell[j, i + 1].Text := IntToStr(SumTable[Opt, i])
        else
          T.Cell[j, i + 1].Text := 'n/a';

        RV.AsIntegerVector[i] := SumTable[Opt, i];
      end;
    Inc(j);
  end;

  i := 2;
  for Opt in lValidateOptions do
    begin
      RV := FExecutor.AddResultConst('$check_data_total_' + CheckDataResultNames[Opt], ftInteger);
      RV.AsIntegerVector[0] := TotalTable[Opt];
      T.Cell[PostInc(i), ValidationFields.Count + 1].Text := IntToStr(TotalTable[Opt]);
      T.SetColAlignment(i + 1, taCenter);
    end;
  T.Footer.Text := 'Counts indicate number of errors.';
end;

procedure TProjectChecker.DoRecordsReport(const Relation: TEpiMasterRelation;
  const RecordResult: TEpiProjectResultArray);
var
  S: String;
  i: Integer;
  j: Integer;
  ResRecord: TEpiProjectValidateResultRecord;
  Jmp: TEpiJump;
  SortFields: TStrings;
  T: TOutputTable;
  DF: TEpiDataFile;
  F: TEpiField;
begin
  DF := Relation.Datafile;

  i := Low(RecordResult);
  while i <= High(RecordResult) do
  begin
    T  := FOutputCreator.AddTable;
    T.ColCount := 2;
    T.Header.Text := 'Record no: ' + IntToStr(RecordResult[i].RecNo + 1);

    if Assigned(DF.KeyFields) and
       (DF.KeyFields.Count > 0)
    then
      begin
        T.RowCount := T.RowCount + 1;
        T.Cell[0, T.RowCount - 1].Text := 'List by Fields:';
        T.SetRowBorders(T.RowCount - 1, [cbTop]);

        S := '';
        for F in DF.KeyFields do
          S += F.Name + ' = ' + F.AsString[RecordResult[i].RecNo];

        T.Cell[1, T.RowCount - 1].Text := S;
      end;

    repeat
      ResRecord := RecordResult[i];
      T.RowCount := T.RowCount + 1;
      T.Cell[0, T.RowCount - 1].Text := ResRecord.Field.GetVariableLabel(FVariableLabelFormatType);

      case ResRecord.FailedCheck of
        pvCheckSystemMissing:
          S := 'System Missing';
        pvCheckMustEnter:
          S := 'Must Enter has system missing';
        pvCheckKeyFields:
          S := 'Key Field has system missing';
        pvCheckDataRange:
          with ResRecord do
          begin
            S := 'Value = ' + Field.GetValueLabel(RecNo, FValuelabelFormatType) + ', ';
            if Assigned(Field.Ranges) and
               (not Field.Ranges.InRange(Field.AsValue[RecNo]))
            then
              S += LineEnding + Format('Not in range = (%s, %s) ',
                [Field.Ranges[0].AsString[true],
                 Field.Ranges[0].AsString[false]]
              );

            if Assigned(Field.ValueLabelSet) and
               (not Field.ValueLabelSet.ValueLabelExists[Field.AsValue[RecNo]])
            then
              S += LineEnding + 'Not a valid value label!'
          end;
        pvCheckComparison:
          with ResRecord.Field do
            S := Format('Comparison: %s=%s %s %s=%s',
                        [Name, AsString[ResRecord.RecNo],
                         ComparisonTypeToString(Comparison.CompareType),
                         Comparison.CompareField.Name, Comparison.CompareField.AsString[ResRecord.RecNo]
                        ]);
        pvCheckDataLength:
          with ResRecord.Field do
            S := Format('Field length = %d' + LineEnding +
                        'Data length: %d',
                        [Length, UTF8Length(AsString[ResRecord.RecNo])]);
        pvCheckJumpReset:
          // Either a reset value is NOT missing OR a jump back in flow.
          with ResRecord.Field do
          begin
            Jmp := Jumps.JumpFromValue[AsString[ResRecord.RecNo]];
            if not (Jmp.ResetType in [jrMaxMissing, jr2ndMissing]) then
              S := Format('Value = %s, is not a valid jump reset value (MaxMissing or 2ndMaxMissing)!', [AsString[ResRecord.RecNo]])
            else
              S := Format('Value = %s, is a jump backward in flow!', [AsString[ResRecord.RecNo]]);
          end;

        pvCheckKeyData:
          begin
            S := 'Key values not unique';
          end;

        pvCheckRelateData:
          begin
            S := 'No record in parant dataset exists!';
          end;

      else
          S := Format('Report not implemented for ToolCheck: %s',
                      [GetEnumName(TypeInfo(TEpiToolsProjectValidateOption), Integer(ResRecord.FailedCheck))]);
      end;

      T.Cell[1, T.RowCount - 1].Text := S;
      Inc(i);
    until (i > High(RecordResult)) or (RecordResult[i].RecNo <> ResRecord.RecNo);
    T.SetRowBorders(T.RowCount - 1, [cbBottom]);
  end;
end;

procedure TProjectChecker.DoCheckData(ST: TCheckDataCommand);
var
  KeyList: TStrings;
  DF: TEpiDataFile;
  T: TOutputTable;
  i: Integer;
  ProjectValidator: TEpiProjectValidationTool;
  MR: TEpiMasterRelation;
begin
  FCheckDataST := ST;
  KeyList := ST.VariableList.GetIdentsAsList;
  DF := FExecutor.PrepareDatafile(KeyList, nil);
  MR := TEpiMasterRelation.Create(nil);
  MR.Datafile := DF;

  FCheckDataOptions := [pvCheckComparison, pvCheckDataLength, pvCheckDataRange, pvCheckJumpReset, pvCheckMustEnter];
  if ST.HasOption('sysmis') then
    Include(FCheckDataOptions, pvCheckSystemMissing);

  ProjectValidator := TEpiProjectValidationTool.Create;
  ProjectValidator.OnDataFileResult := @CheckDataReport;
  ProjectValidator.ValidateDatafile(MR, FCheckDataOptions);
  ProjectValidator.Free;

  KeyList.Free;
  DF.Free;
  MR.Free;
  FCheckDataST := nil;
end;

procedure TProjectChecker.DoCheckRelate(ST: TCheckRelateCommand);
var
  MR: TEpiMasterRelation;
  DR: TEpiDetailRelation absolute MR;
  ParentDF, DF: TEpiDataFile;
  DFRunner, PFRunner, ChildObsCount, Idx, i, j, SeqNo: Integer;
  CmpResult: TValueSign;
  T: TOutputTable;
  MainSortField, pKF, cKF, ErrorObs, ErrorType, ErrorSeq, F: TEpiField;
  RV, RVObs, RVTyp, RVSeq, RVVars: TExecVarVector;

begin
  DF := FExecutor.PrepareDatafile(Nil, Nil, [pdoAddOrgObsNo]);
  MR := FExecutor.Document.Relations.MasterRelationFromDatafileName(FExecutor.DataFile.Name);
  if (not (MR is TEpiDetailRelation)) then
    begin
      FExecutor.Error('Current Dataset is not a child of any other dataset!');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  if (DF.Size = 0) then
    begin
      FOutputCreator.DoInfoAll('Check Related Data: No Data in current dataset!');
      ST.ExecResult := csrSuccess;
      Exit;
    end;

  MainSortField := DF.Fields.FieldByName[ANA_EXEC_PREPAREDS_OBSNO_FIELD];
  DF.SortRecords(DF.KeyFields);

  ParentDF := TEpiDataFile(DR.MasterRelation.Datafile.Clone(nil));
  ParentDF.SortRecords(ParentDF.KeyFields);

  T := FOutputCreator.AddTable;
  T.ColCount := 2 + DF.KeyFields.Count;
  T.RowCount := 1;

  T.Header.Text := DF.Caption.Text + '(' + DF.Name + ')';
  T.Cell[0, 0].Text := 'Obs. no:';
  T.Cell[1, 0].Text := 'Error';

  I := 2;
  for F in DF.KeyFields do
    T.Cell[PostInc(I), 0].Text := F.GetVariableLabel(FVariableLabelFormatType);

  T.SetRowBorders(0, [cbBottom]);

  DFRunner := 0;
  PFRunner := 0;

  ErrorObs  := TEpiField.CreateField(nil, ftInteger);
  ErrorType := TEpiField.CreateField(nil, ftInteger);
  ErrorSeq  := TEpiField.CreateField(nil, ftInteger);
  SeqNo     := 0;
  ChildObsCount := 0;

  while DFRunner < DF.Size do
  begin
    for pKF in ParentDF.KeyFields do
    begin
      cKF := DF.KeyFields.FieldByName[pKF.Name];
      CompareFieldRecords(CmpResult, pKF, cKF, PFRunner, DFRunner);

      case CmpResult of
        -1: begin
              Inc(PFRunner);
              Break;
            end;

        0:  Continue;

        1:  begin
              ChildObsCount := 1;
              Break;
            end;
      end;
    end;

    if (CmpResult < 0) then
      begin
        if (ChildObsCount > DR.MaxRecordCount) then
          begin
            for i := 0 to ChildObsCount - 1 do
              begin
                //i := 0;
                Idx := T.RowCount;
                T.RowCount := Idx + 1;
                T.Cell[0, Idx].Text := IntToStr(MainSortField.AsInteger[DFRunner - ChildObsCount + i] + 1);
                T.Cell[1, Idx].Text := 'Observation exceeds number of allowed child records!';

                ErrorObs.Size  := ErrorObs.Size + 1;
                ErrorType.Size := ErrorType.Size + 1;
                ErrorSeq.Size  := ErrorSeq.Size + 1;

                ErrorObs.AsInteger[Idx - 1]  := MainSortField.AsInteger[DFRunner - ChildObsCount + i] + 1;
                ErrorType.AsInteger[Idx - 1] := 0;
                ErrorSeq.AsInteger[Idx - 1]  := SeqNo;

                j := 2;
                for F in DF.KeyFields do
                  T.Cell[PostInc(j), Idx].Text := F.GetValueLabel(DFRunner - ChildObsCount + i, FValuelabelFormatType);
              end;
            Inc(SeqNo);
          end;
        ChildObsCount := 0;
      end;


    if (CmpResult = 0) then
      begin
        Inc(ChildObsCount);
        Inc(DFRunner);
      end;

    if (CmpResult > 0) then
      begin
        Idx := T.RowCount;
        T.RowCount := Idx + 1;
        T.Cell[0, Idx].Text := IntToStr(MainSortField.AsInteger[DFRunner] + 1);
        T.Cell[1, Idx].Text := 'Observation has no parent key!';

        ErrorObs.Size  := ErrorObs.Size + 1;
        ErrorType.Size := ErrorType.Size + 1;
        ErrorSeq.Size  := ErrorSeq.Size + 1;

        ErrorObs.AsInteger[Idx - 1]  := MainSortField.AsInteger[DFRunner] + 1;
        ErrorType.AsInteger[Idx - 1] := 1;
        ErrorSeq.AsInteger[Idx - 1]  := SeqNo;

        j := 2;
        for F in DF.KeyFields do
          T.Cell[PostInc(j), Idx].Text := F.GetValueLabel(DFRunner, FValuelabelFormatType);

        ChildObsCount := 0;

        Inc(SeqNo);
        Inc(DFRunner);
      end;
  end;

  T.SetColAlignment(1, taLeftJustify);
  T.Footer.Text := 'Relate Errors Found: ' + IntToStr(ErrorObs.Size);
  T.Footer.Alignment := taLeftJustify;

  RVObs := FExecutor.AddResultVector('$check_relate_obs',       ftInteger, ErrorObs.Size);
  RVTyp := FExecutor.AddResultVector('$check_relate_error',     ftInteger, ErrorType.Size);
  RVSeq := FExecutor.AddResultVector('$check_relate_seqno',     ftInteger, ErrorSeq.Size);

  for i := 0 to ErrorType.Size - 1 do
    begin
      RVObs.AsIntegerVector[i] := ErrorObs.AsInteger[i];
      RVTyp.AsIntegerVector[i] := ErrorType.AsInteger[i];
      RVSeq.AsIntegerVector[i] := ErrorSeq.AsInteger[i];
    end;

  RVVars := FExecutor.AddResultVector('$check_relate_varnames', ftString,  DF.KeyFields.Count);
  i := 0;
  for F in DF.KeyFields do
    RVVars.AsStringVector[PostInc(i)] := F.Name;

  ErrorObs.Free;
  ErrorType.Free;
end;

procedure TProjectChecker.DoCheckKey(ST: TCheckKeyCommand);
var
  FailedRecords, FailedValues, FailedSeqNo: TBoundArray;
  KeyFields: TEpiFields;
  DF: TEpiDataFile;
  KeyList: TStrings;
  S: String;
  T: TOutputTable;
  i, j, DFRunner, EqualKeyCount, L, SeqNo: Integer;
  F, OrgRecsField, LocalRecsField: TEpiField;
  RVObsL, RVObsG, RVObsT, RVVars, RVSeq: TExecVarVector;

  // Return -1 if Index contains a missing value
  // Return  0 if Index and the index before have same values in all KeyFields.
  // Return  1 if Index and the index before differ in at least one KeyField
  function CompareKeys(KeyFields: TEpiFields; Index: Integer): TValueSign;
  var
    F: TEpiField;
  begin
    Result := 0;

    for F in KeyFields do
      if F.IsMissing[Index] then
        Exit(-1)
      else
        if (F.Compare(Index - 1, Index) <> 0) then
          Exit(1);
  end;

  procedure AddError(Index: Integer; FailValue: Integer);
  begin
    L := Length(FailedRecords);
    SetLength(FailedRecords, L + 1);
    SetLength(FailedValues,  L + 1);
    SetLength(FailedSeqNo,  L + 1);

    FailedRecords[L] := Index;
    FailedValues[L]  := FailValue;
    FailedSeqNo[L]   := SeqNo;
  end;

begin
  if ST.VariableList.Count > 0 then
    KeyList := ST.VariableList.GetIdentsAsList
  else
    KeyList := FExecutor.DataFile.KeyFields.GetItemNames(false);

  DF := FExecutor.PrepareDatafile(KeyList, nil, [pdoAddOrgObsNo]);

  if KeyList.Count > 0 then
    begin
      KeyFields := TEpiFields.Create(nil);
      for S in KeyList do
        KeyFields.AddItem(DF.Fields.FieldByName[S]);
    end
  else
    begin
      FExecutor.Error('No variables chosen and no key assigned to dataset!');
      ST.ExecResult := csrFailed;
      KeyList.Free;
      DF.Free;
      Exit;
    end;


  OrgRecsField := DF.Fields.FieldByName[ANA_EXEC_PREPAREDS_OBSNO_FIELD];
  LocalRecsField := DF.NewField(ftInteger);
  for i := 0 to DF.Size - 1 do
    LocalRecsField.AsInteger[i] := i;

  DF.SortRecords(KeyFields);

  SeqNo := 0;
  DFRunner := 1;
  EqualKeyCount := 0;
  while DFRunner < DF.Size do
    begin
      Case CompareKeys(KeyFields, DFRunner) of
        -1:
          begin
            if EqualKeyCount > 0 then
              begin
                for i := 0 to EqualKeyCount do
                  AddError(DFRunner - EqualKeyCount + i - 1, 1);

                Inc(SeqNo);
              end;

            AddError(DFRunner, 0);
            Inc(SeqNo);
            EqualKeyCount := 0;
          end;

        0:
          Inc(EqualKeyCount);

        1:
          begin
            if EqualKeyCount > 0 then
              begin
                for i := 0 to EqualKeyCount do
                  AddError(DFRunner - EqualKeyCount + i - 1, 1);

                Inc(SeqNo);
              end;

            EqualKeyCount := 0;
          end;
      end;

      Inc(DFRunner);
    end;

  if EqualKeyCount > 0 then
    for i := 0 to EqualKeyCount do
      AddError(DFRunner - EqualKeyCount + i - 1, 1);

  if (Length(FailedRecords) = 0) then
    begin
      FOutputCreator.DoInfoAll('No Errors');
      ST.ExecResult := csrSuccess;
      Exit;
    end;

  T := FOutputCreator.AddTable;
  T.Header.Text := DF.Caption.Text + '(' + DF.Name + ')';
  T.ColCount := 3 + KeyList.Count;
  T.RowCount := Length(FailedRecords) + 1;

  RVVars := FExecutor.AddResultVector('$check_key_varnames', ftString,  DF.KeyFields.Count);
  i := 0;
  for F in DF.KeyFields do
    RVVars.AsStringVector[PostInc(i)] := F.Name;

  RVObsL := FExecutor.AddResultVector('$check_key_obs',        ftInteger, Length(FailedRecords));
  RVObsG := FExecutor.AddResultVector('$check_key_obs_global', ftInteger, Length(FailedRecords));
  RVObsT := FExecutor.AddResultVector('$check_key_error',      ftInteger, Length(FailedRecords));
  RVSeq  := FExecutor.AddResultVector('$check_key_seqno',      ftInteger, Length(FailedRecords));

  T.Cell[0, 0].Text := 'Obs. no';
  T.Cell[1, 0].Text := 'Global obs. no';
  T.Cell[2, 0].Text := 'Error';
  i := 3;

  for F in KeyFields do
    T.Cell[PostInc(I), 0].Text := F.GetVariableLabel(FVariableLabelFormatType);

  T.SetRowBorders(0, [cbBottom]);

  for i := Low(FailedRecords) to High(FailedRecords) do
    begin
      T.Cell[0, i + 1].Text := IntToStr(LocalRecsField.AsInteger[FailedRecords[i]] + 1);
      T.Cell[1, i + 1].Text := IntToStr(OrgRecsField.AsInteger[FailedRecords[i]] + 1);

      RVObsL.AsIntegerVector[i] := LocalRecsField.AsInteger[FailedRecords[i]] + 1;
      RVObsG.AsIntegerVector[i] := OrgRecsField.AsInteger[FailedRecords[i]] + 1;
      RVObsT.AsIntegerVector[i] := FailedValues[i];
      RVSeq.AsIntegerVector[i]  := FailedSeqNo[i];

      if FailedValues[i] = 1 then
        T.Cell[2, i + 1].Text := 'Non unique!'
      else
        T.Cell[2, i + 1].Text := 'Sys. Missing';

      j := 3;
      for F in KeyFields do
        T.Cell[PostInc(j), i + 1].Text := F.GetValueLabel(FailedRecords[i], FValuelabelFormatType);
    end;
  T.Footer.Text := 'Key Errors Found: ' + IntToStr(Length(FailedRecords));
  T.Footer.Alignment := taLeftJustify;
end;

procedure TProjectChecker.DoCheckStudy(ST: TCheckStudyCommand);
var
  T: TOutputTable;
  HasError: Boolean;

  procedure NewStudyRecord(Study: String);
  var
    Idx: Integer;
  begin
    Idx := T.RowCount;
    T.RowCount := Idx + 1;
    T.Cell[0, Idx].Text := Study;
    HasError := true;
  end;

begin
  T := FOutputCreator.AddTable;
  T.Header.Text := 'Study information:';
  T.ColCount := 1;
  T.RowCount := 1;
  T.Cell[0,0].Text := 'No Study Information for:';
  T.SetRowBorders(0, [cbBottom]);
  HasError := false;

  with FExecutor.Document.Study do
  begin
    if AbstractText.Text = ''         then NewStudyRecord('Abstract');
    if Author = ''                    then NewStudyRecord('Author');
    if Agency = ''                    then NewStudyRecord('Agency');
    if Citations.Text = ''            then NewStudyRecord('Citations');
    if (DataCollectionStart = MaxDateTime) or
       (DataCollectionEnd = MaxDateTime)
    then
      NewStudyRecord('Data Time Coverage');
    if Design.Text = ''               then NewStudyRecord('Design');
    if Funding.Text = ''              then NewStudyRecord('Funding');
    if GeographicalCoverage.Text = '' then NewStudyRecord('Geographical Coverage');
    if Publisher.Text = ''            then NewStudyRecord('Publisher');
    if Purpose.Text = ''              then NewStudyRecord('Purpose');
    if Population.Text = ''           then NewStudyRecord('Population');
    if Rights.Text = ''               then NewStudyRecord('Rights');
    if Title.Text = ''                then NewStudyRecord('Title');
    if Keywords = ''                  then NewStudyRecord('Keywords');
    if UnitOfObservation.Text = ''    then NewStudyRecord('Unit of obs.');
  end;

  FExecutor.AddResultConst('$check_study_errors', ftInteger).AsIntegerVector[0] := T.RowCount - 1;

  T.SetColAlignment(0, taLeftJustify);
  if (not HasError) then
    begin
     T.Cell[0,0].Text := 'No errors found';
     T.SetRowBorders(0, []);
    end;
end;

constructor TProjectChecker.Create(AExecutor: TExecutor;
  AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

procedure TProjectChecker.Check(ST: TCustomCheckCommand);
begin
  FVariableLabelFormatType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValuelabelFormatType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  case ST.SubCmd of
    cscRelate:
      DoCheckRelate(TCheckRelateCommand(ST));

    cscKey:
      DoCheckKey(TCheckKeyCommand(ST));

    cscStudy:
      DoCheckStudy(TCheckStudyCommand(ST));

    cscData:
      DoCheckData(TCheckDataCommand(ST));
  end;
end;

end.

