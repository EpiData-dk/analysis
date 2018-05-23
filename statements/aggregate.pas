unit aggregate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidatafiles, executor, outputcreator, ast,
  result_variables, epicustombase, aggregate_types, epifields_helper,
  epidatafilerelations;

type

  { TAggregateDatafile }


  TAggregate = class(TObject)
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FResultDataFileClass: TEpiDataFileClass;
    procedure AddFieldChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure DoCaptionHeadersAndLabels(ResultDF: TEpiDataFile; FunctionList: TAggrFuncList; ST: TAggregateCommand);
  protected
    // Takes the option and creates function(s) if the idens matches a function, return true - else return false
    function  OptionToFunctions(InputDF: TEpiDataFile; Opt: TOption; CreateCountFunction: boolean; FunctionList: TAggrFuncList): boolean; virtual;
    function  DoCalcAggregate(InputDF: TEpiDataFile; Variables: TStrings; FunctionList: TAggrFuncList; Out RefMap: TEpiReferenceMap): TEpiDataFile;
    // AggregateVariables is a list of each stratification variable aggregated by itself,
    // such that all unique values for that variable is present. This has the implication
    // that the size of the AggregateVariables may vary.
    procedure DoExpandDatafile(InputDF, ResultDF: TEpiDataFile; Variables: TStrings; Out AggregateVariables: TEpiFields);
    procedure DoOutputAggregate(ResultDF: TEpiDataFile; ST: TAggregateCommand);
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator); virtual;
    destructor Destroy; override;

    // If Assigned, the resulting datafile from the aggregate will be of this class
    property  ResultDataFileClass: TEpiDataFileClass read FResultDataFileClass write FResultDataFileClass;

    // Method called from Executor, does calculation + result vars + output
    procedure ExecAggregate(InputDF: TEpiDataFile; ST: TAggregateCommand; Out ResultDF: TEpiDataFile);

    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    // - InputDf:
    //     The datafile to run aggregate on
    // - ByVariables:
    //     The variable names to stratify by
    // - FunctionList:
    //     The list of aggregate functions.
    // - ExpandedDatafile:
    //     The result datafile will contain ALL combinations of the ByVariables, even if no such entries exists.
    // - out AggregateVariable:
    //     See DoExpandDatafile() above!
    // - out RefMap:
    //     During aggregate the ByVariables are cloned to the resulting data, this is the RefMap from the cloning.
    //     It has NOT fixed the references (fixupReferences)
    function  CalcAggregate(InputDF: TEpiDataFile; ByVariables: TStrings; FunctionList: TAggrFuncList; ExpandedDataFile: boolean;
      Out AggregateVariables: TEpiFields; Out RefMap: TEpiReferenceMap): TEpiDataFile;
  end;

implementation

uses
  epicustomlist_helper, options_utils, epidatafileutils;

{ TAggregate }

procedure TAggregate.AddFieldChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  Fields: TEpiFields absolute Sender;
  F, PrevF: TEpiField;
begin
  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceAddItem) then exit;

  if (Fields.Count = 1) then
    begin
      Fields[0].Left := 200;
      Fields[0].Top  := 30;
      Exit;
    end;

  PrevF := Fields[Fields.Count - 2];
  F := Fields[Fields.Count - 1];
  F.Left := 50;
  F.Top := PrevF.Top + 30;
end;

procedure TAggregate.DoCaptionHeadersAndLabels(ResultDF: TEpiDataFile;
  FunctionList: TAggrFuncList; ST: TAggregateCommand);
var
  Opt: TOption;
  GV: TExecVarGlobalVector;
  F: TEpiField;
  I, Decimals: Integer;
begin
  if (ST.HasOption('caption', opt)) then
    ResultDF.Caption.Text := Opt.Expr.AsString
  else
    ResultDF.Caption.Text := 'Aggregated Table';

  Decimals     := DecimalFromOption(ST.Options);
  FunctionList.UpdateAllResultLabels(Decimals);

  if (ST.HasOption('h', Opt)) then
    begin
      // If not value is assigned to !headers - this is a flag to delete all variable labels
      if (not Assigned(Opt.Expr)) then
        begin
          for F in ResultDF.Fields do
            F.Question.Text := '';
          Exit;
        end;

      GV := TExecVarGlobalVector(FExecutor.GetExecDataVariable(Opt.Expr.AsIdent));
      I := 0;
      for F in ResultDF.Fields do
        begin
          if (I >= GV.Length) then
            break;

          F.Question.Text := GV.AsStringVector[I];
          Inc(I);
        end;
    end;
end;

function TAggregate.OptionToFunctions(InputDF: TEpiDataFile; Opt: TOption;
  CreateCountFunction: boolean; FunctionList: TAggrFuncList): boolean;
var
  S: UTF8String;
  AggregateVariable: TEpiField;
begin
  if (not (Assigned(Opt.Expr))) then
    Exit(false);

  S := Opt.Expr.AsIdent;
  AggregateVariable := InputDF.Fields.FieldByName[S];

  if (not Assigned(AggregateVariable)) then
    Exit(false);

  // Create a count vector for the aggregation variable. But only create one!
  if (CreateCountFunction) and
     (FunctionList.IndexOf('N' + AggregateVariable.Name) = -1)
  then
    FunctionList.Add(TAggrCount.Create('N'  + AggregateVariable.Name, AggregateVariable, acNotMissing));

  Result := true;
  case Opt.Ident of
    'des':
      begin
        FunctionList.Add(TAggrMinMax.Create('MIN' + AggregateVariable.Name, AggregateVariable, true));
        FunctionList.Add(TAggrPercentile.Create('MED' + AggregateVariable.Name, AggregateVariable, ap50));
        FunctionList.Add(TAggrMinMax.Create('MAX' + AggregateVariable.Name, AggregateVariable, false));
      end;

    'iqr':
      begin
        FunctionList.Add(TAggrPercentile.Create('P25' + AggregateVariable.Name, AggregateVariable, ap25));
        FunctionList.Add(TAggrPercentile.Create('P75' + AggregateVariable.Name, AggregateVariable, ap75));
      end;

    'idr':
      begin
        FunctionList.Add(TAggrPercentile.Create('P10' + AggregateVariable.Name, AggregateVariable, ap10));
        FunctionList.Add(TAggrPercentile.Create('P90' + AggregateVariable.Name, AggregateVariable, ap90));
      end;

    'isr':
      begin
        FunctionList.Add(TAggrPercentile.Create('P5',  AggregateVariable, ap5));
        FunctionList.Add(TAggrPercentile.Create('P95' + AggregateVariable.Name, AggregateVariable, ap95));
      end;

    'mci':
      FunctionList.Add(TAggrMean.Create('MEAN' + AggregateVariable.Name, AggregateVariable, amMCI));

    'mv':
      begin
        FunctionList.Add(TAggrCount.Create('MIS' + AggregateVariable.Name, AggregateVariable, acMissing));
        FunctionList.Add(TAggrCount.Create('MVD' + AggregateVariable.Name, AggregateVariable, acMissingValue));
      end;

    'mean':
      FunctionList.Add(TAggrMean.Create('MEAN' + AggregateVariable.Name, AggregateVariable, amMean));

    'sd':
      FunctionList.Add(TAggrMean.Create('SD' + AggregateVariable.Name, AggregateVariable, amStdDev));

    'sv':
      FunctionList.Add(TAggrMean.Create('SV' + AggregateVariable.Name, AggregateVariable, amStdVar));

    'min':
      FunctionList.Add(TAggrMinMax.Create('MIN' + AggregateVariable.Name, AggregateVariable, True));

    'p1':
      FunctionList.Add(TAggrPercentile.Create('P1' + AggregateVariable.Name, AggregateVariable, ap1));

    'p5':
      FunctionList.Add(TAggrPercentile.Create('P5' + AggregateVariable.Name, AggregateVariable, ap5));

    'p10':
      FunctionList.Add(TAggrPercentile.Create('P10' + AggregateVariable.Name, AggregateVariable, ap10));

    'p25':
      FunctionList.Add(TAggrPercentile.Create('P25' + AggregateVariable.Name, AggregateVariable, ap25));

    'p50',
    'med':
      FunctionList.Add(TAggrPercentile.Create('MED' + AggregateVariable.Name, AggregateVariable, ap50));

    'p75':
      FunctionList.Add(TAggrPercentile.Create('P75' + AggregateVariable.Name, AggregateVariable, ap75));

    'p90':
      FunctionList.Add(TAggrPercentile.Create('P90' + AggregateVariable.Name, AggregateVariable, ap90));

    'p95':
      FunctionList.Add(TAggrPercentile.Create('P95' + AggregateVariable.Name, AggregateVariable, ap95));

    'p99':
      FunctionList.Add(TAggrPercentile.Create('P99' + AggregateVariable.Name, AggregateVariable, ap99));

    'max':
      FunctionList.Add(TAggrMinMax.Create('MAX' + AggregateVariable.Name, AggregateVariable, False));

    'sum':
      FunctionList.Add(TAggrSum.Create('SUM' + AggregateVariable.Name, AggregateVariable));

  else
    Result := false
  end;
end;

function TAggregate.DoCalcAggregate(InputDF: TEpiDataFile; Variables: TStrings;
  FunctionList: TAggrFuncList; out RefMap: TEpiReferenceMap): TEpiDataFile;
var
  SortList, CountVariables: TEpiFields;
  V: String;
  F, NewF: TEpiField;
  Func: TAggrFunc;
  PercList, TempFuncList: TAggrFuncList;
  Runner, i, Index: Integer;

begin
  if (Variables.Count > 0) then
    begin
      SortList := TEpiFields(InputDF.Fields.GetItemFromList(Variables));
      InputDF.SortRecords(SortList);
    end
  else
    SortList := TEpiFields.Create(nil);

  // Create the resulting datafile
  if (Assigned(ResultDataFileClass)) then
    Result := ResultDataFileClass.Create(nil, 0)
  else
    Result := TEpiDataFile.Create(nil, 0);
  Result.SetLanguage(InputDF.DefaultLang, true);

  // The AddFieldChange makes sure that each new added variable is positioned sequentially
  // after eachother.
  Result.Fields.RegisterOnChangeHook(@AddFieldChange, True);

  // Create a clone of the stratefication variables
  CountVariables := TEpiFields.Create(nil);
  RefMap := TEpiReferenceMap.Create;
  for V in Variables do
    begin
      F := InputDF.Fields.FieldByName[V];
      NewF := TEpiField(F.Clone(nil, RefMap));
      // Manually assign the valuelabelset, such that RefMap only needs to be run
      // in case more advanced assignments are needed
      NewF.ValueLabelSet := F.ValueLabelSet;
      Result.MainSection.Fields.AddItem(NewF);
      CountVariables.AddItem(NewF);
    end;

  // Let each function create the result variables they need
  for i := 0 to FunctionList.Count - 1 do
    begin
      Func := FunctionList[i];
      Func.CreateResultVector(Result);
    end;
  Result.Fields.UnRegisterOnChangeHook(@AddFieldChange);

  // First run through "normal" statistic. Those that don't need a particular sort order.
  TempFuncList := FunctionList.GetFunctions(AllAggrFuncTypes - [afPercentile]);

  // Do "normal" aggregate operations (this is without any percentile calc.)
  Runner := 0;
  while Runner < InputDF.Size do
    begin
      if (Runner > 0 ) and
         (SortList.CompareRecords(Runner, Runner - 1) <> 0)
      then
        begin
          Index := Result.NewRecords();

          for F in CountVariables do
            F.CopyValue(InputDF.Fields.FieldByName[F.Name], Runner - 1, Index);

          TempFuncList.SetOutputs(Index);
          TempFuncList.ResetAll;
        end;

      for i := 0 to TempFuncList.Count - 1 do
        TempFuncList[i].Execute(Runner);

      Inc(Runner);
    end;
  Index := Result.NewRecords();
  for F in CountVariables do
    F.CopyValue(InputDF.Fields.FieldByName[F.Name], Runner - 1, Index);
  TempFuncList.SetOutputs(Index);
  TempFuncList.ResetAll;
  TempFuncList.Free;

  // Now run through each percentile calculation, since they need sorting
  TempFuncList := FunctionList.GetFunctions([afPercentile]);
  for i := 0 to TempFuncList.Count - 1 do
    begin
      Func := TempFuncList[i];

      // Sort by "normal" variables AND the current percentile vector
      // - but only add the percentile vector if it is not already a by-variable
      if (not SortList.FieldExists(Func.AggregateVector)) then
        begin
          SortList.AddItem(Func.AggregateVector);
          InputDF.SortRecords(SortList);
          SortList.RemoveItem(Func.AggregateVector);
        end
      else
        InputDF.SortRecords(SortList);

      Runner := 0;
      Index := 0;
      while Runner < InputDF.Size do
        begin
          if (Runner > 0 ) and
             (SortList.CompareRecords(Runner, Runner - 1) <> 0)
          then
            begin
              Func.SetOutput(Index);
              Func.Reset();
              Inc(Index);
            end;
          Func.Execute(Runner);
          Inc(Runner);
        end;

      Func.SetOutput(Index);
      Func.Reset();
    end;
  TempFuncList.Free;
end;

procedure TAggregate.DoExpandDatafile(InputDF, ResultDF: TEpiDataFile;
  Variables: TStrings; out AggregateVariables: TEpiFields);
var
  V: TStringList;
  S: String;
  FuncList: TAggrFuncList;
  RefMap: TEpiReferenceMap;
  TempDF: TEpiDataFile;
  SortFields: TEpiFields;
  Runners: TBoundArray;
  ResultIdx, i, ResultSize, Idx: Integer;
  InputVar, ResultVar: TEpiField;


  function ProperLevelChange(): Boolean;
  var
    InputVar, ResultVar: TEpiField;
    i: Integer;
    Cmp: TValueSign;
  begin
    result := true;

    i := 0;
    for InputVar in AggregateVariables do
      begin
        ResultVar := ResultDF.Fields.FieldByName[InputVar.Name];

        CompareFieldRecords(Cmp, InputVar, ResultVar, Runners[i], ResultIdx);
        result := result and (Cmp = ZeroValue);
        Inc(i);
      end;
  end;

begin
  AggregateVariables := TEpiFields.Create(nil);
  AggregateVariables.SetLanguage(InputDF.DefaultLang, true);
  AggregateVariables.ItemOwner := true;
  V := TStringList.Create;
  for S in Variables do
    begin
      V.Clear;
      V.Add(S);

      FuncList := TAggrFuncList.Create(true);
      FuncList.Add(TAggrCount.Create('n', nil, acAll));
      TempDF := DoCalcAggregate(InputDF, V, FuncList, RefMap);
      RefMap.Free;
      FuncList.Free;

      AggregateVariables.AddItem(TempDF.Fields[0].Section.Fields.DeleteItem(0));
      TempDF.Free;
    end;
  V.Free;

  // Runners - indices into the variables stored in "AggregateVariables".
  // Used to run sequencial from Last -> First
  SetLength(Runners, AggregateVariables.Count);
  for i := Low(Runners) to High(Runners) do
    Runners[i] := 0;

  // Idea:
  //   Loop over the entire dataset.
  //   Compare each entry of ResultDF's stratification variables with the
  //   entries in "AggregateVariables". If an entry is missing, then add a new observation
  //   and copy the value. All additional data is set to missing.
  ResultIdx := 0;
  ResultSize := ResultDF.Size;
  while ResultIdx < ResultSize do
    begin
      // Check that levels have change properly
      if (not ProperLevelChange()) then
        begin
          // This was not the case, then we must insert the missing record into the
          // resulting dataset.
          Idx := ResultDF.NewRecords();

          i := 0;
          for InputVar in AggregateVariables do
            begin
              ResultVar := ResultDF.Fields.FieldByName[InputVar.Name];
              ResultVar.CopyValue(InputVar, Runners[i], Idx);
              Inc(i);
            end;
        end
      else
        // The level change was accepted - bump index on ResultDF
        Inc(ResultIdx);

      // Bump the runners sequencially (last -> first)
      for i := High(Runners) downto Low(Runners) do
        begin
          Runners[i] := Runners[i] + 1;

          // Have we reach "end-of-size" for current runner?
          if (Runners[i] < AggregateVariables[i].Size) then
            break;

          // Yes - then reset it back to 0
          Runners[i] := 0;
        end;
    end;

  // Check for missing obs. at the end of aggregate variables
  while (Runners[0] > 0) do
    begin
      Idx := ResultDF.NewRecords();

      i := 0;
      for InputVar in AggregateVariables do
        begin
          ResultVar := ResultDF.Fields.FieldByName[InputVar.Name];
          ResultVar.CopyValue(InputVar, Runners[i], Idx);
          Inc(i);
        end;

      // Bump the runners sequencially (last -> first)
      for i := High(Runners) downto Low(Runners) do
        begin
          Runners[i] := Runners[i] + 1;

          // Have we reach "end-of-size" for current runner?
          if (Runners[i] < AggregateVariables[i].Size) then
            break;

          // Yes - then reset it back to 0
          Runners[i] := 0;
        end;
    end;

  SortFields := TEpiFields(ResultDF.Fields.GetItemFromList(Variables));
  ResultDF.SortRecords(SortFields);
  SortFields.Free;
end;

procedure TAggregate.DoOutputAggregate(ResultDF: TEpiDataFile;
  ST: TAggregateCommand);
var
  T: TOutputTable;
  Col, Row: Integer;
  Opt: TOption;
  ValueLabelType: TEpiGetValueLabelType;
  GV: TExecVarGlobalVector;
  VariableLabelType: TEpiGetVariableLabelType;
begin
  T := FOutputCreator.AddTable;
  T.ColCount := ResultDF.Fields.Count;
  T.RowCount := ResultDF.Size + 1;
  T.Header.Text := ResultDF.Caption.Text;

  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  for Col := 0 to ResultDF.Fields.Count - 1 do
    T.Cell[Col, 0].Text := ResultDF.Field[Col].GetVariableLabel(VariableLabelType);

  T.SetRowBorders(0, [cbTop, cbBottom]);

  ValueLabelType := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  for Row := 0 to ResultDF.Size - 1 do
    for Col := 0 to ResultDF.Fields.Count - 1 do
      T.Cell[Col, Row + 1].Text := ResultDF.Field[Col].GetValueLabelFormatted(Row, ValueLabelType);
  T.SetRowBorders(T.RowCount - 1, [cbBottom]);
end;

constructor TAggregate.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
end;

destructor TAggregate.Destroy;
begin
  inherited Destroy;
end;

procedure TAggregate.ExecAggregate(InputDF: TEpiDataFile;
  ST: TAggregateCommand; out ResultDF: TEpiDataFile);
var
  VarNames: TStrings;
  Opt: TOption;
  FunctionList: TAggrFuncList;
  i: Integer;
  F: TEpiField;
  RefMap: TEpiReferenceMap;
  CreateSumOnStatFunctions, PrevModified: Boolean;
  MR: TEpiMasterRelation;
  DF: TEpiCustomItem;
begin
  VarNames := ST.VariableList.GetIdentsAsList;
  CreateSumOnStatFunctions := (not ST.HasOption('nc'));

  try
    FunctionList := TAggrFuncList.Create(true);

    if (not ST.HasOption('nt')) then
      FunctionList.Add(TAggrCount.Create('N', nil, acAll));

    for i := 0 to ST.Options.Count - 1 do
      begin
        Opt := ST.Options[i];

        // If this options is a function, create it/them and continue to next option.
        if OptionToFunctions(InputDF, Opt, CreateSumOnStatFunctions, FunctionList) then
          Continue;
      end;
  except
    On E: EAggrFunc do
      begin
        FOutputCreator.DoError(E.Message);
        FunctionList.Free;
        ST.ExecResult := csrFailed;
        Exit;
      end;

    else
      Raise;
  end;

  if (FunctionList.Count = 0) then
    begin
      FExecutor.Error('No aggregate functions selected!');
      ST.ExecResult := csrFailed;
      FunctionList.Free;
      Exit;
    end;

  ResultDF := DoCalcAggregate(InputDF, Varnames, FunctionList, RefMap);
  PrevModified := FExecutor.Document.Modified;
  FExecutor.Document.DataFiles.AddItem(ResultDF);
  RefMap.FixupReferences;
  RefMap.Free;

  DoCaptionHeadersAndLabels(ResultDF, FunctionList, ST);


  if (not ST.HasOption('q')) then
    DoOutputAggregate(ResultDF, ST);

  if (ST.HasOption('ds', Opt)) then
    begin
      DF := FExecutor.Document.DataFiles.GetItemByName(Opt.Expr.AsIdent);
      if (Assigned(DF)) then
        DF.Free;

      MR := FExecutor.Document.Relations.NewMasterRelation;
      MR.Datafile := ResultDF;
      ResultDF.Name := Opt.Expr.AsIdent;
      FExecutor.UpdateDatasetResultVar;
    end
  else
    begin
      FreeAndNil(ResultDF);
      FExecutor.Document.Modified := PrevModified;
    end;
end;

function TAggregate.CalcAggregate(InputDF: TEpiDataFile; ByVariables: TStrings;
  FunctionList: TAggrFuncList; ExpandedDataFile: boolean; out
  AggregateVariables: TEpiFields; out RefMap: TEpiReferenceMap): TEpiDataFile;
begin
  Result := DoCalcAggregate(InputDF, ByVariables, FunctionList, RefMap);

  AggregateVariables := nil;
  if ExpandedDataFile then
    DoExpandDatafile(InputDF, Result, ByVariables, AggregateVariables);
end;

end.

