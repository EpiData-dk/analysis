unit aggregate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, executor, outputcreator, ast, epicustombase,
  aggregate_types, epifields_helper;

type

  { TAggregateDatafile }

  TAggregateDatafile = class(TEpiDataFile)
  private
    procedure AddFieldChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
  end;

  TAggregate = class(TObject)
  private
    FVariableLabelType: TEpiGetVariableLabelType;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  protected
    // Takes the option and creates function(s) if the idens matches a function, return true - else return false
    function OptionToFunctions(InputDF: TEpiDataFile; Opt: TOption; FunctionList: TAggrFuncList): boolean; virtual;
    function DoCalcAggregate(InputDF: TEpiDataFile; Variables: TStrings; FunctionList: TAggrFuncList; Out RefMap: TEpiReferenceMap): TAggregateDatafile;
    procedure DoOutputAggregate(ResultDF: TAggregateDatafile; ST: TAggregateCommand);
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator); virtual;
    destructor Destroy; override;

    // Method called from Executor, does calculation + result vars + output
    procedure ExecAggregate(InputDF: TEpiDataFile; ST: TAggregateCommand);

    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function  CalcAggregate(InputDF: TEpiDataFile{; FunctionList: TAggrFuncList}): TAggregateDatafile;
    property  VariableLabelType: TEpiGetVariableLabelType read FVariableLabelType write FVariableLabelType;
  end;

implementation

uses
  epicustomlist_helper;

{ TAggregateDatafile }

procedure TAggregateDatafile.AddFieldChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
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

constructor TAggregateDatafile.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);
  Fields.RegisterOnChangeHook(@AddFieldChange, true);
end;

{ TAggregate }


function TAggregate.OptionToFunctions(InputDF: TEpiDataFile; Opt: TOption;
  FunctionList: TAggrFuncList): boolean;
var
  S: UTF8String;
  AggregateVariable: TEpiField;
begin
  if (not (Assigned(Opt.Expr))) then
    Exit(false);

  S := Opt.Expr.AsIdent;
  AggregateVariable := InputDF.Fields.FieldByName[S];

  // Create a count vector for the aggregation variable. But only create one!
  if (FunctionList.IndexOf('N' + AggregateVariable.Name) = -1) then
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
  FunctionList: TAggrFuncList; out RefMap: TEpiReferenceMap
  ): TAggregateDatafile;
var
  SortList, CountVariables: TEpiFields;
  V: String;
  F, NewF: TEpiField;
  Func: TAggrFunc;
  PercList: TAggrFuncList;
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
  Result := TAggregateDatafile.Create(nil);
  Result.SetLanguage(InputDF.DefaultLang, true);

  // Create a copy of the variables
  CountVariables := TEpiFields.Create(nil);
  RefMap := TEpiReferenceMap.Create;
  for V in Variables do
    begin
      F := InputDF.Fields.FieldByName[V];
      NewF := TEpiField(F.Clone(nil, RefMap));
      Result.MainSection.Fields.AddItem(NewF);
      CountVariables.AddItem(NewF);
    end;

  for i := 0 to FunctionList.Count - 1 do
    begin
      Func := FunctionList[i];
      Func.CreateResultVector(Result, VariableLabelType);
    end;

  // Percentile calculations not correct yet... :(
  PercList := FunctionList.ExtractPercentiles();

  // AGGREGATE!!! Yeeeha.
  // Do "normal" aggregate operations (this means without any percentile calc.
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

          FunctionList.SetOutputs(Index);
          FunctionList.ResetAll;
        end;

      for i := 0 to FunctionList.Count - 1 do
        FunctionList[i].Execute(Runner);

      Inc(Runner);
    end;
  Index := Result.NewRecords();
  for F in CountVariables do
    F.CopyValue(InputDF.Fields.FieldByName[F.Name], Runner - 1, Index);
  FunctionList.SetOutputs(Index);
  FunctionList.ResetAll;

  for i := 0 to PercList.Count - 1 do
    begin
      Func := PercList[i];

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
end;

procedure TAggregate.DoOutputAggregate(ResultDF: TAggregateDatafile;
  ST: TAggregateCommand);
var
  T: TOutputTable;
  Col, Row: Integer;
begin
  T := FOutputCreator.AddTable;
  T.ColCount := ResultDF.Fields.Count;
  T.RowCount := ResultDF.Size + 1;
  T.Header.Text := 'AGGREGATE TABLE (PROTOTYPE)';
  T.Footer.Text := 'This is NOT a final version, but at least it outputs something...';

  for Col := 0 to ResultDF.Fields.Count - 1 do
    T.Cell[Col, 0].Text := ResultDF.Field[Col].GetVariableLabel();
  T.SetRowBorders(0, [cbTop, cbBottom]);

  for Row := 0 to ResultDF.Size - 1 do
    for Col := 0 to ResultDF.Fields.Count - 1 do
      T.Cell[Col, Row + 1].Text := ResultDF.Field[Col].GetValueLabel(Row);
  T.SetRowBorders(T.RowCount - 1, [cbBottom]);
end;

constructor TAggregate.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
  FVariableLabelType := gvtVarName;
end;

destructor TAggregate.Destroy;
begin
  inherited Destroy;
end;

procedure TAggregate.ExecAggregate(InputDF: TEpiDataFile; ST: TAggregateCommand
  );
var
  VarNames: TStrings;
  Opt: TOption;
  FunctionList: TAggrFuncList;
  i: Integer;
  F: TEpiField;
  TempDF: TAggregateDatafile;
  RefMap: TEpiReferenceMap;
begin
  VarNames := ST.VariableList.GetIdentsAsList;

  try
    FunctionList := TAggrFuncList.Create;
    FunctionList.Add(TAggrCount.Create('N', nil, acAll));

    for i := 0 to ST.Options.Count - 1 do
      begin
        Opt := ST.Options[i];

        // If this options is a function, create it/them and continue to next option.
        if OptionToFunctions(InputDF, Opt, FunctionList) then
          Continue;

        //TODO: Check for !by options
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

  TempDF := DoCalcAggregate(InputDF, Varnames, FunctionList, RefMap);

  FExecutor.Document.DataFiles.AddItem(TempDF);
  RefMap.FixupReferences;
  RefMap.Free;

  DoOutputAggregate(TempDF, ST);
  TempDF.Free;
end;

function TAggregate.CalcAggregate(InputDF: TEpiDataFile): TAggregateDatafile;
begin

end;

end.

