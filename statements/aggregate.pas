unit aggregate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, executor, outputcreator, ast, epicustombase,
  aggregate_types, epifields_helper;

type

  { TAggregateDatafile }

  TAggregateDatafile = class(TEpiDataFile)
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
    function DoCalcAggregate(InputDF: TEpiDataFile; Variables: TStrings; FunctionList: TAggrFuncList): TAggregateDatafile;
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

constructor TAggregateDatafile.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);
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

  Result := true;
  case Opt.Ident of
    'des':
      begin
        FunctionList.Add(TAggrMinMax.Create('MIN', AggregateVariable, true));
        FunctionList.Add(TAggrPercentile.Create('MED', AggregateVariable, ap50));
        FunctionList.Add(TAggrMinMax.Create('MAX', AggregateVariable, false));
      end;

    'iqr':
      begin
        FunctionList.Add(TAggrPercentile.Create('P25', AggregateVariable, ap25));
        FunctionList.Add(TAggrPercentile.Create('P75', AggregateVariable, ap75));
      end;

    'idr':
      begin
        FunctionList.Add(TAggrPercentile.Create('P10', AggregateVariable, ap10));
        FunctionList.Add(TAggrPercentile.Create('P90', AggregateVariable, ap90));
      end;

    'isr':
      begin
        FunctionList.Add(TAggrPercentile.Create('P5',  AggregateVariable, ap5));
        FunctionList.Add(TAggrPercentile.Create('P95', AggregateVariable, ap95));
      end;

    'mci':
      FunctionList.Add(TAggrMean.Create('MEAN', AggregateVariable, amMCI));

    'mv':
      begin
        FunctionList.Add(TAggrCount.Create('MIS', AggregateVariable, acMissing));
        FunctionList.Add(TAggrCount.Create('MVD', AggregateVariable, acMissingValue));
      end;

    'mean':
      FunctionList.Add(TAggrMean.Create('MEAN', AggregateVariable, amMean));

    'sd':
      FunctionList.Add(TAggrMean.Create('SD', AggregateVariable, amStdDev));

    'sv':
      FunctionList.Add(TAggrMean.Create('SV', AggregateVariable, amStdVar));

    'min':
      FunctionList.Add(TAggrMinMax.Create('MIN', AggregateVariable, True));

    'p1':
      FunctionList.Add(TAggrPercentile.Create('P1', AggregateVariable, ap1));

    'p5':
      FunctionList.Add(TAggrPercentile.Create('P%', AggregateVariable, ap5));

    'p10':
      FunctionList.Add(TAggrPercentile.Create('P10', AggregateVariable, ap10));

    'p25':
      FunctionList.Add(TAggrPercentile.Create('P25', AggregateVariable, ap25));

    'p50',
    'med':
      FunctionList.Add(TAggrPercentile.Create('MED', AggregateVariable, ap50));

    'p75':
      FunctionList.Add(TAggrPercentile.Create('P75', AggregateVariable, ap75));

    'p90':
      FunctionList.Add(TAggrPercentile.Create('P90', AggregateVariable, ap90));

    'p95':
      FunctionList.Add(TAggrPercentile.Create('P95', AggregateVariable, ap95));

    'p99':
      FunctionList.Add(TAggrPercentile.Create('P99', AggregateVariable, ap99));

    'max':
      FunctionList.Add(TAggrMinMax.Create('MAX', AggregateVariable, True));

    'sum':
      FunctionList.Add(TAggrSum.Create('SUM', AggregateVariable));

  else
    Result := false
  end;
end;

function TAggregate.DoCalcAggregate(InputDF: TEpiDataFile; Variables: TStrings;
  FunctionList: TAggrFuncList): TAggregateDatafile;
var
  SortList: TEpiFields;
  V: String;
  F, NewF: TEpiField;
  RefMap: TEpiReferenceMap;
  Func: TAggrFunc;
  PercList: TAggrFuncList;
  Runner, i, Index: Integer;

begin
  SortList := TEpiFields(InputDF.Fields.GetItemFromList(Variables, TEpiFields));
  InputDF.SortRecords(SortList);

  // Create the resulting datafile
  Result := TAggregateDatafile.Create(nil);

  // Create a copy of the variables
  RefMap := TEpiReferenceMap.Create;
  for V in Variables do
    begin
      F := InputDF.Fields.FieldByName[V];
      NewF := TEpiField(F.Clone(nil, RefMap));
      Result.MainSection.Fields.AddItem(NewF);
    end;
//  RefMap.FixupReferences;
//  RefMap.Free;

  for i := 0 to FunctionList.Count - 1 do
    begin
      Func := FunctionList[i];
      Func.CreateResultVector(Result, VariableLabelType);
    end;

  // Percentile calculations not correct yet... :(
  PercList := FunctionList.ExtractPercentiles();

  Runner := 0;
  while Runner < InputDF.Size do
    begin
      if (Runner > 0 ) and
         (SortList.CompareRecords(Runner, Runner - 1) <> 0)
      then
        begin
          Index := Result.NewRecords();
          FunctionList.SetOutputs(Index);
          FunctionList.ResetAll;
        end;

      for i := 0 to FunctionList.Count - 1 do
        FunctionList[i].Execute(Runner);

      Inc(Runner);
    end;
  Index := Result.NewRecords();
  FunctionList.SetOutputs(Index);
  FunctionList.ResetAll;
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
    T.Cell[Col, 0].Text := ResultDF.Field[Col].Question.Text;

  for Row := 0 to ResultDF.Size - 1 do
    for Col := 0 to ResultDF.Fields.Count - 1 do
      T.Cell[Col, Row + 1].Text := ResultDF.Field[Col].AsString[Row];
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
          begin
            F := InputDF.Fields.FieldByName[Opt.Expr.AsIdent];
            FunctionList.Add(TAggrCount.Create('N', F, acNotMissing));
            Continue;
          end;

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

  TempDF := DoCalcAggregate(InputDF, Varnames, FunctionList);
  DoOutputAggregate(TempDF, ST);
  TempDF.Free;
end;

function TAggregate.CalcAggregate(InputDF: TEpiDataFile): TAggregateDatafile;
begin

end;

end.

