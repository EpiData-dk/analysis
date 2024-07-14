unit freq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  executor, result_variables, epifields_helper, outputcreator, math;


type

  { TFreqDatafile }

  TFreqDatafile = class(TEpiDataFile)
  private
    FCateg: TEpiField;
    FCount: TEpiField;
    FPercent: TEpiField;
    FCumPercent: TEpiField;
    FLowCI: TEpiField;
    FHighCI: TEpiField;
    FSum: Integer;
  public
    property Categ: TEpiField read FCateg;
    property Count: TEpiField read FCount;
    property Percent: TEpiField read FPercent;
    property CumPercent: TEpiField read FCumPercent;
    property LowCI: TEpiField read FLowCI;
    property HighCI: TEpiField read FHighCI;
    property Sum: Integer read FSum;
  end;

  { TFreqCommand }

  TFreqCommand = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FDecimals: Integer;
    FHasWeights: Boolean;
    FWeightVarName: String;
    function  DoCalcFreqTable(InputDF: TEpiDataFile; VariableName: String; Out RefMap: TEpiReferenceMap): TFreqDatafile;
    procedure PrepareResultVariables(Variables: TStrings);
    procedure DoResultVariables(ResultDF: TFreqDatafile; VariableName: String; ValueLabelType: TEpiGetValueLabelType);
    procedure DoOutputFreqTable(ResultDF: TFreqDatafile; ST: TCustomVariableCommand);
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;
    // Method called from Executor, does calculation + result vars + output
    procedure ExecFreq(DF: TEpiDataFile; ST: TCustomVariableCommand);
    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset;
    function  CalcFreq(DF: TEpiDataFile; VariableName: String; Out RefMap: TEpiReferenceMap): TFreqDatafile;
  end;

implementation

uses
  epimiscutils, options_utils, statfunctions, aggregate, aggregate_types;

type

  { TFreqAggrFunc }

  TFreqAggrFunc = class(TAggrFunc)
  private
    FTotalCount: EpiFloat;
    FCount: EpiFloat;
    FCumCount: EpiFloat;
    FCountV: TEpiField;
    FPercV: TEpiField;
    FCumV: TEpiField;
    FLowCIV: TEpiField;
    FHighCIV: TEpiField;
    FWeights: Boolean;
  public
    constructor Create(AResultVarName: UTF8String; AAggregateVector: TEpiField);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(DataFile: TEpiDataFile); override;
    property Weights: Boolean read FWeights write FWeights;
    property TotalCount: EpiFloat read FTotalCount;
  end;

constructor TFreqAggrFunc.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField);
var
  i: integer;
begin
  FWeights := AResultVarName <> '';
  if (FWeights) then
    begin
      inherited Create(AResultVarName, AAggregateVector, afSum);
      FTotalCount := 0;
      for i := 0 to AAggregateVector.Size - 1 do
        if not AAggregateVector.IsMissing[i] then
          FTotalCount += AAggregateVector.AsFloat[i];
    end
  else
    begin
      inherited Create(AResultVarName, AAggregateVector, afCount);
      FTotalCount := AAggregateVector.Size;
    end;
end;

procedure TFreqAggrFunc.Execute(Idx: integer);
begin
  if (FWeights) then
    begin
      if AggregateVector.IsMissing[idx] then exit;
      FCount := FCount + AggregateVector.AsFloat[idx];
      FCumCount := FCumCount + AggregateVector.AsFloat[idx];
    end
  else
    begin
      FCount += 1;
      FCumCount += 1;
    end;
end;

procedure TFreqAggrFunc.SetOutput(Idx: integer);
var
  ACount: EpiInteger;
  CIHigh, CILow: EpiFloat;
begin
  ACount := math.floor(FCount);
  FCountV.AsInteger[Idx] := ACount;
  FPercV.AsFloat[Idx]    := (FCount / FTotalCount) * 100;
  FCumV.AsFloat[Idx]        := (FCumCount / FTotalCount) * 100;

  EpiProportionCI(ACount, floor(FTotalCount), CIHigh, CILow);
  FLowCIV.AsFloat[Idx]      := CILow * 100;
  FHighCIV.AsFloat[Idx]     := CIHigh * 100;
end;

procedure TFreqAggrFunc.Reset();
begin
  FCount := 0;
end;

procedure TFreqAggrFunc.CreateResultVector(DataFile: TEpiDataFile);
begin
  //inherited DoCreateResultVector(DataFile, ftInteger);

  FCountV       := DataFile.NewField(ftInteger);   // TFreqDatafile(DataFile).Count;
  FCountV.Name  := 'freq_count';
  FPercV        := DataFile.NewField(ftFloat);     // TFreqDatafile(DataFile).Percent;
  FPercV.Name   := 'freq_percent';
  FCumV         := DataFile.NewField(ftFloat);     // TFreqDatafile(DataFile).CumPercent;
  FCumV.Name    := 'freq_cumulative';
  FLowCIV       := DataFile.NewField(ftFloat);     // TFreqDatafile(DataFile).LowCI;
  FLowCIV.Name  := 'freq_lowci';
  FHighCIV      := DataFile.NewField(ftFloat);     // TFreqDatafile(DataFile).HighCI;
  FHighCIV.Name := 'freq_highci';

  with TFreqDatafile(DataFile) do
    begin
      FCateg      := Field[0];
      FCount      := FCountV;
      FPercent    := FPercV;
      FCumPercent := FCumV;
      FLowCI      := FLowCIV;
      FHighCI     := FHighCIV;
    end;
end;

{ TFreqCommand }

function TFreqCommand.DoCalcFreqTable(InputDF: TEpiDataFile;
  VariableName: String; out RefMap: TEpiReferenceMap): TFreqDatafile;
var
  FunctionList: TAggrFuncList;
  Aggregator: TAggregate;
  Varnames: TStringList;
  Func: TFreqAggrFunc;
  Dummy: TEpiFields;
begin
  Varnames := TStringList.Create;
  VarNames.Add(VariableName);

  FunctionList := TAggrFuncList.Create(true);
  if (FHasWeights) then
    Func := TFreqAggrFunc.Create('w', InputDF.Field[0])
  else
    Func := TFreqAggrFunc.Create('', InputDF.Field[0]);
  FunctionList.Add(Func);

  Aggregator := TAggregate.Create(FExecutor, FOutputCreator);
  Aggregator.ResultDataFileClass := TFreqDatafile;
  Result := TFreqDatafile(Aggregator.CalcAggregate(InputDF, Varnames, FunctionList, False, Dummy, RefMap));
  Result.FSum := floor(Func.TotalCount); //InputDF.Size;

  // Dummy MUST be freed - otherwise we may get a memory leak.
  //  if Dummy = nil, then nothing will happen since .Free also work with nil pointer
  Dummy.Free;
end;

procedure TFreqCommand.PrepareResultVariables(Variables: TStrings);
begin
  // TODO: Created resultvariables like in tables
end;

procedure TFreqCommand.DoResultVariables(ResultDF: TFreqDatafile;
  VariableName: String; ValueLabelType: TEpiGetValueLabelType);
var
  RCount, RCateg, RPerc, RCum, RLowCI, RHighCI: TExecVarVector;
  i: Integer;
begin
  FExecutor.AddResultConst(            '$freq_' + VariableName + '_total',      ftInteger).AsIntegerVector[0] := ResultDF.Sum;
  FExecutor.AddResultConst(            '$freq_' + VariableName + '_rows',       ftInteger).AsIntegerVector[0]  := ResultDF.Size;
  RCount  := FExecutor.AddResultVector('$freq_' + VariableName + '_count',      ftInteger, ResultDF.Size);
  RCateg  := FExecutor.AddResultVector('$freq_' + VariableName + '_labels',     ftString,  ResultDF.Size);
  RPerc   := FExecutor.AddResultVector('$freq_' + VariableName + '_rowpercent', ftFloat,   ResultDF.Size);
  RCum    := FExecutor.AddResultVector('$freq_' + VariableName + '_cumpercent', ftFloat,   ResultDF.Size);
  RLowCI  := FExecutor.AddResultVector('$freq_' + VariableName + '_lowCI',      ftFloat,   ResultDF.Size);
  RHighCI := FExecutor.AddResultVector('$freq_' + VariableName + '_highCI',     ftFloat,   ResultDF.Size);

  for i := 0 to ResultDF.Size - 1 do
    begin
      RCount.AsIntegerVector[i] := ResultDF.Count.AsInteger[i];
      RCateg.AsStringVector[i]  := ResultDF.Categ.GetValueLabel(i, ValueLabelType);
      RPerc.AsFloatVector[i]    := ResultDF.Percent.AsFloat[i];
      RCum.AsFloatVector[i]     := ResultDF.CumPercent.AsFloat[i];
      RLowCI.AsFloatVector[i]   := ResultDF.LowCI.AsFloat[i];
      RHighCI.AsFloatVector[i]  := ResultDF.HighCI.AsFloat[i];
    end;
end;

procedure TFreqCommand.DoOutputFreqTable(ResultDF: TFreqDatafile;
  ST: TCustomVariableCommand);
var
  T: TOutputTable;
  CategV, CountV: TEpiField;
  i, CCount, Col: Integer;
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
begin
  CategV := ResultDF.Categ;
  CountV := ResultDF.Count;

  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  T := FOutputCreator.AddTable;
  T.Header.Text := CategV.GetVariableLabel(VariableLabelType);

  CCount := 2;
  if ST.HasOption('pr') then  inc(CCount);
  if ST.HasOption('cum') then inc(CCount);
  if ST.HasOption('ci') then  inc(CCount);


  T.ColCount := CCount;
  T.RowCount := ResultDF.Size + 2;

  Col := 1;
  T.Cell[PostInc(Col), 0].Text := 'N';
  if ST.HasOption('pr') then  T.Cell[PostInc(Col), 0].Text := '%';
  if ST.HasOption('ci') then
  if ST.HasOption('cum') then T.Cell[PostInc(Col), 0].Text := 'Cum %';
  T.SetRowBorders(0, [cbTop, cbBottom]);
  if (ST.HasOption('ci')) then
    begin
      if (ST.HasOption('w')) then
        begin
          T.Cell[PostInc(Col), 0].Text := '(95% CI)*';
          T.Footer.Text := '*Confidence intervals assume that weights are counts from Aggregate.';
        end
      else
        T.Cell[PostInc(Col), 0].Text := '(95% CI)';
    end;
  ValueLabelType := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  for i := 0 to ResultDF.Size - 1 do
    begin
      Col := 0;
      T.Cell[PostInc(Col), i + 1].Text := CategV.GetValueLabel(i, ValueLabelType);
      T.Cell[PostInc(Col), i + 1].Text := CountV.AsString[i];

      if ST.HasOption('pr') then
        T.Cell[PostInc(Col), i + 1].Text := Format('%8.' + IntToStr(FDecimals) + 'F', [ResultDF.Percent.AsFloat[i]]);

      if ST.HasOption('ci') then
        T.Cell[PostInc(Col), i + 1].Text := Format('(%.' + IntToStr(FDecimals) + 'F - %.' + IntToStr(FDecimals) + 'F)',
                                                   [ResultDF.LowCI.AsFloat[i], ResultDF.HighCI.AsFloat[i]]);

      if ST.HasOption('cum') then
        T.Cell[PostInc(Col), i + 1].Text := Format('%8.' + IntToStr(FDecimals) + 'F', [ResultDF.CumPercent.AsFloat[i]]);
    end;

  T.Cell[0, T.RowCount-1].Text := 'Total';
  T.Cell[1, T.RowCount-1].Text := IntToStr(ResultDF.Sum);

  if ST.HasOption('r') then
    T.Cell[2, T.RowCount-1].Text := Format('%8.' + IntToStr(FDecimals) + 'F', [100.00]);

  T.SetRowBorders(T.RowCount - 1, [cbTop, cbBottom]);
end;

constructor TFreqCommand.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
end;

destructor TFreqCommand.Destroy;
begin
  inherited Destroy;
end;

procedure TFreqCommand.ExecFreq(DF: TEpiDataFile; ST: TCustomVariableCommand);
var
  ResDF: TFreqDatafile;
  RefMap: TEpiReferenceMap;
  Variables: TStrings;
  Variable: String;
  PrepDF: TEpiDataFile;
  PrepareVariable: TStringList;
  HasMissing: Boolean;
  Opt: TOption;
begin
  FExecutor.ClearResults('$freq');

  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FDecimals            := DecimalFromOption(ST.Options);

  Variables := ST.VariableList.GetIdentsAsList;
  PrepareVariable := TStringList.Create;
  HasMissing := ST.HasOption('m');

  FWeightVarName := '';
  FHasWeights := ST.HasOption('w',Opt);
  if (FHasWeights) then
    begin
      FWeightVarName := Opt.Expr.AsIdent;
      if (Variables.IndexOf(FWeightVarName) <> -1) then
        begin
          FExecutor.Error('Cannot get frequencies for ' + FWeightVarName + ' and use it as weights.');
          exit;
        end;
    end;

  for Variable in Variables do
    begin
      PrepareVariable.Clear;
      PrepareVariable.Add(Variable);

      if (FHasWeights) then
        PrepareVariable.Add(FWeightVarName);

      if (HasMissing) then
        PrepDF := FExecutor.PrepareDatafile(PrepareVariable, nil)
      else
        PrepDF := FExecutor.PrepareDatafile(PrepareVariable, PrepareVariable);

      if (PrepDF.Size = 0) then
        begin
          FOutputCreator.DoWarning('Variable "' + Variable + '" contains no data!');
          FOutputCreator.DoNormal('');
          PrepDF.Free;
          Continue;
        end;

      ResDF := DoCalcFreqTable(PrepDF, Variable, RefMap);
      DoResultVariables(ResDF, Variable, ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions));

      if (not ST.HasOption('q')) then
        DoOutputFreqTable(ResDF, ST);

      RefMap.Free;
      ResDF.Free;
      PrepDF.Free;
    end;
  PrepareVariable.Free;
end;

function TFreqCommand.CalcFreq(DF: TEpiDataFile; VariableName: String;
  out RefMap: TEpiReferenceMap): TFreqDatafile;
begin
  Result := DoCalcFreqTable(DF, VariableName, RefMap);
end;

end.

