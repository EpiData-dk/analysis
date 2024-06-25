unit freq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  executor, result_variables, epifields_helper, outputcreator;


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
// TODO: Jamie - add !w:=weightVar
// This should be as simple as using the AggrSum function on w instead of AggrCount on var
// be sure to restrict the dataset to records where w is not missing, even when !m is not specified
  TFreqCommand = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FDecimals: Integer;
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
  epimiscutils, options_utils, statfunctions, aggregate, aggregate_types, LazUTF8Classes;

type

  { TFreqAggrFunc }

  TFreqAggrFunc = class(TAggrFunc)
  private
    FTotalCount: EpiInteger;
    FCount: EpiInteger;
    FCumCount: EpiInteger;
    FCountV: TEpiField;
    FPercV: TEpiField;
    FCumV: TEpiField;
    FLowCIV: TEpiField;
    FHighCIV: TEpiField;
  public
    constructor Create(AResultVarName: UTF8String; AAggregateVector: TEpiField);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(DataFile: TEpiDataFile); override;
  end;

constructor TFreqAggrFunc.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField);
begin
  inherited Create(AResultVarName, AAggregateVector, afCount);
end;

procedure TFreqAggrFunc.Execute(Idx: integer);
begin
  Inc(FCount);
  Inc(FCumCount);
end;

procedure TFreqAggrFunc.SetOutput(Idx: integer);
var
  CIHigh, CILow: EpiFloat;
begin
  FCountV.AsInteger[Idx] := FCount;
  FPercV.AsFloat[Idx]    := (FCount / FTotalCount) * 100;
  FCumV.AsFloat[Idx]        := (FCumCount / FTotalCount) * 100;

  EpiProportionCI(FCount, FTotalCount, CIHigh, CILow);
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
  FCumV.Name    := 'freq_cummilative';
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
  Func := TFreqAggrFunc.Create('', InputDF.Field[0]);
  Func.FTotalCount := InputDF.Size;
  FunctionList.Add(Func);

  Aggregator := TAggregate.Create(FExecutor, FOutputCreator);
  Aggregator.ResultDataFileClass := TFreqDatafile;
  Result := TFreqDatafile(Aggregator.CalcAggregate(InputDF, Varnames, FunctionList, False, Dummy, RefMap));
  Result.FSum := InputDF.Size;

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
  if ST.HasOption('ci') then  inc(CCount);
  if ST.HasOption('cum') then inc(CCount);


  T.ColCount := CCount;
  T.RowCount := ResultDF.Size + 2;

  Col := 1;
  T.Cell[PostInc(Col), 0].Text := 'N';
  if ST.HasOption('pr') then  T.Cell[PostInc(Col), 0].Text := '%';
  if ST.HasOption('ci') then  T.Cell[PostInc(Col), 0].Text := '(95% CI)';
  if ST.HasOption('cum') then T.Cell[PostInc(Col), 0].Text := 'Cum %';
  T.SetRowBorders(0, [cbTop, cbBottom]);

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
  PrepareVariable: TStringListUTF8;
  HasMissing: Boolean;
begin
  FExecutor.ClearResults('$freq');

  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FDecimals            := DecimalFromOption(ST.Options);

  Variables := ST.VariableList.GetIdentsAsList;
  PrepareVariable := TStringListUTF8.Create;
  HasMissing := ST.HasOption('m');

  for Variable in Variables do
    begin
      PrepareVariable.Clear;
      PrepareVariable.Add(Variable);

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

