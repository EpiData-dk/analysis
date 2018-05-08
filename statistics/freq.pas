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
    constructor Create(AOwner: TEpiCustomBase);
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
    function  DoFreqTable(InputDF: TEpiDataFile): TFreqDatafile;
    procedure DoResultVariables(ResultDF: TFreqDatafile);
    procedure DoOutputFreqTable(ResultDF: TFreqDatafile; ST: TCustomVariableCommand);
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;
    // Method called from Executor, does calculation + result vars + output
    procedure ExecFreq(DF: TEpiDataFile; ST: TCustomVariableCommand);
    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function  CalcFreq(DF: TEpiDataFile; VariableLabelOutput: TEpiGetVariableLabelType = gvtVarName): TFreqDatafile;
  end;

implementation

uses
  epimiscutils, options_utils, statfunctions;

{ TFreqDatafile }

constructor TFreqDatafile.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner, 0);

  FCateg      := NewField(ftString);
  FCateg.Name := 'categ';

  FCount      := NewField(ftInteger);
  FCount.Name := 'count';

  FPercent    := NewField(ftFloat);
  FCumPercent := NewField(ftFloat);
  FLowCI      := NewField(ftFloat);
  FHighCI     := NewField(ftFloat);
end;

{ TFreqCommand }

function TFreqCommand.DoFreqTable(InputDF: TEpiDataFile): TFreqDatafile;
var
  CountV, CategV, RunV, PercV, CumV, LowCIV, HighCIV: TEpiField;
  Runner, RIdx, i, RunSum, Sum: Integer;
  CIHigh, CILow: EpiFloat;
begin
  RunV   := InputDF.Fields[0];

  result := TFreqDatafile.Create(nil);

  CategV := Result.Categ;
  CategV.Question.Text := RunV.GetVariableLabel(FVariableLabelOutput);

  CountV  := Result.Count;
  PercV   := Result.Percent;
  CumV    := Result.CumPercent;
  LowCIV  := Result.LowCI;
  HighCIV := Result.HighCI;

  InputDF.SortRecords(InputDF.Fields[0]);

  RIdx := Result.NewRecords();
  CategV.AsString[RIdx] := RunV.GetValueLabel(0, FValuelabelOutput);

  Runner := 1;
  for i := 1 to InputDF.Size - 1 do
  begin
    if (RunV.Compare(i-1, i) <> 0) then
      begin
        CountV.AsInteger[RIdx] := Runner;
        Runner := 0;

        RIdx := Result.NewRecords();
        CategV.AsString[RIdx] := RunV.GetValueLabel(i, FValuelabelOutput);
      end;

    Inc(Runner);
  end;
  CountV.AsInteger[RIdx] := Runner;

  Sum := InputDF.Size;
  RunSum := 0;
  for i := 0 to Result.Size - 1 do
    begin
      RunSum := RunSum + CountV.AsInteger[i];

      PercV.AsFloat[i] := (CountV.AsInteger[i] / Sum) * 100;
      CumV.AsFloat[i]  := (RunSum / Sum) * 100;

      EpiProportionCI(CountV.AsInteger[i], Sum, CIHigh, CILow);
      LowCIV.AsFloat[i]  := CILow * 100;
      HighCIV.AsFloat[i] := CIHigh  * 100;
    end;
  Result.FSum := Sum;
end;

procedure TFreqCommand.DoResultVariables(ResultDF: TFreqDatafile);
var
  RCount, RCateg, RPerc, RCum, RLowCI, RHighCI: TExecVarVector;
  i: Integer;
begin
  FExecutor.AddResultConst('$freq_total', ftInteger).AsIntegerVector[0] := ResultDF.Sum;
  FExecutor.AddResultConst('$freq_rows', ftInteger).AsIntegerVector[0]  := ResultDF.Size;
  RCount  := FExecutor.AddResultVector('$freq_count',      ftInteger, ResultDF.Size);
  RCateg  := FExecutor.AddResultVector('$freq_labels',     ftString,  ResultDF.Size);
  RPerc   := FExecutor.AddResultVector('$freq_rowpercent', ftFloat,   ResultDF.Size);
  RCum    := FExecutor.AddResultVector('$freq_cumpercent', ftFloat,   ResultDF.Size);
  RLowCI  := FExecutor.AddResultVector('$freq_lowCI',      ftFloat,   ResultDF.Size);
  RHighCI := FExecutor.AddResultVector('$freq_highCI',     ftFloat,   ResultDF.Size);

  for i := 0 to ResultDF.Size - 1 do
    begin
      RCount.AsIntegerVector[i] := ResultDF.Count.AsInteger[i];
      RCateg.AsStringVector[i]  := ResultDF.Categ.AsString[i];
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
begin
  CategV := ResultDF.Categ;
  CountV := ResultDF.Count;

  T := FOutputCreator.AddTable;
  T.Header.Text := CategV.Question.Text;

  CCount := 2;
  if ST.HasOption('r') then   inc(CCount);
  if ST.HasOption('ci') then  inc(CCount);
  if ST.HasOption('cum') then inc(CCount);


  T.ColCount := CCount;
  T.RowCount := ResultDF.Size + 2;

  Col := 1;
  T.Cell[PostInc(Col), 0].Text := 'N';
  if ST.HasOption('r') then   T.Cell[PostInc(Col), 0].Text := '%';
  if ST.HasOption('ci') then  T.Cell[PostInc(Col), 0].Text := '(95% CI)';
  if ST.HasOption('cum') then T.Cell[PostInc(Col), 0].Text := 'Cum %';
  T.SetRowBorders(0, [cbTop, cbBottom]);

  for i := 0 to ResultDF.Size - 1 do
    begin
      Col := 0;
      T.Cell[PostInc(Col), i + 1].Text := CategV.AsString[i];
      T.Cell[PostInc(Col), i + 1].Text := CountV.AsString[i];

      if ST.HasOption('r') then
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
begin
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  FDecimals := DecimalFromOption(ST.Options);

  ResDF := DoFreqTable(DF);
  DoResultVariables(ResDF);

  if (not ST.HasOption('q')) then
    DoOutputFreqTable(ResDF, ST);
  ResDF.Free;
end;

function TFreqCommand.CalcFreq(DF: TEpiDataFile;
  VariableLabelOutput: TEpiGetVariableLabelType): TFreqDatafile;
begin
  FVariableLabelOutput := VariableLabelOutput;
  Result := DoFreqTable(DF);
end;

end.

