unit freq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  executor, result_variables, interval_types, epifields_helper,
  outputcreator;


type

  { TFreqCommand }

  TFreqCommand = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FDecimals: Integer;
    function DoFreqTable(InputDF: TEpiDataFile): TEpiDataFile;
    procedure DoOutputFreqTable(InputDF: TEpiDataFile; ST: TCustomVariableCommand);
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecFreq(DF: TEpiDataFile; ST: TCustomVariableCommand);
  end;

implementation

uses
  epimiscutils, options_utils;

{ TFreqCommand }

function TFreqCommand.DoFreqTable(InputDF: TEpiDataFile): TEpiDataFile;
var
  CountV, CategV, RunV: TEpiField;
  Runner, RIdx, i: Integer;
begin
  result := TEpiDataFile.Create(nil);

  RunV   := InputDF.Fields[0];
  CategV := Result.NewField(ftString);
  CategV.Question.Text := RunV.GetVariableLabel(FVariableLabelOutput);
  CategV.Name := 'categ';

  CountV := Result.NewField(ftInteger);
  CountV.Name := 'count';

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
end;

procedure TFreqCommand.DoOutputFreqTable(InputDF: TEpiDataFile;
  ST: TCustomVariableCommand);
var
  T: TOutputTable;
  CategV, CountV: TEpiField;
  i, Sum, CCount, Col, RunSum: Integer;
  RCount, RCateg,  RPerc, RCum: TExecVarVector;
begin
  CategV := InputDF.Fields.FieldByName['categ'];
  CountV := InputDF.Fields.FieldByName['count'];

  T := FOutputCreator.AddTable;
  T.Header.Text := CategV.Question.Text;

  CCount := 2;
  if ST.HasOption('r') then
    begin
      inc(CCount);
    end;
  if ST.HasOption('cum') then
    inc(CCount);

  T.ColCount := CCount;
  T.RowCount := InputDF.Size + 2;

  Col := 1;
  T.Cell[PostInc(Col), 0].Text := 'N';
  if ST.HasOption('r') then
    T.Cell[PostInc(Col), 0].Text := '%';
  if ST.HasOption('cum') then
    T.Cell[PostInc(Col), 0].Text := 'Cum %';

  Sum := 0;
  for i := 0 to InputDF.Size - 1 do
    Sum := Sum + CountV.AsInteger[i];

  FExecutor.AddResultConst('$total', ftInteger).AsIntegerVector[0] := Sum;
  FExecutor.AddResultConst('$rows', ftInteger).AsIntegerVector[0]  := InputDF.Size;


  RCount := FExecutor.AddResultVector('$count', ftInteger, InputDF.Size);
  RCateg := FExecutor.AddResultVector('$labels', ftString,  InputDF.Size);
  if ST.HasOption('r') then
    RPerc := FExecutor.AddResultVector('$rowpercent', ftFloat,  InputDF.Size);
  if ST.HasOption('cum') then
    RCum  := FExecutor.AddResultVector('$cumpercent', ftFloat,  InputDF.Size);

  RunSum := 0;
  for i := 0 to InputDF.Size - 1 do
    begin
      Col := 0;
      T.Cell[PostInc(Col), i + 1].Text := CategV.AsString[i];
      T.Cell[PostInc(Col), i + 1].Text := CountV.AsString[i];

      RCateg.AsStringVector[i]  := CategV.AsString[i];
      RCount.AsIntegerVector[i] := CountV.AsInteger[i];

      if ST.HasOption('r') then
        begin
          T.Cell[PostInc(Col), i + 1].Text := Format('%8.' + IntToStr(FDecimals) + 'F', [(CountV.AsInteger[i] / Sum) * 100]);
          RPerc.AsFloatVector[i] := (CountV.AsInteger[i] / Sum) * 100;
        end;

      if ST.HasOption('cum') then
        begin
          RunSum := RunSum + CountV.AsInteger[i];
          T.Cell[PostInc(Col), i + 1].Text := Format('%8.' + IntToStr(FDecimals) + 'F', [(RunSum / Sum) * 100]);
          RCum.AsFloatVector[i] := (RunSum / Sum) * 100;
        end;
    end;

  T.Cell[0, T.RowCount-1].Text := 'Total';
  T.Cell[1, T.RowCount-1].Text := IntToStr(Sum);

  if ST.HasOption('r') then
    T.Cell[2, T.RowCount-1].Text := Format('%8.' + IntToStr(FDecimals) + 'F', [100.00]);
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
  ResDF: TEpiDataFile;
begin
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  FDecimals := 2;
  if ST.HasOption('d0') then
    FDecimals := 0;
  if ST.HasOption('d1') then
    FDecimals := 1;
  if ST.HasOption('d2') then
    FDecimals := 2;

  ResDF := DoFreqTable(DF);
  DoOutputFreqTable(ResDF, ST);
  ResDF.Free;
end;

end.

