unit describe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  executor, result_variables, epifields_helper, outputcreator, options_utils,
  freq, means;

{
makes use of freq and means
function  CalcFreq(DF: TEpiDataFile; VariableName: String; Out RefMap: TEpiReferenceMap): TFreqDatafile;

function CalcMeans(DataFile: TEpiDataFile; Const CountVarName, StratifyVarName: UTF8String;
  ValueLabelOutput: TEpiGetValueLabelType = gvtValue; VariableLabelOutput: TEpiGetVariableLabelType = gvtVarName): TMeansDatafile;

  Handle all output with mean/freq routines? or do it here; should be able to manage one row of freqs per variable
}

type
  {DescribeCommand}

  TDescribeCommand = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FDecimals: Integer;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
  protected
    FFreqData: TFreqDataFile;
    FMeanData: TMeansDataFile;
    function CreateFreqOutputHeader(Data: TFreqDataFile; ST: TCustomVariableCommand): TOutputTable;
    function CreateMeansOutputHeader(Data: TMeansDataFile; ST: TCustomVariableCommand): TOutputTable;
    procedure DoOutputFreqRow(Data: TFreqDataFile; ST: TCustomVariableCommand; T: TOutputTable);
    procedure DoOutputMeansRow(Data: TMeansDataFile; ST: TCustomVariableCommand; T: TOutputTable);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor  Destroy; override;
    procedure   ExecDescribe(VarNames: TStrings; ST: TCustomVariableCommand);
  end;

implementation

constructor TDescribeCommand.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator
  );
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TDescribeCommand.Destroy;
begin
  inherited Destroy;
end;

function TDescribeCommand.CreateFreqOutputHeader(Data: TFreqDataFile; ST: TCustomVariableCommand): TOutputTable;
var
  Offset: Integer;
begin
  result := FOutputCreator.AddTable;
  result.ColCount := 8;
  result.RowCount := 2;
  Offset          := 2;
  result.Cell[0, 1].Text := 'Var';
  result.Cell[1, 1].Text := 'obs';
  if (ST.HasOption('m')) then
  begin
    Offset += 1;
    Result.ColCount := 9;
    result.Cell[2, 1].Text := 'missing';
  end;
  result.Cell[Offset,     0].Text := 'unique';
  result.Cell[Offset,     1].Text := 'values';
  result.Cell[Offset + 1, 0].Text := 'value';
  result.Cell[Offset + 1, 1].Text := '(#)';
  result.Cell[Offset + 2, 0].Text := 'value';
  result.Cell[Offset + 2, 1].Text := '(#)';
  result.Cell[Offset + 3, 0].Text := 'value';
  result.Cell[Offset + 3, 1].Text := '(#)';
  result.Cell[Offset + 4, 0].Text := 'value';
  result.Cell[Offset + 4, 1].Text := '(#)';
  result.Cell[Offset + 5, 0].Text := 'value';
  result.Cell[Offset + 5, 1].Text := '(#)';
  result.SetRowBorders(0, [cbTop]);
  result.SetRowBorders(1, [cbBottom]);

end;

function TDescribeCommand.CreateMeansOutputHeader(Data: TMeansDataFile; ST: TCustomVariableCommand): TOutputTable;
begin
  result := FOutputCreator.AddTable;
  result.ColCount := 8;
  result.RowCount := 1;
  result.Cell[0, 0].Text := 'Var';
  result.Cell[1, 0].Text := 'obs';
  result.Cell[2, 0].Text := 'sum';
  result.Cell[3, 0].Text := 'mean';
  result.Cell[4, 0].Text := 'sd';
  // TODO: if (ST.HasOption('ci') then show ci
  result.Cell[5, 0].Text := 'min';
  result.Cell[6, 0].Text := 'median';
  result.Cell[7, 0].Text := 'max';
  result.SetRowAlignment(0, taRightJustify);
  result.SetColAlignment(0, taLeftJustify);
  result.SetRowBorders(0, [cbTop, cbBottom]);
end;

procedure TDescribeCommand.DoOutputFreqRow(Data: TFreqDataFile; ST: TCustomVariableCommand; T: TOutputTable);

var
  Offset: Integer;
  CategV, CountV: TEpiField;
  RowIdx, ix, NCat, aCount, aIx, i: Integer;
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  CategIx: array [0..4] of Integer;
  CountIx: array [0..4] of Integer;
begin
  CategV := Data.Categ;
  CountV := Data.Count;
  Offset := 2;
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  RowIdx     := T.RowCount;
  T.RowCount := RowIdx + 2;
  T.Cell[0, RowIdx].Text := CategV.GetVariableLabel(VariableLabelType);
  T.SetColAlignment(0, taLeftJustify);
  T.Cell[1, RowIdx].Text := IntToStr(Data.Sum);
  NCat := Data.Size;
  if (ST.HasOption('m')) then
  begin
    Offset += 1;
    T.Cell[2, RowIdx].Text := '-'; // check for missing value as last category
    if (CategV.IsMissing[Data.Size-1]) then
    begin
      T.Cell[2,RowIdx].Text := CountV.AsString[Data.Size-1];
      NCat := Data.Size - 1;
    end;
  end;
  T.Cell[Offset   , RowIdx].Text := IntToStr(Data.Size);

// here we search for top 5 frequencies
  CategIx[0] := 0;
  CountIx[0] := CountV[0];
  for ix := 1 to 4 do
  begin
    CategIx[ix] := 0;
    CountIx[ix] := 0;
  end;
  for ix := 1 to NCat - 1 do
  begin
    aCount := CountV.AsInteger[ix];
    if (aCount > CountIx[4]) then
    begin
      CountIx[4] := aCount;
      CategIx[4] := ix;
      for i := 3 downto 0 do
      begin
        if (aCount > CountIx[i]) then
        begin
          CountIx[i+1] := CountIx[i];
          CategIx[i+1] := CategIx[i];
          CountIx[i]   := aCount;
          CategIx[i]   := ix;
        end;
      end;
    end;
  end;

  // now show the top categories
  Offset += 1;
  if (NCat > 5) then NCat := 5;
  for ix := 0 to NCat - 1 do
  begin
    T.Cell[Offset + ix, RowIdx    ].Text := CategV.GetValueLabel(CategIx[ix], ValueLabelType);
    T.Cell[Offset + ix, RowIdx + 1].Text := CountV.AsString[CategIx[ix]];
  end;
  T.SetRowBorders(RowIdx + 1, [cbBottom]);
end;

procedure TDescribeCommand.DoOutputMeansRow(Data: TMeansDataFile; ST: TCustomVariableCommand; T: TOutputTable);
var
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  RowIdx: Integer;
  StatFmt: String;

function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;  // from means.pas
  begin
    if (val = TEpiFloatField.DefaultMissing) then
      Result := TEpiStringField.DefaultMissing
    else
      Result := Format(fmt, [val]);
  end;

begin
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FDecimals  := DecimalFromOption(ST.Options);
  StatFmt    := '%8.' + IntToStr(FDecimals) + 'F';
  RowIdx     := T.RowCount;
  T.RowCount := RowIdx + 1;
  with Data do
  begin
    T.Cell[0, RowIdx].Text := CountVarText;
    T.Cell[1, RowIdx].Text := N.AsString[0];
    T.Cell[2, RowIdx].Text := Format(StatFmt, [Sum.AsFloat[0]]);
    T.Cell[3, RowIdx].Text := Format(StatFmt, [Mean.AsFloat[0]]);
    T.Cell[4, RowIdx].Text := StatFloatDisplay(StatFmt, StdDev.AsFloat[0]);
    T.Cell[5, RowIdx].Text := Format(StatFmt, [Min.AsFloat[0]]);
    T.Cell[6, RowIdx].Text := Format(StatFmt, [Median.AsFloat[0]]);
    T.Cell[7, RowIdx].Text := Format(StatFmt, [Max.AsFloat[0]]);
  end;
  T.SetColAlignment(0, taLeftJustify);
end;

procedure TDescribeCommand.ExecDescribe(VarNames: TStrings; ST: TCustomVariableCommand);
var
  DF: TEpiDataFile;
  VariableLabelType: TEpiGetVariableLabelType;
  AFieldType: TEpiFieldType;
  AVar: TStrings;
  Opt: TOption;
  i, ix, VarCount: Integer;
  FirstPass: Boolean;
  FreqData:  TFreqDataFile;
  MeansData: TMeansDataFile;
  SummaryTable: TOutputTable;
  F:   TFreqCommand;
  M:   TMeans;
  O:   TEpiReferenceMap;

  // invoke freq for one variable
  procedure DoDescribeFreq(VarName: String);

  begin;
    try
      FreqData := F.CalcFreq(DF, VarName, O);
{        if (FirstPass) then
          CreateFreqResultVariables(VarNames); // set up result variables
        DoFreqResultVariables(i); // results for one table row
}
        if (not ST.HasOption('q')) then
        begin
          if (FirstPass) then // cannot create header until we have done calcfreq once
            SummaryTable := CreateFreqOutputHeader(FreqData, ST);
          DoOutputFreqRow(FreqData, ST, SummaryTable);
        end;
        FirstPass := FALSE;

    finally
      DF.Free;
    end;

  end;

  // invoke means for one variable
  procedure DoDescribeMeans(VarName: String);

  begin;
    try
      MeansData := M.CalcMeans(DF, VarName, '');

{        if (FirstPass) then
          CreateMeansResultVariables(VarNames); // set up result variables
        DoMeansResultVariables(i); // results for one table row
}
        if (not ST.HasOption('q')) then
        begin
          if (FirstPass) then // cannot create header until we have done calcfreq once
            SummaryTable := CreateMeansOutputHeader(MeansData, ST);
          DoOutputMeansRow(MeansData, ST, SummaryTable);
        end;
        FirstPass := FALSE;

    finally
      DF.Free;
    end;

  end;

begin
  VarCount := VarNames.Count;
  if (VarCount < 1) then
  begin
    // error message (should not happen)
    exit;
  end;
  AVar := TStringList.Create;
  // Process categorical variables first
  F := TFreqCommand.Create(FExecutor, FOutputCreator);
  FirstPass := TRUE;
  for i := 0 to VarCount - 1 do
  begin
    AVar.Add(VarNames[i]);
    if ST.HasOption('m') then
      DF := FExecutor.PrepareDatafile(AVar, nil)
    else
      DF := FExecutor.PrepareDatafile(AVar, AVar);
    AVar.Delete(0);
    AFieldType := DF.Field[0].FieldType;
    if (ST.HasOption('c') or
       not ((AFieldType = ftInteger) or (AfieldType = ftFloat) or (AFieldType = ftAutoInc))) then
      begin
        if (DF.Size = 0) then
          FOutputCreator.DoWarning(VarNames[i] + ': No data')
        else
          DoDescribeFreq(VarNames[i]);
      end;
  end;

  if (not ST.HasOption('c')) then
  begin
  // Process integer and float variables
    FDecimals := DecimalFromOption(ST.Options);
    FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
    FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

    M := TMeans.Create(FExecutor, FOutputCreator);
    FirstPass := TRUE;
    for i := 0 to VarCount - 1 do
    begin
      AVar.Add(VarNames[i]);
      DF := FExecutor.PrepareDatafile(AVar, AVar);
      AFieldType := DF.Field[0].FieldType;
      if ((AFieldType = ftInteger) or (AfieldType = ftFloat) or (AFieldType = ftAutoInc)) then
      begin
        if (DF.Size = 0) then
          FOutputCreator.DoWarning(VarNames[i] + ': No data')
        else
          DoDescribeMeans(VarNames[i]);
      end;
      AVar.Delete(0);
    end;
  end;
end;

end.

