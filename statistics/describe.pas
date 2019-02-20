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
  {ShowStat} // only used in describe
  FShowStat = record
    Show: Boolean;
    Row:  Integer;
    Col:  Integer
  end;

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
    FMeansData: TMeansDataFile;
    function CreateOutputHeader(ST: TCustomVariableCommand): TOutputTable;
    function CreateOutputHeaderFromTemplate(): TOutputTable;
    procedure CreateTemplateFromHeader(T: TOutputTable);
    procedure DoOutputRows(ST: TCustomVariableCommand; DoMeans: Boolean; T: TOutputTable);
//    procedure DoOutputTable(ST: TCustomVariableCommand; DoMeans: Boolean; T: TOutputTable);
// need to put these here to make them easily available
// add other stats here as necessary
  protected
    Fmissing,
    Fsum, Fmean, Fsd, Fcfil, Fcfih,
    Fmin, Fp10, Fp25, Fmedian, Fp75, Fp90, Fmax,
    Ffreqlo, Ffreqhi: FShowStat;
    FRowsPerVar: Integer;
    FOneTable: Boolean;
    FTemplate: Array of Array of UTF8String;
    FFirstPass: Boolean;
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

function TDescribeCommand.CreateOutputHeaderFromTemplate(): TOutputTable;
var
  i, j: Integer;
begin
  result := FOutputCreator.AddTable;
  result.RowCount := FRowsPerVar;
  result.ColCount := high(FTemplate) + 1;
  for i := 0 to high(FTemplate) do
    for j := 0 to high(FTemplate[0]) do
      result.Cell[i,j].Text := FTemplate[i,j];
end;

procedure TDescribeCommand.CreateTemplateFromHeader(T: TOutputTable);
var
  i, j: Integer;
begin
  setLength(FTemplate,T.ColCount,FRowsPerVar);
  for i := 0 to T.ColCount - 1 do
    for j := 0 to FRowsPerVar - 1 do
      FTemplate[i,j] := T.Cell[i,j].Text;
end;

function TDescribeCommand.CreateOutputHeader(ST: TCustomVariableCommand): TOutputTable;
// TODO: one header setup only
{
  parameter  output
  always     var name, obs, unique values
  from freq
     fl      low 5 frequencies
     fh      high 5 frequencies
     fb      low and high frequencies
     m       missing count
  from means
     mci     mean and ci
     msd     sum, mean, sd
     iqr     p25, median, p75
     idr     p10, median, p90
     rm      min, median, max
}
var
  OptCount, Offset, ROffset, i, HeaderRows: Integer;
  aOpt, aStr: UTF8String;
  aMedianCount: Integer;
  ncol, nrow: Integer;
  aStatsOption: Boolean;
  aDefaultStats: array [1..5] of UTF8String = ('mean','sd','min','median','max');

  function SetStat(Col, Row: Integer): FShowStat;
    begin
      result.Show := true;
      result.Row  := Row;
      result.Col  := Col;
    end;

begin
  // start by counting up columns to show (use case of)
  // if more than 11, then go to one table per variable
  // - set up header array as a dummy table and use it to populate SummaryTable for each var
  // if 11 or less, set up SummaryTable headers

  // set all stats to not show
  Fmissing.Show:= false;
  Fsum.Show    := false;
  Fmean.Show   := false;
  Fsd.Show     := false;
  Fcfil.Show   := false;
  Fcfih.Show   := false;
  Fmin.Show    := false;
  Fp10.Show    := false;
  Fp25.Show    := false;
  Fmedian.Show := false;
  Fp75.Show    := false;
  Fp90.Show    := false;
  Fmax.Show    := false;
  Ffreqlo.Show := false;
  Ffreqhi.Show := false;
// are there any stats options?

  FOneTable := true;
  FRowsPerVar := 1;
  aStatsOption := false;
// check for stats options
  OptCount := ST.Options.Count;
  for i := 0 to OptCount - 1 do
  case ST.Options[i].Ident of
    'mci',
    'msd',
    'rm',
    'iqr',
    'idr',
    'fb',
    'fh',
    'fl':
      begin
          if not (ST.HasOption('ct')) then
          begin
            FOneTable := false;
            FRowsPerVar := 2;   // default number of rows
          end;
          aStatsOption := true;
        break;
      end;
  end;

  result := FOutputCreator.AddTable;

  result.RowCount := 1;
  result.ColCount := 0;
  Offset          := 0;
  // variable name
  if (FOneTable) then
  begin
    Offset := 1;
    result.ColCount := 1;
    result.Cell[0,0].Text := 'var';
  end;
  // obs and unique values
  //Offset := result.ColCount;
  result.ColCount := Offset + 2;
  result.Cell[Offset, 0].Text := 'obs';
  result.Cell[Offset+1,0].Text := 'unique';
  // missing count
  if (ST.HasOption('m')) then
  begin
    Offset := result.ColCount;
    result.ColCount := Offset + 1;
    result.Cell[Offset, 0].Text := 'missing';
    Fmissing := SetStat(Offset,0);
  end;

  if (not aStatsOption) then
  begin
    // no other options - standard output
    Offset := result.ColCount;
    result.ColCount := Offset + 5;
    Fmean          := SetStat(Offset, 0);
    Fsd            := SetStat(Offset+1, 0);
    Fmin           := SetStat(Offset+2, 0);
    Fmedian        := SetStat(Offset+3, 0);
    Fmax           := SetStat(Offset+4, 0);
    for aStr in aDefaultStats do
    begin
      result.Cell[Offset,0].Text := aStr;
      Offset += 1;
    end;
    exit;
  end;

  // check for other means output
  // if too many for one row, shift to alternate presentation


  if (ST.HasOption('msd')) then    // mean, sd, sum
  begin
    Offset := result.ColCount;
    result.ColCount := Offset + 3;
    result.Cell[Offset, 0].Text := 'sum';
    Fsum := SetStat(Offset, 0);
    result.Cell[Offset+1, 0].Text := 'mean';
    Fmean := SetStat(Offset+1, 0);
    result.Cell[Offset+2, 0].Text := 'sd';
    Fsd := SetStat(Offset + 2, 0);
  end;
  if (ST.HasOption('mci')) then   // mean, ci of mean
  begin
    if not Fmean.Show then
    begin
      Offset := result.ColCount;
      result.ColCount := Offset + 1;
      result.Cell[Offset, 0].Text := 'mean';
      Fmean := SetStat(Offset, 0);
    end;
    Offset := result.ColCount;
    result.ColCount := Offset + 2;
    result.Cell[Offset, 0].Text := 'cfil';
    Fcfil := SetStat(Offset, 0);
    result.Cell[Offset+1, 0].Text := 'cfih';
    Fcfih := SetStat(Offset+1, 0);
  end;
 // how many options with median? decide if they fit on one row or not
 ROffset      := 0;
 aMedianCount := 0;
 if (ST.HasOption('rm')) then aMedianCount := 1;
 if (ST.HasOption('iqr')) then aMedianCount += 1;
 if (ST.HasOption('idr')) then aMedianCount += 1;
 Offset := result.ColCount;
 if (aMedianCount > 0) then
   if ((not FOneTable) and ((Offset + 1 + (aMedianCount * 2)) > 11)) then
   begin
     ROffset := 2;
     Offset  := 0;
     FRowsPerVar := 4;
     result.RowCount := 4;
     if (result.ColCount < (1 + aMedianCount*2)) then result.ColCount := 1 + aMedianCount*2;
   end
   else
     result.ColCount := Offset + 1 + aMedianCount*2;

 result.RowCount := FRowsPerVar;
 if (ST.HasOption('rm')) then
 begin
   result.Cell[Offset,ROffset].Text := 'min';
   result.Cell[Offset+aMedianCount,ROffset].Text := 'median';
   result.Cell[Offset+2*aMedianCount,ROffset].Text := 'max';
   Fmin := SetStat(Offset,ROffset);
   Fmedian := SetStat(Offset+aMedianCount,ROffset);
   Fmax := SetStat(Offset+2*aMedianCount,ROffset);
   Offset += 1;
   aMedianCount -= 1;
 end;

 if (ST.HasOption('idr')) then
 begin
   result.Cell[Offset,ROffset].Text := 'p10';
   result.Cell[Offset+aMedianCount,ROffset].Text := 'median';
   result.Cell[Offset+2*aMedianCount,ROffset].Text := 'p90';
   Fp10 := SetStat(Offset,ROffset);
   FMedian := SetStat(Offset+aMedianCount,ROffset);
   Fp90 := SetStat(Offset+2*aMedianCount,ROffset);
   Offset += 1;
   aMedianCount -= 1;
 end;

 if (ST.HasOption('iqr')) then
  begin
    result.Cell[Offset,ROffset].Text := 'p25';
    result.Cell[Offset+1,ROffset].Text := 'median';
    result.Cell[Offset+2,ROffset].Text := 'p75';
    Fp25 := SetStat(Offset,ROffset);
    FMedian := SetStat(Offset+1,ROffset);
    Fp75 := SetStat(Offset+2,ROffset);
  end;

 {  for i:= 1 to 5 do
  begin
    result.Cell[Offset + i, 0].Text := 'value';
    result.Cell[Offset + i, 1].Text := '(#)';
  end;
 }
 {
 result.SetRowAlignment(0, taRightJustify);
 result.SetRowAlignment(1, taRightJustify);
 result.SetColAlignment(0, taLeftJustify);
 result.SetRowBorders(0, [cbTop]);
 result.SetRowBorders(HeaderRows - 1, [cbBottom]);
 }
end;

procedure TDescribeCommand.DoOutputRows(ST: TCustomVariableCommand; DoMeans: Boolean; T: TOutputTable);

var
  aStr: String;
  CategV, CountV: TEpiField;
  RowIdx, ix, NCat, aCount, aIx, i, j, Offset: Integer;
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  CategIx: array [0..4] of Integer;
  CountIx: array [0..4] of Integer;
  StatFmt: String;

  function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;  // from means.pas
    begin
      if (val = TEpiFloatField.DefaultMissing) then
        Result := TEpiStringField.DefaultMissing
      else
        Result := Format(fmt, [val]);
    end;

begin
  CategV := FFreqData.Categ;
  CountV := FFreqData.Count;

  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  if (FOneTable) then
  begin
    RowIdx     := T.RowCount;
    T.RowCount := RowIdx + FRowsPerVar;
    T.Cell[0, RowIdx].Text := CategV.GetVariableLabel(VariableLabelType);
    T.SetColAlignment(0, taLeftJustify);
    Offset := 1;
  end
  else
  begin
    Offset := 0;
    T.RowCount := FRowsPerVar;
    RowIdx := 1;
    T.Header.Text := CategV.GetVariableLabel(VariableLabelType);
  end;

  // need to use proper row
  NCat := FFreqData.Size;
  T.Cell[Offset,     RowIdx].Text := IntToStr(FFreqData.Sum);
  T.Cell[Offset + 1, RowIdx].Text := IntToStr(NCat);

  if (Fmissing.Show) then
  begin
    aStr := '-';
    if (CategV.IsMissing[FFreqData.Size-1]) then
    begin
      aStr := CountV.AsString[FFreqData.Size-1];
      NCat := FFreqData.Size - 1;
    end;
    T.Cell[FMissing.Col, RowIdx].Text := aStr;
  end;
  aStr := CategV.GetVariableLabel(VariableLabelType);
  // check stats one by one
  if (DoMeans) then with FMeansData do
  begin
    FDecimals  := DecimalFromOption(ST.Options);
    StatFmt    := '%8.' + IntToStr(FDecimals) + 'F';
    if (Fsum.Show)    then T.Cell[Fsum.Col,   RowIdx + Fsum.Row   ].Text := Format(StatFmt, [Sum.AsFloat[0]]);
    if (Fmean.Show)   then T.Cell[Fmean.Col,  RowIdx + Fmean.Row  ].Text := Format(StatFmt, [Mean.AsFloat[0]]);
    if (Fsd.Show)     then T.Cell[Fsd.Col,    RowIdx + Fsd.Row    ].Text := StatFloatDisplay(StatFmt, StdDev.AsFloat[0]);
    if (Fcfih.Show)   then T.Cell[Fcfih.Col,  RowIdx + Fcfih.Row  ].Text := StatFloatDisplay(StatFmt, Cfih.AsFloat[0]);
    if (Fcfil.Show)   then T.Cell[Fcfil.Col,  RowIdx + Fcfil.Row  ].Text := StatFloatDisplay(StatFmt, Cfil.AsFloat[0]);
    if (Fmin.Show)    then T.Cell[Fmin.Col,   RowIdx + Fmin.Row   ].Text := Format(StatFmt, [Min.AsFloat[0]]);
    if (Fp10.Show)    then T.Cell[Fp10.Col,   RowIdx + Fp10.Row   ].Text := Format(StatFmt, [P10.AsFloat[0]]);
    if (Fp25.Show)    then T.Cell[Fp25.Col,   RowIdx + Fp25.Row   ].Text := Format(StatFmt, [P25.AsFloat[0]]);
    if (Fmedian.Show) then T.Cell[Fmedian.Col,RowIdx + Fmedian.Row].Text := Format(StatFmt, [Median.AsFloat[0]]);
    if (Fp75.Show)    then T.Cell[Fp75.Col,   RowIdx + Fp75.Row   ].Text := Format(StatFmt, [P75.AsFloat[0]]);
    if (Fp90.Show)    then T.Cell[Fp90.Col,   RowIdx + Fp90.Row   ].Text := Format(StatFmt, [P90.AsFloat[0]]);
    if (Fmax.Show)    then T.Cell[Fmax.Col,   RowIdx + Fmax.Row   ].Text := Format(StatFmt, [Max.AsFloat[0]]);
  end;
{
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
    T.Cell[Offset + ix, RowIdx + 1].Text := '(' + CountV.AsString[CategIx[ix]] + ')';
  end;
}
T.SetRowBorders(0, [cbTop]);
T.SetRowBorders(T.RowCount-1, [cbBottom]);
end;

{procedure TDescribeCommand.DoOutputMeansRow(Data: TMeansDataFile; ST: TCustomVariableCommand; T: TOutputTable);
var
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  RowIdx: Integer;
  StatFmt: String;

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
}

procedure TDescribeCommand.ExecDescribe(VarNames: TStrings; ST: TCustomVariableCommand);
var
  DF: TEpiDataFile;
  VariableLabelType: TEpiGetVariableLabelType;
  AFieldType: TEpiFieldType;
  AVar: TStrings;
  Opt: TOption;
  i, ix, VarCount, TableIx: Integer;
  SummaryTable: TOutputTable;
  F:   TFreqCommand;
  M:   TMeans;
  O:   TEpiReferenceMap;
  DoOutput: Boolean;
  DoMeans:  Boolean;
begin
  VarCount := VarNames.Count;
  if (VarCount < 1) then
  begin
    // error message (should not happen)
    exit;
  end;
  AVar := TStringList.Create;

  //TODO: do freq on all variables
  //      do means on numeric
  //      one function to create headers
  //      output means/freq uses appropriate columns

  //
  DoOutput := not ST.HasOption('q');
  F := TFreqCommand.Create(FExecutor, FOutputCreator);
  M := TMeans.Create(FExecutor, FOutputCreator);
  if (DoOutput) then
  begin
    FDecimals := DecimalFromOption(ST.Options);
    FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
    FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
    FFirstPass   := true;
  end;
  for i := 0 to VarCount - 1 do
  begin
    if (FFirstPass) then
    begin
      SummaryTable := CreateOutputHeader(ST);
      CreateTemplateFromHeader(SummaryTable);
      TableIx := 0;
    end
    else if (not FOneTable) then
    begin
      SummaryTable := CreateOutputHeaderFromTemplate();
      TableIx := i;
    end;

    AVar.Add(VarNames[i]);
    if ST.HasOption('m') then
      DF := FExecutor.PrepareDatafile(AVar, nil)
    else
      DF := FExecutor.PrepareDatafile(AVar, AVar);
    AFieldType := DF.Field[0].FieldType;
    DoMeans  := (AFieldType = ftInteger) or (AfieldType = ftFloat) or (AFieldType = ftAutoInc);
    if (DF.Size = 0) then
      begin
        FOutputCreator.DoWarning(VarNames[i] + ': No data');
      end
    else begin
      FFreqData := F.CalcFreq(DF, VarNames[i], O);

      if (DoMeans) then
      begin
        DF.Free;
        DF := FExecutor.PrepareDatafile(AVar, AVar);
        FMeansData := M.CalcMeans(DF, VarNames[i], '');
      end;
      if (DoOutput) then
        DoOutputRows(ST, DoMeans, SummaryTable);
    end;
    DF.Free;
    FFreqData.Free;
    AVar.Delete(0);
    if (DoMeans) then FMeansData.Free;
    FFirstPass := false;
  end;
  F.Free;
  M.Free;
end;

end.

