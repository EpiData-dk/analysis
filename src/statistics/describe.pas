unit describe;

{$mode objfpc}{$H+}

{ EpiData Analysis Describe command
  Jamie Hockin
  2019/02/25
  For command syntax, see commands.html

  Describe provides brief descriptive statistics on one or more variables. It has no inbuilt
  statistical code, but instead makes use of the freq and means commands to generate statistics.

  Rather than write new procedures within freq and means to provide output, it is simpler
  to do it here. Most of the code here is to determine where in the output to place statistics.

  The general approach is to call execfreq for each variable, call execmeans for numeric variables
  and then place certain results into a templated output table, or rows of a single output table.

  freq and means are used to create result variables. As a consequence, all result variables
  from the two commands are available to the user.
}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  executor, result_variables, epifields_helper, outputcreator, options_utils,
  ana_globals, freq, means, math;

type
  {ShowStat}

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
  protected
    FFreqData: TFreqDataFile;
    FMeansData: TMeansDataFile;
    function CreateOutputHeader(ST: TCustomVariableCommand): TOutputTable;
    function CreateOutputHeaderFromTemplate(DoMeans: Boolean): TOutputTable;
    procedure CreateTemplateFromHeader(T: TOutputTable);
    procedure DoOutputRows(ST: TCustomVariableCommand; DoMeans: Boolean; T: TOutputTable);
    procedure DoOutputFreq(ST: TCustomVariableCommand; T: TOutputTable);
    procedure PrepareResultVariables(VarNames: TStrings);
    procedure DoResultVariables(Index: Integer; DoMeans: Boolean);
// add other stats here as necessary
  protected
    Fmissing,                                       // !m
    Fsum, Fmean, Fsd, Fcfil, Fcfih,                 // !msd !mci
    Fmin, Fp10, Fp25, Fmedian, Fp75, Fp90, Fmax,    // !rm !idr !iqr
    Ffreqlo, Ffreqhi: FShowStat;                    // !fl !fh !fb
    FCategV, FCountV: TEpiField;
// result variable vectors
    RVarName, Rcount, Rcat, RMissing,
    Rsum, Rmean, Rsd, Rcfil, Rcfih,
    Rmin, Rp10, Rp25, Rmedian, Rp75, Rp90, Rmax: TExecVarVector;
    Robslo, Robshi, Rvallo, Rvalhi: TExecVarMatrix;
// output control
    FDecimals: Integer;
    FStatFmt, FFreqFmt, FFreqLabel: String;
    FPct: Boolean;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FRowsPerVar: Integer;
    FOneTable, FFreqTable: Boolean;
    FTemplate: Array of Array of UTF8String;
    FFirstPass: Boolean;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor  Destroy; override;
    procedure   ExecDescribe(VarNames: TStrings; ST: TCustomVariableCommand);
  end;


implementation

constructor TDescribeCommand.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TDescribeCommand.Destroy;
begin
  inherited Destroy;
end;

function TDescribeCommand.CreateOutputHeaderFromTemplate(DoMeans: Boolean): TOutputTable;
// copy headings for a single output table
var
  i, j: Integer;
begin
  result := FOutputCreator.AddTable;
  if (DoMeans) then
  begin
    result.RowCount := FRowsPerVar;
    result.ColCount := high(FTemplate) + 1;
  end
  else
  begin
    result.RowCount := 2;
    if (Fmissing.Show) then
      result.ColCount := 3
    else
      result.ColCount := 2;
  end;

  for i := 0 to result.ColCount-1 do
    for j := 0 to result.RowCount-1 do
      result.Cell[i,j].Text := FTemplate[i,j];
end;

procedure TDescribeCommand.CreateTemplateFromHeader(T: TOutputTable);
// copy headings from a single output table
var
  i, j: Integer;
begin
  setLength(FTemplate,T.ColCount,FRowsPerVar);
  for i := 0 to T.ColCount - 1 do
    for j := 0 to FRowsPerVar - 1 do
      FTemplate[i,j] := T.Cell[i,j].Text;
end;

function TDescribeCommand.CreateOutputHeader(ST: TCustomVariableCommand): TOutputTable;

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
  Offset, ROffset, i: Integer;
  aStr: UTF8String;
  aOptCount, aMedianCount, aMaxColumnsForOneTable: Integer;
  aStatsOption, aFreqOption: Boolean;
  aDefaultStats: array [1..5] of UTF8String = ('mean','sd','min','median','max');
  aHasMedian:    array [0..2] of UTF8String = ('rm','idr','iqr');

  function SetStat(Col, Row: Integer): FShowStat;
  // save location of stats output to match header
  begin
    result.Show := true;
    result.Row  := Row;
    result.Col  := Col;
  end;

begin
  // process output options
  FPct     := ST.HasOption('pc');
  FDecimals:= DecimalFromOption(ST.Options);
  FStatFmt := '%8.' + IntToStr(FDecimals) + 'F';
  if (FPct) then
  begin
    FFreqFmt := '%8.' + IntToStr(FDecimals) + 'F%%';
    FFreqLabel := 'percent'
  end
  else
  begin
    FFreqFmt := '%8.0F';
    FFreqLabel := 'count';
  end;
  FDecimals            := DecimalFromOption(ST.Options);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  // now count up columns to show
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

  // starting position
  FOneTable              := true;
  aMaxColumnsForOneTable := 11;
  FFreqTable             := false;
  FRowsPerVar            := 1;
  aStatsOption           := false;
  aFreqOption            := false;

  // check for stats options
  aOptCount := ST.Options.Count;
  for i := 0 to aOptCount - 1 do
  case ST.Options[i].Ident of
    'mci',
    'msd',
    'rm',
    'iqr',
    'idr':
      begin
        if not (ST.HasOption('ct')) then
        begin
          FOneTable := false;
          FRowsPerVar := 2;   // default number of rows for a single table per var
        end;
        aStatsOption := true;
      end;
    'fb',
    'fh',
    'fl' :
      begin
        FOneTable := false;
        FFreqTable := true;
        FRowsPerVar := 2;
        aFreqOption := true;
      end;
  end;

  // set up output table
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
  result.ColCount := Offset + 2;
  result.Cell[Offset,   0].Text := 'obs';
  result.Cell[Offset+1, 0].Text := 'unique';

  // missing count
  if (ST.HasOption('m')) then
  begin
    Offset := result.ColCount;
    result.ColCount := Offset + 1;
    result.Cell[Offset, 0].Text := 'missing';
    Fmissing := SetStat(Offset,0);
  end;

  if (not (aStatsOption or aFreqOption)) then
  begin
    // no other options - standard output
    Offset := result.ColCount;
    result.ColCount := Offset + 5;
    Fmean           := SetStat(Offset, 0);
    Fsd             := SetStat(Offset+1, 0);
    Fmin            := SetStat(Offset+2, 0);
    Fmedian         := SetStat(Offset+3, 0);
    Fmax            := SetStat(Offset+4, 0);
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
    result.Cell[Offset,   0].Text := 'sum';
    result.Cell[Offset+1, 0].Text := 'mean';
    result.Cell[Offset+2, 0].Text := 'sd';
    Fsum  := SetStat(Offset, 0);
    Fmean := SetStat(Offset+1, 0);
    Fsd   := SetStat(Offset+2, 0);
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
    result.Cell[Offset,   0].Text := '[' + FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL] + '%';//'cfil';
    result.Cell[Offset+1, 0].Text := 'CI]';//'cfih';
    Fcfil := SetStat(Offset, 0);
    Fcfih := SetStat(Offset+1, 0);
  end;

  // how many options with median? decide if they fit on one row or not
 ROffset      := 0;
 aMedianCount := 0;
 for aStr in (aHasMedian) do
   if (ST.HasOption(aStr)) then aMedianCount += 1;
 Offset := result.ColCount;
 if (aMedianCount > 0) then
   if ((not FOneTable) and ((Offset + 1 + (aMedianCount * 2)) > aMaxColumnsForOneTable)) then
   begin
     ROffset := 2;
     Offset  := 0;
     FRowsPerVar := 4;
//     result.RowCount := 4;
     if (result.ColCount < (1 + aMedianCount*2)) then result.ColCount := 1 + aMedianCount*2;
   end
   else
     result.ColCount := Offset + 1 + aMedianCount*2;

 result.RowCount := FRowsPerVar;
 if (ST.HasOption('rm')) then
 begin
   result.Cell[Offset,               ROffset].Text := 'min';
   result.Cell[Offset+aMedianCount,  ROffset].Text := 'median';
   result.Cell[Offset+2*aMedianCount,ROffset].Text := 'max';
   Fmin    := SetStat(Offset,ROffset);
   Fmedian := SetStat(Offset+aMedianCount,ROffset);
   Fmax    := SetStat(Offset+2*aMedianCount,ROffset);
   Offset  += 1;
   aMedianCount -= 1;
 end;

 if (ST.HasOption('idr')) then
 begin
   result.Cell[Offset,              ROffset].Text := 'p10';
   result.Cell[Offset+aMedianCount, ROffset].Text := 'median';
   result.Cell[Offset+2*aMedianCount,ROffset].Text := 'p90';
   Fp10    := SetStat(Offset,ROffset);
   FMedian := SetStat(Offset+aMedianCount,ROffset);
   Fp90    := SetStat(Offset+2*aMedianCount,ROffset);
   Offset  += 1;
   aMedianCount -= 1;
 end;

 if (ST.HasOption('iqr')) then
  begin
    result.Cell[Offset,  ROffset].Text := 'p25';
    result.Cell[Offset+1,ROffset].Text := 'median';
    result.Cell[Offset+2,ROffset].Text := 'p75';
    Fp25    := SetStat(Offset,  ROffset);
    FMedian := SetStat(Offset+1,ROffset);
    Fp75    := SetStat(Offset+2,ROffset);
  end;

// no need to set up template for high and low frequencies
// as they always force one table per var and have their own output tables
end;

procedure TDescribeCommand.DoOutputRows(ST: TCustomVariableCommand; DoMeans: Boolean; T: TOutputTable);
// add a single variable stats to the current output table
var
//  aStr: String;
  RowIdx, NCat, Offset, i, mCount: Integer;
  VariableLabelType: TEpiGetVariableLabelType;
//  ValueLabelType: TEpiGetValueLabelType;

  function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;  // from means.pas
    begin
      if (val = TEpiFloatField.DefaultMissing) then
        Result := TEpiStringField.DefaultMissing
      else
        Result := Format(fmt, [val]);
    end;

begin

  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
//  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  if (FOneTable) then
  begin
    RowIdx     := T.RowCount;
    T.RowCount := RowIdx + FRowsPerVar;
    T.Cell[0, RowIdx].Text := FCategV.GetVariableLabel(VariableLabelType);
    T.SetColAlignment(0, taLeftJustify);
    Offset := 1;
  end
  else
  begin
    Offset        := 0;
    RowIdx        := 1;
    T.Header.Text := FCategV.GetVariableLabel(VariableLabelType);
    T.RowCount    := FRowsPerVar;
  end;

  // always show total count and number of unique values
  NCat := FFreqData.Size;
  T.Cell[Offset,   RowIdx].Text := IntToStr(FFreqData.Sum);
  T.Cell[Offset+1, RowIdx].Text := IntToStr(NCat);
  Offset += 2;

  if (Fmissing.Show) then
  begin
    mCount := 0;
    // loop through all categories here to find missing
    for i := 0 to FFreqData.Size-1 do
      if ((FCategV.IsMissing[i]) or (FCategV.IsMissingValue[i])) then
        mCount += FCountV[i];
    T.Cell[FMissing.Col, RowIdx].Text := IntToStr(mCount);
    Offset += 1;
  end;

//  aStr := FCategV.GetVariableLabel(VariableLabelType);

  // check stats one by one
  if (DoMeans) then with FMeansData do
  begin
    if (Fsum.Show)    then T.Cell[Fsum.Col,   RowIdx + Fsum.Row   ].Text := Format(FStatFmt, [Sum.AsFloat[0]]);
    if (Fmean.Show)   then T.Cell[Fmean.Col,  RowIdx + Fmean.Row  ].Text := trim(Format(FStatFmt, [Mean.AsFloat[0]]));
    if (Fsd.Show)     then T.Cell[Fsd.Col,    RowIdx + Fsd.Row    ].Text := trim(StatFloatDisplay(FStatFmt, StdDev.AsFloat[0]));
    if (Fcfih.Show)   then T.Cell[Fcfih.Col,  RowIdx + Fcfih.Row  ].Text := trim(StatFloatDisplay(FStatFmt, Cfih.AsFloat[0])) + ']';
    if (Fcfil.Show)   then T.Cell[Fcfil.Col,  RowIdx + Fcfil.Row  ].Text := '[' + trim(StatFloatDisplay(FStatFmt, Cfil.AsFloat[0]));
    if (Fmin.Show)    then T.Cell[Fmin.Col,   RowIdx + Fmin.Row   ].Text := Format(FStatFmt, [Min.AsFloat[0]]);
    if (Fp10.Show)    then T.Cell[Fp10.Col,   RowIdx + Fp10.Row   ].Text := Format(FStatFmt, [P10.AsFloat[0]]);
    if (Fp25.Show)    then T.Cell[Fp25.Col,   RowIdx + Fp25.Row   ].Text := Format(FStatFmt, [P25.AsFloat[0]]);
    if (Fmedian.Show) then T.Cell[Fmedian.Col,RowIdx + Fmedian.Row].Text := Format(FStatFmt, [Median.AsFloat[0]]);
    if (Fp75.Show)    then T.Cell[Fp75.Col,   RowIdx + Fp75.Row   ].Text := Format(FStatFmt, [P75.AsFloat[0]]);
    if (Fp90.Show)    then T.Cell[Fp90.Col,   RowIdx + Fp90.Row   ].Text := Format(FStatFmt, [P90.AsFloat[0]]);
    if (Fmax.Show)    then T.Cell[Fmax.Col,   RowIdx + Fmax.Row   ].Text := Format(FStatFmt, [Max.AsFloat[0]]);
  end;

  T.SetRowBorders(0, [cbTop]);
  if (FOneTable) then
    T.SetRowBorders(0, [cbTop, cbBottom])
  else
  begin
    T.SetRowBorders(1, [cbBottom]);
    if (T.RowCount > 2) then T.SetRowBorders(T.RowCount-1, [cbBottom]);
  end;
end;

procedure TDescribeCommand.DoOutputFreq(ST: TCustomVariableCommand; T: TOutputTable);
// output high / low frequencies as a new output table
var
  ValueLabelType: TEpiGetValueLabelType;
  RowIdx, NCat, DCat, Offset: Integer;
  ix: Integer;
  FoundHighFreq: Boolean;
  aStr: String;
//  aPct: EpiFloat;
begin
  if (FPct) then
    FCountV := FFreqData.Percent;
  NCat   := FCategV.Size;

  ValueLabelType := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FoundHighFreq  := false;
  RowIdx         := 0;
  T.ColCount     := 7;

  // use header to subtitle high / low frequencies
  aStr := '';
  if (ST.HasOption('fh') or ST.HasOption('fb')) then aStr := 'Most ';
  if (ST.HasOption('fb') or ST.HasOption('fl')) then
    if (aStr <> '') then aStr += 'and Least '
      else aStr := 'Least ';
  if (aStr <> '') then T.Header.Text := aStr + 'frequent values';

 // if (ST.HasOption('m') and FCategV.IsMissing[NCat-1])
 //   then NCat -= 1;
  if (ST.HasOption('fh') or ST.HasOption('fb')) then
  begin
    FoundHighFreq := true;
    T.RowCount    := RowIdx + 2;
    T.Cell[0,RowIdx].Text   := 'value';
    T.Cell[0,RowIdx+1].Text := FFreqLabel;

   // now show the top 5 frequencies
    Offset := 1;
    DCat   := max(0, NCat-5);
    for ix := NCat-1 downto DCat do
    begin
      T.Cell[Offset, RowIdx    ].Text := FCategV.GetValueLabel(ix, ValueLabelType);
      T.Cell[Offset, RowIdx + 1].Text := Format(FFreqFmt, [FCountV.AsFloat[ix]]);
      Offset += 1;
    end;
    RowIdx := 2;
  end;

  if ((not FoundHighFreq) or (Ncat >5)) then // no need for this if all frequencies already shown
  begin
  // show low 5 frequencies (fewer if some already shown as high frequencies)
    if (ST.HasOption('fl') or ST.HasOption('fb')) then
    begin
      T.RowCount := RowIdx + 2;
      T.Cell[0,RowIdx].Text   := 'value';
      T.Cell[0,RowIdx+1].Text := FFreqLabel;
      if (FoundHighFreq) then
        if (NCat > 10) then
          DCat := 4
        else
          DCat := NCat - 6
      else
        if (NCat < 6) then
          DCat := NCat - 1
        else
          DCat := 4;
      // now show the bottom 5 frequencies
      Offset := 1;
      for ix := 0 to DCat do
      begin
        T.Cell[Offset, RowIdx    ].Text := FCategV.GetValueLabel(ix, ValueLabelType);
        T.Cell[Offset, RowIdx + 1].Text := Format(FFreqFmt, [FCountV.AsFloat[ix]]);
        Offset += 1;
      end;
    end;
  end;
  T.SetRowBorders(0, [cbTop]);
  T.SetRowBorders(1, [cbBottom]);
  if (T.RowCount > 2) then T.SetRowBorders(T.RowCount-1, [cbBottom]);

end;

procedure TDescribeCommand.PrepareResultVariables(VarNames: TStrings);
var
  i: Integer;
  Prefix: UTF8String;
  ResultRows: Integer;
begin
  Prefix := '$describe_';
  FExecutor.ClearResults('$describe');
  ResultRows := VarNames.Count;
  FExecutor.AddResultConst(Prefix + 'nvar', ftInteger).AsIntegerVector[0] := VarNames.Count;
  RVarName := FExecutor.AddResultVector(Prefix + 'varname', ftString, ResultRows);
  for i := 0 to ResultRows - 1 do
    begin
      RVarName.AsStringVector[i] := VarNames[i];
    end;
  // all result variables
  Rcount   := FExecutor.AddResultVector(Prefix + 'obs',     ftInteger, ResultRows);
  Rcat     := FExecutor.AddResultVector(Prefix + 'unique',  ftInteger, ResultRows);
  Rmissing := FExecutor.AddResultVector(Prefix + 'missing', ftInteger, ResultRows);
  Rsum     := FExecutor.AddResultVector(Prefix + 'sum',     ftFloat,   ResultRows);
  Rmean    := FExecutor.AddResultVector(Prefix + 'mean',    ftFloat,   ResultRows);
  Rsd      := FExecutor.AddResultVector(Prefix + 'sd',      ftFloat,   ResultRows);
  Rcfil    := FExecutor.AddResultVector(Prefix + 'cfil',    ftFloat,   ResultRows);
  Rcfih    := FExecutor.AddResultVector(Prefix + 'cfih',    ftFloat,   ResultRows);
  Rmin     := FExecutor.AddResultVector(Prefix + 'min',     ftFloat,   ResultRows);
  Rp10     := FExecutor.AddResultVector(Prefix + 'p10',     ftFloat,   ResultRows);
  Rp25     := FExecutor.AddResultVector(Prefix + 'p25',     ftFloat,   ResultRows);
  Rmedian  := FExecutor.AddResultVector(Prefix + 'median',  ftFloat,   ResultRows);
  Rp75     := FExecutor.AddResultVector(Prefix + 'p75',     ftFloat,   ResultRows);
  Rp90     := FExecutor.AddResultVector(Prefix + 'p90',     ftFloat,   ResultRows);
  Rmax     := FExecutor.AddResultVector(Prefix + 'max',     ftFloat,   ResultRows);
  Robslo   := FExecutor.AddResultMatrix(Prefix + 'obslo',   ftInteger, ResultRows, 5);
  Robshi   := FExecutor.AddResultMatrix(Prefix + 'obshi',   ftInteger, ResultRows, 5);
  Rvallo   := FExecutor.AddResultMatrix(Prefix + 'vallo',   ftString,  ResultRows, 5);
  Rvalhi   := FExecutor.AddResultMatrix(Prefix + 'valhi',   ftString,  ResultRows, 5);

end;

procedure TDescribeCommand.DoResultVariables(Index: Integer; DoMeans: Boolean);
var
  i, NCat, DCat: Integer;

begin
  Rcount.AsIntegerVector[Index] := FFreqData.Sum;
  Rcat.AsIntegerVector[Index]   := FFreqData.Size;
  if (FFreqData.Categ.IsMissing[FFreqData.Size-1]) then
    Rmissing.AsIntegerVector[Index] := FFreqData.Count[FFreqData.Size-1].MaxValue;

  if (DoMeans) then
  begin
    Rsum.AsFloatVector[Index]     := FMeansData.Sum.AsFloat[0];
    Rmean.AsFloatVector[Index]    := FMeansData.Mean.AsFloat[0];
    Rsd.AsFloatVector[Index]      := FMeansData.StdDev.AsFloat[0];
    Rcfil.AsFloatVector[Index]    := FMeansData.Cfil.AsFloat[0];
    Rcfih.AsFloatVector[Index]    := FMeansData.Cfih.AsFloat[0];
    Rmin.AsFloatVector[Index]     := FMeansData.Min.AsFloat[0];
    Rp10.AsFloatVector[Index]     := FMeansData.P10.AsFloat[0];
    Rp25.AsFloatVector[Index]     := FMeansData.P25.AsFloat[0];
    Rmedian.AsFloatVector[Index]  := FMeansData.Median.AsFloat[0];
    Rp75.AsFloatVector[Index]     := FMeansData.P75.AsFloat[0];
    Rp90.AsFloatVector[Index]     := FMeansData.P90.AsFloat[0];
    Rmax.AsFloatVector[Index]     := FMeansData.Max.AsFloat[0];
  end;

  FFreqData.SortRecords(FFreqData.Count); // sorts counts into ascending order
  FCategV := FFreqData.Categ;
  FCountV  := FFreqData.Count;
  NCat := FCategV.Size - 1;
  DCat := Min(4, NCat);
  for i:= 0 to DCat do
  begin
    Robshi.AsIntegerMatrix[Index, i] := FCountV.AsInteger[NCat - i];
    Rvalhi.AsStringMatrix[Index, i]  := FCategV.AsString[NCat - i];
    Robslo.AsIntegerMatrix[Index, i] := FCountV.AsInteger[i];
    Rvallo.AsStringMatrix[Index, i]  := FCategV.AsString[i];
  end;

end;

procedure TDescribeCommand.ExecDescribe(VarNames: TStrings; ST: TCustomVariableCommand);
var
  DF: TEpiDataFile;
  AFieldType: TEpiFieldType;
  AVar: TStrings;
  i, VarCount: Integer;
  SummaryTable, FreqTable: TOutputTable;
  F:   TFreqCommand;
  M:   TMeans;
  O:   TEpiReferenceMap;
  DoOutput, DoMeans:  Boolean;
begin
  FFirstPass := true;
  VarCount   := VarNames.Count;
  if (VarCount < 1) then
  begin
    // no error message (should not happen)
    exit;
  end;

  PrepareResultVariables(VarNames);

  AVar     := TStringList.Create;
  DoOutput := not ST.HasOption('q');
  F        := TFreqCommand.Create(FExecutor, FOutputCreator);
  M        := TMeans.Create(FExecutor, FOutputCreator);

  for i := 0 to VarCount - 1 do
  begin
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
    else
    begin
      FFreqData := F.CalcFreq(DF, VarNames[i], O);
      if (DoMeans) then
      begin
        if (ST.HasOption('m')) then      // must recreate DF without missing vars for means
        begin
          DF.Free;
          DF := FExecutor.PrepareDatafile(AVar, AVar);
        end;
        if (DF.Size = 0) then
          DoMeans := false
        else
          FMeansData := M.CalcMeans(DF, VarNames[i], '');
      end;

      DoResultVariables(i, DoMeans);

      if (DoOutput) then
      begin
        if (FFirstPass) then
        begin
          SummaryTable := CreateOutputHeader(ST);
          CreateTemplateFromHeader(SummaryTable);
        end
        else if (not FOneTable) then
        begin
          SummaryTable := CreateOutputHeaderFromTemplate(DoMeans);
        end;

        DoOutputRows(ST, DoMeans, SummaryTable);
        if (FFreqTable) then
        begin
          FreqTable := FOutputCreator.AddTable;
          DoOutputFreq(ST,FreqTable);
        end;
      end;

      FFreqData.Free;
      if (DoMeans) then FMeansData.Free;
      FFirstPass := false;
    end;
    DF.Free;
    AVar.Delete(0);
  end;
  F.Free;
  M.Free;
end;

end.

