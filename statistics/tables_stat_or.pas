unit tables_stat_or;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, epidatafiles,
  epifields_helper, options_utils, result_variables, executor, ast;

type

  { TTwoWayStatisticOR }

  TTwoWayStatisticOR = class(TTwoWayStatistic)
  private
    FConf: Integer;
    FMName: String;
    FOddsRatio: EpiFloat;
    FOddsRatioLL, FOddsRatioUL: EpiFloat;
    FMessage: UTF8String;
    FOrgTable: TTwoWayTable;
  public
    property Message: UTF8String read FMessage;
    procedure CalcTable(Table: TTwoWayTable;Conf: Integer); override;
    procedure AddToOutput(OutputTable: TOutputTable; Options: TOptionList); override;
    procedure CreateResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
  end;

  { TTwoWayStatisticsOR }

  TTwoWayStatisticsOR = class(TTwoWayStatistics)
  private
    FConf: Integer;
    FMHOR: EpiFloat;
    FMHORLL, FMHORUL: EpiFloat;
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatisticOR; override;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList); override;
    procedure AddToCompactTable(Executor: TExecutor; T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList); override;
    procedure AddToCompactHeader(T: TOutputTable; Options: TOptionList); override;
    procedure CalcSummaryStatistics(Tables: TTwoWayTables;Conf: Integer); override;
    procedure CreateSummaryResultVariables(Executor: TExecutor; const NamePrefix: UTF8STring); override;
    function  CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String; ResultRows: Integer): TStatResult; override;
    procedure AddCompactResultVariables(Executor: TExecutor; Index: Integer; Results: TStatResult); override;
    property Statistics[Const Index: Integer]: TTwoWayStatisticOR read GetStatistics;
  end;


implementation

uses
  tables, epimiscutils, generalutils, statfunctions, Math, ana_globals;

{ CalcTable}

procedure TTwoWayStatisticOR.CalcTable(Table: TTwoWayTable; Conf: Integer);

var
  a, b, c, d, n: Integer;
  p, q, r, s, f1, f2, f3: EpiFloat;
  Zconf: EpiFLoat;
begin
  FMName       := 'Odds Ratio';
  FOrgTable    := Table;
  FOddsRatio   := TEpiFloatField.DefaultMissing;
  FOddsRatioUL := TEpiFloatField.DefaultMissing;
  FOddsRatioLL := TEpiFloatField.DefaultMissing;

  if (FOrgTable.ColCount <> 2) or (FOrgTable.RowCount <> 2 ) then
  begin
    FMessage := 'Table is not 2x2.';
    exit;
  end;
  Zconf := PNormalInv((1 - (Conf / 100)) / 2);
  FConf := Conf; // save confidence level for output
  a := FOrgTable.Cell[0,0].N;
  b := FOrgTable.Cell[1,0].N;
  c := FOrgTable.Cell[0,1].N;
  d := FOrgTable.Cell[1,1].N;

  if ((a * d) = 0) and ((b * c) = 0) then
  begin
    FMessage   := 'Table has a zero marginal.';
    exit
  end
  else
    FOddsRatio := (a * d) / (b * c);

  // GreenlandRobins confidence limits
  n := FOrgTable.Total;
  p := (a + d) / n;
  q := (b + c) / n;
  r := a * d / n;
  s := b * c / n;
  f1 := 0;
  f2 := 0;
  f3 := 0;
  if (r <> 0) then f1 := (p * r) / (2 * r * r);
  if ((r * s) <> 0) then f2 := ((p * s) + (q * r)) / (2 * r * s);
  if (s <> 0) then f3 := (q * s) / (2 * s * s);
  if (f2 = 0) then exit
  else
  begin
    FOddsRatioLL := exp(ln(FOddsRatio) - (Zconf  * sqrt(f1 + f2 + f3)));
    FOddsRatioUL := exp(ln(FOddsRatio) + (Zconf  * sqrt(f1 + f2 + f3)));
  end;
end;

procedure TTwoWayStatisticOR.AddToOutput(OutputTable: TOutputTable; Options: TOptionList);
var
  S: String;
begin
  if (FOddsRatio = TEpiFloatField.DefaultMissing) then
    S := 'Cannot estimate the ' + FMName + '. '+ FMessage
  else
  begin
    S := FMName + ': '+ FormatRatio(FOddsRatio,Options);
    if (FOddsRatioLL <> TEpiFloatField.DefaultMissing) then
       S += ' ' + FormatCI(FOddsRatioLL, FOddsRatioUL, FConf, Options);
    OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + S;
  end;
end;

procedure TTwoWayStatisticOR.CreateResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);
begin
  if (FOddsRatio = TEpiFloatField.DefaultMissing) then exit;
  inherited CreateResultVariables(Executor, NamePrefix);

  Executor.AddResultConst(NamePrefix + 'OR', ftFloat).AsFloatVector[0] := FOddsRatio;
end;

function TTwoWayStatisticsOR.GetStatistics(const Index: Integer): TTwoWayStatisticOR;
begin
  result := TTwoWayStatisticOR(inherited GetStatistics(Index));
end;

function TTwoWayStatisticsOR.GetTwoWayStatisticClass: TTwoWayStatisticClass;
begin
  result := TTwoWayStatisticOR;
end;

procedure TTwoWayStatisticsOR.AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList);
var
  ColIdx, i: Integer;
  Stat: TTwoWayStatisticOR;
  S: string;
begin
  Stat := Statistics[0];
  with Stat do
  begin
   if (FOddsRatio = TEpiFloatField.DefaultMissing) then exit;

    ColIdx := OutputTable.ColCount;
    OutputTable.ColCount := OutputTable.ColCount + 2;
    OutputTable.Cell[ColIdx    , 0].Text := 'Odds Ratio';
    OutputTable.Cell[ColIdx + 1, 0].Text := IntToStr(FConf) + '% CI';
    OutputTable.Cell[ColIdx    , 1].Text := FormatRatio(FOddsRatio, Options);
    OutputTable.Cell[ColIdx + 1, 1].Text := FormatCI(FOddsRatioLL, FOddsRatioUL, 0, Options);
  end;
  if (StatisticsCount = 1) then begin
     OutputTable.Cell[ColIdx    , 2].Text := '-';   // will be replaced by M-H OR
     exit;
  end;

  for i := 1 to StatisticsCount - 1 do   // skips unstratified table
  begin
    Stat := Statistics[i];
    with Stat do
      begin
      if (FOddsRatio = TEpiFloatField.DefaultMissing) then
      begin
        OutputTable.Cell[ColIdx,     2].Text := '-';
        OutputTable.Cell[ColIdx + 1, 2].Text := '';
      end
      else
      begin
        OutputTable.Cell[ColIdx,     i + 2].Text := FormatRatio(FOddsRatio, Options);
        if (FOddsRatioLL <> TEpiFloatField.DefaultMissing) then
          OutputTable.Cell[ColIdx + 1, i + 2].Text := FormatCI(FOddsRatioLL, FOddsRatioUL, 0, Options);
      end;
    end;
  end;
  if (isInfinite(FMHOR)) then exit;
  OutputTable.Cell[ColIdx,     2].Text := FormatRatio(FMHOR, Options);
  OutputTable.Cell[ColIdx + 1, 2].Text := FormatCI(FMHORLL, FMHORUL, 0, Options);
end;

procedure TTwoWayStatisticsOR.AddToCompactHeader(T: TOutputTable; Options: TOptionList);
var
  ColIdx: Integer;
  Stat: TTwoWayStatisticOR;

begin
  Stat := Statistics[0];
  if (T.RowCount <> 2) then exit;
  ColIdx                      := T.ColCount;
  T.ColCount                  := ColIdx + 2;
  T.Cell[ColIdx     , 1].Text := 'OR';
  T.Cell[ColIdx + 1 , 1].Text := IntToStr(Stat.FConf) + '% CI';
  T.SetColAlignment(ColIdx + 1, taCenter);  // just header alignment

end;

procedure TTwoWayStatisticsOR.AddToCompactTable(Executor: TExecutor;
         T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList);
var
  i: Integer;
  Stat: TTwoWayStatisticOR;

begin
  Stat := Statistics[0];
  with Stat do
  begin
    if (Message <> '') then
      T.Footer.Text := T.Footer.Text + LineEnding +
                       T.Cell[0,RowIdx].Text + ': ' + Message;

    if (StatisticsCount = 1) then
    // unstratified - crude result
    begin
      if (FOddsRatio = TEpiFloatField.DefaultMissing) then
      begin
        T.Cell[ColIdx    , RowIdx].Text := '-';
        T.Cell[ColIdx + 1, RowIdx].Text := '';
      end
      else
      begin
        T.Cell[ColIdx    , RowIdx].Text := FormatRatio(FOddsRatio, Options);
        if (IsInfinite(FOddsRatio)) then exit;
        T.Cell[ColIdx + 1, RowIdx].Text := FormatCI(FOddsRatioLL, FOddsRatioUL, 0, Options);
      end;
      exit;
    end;
  end;
  // stratified - summary result
  if (FMHOR = TEpiFLoatField.DefaultMissing) then exit;
  T.Cell[ColIdx    , RowIdx].Text := FormatRatio(FMHOR, Options);
  if (IsInfinite(FMHOR)) then exit;
  T.Cell[ColIdx + 1, RowIdx].Text := FormatCI(FMHORLL, FMHORUL, 0, Options);
end;

procedure TTwoWayStatisticsOR.CreateSummaryResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);

begin
  if (StatisticsCount = 1) or (isInfinite(FMHOR)) then exit;   // No stratified tables

  Executor.AddResultConst(NamePrefix + 'MHOR',   ftFloat).AsFloatVector[0] := FMHOR;
  Executor.AddResultConst(NamePrefix + 'MHORLL', ftFloat).AsFloatVector[0] := FMHORLL;
  Executor.AddResultConst(NamePrefix + 'MHORUL', ftFloat).AsFloatVector[0] := FMHORUL;

end;

procedure TTwoWayStatisticsOR.CalcSummaryStatistics(Tables: TTwoWayTables;Conf: Integer);
var
  a, b, c, d, n: Integer;
  Tab: TTwoWayTable;
  p, q, r, s,
  SumR, SumS, SumRS, SumPR, SumPSQR, SumSQ: EpiFloat;
  Zconf, variance: EpiFloat;
  t: String;

  begin
  if (StatisticsCount = 1) then exit;   // No stratified tables
  Zconf     := PNormalInv((1 - (Conf / 100)) / 2);
  FConf     := Conf;
  SumR      := 0;
  SumS      := 0;
  SumRS     := 0;
  SumPR     := 0;
  SumPSQR   := 0;
  SumSQ     := 0;

  for Tab in Tables do
  begin
    a := Tab.Cell[0,0].N;
    b := Tab.Cell[1,0].N;
    c := Tab.Cell[0,1].N;
    d := Tab.Cell[1,1].N;
    n := Tab.Total;
    if (n > 0) then
    begin
      p := (a + d) / n;
      q := 1 - p;
      r := (a * d) / n;
      s := (b * c) / n;
      SumR    += r;
      SumS    += s;
      SumRS   += r*s;
      SumPR   += p*r;
      SumSQ   += s*q;
      SumPSQR += (p*s) + (q*r);
      end;
   end;
  FMHOR := SumR / SumS;

  // Estimate upper and lower confidence limits
  { NIST reference has same formula
    https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/mantel.htm
  }
  if ((SumR * SumS) = 0) or (isInfinite(FMHOR)) then exit;

  variance := abs((SumPR/(SumR * SumR)) + (SumPSQR/(SumR * SumS)) + (SumSQ / (SumS * Sums))) / 2;
  FMHORLL := exp(ln(FMHOR) - (Zconf * sqrt(variance)));
  FMHORUL := exp(ln(FMHOR) + (Zconf * sqrt(variance)));
end;

function TTwoWayStatisticsOR.CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String;
         ResultRows: Integer): TStatResult;

begin
  setlength(Result,3);
  Result[0] := Executor.AddResultVector(Prefix + 'or', ftFloat, ResultRows);
  Result[1] := Executor.AddResultVector(Prefix + 'orll', ftFloat, ResultRows);
  Result[2] := Executor.AddResultVector(Prefix + 'orul', ftFloat, ResultRows);
end;

procedure TTwoWayStatisticsOR.AddCompactResultVariables(Executor: TExecutor;
          Index: Integer; Results: TStatResult);
var
  Stat: TTwoWayStatisticOR;
begin
  if (StatisticsCount = 1) then
  begin
    Stat := Statistics[0];
    with Stat do
    begin
      Results[0].AsFloatVector[Index] := FOddsRatio;
      Results[1].AsFloatVector[Index] := FOddsRatioLL;
      Results[0].AsFloatVector[Index] := FOddsRatioUL;
    end;
    exit;
  end;
// stratified
  Results[0].AsFloatVector[Index] := FMHOR;
  Results[1].AsFloatVector[Index] := FMHORLL;
  Results[0].AsFloatVector[Index] := FMHORUL;
end;

initialization
  RegisterTableStatistic(tsOR, TTwoWayStatisticsOR);


end.

