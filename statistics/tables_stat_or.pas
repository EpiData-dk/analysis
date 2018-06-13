unit tables_stat_or;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, epidatafiles,
  result_variables, executor, statfunctions;

type

  { TTwoWayStatisticOR }

  TTwoWayStatisticOR = class(TTwoWayStatistic)
  private
    FConf: Integer;
    FOrgTable: TTwoWayTable;
    FOddsRatio: EpiFloat;
    FORLL, FORUL: EpiFloat;
    FMessage: String;
  public
    procedure CalcTable(Table: TTwoWayTable); override;
    procedure AddToOutput(OutputTable: TOutputTable); override;
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
    procedure AddToSummaryTable(OutputTable: TOutputTable); override;
    procedure CalcSummaryStatistics(Tables: TTwoWayTables); override;
    procedure CreateSummaryResultVariables(Executor: TExecutor; const NamePrefix: UTF8STring); override;
    property Statistics[Const Index: Integer]: TTwoWayStatisticOR read GetStatistics;
  end;


implementation

uses
  tables, epimiscutils, generalutils, Math, ana_globals;

{ CalcTable}

procedure TTwoWayStatisticOR.CalcTable(Table: TTwoWayTable);
//TODO: confidence interval
var
  a, b, c, d, n: Integer;
  p, q, r, s, f1, f2, f3: EpiFloat;
  conf: EpiFLoat;
begin
  FOrgTable := Table;
  if (FOrgTable.ColCount <> 2) or (FOrgTable.RowCount <> 2 ) then
  begin
    FOddsRatio := TEpiFloatField.DefaultMissing;
    FMessage   := 'Table is not 2x2.';
    exit;
  end;
  FConf      := 95;  // this should get set by option
  conf       := PNormalInv((1 - (FConf / 100)) / 2);

  a := FOrgTable.Cell[0,0].N;
  b := FOrgTable.Cell[1,0].N;
  c := FOrgTable.Cell[0,1].N;
  d := FOrgTable.Cell[1,1].N;

  if ((a * d) = 0) and ((b * c) = 0) then
  begin
    FMessage   := 'Table has a zero marginal.';
    FOddsRatio := TEpiFloatField.DefaultMissing;
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
  if (f2 = 0) then
  begin
    FORUL := TEpiFloatField.DefaultMissing;
    FORLL := FORUL;
  end
  else
  begin
    FORLL := exp(ln(FOddsRatio) - (conf  * sqrt(f1 + f2 + f3)));
    FORUL := exp(ln(FOddsRatio) + (conf  * sqrt(f1 + f2 + f3)));
  end;
end;

procedure TTwoWayStatisticOR.AddToOutput(OutputTable: TOutputTable);
var
  S: String;
begin
  if (FOddsRatio = TEpiFloatField.DefaultMissing) then
    S := 'Cannot estimate the Odds Ratio. '+ FMessage
  else
    S := 'Odds Ratio: '+ Format('%.2f', [FOddsRatio]);
  if (FORLL <> TEpiFloatField.DefaultMissing) then
    S += ' ' + FormatCI(FORLL, FORUL, FConf);
  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + S;
end;

procedure TTwoWayStatisticOR.CreateResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);
begin
  if (FOddsRatio = TEpiFloatField.DefaultMissing) then exit;
  inherited CreateResultVariables(Executor, NamePrefix);

  Executor.AddResultConst(NamePrefix + 'OR', ftFloat).AsFloatVector[0] := FOddsRatio;
end;

function TTwoWayStatisticsOR.GetStatistics(const Index: Integer
  ): TTwoWayStatisticOR;
begin
  result := TTwoWayStatisticOR(inherited GetStatistics(Index));
end;

function TTwoWayStatisticsOR.GetTwoWayStatisticClass: TTwoWayStatisticClass;
begin
  result := TTwoWayStatisticOR;
end;

procedure TTwoWayStatisticsOR.AddToSummaryTable(OutputTable: TOutputTable);
var
  ColIdx, i: Integer;
  Stat: TTwoWayStatisticOR;
  S: string;
begin
  Stat := Statistics[0];
  if (Stat.FOddsRatio = TEpiFloatField.DefaultMissing) then exit;

  ColIdx := OutputTable.ColCount;
  OutputTable.ColCount := OutputTable.ColCount + 2;
  OutputTable.Cell[ColIdx    , 0].Text := 'Odds Ratio';
  OutputTable.Cell[ColIdx + 1, 0].Text := IntToStr(FConf) + '% CI';
  OutputTable.Cell[ColIdx    , 1].Text := Format('%.2f', [Stat.FOddsRatio]);
  OutputTable.Cell[ColIdx + 1, 1].Text := FormatCI(Stat.FORUL, Stat.FORLL, 0);
  if (StatisticsCount = 1) then begin
     OutputTable.Cell[ColIdx    , 2].Text := '-';   // will be replaced by M-H OR
     exit;
  end;

  for i := 1 to StatisticsCount - 1 do   // skips unstratified table
    begin
      Stat := Statistics[i];
      if (Stat.FOddsRatio = TEpiFloatField.DefaultMissing) then
      begin
        OutputTable.Cell[ColIdx,     2].Text := '-';
        OutputTable.Cell[ColIdx + 1, 2].Text := '';
      end
      else
      begin
        OutputTable.Cell[ColIdx,     i + 2].Text := Format('%.2f', [Stat.FOddsRatio]);
        if (Stat.FORLL <> TEpiFloatField.DefaultMissing) then
          OutputTable.Cell[ColIdx + 1, i + 2].Text := FormatCI(Stat.FORUL, Stat.FORLL, 0);
      end;
    end;
  if (isInfinite(FMHOR)) then exit;
  OutputTable.Cell[ColIdx,     2].Text := Format('%.2f', [FMHOR]);
  OutputTable.Cell[ColIdx + 1, 2].Text := FormatCI(FMHORLL, FMHORUL, 0);
end;

procedure TTwoWayStatisticsOR.CreateSummaryResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);

begin
  if (StatisticsCount = 1) or (isInfinite(FMHOR)) then exit;   // No stratified tables

  Executor.AddResultConst(NamePrefix + 'MHOR',   ftFloat).AsFloatVector[0] := FMHOR;
  Executor.AddResultConst(NamePrefix + 'MHORLL', ftFloat).AsFloatVector[0] := FMHORLL;
  Executor.AddResultConst(NamePrefix + 'MHORUL', ftFloat).AsFloatVector[0] := FMHORUL;

end;

procedure TTwoWayStatisticsOR.CalcSummaryStatistics(Tables: TTwoWayTables);
var
  a, b, c, d, n: Integer;
  Tab: TTwoWayTable;
  p, q, r, s,
  SumR, SumS, SumRS, SumPR, SumPSQR, SumSQ: EpiFloat;
  conf, variance: EpiFloat;
  t: String;

  begin
  if (StatisticsCount = 1) then exit;   // No stratified tables
  FConf     := 95;  // this should get set by option
  conf      := PNormalInv((1 - (FConf / 100)) / 2);
  SumR      := 0;
  SumS      := 0;
  SumRS     := 0;
  SumPR     := 0;
  SumPSQR   := 0;
  SumSQ     := 0;

  for Tab in Tables do
    begin
      with Tab do begin
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
    end;
  FMHOR := SumR / SumS;

  // Estimate upper and lower confidence limits
  { NIST reference has same formula
    https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/mantel.htm
  }
  if ((SumR * SumS) = 0) or (isInfinite(FMHOR)) then exit;

  variance := abs((SumPR/(SumR * SumR)) + (SumPSQR/(SumR * SumS)) + (SumSQ / (SumS * Sums))) / 2;
  FMHORLL := exp(ln(FMHOR) - (conf * sqrt(variance)));
  FMHORUL := exp(ln(FMHOR) + (conf * sqrt(variance)));
end;

initialization
  RegisterTableStatistic(tsOR, TTwoWayStatisticsOR);


end.

