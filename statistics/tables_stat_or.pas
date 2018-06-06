unit tables_stat_or;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, epidatafiles,
  result_variables, executor;

type

  { TTwoWayStatisticOR }

  TTwoWayStatisticOR = class(TTwoWayStatistic)
  private
    FOrgTable: TTwoWayTable;
    FOddsRatio: EpiFloat;
    Fa, Fb, Fc, Fd, Ftotal: Integer;  // save table contents for Mantel-Haenzel estimate
  public
    procedure CalcTable(Table: TTwoWayTable); override;
    procedure AddToOutput(OutputTable: TOutputTable); override;
    procedure CreateResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
  end;

  { TTwoWayStatisticsOR }

  TTwoWayStatisticsOR = class(TTwoWayStatistics)
  private
    FMHOR: EpiFloat;
    FORLL, FORUL: EpiFloat;
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatisticOR; override;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
    procedure CalcMHOR; // rename to CalcSummaryStatistics
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable); override;
//    procedure CreateSummaryResultVariables(Executor: TExecutor); override;
    property Statistics[Const Index: Integer]: TTwoWayStatisticOR read GetStatistics;
  end;


implementation

uses
  tables, epimiscutils;

{ CalcTable}

procedure TTwoWayStatisticOR.CalcTable(Table: TTwoWayTable);

Begin
  FOrgTable := Table;
  FOddsRatio := TEpiFloatField.DefaultMissing; // Missing value will suppress output
  if (FOrgTable.ColCount <> 2) or (FOrgTable.RowCount <> 2 ) then exit;

  Fa := FOrgTable.Cell[0,0].N;
  Fb := FOrgTable.Cell[1,0].N;
  Fc := FOrgTable.Cell[0,1].N;
  Fd := FOrgTable.Cell[1,1].N;

  if ((Fa * Fd) = 0) and ((Fb * Fc) = 0) then
     FOddsRatio := TEpiFloatField.DefaultMissing
  else
    FOddsRatio := (Fa * Fd) / (Fb * Fc);
  Ftotal := FOrgTable.Total;
end;

procedure TTwoWayStatisticOR.AddToOutput(OutputTable: TOutputTable);
var
  S: String;
begin
  if (FOddsRatio = TEpiFloatField.DefaultMissing) then exit;
  S := 'Odds Ratio: '+ Format('%.2f', [FOddsRatio]);
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
  OutputTable.ColCount := OutputTable.ColCount + 1;
  OutputTable.Cell[ColIdx    , 0].Text := 'Odds Ratio';
  OutputTable.Cell[ColIdx    , 1].Text := Format('%.2f', [Stat.FOddsRatio]);
  if (StatisticsCount = 1) then begin
     OutputTable.Cell[ColIdx    , 2].Text := '-';   // will be replaced by M-H OR
     exit;
  end;
  CalcMHOR; // Mantel-Haenzel Odds ratio and CI
  // We need a way to create summary output variables as this is the only point
  // where we have all the tables. However, I'm missing something here.



  for i := 1 to StatisticsCount - 1 do
    begin
      Stat := Statistics[i];
      if (Stat.FOddsRatio = TEpiFloatField.DefaultMissing) then
        S := '-'
      else
        S := Format('%.2f', [Stat.FOddsRatio]);
      OutputTable.Cell[ColIdx    , i + 2].Text := S;
    end;
  OutputTable.ColCount := OutputTable.ColCount + 2;
  OutputTable.Cell[ColIdx + 1, 0].Text := '95% Conf.';
  OutputTable.Cell[ColIdx + 2, 0].Text := 'Interval';
  OutputTable.Cell[ColIdx,     2].Text := Format('%.2f', [FMHOR]);
  OutputTable.Cell[ColIdx + 1, 2].Text := Format('%.2f', [FORLL]);
  OutputTable.Cell[ColIdx + 2, 2].Text := Format('%.2f', [FORUL]);
end;
{
procedure TTwoWayStatisticsOR.CreateSummaryResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);

begin
  if (Statistics.Count = 1) then exit;
  inherited CreateResultVariables(Executor, NamePrefix);

  FExecutor.AddResultConst(NamePrefix + 'MHOR',   ftFloat).AsFloatVector[0] := FMHOR;
  FExecutor.AddResultConst(NamePrefix + 'MHORLL', ftFloat).AsFloatVector[0] := FORLL;
  FExecutor.AddResultConst(NamePrefix + 'MHORUL', ftFloat).AsFloatVector[0] := FORUL;

end;
}
// rename this as CalcSummaryStatistics(Statistics: ...) // do we need parameters? to be called from tables unit
procedure TTwoWayStatisticsOR.CalcMHOR;
var
  i: Integer;
  Stat: TTwoWayStatisticOR;
  p, q, r, s,
  SumR, SumS, SumRS, SumPR, SumPSQR, SumSQ: EpiFloat;
  MantelNum, MantelDen: EpiFloat;
  conf, variance: EpiFloat;
begin
  conf      := 1.96;
  MantelNum := 0;
  MantelDen := 0;
  SumR      := 0;
  SumS      := 0;
  SumRS     := 0;
  SumPR     := 0;
  SumPSQR   := 0;
  SumSQ     := 0;

  for i := 1 to StatisticsCount - 1 do
    begin
      Stat := Statistics[i];
      with Stat do begin
        MantelNum += ((Fa * Fd) / Ftotal);
        MantelDen += ((Fb * Fc) / Ftotal);
        if (Ftotal > 0) and ((Fb * Fc) > 0) then
        begin
          p := (Fa + Fd) / Ftotal;
          q := (Fb + Fc) / Ftotal;
          r := (Fa * Fd);
          s := (Fb * Fc);
          SumR    += r;
          SumS    += s;
          SumRS   += r*s;
          SumPR   += p*r;
          SumSQ   += s*q;
          SumPSQR += (p*s) + (q*r);
        end;
      end;
    end;
  FMHOR := MantelNum / MantelDen;

  // Estimate upper and lower confidence limits

  if ((SumR * SumS) = 0) then exit;

  variance := abs(SumPR / (2 * (SumR * SumR)) + (SumPSQR / (2 * SumR * SumS)) + (SumSQ / (2 * SumS * SumS)));
  FORLL := exp(ln(FMHOR) - (conf * sqrt(variance)));
  FORUL := exp(ln(FMHOR) + (conf * sqrt(variance)));
end;

initialization
  RegisterTableStatistic(tsOR, TTwoWayStatisticsOR);


end.

