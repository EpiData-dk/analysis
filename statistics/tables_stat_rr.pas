unit tables_stat_rr;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, epidatafiles,
  result_variables, executor;

type

  { TTwoWayStatisticRR }

  TTwoWayStatisticRR = class(TTwoWayStatistic)
  private
    FOrgTable: TTwoWayTable;
    FRelativeRisk: EpiFloat;
    Fa, Fab, Fc, Fcd, Ftotal: Integer;  // save table contents for Mantel-Haenzel estimate
  public
    procedure CalcTable(Table: TTwoWayTable); override;
    procedure AddToOutput(OutputTable: TOutputTable); override;
    procedure CreateResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
  end;

  { TTwoWayStatisticsRR }

  TTwoWayStatisticsRR = class(TTwoWayStatistics)
  private
    FMHRR: EpiFloat;
    FRRLL, FRRUL: EpiFloat;
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatisticRR; override;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable); override;
    procedure CalcSummaryStatistics(Tables: TTwoWayTables); override;
    procedure CreateSummaryResultVariables(Executor: TExecutor; const NamePrefix: UTF8STring);
    property Statistics[Const Index: Integer]: TTwoWayStatisticRR read GetStatistics;
  end;


implementation

uses
  tables, epimiscutils;

{ CalcTable}

procedure TTwoWayStatisticRR.CalcTable(Table: TTwoWayTable);

Begin
  FOrgTable := Table;
  FRelativeRisk := TEpiFloatField.DefaultMissing; // Missing value will suppress output
  if (FOrgTable.ColCount <> 2) or (FOrgTable.RowCount <> 2 ) then exit;

  Fa := FOrgTable.Cell[0,0].N;
  Fab := FOrgTable.RowTotal[0];
  Fc := FOrgTable.Cell[0,1].N;
  Fcd := FOrgTable.RowTotal[1];

  if (Fc = 0) or (Fab = 0) then // or (Fcd = 0) then
     FRelativeRisk := TEpiFloatField.DefaultMissing
  else
    FRelativeRisk := (Fa * Fcd) / (Fab * Fc);
  Ftotal := FOrgTable.Total;
end;

procedure TTwoWayStatisticRR.AddToOutput(OutputTable: TOutputTable);
var
  S: String;
begin
  if (FRelativeRisk = TEpiFloatField.DefaultMissing) then exit;
  S := 'Risk Ratio: '+ Format('%.2f', [FRelativeRisk]);
  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + S;
end;

procedure TTwoWayStatisticRR.CreateResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);
begin
  if (FRelativeRisk = TEpiFloatField.DefaultMissing) then exit;
  inherited CreateResultVariables(Executor, NamePrefix);

  Executor.AddResultConst(NamePrefix + 'RR', ftFloat).AsFloatVector[0] := FRelativeRisk;
end;

function TTwoWayStatisticsRR.GetStatistics(const Index: Integer
  ): TTwoWayStatisticRR;
begin
  result := TTwoWayStatisticRR(inherited GetStatistics(Index));
end;

function TTwoWayStatisticsRR.GetTwoWayStatisticClass: TTwoWayStatisticClass;
begin
  result := TTwoWayStatisticRR;
end;

procedure TTwoWayStatisticsRR.AddToSummaryTable(OutputTable: TOutputTable);
var
  ColIdx, i: Integer;
  Stat: TTwoWayStatisticRR;
  S: string;
begin
  Stat := Statistics[0];
  if (Stat.FRelativeRisk = TEpiFloatField.DefaultMissing) then exit;

  ColIdx := OutputTable.ColCount;
  OutputTable.ColCount := OutputTable.ColCount + 1;
  OutputTable.Cell[ColIdx    , 0].Text := 'Risk Ratio';
  OutputTable.Cell[ColIdx    , 1].Text := Format('%.2f', [Stat.FRelativeRisk]);
  if (StatisticsCount = 1) then begin
     OutputTable.Cell[ColIdx    , 2].Text := '-';   // will be replaced by M-H OR
     exit;
  end;
//  CalcSummaryStatistics; // Mantel-Haenzel Odds ratio and CI
  // This is the wrong place for summary stat calculation
  // We need a way to create summary output variables as this is the only point
  // where we have all the tables. i.e. requires change to tables unit



  for i := 1 to StatisticsCount - 1 do   // skips unstratified table
    begin
      Stat := Statistics[i];
      if (Stat.FRelativeRisk = TEpiFloatField.DefaultMissing) then
        S := '-'
      else
        S := Format('%.2f', [Stat.FRelativeRisk]);
      OutputTable.Cell[ColIdx    , i + 2].Text := S;
    end;
  OutputTable.ColCount := OutputTable.ColCount + 2;
  OutputTable.Cell[ColIdx + 1, 0].Text := '95% Conf.';
  OutputTable.Cell[ColIdx + 2, 0].Text := 'Interval';
  OutputTable.Cell[ColIdx,     2].Text := Format('%.2f', [FMHRR]);
  OutputTable.Cell[ColIdx + 1, 2].Text := Format('%.2f', [FRRLL]);
  OutputTable.Cell[ColIdx + 2, 2].Text := Format('%.2f', [FRRUL]);
end;

procedure TTwoWayStatisticsRR.CreateSummaryResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);

begin
  if (StatisticsCount = 1) then exit;
//  inherited CreateResultVariables(Executor, NamePrefix);

  Executor.AddResultConst(NamePrefix + 'MHOR',   ftFloat).AsFloatVector[0] := FMHRR;
  Executor.AddResultConst(NamePrefix + 'MHORLL', ftFloat).AsFloatVector[0] := FRRLL;
  Executor.AddResultConst(NamePrefix + 'MHORUL', ftFloat).AsFloatVector[0] := FRRUL;

end;

procedure TTwoWayStatisticsRR.CalcSummaryStatistics(Tables: TTwoWayTables);
var
  i: Integer;
  Stat: TTwoWayStatisticRR;
  p, q, r, s,
  SumR, SumS: EpiFloat;
  SumV: EpiFloat;
  conf, variance: EpiFloat;
begin
  if (StatisticsCount = 1) then exit;   // No stratified tables
  conf := 1.96;
  SumV := 0;
  SumR := 0;
  SumS := 0;

  for i := 1 to StatisticsCount - 1 do   // skips unstratified table
    begin
      Stat := Statistics[i];
      with Stat do begin
        if ((Fa + Fc) > 0) then
        begin
          r    := (Fa * Fcd) / Ftotal;
          s    := (Fc * Fab) / Ftotal;
          SumR += r;
          SumS += s;
          SumV += ((Fcd * Fab * (Fa + Fc)) - (Fa * Fc * FTotal)) / (FTotal * FTotal);
        end;
      end;
    end;

  if (SumR = 0 ) then FMHRR := 0
  else if (Sums = 0) then FMHRR := TEpiFloatField.DefaultMissing
  else FMHRR := SumR / SumS;

  // Estimate upper and lower confidence limits

  if ((SumR * SumS) = 0) then
  begin
    FRRUL := TEpiFloatField.DefaultMissing;
    FRRLL := TEpiFloatField.DefaultMissing;
  end
  else begin
    variance := abs(SumV / (SumR * SumS));
    FRRLL := exp(ln(FMHRR) - (conf * sqrt(variance)));
    FRRUL := exp(ln(FMHRR) + (conf * sqrt(variance)));
  end;
end;

initialization
  RegisterTableStatistic(tsRR, TTwoWayStatisticsRR);


end.

