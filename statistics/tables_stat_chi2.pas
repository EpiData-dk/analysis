unit tables_stat_chi2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, executor;

type

  { TTableCellChi2Expected }

  TTableCellChi2Expected = class(TTableCell)
  private
    FN: EpiFloat;
    procedure SetN(AValue: EpiFloat);
  public
    property N: EpiFloat read FN write SetN;
  end;

  { TTwoWayStatisticChi2 }

  TTwoWayStatisticChi2 = class(TTwoWayStatistic)
  private
    FOrgTable: TTwoWayTable;
    FExpected: TTwoWayTable;
    FChi2: EpiFloat;
    FChiP: EpiFloat;
    FExpectedCellsLessThan5: Integer;
  public
    procedure CalcTable(Table: TTwoWayTable); override;
    procedure AddToOutput(OutputTable: TOutputTable); override;
    procedure CreateResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
    procedure DebugOutput(OutputCreator: TOutputCreator); override;
  end;

  { TTwoWayStatisticsChi2 }

  TTwoWayStatisticsChi2 = class(TTwoWayStatistics)
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatisticChi2; override;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable); override;
//    procedure CalcSummaryStatistics(Tables: TTwoWayTables); override;
//    procedure CreateSummaryResultVariables(Executor: TExecutor; const NamePrefix: UTF8STring);
    property Statistics[Const Index: Integer]: TTwoWayStatisticChi2 read GetStatistics;
  end;

implementation

uses
  tables, statfunctions, epimiscutils, generalutils;

{ TTableCellChi2Expected }

procedure TTableCellChi2Expected.SetN(AValue: EpiFloat);
begin
  if FN = AValue then Exit;
  FN := AValue;
  Table.MarkDirty(Col, Row);
end;

{ TTwoWayStatisticChi2 }

procedure TTwoWayStatisticChi2.CalcTable(Table: TTwoWayTable);
var
  Row, Col, I: Integer;
  C: TTableCellChi2Expected;
  Val: EpiFloat;
  O: TTableCell;
begin
  FOrgTable               := Table;
  FExpected               := TTwoWayTable.Create(FOrgTable.ColCount, FOrgTable.RowCount, TTableCellChi2Expected);
  FChi2                   := 0;
  FExpectedCellsLessThan5 := 0;

  for Row := 0 to FExpected.RowCount - 1 do
    for Col := 0 to FExpected.ColCount - 1 do
      begin
        O   := FOrgTable.Cell[Col, Row];
        C   := TTableCellChi2Expected(FExpected.Cell[Col, Row]);
        if FOrgTable.Total > 0 then
          C.N := (FOrgTable.RowTotal[Row] * FOrgTable.ColTotal[Col]) / FOrgTable.Total
        else
          C.N := 0;

        if (C.N < 5) then Inc(FExpectedCellsLessThan5);

        Val := (O.N - C.N);

        // for Yates' correction:
        // Val := (abs(FOrgTable.Cell[Col, Row].N - C.N) - 0.05);
        if (C.N > 0) then
          FChi2 := FChi2 + ((Val * Val) / C.N); // No contribution if C.N = 0  or impose Yates !!
      end;
  FChiP  := ChiPValue(FChi2 , FOrgTable.DF);
end;

procedure TTwoWayStatisticChi2.AddToOutput(OutputTable: TOutputTable);
var
  S: String;
  PCt: Int64;
begin
  S := 'Chi{\S 2}: ' + Format('%.2f', [FChi2]) + ' Df(' + IntToStr(FOrgTable.DF)+')';

  S := S + FormatP(FChiP, true);

  if (FExpectedCellsLessThan5 > 5) then
    begin
      PCt := Trunc((100 * FExpectedCellsLessThan5) / (FOrgTable.ColCount * FOrgTable.RowCount));
      S := S + LineEnding + 'Cells (expected < 5): ' + IntToStr(FExpectedCellsLessThan5) +
           Format(' (%d pct.)', [Pct]);
    end;

  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + S;
end;

procedure TTwoWayStatisticChi2.CreateResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);
begin
  inherited CreateResultVariables(Executor, NamePrefix);

  Executor.AddResultConst(NamePrefix + 'chi2', ftFloat).AsFloatVector[0] := FChi2;
  Executor.AddResultConst(NamePrefix + 'p',    ftFloat).AsFloatVector[0] := FChiP;
end;

procedure TTwoWayStatisticChi2.DebugOutput(OutputCreator: TOutputCreator);
var
  T: TOutputTable;
  Col, Row: Integer;
begin
  T := OutputCreator.AddTable;
  T.ColCount := FExpected.ColCount;
  T.RowCount := FExpected.RowCount;

  for Col := 0 to FExpected.ColCount - 1 do
    for Row := 0 to FExpected.RowCount - 1 do
      T.Cell[Col, Row].Text := Format('%.2f', [TTableCellChi2Expected(FExpected.Cell[Col, Row]).N]);
end;

{ TTwoWayStatisticsChi2 }

function TTwoWayStatisticsChi2.GetStatistics(const Index: Integer
  ): TTwoWayStatisticChi2;
begin
  result := TTwoWayStatisticChi2(inherited GetStatistics(Index));
end;

function TTwoWayStatisticsChi2.GetTwoWayStatisticClass: TTwoWayStatisticClass;
begin
  result := TTwoWayStatisticChi2;
end;

procedure TTwoWayStatisticsChi2.AddToSummaryTable(OutputTable: TOutputTable);
var
  ColIdx, i: Integer;
  Stat: TTwoWayStatisticChi2;
begin
  ColIdx := OutputTable.ColCount;

  OutputTable.ColCount := OutputTable.ColCount + 3;

  OutputTable.Cell[ColIdx    , 0].Text := 'Chi{\S 2}';
  OutputTable.Cell[ColIdx + 1, 0].Text := 'Df';
  OutputTable.Cell[ColIdx + 2, 0].Text := 'p';

  Stat := Statistics[0];
  OutputTable.Cell[ColIdx    , 1].Text := Format('%.2f', [Stat.FChi2]);
  OutputTable.Cell[ColIdx + 1, 1].Text := IntToStr(Stat.FOrgTable.DF);
  OutputTable.Cell[ColIdx + 2, 1].Text := FormatP(Stat.FChiP, false);

  OutputTable.Cell[ColIdx    , 2].Text := '-';
  OutputTable.Cell[ColIdx + 1, 2].Text := '-';
  OutputTable.Cell[ColIdx + 2, 2].Text := '-';

  for i := 1 to StatisticsCount - 1 do
    begin
      Stat := Statistics[i];

      OutputTable.Cell[ColIdx    , i + 2].Text := Format('%.2f', [Stat.FChi2]);
      OutputTable.Cell[ColIdx + 1, i + 2].Text := IntToStr(Stat.FOrgTable.DF);
      OutputTable.Cell[ColIdx + 2, i + 2].Text := FormatP(Stat.FChiP, false);
    end;
end;
{
procedure CalcSummaryStatistics(Tables: TTwoWayTables);
begin
end;

procedure CreateSummaryResultVariables(Executor: TExecutor; const NamePrefix: UTF8STring);
begin
end;
}
initialization
  RegisterTableStatistic(tsChi2, TTwoWayStatisticsChi2);

end.

