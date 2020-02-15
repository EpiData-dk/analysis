unit tables_stat_chi2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, epidatafiles,
  executor, ast;

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
    FChi2, FChiP: EpiFloat;
    FExpectedCellsLessThan5: Integer;
  public
    procedure CalcTable(Table: TTwoWayTable;Conf: Integer); override;
    procedure AddToOutput(OutputTable: TOutputTable; Options: TOptionList); override;
    procedure CreateResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
    procedure DebugOutput(OutputCreator: TOutputCreator); override;
  end;

  { TTwoWayStatisticsChi2 }

  TTwoWayStatisticsChi2 = class(TTwoWayStatistics)
  private
    FMHChi2, FMHChiP: EpiFloat;
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatisticChi2; override;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList); override;
    procedure AddToCompactTable(Executor: TExecutor; T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList); override;
    procedure AddToCompactHeader(T: TOutputTable; Options: TOptionList); override;
    procedure CalcSummaryStatistics(Tables: TTwoWayTables;Conf: Integer); override;
    procedure CreateSummaryResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
    function  CompactSortValue: EpiFloat; override;
    function  CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String; ResultRows: Integer): TStatResult; override;
    procedure AddCompactResultVariables(Executor: TExecutor; Index: Integer; Results: TStatResult); override;
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

procedure TTwoWayStatisticChi2.CalcTable(Table: TTwoWayTable;Conf: Integer);
var
  Row, Col: Integer;
  C: TTableCellChi2Expected;
  Val: EpiFloat;
  O: TTableCell;
begin
  FOrgTable               := Table;
  FExpected               := TTwoWayTable.Create(FOrgTable.ColCount, FOrgTable.RowCount, TTableCellChi2Expected);
  FChi2 := TEpiFloatField.DefaultMissing;
  FChiP := TEpiFloatField.DefaultMissing;
  if (Fexpected.RowCount <2) or (FExpected.ColCount < 2) then
    exit;       // No statistic if only one row or column

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
        // Val := (abs(O.N - C.N) - 0.05);
        if (C.N > 0) then
          FChi2 := FChi2 + ((Val * Val) / C.N); // No contribution if C.N = 0  or impose Yates !!
      end;
  FChiP  := ChiPValue(FChi2 , FOrgTable.DF);
end;

procedure TTwoWayStatisticChi2.AddToOutput(OutputTable: TOutputTable; Options: TOptionList);
var
  S: UTF8String;
  PCt: Int64;
begin
  if FChi2 = TEpiFloatField.DefaultMissing then
    S := 'Chi{\S 2} not calculated'
  else
    begin
    S := 'Chi{\S 2}: ' + Format('%.2f', [FChi2]) + ' Df(' + IntToStr(FOrgTable.DF)+') ';
    S := S + FormatP(FChiP, true);
    if (FExpectedCellsLessThan5 > 5) then
      begin
        PCt := Trunc((100 * FExpectedCellsLessThan5) / (FOrgTable.ColCount * FOrgTable.RowCount));
        S := S + LineEnding + 'Cells (expected < 5): ' + IntToStr(FExpectedCellsLessThan5) +
             Format(' (%d pct.)', [Pct]);
      end;
    end;
  OutputTable.Footer.Text := OutputTable.Footer.Text + S + LineEnding;
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

procedure TTwoWayStatisticsChi2.AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList);
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
  if (Stat.FChi2 = TEpiFloatField.DefaultMissing) then
    begin
      OutputTable.Cell[ColIdx    , 1].Text := '-';
      OutputTable.Cell[ColIdx + 1, 1].Text := '-';
      OutputTable.Cell[ColIdx + 2, 1].Text := '-';
    end
  else
    begin
      OutputTable.Cell[ColIdx    , i + 1].Text := Format('%.2f', [Stat.FChi2]);
      OutputTable.Cell[ColIdx + 1, i + 1].Text := IntToStr(Stat.FOrgTable.DF);
      OutputTable.Cell[ColIdx + 2, i + 1].Text := FormatP(Stat.FChiP, false);
    end;

  if (Stat.FOrgTable.DF = 1) then // 2x2 table
  begin
    OutputTable.Cell[ColIdx    , 2].Text := Format('%.2f', [FMHChi2]);
    OutputTable.Cell[ColIdx + 1, 2].Text := '1';
    OutputTable.Cell[ColIdx + 2, 2].Text := FormatP(FMHChiP, false);
  end
  else begin
    OutputTable.Cell[ColIdx    , 2].Text := '-';
    OutputTable.Cell[ColIdx + 1, 2].Text := '-';
    OutputTable.Cell[ColIdx + 2, 2].Text := '-';
  end;

  for i := 1 to StatisticsCount - 1 do
    begin
      Stat := Statistics[i];
      if (Stat.FChi2 <> TEpiFloatField.DefaultMissing) then
      begin
        OutputTable.Cell[ColIdx    , i + 2].Text := Format('%.2f', [Stat.FChi2]);
        OutputTable.Cell[ColIdx + 1, i + 2].Text := IntToStr(Stat.FOrgTable.DF);
        OutputTable.Cell[ColIdx + 2, i + 2].Text := FormatP(Stat.FChiP, false);
      end;
    end;
end;
// M-H Chi-square for 2x2 stratified tables
// Formula from  http://www.biostathandbook.com/cmh.html
procedure TTwoWayStatisticsChi2.CalcSummaryStatistics(Tables: TTwoWayTables;Conf: Integer);
var
  a, b, c, d: Integer;
  k, n: Integer;
  Tab: TTwoWayTable;
  Num, Denom: EpiFloat;

  begin
  if (StatisticsCount = 1) then exit;   // No stratified tables
  FMHChi2 := TEpiFloatField.DefaultMissing;
  FMHChiP := FMHChi2;
  Tab := Tables.UnstratifiedTable;
  if (Tab.ColCount <> 2) or (Tab.RowCount <> 2) then exit;
  k         := 0;
  Num       := 0;
  Denom     := 0;
  for Tab in Tables do
    begin
      with Tab do begin
        k += 1;
        a := Tab.Cell[0,0].N;
        b := Tab.Cell[1,0].N;
        c := Tab.Cell[0,1].N;
        d := Tab.Cell[1,1].N;
        n := Tab.Total;
        Num     += a - ((a + b) * (a + c) / n);
        Denom   += ((a + b) * (a + c) * (b + d) * (c + d))
                   / ((n * n * n) - (n * n));
      end;
    end;
  Num     := abs(Num) - 0.5;
  FMHChi2  := Num * Num / Denom;
  FMHChiP := ChiPValue(FMHChi2,1);

end;

procedure TTwoWayStatisticsChi2.CreateSummaryResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);

begin
  if (StatisticsCount = 1) then exit;   // No stratified tables

  Executor.AddResultConst(NamePrefix + 'MHChi2', ftFloat).AsFloatVector[0] := FMHChi2;
  Executor.AddResultConst(NamePrefix + 'MHChip', ftFloat).AsFloatVector[0] := FMHChiP;

end;

procedure TTwoWayStatisticsChi2.AddToCompactHeader(T: TOutputTable; Options: TOptionList);
var
  ColIdx: Integer;
  Stat: TTwoWayStatisticChi2;

begin
  Stat := Statistics[0];
  if (T.RowCount <> 2) then exit;
  ColIdx                      := T.ColCount;
  T.ColCount                  := ColIdx + 3;
  T.Cell[ColIdx     , 1].Text := 'Chi^2';
  T.Cell[ColIdx + 1 , 1].Text := 'df';
  T.Cell[ColIdx + 2 , 1].Text := 'p';

end;

procedure TTwoWayStatisticsChi2.AddToCompactTable(Executor: TExecutor;
         T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList);
var
  Stat: TTwoWayStatisticChi2;

begin
  Stat := Statistics[0];
  with Stat do
  begin
    if (StatisticsCount = 1) then
    // unstratified - crude result
    begin
      if (FChi2 = TEpiFloatField.DefaultMissing) then exit;

      T.Cell[ColIdx    , RowIdx].Text := Format('%.2f', [FChi2]);
      T.Cell[ColIdx + 1, RowIdx].Text := IntToStr(FOrgTable.DF);
      T.Cell[ColIdx + 2, RowIdx].Text := FormatP(FChiP, false);
      exit;
    end;
  end;
  // stratified - summary result
  if (FMHChi2 = TEpiFloatField.DefaultMissing) then exit;
  T.Cell[ColIdx    , RowIdx].Text := Format('%.2f', [FMHChi2]);
  T.Cell[ColIdx + 1, RowIdx].Text := '1';
  T.Cell[ColIdx + 2, RowIdx].Text := FormatP(FMHChiP, false);
end;

function TTwoWayStatisticsChi2.CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String;
         ResultRows: Integer): TStatResult;

begin
  setlength(Result,3); // three statistics
  Result[0] := Executor.AddResultVector(Prefix + 'chi2', ftFloat, ResultRows);
  Result[1] := Executor.AddResultVector(Prefix + 'chip', ftFloat, ResultRows);
  Result[2] := Executor.AddResultVector(Prefix + 'df', ftInteger, ResultRows);
end;

procedure TTwoWayStatisticsChi2.AddCompactResultVariables(Executor: TExecutor;
          Index: Integer; Results: TStatResult);
var
  Stat: TTwoWayStatisticChi2;
begin
  Stat := Statistics[0];
  with Stat do
  begin
    if (StatisticsCount = 1) or (FOrgTable.DF > 1) then
    begin
      Results[0].AsFloatVector[Index] := FChi2;
      Results[1].AsFloatVector[Index] := FChiP;
    end
    else
    begin
      Results[0].AsFloatVector[Index] := FMHChi2;
      Results[1].AsFloatVector[Index] := FMHChiP;
    end;
    Results[2].AsIntegerVector[Index] := FOrgTable.DF;
  end;
end;

function TTwoWayStatisticsChi2.CompactSortValue: EpiFloat;

begin
  if (StatisticsCount = 1) then
    result := Statistics[0].FChiP
  else
    result := FMHChiP;
  if result = TEpiFloatField.DefaultMissing then
    result := 2;
end;

initialization
  RegisterTableStatistic(tsChi2, TTwoWayStatisticsChi2);

end.
