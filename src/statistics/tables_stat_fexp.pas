unit tables_stat_fexp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, epidatafiles,
    result_variables, executor, ast;

type

  { TTwoWayStatisticFExP }

  TTwoWayStatisticFExP = class(TTwoWayStatistic)
  private
    FOrgTable: TTwoWayTable;
    FFExP: EpiFloat;
    FMessage: UTF8String;
  public
    procedure CalcTable(Table: TTwoWayTable;Conf: Integer); override;
    procedure AddToOutput(OutputTable: TOutputTable; Options: TOptionList); override;
    procedure CreateResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
  end;

  { TTwoWayStatisticsFExP }

  TTwoWayStatisticsFExP = class(TTwoWayStatistics)
  private
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatisticFExP; override;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList); override;
    procedure AddToCompactTable(Executor: TExecutor; T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList); override;
    procedure AddToCompactHeader(T: TOutputTable; Options: TOptionList); override;
    function  CompactSortValue: EpiFloat; override;
    function  CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String; ResultRows: Integer): TStatResult; override;
    procedure AddCompactResultVariables(Executor: TExecutor; Index: Integer; Results: TStatResult); override;
//    procedure CalcSummaryStatistics(Tables: TTwoWayTables); override;
//    procedure CreateSummaryResultVariables(Executor: TExecutor; const NamePrefix: UTF8STring); override;
    property Statistics[Const Index: Integer]: TTwoWayStatisticFExP read GetStatistics;
  end;

implementation

uses
  tables, epimiscutils, generalutils;

{ TTwoWayStatisticFExP }

procedure TTwoWayStatisticFExP.CalcTable(Table: TTwoWayTable;Conf: Integer);
Var
  AF, BF, CF, DF : EpiFloat;
  A2, B2, C2, D2 : EpiFloat;
  X, SN, ON, DEN : EpiFloat;
Begin
  FOrgTable := Table;
  FFExP     := TEpiFloatField.DefaultMissing; // Missing value will suppress output
  if (FOrgTable.ColCount <> 2) or (FOrgTable.RowCount <> 2 ) then
  begin
    FMessage := 'Table is not 2x2';
    exit;
  end;
  if ((FOrgTable.ColTotal[0] = 0) or (FOrgTable.ColTotal[1] = 0) or
      (FOrgTable.RowTotal[0] = 0) or (FOrgTable.RowTotal[1] = 0)) then
  begin
    FMessage := 'Table has a zero column marginal';
    exit;
  end;
  If (FOrgTable.Cell[0,0].N * FOrgTable.RowTotal[1]) < (FOrgTable.Cell[0,1].N * FOrgTable.RowTotal[0]) then
    Begin
    AF := FOrgTable.Cell[0,0].N; //A
    BF := FOrgTable.Cell[1,0].N; //B
    CF := FOrgTable.Cell[0,1].N; //C
    DF := FOrgTable.Cell[1,1].N; //D
    End
  Else
    Begin
    AF := FOrgTable.Cell[0,1].N; //C;
    BF := FOrgTable.Cell[1,1].N; //D;
    CF := FOrgTable.Cell[0,0].N; //A;
    DF := FOrgTable.Cell[1,0].N; //B
  End;

  SN  := 1;
  ON  := 0;
  DEN := 1;
  A2  := AF;
  D2  := DF;
  B2  := BF + 1;
  C2  := CF + 1;
  X   := 1;
  While (X > 9.999999e-21) Do
      Begin
      X   := X * A2 * D2 / (B2 * C2);
      SN  := X + SN;
      DEN := X + DEN;
      A2  := A2 - 1;
      D2  := D2 - 1;
      B2  := B2 + 1;
      C2  := C2 + 1
      End;
  B2 := BF;
  C2 := CF;
  A2 := AF + 1;
  D2 := DF + 1;
  X := 1;
  While (X > 9.999999e-21) and (X < 1.0E+30) Do
      Begin
      X := X * B2 * C2 / (A2 * D2);
      DEN := X + DEN;
      If (X <= 1) then
         ON := X + ON;
      A2 := A2 + 1;
      D2 := D2 + 1;
      B2 := B2 - 1;
      C2 := C2 - 1
      End;
  // result := SN / DEN;  one sided fishers exact
  FFExP := (SN + ON) / DEN;

end;

procedure TTwoWayStatisticFExP.AddToOutput(OutputTable: TOutputTable; Options: TOptionList);
var
  S: UTF8String;
begin
  if (FFExP = TEpiFloatField.DefaultMissing) then
  begin
    OutputTable.Footer.Text := OutputTable.Footer.Text + 'Fisher Exact p not calculated' + LineEnding;
    exit;
  end;
  S := 'Fisher Exact '+ FormatP(FFExP, true);
  OutputTable.Footer.Text := OutputTable.Footer.Text + S + LineEnding;
end;

procedure TTwoWayStatisticFExP.CreateResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);
begin
  if (FFExP = TEpiFloatField.DefaultMissing) then exit;
  inherited CreateResultVariables(Executor, NamePrefix);

  Executor.AddResultConst(NamePrefix + 'fexp', ftFloat).AsFloatVector[0] := FFExP;
end;

function TTwoWayStatisticsFExP.GetStatistics(const Index: Integer
  ): TTwoWayStatisticFExP;
begin
  result := TTwoWayStatisticFExP(inherited GetStatistics(Index));
end;

function TTwoWayStatisticsFExP.GetTwoWayStatisticClass: TTwoWayStatisticClass;
begin
  result := TTwoWayStatisticFExP;
end;

procedure TTwoWayStatisticsFExP.AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList);
var
  ColIdx, i: Integer;
  Stat: TTwoWayStatisticFExP;
begin
  Stat := Statistics[0];
  if (Stat.FFExP = TEpiFloatField.DefaultMissing) then exit;

  ColIdx := OutputTable.ColCount;
  OutputTable.ColCount := OutputTable.ColCount + 1;
  OutputTable.Cell[ColIdx, 0].Text := 'Fisher Exact p';
  OutputTable.Cell[ColIdx, 1].Text := FormatP(Stat.FFExP, false);
  OutputTable.Cell[ColIdx, 2].Text := '-';

  for i := 1 to StatisticsCount - 1 do
    begin
      Stat := Statistics[i];
      OutputTable.Cell[ColIdx    , i + 2].Text := FormatP(Stat.FFExP, false);
    end;
end;

procedure TTwoWayStatisticsFExP.AddToCompactHeader(T: TOutputTable; Options: TOptionList);
var
  ColIdx: Integer;
//  Stat: TTwoWayStatisticFExP;

begin
//  Stat := Statistics[0];
  if (T.RowCount <> 2) then exit;
  ColIdx                      := T.ColCount;
  T.ColCount                  := ColIdx + 1;
  T.Cell[ColIdx, 0].Text := 'Fisher';
  T.Cell[ColIdx, 1].Text := 'Exact p';

end;

procedure TTwoWayStatisticsFExP.AddToCompactTable(Executor: TExecutor;
         T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList);
var
  Stat: TTwoWayStatisticFExP;

begin
  Stat := Statistics[0];
  with Stat do
  begin
    if ((FMessage <> '') and (Options.HasOption('inc'))) then
      T.Footer.Text := T.Footer.Text + LineEnding +
                       T.Cell[0,RowIdx].Text + ': ' + FMessage;

      if (FFExP = TEpiFloatField.DefaultMissing) then
      begin
        T.Cell[ColIdx, RowIdx].Text := '-';
      end
      else
      begin
        T.Cell[ColIdx, RowIdx].Text := FormatP(Stat.FFExP, false);
      end;
      exit;
  end;
end;

function TTwoWayStatisticsFExP.CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String;
         ResultRows: Integer): TStatResult;

begin
  setlength(Result,1); // one statistic
  Result[0] := Executor.AddResultVector(Prefix + 'FExP', ftFloat, ResultRows);
end;

procedure TTwoWayStatisticsFExP.AddCompactResultVariables(Executor: TExecutor;
          Index: Integer; Results: TStatResult);
var
  Stat: TTwoWayStatisticFExP;
begin
  Stat := Statistics[0];
  Results[0].AsFloatVector[Index] := Stat.FFExP;
end;

function TTwoWayStatisticsFExP.CompactSortValue: EpiFloat;

begin
  result := Statistics[0].FFExP;
  if result = TEpiFloatField.DefaultMissing then
    result := 2;
end;

initialization
  RegisterTableStatistic(tsFExP, TTwoWayStatisticsFExP);

end.
