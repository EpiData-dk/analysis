unit tables_stat_fexp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, executor;

type

  { TTwoWayStatisticFExP }

  TTwoWayStatisticFExP = class(TTwoWayStatistic)
  private
    FOrgTable: TTwoWayTable;
    FFExP: EpiFloat;
  public
    procedure CalcTable(Table: TTwoWayTable); override;
    procedure AddToOutput(OutputTable: TOutputTable); override;
    procedure CreateResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
  end;

  { TTwoWayStatisticsFExP }

  TTwoWayStatisticsFExP = class(TTwoWayStatistics)
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatisticFExP; override;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable); override;
    property Statistics[Const Index: Integer]: TTwoWayStatisticFExP read GetStatistics;
  end;

implementation

uses
  tables, epimiscutils;

function FormatFExP(Val: EpiFloat; ShowP: Boolean): UTF8String;
// this should probably be a standard function in table_types
var
  prefix: string = '';
begin
  if (ShowP) then prefix := ' p';
  if (Val < 0.0001) then
    Result := prefix + ' < 0.0001'
  else
    if (Val < 0.001) then
      Result := prefix + ' < 0.001'
  else
    if (ShowP) then prefix += ' =';
    Result := prefix + Format('%.3f', [Val]) ;
end;

{ TTwoWayStatisticFExP }

procedure TTwoWayStatisticFExP.CalcTable(Table: TTwoWayTable);
Var
  AF, BF, CF, DF : EpiFloat;
  A2, B2, C2, D2, N1F, N0F, M1F, M0F, TF : EpiFloat;
  X, SN, ON, DEN : EpiFloat;
Begin
  FOrgTable := Table;
  FFExP     := -1; // default value that will suppress output (could use Missing value)
  if (FOrgTable.ColCount <> 2) or (FOrgTable.RowCount <> 2 ) then exit;
  if ((FOrgTable.ColTotal[0] = 0) or (FOrgTable.ColTotal[1] = 0)) then exit;
  If (FOrgTable.Cell[0,0].N * FOrgTable.RowTotal[1]) < (FOrgTable.Cell[0,1].N * FOrgTable.RowTotal[0]) then
    Begin
    AF := FOrgTable.Cell[0,0].N; //A
    BF := FOrgTable.Cell[1,0].N; //B
    CF := FOrgTable.Cell[0,1].N; //C
    DF := FOrgTable.Cell[1,1].N; //D
    End (*If*)
  Else
    Begin
    AF := FOrgTable.Cell[0,1].N; //C;
    BF := FOrgTable.Cell[1,1].N; //D;
    CF := FOrgTable.Cell[0,0].N; //A;
    DF := FOrgTable.Cell[1,0].N; //B
  End (*Else*);
  N1F := AF + BF;
  N0F := CF + DF;
  M1F := AF + CF;
  M0F := BF + DF;
  TF := N1F + N0F;
  SN := 1;
  ON := 0;
  DEN := 1;
  A2 := AF;
  D2 := DF;
  B2 := BF + 1;
  C2 := CF + 1;
  X := 1;
  While (X > 9.999999e-21) Do
      Begin
      X := X * A2 * D2 / (B2 * C2);
      SN := X + SN;
      DEN := X + DEN;
      A2 := A2 - 1;
      D2 := D2 - 1;
      B2 := B2 + 1;
      C2 := C2 + 1
      End (*While*);
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
      End (*While*);
  // result := SN / DEN;  one sided fishers exact
  FFExP := (SN + ON) / DEN;

end;

procedure TTwoWayStatisticFExP.AddToOutput(OutputTable: TOutputTable);
var
  S: String;
begin
  if (FFExP < 0) then exit;
  S := 'Fisher Exact '+ FormatFExp(FFExP, true);
  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + S;
end;

procedure TTwoWayStatisticFExP.CreateResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);
begin
  if (FFExP < 0) then exit;
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

procedure TTwoWayStatisticsFExP.AddToSummaryTable(OutputTable: TOutputTable);
var
  ColIdx, i: Integer;
  Stat: TTwoWayStatisticFExP;
begin
  ColIdx := OutputTable.ColCount;

  OutputTable.ColCount := OutputTable.ColCount + 1;

  OutputTable.Cell[ColIdx    , 0].Text := 'Fisher Exact p';

  Stat := Statistics[0];
  if (Stat.FFExP >= 0) then
     OutputTable.Cell[ColIdx , 1].Text := FormatFExP(Stat.FFExP, false);

  OutputTable.Cell[ColIdx    , 2].Text := '-';

  for i := 1 to StatisticsCount - 1 do
    begin
      Stat := Statistics[i];
      if (Stat.FFExP >= 0) then
      OutputTable.Cell[ColIdx    , i + 2].Text := FormatFExP(Stat.FFExP, false);
    end;
end;

initialization
  RegisterTableStatistic(tsFExP, TTwoWayStatisticsFExP);

end.

