unit tables_types;

{$interfaces CORBA}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes, epidatafiles, epifields_helper,
  outputcreator, fgl, executor, ast, ana_globals, result_variables;

type

  TTwoWayTable = class;
  TTwoWayTables = class;

  // The order here MUST match the order in which statistics are output in ctable.pas
  // This also sets the order or priority for ctable option !ss (tsRR always used, if present, etc)
  TTableStatistic  = (
    tsRR,
    tsOR,
    tsFExP,
    tsChi2
  );

  TTableStatistics = set of TTableStatistic;

  {TStatResult}

  TStatResult = array of TExecVarVector;

  { TTwoWayStatistic }

  TTwoWayStatistic = class
  public
    procedure CalcTable(Table: TTwoWayTable; Conf: Integer = 95); virtual; abstract;
    procedure AddToOutput(OutputTable: TOutputTable; Options: TOptionList); virtual; abstract;     //*
    procedure CreateResultVariables(Executor: TExecutor; Const NamePrefix: UTF8String); virtual;
    procedure DebugOutput(OutputCreator: TOutputCreator); virtual;
  end;
  TTwoWayStatisticClass = class of TTwoWayStatistic;

  { TTwoWayStatistics }

  TTwoWayStatistics = class
  private
    FStatistics: TList;
    function GetStatisticsCount: Integer;
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatistic; virtual;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; virtual; abstract;
    property Statistics[Const Index: Integer]: TTwoWayStatistic read GetStatistics;
    property StatisticsCount: Integer read GetStatisticsCount;
  public
    constructor Create; virtual;
    procedure CalcTables(Tables: TTwoWayTables; Executor: TExecutor);
    procedure CreateResultVariables(Tables: TTwoWayTables; Executor: TExecutor); virtual;
    procedure AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList); virtual;  abstract;  //*
    procedure AddToCompactTable(Executor: TExecutor; T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList); virtual; abstract;
    procedure AddToCompactHeader(T: TOutputTable; Options: TOptionList); virtual; abstract;
    function  CompactSortValue: EpiFloat; virtual; abstract;
    function  CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String; ResultRows: Integer): TStatResult; virtual; abstract;
    procedure AddCompactResultVariables(Executor: TExecutor; Index: Integer; Results: TStatResult); virtual; abstract;
    procedure AddCompactResultVariables(Executor: TExecutor; Prefix: UTF8String; Index: Integer; ResultTables: TTwoWayTables; ResultRows: Integer); virtual; abstract;
    procedure CalcSummaryStatistics(Tables: TTwoWayTables; Conf: Integer = 95); virtual;
    procedure CreateSummaryResultVariables(Executor: TExecutor; Const NamePrefix: UTF8STRING); virtual;
    property TwoWayStatisticClass: TTwoWayStatisticClass read GetTwoWayStatisticClass;
  end;
  TTwoWayStatisticsClass = class of TTwoWayStatistics;

  { TTableCell }

  TTableCell = class
  private
    Fn: EpiInteger;
    FCol, FRow: Integer;
    FTable: TTwoWayTable;
    function GetColPct: EpiFloat;
    function GetRowPct: EpiFloat;
    function GetTotalPct: EpiFloat;
    procedure SetN(AValue: EpiInteger);
  protected
    property Table: TTwoWayTable read FTable;
    property Col: Integer read FCol;
    property Row: Integer read FRow;
  public
    constructor Create(TwoWayTable: TTwoWayTable; ACol, ARow: Integer);
    property N:        EpiInteger read Fn write SetN;
    property ColPct:   EpiFloat   read GetColPct;
    property RowPct:   EpiFloat   read GetRowPct;
    property TotalPct: EpiFloat   read GetTotalPct;
  end;
  TTableCellClass = class of TTableCell;

  { TTableVector }

  TTableVectorType = (tvtColumn, tvtRow);
  TTableVectorEnumerator = class;
  TTableVector = class
  private
    FTable: TTwoWayTable;
    FIndex: Integer;
    FVectorType: TTableVectorType;
    function GetCells(const Index: Integer): TTableCell;
    function GetTotal: EpiInteger;
    function GetCount: Integer;
  public
    constructor Create(TwoWayTable: TTwoWayTable; Index: Integer; VectorType: TTableVectorType);
    function GetEnumerator: TTableVectorEnumerator;
    property Cells[Const Index: Integer]: TTableCell read GetCells;
    property Total: EpiInteger read GetTotal;
    property Count: Integer read GetCount;
  end;

  { TTableVectorEnumerator }

  TTableVectorEnumerator = class
  private
    FVector: TTableVector;
    FIndex: Integer;
    function GetCurrent: TTableCell;
  public
    constructor Create(Vector: TTableVector);
    function MoveNext: Boolean;
    property Current: TTableCell read GetCurrent;
  end;

  { ITableCounts }

  ITableCounts = interface['ITableCounts']
    function GetCounts(Const Col, Row: Integer): Integer;
    property Counts[Const Col, Row: Integer]: Integer read GetCounts;
  end;

  ITableVariables = interface(ITableCounts)['ITableVariables']
    function GetColVar: TEpiField;
    function GetRowVar: TEpiField;
    property ColVariable: TEpiField read GetColVar;
    property RowVariable: TEpiField read GetRowVar;
  end;

  { TTwoWayTable }

  TTwoWaySortCompare = function(I1, I2: Integer; const index: integer): Integer of object;
  TTwoWaySortExchange = procedure(Index1, Index2: Integer) of object;
  ETwoWaySortException = class(Exception);

  TTwoWayTable = class(ITableCounts)
  private
    FRowCount: Integer;
    FColCount: Integer;
    FCells: Array of Array of TTableCell;
    FRowAggrVariable: TEpiField;
    FColAggrVariable: TEpiField;
    FStratifyIndices: TBoundArray;

  { Housekeeping for totals }
  private
    FTotal: EpiInteger;
    FColTotals: TBoundArray;
    FRowTotals: TBoundArray;
    function GetRowCount: Integer;
    function GetColumnCount: Integer;
    function GetCell(const Col, Row: Cardinal): TTableCell;
    function GetColTotal(const Index: Integer): EpiInteger;
    function GetRowTotal(const Index: Integer): EpiInteger;
    function GetCols(const Index: Integer): TTableVector;
    function GetRows(const Index: Integer): TTableVector;
    function GetCounts(Const Col, Row: Integer): Integer;
    function GetColTotalPct(const Index: Integer): EpiFloat;
    function GetRowTotalPct(const Index: Integer): EpiFloat;
    function GetTotal: EpiInteger;
    procedure CalcTotals;
  protected
    procedure DoError(Const Msg: UTF8String; EClass: ExceptClass);
    procedure DoMarkDirty(Col, Row: Integer); virtual;
  public
    constructor Create(AggregatedTable: ITableVariables; AStratifyIndices: TBoundArray); overload;
    constructor Create(Cols, Rows: Integer; CellClass: TTableCellClass); overload;
    procedure MarkDirty(Col, Row: Integer);
    property StratifyIndices: TBoundArray read FStratifyIndices;
    property ColVariable: TEpiField read FColAggrVariable;
    property RowVariable: TEpiField read FRowAggrVariable;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
    property Total: EpiInteger read GetTotal;
    property Counts[Const Col, Row: Integer]: Integer read GetCounts;
    property Cell[const Col, Row: Cardinal]: TTableCell read GetCell;
    property ColTotal[const Index: Integer]: EpiInteger read GetColTotal;
    property RowTotal[const Index: Integer]: EpiInteger read GetRowTotal;
    property ColTotalPct[const Index: Integer]: EpiFloat read GetColTotalPct;
    property RowTotalPct[const Index: Integer]: EpiFloat read GetRowTotalPct;
    property Cols[Const Index: Integer]: TTableVector read GetCols;
    property Rows[Const Index: Integer]: TTableVector read GetRows;

  { Simple Statistics }
  protected
    function GetDf: Integer;
  public
    property DF: Integer read GetDf;

  { Advanced Statistics Inteface }
  private
    FStatisticsList: TList;
    function GetStatisticsCount: Integer;
    function GetStatistics(const Index: Integer): TTwoWayStatistic;
  public
    procedure AddStatistic(Stat: TTwoWayStatistic);
    property Statistics[Const Index: Integer]: TTwoWayStatistic read GetStatistics;
    property StatisticsCount: Integer read GetStatisticsCount;

  { Sorting }
  private
    // Compare functions
    function  CompareColValue(I1, I2: Integer; const index: integer): Integer;
    function  CompareColLabel(I1, I2: Integer; const index: integer): Integer;
    function  CompareCols(I1, I2: Integer; const index: integer): Integer;
    function  CompareRowValue(I1, I2: Integer; const index: integer): Integer;
    function  CompareRowLabel(I1, I2: Integer; const index: integer): Integer;
    function  CompareRows(I1, I2: Integer; const index: integer): Integer;
    function  CompareColTotals(I1, I2: Integer; const index: integer): Integer;
    function  CompareRowTotals(I1, I2: Integer; const index: integer): Integer;
    // Exchange functions
    procedure ExchangeRows(Index1, Index2: Integer);
    procedure ExchangeColumns(Index1, Index2: Integer);
    // Sorting algorithmn
    procedure InternalSortGeneric(CompareFunction: TTwoWaySortCompare; ExchangeFunction: TTwoWaySortExchange; L, R: integer; const Index: integer; Desc: boolean);
  public
    procedure Sort(CompareFunction: TTwoWaySortCompare; ExchangeFunction: TTwoWaySortExchange; StartIndex, EndIndex: integer; const CustomIndex: integer; Descending: boolean);
    procedure SortByColValue(Desc: boolean);
    procedure SortByColLabel(Desc: boolean);
    procedure SortByCol(const Index: Integer; Desc: boolean);
    procedure SortByColTotal(Desc: boolean);
    procedure SortByRowValue(Desc: boolean);
    procedure SortByRowLabel(Desc: boolean);
    procedure SortByRow(const Index: Integer; Desc: boolean);
    procedure SortByRowTotal(Desc: boolean);
  end;

  { TTwoWayTablesEnumerator }

  TTwoWayTablesEnumerator = class
  private
    FTables: TTwoWayTables;
    FCurrentIndex: Integer;
    function GetCurrent: TTwoWayTable;
  public
    constructor Create(Tables: TTwoWayTables);
    function MoveNext: Boolean;
    property Current: TTwoWayTable read GetCurrent;
  end;

  { TTwoWayTables }

  TTwoWayTables = class
  private
    FStratifyVariables: TEpiFields;
    FStratifiedTables: TList;
    FUnstratifiedTable: TTwoWayTable;
    function GetDFZeroCount: Integer;
    function GetTables(const Index: Integer): TTwoWayTable;
    function GetCount: Integer;
    procedure DoError(Const Msg: UTF8String);

  { Advanced Statistics Interface }
  private
    FStatisticsList: TList;
    function GetStatisticsCount: Integer;
    function GetStatistics(const Index: Integer): TTwoWayStatistics;
  public
    procedure AddStatistic(Stat: TTwoWayStatistics);
    property Statistics[Const Index: Integer]: TTwoWayStatistics read GetStatistics;
    property StatisticsCount: Integer read GetStatisticsCount;

  public
    constructor Create(AStratifyVariables: TEpiFields = nil);
    procedure AddTable(Table: TTwoWayTable; StratifiedTable: Boolean = false);
    function GetEnumerator: TTwoWayTablesEnumerator;
    property UnstratifiedTable: TTwoWayTable read FUnstratifiedTable;
    property Tables[Const Index: Integer]: TTwoWayTable read GetTables; default;
    property Count: Integer read GetCount;
    property StratifyVariables: TEpiFields read FStratifyVariables;
    property DFZeroCount: Integer read GetDFZeroCount;
  end;

  TStatisticsMap = specialize TFPGMap<TTableStatistic, TTwoWayStatisticsClass>;

implementation

uses
  LazUTF8, math;

{ TTwoWayStatistic }

procedure TTwoWayStatistic.CreateResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);
begin
  // Create nothing
end;

procedure TTwoWayStatistic.DebugOutput(OutputCreator: TOutputCreator);
begin
  // Do nothing...
end;

{ TTwoWayStatistics }

function TTwoWayStatistics.GetStatistics(const Index: Integer
  ): TTwoWayStatistic;
begin
  result := TTwoWayStatistic(FStatistics[Index]);
end;

function TTwoWayStatistics.GetStatisticsCount: Integer;
begin
  result := FStatistics.Count;
end;

constructor TTwoWayStatistics.Create;
begin
  FStatistics := TList.Create;
end;

procedure TTwoWayStatistics.CalcTables(Tables: TTwoWayTables; Executor: TExecutor);
var
  Stat: TTwoWayStatistic;
  Tab: TTwoWayTable;
  Conf: Integer;
begin
  Conf := StrToInt(Executor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  // First do calculations on the unstratified table
  Tab := Tables.UnstratifiedTable;
  Stat := TwoWayStatisticClass.Create;
  Stat.CalcTable(Tab, Conf);
  FStatistics.Add(Stat);
  Tab.AddStatistic(Stat);

  // Then do same calculation on each stratified table
  for Tab in Tables do
    begin
      Stat := TwoWayStatisticClass.Create;
      Stat.CalcTable(Tab, Conf);
      FStatistics.Add(Stat);
      Tab.AddStatistic(Stat);
    end;

  // Finally do any summary calculations that depend on stratified table calculations
   CalcSummaryStatistics(Tables, Conf);

end;

procedure TTwoWayStatistics.CreateResultVariables(Tables: TTwoWayTables;
  Executor: TExecutor);
var
  i: Integer;
  S: UTF8String;
begin
  S := '';
  for i := 0 to StatisticsCount - 1 do
    begin
      if (i = 0) then
        S := '$tables_unstratified_'
      else
        S := '$tables_table' + IntToStr(i) + '_';

      Statistics[i].CreateResultVariables(Executor, S);
    end;
  S := '$tables_adjusted_';
  CreateSummaryResultVariables(Executor, S);

end;

{ TTableVectorEnumerator }

function TTableVectorEnumerator.GetCurrent: TTableCell;
begin
  result := FVector.Cells[FIndex];
end;

constructor TTableVectorEnumerator.Create(Vector: TTableVector);
begin
  FVector := Vector;
  FIndex := -1;
end;

function TTableVectorEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  result := (FIndex < FVector.Count);
end;

{ TTableCell }

function TTableCell.GetColPct: EpiFloat;
begin
  if (FTable.ColTotal[FCol] = 0) then
    result := math.NaN
  else
    result := N / FTable.ColTotal[FCol];
end;

function TTableCell.GetRowPct: EpiFloat;
begin
  if (FTable.RowTotal[FRow] = 0) then
    result := math.NaN
  else
    result := N / FTable.RowTotal[FRow];
end;

function TTableCell.GetTotalPct: EpiFloat;
begin
  if (FTable.Total = 0) then
    result := math.NaN
  else
    result := N / FTable.Total;
end;

procedure TTableCell.SetN(AValue: EpiInteger);
begin
  if Fn = AValue then Exit;
  Fn := AValue;
  FTable.MarkDirty(FCol, FRow);
end;

constructor TTableCell.Create(TwoWayTable: TTwoWayTable; ACol, ARow: Integer);
begin
  Fn := 0;
  FTable := TwoWayTable;
  FCol := ACol;
  FRow := ARow;
end;

{ TTableVector }

function TTableVector.GetCells(const Index: Integer): TTableCell;
begin
  case FVectorType of
    tvtColumn: result := FTable.Cell[FIndex, Index];
    tvtRow:    result := FTable.Cell[Index, FIndex];
  end;
end;

function TTableVector.GetTotal: EpiInteger;
begin
  case FVectorType of
    tvtColumn: Result := FTable.ColTotal[FIndex];
    tvtRow:    Result := FTable.RowTotal[FIndex];
  end;
end;

function TTableVector.GetCount: Integer;
begin
  case FVectorType of
    tvtColumn: Result := FTable.ColCount;
    tvtRow:    Result := FTable.RowCount;
  end;
end;

constructor TTableVector.Create(TwoWayTable: TTwoWayTable; Index: Integer;
  VectorType: TTableVectorType);
begin
  FTable := TwoWayTable;
  FIndex := Index;
  FVectorType := VectorType;
end;

function TTableVector.GetEnumerator: TTableVectorEnumerator;
begin
  Result := TTableVectorEnumerator.Create(Self);
end;

{ TTwoWayTable }

function TTwoWayTable.GetCell(const Col, Row: Cardinal): TTableCell;
begin
  result := FCells[Col, Row];
end;

function TTwoWayTable.GetColTotal(const Index: Integer): EpiInteger;
begin
  if (FColTotals[Index] < 0) then
    CalcTotals;
  Result := FColTotals[Index];
end;

function TTwoWayTable.GetColumnCount: Integer;
begin
  result := FColCount;
end;

function TTwoWayTable.GetRowCount: Integer;
begin
  result := FRowCount;
end;

function TTwoWayTable.GetRowTotal(const Index: Integer): EpiInteger;
begin
  if (FRowTotals[Index] < 0) then
    CalcTotals;
  Result := FRowTotals[Index];
end;

function TTwoWayTable.GetCols(const Index: Integer): TTableVector;
begin
  Result := TTableVector.Create(Self, Index, tvtColumn);
end;

function TTwoWayTable.GetRows(const Index: Integer): TTableVector;
begin
  Result := TTableVector.Create(Self, Index, tvtRow);
end;

function TTwoWayTable.GetCounts(const Col, Row: Integer): Integer;
begin
  result := Cell[Col, Row].N;
end;

function TTwoWayTable.GetColTotalPct(const Index: Integer): EpiFloat;
begin
  if (Total = 0) then
    result := math.NaN
  else
    result := ColTotal[Index] / Total;
end;

function TTwoWayTable.GetRowTotalPct(const Index: Integer): EpiFloat;
begin
  if (Total = 0) then
    result := math.NaN
  else
    result := RowTotal[Index] / Total;
end;

function TTwoWayTable.GetTotal: EpiInteger;
begin
  if (FTotal < 0) then
    CalcTotals;

  Result := FTotal;
end;

procedure TTwoWayTable.CalcTotals;
var
  Val: EpiInteger;
  Col, Row: Integer;
begin
  FTotal := 0;

  {$IFDEF CPU32}
  FillDWord(FColTotals[0], ColCount, 0);
  FillDWord(FRowTotals[0], RowCount, 0);
  {$ENDIF}

  {$IFDEF CPU64}
  FillQWord(FColTotals[0], ColCount, 0);
  FillQWord(FRowTotals[0], RowCount, 0);
  {$ENDIF}

  for Col := 0 to ColCount - 1 do
    for Row := 0 to RowCount - 1 do
      begin
        Val := Cell[Col, Row].N;
        FTotal += Val;
        FColTotals[Col] += Val;
        FRowTotals[Row] += Val;
      end;
end;

procedure TTwoWayTable.DoError(const Msg: UTF8String; EClass: ExceptClass);
begin
  raise EClass.Create(Msg);
end;

procedure TTwoWayTable.DoMarkDirty(Col, Row: Integer);
begin
  FTotal          := -1;
  FColTotals[Col] := -1;
  FRowTotals[Row] := -1;
end;

procedure TTwoWayTable.MarkDirty(Col, Row: Integer);
begin
  DoMarkDirty(Col, Row);
end;

function TTwoWayTable.GetDf: Integer;
begin
  result := (ColCount - 1) * (RowCount - 1);
end;

constructor TTwoWayTable.Create(AggregatedTable: ITableVariables;
  AStratifyIndices: TBoundArray);
var
  Col, Row: Integer;
  C: TTableCell;
  TotalVal: EpiInteger;
  Val: LongInt;
begin
  FColAggrVariable := TEpiField(AggregatedTable.ColVariable.Clone(nil));
  FColAggrVariable.ValueLabelSet := AggregatedTable.ColVariable.ValueLabelSet;

  FRowAggrVariable := TEpiField(AggregatedTable.RowVariable.Clone(nil));
  FRowAggrVariable.ValueLabelSet := AggregatedTable.RowVariable.ValueLabelSet;

  Create(FColAggrVariable.Size, FRowAggrVariable.Size, TTableCell);

  for Col := 0 to ColCount - 1 do
    for Row := 0 to RowCount - 1 do
      Cell[Col, Row].N := AggregatedTable.Counts[Col, Row];

  FStratifyIndices := Copy(AStratifyIndices);
end;

constructor TTwoWayTable.Create(Cols, Rows: Integer; CellClass: TTableCellClass
  );
var
  Col, Row: Integer;
begin
  FColCount := Cols;
  FRowCount := Rows;

  FTotal := 0;
  SetLength(FCells, FColCount, FRowCount);
  SetLength(FColTotals, FColCount);
  SetLength(FRowTotals, FRowCount);

  for Col := 0 to ColCount - 1 do
    for Row := 0 to RowCount - 1 do
      FCells[Col, Row] := CellClass.Create(Self, Col, Row);

  FStatisticsList := TList.Create;
end;

function TTwoWayTable.GetStatisticsCount: Integer;
begin
  result := FStatisticsList.Count;
end;

function TTwoWayTable.GetStatistics(const Index: Integer): TTwoWayStatistic;
begin
  result := TTwoWayStatistic(FStatisticsList[Index]);
end;

procedure TTwoWayTable.AddStatistic(Stat: TTwoWayStatistic);
begin
  FStatisticsList.Add(Stat);
end;

function TTwoWayTable.CompareColValue(I1, I2: Integer; const index: integer): Integer;
begin
  result := ColVariable.Compare(I1, I2);
end;

function TTwoWayTable.CompareColLabel(I1, I2: Integer; const index: integer
  ): Integer;
begin
  result := UTF8CompareStr(ColVariable.GetValueLabel(I1), ColVariable.GetValueLabel(I2));
end;

function TTwoWayTable.CompareCols(I1, I2: Integer; const index: integer
  ): Integer;
begin
  result := Cell[Index, I1].N - Cell[Index, I2].N;
end;

function TTwoWayTable.CompareRowValue(I1, I2: Integer; const index: integer
  ): Integer;
begin
  result := RowVariable.Compare(I1, I2);
end;

function TTwoWayTable.CompareRowLabel(I1, I2: Integer; const index: integer
  ): Integer;
begin
  result := UTF8CompareStr(RowVariable.GetValueLabel(I1), RowVariable.GetValueLabel(I2));
end;

function TTwoWayTable.CompareRows(I1, I2: Integer; const index: integer
  ): Integer;
begin
  result := Cell[I1, Index].N - Cell[I2, Index].N;
end;

function TTwoWayTable.CompareColTotals(I1, I2: Integer; const index: integer
  ): Integer;
begin
  result := ColTotal[I1] - ColTotal[I2];
end;

function TTwoWayTable.CompareRowTotals(I1, I2: Integer; const index: integer
  ): Integer;
begin
  result := RowTotal[I1] - RowTotal[I2];
end;

procedure TTwoWayTable.ExchangeRows(Index1, Index2: Integer);
var
  Col, Val: Integer;
  TempCell: TTableCell;
begin
  for Col := 0 to ColCount - 1 do
    begin
      TempCell      := Cell[Col, Index1];
      TempCell.FRow := Index2;
      FCells[Col, Index1] := Cell[Col, Index2];
      FCells[Col, Index1].FRow := Index1;
      FCells[Col, Index2] := TempCell;
    end;

  Val := FRowTotals[Index1];
  FRowTotals[Index1] := FRowTotals[Index2];
  FRowTotals[Index2] := Val;

  RowVariable.Exchange(Index1, Index2);
end;

procedure TTwoWayTable.ExchangeColumns(Index1, Index2: Integer);
var
  Row, Val: Integer;
  TempCell: TTableCell;
begin
  for Row := 0 to RowCount - 1 do
    begin
      TempCell := Cell[Index1, Row];
      TempCell.FCol := Index2;
      FCells[Index1, Row] := Cell[Index2, Row];
      FCells[Index1, Row].FCol := Index1;
      FCells[Index2, Row] := TempCell;
    end;

  Val := FColTotals[Index1];
  FColTotals[Index1] := FColTotals[Index2];
  FColTotals[Index2] := Val;

  ColVariable.Exchange(Index1, Index2);
end;

procedure TTwoWayTable.InternalSortGeneric(CompareFunction: TTwoWaySortCompare;
  ExchangeFunction: TTwoWaySortExchange; L, R: integer; const Index: integer;
  Desc: boolean);
var
   I, J, P: Integer;
begin
  I:=L;
  J:=R;
  P:=(L + R) shr 1;
  repeat
    if Desc then
    begin
      while CompareFunction(I, P, Index) > 0 do Inc(I);
      while CompareFunction(J, P, Index) < 0 do Dec(J);
    end else begin
      while CompareFunction(I, P, Index) < 0 do Inc(I);
      while CompareFunction(J, P, Index) > 0 do Dec(J);
    end;
    if I <= J then
    begin
      ExchangeFunction(J, I);
      if (P = I) then
        P := J
      else
        if (P = J) then
          P := I;

      Inc(I);
      Dec(J);
    end;
  until I > J;

  if (J > L) then InternalSortGeneric(CompareFunction, ExchangeFunction, L, J, Index, Desc);
  if (I < R) then InternalSortGeneric(CompareFunction, ExchangeFunction, I, R, Index, Desc);
end;

procedure TTwoWayTable.Sort(CompareFunction: TTwoWaySortCompare; ExchangeFunction: TTwoWaySortExchange;
  StartIndex, EndIndex: integer; const CustomIndex: integer; Descending: boolean);
begin
  InternalSortGeneric(CompareFunction, ExchangeFunction, StartIndex, EndIndex, CustomIndex, Descending);
end;

procedure TTwoWayTable.SortByColValue(Desc: boolean);
begin
  Sort(@CompareColValue, @ExchangeColumns, 0, ColCount - 1, 0, Desc);
end;

procedure TTwoWayTable.SortByColLabel(Desc: boolean);
begin
  Sort(@CompareColLabel, @ExchangeColumns, 0, ColCount - 1, 0, Desc);
end;

procedure TTwoWayTable.SortByCol(const Index: Integer; Desc: boolean);
begin
  if (Index < 0) or (Index >= ColCount) then DoError('Sorting index out of bounds', ETwoWaySortException);
  Sort(@CompareCols, @ExchangeRows, 0, RowCount - 1,    index, desc);
end;

procedure TTwoWayTable.SortByColTotal(Desc: boolean);
begin
  Sort(@CompareColTotals, @ExchangeColumns, 0, ColCount - 1, 0, desc);
end;

procedure TTwoWayTable.SortByRowValue(Desc: boolean);
begin
  Sort(@CompareRowValue, @ExchangeRows, 0, RowCount - 1, 0, Desc);
end;

procedure TTwoWayTable.SortByRowLabel(Desc: boolean);
begin
  Sort(@CompareRowLabel, @ExchangeRows, 0, RowCount - 1, 0, Desc);
end;

procedure TTwoWayTable.SortByRow(const Index: Integer; Desc: boolean);
begin
  if (Index < 0) or (Index >= RowCount) then DoError('Sorting index out of bounds', ETwoWaySortException);
  Sort(@CompareRows, @ExchangeColumns, 0, ColCount - 1, index, desc);
end;

procedure TTwoWayTable.SortByRowTotal(Desc: boolean);
begin
  Sort(@CompareRowTotals, @ExchangeRows, 0, RowCount - 1, 0, desc);
end;

{ TTwoWayTablesEnumerator }

function TTwoWayTablesEnumerator.GetCurrent: TTwoWayTable;
begin
  Result := FTables[FCurrentIndex];
end;

constructor TTwoWayTablesEnumerator.Create(Tables: TTwoWayTables);
begin
  FTables := Tables;
  FCurrentIndex := -1;
end;

function TTwoWayTablesEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  result := (FCurrentIndex < FTables.Count);
end;

{ TTwoWayTables }

function TTwoWayTables.GetTables(const Index: Integer): TTwoWayTable;
begin
  result := TTwoWayTable(FStratifiedTables[Index]);
end;

function TTwoWayTables.GetDFZeroCount: Integer;
var
  Tab: TTwoWayTable;
begin
  Result := 0;
  for Tab in Self do
    if (Tab <> Tables[0]) and
       (Tab.DF = 0)
    then
      Inc(Result);
end;

function TTwoWayTables.GetCount: Integer;
begin
  result := FStratifiedTables.Count;
end;

procedure TTwoWayTables.DoError(const Msg: UTF8String);
begin
  raise Exception.Create(Msg);
end;

function TTwoWayTables.GetStatisticsCount: Integer;
begin
  result := FStatisticsList.Count;
end;

function TTwoWayTables.GetStatistics(const Index: Integer): TTwoWayStatistics;
begin
  Result := TTwoWayStatistics(FStatisticsList[Index]);
end;

procedure TTwoWayTables.AddStatistic(Stat: TTwoWayStatistics);
begin
  FStatisticsList.Add(Stat);
end;

constructor TTwoWayTables.Create(AStratifyVariables: TEpiFields);
begin
  FStratifiedTables := TList.Create;
  FStratifyVariables := AStratifyVariables;
  FUnstratifiedTable := nil;
  FStatisticsList    := TList.Create;
end;

procedure TTwoWayTables.AddTable(Table: TTwoWayTable; StratifiedTable: Boolean);
begin
  if StratifiedTable then
    begin
      if Assigned(FUnstratifiedTable) then
        DoError('Unstratified table already assigned');

      FUnstratifiedTable := Table;
    end
  else
    FStratifiedTables.Add(Table);
end;

function TTwoWayTables.GetEnumerator: TTwoWayTablesEnumerator;
begin
  result := TTwoWayTablesEnumerator.Create(Self);
end;

procedure TTwoWayStatistics.CalcSummaryStatistics(Tables: TTwoWayTables; Conf: Integer);
begin
  // do nothing
end;

procedure TTwoWayStatistics.CreateSummaryResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8STRING);
begin
  // do nothing
end;

end.

