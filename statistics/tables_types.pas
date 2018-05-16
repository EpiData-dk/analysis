unit tables_types;

{$interfaces CORBA}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes, epidatafiles;

type

  { ITableCounts }

  ITableCounts = interface['ITableCounts']
    function GetCounts(Const Col, Row: Integer): Integer;
    function GetColVar: TEpiField;
    function GetRowVar: TEpiField;
    property Counts[Const Col, Row: Integer]: Integer read GetCounts;
    property ColVariable: TEpiField read GetColVar;
    property RowVariable: TEpiField read GetRowVar;
  end;


  { TTableCell }

  TTableCell = class
  private
    Fn: EpiInteger;
    FRowPct: EpiFloat;
    FColPct: EpiFloat;
    FTotalPct: EpiFloat;
  public
    constructor Create;
    property N:        EpiInteger read Fn;
    property RowPct:   EpiFloat   read FRowPct;
    property ColPct:   EpiFloat   read FColPct;
    property TotalPct: EpiFloat   read FTotalPct;
  end;


  { TTableVector }

  TTableVectorEnumerator = class;
  TTableVector = class
  private
    FCells: TList;
    function GetCells(const Index: Integer): TTableCell;
    function GetTotal: EpiInteger;
    function GetCount: Integer;
  public
    constructor Create;
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

  { TTwoWayTable }

  TTwoWayTable = class;
  TTwoWaySortCompare = function(I1, I2: Integer; const index: integer): Integer of object;
  TTwoWaySortExchange = procedure(Index1, Index2: Integer) of object;
  ETwoWaySortException = class(Exception);

  TTwoWayTable = class
  private
    FRowCount: Integer;
    FColCount: Integer;
    FCells: Array of Array of TTableCell;
    FRowAggrVariable: TEpiField;
    FColAggrVariable: TEpiField;
    FStratifyIndices: TBoundArray;
    function GetRowCount: Integer;
    function GetColumnCount: Integer;
    function GetTotal: EpiInteger;
    function GetCell(const Col, Row: Cardinal): TTableCell;
    function GetColTotal(const Index: Integer): EpiInteger;
    function GetRowTotal(const Index: Integer): EpiInteger;
    function GetCols(const Index: Integer): TTableVector;
    function GetRows(const Index: Integer): TTableVector;
  protected
    procedure DoError(Const Msg: UTF8String; EClass: ExceptClass);
  public
    constructor Create(TableCounts: ITableCounts; AStratifyIndices: TBoundArray);
    property StratifyIndices: TBoundArray read FStratifyIndices;
    property ColVariable: TEpiField read FColAggrVariable;
    property RowVariable: TEpiField read FRowAggrVariable;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
    property Total: EpiInteger read GetTotal;
    property Cell[const Col, Row: Cardinal]: TTableCell read GetCell;
    property ColTotal[const Index: Integer]: EpiInteger read GetColTotal;
    property RowTotal[const Index: Integer]: EpiInteger read GetRowTotal;
    property Cols[Const Index: Integer]: TTableVector read GetCols;
    property Rows[Const Index: Integer]: TTableVector read GetRows;

  { Simple Statistics }
  private
    FDf: Integer;
  public
    property DF: Integer read FDf;

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

  TTwoWayTables = class;

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
    FList: TList;
    function GetDFZeroCount: Integer;
    function GetTables(const Index: Integer): TTwoWayTable;
    function GetCount: Integer;
  public
    constructor Create(AStratifyVariables: TEpiFields = nil);
    procedure AddTable(Table: TTwoWayTable; GrandTable: Boolean = false);
    function GetEnumerator: TTwoWayTablesEnumerator;
    property Tables[Const Index: Integer]: TTwoWayTable read GetTables; default;
    property Count: Integer read GetCount;
    property StratifyVariables: TEpiFields read FStratifyVariables;
    property DFZeroCount: Integer read GetDFZeroCount;
  end;

implementation

uses
  epifields_helper, LazUTF8;

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

constructor TTableCell.Create;
begin
  Fn := 0;
  FRowPct   := 0;
  FColPct   := 0;
  FTotalPct := 0;
end;

{ TTableVector }

function TTableVector.GetCells(const Index: Integer): TTableCell;
begin
  Result := TTableCell(FCells[Index]);
end;

function TTableVector.GetTotal: EpiInteger;
var
  C: TTableCell;
begin
  Result := 0;
  for C in Self do
    Result := Result + C.N;
end;

function TTableVector.GetCount: Integer;
begin
  result := FCells.Count;
end;

constructor TTableVector.Create;
begin
  FCells := TList.Create;
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
var
  Row: Integer;
begin
  Result := 0;

  for Row := 0 to RowCount - 1 do
    Result := Result + Cell[Index, Row].N;
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
var
  Col: Integer;
begin
  Result := 0;

  for Col := 0 to ColCount - 1 do
    Result := Result + Cell[Col, Index].N;
end;

function TTwoWayTable.GetCols(const Index: Integer): TTableVector;
var
  Row: Integer;
begin
  Result := TTableVector.Create;
  for Row := 0 to RowCount - 1 do
    Result.FCells.Add(Cell[Index, Row]);
end;

function TTwoWayTable.GetRows(const Index: Integer): TTableVector;
var
  Col: Integer;
begin
  Result := TTableVector.Create;
  for Col := 0 to ColCount - 1 do
    Result.FCells.Add(Cell[Col, Index]);
end;

procedure TTwoWayTable.DoError(const Msg: UTF8String; EClass: ExceptClass);
begin
  raise EClass.Create(Msg);
end;

function TTwoWayTable.GetTotal: EpiInteger;
var
  Col, Row: Integer;
begin
  Result := 0;
  for Col := 0 to ColCount - 1 do
    for Row := 0 to RowCount - 1 do
      Result := Result + Cell[Col, Row].N;
end;

constructor TTwoWayTable.Create(TableCounts: ITableCounts; AStratifyIndices: TBoundArray);
var
  Col, Row: Integer;
  C: TTableCell;
begin
  FRowAggrVariable := TEpiField(TableCounts.RowVariable.Clone(nil));
  FRowAggrVariable.ValueLabelSet := TableCounts.RowVariable.ValueLabelSet;

  FColAggrVariable := TEpiField(TableCounts.ColVariable.Clone(nil));
  FColAggrVariable.ValueLabelSet := TableCounts.ColVariable.ValueLabelSet;

  FRowCount := FRowAggrVariable.Size;
  FColCount := FColAggrVariable.Size;

  SetLength(FCells, FColCount, FRowCount);

  for Col := 0 to ColCount - 1 do
    for Row := 0 to RowCount - 1 do
      begin
        FCells[Col, Row] := TTableCell.Create;
        FCells[Col, Row].Fn := TableCounts.Counts[Col, Row];
      end;

  for Col := 0 to ColCount - 1 do
    for Row := 0 to RowCount - 1 do
      begin
        C := Cell[Col, Row];
        C.FColPct := (C.N / ColTotal[Col]);
        C.FRowPct := (C.N / RowTotal[Row]);
      end;

  FDf := (RowCount - 1) * (ColCount - 1);
  FStratifyIndices := Copy(AStratifyIndices);
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
  Col: Integer;
  TempCell: TTableCell;
begin
  for Col := 0 to ColCount - 1 do
    begin
      TempCell := Cell[Col, Index1];
      FCells[Col, Index1] := Cell[Col, Index2];
      FCells[Col, Index2] := TempCell;
    end;
  RowVariable.Exchange(Index1, Index2);
end;

procedure TTwoWayTable.ExchangeColumns(Index1, Index2: Integer);
var
  Row: Integer;
  TempCell: TTableCell;
begin
  for Row := 0 to RowCount - 1 do
    begin
      TempCell := Cell[Index1, Row];
      FCells[Index1, Row] := Cell[Index2, Row];
      FCells[Index2, Row] := TempCell;
    end;
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
  result := TTwoWayTable(FList[Index]);
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
  result := FList.Count;
end;

constructor TTwoWayTables.Create(AStratifyVariables: TEpiFields);
begin
  FList := TList.Create;
  FStratifyVariables := AStratifyVariables;
end;

procedure TTwoWayTables.AddTable(Table: TTwoWayTable; GrandTable: Boolean);
begin
  if GrandTable then
    FList.Insert(0, Table)
  else
    FList.Add(Table);
end;

function TTwoWayTables.GetEnumerator: TTwoWayTablesEnumerator;
begin
  result := TTwoWayTablesEnumerator.Create(Self);
end;

end.

