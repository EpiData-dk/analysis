unit tables_stat_chi2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes;

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
  public
    procedure CalcTable(Table: TTwoWayTable); override;
    procedure AddToOutput(OutputTable: TOutputTable); override;
    procedure DebugOutput(OutputCreator: TOutputCreator); override;
  end;

  { TTwoWayStatisticsChi2 }

  TTwoWayStatisticsChi2 = class(TTwoWayStatistics)
  protected
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable); override;
  end;

implementation

uses
  tables;

{ TTwoWayStatisticsChi2 }

function TTwoWayStatisticsChi2.GetTwoWayStatisticClass: TTwoWayStatisticClass;
begin
  result := TTwoWayStatisticChi2;
end;

procedure TTwoWayStatisticsChi2.AddToSummaryTable(OutputTable: TOutputTable);
begin
  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + 'JUST TESTINGS!!!';
end;

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
  Row, Col: Integer;
  C: TTableCellChi2Expected;
  Val: EpiFloat;
begin
  FOrgTable := Table;
  FExpected := TTwoWayTable.Create(FOrgTable.ColCount, FOrgTable.RowCount, TTableCellChi2Expected);
  FChi2     := 0;

  for Row := 0 to FExpected.RowCount - 1 do
    for Col := 0 to FExpected.ColCount - 1 do
      begin
        C   := TTableCellChi2Expected(FExpected.Cell[Col, Row]);
        C.N := (FOrgTable.RowTotal[Row] * FOrgTable.ColTotal[Col]) / FOrgTable.Total;
        Val := (FOrgTable.Cell[Col, Row].N - C.N);

        FChi2 := FChi2 + ((Val * Val) / C.N);
      end;
end;

procedure TTwoWayStatisticChi2.AddToOutput(OutputTable: TOutputTable);
var
  S: String;
begin
  S := 'Chi{\S 2}: ' + Format('%.2f', [FChi2]) + LineEnding +
       'Df: ' + IntToStr(FOrgTable.DF);
  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + S;
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

initialization
  RegisterTableStatistic(tsChi2, TTwoWayStatisticsChi2);

end.

