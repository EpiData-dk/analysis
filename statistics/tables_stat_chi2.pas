unit tables_stat_chi2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes;

type

  { TChi2ExpectedTableCell }

  TChi2ExpectedTableCell = class(TTableCell)
  private
    FN: EpiFloat;
    procedure SetN(AValue: EpiFloat);
  public
    property N: EpiFloat read FN write SetN;
  end;


  { TTableStatChi2 }

  TTableStatChi2 = class(TObject, ITwoWayStatistic)
  private
    FOrgTable: TTwoWayTable;
    FExpected: TTwoWayTable;
    FChi2: EpiFloat;
  public
    function GetTableStatistic: TTableStatistic;
    procedure CalcTable(Table: TTwoWayTable);
    procedure AddToOutput(OutputTable: TOutputTable);
  end;

implementation

{ TChi2ExpectedTableCell }

procedure TChi2ExpectedTableCell.SetN(AValue: EpiFloat);
begin
  if FN = AValue then Exit;
  FN := AValue;
  Table.MarkDirty(Col, Row);
end;

{ TTableStatChi2 }

function TTableStatChi2.GetTableStatistic: TTableStatistic;
begin
  result := tsChi2;
end;

procedure TTableStatChi2.CalcTable(Table: TTwoWayTable);
var
  Row, Col: Integer;
  C: TChi2ExpectedTableCell;
begin
  FOrgTable := Table;
  FExpected := TTwoWayTable.Create(FOrgTable.ColCount, FOrgTable.RowCount, TChi2ExpectedTableCell);
  FChi2     := 0;

  for Row := 0 to FExpected.RowCount - 1 do
    for Col := 0 to FExpected.ColCount - 1 do
      begin
        C   := TChi2ExpectedTableCell(FExpected.Cell[Col, Row]);
        C.N := (FOrgTable.RowTotal[Row] * FOrgTable.ColTotal[Col]) / FOrgTable.Total;
        FChi2 := FChi2 + (C.N - FOrgTable.Cell[Col, Row].N);
      end;
end;

procedure TTableStatChi2.AddToOutput(OutputTable: TOutputTable);
var
  S: String;
begin
  S := 'Chi{\S 2}: ' + Format('%8.2f', [FChi2]); // FloatToStr(FChi2) - always two decimals on Chi2 value;
  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + S;
end;

end.

