unit tables_stat_chi2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, statfunctions;

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
    FChiDF: Integer;
    FClt5: Integer;
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
  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + 'JUST TESTING!!!';
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
  // Val: EpiFloat;
begin
  FOrgTable := Table;
  FExpected := TTwoWayTable.Create(FOrgTable.ColCount, FOrgTable.RowCount, TTableCellChi2Expected);
  FChi2     := 0;
  FClt5     := 0;  // should this be a property N5 of object  TTableCellChi2Expected  instead ??

  for Row := 0 to FExpected.RowCount - 1 do
    for Col := 0 to FExpected.ColCount - 1 do
      begin
        if (FExpected.Cell[Col, Row].N < 5) then  FClt5 := FClt5 +1;  // gives always
        C   := TTableCellChi2Expected(FExpected.Cell[Col, Row]);
        C.N := (FOrgTable.RowTotal[Row] * FOrgTable.ColTotal[Col]) / FOrgTable.Total;
        Val := (FOrgTable.Cell[Col, Row].N - C.N);
        // for Yates' correction:
        // Val := (abs(FOrgTable.Cell[Col, Row].N - C.N) - 0.05);
        if C.N > 0 then FChi2 := FChi2 + ((Val * Val) / C.N); // No contribution if C.N = 0  or impose Yates !!
      end;
  FChiDF := (FExpected.RowCount - 1) * (FExpected.ColCount - 1);
  FChiP  := ChiPValue(FChi2 , FChiDF);
end;

procedure TTwoWayStatisticChi2.AddToOutput(OutputTable: TOutputTable);
var
  S: String;
begin
  S := 'Chi{\S 2}: ' + Format('%.3f', [FChi2]) +
       ' Df(' + IntToStr(FOrgTable.DF)+') ';   // removed LineEnding +
  if (FChiP < 0.0001) then
    S := S + ' p<0.0001'
  else
    if (FChiP < 0.001) then
      S := S + ' p<0.001'
  else
    S := S + ' p=' + Format('%.3f', [FChiP]) ;
  if (FClt5 > 5) then  S:= s +  LineEnding + '(Warning: Cells - expected<5:' + IntToStr(FClt5) + ')';
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

