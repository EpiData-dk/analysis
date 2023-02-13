unit fbarchartsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TACustomSource, executor,
  epidatafiles, epifields_helper, tables_types, tables;

type

  { TFBarSource }

  TFBarSource = class(TUserDefinedChartSource)
  private
    FPct:    Boolean;
    FTable: TTwoWayTable;
    FValueLabelOutput: TEpiGetValueLabelType;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  public
    property Pct: Boolean read FPct write FPct;
    constructor Create(AOwner: TComponent); override;
    procedure SetSource(T: TTwoWayTable; V: TEpiGetValueLabelType);
    destructor Destroy; override;
  end;

implementation

{ TFBarSource }

procedure TFBarSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  i: Integer;
begin
  AItem.X := AIndex.ToDouble;
  if (FPct) then
    for i := 0 to YCount - 1 do
      AItem.SetY(i, 100 * FTable.Cell[AIndex, i].ColPct)
  else
    for i := 0 to YCount - 1 do
      AItem.SetY(i, FTable.Cell[AIndex, i].N.ToDouble);
end;

procedure TFBarSource.SetSource(T: TTwoWayTable; V: TEpiGetValueLabelType);
begin
  FValueLabelOutput := V;
  FTable  := T;
  YCount := T.RowCount;
  PointsNumber := T.ColCount;
end;

constructor TFBarSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TFBarSource.Destroy;
begin
  FreeAndNil(FTable);
  inherited Destroy;
end;

end.
