unit paretosource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TACustomSource, executor,
  epidatafiles, epifields_helper, tables_types, tables;

type

  { TParetoBarSource }

  TParetoBarSource = class(TUserDefinedChartSource)
  private
    FTable: TTwoWayTable;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetSource(T: TTwoWayTable);
    destructor Destroy; override;
  end;

  { TParetoLineSource }

  TParetoLineSource = class(TUserDefinedChartSource)
  private
    FCumPct: array of Double;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetSource(T: TTwoWayTable);
    destructor Destroy; override;
  end;

implementation

{ TParetoBarSource }

procedure TParetoBarSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := AIndex.ToDouble;
  AItem.Y := FTable.Cell[AIndex, 0].N.ToDouble;
end;

procedure TParetoBarSource.SetSource(T: TTwoWayTable);
begin
  FTable  := T;
  YCount := 1;
  PointsNumber := T.ColCount;
end;

constructor TParetoBarSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TParetoBarSource.Destroy;
begin
  FreeAndNil(FTable);
  inherited Destroy;
end;

{ TParetoLineSource }

procedure TParetoLineSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := AIndex.ToDouble;
  AItem.Y := FCumPct[AIndex];
end;

procedure TParetoLineSource.SetSource(T: TTwoWayTable);
var
  CumCount: Integer;
  Total: Double;
  i: Integer;
begin
  YCount := 1;
  PointsNumber := T.ColCount;
  Total := T.Total.ToDouble;
  CumCount := 0;
  SetLength(FCumPct, PointsNumber);
  for i := 0 to High(FCumPct) do
    begin
      CumCount += T.ColTotal[i];
      FCumPct[i] := 100 * CumCount.ToDouble / Total;
    end;
end;

constructor TParetoLineSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TParetoLineSource.Destroy;
begin
  FCumPct := nil;
  inherited Destroy;
end;

end.
