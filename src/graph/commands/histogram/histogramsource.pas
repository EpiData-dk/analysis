unit histogramsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, TASources, TACustomSource, histogramdata;

type

  { THistogramSource }

  THistogramSource = class(TUserDefinedChartSource)
  private
    FHistogram: THistogram;
    FBarMultiplier: Double;
    procedure setHistogram(AValue: THistogram);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Histogram: THistogram read FHistogram write setHistogram;
    procedure GetDataItem(ASource: TUserDefinedChartSource;
              AIndex: Integer; var AItem: TChartDataItem);
    function AddXAxisScales(Chart: TChart): TListChartSource;
    function AddYAxisScales(Chart: TChart): TListChartSource;
  end;

implementation

{ THistogramSource }

procedure THistogramSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  i: Integer;
  s: array of Integer;
begin
  AItem.X := FHistogram.XValue[AIndex].ToDouble;
  s := FHistogram.SlotCounts[Aindex];
  for i := 0 to high(s) do
    AItem.SetY(i, s[i].ToDouble * FBarMultiplier);
end;

function THistogramSource.AddXAxisScales(Chart: TChart): TListChartSource;
// ensures that x-axis labels will be at intervals only
var
  i: Integer;
  tick: Double;
begin
  result := TListChartSource.Create(Chart);
  for i := 0 to FHistogram.Count - 1 do
    begin
      tick := FHistogram.XValue[i];
      result.Add(tick, tick);
    end;
end;

function THistogramSource.AddYAxisScales(Chart: TChart): TListChartSource;
// ensures that vertical labels (counts) will be integers
var
  i: Integer;
  tick: Double;
begin
  result := TListChartSource.Create(Chart);
  for i := 0 to FHistogram.MaxCount[0] do
    begin
      tick := double(i);
      result.Add(tick, tick);
    end;
end;

constructor THistogramSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
  FBarMultiplier := 1;
end;

procedure THistogramSource.setHistogram(AValue: THistogram);
begin
  FHistogram := AValue;
  PointsNumber := FHistogram.Count;
  YCount := FHistogram.Strata;
  if (FHistogram.PctCalc) then
    FBarMultiplier := 100 / FHistogram.Total;
end;

destructor THistogramSource.Destroy;
begin
  FHistogram.Destroy;
  inherited Destroy;
end;

end.
