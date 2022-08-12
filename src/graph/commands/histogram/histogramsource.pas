unit histogramsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, TASources, TACustomSource, histogramdata, outputcreator;

type

  { THistogramSource }

  THistogramSource = class(TUserDefinedChartSource)
  private
    FHistogram: THistogram;
    procedure setHistogram(AValue: THistogram);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Histogram: THistogram read FHistogram write setHistogram;
    procedure GetDataItem(ASource: TUserDefinedChartSource;
              AIndex: Integer; var AItem: TChartDataItem);
    function AddAxisScales(Chart: TChart): TListChartSource;
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
    AItem.SetY(i, s[i].ToDouble);
end;

function THistogramSource.AddAxisScales(Chart: TChart): TListChartSource;
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

constructor THistogramSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

procedure THistogramSource.setHistogram(AValue: THistogram);
begin
  if (AValue = FHistogram) then exit;
  FHistogram := AValue;
  PointsNumber := FHistogram.Count;
  YCount := FHistogram.Strata;
end;

destructor THistogramSource.Destroy;
begin
  inherited Destroy;
  FHistogram.Destroy;
end;

end.
