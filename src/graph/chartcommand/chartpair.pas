unit chartpair;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pair, TAGraph, chartconfiguration, fgl;

type
  { TChartPair }

  TChartPair = class(specialize TPair<TChart, IChartConfiguration>)
  private
    function GetChart: TChart;
    function GetConfiguration: IChartConfiguration;
    procedure SetChart(AValue: TChart);
    procedure SetConfiguration(AValue: IChartConfiguration);
  public
    destructor Destroy; override;
    property Chart: TChart read GetChart write SetChart;
    property Configuration: IChartConfiguration read GetConfiguration write SetConfiguration;
  end;
  TChartPairList = specialize TFPGObjectList<TChartPair>;


implementation

{ TChartPair }

function TChartPair.GetChart: TChart;
begin
  Result := First;
end;

function TChartPair.GetConfiguration: IChartConfiguration;
begin
  Result := Second;
end;

procedure TChartPair.SetChart(AValue: TChart);
begin
  First := AValue;
end;

procedure TChartPair.SetConfiguration(AValue: IChartConfiguration);
begin
  Second := AValue;
end;

destructor TChartPair.Destroy;
var
  i: integer;
begin
  // must clear any axis transformations before destroying chart
  for i := 0 to Chart.AxisList.Count - 1 do
    if (Assigned(Chart.AxisList.Axes[i].Transformations)) then
      Chart.AxisList.Axes[i].Transformations := nil;
  Chart := nil;
  Configuration := nil;
  inherited Destroy;
end;

end.
