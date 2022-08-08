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
    FOutputCreator: TOutputCreator;
  public
    constructor Create(AOwner: TComponent; AHistogram: THistogram; OutputCreator: TOutputCreator); //override;
    destructor Destroy; override;
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
  s: array of Double;
begin
  AItem.X := FHistogram.SlotValue[AIndex];
  s := FHistogram.Slot[Aindex];
  for i := 0 to high(s) do
    AItem.SetY(i, s[i]);
  FOutputCreator.DoInfoShort('Index: ' + AIndex.ToString + ' X: ' + FHistogram.SlotValue[AIndex].ToString + ' s: ' + s[0].ToString + ', ' + s[1].ToString);
end;

function THistogramSource.AddAxisScales(Chart: TChart): TListChartSource;
var
  i: Integer;
  tick: Double;
begin
  result := TListChartSource.Create(Chart);
  for i := FHistogram.Base to FHistogram.Base + FHistogram.Count - 1 do
    begin
      tick := i.ToDouble;
      result.Add(tick, tick);
    end;
end;

constructor THistogramSource.Create(AOwner: TComponent; AHistogram: THistogram; OutputCreator: TOutputCreator);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
  FOutputCreator := OutputCreator;
  if (AHistogram = FHistogram) then
    exit;
  FHistogram := AHistogram;
end;

destructor THistogramSource.Destroy;
begin
  inherited Destroy;
  FHistogram.Destroy;
end;

end.
