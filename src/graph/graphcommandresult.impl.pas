unit graphcommandresult.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphcommandresult, TAGraph, gmap, charttitles;

type

  { TChartTitlesCompare }

  TChartCompare = class
    class function C(A, B: TChart): boolean;
  end;

  TChartTitlesMap = specialize TMap<TChart, IChartTitles, TChartCompare>;

  { TGraphCommandResult }

  TGraphCommandResult = class(TObject, IGraphCommandResult)
  private
    FChartList: TChartList;
    FChartTitleMap: TChartTitlesMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChart(AChart: TChart);
    function GetCharts(): TChartList;
    procedure SetChartTitles(AChart: TChart; Titles: IChartTitles);
    function GetChartTitles(AChart: TChart): IChartTitles;
  end;

implementation

{ TChartTitlesCompare }

class function TChartCompare.C(A, B: TChart): boolean;
begin
  Result := Pointer(A) < Pointer(B);
end;

{ TGraphCommandResult }

constructor TGraphCommandResult.Create;
begin
  FChartList := TChartList.Create;
  FChartTitleMap := TChartTitlesMap.Create;
end;

destructor TGraphCommandResult.Destroy;
begin
  FChartList.Free;
  FChartTitleMap.Free;
  inherited Destroy;
end;

procedure TGraphCommandResult.AddChart(AChart: TChart);
begin
  FChartList.Add(AChart);
end;

function TGraphCommandResult.GetCharts(): TChartList;
begin
  result := FChartList;
end;

procedure TGraphCommandResult.SetChartTitles(AChart: TChart;
  Titles: IChartTitles);
begin
  if (FChartList.IndexOf(AChart) < 0) then
    raise Exception.Create('Chart not found! Has it been added first?');

  FChartTitleMap.Insert(AChart, Titles);
end;

function TGraphCommandResult.GetChartTitles(AChart: TChart): IChartTitles;
begin
  Result := FChartTitleMap.GetValue(AChart);
end;

end.

