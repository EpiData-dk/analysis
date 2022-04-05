unit chartcommandresult.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, gmap, charttitles;

type

  { TChartTitlesCompare }

  TChartCompare = class
    class function C(A, B: TChart): boolean;
  end;

  TChartTitlesMap = specialize TMap<TChart, IChartTitles, TChartCompare>;

  { TChartCommandResult }

  TChartCommandResult = class(TObject, IChartCommandResult)
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

{ TChartCommandResult }

constructor TChartCommandResult.Create;
begin
  FChartList := TChartList.Create;
  FChartTitleMap := TChartTitlesMap.Create;
end;

destructor TChartCommandResult.Destroy;
begin
  FChartList.Free;
  FChartTitleMap.Free;
  inherited Destroy;
end;

procedure TChartCommandResult.AddChart(AChart: TChart);
begin
  FChartList.Add(AChart);
end;

function TChartCommandResult.GetCharts(): TChartList;
begin
  result := FChartList;
end;

procedure TChartCommandResult.SetChartTitles(AChart: TChart;
  Titles: IChartTitles);
begin
  if (FChartList.IndexOf(AChart) < 0) then
    raise Exception.Create('Chart not found! Has it been added first?');

  FChartTitleMap.Insert(AChart, Titles);
end;

function TChartCommandResult.GetChartTitles(AChart: TChart): IChartTitles;
begin
  Result := FChartTitleMap.GetValue(AChart);
end;

end.

