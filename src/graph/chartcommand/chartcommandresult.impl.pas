unit chartcommandresult.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, gmap, chartconfiguration;

type

  { TChartCommandResult }

  TChartCommandResult = class(TInterfacedObject, IChartCommandResult)
  private
    FChartList: TChartPairList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChart(Pair: TChartPair);
    procedure AddChart(AChart: TChart; Configuration: IChartConfiguration = nil);
    function GetChartPairs(): TChartPairList;
  end;

implementation

{ TChartCommandResult }

constructor TChartCommandResult.Create;
begin
  FChartList := TChartPairList.Create(true);
end;

destructor TChartCommandResult.Destroy;
begin
  FChartList.Free;
  inherited Destroy;
end;

procedure TChartCommandResult.AddChart(Pair: TChartPair);
begin
  FChartList.Add(Pair);
end;

procedure TChartCommandResult.AddChart(AChart: TChart;
  Configuration: IChartConfiguration);
begin
  FChartList.Add(TChartPair.Create(AChart, Configuration));
end;

function TChartCommandResult.GetChartPairs(): TChartPairList;
begin
  result := FChartList;
end;

end.

