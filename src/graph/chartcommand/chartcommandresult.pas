unit chartcommandresult;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, TAGraph, chartconfiguration, pair;

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

  { IChartCommandResult }

  IChartCommandResult = interface['{7FC61DE9-532D-45BC-A182-321B190EEB8A}']
    procedure AddChart(Pair: TChartPair);
    procedure AddChart(AChart: TChart; Configuration: IChartConfiguration = nil);
    function GetChartPairs(): TChartPairList;
  end;

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
begin
  Chart := nil;
  Configuration := nil;
  inherited Destroy;
end;

end.

