unit chartfactory.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartfactory, TAGraph, charttitles, graphcommandresult;

type

  { TChartFactory }

  TChartFactory = class(TObject, IChartFactory)
  private
    FChartList: TList;
    procedure BeforeChartDestruction(Sender: TObject);
  public
    constructor Create();
    destructor Destroy; override;
    function NewChart(): TChart;
    function NewGraphCommandResult(): IGraphCommandResult;
    function NewChartTitleConfiguration(): IChartTitleConfiguration;
  end;

implementation

uses
  graphcommandresult.impl, charttitles.impl;

{ TChartFactory }

procedure TChartFactory.BeforeChartDestruction(Sender: TObject);
begin
  FChartList.Remove(TChart(Sender));
end;

constructor TChartFactory.Create();
begin
  FChartList := TList.Create;
end;

destructor TChartFactory.Destroy;
var
  ListEnumerator: TListEnumerator;
begin
  ListEnumerator := FChartList.GetEnumerator;

  while (ListEnumerator.MoveNext) do
    TChart(ListEnumerator.Current).RemoveHandlerOnBeforeDestruction(@BeforeChartDestruction);
  FChartList.Clear;
  FChartList.Free;

  inherited Destroy;
end;

function TChartFactory.NewChart(): TChart;
begin
  Result := TChart.Create(nil);
  Result.AddHandlerOnBeforeDestruction(@BeforeChartDestruction);
  FChartList.Add(Result);
end;

function TChartFactory.NewGraphCommandResult(): IGraphCommandResult;
begin
  result := TGraphCommandResult.Create();
end;

function TChartFactory.NewChartTitleConfiguration(): IChartTitleConfiguration;
begin
  result := TChartTitlesConfiguration.Create;
end;

end.

