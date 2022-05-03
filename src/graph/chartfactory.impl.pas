unit chartfactory.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartfactory, TAGraph, charttitles, chartcommandresult,
  chartconfiguration;

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
    function NewGraphCommandResult(): IChartCommandResult;
    function NewChartConfiguration(): IChartConfiguration;
    function NewChartTitleConfiguration(): IChartTitleConfiguration;
  end;

implementation

uses
  chartcommandresult.impl, chartconfiguration.impl, charttitles.impl, Graphics;

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
  Result.BackColor := clWhite;

  Result.AddHandlerOnBeforeDestruction(@BeforeChartDestruction);
  FChartList.Add(Result);
end;

function TChartFactory.NewGraphCommandResult(): IChartCommandResult;
begin
  result := TChartCommandResult.Create();
end;

function TChartFactory.NewChartConfiguration(): IChartConfiguration;
begin
  Result := TChartConfiguration.Create;
end;

function TChartFactory.NewChartTitleConfiguration(): IChartTitleConfiguration;
begin
  result := TChartTitlesConfiguration.Create;
end;

end.

