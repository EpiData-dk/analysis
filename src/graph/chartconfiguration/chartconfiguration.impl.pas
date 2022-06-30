unit chartconfiguration.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartconfiguration, charttitles, chartaxesconfiguration;

type

  { TChartConfiguration }

  TChartConfiguration = class(TInterfacedObject, IChartConfiguration)
  private
    FTitleConfiguration: IChartTitleConfiguration;
    FAxesConfiguration: IChartAxesConfiguration;
  public
    constructor Create;
    destructor Destroy; override;
    function GetAxesConfiguration(): IChartAxesConfiguration;
    function GetTitleConfiguration(): IChartTitleConfiguration;
  end;

implementation

uses
  chartaxesconfiguration.impl, charttitles.impl;

{ TChartConfiguration }

constructor TChartConfiguration.Create;
begin
  FAxesConfiguration := TChartAxesConfiguration.Create;
  FTitleConfiguration := TChartTitlesConfiguration.Create;
end;

destructor TChartConfiguration.Destroy;
begin
  FTitleConfiguration := nil;
  FAxesConfiguration := nil;
  inherited destroy;
end;

function TChartConfiguration.GetAxesConfiguration(): IChartAxesConfiguration;
begin
  Result := FAxesConfiguration;
end;

function TChartConfiguration.GetTitleConfiguration(): IChartTitleConfiguration;
begin
  Result := FTitleConfiguration;
end;

end.
