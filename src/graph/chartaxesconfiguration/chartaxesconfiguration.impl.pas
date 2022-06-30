unit chartaxesconfiguration.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartaxesconfiguration;

type

  { TChartAxisConfiguration }

  TChartAxisConfiguration = class(TInterfacedObject, IChartAxisConfiguration)
  private
    FShowAxisMarksAsDates: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetShowAxisMarksAsDates: boolean;
    function SetShowAxisMarksAsDates(Value: boolean): IChartAxisConfiguration;
  end;

  { TChartAxesConfiguration }

  TChartAxesConfiguration = class(TInterfacedObject, IChartAxesConfiguration)
  private
    FXAxisConfiguration: IChartAxisConfiguration;
    FYAxisConfiguration: IChartAxisConfiguration;
  public
    constructor Create;
    destructor Destroy; override;
    function GetXAxisConfiguration(): IChartAxisConfiguration;
    function GetYAxisConfiguration(): IChartAxisConfiguration;
  end;

implementation

{ TChartAxisConfiguration }

constructor TChartAxisConfiguration.Create;
begin
  FShowAxisMarksAsDates := false;
end;

destructor TChartAxisConfiguration.Destroy;
begin
  inherited Destroy;
end;

function TChartAxisConfiguration.GetShowAxisMarksAsDates: boolean;
begin
  Result := FShowAxisMarksAsDates;
end;

function TChartAxisConfiguration.SetShowAxisMarksAsDates(Value: boolean
  ): IChartAxisConfiguration;
begin
  FShowAxisMarksAsDates := Value;
  Result := Self;
end;

{ TChartAxesConfiguration }

constructor TChartAxesConfiguration.Create;
begin
  FXAxisConfiguration := TChartAxisConfiguration.Create;
  FYAxisConfiguration := TChartAxisConfiguration.Create;
end;

destructor TChartAxesConfiguration.Destroy;
begin
  FXAxisConfiguration := nil;
  FYAxisConfiguration := nil;
  inherited Destroy;
end;

function TChartAxesConfiguration.GetXAxisConfiguration(): IChartAxisConfiguration;
begin
  Result := FXAxisConfiguration;
end;

function TChartAxesConfiguration.GetYAxisConfiguration(): IChartAxisConfiguration;
begin
  Result := FYAxisConfiguration;
end;

end.
