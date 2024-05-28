unit charttitles.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, charttitles, ast;

type

  { TChartTitlesConfiguration }

  TChartTitlesConfiguration = class(TInterfacedObject, IChartTitleConfiguration)
  private
    FTitle: UTF8String;
    FFootnote: UTF8String;
    FXAxisTitle: UTF8String;
    FYAxisTitle: UTF8String;
    FY2AxisTitle: UTF8String;
    FStratumValue: UTF8String;
  public
    constructor Create;
    function GetTitle(): UTF8String;
    function GetFootnote(): UTF8String;
    function GetXAxisTitle(): UTF8String;
    function GetYAxisTitle(): UTF8String;
    function GetY2AxisTitle(): UTF8String;
    function GetStratumValue(): UTF8String;
    function SetTitle(Text: UTF8String): IChartTitleConfiguration;
    function SetFootnote(Text: UTF8String): IChartTitleConfiguration;
    function SetXAxisTitle(Text: UTF8String): IChartTitleConfiguration;
    function SetYAxisTitle(Text: UTF8String): IChartTitleConfiguration;
    function SetY2AxisTitle(Text: UTF8String): IChartTitleConfiguration;
    function SetStratumValue(Value: UTF8String): IChartTitleConfiguration;
  end;

implementation

{ TChartTitlesConfiguration }

function TChartTitlesConfiguration.GetTitle(): UTF8String;
begin
  Result := FTitle;
end;


function TChartTitlesConfiguration.GetFootnote(): UTF8String;
begin
  Result := FFootnote;
end;

function TChartTitlesConfiguration.GetXAxisTitle(): UTF8String;
begin
  Result := FXAxisTitle;
end;

function TChartTitlesConfiguration.GetYAxisTitle(): UTF8String;
begin
  Result := FYAxisTitle;
end;

function TChartTitlesConfiguration.GetY2AxisTitle(): UTF8String;
begin
  Result := FY2AxisTitle;
end;

function TChartTitlesConfiguration.GetStratumValue(): UTF8String;
begin
    Result := FStratumValue
end;

function TChartTitlesConfiguration.SetTitle(Text: UTF8String): IChartTitleConfiguration;
begin
  FTitle := Text;
  Result := Self;
end;

function TChartTitlesConfiguration.SetFootnote(Text: UTF8String): IChartTitleConfiguration;
begin
  FFootnote := Text;
  Result := Self;
end;

function TChartTitlesConfiguration.SetXAxisTitle(Text: UTF8String): IChartTitleConfiguration;
begin
  FXAxisTitle := Text;
  Result := Self;
end;

function TChartTitlesConfiguration.SetYAxisTitle(Text: UTF8String): IChartTitleConfiguration;
begin
  FYAxisTitle := Text;
  Result := Self;
end;

function TChartTitlesConfiguration.SetY2AxisTitle(Text: UTF8String): IChartTitleConfiguration;
begin
  FY2AxisTitle := Text;
  Result := Self;
end;

function TChartTitlesConfiguration.SetStratumValue(Value: UTF8String): IChartTitleConfiguration;
begin
  FStratumValue := Value;
  Result := Self;
end;

constructor TChartTitlesConfiguration.Create;
begin

end;

end.

