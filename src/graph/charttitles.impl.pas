unit charttitles.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, charttitles;

type

  { TChartTitlesConfiguration }

  TChartTitlesConfiguration = class(TObject, IChartTitleConfiguration)
  private
    FTitle: UTF8String;
    FFootnote: UTF8String;
    FXAxisTitle: UTF8String;
    FYAxisTitle: UTF8String;
  public
    function GetTitle(): UTF8String;
    function GetFootnote(): UTF8String;
    function GetXAxisTitle(): UTF8String;
    function GetYAxisTitle(): UTF8String;
    function SetTitle(Text: UTF8String): IChartTitleConfiguration;
    function SetFootnote(Text: UTF8String): IChartTitleConfiguration;
    function SetXAxisTitle(Text: UTF8String): IChartTitleConfiguration;
    function SetYAxisTitle(Text: UTF8String): IChartTitleConfiguration;
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

function TChartTitlesConfiguration.SetTitle(Text: UTF8String): IChartTitleConfiguration;
begin
  FTitle := Text;
  Result := self;
end;

function TChartTitlesConfiguration.SetFootnote(Text: UTF8String): IChartTitleConfiguration;
begin
  FFootnote := Text;
  Result := self;
end;

function TChartTitlesConfiguration.SetXAxisTitle(Text: UTF8String): IChartTitleConfiguration;
begin
  FXAxisTitle := Text;
  Result := self;
end;

function TChartTitlesConfiguration.SetYAxisTitle(Text: UTF8String): IChartTitleConfiguration;
begin
  FYAxisTitle := Text;
  Result := self;
end;

end.

