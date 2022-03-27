unit charttitles.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, charttitles, ast;

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
    function OverrideFromOptions(Options: TOptionList): IChartTitleConfiguration;
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

function TChartTitlesConfiguration.OverrideFromOptions(Options: TOptionList
  ): IChartTitleConfiguration;
var
  Opt: TOption;
begin
  if (Options.HasOption('title', Opt) or Options.HasOption('t', Opt)) then
    FTitle := Opt.Expr.AsString;

  if (Options.HasOption('footer', Opt) or Options.HasOption('f', Opt)) then
    FFootnote := Opt.Expr.AsString;

  if (Options.HasOption('xtitle', Opt) or Options.HasOption('xt', Opt)) then
    FXAxisTitle := Opt.Expr.AsString;

  if (Options.HasOption('ytitle', Opt) or Options.HasOption('yt', Opt)) then
    FYAxisTitle := Opt.Expr.AsString;

  Result := Self;
end;

end.

