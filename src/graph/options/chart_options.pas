unit chart_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, ast, options_hashmap, ast_types, Graphics;

const
  anaColors: array of TColor =
    (clBlack, clBlue, clRed, clGreen, clYellow, clWhite, clSkyBlue, clFuchsia, clGray, clAqua);

type

  TColorMap = array of TColor;

  TChartColors = class(TSetOption)
private
  FColorMap:   TColorMap;
protected
  procedure SetColors(AValue: String);
  procedure SetColors(AHexColors: Array of String);
public
  constructor Create(AValue: UTF8String; AAstType: TASTResultType);
end;

  function GetColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TColorMap;

implementation

uses
  LazUTF8, ana_globals, typinfo, math, strutils;

function TChartColors.GetColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TColorMap;
var
  Opt:  TOption;
begin
  if (Assigned(SetOptions)) then
    SetColors(SetOptions.GetValue(ANA_SO_CHART_COLORS).Value);
  if (OptionList.HasOption('c', Opt)) then
    SetColors(Opt.Expr.AsString);
  result := FColorMap;
end;

procedure TChartColors.SetColors(AValue: String);
var
  i,
  l,
  ix:   Integer;
  d:    Char;
  aColorMap: TColorMap;
  cColors: Array of String;
  Opt:  TOption;
begin
  if (AValue[1] = '#') then
    SetColors(SplitString(copy(AValue,1,length(AValue) - 1), '#'))
  else
    begin
    aColorMap := copy(FColorMap);
      l := min(length(anaColors), length(AValue));
      for i := 0 to l-1 do
        begin
        d := AValue[i];
        ix := ord(d) - ord('0');
        if (ix < 0) or (ix > 9) then
          DoError('Invalid color option ' + d + ' in "' + AValue + '"')
        else
          FColorMap[i] := aColorMap[ix];
        end;
      end;
end;

procedure TChartColors.SetColors(AHexColors: Array of String);
var
  i: Integer;
  Dummy: longint;
  s: String;
begin
  for i := 0 to high(AHexColors) do
    begin
      s := AHexColors[i];
      if (Length(s) <> 6) then
        DoError('"' + s + '" is not a valid color (hexadecimal)! e.g. #000000 = black');
      s := '$' + s;
      if (not TryStrToInt(s, Dummy)) then
        DoError('"#' + s + '" is not a valid color (hexadecimal)! e.g. #000000 = black');
      FColorMap[i] := StrToInt(s); //TColor(s);
    end;
end;

constructor TChartColors.Create(AValue: UTF8String; AAstType: TASTResultType);
begin
  FColorMap := copy(anaColors);
  inherited Create(AValue, AAstType);
end;
end.
