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

  function ToColors(AValue: String; out Msg: UTF8String): TColorMap;
  function ToColors(AHexValues: Array of String; out Msg: UTF8String): TColorMap;
  function ChartColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap;
    out Msg:UTF8String): TColorMap;

implementation

uses
  LazUTF8, ana_globals, typinfo, math, strutils, options_utils;

function ChartColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap;
  out Msg:UTF8String): TColorMap;
var
  Opt:  TOption;
  aMsg: UTF8String;
  aColorOption: String;
begin
  result := anaColors;
  if (Assigned(SetOptions)) then
    aColorOption := SetOptions.GetValue(ANA_SO_CHART_COLORS).Value;
  if (OptionList.HasOption('colors', Opt)) then
    aColorOption := Opt.Expr.AsString;
  result := ToColors(aColorOption, Msg);
end;

function ToColors(AValue: String; out Msg: UTF8String): TColorMap;
var
  i,
  l,
  ix:   Integer;
  d:    Char;
begin
  Msg := '';
  if (AValue[1] = '#') then
    result := ToColors(SplitString(AValue, '#'), Msg)
  else
    begin
      l := min(length(anaColors), length(AValue));
      SetLength(result, l);
      for i := 0 to l - 1 do
        begin
        d := aValue.Chars[i];
        ix := ord(d) - ord('0');
        if (ix < 0) or (ix > 9) then
          Msg += 'Invalid color option ' + d + ' in "' + AValue + '". '
        else
          result[i] := anaColors[ix];
        end;
      end;
end;

function ToColors(AHexValues: Array of String; out Msg: UTF8String): TColorMap;
var
  i: Integer;
  Dummy: longint;
  s: String;
begin
  Msg := '';
  SetLength(result, length(AHexValues) - 1);
  for i := 1 to high(AHexValues) do
    begin
      s := AHexValues[i].ToUpper;
      if (Length(s) <> 6) then
        Msg += '"#' + s + '" is not a valid color (hexadecimal)! e.g. #000000 = black '
      else if (not TryStrToInt('$'+s, Dummy)) then
        Msg += '"#' + s + '" is not a valid color (hexadecimal)! e.g. #000000 = black';
      result[i-1] := RGBToBGR(StrToInt('$'+s));
    end;
end;

end.
