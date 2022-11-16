unit graph_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, ast, options_hashmap, ast_types, Graphics;

type

  TColorMap = Array of TColor;
  TDigitIndex = Array of Integer;

  function ChartColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TColorMap;


implementation

uses
  LazUTF8, ana_globals, typinfo, math;

function ChartColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TColorMap;
var
  i,
  l,
  ix:   Integer;
  d:    Char;
  cMap: String;
  Opt:  TOption;
  anaColors: array of TColor = (clBlack, clBlue, clRed, clGreen, clYellow, clWhite, clSkyBlue, clFuchsia, clGray, clAqua);
begin
  result := copy(anaColors);
  if (Assigned(SetOptions)) then
    cMap := SetOptions.GetValue(ANA_SO_CHART_COLORS).Value;
  if (OptionList.HasOption('colors', Opt)) then
    cMap := Opt.Expr.AsString;
  l := min(length(anaColors), length(cMap));
  for i := 0 to l-1 do
    begin
    d := cMap.Chars[i];
    ix := ord(d) - ord('0');
    if (ix < 0) or (ix > 9) then
// when this function becomes a method of TChartColors [= class(TSetOption)]
//      DoError('Invalid color option ' + d + ' in "' + cMap + '"')
      result[i] := anaColors[0]
    else
      result[i] := anaColors[ix];
    end;
end;

end.
