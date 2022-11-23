unit chart_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, ast, options_hashmap, ast_types, Graphics;

type

TColorMap = array of TColor;

const
  anaColors: array of TColor = 
    (clBlack, clBlue, clRed, clGreen, clYellow, clWhite, clSkyBlue, clFuchsia, clGray, clAqua);

TChartColors = class(TSetOption)
private
  FColorMap:   TColorMap;
protected
  procedure SetColors(AValue: String); override;
  procedure SetColors(AHexColors: Array of String); override;
  function GetColor(AIndex: Integer): TColor;
public
  procedure SetColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil);
  property Color[AIndex: Integer]: TColor read GetColor;
  property Colors: String write SetColors;

implementation

uses
  LazUTF8, ana_globals, typinfo, math, strutils;

procedure TChartColors.SetColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil);
var
  Opt:  TOption;
begin
  FColorMap := copy(anaColors);
  if (Assigned(SetOptions)) then
    SetColors(SetOptions.GetValue(ANA_SO_CHART_COLORS).Value);
  if (OptionList.HasOption(['c','colors'], Opt)) then
    SetColors(Opt.Expr.AsString);
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
    SetColors(SplitString(copy(AValue,1,length(AValue) - 1), '#')
  else
    begin
    aColorMap := copy(FColorMap);
      l := min(length(anaColors), length(cMap));
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
  s: String;
begin
  for i := 0 to high(AHexColors) do
    begin
      s := AHexColors[i];
      if (Length(s) <> 6) then
        DoError('"' + s + '" is not a valid color (hexadecimal)! e.g. #000000 = black');
      s := '$' + s;
      if (not TryStrToInt(s, Dummy)) then
        DoError('"' + s + '" is not a valid color (hexadecimal)! e.g. #000000 = black');
      FColorMap[i] := '#' + s;
    end;
end;

function TChartColors.GetColor(AIndex: Integer): TColor;
begin
  if (AIndex < 0 or AIndex > length(FColorMap)) then
    Exception('illegal call to GetColor with index ' + AIndex.ToString);
  result := FColorMap[AIndex];
end;

end.
