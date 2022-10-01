unit options_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, ast, epifields_helper, options_hashmap, ast_types, Graphics;


type
  TSetOptionVariant = (
    sovStatistics,   // Set options accociated with ANA_SO_FORMAT_VALUE_LABEL and ANA_SO_FORMAT_VARIABLE_LABEL
    sovBrowser       //      -- do --               ANA_SO_BROWSE_VALUE_LABEL and ANA_SO_BROWSE_VARIABLE_LABEL
  );

  TColorMap = Array of TColor;
  TDigitIndex = Array of Integer;

procedure AddValueLabelOptions(STOptionList: TStatementOptionsMap);
function ValueLabelTypeFromOptionList(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil; SetOptionVariant: TSetOptionVariant = sovStatistics): TEpiGetValueLabelType;

procedure AddVariableLabelOptions(STOptionList: TStatementOptionsMap);
function VariableLabelTypeFromOptionList(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil; SetOptionVariant: TSetOptionVariant = sovStatistics): TEpiGetVariableLabelType;

procedure AddDecimalOptions(STOptionList: TStatementOptionsMap);
function DecimalFromOption(OptionList: TOptionList; DefaultValue: Integer = 1): Integer;

procedure AddReadOptions(STOptionList: TStatementOptionsMap);

procedure AddSortingOptions(STOptionList: TStatementOptionsMap);

function FontFromSetOptions(Const FontName, SizeName, ColorName, StyleName: String; SetOptions: TSetOptionsMap): TFont;
procedure FontToSetOptions(AFont: TFont; Const FontName, SizeName, ColorName, StyleName: String;
  SetOptions: TSetOptionsMap);

function ChartColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TColorMap;
function IntToDigits(n: Integer): TDigitIndex;

function BGRToRGB(Color: TColor): Integer;
function RGBToBGR(Color: Integer): TColor;

implementation

uses
  LazUTF8, ana_globals, typinfo, main;

procedure AddValueLabelOptions(STOptionList: TStatementOptionsMap);
begin
  STOptionList.Insert('v',  [rtUndefined]);
  STOptionList.Insert('l',  [rtUndefined]);
  STOptionList.Insert('vl', [rtUndefined]);
  STOptionList.Insert('lv', [rtUndefined]);
end;

function ValueLabelTypeFromOptionList(OptionList: TOptionList;
  SetOptions: TSetOptionsMap; SetOptionVariant: TSetOptionVariant
  ): TEpiGetValueLabelType;
var
  S: String;
begin
  S := '';

  if (Assigned(SetOptions)) then
    case SetOptionVariant of
      sovStatistics:
        S := UTF8LowerString(SetOptions.GetValue(ANA_SO_STATISTICS_VALUE_LABEL).Value);

      sovBrowser:
        S := UTF8LowerString(SetOptions.GetValue(ANA_SO_BROWSE_VALUE_LABEL).Value)
    end;

  case S of
    'v':  Result := gvtValue;
    'l':  Result := gvtLabel;
    'vl': Result := gvtValueLabel;
    'lv': Result := gvtLabelValue;
  else
    Result := gvtLabel;
  end;

  if OptionList.HasOption('l') then  Result := gvtLabel;
  if OptionList.HasOption('v') then  Result := gvtValue;
  if OptionList.HasOption('vl') then Result := gvtValueLabel;
  if OptionList.HasOption('lv') then Result := gvtLabelValue;
end;

procedure AddVariableLabelOptions(STOptionList: TStatementOptionsMap);
begin
  STOptionList.Insert('vn',  [rtUndefined]);
  STOptionList.Insert('vnl', [rtUndefined]);
  STOptionList.Insert('vla', [rtUndefined]);
  STOptionList.Insert('vln', [rtUndefined]);
end;

function VariableLabelTypeFromOptionList(OptionList: TOptionList;
  SetOptions: TSetOptionsMap; SetOptionVariant: TSetOptionVariant
  ): TEpiGetVariableLabelType;
var
  S: String;
begin
  S := '';

  if (Assigned(SetOptions)) then
    case SetOptionVariant of
      sovStatistics:
        S := UTF8LowerString(SetOptions.GetValue(ANA_SO_STATISTICS_VARIABLE_LABEL).Value);

      sovBrowser:
        S := UTF8LowerString(SetOptions.GetValue(ANA_SO_BROWSE_VARIABLE_LABEL).Value)
    end;

  case S of
      'vn':  Result := gvtVarName;
      'vnl': Result := gvtVarNameLabel;
      'vla': Result := gvtVarLabel;
      'vln': Result := gvtVarLabelName;
  else
    Result := gvtVarName;
  end;

  if OptionList.HasOption('vn') then  Result := gvtVarName;
  if OptionList.HasOption('vnl') then Result := gvtVarNameLabel;
  if OptionList.HasOption('vla') then Result := gvtVarLabel;
  if OptionList.HasOption('vln') then Result := gvtVarLabelName;
end;

procedure AddDecimalOptions(STOptionList: TStatementOptionsMap);
begin
  STOptionList.Insert('d0', [rtUndefined]);
  STOptionList.Insert('d1', [rtUndefined]);
  STOptionList.Insert('d2', [rtUndefined]);
  STOptionList.Insert('d3', [rtUndefined]);
  STOptionList.Insert('d4', [rtUndefined]);
  STOptionList.Insert('d5', [rtUndefined]);
end;

function DecimalFromOption(OptionList: TOptionList; DefaultValue: Integer
  ): Integer;
begin
  Result := DefaultValue;
  if OptionList.HasOption('d0') then Result := 0;
  if OptionList.HasOption('d1') then Result := 1;
  if OptionList.HasOption('d2') then Result := 2;
  if OptionList.HasOption('d3') then Result := 3;
  if OptionList.HasOption('d4') then Result := 4;
  if OptionList.HasOption('d5') then Result := 5;
end;

procedure AddReadOptions(STOptionList: TStatementOptionsMap);
begin
  STOptionList.Insert('d',        [rtString]);
  STOptionList.Insert('q',        [rtString]);
  STOptionList.Insert('fn',       [rtString, rtUndefined]);
  STOptionList.Insert('pw',       [rtString]);
  STOptionList.Insert('login',    [rtString]);
  STOptionList.Insert('force',    [rtUndefined]);
end;

procedure AddSortingOptions(STOptionList: TStatementOptionsMap);
begin
  STOptionList.Insert('sa',  [rtUndefined]);
  STOptionList.Insert('sd',  [rtUndefined]);
  STOptionList.Insert('sla', [rtUndefined]);
  STOptionList.Insert('sld', [rtUndefined]);

  STOptionList.Insert('sra', [rtInteger]);
  STOptionList.Insert('srd', [rtInteger]);
  STOptionList.Insert('sca', [rtInteger]);
  STOptionList.Insert('scd', [rtInteger]);

  STOptionList.Insert('srta', [rtUndefined]);
  STOptionList.Insert('srtd', [rtUndefined]);
  STOptionList.Insert('scta', [rtUndefined]);
  STOptionList.Insert('sctd', [rtUndefined]);
end;

function FontFromSetOptions(const FontName, SizeName, ColorName,
  StyleName: String; SetOptions: TSetOptionsMap): TFont;
var
  Val: LongInt;
  R, G, B: Byte;
begin
  Result := TFont.Create;
  if FontName <> ''  then Result.Name := SetOptions[FontName].Value;
  if SizeName <> ''  then Result.Size := StrToInt(SetOptions[SizeName].Value);

  Val := StrToInt('$' + Copy(SetOptions[ColorName].Value, 2, 6));  // RGB
  if ColorName <> '' then Result.Color := RGBToBGR(Val);
  if StyleName <> '' then Result.Style := TFontStyles(StringToSet(PTypeInfo(TypeInfo(TFontStyles)), SetOptions[StyleName].Value));
end;

procedure FontToSetOptions(AFont: TFont; const FontName, SizeName, ColorName,
  StyleName: String; SetOptions: TSetOptionsMap);

  procedure ChangeSetOption(Option: TSetOption; Const Name, Value: String; ASType: TASTResultType);
  var
    S: String;
  begin
    if (Option.Value <> Value) then
      begin
        S := 'set "' + Name + '" := ';
        if ASType = rtString then
          S := S + '"' + Value + '";'
        else
          S := S + Value + ';';

        MainForm.InterfaceRunCommand(S);
      end;
  end;

var
  Res: Boolean;
begin
  if FontName <> '' then
    ChangeSetOption(SetOptions[FontName],  FontName,  AFont.Name,               rtString);

  if SizeName <> '' then
    ChangeSetOption(SetOptions[SizeName],  SizeName,  IntToStr(AFont.Size),     rtInteger);

  if ColorName <> '' then
    ChangeSetOption(SetOptions[ColorName], ColorName, '#' + IntToHex(BGRToRGB(AFont.Color), 6), rtString);

  if StyleName <> '' then
    ChangeSetOption(SetOptions[StyleName], StyleName, SetToString(PTypeInfo(TypeInfo(TFontStyles)), Integer(AFont.Style), false), rtString);
end;

function ChartColorsFromOptions(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TColorMap;
var
  i:    Integer;
  iMap: Integer;
  vMap: TDigitIndex;
  Opt:  TOption;
  anaColors: array of TColor = (clBlue, clRed, clBlack, clGreen, clYellow, clWhite, clSkyBlue, clFuchsia, clGray, clAqua);
begin
  result := copy(anaColors);
  if (Assigned(SetOptions)) then
    begin
      iMap := StrToInt(SetOptions.GetValue(ANA_SO_CHART_COLORS).Value);
    end;
  if (OptionList.HasOption('colors', Opt)) then
    iMap := Opt.Expr.AsInteger;
  vMap := IntToDigits(iMap);
  for i := 0 to high(vMap) do
    result[i] := anaColors[vMap[i]-1];
end;

{ IntToDigits parses integer n into array of it's non-zero digits
  Use it to convert a set option like 12345 into a color or pattern array
}
function IntToDigits(n: Integer): TDigitIndex;
var
  i: Integer;
  s, d: String;
begin
  if (n < 0) then n := -n;
  if (n > 999999999) then
    n := 123456789; // fallback to legal value
  s := IntToStr(n);
  result := [];
  for i := 0 to Length(s)-1 do
    begin
      d := s.Substring(i,1);
      if (d <> '0') then
        result := concat(result, [d.ToInteger]);
    end;
end;

function BGRToRGB(Color: TColor): Integer;
begin
  result := ((Color and $FF0000) shr 16) or
            ((Color and $00FF00))        or
            ((Color and $0000FF) shl 16);
end;

function RGBToBGR(Color: Integer): TColor;
begin
  result := ((Color and $FF0000) shr 16) or
            ((Color and $00FF00))        or
            ((Color and $0000FF) shl 16);
end;

end.

