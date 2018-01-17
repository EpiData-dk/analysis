unit options_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, ast, epifields_helper, options_hashmap, ast_types, Graphics;


procedure AddValueLabelOptions(STOptionList: TStatementOptionsMap);
function ValueLabelTypeFromOptionList(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TEpiGetValueLabelType;

procedure AddVariableLabelOptions(STOptionList: TStatementOptionsMap);
function VariableLabelTypeFromOptionList(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TEpiGetVariableLabelType;

procedure AddReadOptions(STOptionList: TStatementOptionsMap);

function FontFromSetOptions(Const FontName, SizeName, ColorName, StyleName: String; SetOptions: TSetOptionsMap): TFont;
procedure FontToSetOptions(AFont: TFont; Const FontName, SizeName, ColorName, StyleName: String;
  SetOptions: TSetOptionsMap);


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

function ValueLabelTypeFromOptionList(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TEpiGetValueLabelType;
begin
  Result := gvtLabel;

  if (Assigned(SetOptions)) then
    case UTF8LowerString(SetOptions.GetValue(ANA_SO_FORMAT_VALUE_LABEL).Value) of
      'v':  Result := gvtValue;
      'l':  Result := gvtLabel;
      'vl': Result := gvtValueLabel;
      'lv': Result := gvtLabelValue;
    end;

  if OptionList.HasOption('l') then
    Result := gvtLabel;
  if OptionList.HasOption('v') then
    Result := gvtValue;
  if OptionList.HasOption('vl') then
    Result := gvtValueLabel;
  if OptionList.HasOption('lv') then
    Result := gvtLabelValue;
end;

procedure AddVariableLabelOptions(STOptionList: TStatementOptionsMap);
begin
  STOptionList.Insert('vn',  [rtUndefined]);
  STOptionList.Insert('vnl', [rtUndefined]);
  STOptionList.Insert('vla', [rtUndefined]);
  STOptionList.Insert('vln', [rtUndefined]);
end;

function VariableLabelTypeFromOptionList(OptionList: TOptionList; SetOptions: TSetOptionsMap = nil): TEpiGetVariableLabelType;
begin
  Result := gvtVarName;

  if (Assigned(SetOptions)) then
    case UTF8LowerString(SetOptions.GetValue(ANA_SO_FORMAT_VARIABLE_LABEL).Value) of
      'vn':  Result := gvtVarName;
      'vnl': Result := gvtVarNameLabel;
      'vla': Result := gvtVarLabel;
      'vln': Result := gvtVarLabelName;
    end;

  if OptionList.HasOption('vn') then  Result := gvtVarName;
  if OptionList.HasOption('vnl') then Result := gvtVarNameLabel;
  if OptionList.HasOption('vla') then Result := gvtVarLabel;
  if OptionList.HasOption('vln') then Result := gvtVarLabelName;
end;

procedure AddReadOptions(STOptionList: TStatementOptionsMap);
begin
  STOptionList.Insert('fn',       [rtString, rtUndefined]);
  STOptionList.Insert('d',        [rtString]);
  STOptionList.Insert('q',        [rtString]);
  STOptionList.Insert('h',        [rtBoolean]);
  STOptionList.Insert('pw',       [rtString]);
  STOptionList.Insert('login',    [rtString]);
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

