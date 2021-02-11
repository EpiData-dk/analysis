unit options_fontoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, options_hashmap, Graphics;

type

  { TFontColorOption }

  TFontColorOption = class(TSetOption)
  protected
    procedure SetValue(AValue: UTF8String); override;
  public
    function  GetRGBValue: Integer;
    function  GetBGRValue: TColor;
  end;

  { TFontStyleOption }

  TFontStyleOption = class(TSetOption)
  protected
    procedure SetValue(AValue: UTF8String); override;
  public
    function GetAsStyle: TFontStyles;
  end;

implementation

uses
  typinfo, options_utils;

{ TFontColorOption }

procedure TFontColorOption.SetValue(AValue: UTF8String);
var
  Dummy: Longint;
  S: String;
begin
  if (AValue[1] <> '#') then
    DoError('"' + AValue + '" is not a valid color (hexadecimal)! Eg. #000000 = black');

  if (Length(AValue) <> 7) then
    DoError('"' + AValue + '" is not a valid color (hexadecimal)! Eg. #000000 = black');

  S := '$' + Copy(AValue, 2, 6);
  if (not TryStrToInt(S, Dummy)) then
    DoError('"' + AValue + '" is not a valid color (hexadecimal)! Eg. #000000 = black');

  inherited SetValue(AValue);
end;

function TFontColorOption.GetRGBValue: Integer;
begin
  result := StrToInt('$' + Copy(Value, 2, 6));
end;

function TFontColorOption.GetBGRValue: TColor;
begin
  result := RGBToBGR(GetRGBValue);
end;

{ TFontStyleOption }

procedure TFontStyleOption.SetValue(AValue: UTF8String);
var
  Val: Integer;
begin
  Val := Integer(GetAsStyle);
  try
    Val := StringToSet(PTypeInfo(TypeInfo(TFontStyles)), AValue);
  except
    DoError('"' + AValue + '" is not a valid font style! Eg. "fsBold,fsUnderline"');
  end;

  inherited SetValue(AValue);
end;

function TFontStyleOption.GetAsStyle: TFontStyles;
begin
  result := TFontStyles(StringToSet(PTypeInfo(TypeInfo(TFontStyles)), Value));
end;

end.

