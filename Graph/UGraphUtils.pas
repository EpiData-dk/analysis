unit UGraphUtils;

interface

uses
  Graphics, TeEngine;

type

   TGraphUtils = class(TObject)
   private
      class function GraphColour(Index: integer; OptionStr, DefaultStr: string): TColor;
   protected

   public
     constructor Create;
     destructor Destroy; override;
     class function GetGraphBrushStyle(index: integer): TBrushStyle;
     class function GetGraphColour(index: integer): TColor;
     class function GetSPCColour(index: integer): TColor;
     class function GetGraphPointerStyle(index: integer): TSeriesPointerStyle;
     class function GetGraphTextColour(index: integer): TColor;
     class function GetHisColor(index: integer): TColor;
   published 

   end;


implementation

uses
  UCmdProcessor, AnsDataTypes, SysUtils, Math;

{****************************
*  TSeriesColorFunctions    *
*****************************}

class function TGraphUtils.GetGraphColour(index: integer): TColor;
begin
  Result := GraphColour(Index + 1, 'GRAPH COLOUR', '01234567890123456789');
end;

class function TGraphUtils.GetGraphPointerStyle(index: integer): TSeriesPointerStyle;
var
  opt: TEpiOption;
const
  stylelist: array[0..9] of TSeriesPointerStyle =
  (psCircle, psTriangle, psDownTriangle, psLeftTriangle, psRightTriangle, psRectangle, psSmallDot,
  psDiagCross, psStar,psCross);

begin
  result := stylelist[0];
  if dm.GetOptionValue('GRAPH SYMBOL',opt) then
  begin
      try
        if length(opt.Value) < 20 then
           dm.SetOptionValue('GRAPH SYMBOL', opt.value + copy('01234567890123456789',1,20-length(opt.value)));
        index := StrToInt(opt.Value[(index mod 20)+1]);
        result := stylelist[index];
      except
        result := stylelist[1];
      end;
  end;
end;

class function TGraphUtils.GetGraphBrushStyle(index: integer): TBrushStyle;
var
  opt: TEpiOption;
const
  brushlist: array[0..1] of TBrushStyle =
    (bsClear, bsSolid);
begin
  result := brushlist[0];
  if dm.GetOptionValue('GRAPH SYMBOL FILLED',opt) then
  begin
      try
        if length(opt.Value) < 20 then
           dm.SetOptionValue('GRAPH SYMBOL FILLED', opt.value + copy('01010101010101010101',1,20-length(opt.value)));
        index := StrToInt(opt.Value[(index mod 20)+1]);
        result := brushlist[Min(1, index)];
      except
        result := bsSolid;
      end;
  end;
end;

class function TGraphUtils.GetGraphTextColour(index: integer): TColor;
begin
  Result := GraphColour(Index, 'GRAPH COLOUR TEXT', '102222222');
end;

class function TGraphUtils.GetHisColor(index: integer): TColor;
begin
  result := clTeeColor;
end;

constructor TGraphUtils.Create;
begin

end;

destructor TGraphUtils.Destroy;
begin

  inherited;
end;

class function TGraphUtils.GetSPCColour(index: integer): TColor;
begin
  Result := GraphColour(Index, 'GRAPH COLOUR SPC', '1307946');
end;

class function TGraphUtils.GraphColour(Index: integer;
  OptionStr, DefaultStr: string): TColor;
var
  Opt: TEpiOption;
  Val: integer;
  DefLen, OptLen: Integer;
const
  colorlist: array[0..9] of TColor =
  (clRed, clBlue, clBlack,clGreen, clYellow, clWhite, clSkyBlue, clFuchsia,  clGray , clAqua);
  //Red,  Blue,   Black,   Green,  Yellow,    White,   SkyBlue,  Fuchsia,    Gray ,   Aqua;
begin
  Result := colorlist[0];
  if Dm.GetOptionValue(OptionStr, Opt) then
  try
    DefLen := Length(DefaultStr);
    OptLen := Length(Opt.Value);
    if OptLen < DefLen then
      dm.SetOptionValue(OptionStr, Opt.value + Copy(DefaultStr, 1, DefLen - OptLen));
    Val := StrToInt(Opt.Value[Index]);
    Result := Colorlist[Val];
  except
    result := 0;
  end;
end;

end.
