unit generalutils;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes, options_utils, ast, epidatafiles;

function PercentileIndex(const Size: integer; const Percentile: Double; out Factor: EpiFloat): Integer;
function PercentileIndexNIST(const Size: integer; const Percentile: Double; out Factor: EpiFloat): Integer;
function FormatP(Val: EpiFloat; ShowP: Boolean): UTF8String;
function FormatCI(Val1: EpiFloat; Val2: EpiFLoat; Pct: Integer; Options:TOptionList): UTF8String;
function FormatRatio(Val: EpiFloat; Options: TOptionList): UTF8String;

implementation

uses
  Math;

// TODO: why is this 'shortcut' taken? NIST suggests using standard approach
//       even with small numbers (https://www.itl.nist.gov/div898/handbook/prc/section2/prc262.htm)
//       can stay with this method (Hyndman & Tan, method R6) or use method R7 as in R or Excel
//       NIST also suggests using max when p ≥ N/(N+1) and min when p ≤ 1/(N+1)
const
  SmallPercentileMatrix: Array[1..19] of Array[1..11] of Integer =
//   N =      P1, P2.5, P5, P10, P25, P50, P75, P90, P95, P97.5, P99
     {1}    ((1,  1,    1,  1,   1,   1,   1,   1,   1,   1,     1),
     {2}     (1,  1,    1,  1,   1,   2,   2,   2,   2,   2,     2),
     {3}     (1,  1,    1,  1,   1,   2,   3,   3,   3,   3,     3),
     {4}     (1,  1,    1,  1,   1,   3,   4,   4,   4,   4,     4),
     {5}     (1,  1,    1,  1,   2,   3,   5,   5,   5,   5,     5),
     {6}     (1,  1,    1,  1,   2,   4,   5,   6,   6,   6,     6),
     {7}     (1,  1,    1,  1,   2,   4,   6,   7,   7,   7,     7),
     {8}     (1,  1,    1,  1,   2,   5,   7,   8,   8,   8,     8),
     {9}     (1,  1,    1,  1,   3,   5,   8,   9,   9,   9,     9),
    {10}     (1,  1,    1,  1,   3,   6,   8,  10,  10,  10,    10),
    {11}     (1,  1,    1,  1,   3,   6,   9,  11,  11,  11,    11),
    {12}     (1,  1,    1,  1,   3,   7,  10,  12,  12,  12,    12),
    {13}     (1,  1,    1,  1,   4,   7,  11,  13,  13,  13,    13),
    {14}     (1,  1,    1,  2,   4,   8,  11,  14,  14,  14,    14),
    {15}     (1,  1,    1,  2,   4,   8,  12,  14,  15,  15,    15),
    {16}     (1,  1,    1,  2,   4,   9,  13,  15,  16,  16,    16),
    {17}     (1,  1,    1,  2,   5,   9,  14,  16,  17,  17,    17),
    {18}     (1,  1,    1,  2,   5,  10,  14,  17,  18,  18,    18),
    {19}     (1,  1,    1,  2,   5,  10,  15,  18,  19,  19,    19));


function PercentileIndex(const Size: integer; const Percentile: Double; out Factor: EpiFloat): Integer;

  function GetPercentileIndex(const Percentile: Double): Integer;
  begin
    result := 1;
    if Percentile >= 0.025 then result := 2;
    if Percentile >= 0.05  then result := 3;
    if Percentile >= 0.10  then result := 4;
    if Percentile >= 0.25  then result := 5;
    if Percentile >= 0.50  then result := 6;
    if Percentile >= 0.75  then result := 7;
    if Percentile >= 0.90  then result := 8;
    if Percentile >= 0.95  then result := 9;
    if Percentile >= 0.975 then result := 10;
    if Percentile >= 0.99  then result := 11;
  end;

var
  FPos: Extended;
  IPos: Integer;
begin
  Factor := 0;
  If (Size < 20) and (Percentile <> 0.5) then
  begin
    Result := SmallPercentileMatrix[Size][GetPercentileIndex(Percentile)];
    Exit;
  end;
  // method R6 of Hyndman and Fan; can apply this to any Size > 1
  FPos := min(max((Size + 1) * (Percentile), 1), Size);
  IPos := max(trunc((Size + 1) * (Percentile)), 1);

  Result := IPos;
  if FPos <> IPos then
    Factor := (FPos - IPos);
end;

function PercentileIndexNIST(const Size: integer; const Percentile: Double; out Factor: EpiFloat): Integer;
 var
   FPos: Extended;
   IPos: Integer;
 begin
   Factor := 0;
   if Percentile <= 1 / (Size + 1) then
   begin
     result := 1;
     exit
   end;
   if Percentile >= Size / (Size + 1) then
   begin
     result := Size;
     exit
   end;
   FPos   := (Size + 1) * Percentile;
   IPos   := trunc(FPos);
   Factor := FPos-Ipos;
   if Factor < 1/ (Size + 1) then
      Factor := 0;
   result := IPos;
 end;

function FormatP(Val: EpiFloat; ShowP: Boolean): UTF8String;

var
  prefix: UTF8string = '';
begin
  // special case of zero ([p=]0)
  if (ShowP) then prefix := 'p=';
  if (Val = 0) then
    begin
      Result := prefix + '0';
      exit;
    end;
  // special cases of low p ([p]< ...)
  if (ShowP) then prefix := 'p';
  if (Val < 0.0001) then
    begin
      Result := prefix + '<0.0001';
      exit;
    end;
  if (Val < 0.001) then
    begin
      Result := prefix + '<0.001';
      exit;
    end;
  // p >= 0.001, show actual value
  if (ShowP) then prefix := 'p=';
  if (Val = TEpiFloatField.DefaultMissing) then
    Result := prefix + '-'
  else
    Result := prefix + Format('%.3f', [Val]) ;
end;

function FormatCI(Val1: EpiFloat; Val2: EpiFLoat; Pct: Integer; Options: TOptionList): UTF8String;
begin
  Result := '';
  if (Pct > 0) then Result += IntToStr(Pct) + '% CI: ';
  Result += '(' + FormatRatio(Val1,Options)
         + ', ' + FormatRatio(Val2,Options) + ')';
end;

function FormatRatio(Val: EpiFloat; Options: TOptionList): UTF8String;
begin
  if (Val = TEpiFloatField.DefaultMissing) then
    Result := '.'
  else
    Result := Format('%.' + IntToStr(DecimalFromOption(Options, 2)) + 'f', [Val]);
end;

end.

