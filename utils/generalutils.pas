unit generalutils;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes;

function PercentileIndex(const Size: integer; const Percentile: Double; out Factor: EpiFloat): Integer;

implementation

uses
  Math;

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

  FPos := min(max((Size + 1) * (Percentile), 1), Size);
  IPos := max(trunc((Size + 1) * (Percentile)), 1);

  Result := IPos;
  if FPos <> IPos then
    Factor := (FPos - IPos);
end;


end.
