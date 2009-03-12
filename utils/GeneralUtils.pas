unit GeneralUtils;

interface

uses
  SysUtils, Windows, Classes, UEpiDataTypes, AnsDatatypes, Math;

type
  TString = class(TObject)
  private
    fStr: String;
  public
    constructor Create(const AStr: String) ;
    property Str: String read FStr write FStr;
  end;

  procedure SplitString(const source: string; List: TStrings; const Splitters: TCharset = [' ']);
  function PercentileIndex(const Size: integer; const Percentile: Double; var Factor: EpiFloat): Integer;
  function R2(const AValue : extended ; const ADigit : TRoundToRange):extended ;
  function PreInc(var i: integer): integer;
  function PostInc(var i: integer): integer; 

implementation


{******************************
 *   TString
 ******************************}

constructor TString.Create(const AStr: String) ;
begin
   inherited Create;
   FStr := AStr;
end;

function R2(const AValue : extended ; const ADigit : TRoundToRange):extended ;
var
  X : extended;
  i : integer ;
begin
  X := 1.0 ;
  for i := 1 to Abs(ADigit) do X := X * 10 ;
  if ADigit<0
    then Result := Round(AValue * X) / X
    else Result := Round(AValue / X) * X;
end;

procedure SplitString(const source: string; list: TStrings; const Splitters: TCharset = [' ']);
var
  P, P1: PChar;
  S: string;
begin
  if not Assigned(List) then
    List := TStringList.Create;
  list.BeginUpdate;
  try
    list.Clear;
    P := PChar(source);
    while P^ in [#1..' '] do P := CharNext(P);
    while P^ <> #0 do
    begin
      if P^ = '"' then
        S := AnsiExtractQuotedStr(P, '"')
      else
      begin
        P1 := P;
        while (not (P^ in Splitters)) and (P^ <> #0) do P := CharNext(P);
        SetString(S, P1, P - P1);
      end;
      list.Add(S);
      while P^ in Splitters do P := CharNext(P);
    end;
  finally
    list.EndUpdate;
  end;
end;


function PercentileIndex(const Size: integer; const Percentile: Double; var Factor: EpiFloat): Integer;

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
  If Size < 20 then
  begin
    Result := SmallPercentileMatrix[Size][GetPercentileIndex(Percentile)];
    Exit;
  end;

  FPos := Math.min(Math.max((Size + 1) * (Percentile), 1), Size);
  IPos := Math.max(trunc((Size + 1) * (Percentile)), 1);

  Result := IPos;
  if FPos <> IPos then
    Factor := (FPos - IPos);
end;

function PreInc(var i: integer): integer;
begin
  inc(i);
  result := i;
end;

function PostInc(var i: integer): integer;
begin
  result := i;
  inc(i);
end;

end.
