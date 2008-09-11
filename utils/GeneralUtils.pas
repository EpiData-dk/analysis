unit GeneralUtils;

interface

uses
  SysUtils, Windows, Classes, UEpiDataTypes;

type
  TString = class(TObject)
  private
    fStr: String;
  public
    constructor Create(const AStr: String) ;
    property Str: String read FStr write FStr;
  end;

  procedure SplitString(const source: string; List: TStrings; const Splitters: TCharset = [' ']);

implementation

{******************************
 *   TString
 ******************************}

constructor TString.Create(const AStr: String) ;
begin
   inherited Create;
   FStr := AStr;
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

end.
