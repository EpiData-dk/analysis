unit options_string_array;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, options_hashmap, ast_types;


type

  { TStringArrayOption }

  TStringArrayOption = class(TSetOption)
  private
    FArraySize: Integer;
  protected
    procedure SetValue(AValue: UTF8String); override;
  public
    constructor Create(Values: array of utf8string; AAstType: TASTResultType);
  end;

implementation

uses
  LazUTF8;

{ TStringArrayOption }

procedure TStringArrayOption.SetValue(AValue: UTF8String);
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  AList.CommaText := AValue;

  try
    if (AList.Count <> FArraySize) then
      DoError('Incorrect number of values given.' + LineEnding +
              'Expected ' + IntToStr(FArraySize) + ', but got ' + IntToStr(AList.Count));

    inherited SetValue(AValue);
  finally
    AList.Free;
  end;
end;

constructor TStringArrayOption.Create(Values: array of utf8string;
  AAstType: TASTResultType);
var
  AllValues, S: UTF8String;
begin
  FArraySize := Length(Values);

  AllValues := '';
  for S in Values do
    AllValues := AllValues + ',' + S;

  UTF8Delete(AllValues, 1, 1);
  inherited Create(AllValues, AAstType);
end;

end.

