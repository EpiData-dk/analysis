unit options_cssfileoption;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, options_filesoptions;


type

  { TCSSFileOption }

  TCSSFileOption = class(TFileOption)
  protected
    procedure SetValue(AValue: UTF8String); override;
  public
    constructor Create(const AValue: UTF8String);
  end;


implementation

uses
  ast_types, LazFileUtils, outputgenerator_html;

{ TCSSFileOption }

procedure TCSSFileOption.SetValue(AValue: UTF8String);
var
  Strings: TStringList;
begin
  inherited SetValue(AValue);

  if (AValue <> '') and (not FileExistsUTF8(AValue)) then
    begin
      Strings := TStringList.Create;
      Strings.Add(HTML_OUTPUT_CSS);
      Strings.SaveToFile(AValue);
      Strings.Free;

      DoWarning('File Not found, Default css content saved as: ' + AValue);
    end;
end;

constructor TCSSFileOption.Create(const AValue: UTF8String);
begin
  inherited Create(AValue, rtString);
  Options := [foAcceptNonExistingFilename, foAcceptEmptyFilename];
end;

end.

