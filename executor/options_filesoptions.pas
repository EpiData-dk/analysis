unit options_filesoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, options_hashmap, Graphics;

type

  { TCustomFileOption }

  TCustomFileOption = class(TSetOption)
  protected
    procedure DoOptionError(Const S: String);
  end;

  { TFolderOption }

  TFolderOption = class(TCustomFileOption)
  protected
    procedure SetValue(AValue: UTF8String); override;
  end;


implementation

uses
  LazFileUtils;

{ TFolderOption }

procedure TFolderOption.SetValue(AValue: UTF8String);
begin
   if (not DirectoryExistsUTF8(AValue)) then
     DoOptionError('Folder: ' + AValue + LineEnding +
                   'Does not exist!'
     );

  inherited SetValue(AValue);
end;

{ TCustomFileOption }

procedure TCustomFileOption.DoOptionError(const S: String);
begin
  raise ESetOption.Create(S);
end;

end.

