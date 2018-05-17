unit options_filesoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, options_hashmap;

type

  { TFolderOption }

  TFolderOption = class(TSetOption)
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
     DoError('Folder: ' + AValue + LineEnding +
             'Does not exist!'
     );

  inherited SetValue(AValue);
end;

end.

