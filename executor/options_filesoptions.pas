unit options_filesoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, options_hashmap, ast_types;

type

  { TFolderOption }

  TFolderOption = class(TSetOption)
  protected
    procedure SetValue(AValue: UTF8String); override;
  end;

  { TFileOption }

  TFileOptionOption = (foAcceptEmptyFilename);
  TFileOptionOptions = set of TFileOptionOption;

  TFileOption = clasS(TSetOption)
  private
    FOptions: TFileOptionOptions;
  protected
    procedure SetValue(AValue: UTF8String); override;
  public
    constructor Create(const AValue: UTF8String; AAstType: TASTResultType); override;
    property Options: TFileOptionOptions read FOptions write FOptions;
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

{ TFileOption }

procedure TFileOption.SetValue(AValue: UTF8String);
begin
  if (AValue = '') and (not (foAcceptEmptyFilename in Options)) then
    DoError('Empty file is not allowed!');

  if (AValue <> '') and (not FileExistsUTF8(AValue)) then
    DoError('File: ' + AValue + LineEnding +
            'Does not exist!'
    );

  inherited SetValue(AValue);
end;

constructor TFileOption.Create(const AValue: UTF8String;
  AAstType: TASTResultType);
begin
  inherited Create(AValue, AAstType);
  FOptions := [];
end;

end.

