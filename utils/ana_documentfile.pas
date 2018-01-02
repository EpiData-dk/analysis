unit ana_documentfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiopenfile, epiv_documentfile, epicustombase, epiadmin, ast;

type

  TAnaDocumentFile = class;

  TAnaGetReadCommand = procedure (Sender: TAnaDocumentFile; out Cmd: TCustomStringCommand) of object;

  { TAnaDocumentFile }

  TAnaDocumentFile = class(TDocumentFile)
  private
    FImportedFilename: UTF8String;
    FOnGetReadCommand: TAnaGetReadCommand;
  protected
    function DoGetReadCommand(out Cmd: TCustomStringCommand): boolean;
    procedure SaveHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer); override;
    function DoPassWord(Sender: TObject; RequestType: TEpiRequestPasswordType;
      RequestNo: Integer; var Login: UTF8String; var Password: UTF8String): TEpiRequestPasswordResponse; override;
    function GetFileName: string; override;
  public
    constructor Create; override;
    property ImportedFilename: UTF8String read FImportedFilename write FImportedFilename;
    property OnGetReadCommand: TAnaGetReadCommand read FOnGetReadCommand write FOnGetReadCommand;
  end;

implementation

{ TAnaDocumentFile }

function TAnaDocumentFile.DoGetReadCommand(out Cmd: TCustomStringCommand): boolean;
begin
  result := false;

  if Assigned(OnGetReadCommand) then
    begin
      OnGetReadCommand(Self, Cmd);
      result := Assigned(Cmd);
    end;
end;

procedure TAnaDocumentFile.SaveHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  // Do not do "inherited" since here in Analysis we do not want to continiously
  // save data on changes. Any saves my be done explicitly using a save command.
end;

function TAnaDocumentFile.DoPassWord(Sender: TObject;
  RequestType: TEpiRequestPasswordType; RequestNo: Integer;
  var Login: UTF8String; var Password: UTF8String): TEpiRequestPasswordResponse;
var
  Cmd: TCustomStringCommand;
  Opt: TOption;
  Res: Boolean;
begin
  // Set to stop on fail, since there is no need to retry passwords
  // multiple times - the answer will not change!
  Result := rprStopOnFail;
  Res := DoGetReadCommand(Cmd);

  if Res then
    begin
      if Cmd.HasOption('pw', Opt) then
        Password := Opt.Expr.AsString;

      if Cmd.HasOption('login', Opt) then
        Login := Opt.Expr.AsString;

      case RequestType of
        erpSinglePassword:
          if Password = '' then
            Res := false;


        erpUserLogin:
          if (Password = '') or
             (Login    = '')
          then
            Res := false;

        erpNewPassword:
          Res := false;
      end;
    end;

  if (not Res) then
    Result := inherited DoPassWord(Sender, RequestType, RequestNo, Login, Password);
end;

function TAnaDocumentFile.GetFileName: string;
begin
  result := inherited GetFileName;

  if (IsSaved) then
    FImportedFilename := '';

  if (FImportedFilename <> '')
  then
    Result := FImportedFilename
end;

constructor TAnaDocumentFile.Create;
begin
  inherited Create;
  FImportedFilename := '';
end;

end.

