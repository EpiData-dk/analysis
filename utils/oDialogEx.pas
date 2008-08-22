unit oDialogEx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   Registry;

const
  DEF_AUTOSAVE = True;
  DEF_AUTOLOAD = True;
  DEF_KEY = '\Software\My Program';
  DEF_SECTION = 'LastOpenPath';

type
{  TAboutOpenDialogEx = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;
}
  TOpenDialogEx = class(TOpenDialog)
  private
    FKey: string;
    FSection: string;
    FAutoSave: boolean;
    FAutoLoad: boolean;
//    FAbout: TAboutOpenDialogEx;
    FOnBeforeExecute: TNotifyEvent;
    FOnAfterExecute: TNotifyEvent;
  public
    function Execute: boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SavePath: boolean;
    function LoadPath: string;
  published
//    property About: TAboutOpenDialogEx read FAbout write FAbout;
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
    property AutoSave: boolean read FAutoSave write FAutoSave;
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property SaveKey: string read FKey write FKey;
    property SaveSection: string read FSection write FSection;
  end;

procedure Register;

implementation
{
procedure TAboutOpenDialogEx.Edit;
begin
  Application.MessageBox('TOpenDialogEx Component v1.00 for Delphi 2.0. Copyright (C) 1997 Ivan Azic.',
                         'About TOpenDialogEx Component', MB_OK + MB_ICONINFORMATION);
end;

function TAboutOpenDialogEx.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutOpenDialogEx.GetValue: string;
begin
  Result:= '(about)';
end;
}

function TOpenDialogEx.Execute: boolean;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);
  if FAutoLoad then
    InitialDir:= LoadPath;
  inherited Execute;
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
  if FAutoSave then
    SavePath;
end;

constructor TOpenDialogEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKey:= DEF_KEY;
  FSection:= DEF_SECTION;
  FAutoSave:= DEF_AUTOSAVE;
  FAutoLoad:= DEF_AUTOLOAD;
end;

destructor TOpenDialogEx.Destroy;
begin
  inherited Destroy;
end;

function TOpenDialogEx.SavePath: boolean;
var
  p: TRegIniFile;
begin
  Result:= False;
  if (FKey = '') or (FSection = '') or (FileName = '')then
    exit;
  try
    p:= TRegIniFile.Create(FKey);
    p.WriteString(FSection, 'LastOpenPath', ExtractFilePath(FileName));
    Result:= True;
  finally
    p.Free;
  end;
end;

function TOpenDialogEx.LoadPath: string;
var
  p: TRegIniFile;
begin
  Result:= '';
  try
    p:= TRegIniFile.Create(FKey);
    Result:= p.ReadString(FSection, 'LastOpenPath', '');
  finally
    p.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('Dialogs', [TOpenDialogEx]);
//  RegisterPropertyEditor(TypeInfo(TAboutOpenDialogEx), TOpenDialogEx, 'ABOUT', TAboutOpenDialogEx);
end;

end.
