unit savegraphdialogaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, savegraphaction, Dialogs;

type

  { TSaveGraphDialogAction }

  TSaveGraphDialogAction = class(TCustomSaveGraphAction)
  private
    FSaveDialog: TSaveDialog;
  protected
    procedure UpdateReadyState(); override;
    procedure SaveGraphExecute(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  LazFileUtils;

{ TSaveGraphDialogAction }

procedure TSaveGraphDialogAction.UpdateReadyState();
begin
  Enabled := true; //Assigned(Chart); // maybe Count > 0?
end;

procedure TSaveGraphDialogAction.SaveGraphExecute(Sender: TObject);
begin
  // TODO: consider bypassing save file dialog and use our own form
  if (not FSaveDialog.Execute) then
    Exit;
  Filename := FSaveDialog.FileName;
  if (not ExtensionOK) then
  begin
    ShowMessage('"' + Filename + '"' + LineEnding +
      'does not have a valid file extension.' + LineEnding +
      '(jpg, jpeg, png, svg)' + LineEnding +
      'Nothing saved.');
    exit;
  end;
  inherited SaveGraphExecute(Sender);
  FSaveDialog.FileName := '';
end;

constructor TSaveGraphDialogAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSaveDialog := TSaveDialog.Create(Self);
  FSaveDialog.Filter := '';
  FSaveDialog.InitialDir := GetCurrentDirUTF8;
  FSaveDialog.Options := [ofPathMustExist, ofEnableSizing];  // omit ofOverwritePrompt
  FSaveDialog.Title := 'File name MUST have a valid type: .jpg, .png or .svg';
end;

end.

