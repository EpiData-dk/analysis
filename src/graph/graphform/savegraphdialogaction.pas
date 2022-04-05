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
    procedure FilterChange(Sender: TObject);
  protected
    procedure UpdateReadyState(); override;
    procedure SaveGraphExecute(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Chart;
  end;

implementation

uses
  LazFileUtils;

const
  ExportExt: array[TGraphExportType] of string = (
    '.svg',
    '.png',
    '.jpg'
  );

{ TSaveGraphDialogAction }

procedure TSaveGraphDialogAction.FilterChange(Sender: TObject);
begin
  case FSaveDialog.FilterIndex of
    1: GraphExportType := etSVG;
    2: GraphExportType := etPNG;
    3: GraphExportType := etJPG;
  end;
  FSaveDialog.DefaultExt := ExportExt[GraphExportType];
end;

procedure TSaveGraphDialogAction.UpdateReadyState();
begin
  Enabled := Assigned(Chart);
end;

procedure TSaveGraphDialogAction.SaveGraphExecute(Sender: TObject);
begin
  if (not FSaveDialog.Execute) then
    Exit;

  Filename := FSaveDialog.FileName;

  inherited SaveGraphExecute(Sender);
end;

constructor TSaveGraphDialogAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSaveDialog := TSaveDialog.Create(Self);
  FSaveDialog.Filter := 'SVG File|*.svg|PNG File|*.png|JPEG File|*.jpg;*.jpeg';
  FSaveDialog.FilterIndex := 0;
  FSaveDialog.OnTypeChange := @FilterChange;
  FSaveDialog.InitialDir := GetCurrentDirUTF8;
  FSaveDialog.Options := [ofOverwritePrompt, ofPathMustExist, ofEnableSizing];
end;

end.

