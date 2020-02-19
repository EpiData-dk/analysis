unit editor_page;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synedit;

type

  { TEditorPage }

  TEditorPage = class
  private
    FEditor: TSynEdit;
    FFileName: UTF8String;
    function GetCaption: UTF8String;
    procedure NoneExecuteAction(Sender: TObject);
    procedure SetFileName(AValue: UTF8String);
  public
    constructor Create;
    destructor Destroy; override;
    property Editor: TSynEdit read FEditor;
    property FileName: UTF8String read FFileName write SetFileName;
    property Caption: UTF8String read GetCaption;
  end;

implementation

uses
  Controls, LazFileUtils, editor_pgm_highlighter, Menus, Dialogs, ActnList;

{ TEditorPage }

procedure TEditorPage.SetFileName(AValue: UTF8String);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
end;

function TEditorPage.GetCaption: UTF8String;
begin
  if (FileName = '') then
    Result := 'Untitled'
  else
    Result := ExtractFileNameOnly(FFileName);

  if (Editor.Modified) then
    Result := Result + '*';
end;

procedure TEditorPage.NoneExecuteAction(Sender: TObject);
begin
  ShowMessage('Action not implemented yet');
end;

constructor TEditorPage.Create;
var
  PGMHighlighter: TPGMHighLighter;
  PopupMenu: TPopupMenu;

  function CreateActionAndMenuItem(Caption: TCaption; OnExecute: TNotifyEvent; ShortCut: TShortCut): TMenuItem;
  var
    TheAction: TAction;
  begin
    TheAction := TAction.Create(FEditor);
    TheAction.Caption := Caption;
    TheAction.OnExecute := OnExecute;
    TheAction.ShortCut := ShortCut;

    Result := TMenuItem.Create(FEditor);
    Result.Action := TheAction;
  end;

  function CreateDivider(): TMenuItem;
  begin
    Result := TMenuItem.Create(FEditor);
    Result.Caption := '-';
  end;

begin
  FEditor := TSynEdit.Create(nil);
  FEditor.Align := alClient;
  FEditor.SetDefaultKeystrokes;
  FEditor.Highlighter := TPGMHighLighter.Create(FEditor);

  PopupMenu := TPopupMenu.Create(FEditor);
  PopupMenu.Items.Add(CreateActionAndMenuItem('Copy', @NoneExecuteAction, 0));
  PopupMenu.Items.Add(CreateActionAndMenuItem('Cut', @NoneExecuteAction, 0));
  PopupMenu.Items.Add(CreateActionAndMenuItem('Paste', @NoneExecuteAction, 0));
  PopupMenu.Items.Add(CreateDivider());
  PopupMenu.Items.Add(CreateActionAndMenuItem('Run Selected', @NoneExecuteAction, 0));
  PopupMenu.Items.Add(CreateActionAndMenuItem('Run All', @NoneExecuteAction, 0));

  FEditor.PopupMenu := PopupMenu;

  FFileName := '';
end;

destructor TEditorPage.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

end.

