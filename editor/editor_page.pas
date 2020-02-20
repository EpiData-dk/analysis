unit editor_page;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synedit, ComCtrls;

type

  { TEditorPage }

  TEditorPage = class(TTabSheet)
  private
    FEditor: TSynEdit;
    FFileName: UTF8String;
    procedure NoneExecuteAction(Sender: TObject);
    procedure SetFileName(AValue: UTF8String);
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Editor: TSynEdit read FEditor;
    property FileName: UTF8String read FFileName write SetFileName;
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

procedure TEditorPage.NoneExecuteAction(Sender: TObject);
begin
  ShowMessage('Action not implemented yet');
end;

constructor TEditorPage.Create(TheOwner: TComponent);
var
  PGMHighlighter: TPGMHighLighter;
  LPopupMenu: TPopupMenu;

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
  inherited Create(TheOwner);

  FEditor := TSynEdit.Create(self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.SetDefaultKeystrokes;
  FEditor.Highlighter := TPGMHighLighter.Create(FEditor);

  LPopupMenu := TPopupMenu.Create(FEditor);
  LPopupMenu.Items.Add(CreateActionAndMenuItem('Copy', @NoneExecuteAction, 0));
  LPopupMenu.Items.Add(CreateActionAndMenuItem('Cut', @NoneExecuteAction, 0));
  LPopupMenu.Items.Add(CreateActionAndMenuItem('Paste', @NoneExecuteAction, 0));
  LPopupMenu.Items.Add(CreateDivider());
  LPopupMenu.Items.Add(CreateActionAndMenuItem('Run Selected', @NoneExecuteAction, 0));
  LPopupMenu.Items.Add(CreateActionAndMenuItem('Run All', @NoneExecuteAction, 0));

  FEditor.PopupMenu := LPopupMenu;

  Caption := 'Untitled';

  FFileName := '';
end;

destructor TEditorPage.Destroy;
begin
  inherited Destroy;
end;

end.

