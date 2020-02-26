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
    function GetModified: boolean;
    procedure NoneExecuteAction(Sender: TObject);
    procedure SetFileName(AValue: UTF8String);
    procedure UpdateCaption;
  private
    // Events
    FOnStatusChange: TNotifyEvent;
    procedure EditorStatusChangeEvent(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoStatusChange;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: UTF8String);
    procedure SaveToFile(FileName: UTF8String); overload;
    procedure SaveToFile; overload;
    property Editor: TSynEdit read FEditor;
    property FileName: UTF8String read FFileName;
    property Modified: boolean read GetModified;
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;
  end;

implementation

uses
  Controls, LazFileUtils, editor_pgm_highlighter, Menus, Dialogs, ActnList,
  SynEditTypes;

{ TEditorPage }

procedure TEditorPage.SetFileName(AValue: UTF8String);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;

  UpdateCaption;
end;

procedure TEditorPage.EditorStatusChangeEvent(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (scModified in Changes) then
    UpdateCaption;

  if (Changes * [scCaretX, scCaretY, scInsertMode, scModified] <> []) then
    DoStatusChange;
end;

procedure TEditorPage.DoStatusChange;
begin
  if (Assigned(OnStatusChange)) then
    OnStatusChange(self);
end;

procedure TEditorPage.UpdateCaption;
var
  S: String;
begin
  if FileName = '' then
    S := 'Untitled'
  else
    S := ExtractFileNameOnly(FFileName);

  if Modified then
    S := '*' + S;

  Caption := S;
end;

procedure TEditorPage.NoneExecuteAction(Sender: TObject);
begin
  ShowMessage('Action not implemented yet');
end;

function TEditorPage.GetModified: boolean;
begin
  result := Editor.Modified;
end;

constructor TEditorPage.Create(TheOwner: TComponent);
var
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
  FEditor.OnStatusChange := @EditorStatusChangeEvent;

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

procedure TEditorPage.LoadFromFile(FileName: UTF8String);
begin
  Editor.Lines.LoadFromFile(FileName);
  Editor.Modified := false;
  SetFileName(FileName);
end;

procedure TEditorPage.SaveToFile(FileName: UTF8String);
begin
  SetFileName(FileName);
  SaveToFile;
end;

procedure TEditorPage.SaveToFile;
begin
  Editor.Lines.SaveToFile(FileName);
  Editor.Modified := false;

  DoStatusChange;
end;

end.

