unit editor_page;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synedit, ComCtrls, executor, history, outputcreator,
  ast, SynEditKeyCmds, Token, lcltype;

type

  { TEditorPage }

  TEditorPage = class(TTabSheet)
  private
    FEditor: TSynEdit;
    FFileName: UTF8String;
    procedure EditorCommand(Sender: TObject; var Command: TSynEditorCommand;
      var AChar: TUTF8Char; Data: pointer);
    function GetModified: boolean;
    procedure NoneExecuteAction(Sender: TObject);
    procedure SetFileName(AValue: UTF8String);
    procedure UpdateCaption;
  private
    // Events
    FOnStatusChange: TNotifyEvent;
    procedure EditorStatusChangeEvent(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoStatusChange;
    procedure ModifyShortCuts;
    procedure AfterStatementHandler(Statement: TCustomStatement);
    procedure BeforeStatementHandler(Statement: TCustomStatement);
    procedure FontChangeEvent(Sender: TObject);
  private
    // Executor, History and OutputCreator
    FExecutor: TExecutor;
    FHistory: THistory;
    FOutputCreator: TOutputCreator;
    procedure SetExecutor(AValue: TExecutor);
    procedure SetHistory(AValue: THistory);
    procedure SetOutputCreator(AValue: TOutputCreator);
  private
    // Parse and Run
    FOldLineNo: Integer;
    FParserStartPoint: TPoint;
    FErrorToken: TToken;
    FExecuting: Boolean;
    FExecutingLines: TStrings;
    procedure DoParse(Const S: UTF8String);
    procedure CommentError(Sender: TObject; ErrorToken: TToken);
    procedure SyntaxError(Sender: TObject; ErrorToken: TToken; TokenTable: TTokenStack);
    procedure LexError(Sender: TObject; ErrorToken: TToken);
    procedure ASTBuildError(Sender: TObject; const Msg: UTF8String; ErrorToken: TToken);
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
  public
    property Executor: TExecutor read FExecutor write SetExecutor;
    property History: THistory read FHistory write SetHistory;
    property OutputCreator: TOutputCreator read FOutputCreator write SetOutputCreator;
  end;

implementation

uses
  Controls, LazFileUtils, editor_pgm_highlighter, Menus, Dialogs, ActnList,
  SynEditTypes, VirtualTrees, ana_globals, parser, LazUTF8Classes;


const
  EDITOR_COMMAND_OUTPUT = 'EDITOR_COMMAND_OUTPUT';
  ecRunAllCommand = ecUserFirst;
  ecRunSelectedCommand = ecRunAllCommand + 1;

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

procedure TEditorPage.ModifyShortCuts;
var
  key: integer;

  procedure RemoveKeyCodeWithIndex(Index: Integer);
  begin
    if (Index <> -1) then
      Editor.Keystrokes.Delete(Index);
  end;

  procedure ModifyKeyCodeCommand(KeyCode: Word; Shift: TShiftState; NewCommand: Word);
  var
    Idx: Integer;
    KeyStroke: TSynEditKeyStroke;
  begin
    RemoveKeyCodeWithIndex(Editor.Keystrokes.FindKeycode(KeyCode, Shift));

    KeyStroke := Editor.Keystrokes.Add;
    KeyStroke.Key := KeyCode;
    KeyStroke.Shift := Shift;
    KeyStroke.Command := NewCommand;
  end;

begin
  with Editor.Keystrokes do
    begin
      for key in [VK_1..VK_9] do
        RemoveKeyCodeWithIndex(FindKeycode(key, [ssCtrlOs, ssShift]));

      for key in [VK_F1..VK_F12] do
        RemoveKeyCodeWithIndex(FindKeycode(key, []));

      RemoveKeyCodeWithIndex(FindKeycode(VK_N, [ssCtrlOs]));
    end;

  ModifyKeyCodeCommand(VK_D, [ssCtrlOs], ecRunSelectedCommand);
  ModifyKeyCodeCommand(VK_R, [ssCtrlOs], ecRunAllCommand);
end;

procedure TEditorPage.SetExecutor(AValue: TExecutor);
begin
  if FExecutor = AValue then Exit;
  FExecutor := AValue;

  FExecutor.AddOnBeforeStatementHandler(@BeforeStatementHandler);
  FExecutor.AddOnAfterStatementHandler(@AfterStatementHandler);

  FExecutor.SetOptions[ANA_SO_EDITOR_FONT_SIZE].AddOnChangeHandler(@FontChangeEvent);
  FExecutor.SetOptions[ANA_SO_EDITOR_FONT_NAME].AddOnChangeHandler(@FontChangeEvent);
end;

procedure TEditorPage.SetHistory(AValue: THistory);
begin
  if FHistory = AValue then Exit;
  FHistory := AValue;
end;

procedure TEditorPage.SetOutputCreator(AValue: TOutputCreator);
begin
  if FOutputCreator = AValue then Exit;
  FOutputCreator := AValue;
end;

procedure TEditorPage.DoParse(const S: UTF8String);
var
  P: TParser;
  Prgm: TStatementList;
begin
  FErrorToken := nil;

  P := TParser.Create(FExecutor);
  P.OnCommentError  := @CommentError;
  P.OnLexError      := @LexError;
  P.OnSyntaxError   := @SyntaxError;
  P.OnASTBuildError := @ASTBuildError;

  if P.ParseText(S, Prgm) then
    begin
      FExecutingLines.Clear;
      FOldLineNo := FHistory.Lines.Count;
      if (FExecutor.SetOptionValue[ANA_SO_EDITOR_HISTORY] = 'ON') then
        FHistory.AddLines(S);
      FExecutingLines.AddText(S);

      FExecuting := true;
      FExecutor.Execute(Prgm);
      FExecuting := false;
    end;
  P.Free;
end;

procedure TEditorPage.CommentError(Sender: TObject; ErrorToken: TToken);
begin

end;

procedure TEditorPage.SyntaxError(Sender: TObject; ErrorToken: TToken;
  TokenTable: TTokenStack);
begin

end;

procedure TEditorPage.LexError(Sender: TObject; ErrorToken: TToken);
begin

end;

procedure TEditorPage.ASTBuildError(Sender: TObject; const Msg: UTF8String;
  ErrorToken: TToken);
begin

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

procedure TEditorPage.EditorCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  case Command of
    ecRunAllCommand:
      DoParse(Editor.Text);

    ecRunSelectedCommand:
      if Editor.SelAvail then
        begin
          FParserStartPoint := Editor.BlockBegin;
          DoParse(Editor.SelText)
        end
      else
        begin
          FParserStartPoint := Point(1, Editor.CaretY);
          DoParse(Editor.Lines[Editor.CaretY - 1]);
        end;
  end;
end;

procedure TEditorPage.BeforeStatementHandler(Statement: TCustomStatement);
var
  Idx: Integer;
begin
  if (not FExecuting) then exit;
  if Assigned(Statement.FindCustomData(EDITOR_COMMAND_OUTPUT)) then exit;
  if (sefInternal in Statement.ExecFlags) then exit;

  Statement.AddCustomData(EDITOR_COMMAND_OUTPUT, TObject(1));

  Idx := Statement.LineNo - 1;
  FOutputCreator.DoCommand('.' + OutputCreatorNormalizeText(FExecutingLines[Idx]));
end;

procedure TEditorPage.FontChangeEvent(Sender: TObject);
begin

end;

procedure TEditorPage.AfterStatementHandler(Statement: TCustomStatement);
begin
  if (not FExecuting) then exit;

  if (Statement.ExecResult <> csrSuccess) and
     (Executor.Cancelled)
  then
    begin
      FErrorToken := TToken.Create(Statement.LineNo, Statement.ColNo, Statement.ByteNo);
      Editor.CaretY := FErrorToken.LineNum + (FParserStartPoint.Y - 1);
      Editor.CaretX := FErrorToken.CaretNum + (FParserStartPoint.X - 1);
      Editor.Invalidate;
    end;
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

  FExecutingLines := TStringListUTF8.Create;

  FEditor := TSynEdit.Create(self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.SetDefaultKeystrokes;
  FEditor.Highlighter := TPGMHighLighter.Create(FEditor);
  FEditor.OnStatusChange := @EditorStatusChangeEvent;
  FEditor.OnCommandProcessed := @EditorCommand;
  ModifyShortCuts;

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

