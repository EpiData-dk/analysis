unit editor_page;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synedit, ComCtrls, executor, history, outputcreator,
  ast, SynEditKeyCmds, Token, lcltype, Graphics, SynEditTypes, Dialogs;

type

  { TEditorPage }
  TFindDialogClass = class of TFindDialog;

  TEditorPage = class(TTabSheet)
  private
    FEditor: TSynEdit;
    FFileName: UTF8String;
    procedure EditorClick(Sender: TObject);
    procedure EditorCommand(Sender: TObject; var Command: TSynEditorCommand;
      var AChar: TUTF8Char; Data: pointer);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
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
    // Search
    FActiveDialog: TFindDialog; static;
    FActiveSearchText: UTF8String; static;
    FActiveSearchOptions: TSynSearchOptions; static;
    procedure PerformSearchAsync(Data: PtrInt);
    function GetSearchText(): UTF8String;
    procedure StartSearch(Const SearchText: UTF8String; DialogClass: TFindDialogClass);
    procedure SearchFind(Sender: TObject);
    procedure SearchDlgShow(Sender: TObject);
    procedure SearchClose(Sender: TObject);
    procedure InternalSearch(Const ReplaceText: UTF8String);
  private
    // Parse and Run
    FOldLineNo: Integer;
    FParserStartPoint: TPoint;
    FErrorToken: TToken;
    FExecuting: Boolean;
    FExecutingLines: TStrings;
    procedure RunAsync(Data: PtrInt);
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
    procedure UpdateEditorFont(NewFont: TFont);
    property Editor: TSynEdit read FEditor;
    property FileName: UTF8String read FFileName;
    property Modified: boolean read GetModified;
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;
  public
    // Editor Form Main Menu Actions
    procedure PerformFind();
    procedure PerformFindNext();
    procedure PerformFindPrev();
    procedure PerformReplace();
    procedure PerformCut();
    procedure PerformCopy();
    procedure PerformPaste();
    procedure PerformInsertHistory();
    procedure PerformInsertSetOptions();
    procedure PerformRunAll();
    procedure PerformRunSelected();
    procedure PerformUndo();
    procedure PerformRedo();
  public
    property Executor: TExecutor read FExecutor write SetExecutor;
    property History: THistory read FHistory write SetHistory;
    property OutputCreator: TOutputCreator read FOutputCreator write SetOutputCreator;
  end;

implementation

uses
  Controls, LazFileUtils, editor_pgm_highlighter, Menus, ActnList,
  VirtualTrees, ana_globals, parser, LazUTF8Classes, GOLDParser,
  Symbol, Forms, Math, options_hashmap, LazUTF8, ast_types, strutils,
  LCLProc;


const
  EDITOR_COMMAND_OUTPUT = 'EDITOR_COMMAND_OUTPUT';
  ecRunAllCommand       = ecUserFirst;
  ecRunSelectedCommand  = ecRunAllCommand + 1;
  ecFindCommand         = ecRunSelectedCommand + 1;
  ecFindNextCommand     = ecFindCommand + 1;
  ecFindPrevCommand     = ecFindNextCommand + 1;
  ecReplaceCommand      = ecFindPrevCommand + 1;
  ecInsertN             = ecReplaceCommand + 1;

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
  ModifyKeyCodeCommand(VK_D, [ssCtrlOS, ssShift], ecRunAllCommand);
  ModifyKeyCodeCommand(VK_F, [ssCtrlOs], ecFindCommand);
  ModifyKeyCodeCommand(VK_R, [ssCtrlOs], ecReplaceCommand);
  ModifyKeyCodeCommand(VK_N, [ssCtrlOs, ssShift], ecFindNextCommand);
  ModifyKeyCodeCommand(VK_P, [ssCtrlOs, ssShift], ecFindPrevCommand);
  ModifyKeyCodeCommand(VK_DELETE, [ssCtrlOS], ecDeleteWord);
  ModifyKeyCodeCommand(VK_N, [ssAlt], ecInsertN);
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

procedure TEditorPage.PerformSearchAsync(Data: PtrInt);
var
  Command: TSynEditorCommand;
begin
  Command := TSynEditorCommand(Data);
  case (Command) of
    ecFindCommand:
      PerformFind();
    ecFindNextCommand:
      PerformFindNext();
    ecFindPrevCommand:
      PerformFindPrev();
    ecReplaceCommand:
      PerformReplace();
  end;
end;

function TEditorPage.GetSearchText(): UTF8String;
begin
  Result := FActiveSearchText;
  if Editor.SelAvail then
    Result := Editor.SelText;
end;

procedure TEditorPage.StartSearch(const SearchText: UTF8String;
  DialogClass: TFindDialogClass);
begin
  if (Assigned(FActiveDialog)) then
    begin
      if (FActiveDialog.ClassType <> DialogClass) then
        begin
          FActiveDialog.CloseDialog;
          FActiveDialog := DialogClass.Create(nil);
          FActiveDialog.Options := [frDown, frHidePromptOnReplace];
        end;
    end
  else
    begin
      FActiveDialog := DialogClass.Create(nil);
      FActiveDialog.Options := [frDown, frHidePromptOnReplace];
    end;

  FActiveDialog.FindText := SearchText;
  FActiveDialog.OnFind   := @SearchFind;
  FActiveDialog.OnShow   := @SearchDlgShow;
  FActiveDialog.OnClose  := @SearchClose;
  if (FActiveDialog is TReplaceDialog) then TReplaceDialog(FActiveDialog).OnReplace := @SearchFind;
  FActiveDialog.Execute;
end;

procedure TEditorPage.SearchFind(Sender: TObject);
var
  Dlg: TFindDialog absolute sender;
  FindText, ReplaceText: String;
  Options: TSynSearchOptions;
begin
  FindText := Dlg.FindText;
  if (FindText = '') then exit;

  ReplaceText := FindText;

  Options := [];
  if (frWholeWord       in Dlg.Options)  then include(Options, ssoWholeWord);
  if (frMatchCase       in Dlg.Options)  then include(Options, ssoMatchCase);
  if (frEntireScope     in Dlg.Options)  then include(Options, ssoEntireScope);
  if (frPromptOnReplace in Dlg.Options)  then include(Options, ssoPrompt);
  if (frReplace         in Dlg.Options)  then Include(Options, ssoReplace);
  if (frReplaceAll      in Dlg.Options)  then Include(Options, ssoReplaceAll);
  if (not (frFindNext   in Dlg.Options)) then ReplaceText := TReplaceDialog(Dlg).ReplaceText;
  if (not (frDown       in Dlg.Options)) then include(Options, ssoBackwards);
  if (frPromptOnReplace in Dlg.Options)  then include(Options, ssoPrompt);

  FActiveSearchOptions := Options;
  FActiveSearchText    := FindText;

  InternalSearch(ReplaceText);
end;

procedure TEditorPage.SearchDlgShow(Sender: TObject);
var
  P: TPoint;
  MfBound: TRect;
begin
  P := ClientOrigin;
  MfBound := ClientRect;
  OffsetRect(MfBound, P.X, P.Y);
  P.Y := MfBound.Top + (((MfBound.Bottom - MfBound.Top)  - FActiveDialog.Height) Div 2);
  P.X := MfBound.Left + (((MfBound.Right - MfBound.Left) - FActiveDialog.Width) Div 2);

  FActiveDialog.Position := P;
end;

procedure TEditorPage.SearchClose(Sender: TObject);
begin
  Application.ReleaseComponent(TComponent(Sender));
  FActiveDialog := nil;
end;

procedure TEditorPage.InternalSearch(const ReplaceText: UTF8String);
var
  Res: Integer;
  Pt: TPoint;
begin
  Pt := Editor.CaretXY;

  if (Editor.SelAvail) and
     (Editor.SelText = FActiveSearchText) and
     ([ssoReplace, ssoReplaceAll] * FActiveSearchOptions <> [])
  then
    begin
      if (ssoBackwards in FActiveSearchOptions) then
        Pt.x := Max(Editor.BlockBegin.X, Editor.BlockEnd.X)
      else
        Pt.x := Min(Editor.BlockBegin.X, Editor.BlockEnd.X);
    end;

  Res := Editor.SearchReplaceEx(FActiveSearchText, ReplaceText, FActiveSearchOptions, Pt);

  if (ssoReplace in FActiveSearchOptions) then
    Res := Editor.SearchReplaceEx(FActiveSearchText, ReplaceText, FActiveSearchOptions - [ssoReplace], Pt);

  if (Res = 0) then
    ShowMessage('"' + FActiveSearchText + '" not found!');
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
var
  S: String;
begin
  FErrorToken := ErrorToken;

  S := Format('Line %d: Unexpected end of comment!', [ErrorToken.LineNum + (FParserStartPoint.Y - 1)]);
  ShowMessage(S);

  Editor.CaretY := ErrorToken.LineNum + (FParserStartPoint.Y - 1);
  Editor.CaretX := ErrorToken.CaretNum + (FParserStartPoint.X - 1);
end;

procedure TEditorPage.SyntaxError(Sender: TObject; ErrorToken: TToken;
  TokenTable: TTokenStack);
var
  T: String;
  i: Integer;
  GP: TGOLDParser;
  S: UTF8String;
begin
  GP := TParser(Sender).GoldParser;
  S := ErrorToken.Name;
  FErrorToken := ErrorToken;

  T := '';

  if TokenTable.Count > 0 then
    begin
      T := '"' + TokenTable[0].Name + '"';
      for i := 1 to TokenTable.Count - 1 do
        T := T + ', ' + '"' + TokenTable[i].Name + '"';
    end;

  if (ErrorToken.ParentSymbol.Kind = SymbolTypeEnd) then
    begin
      ShowMessage('Unexpected end of script!' + LineEnding +
                  'Perhaps a ";" is missing at end of line?');
    end
  else
    begin
      if (Length(S) > 2) and
         (S[1] = 'o') and
         (S[2] = 'p')
      then
        S := 'Line %d: Syntax error at pos %d: %s: Reserved word'
      else
        S := 'Line %d: Syntax error at pos %d: %s';

      ShowMessage(
        Format(S,
               [ErrorToken.LineNum + (FParserStartPoint.Y - 1),
                ErrorToken.CaretNum + (FParserStartPoint.X - 1),
                ErrorToken.DataVar]
              ) + LineEnding +
        'Expected: ' + LineEnding +
        T
      );

      Editor.CaretY := ErrorToken.LineNum + (FParserStartPoint.Y - 1);
      Editor.CaretX := ErrorToken.CaretNum + (FParserStartPoint.X - 1);
    end;
end;
procedure TEditorPage.LexError(Sender: TObject; ErrorToken: TToken);
var
  S: String;
begin
  FErrorToken := ErrorToken;

  S := Format('Line %d: Unrecognised symbol at pos %d - "%s"',
              [ErrorToken.LineNum + (FParserStartPoint.Y - 1),
               ErrorToken.CaretNum + (FParserStartPoint.X - 1),
               ErrorToken.DataVar
              ]
       );
  ShowMessage(S);

  Editor.CaretY := ErrorToken.LineNum + (FParserStartPoint.Y - 1);
  Editor.CaretX := ErrorToken.CaretNum + (FParserStartPoint.X - 1);
end;

procedure TEditorPage.ASTBuildError(Sender: TObject; const Msg: UTF8String;
  ErrorToken: TToken);
begin
  FErrorToken := ErrorToken;

  ShowMessage(Msg);
  Editor.CaretY := ErrorToken.LineNum + (FParserStartPoint.Y - 1);
  Editor.CaretX := ErrorToken.CaretNum + (FParserStartPoint.X - 1);
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

procedure TEditorPage.RunAsync(Data: PtrInt);
var
  Command: TSynEditorCommand;
begin
  Command := TSynEditorCommand(Data);

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

function TEditorPage.GetModified: boolean;
begin
  result := Editor.Modified;
end;

procedure TEditorPage.EditorCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  // If we run the parser here and an error occurs, the editor seems to be in an
  // invalidated state => hence there is no text shown in the UI. If we relay the
  // parsing and execution to the normal event queue then everything works
  if (Command = ecRunAllCommand) or
     (Command = ecRunSelectedCommand)
  then
     Application.QueueAsyncCall(@RunAsync, Command);

  case (Command) of
    ecFindCommand,
    ecFindNextCommand,
    ecFindPrevCommand,
    ecReplaceCommand:
      Application.QueueAsyncCall(@PerformSearchAsync, Command);

    ecInsertN:
      FEditor.InsertTextAtCaret('[_n]');
  end;
end;

procedure TEditorPage.EditorClick(Sender: TObject);
begin
  FErrorToken := nil;
  FEditor.Invalidate;
end;

procedure TEditorPage.EditorSpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  if (Assigned(FErrorToken)) and
     ((FErrorToken.LineNum + (FParserStartPoint.Y - 1)) = Line)
  then
    begin
      Special := true;
      FG := clWhite;
      BG := clRed;
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
  if (Idx >= 0) then
    FOutputCreator.DoCommand('.' + OutputCreatorNormalizeText(FExecutingLines[Idx]));
end;

procedure TEditorPage.FontChangeEvent(Sender: TObject);
begin
  Editor.Font.BeginUpdate;
  Editor.Font.Name := Executor.SetOptions[ANA_SO_EDITOR_FONT_NAME].Value;
  Editor.Font.Size := StrToInt(Executor.SetOptions[ANA_SO_EDITOR_FONT_SIZE].Value);
  Editor.Font.EndUpdate;
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
  FEditor.OnSpecialLineColors := @EditorSpecialLineColors;
  FEditor.OnClick := @EditorClick;
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
  FActiveSearchText := '';
end;

destructor TEditorPage.Destroy;
begin
  if (Assigned(FExecutor)) then
    begin
      FExecutor.RemoveOnBeforeStatementHandler(@BeforeStatementHandler);
      FExecutor.RemoveOnAfterStatementHandler(@AfterStatementHandler);

      FExecutor.SetOptions[ANA_SO_EDITOR_FONT_SIZE].RemoveOnChangeHandler(@FontChangeEvent);
      FExecutor.SetOptions[ANA_SO_EDITOR_FONT_NAME].RemoveOnChangeHandler(@FontChangeEvent);
    end;

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

procedure TEditorPage.UpdateEditorFont(NewFont: TFont);
begin
  if (NewFont.Size <> Editor.Font.Size) then
    DoParse('set "' + ANA_SO_EDITOR_FONT_SIZE + '" := ' + IntToStr(NewFont.Size) + ';');

  if (NewFont.Name <> Editor.Font.Name) then
    DoParse('set "' + ANA_SO_EDITOR_FONT_NAME + '" := "' + NewFont.Name + '";');
end;

procedure TEditorPage.PerformFind();
begin
  StartSearch(GetSearchText(), TFindDialog);
end;

procedure TEditorPage.PerformFindNext();
begin
  FActiveSearchOptions := FActiveSearchOptions - [ssoBackwards];
  InternalSearch(FActiveSearchText);
end;

procedure TEditorPage.PerformFindPrev();
begin
  FActiveSearchOptions := FActiveSearchOptions + [ssoBackwards];
  InternalSearch(FActiveSearchText);
end;

procedure TEditorPage.PerformReplace();
var
  Dlg: TReplaceDialog;
begin
  StartSearch(GetSearchText(), TReplaceDialog);
end;

procedure TEditorPage.PerformCut();
begin
  Editor.ExecuteCommand(ecCut, '', nil);
end;

procedure TEditorPage.PerformCopy();
begin
  Editor.ExecuteCommand(ecCopy, '', nil);
end;

procedure TEditorPage.PerformPaste();
begin
  Editor.ExecuteCommand(ecPaste, '', nil);
end;

procedure TEditorPage.PerformInsertHistory();
begin
  Editor.InsertTextAtCaret(FHistory.Lines.Text);
end;

procedure TEditorPage.PerformInsertSetOptions();
var
  SOM: TSetOptionsMap;
  SO:  TSetOption;
  Iter:  TSetOptionsMap.TIterator;
  S: UTF8String;
  AList: TStringList;
  MaxKeyWidth, MaxValWidth: Integer;
  T: String;
begin
  SOM := Executor.SetOptions;
  AList := TStringList.Create;

  MaxKeyWidth := 0;
  MaxValWidth := 0;
  Iter := SOM.Min;
  repeat
    MaxKeyWidth := Max(MaxKeyWidth, UTF8Length(Iter.Key));
    if Iter.Value.ASTType in [rtInteger, rtFloat] then
      MaxValWidth := Max(MaxValWidth, UTF8Length(Iter.Value.Value))
    else
      MaxValWidth := Max(MaxValWidth, UTF8Length(Iter.Value.Value) + 2);
  until (not Iter.Next);


  Iter := SOM.Min;
  repeat
    S  := Iter.Key;
    SO := Iter.Value;

    S := 'set "' + S + '"' + DupeString(' ', MaxKeyWidth - UTF8Length(S)) + ' := ';

    if SO.ASTType in [rtInteger, rtFloat] then
      S := S + SO.Value + ';'
    else
      S := S + '"' + SO.Value + '";';

    if (SO.LegalValues.Count > 0) or
       (SO.LowRange <> '') or
       (SO.HighRange <> '')
    then
      S := S + DupeString(' ', MaxValWidth - UTF8Length(SO.Value)) + ' // ';


    if (SO.LegalValues.Count > 0) then
      begin
        S := S + ' legal values: ';
        for T in SO.LegalValues do
          S := S + T + ' / ';

        Delete(S, Length(S) - 2, 3)
      end;

    if (SO.LowRange <> '') then
      S := S + ' min = ' + SO.LowRange;

    if (SO.HighRange <> '') then
      S := S + ' max = ' + SO.HighRange;

    AList.Add(S);
  until (not Iter.Next);
  Iter.Free;

  Editor.InsertTextAtCaret(AList.Text);
  AList.Free;
end;

procedure TEditorPage.PerformRunAll();
begin
  Editor.CommandProcessor(ecRunAllCommand, '', nil, []);
end;

procedure TEditorPage.PerformRunSelected();
begin
  Editor.CommandProcessor(ecRunSelectedCommand, '', nil, []);
end;

procedure TEditorPage.PerformUndo();
begin
  Editor.CommandProcessor(ecUndo, '', nil, []);
end;

procedure TEditorPage.PerformRedo();
begin
  Editor.CommandProcessor(ecRedo, '', nil, []);
end;

end.

