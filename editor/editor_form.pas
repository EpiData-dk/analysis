unit editor_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, SynHighlighterAny, Forms,
  Controls, Graphics, Dialogs, ComCtrls, Menus, ExtCtrls, ActnList, executor,
  Token, GOLDParser, history, SynEditMarkupSpecialLine, SynEditMiscClasses,
  SynEditTypes, ast, Types, gmap, LCLType, StdCtrls, outputcreator, SynEditKeyCmds;

type

  { TEditorForm }

  TEditorForm = class(TForm)
    CancelExecutionAction: TAction;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    PopupMenu1: TPopupMenu;
    TutorialSubMenu: TMenuItem;
    MenuItem39: TMenuItem;
    QuitAction: TAction;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    PasteAction: TAction;
    CopyAction: TAction;
    CutAction: TAction;
    InsertHistoryAction: TAction;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    SynCompletion1: TSynCompletion;
    SynAnySyn1: TSynAnySyn;
    ActionList1: TActionList;
    RunAllAction: TAction;
    RunSelectedAction: TAction;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    SaveAction: TAction;
    SaveAsAction: TAction;
    OpenAction: TAction;
    CloseTabAction: TAction;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    NewAction: TAction;
    MenuItem11: TMenuItem;
    TabControl1: TTabControl;
    RecentFilesSubMenu: TMenuItem;
    RecentFilesActionList: TActionList;
    FontDialog1: TFontDialog;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    FontAction: TAction;
    ReplaceDialog1: TReplaceDialog;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    FindDialog1: TFindDialog;
    FindAction: TAction;
    ReplaceAction: TAction;
    FindNextAction: TAction;
    FindPrevAction: TAction;
    InsertSetOptionsAction: TAction;
    MenuItem45: TMenuItem;
    procedure CancelExecutionActionExecute(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure FindPrevActionExecute(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure InsertHistoryActionExecute(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure RunAllActionExecute(Sender: TObject);
    procedure RunSelectedActionExecute(Sender: TObject);
    procedure SynCompletion1CodeCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    procedure SynCompletion1Execute(Sender: TObject);
    procedure SynCompletion1KeyCompletePrefix(Sender: TObject);
    procedure SynCompletion1KeyNextChar(Sender: TObject);
    procedure SynCompletion1KeyPrevChar(Sender: TObject);
    procedure SynCompletion1PositionChanged(Sender: TObject);
    procedure SynEdit1CommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynEdit1SpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SynEdit1Click(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure CloseTabActionExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure TabControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FontActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure ReplaceActionExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure InsertSetOptionsActionExecute(Sender: TObject);
  private
    type
      // Hold to active settings of the editor/text
      TTabRecord = class
        FileName: UTF8String;
        Stream:   TStream;
        Modified: Boolean;
        CaretPos: TPoint;
        TopLine:  Integer;
        Selection: Boolean;
        SelectBegin: TPoint;
        SelectEnd: TPoint;
      end;

    procedure FontChangeEvent(Sender: TObject);
    procedure SearchDlgShow(Sender: TObject);
  private
    function  CloseTab(Index: Integer): boolean;
    function  NewTab: Integer;
    function  TabRecord(Index: integer): TTabRecord;
    function  CurrentTabRecord: TTabRecord;

  private
    procedure UpdateCaption;
    procedure LoadTutorials;
  private
    FExecutor: TExecutor;
    procedure SetExecutor(AValue: TExecutor);
    procedure SetHistory(AValue: THistory);
    procedure SetOutputCreator(AValue: TOutputCreator);
    procedure UpdateFormActions;
    procedure FormChanged(Sender: TObject; Form: TCustomForm);

  private
    FFileName: UTF8String;
    function  DoSavePGM(ForceDialog: boolean): boolean;
    procedure DoOpenPGM(Const filename: UTF8String = ''; ANewTab: boolean = false);

  private
    FOldLineNo: Integer;
    FExecuting: boolean;
    FParserStartPoint: TPoint;
    FHistory: THistory;
    FErrorToken: TToken;
    procedure DoParser(Const S: UTF8String);
    procedure CommentError(Sender: TObject; ErrorToken: TToken);
    procedure LexError(Sender: TObject; ErrorToken: TToken);
    procedure SyntaxError(Sender: TObject; ErrorToken: TToken; TokenTable: TTokenStack);
    procedure ASTBuildError(Sender: TObject; const Msg: UTF8String; ErrorToken: TToken);
    procedure AfterStatementHandler(Statement: TCustomStatement);
    procedure BeforeStatementHandler(Statement: TCustomStatement);

  private
    // Search
    FActiveDialog: TFindDialog;
    FActiveSearchText: UTF8String;
    FActiveSearchOptions: TSynSearchOptions;
    procedure StartSearch(Const SearchText: UTF8String; Dlg: TFindDialog);
    procedure SearchFind(Sender: TObject);
    procedure InternalSearch(Const ReplaceText: UTF8String);

  private
    // Statusbar
    procedure SBUpdateLineCol;
    procedure SBUpdateModified;
    procedure SBUpdateInsert;
    procedure SBUpdateFileName;

  private
    FOutputCreator: TOutputCreator;
    // Recent files
    procedure AsyncOpenRecent(Data: PtrInt);
    procedure OpenRecentMenuItemClick(Sender: TObject);
    procedure BuildRecentFilesActions;
    procedure UpdateRecentFiles;
    procedure UpdateShortCuts;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure  OpenPgm(Const Filename: UTF8String = '');
    class procedure RestoreDefaultPos;
    property Executor: TExecutor read FExecutor write SetExecutor;
    property History: THistory read FHistory write SetHistory;
    property OutputCreator: TOutputCreator read FOutputCreator write SetOutputCreator;
  end;

var
  EditorForm: TEditorForm;

implementation

{$R *.lfm}

uses
  parser, Symbol, LazUTF8, epimiscutils, LazFileUtils, main, ana_procs,
  VirtualTrees, ana_globals, ast_types, epistringutils, LazUTF8Classes,
  strutils, options_hashmap, math;

const
  EDITOR_COMMAND_OUTPUT = 'EDITOR_COMMAND_OUTPUT';

{ TEditorForm }

procedure TEditorForm.RunAllActionExecute(Sender: TObject);
begin
  FParserStartPoint := Point(1, 1);
  DoParser(SynEdit1.Text);
end;

procedure TEditorForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Res: TModalResult;
  i: Integer;
begin
  CanClose := true;
  for i := TabControl1.Tabs.Count - 1 downto 0 do
    begin
      CanClose := CloseTab(I);
      if (not CanClose) then
        Exit;
    end;

  if CanClose then
    SaveFormPosition(Self, 'Editor');
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
  SBUpdateFileName;
  OpenDialog1.InitialDir := GetCurrentDirUTF8;
  SaveDialog1.InitialDir := GetCurrentDirUTF8;

  CopyAction.ShortCut  := SynEdit1.Keystrokes[SynEdit1.Keystrokes.FindCommand(ecCopy)].ShortCut;
  CutAction.ShortCut   := SynEdit1.Keystrokes[SynEdit1.Keystrokes.FindCommand(ecCut)].ShortCut;
  PasteAction.ShortCut := SynEdit1.Keystrokes[SynEdit1.Keystrokes.FindCommand(ecPaste)].ShortCut;

  LoadFormPosition(Self, Self.ClassName);

  UpdateRecentFiles;
  UpdateShortCuts;
  LoadTutorials;
  FontChangeEvent(nil);
end;

procedure TEditorForm.CutActionExecute(Sender: TObject);
begin
  SynEdit1.CutToClipboard;
end;

procedure TEditorForm.CancelExecutionActionExecute(Sender: TObject);
begin
  Executor.Cancelled := true;
end;

procedure TEditorForm.CopyActionExecute(Sender: TObject);
begin
  SynEdit1.CopyToClipboard;
end;

procedure TEditorForm.FindNextActionExecute(Sender: TObject);
begin
  FActiveSearchOptions := FActiveSearchOptions - [ssoBackwards];
  InternalSearch(FActiveSearchText);
end;

procedure TEditorForm.FindPrevActionExecute(Sender: TObject);
begin
  FActiveSearchOptions := FActiveSearchOptions + [ssoBackwards];
  InternalSearch(FActiveSearchText);
end;

procedure TEditorForm.QuitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TEditorForm.InsertHistoryActionExecute(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret(FHistory.Lines.Text);
end;

procedure TEditorForm.PasteActionExecute(Sender: TObject);
begin
  SynEdit1.PasteFromClipboard;
end;

procedure TEditorForm.RunSelectedActionExecute(Sender: TObject);
var
  S: String;
begin
  if SynEdit1.SelAvail then
    begin
      FParserStartPoint := SynEdit1.BlockBegin; //SynEdit1.CharIndexToRowCol(Synedit1.SelStart);
      DoParser(SynEdit1.SelText)
    end
  else
    begin
      FParserStartPoint := Point(1, SynEdit1.CaretY);
      DoParser(SynEdit1.Lines[SynEdit1.CaretY - 1]);
    end;
end;

procedure TEditorForm.SynCompletion1CodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  //
end;

procedure TEditorForm.SynCompletion1Execute(Sender: TObject);
var
  S: String;
begin
  S := SynCompletion1.CurrentString;
//  StatusBar1.Panels[3].Text := 'S: ' + S;
{  SynCompletion1.ItemList.Clear;
  SynCompletion1.ItemList.Add('Test');
  SynCompletion1.ItemList.Add('Tast');}
end;

procedure TEditorForm.SynCompletion1KeyCompletePrefix(Sender: TObject);
begin
  //
end;

procedure TEditorForm.SynCompletion1KeyNextChar(Sender: TObject);
var
  S: String;
begin
  S := SynCompletion1.CurrentString;
//  StatusBar1.Panels[3].Text := 'S: ' + S;
end;

procedure TEditorForm.SynCompletion1KeyPrevChar(Sender: TObject);
var
  S: String;
begin
  S := SynCompletion1.CurrentString;
//  StatusBar1.Panels[3].Text := 'S: ' + S;
end;

procedure TEditorForm.SynCompletion1PositionChanged(Sender: TObject);
begin
  //
end;

procedure TEditorForm.SynEdit1CommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  if (Command <> ecUserDefinedFirst) then exit;
  SynEdit1.InsertTextAtCaret('[_n]');
end;

procedure TEditorForm.SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
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

procedure TEditorForm.SynEdit1SpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
begin
{  if (Assigned(FErrorToken)) and
     (FErrorToken.LineNum = Line)
  then
    begin
      Special := true;

      Markup.SetFrameBoundsLog(FErrorToken.CaretNum, FErrorToken.CaretNum + UTF8Length(FErrorToken.DataVar));
      Markup.Foreground := clWhite;
      Markup.Background := clRed;
    end;   }
end;

procedure TEditorForm.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  Change: TSynStatusChange;
begin
  for Change in Changes do
    case Change of
      scCaretX:
        SBUpdateLineCol;

      scCaretY:
        SBUpdateLineCol;

      scInsertMode:
        SBUpdateInsert;

      scModified:
        SBUpdateModified;
    end;
end;

procedure TEditorForm.SynEdit1Click(Sender: TObject);
begin
  FErrorToken := nil;
  SynEdit1.Invalidate;
end;

procedure TEditorForm.SaveActionExecute(Sender: TObject);
begin
  DoSavePGM(false);
  SBUpdateFileName;
end;

procedure TEditorForm.SaveAsActionExecute(Sender: TObject);
begin
  DoSavePGM(True);
  SBUpdateFileName;
end;

procedure TEditorForm.CloseTabActionExecute(Sender: TObject);
begin
  CloseTab(TabControl1.TabIndex);

  if (TabControl1.Tabs.Count = 0) then
    NewTab;
end;

procedure TEditorForm.OpenActionExecute(Sender: TObject);
begin
  DoOpenPGM;
  SBUpdateFileName;
end;

procedure TEditorForm.NewActionExecute(Sender: TObject);
begin
  NewTab;
end;

procedure TEditorForm.TabControl1Change(Sender: TObject);
var
  PR: TTabRecord;
begin
  PR := CurrentTabRecord;

  PR.Stream.Position := 0;
  SynEdit1.Lines.LoadFromStream(Pr.Stream);
  SynEdit1.Modified := PR.Modified;
  FFileName         := PR.FileName;
  SynEdit1.CaretXY  := PR.CaretPos;
  SynEdit1.TopLine  := PR.TopLine;
  if (PR.Selection) then
    begin
      SynEdit1.BlockBegin := PR.SelectBegin;
      SynEdit1.BlockEnd   := PR.SelectEnd;
    end;

  SBUpdateFileName;
end;

procedure TEditorForm.TabControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
var
  PR: TTabRecord;
begin
  PR := CurrentTabRecord;

  PR.Stream.Position := 0;
  SynEdit1.Lines.SaveToStream(Pr.Stream);
  PR.Modified := SynEdit1.Modified;
  PR.FileName := FFileName;
  PR.CaretPos := SynEdit1.CaretXY;
  PR.TopLine  := SynEdit1.TopLine;
  PR.Selection := SynEdit1.SelAvail;
  PR.SelectBegin := SynEdit1.BlockBegin;
  PR.SelectEnd   := SynEdit1.BlockEnd;
end;

procedure TEditorForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveFormPosition(Self, Self.ClassName);
  case CloseAction of
    caNone: ;

    caHide,
    caFree:
      Application.ReleaseComponent(Self);

    caMinimize: ;
  end;
end;

procedure TEditorForm.FontActionExecute(Sender: TObject);
begin
  FontDialog1.Font.Assign(SynEdit1.Font);
  if FontDialog1.Execute then
    begin
      if (FontDialog1.Font.Size <> SynEdit1.Font.Size) then
        DoParser('set "' + ANA_SO_EDITOR_FONT_SIZE + '" := ' + IntToStr(FontDialog1.Font.Size) + ';');

      if (FontDialog1.Font.Name <> SynEdit1.Font.Name) then
        DoParser('set "' + ANA_SO_EDITOR_FONT_NAME + '" := "' + FontDialog1.Font.Name + '";');
    end;
end;

procedure TEditorForm.SearchFind(Sender: TObject);
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

procedure TEditorForm.InternalSearch(const ReplaceText: UTF8String);
var
  Res: Integer;
  Pt: TPoint;
begin
  Pt := SynEdit1.CaretXY;

  if (SynEdit1.SelAvail) and
     (SynEdit1.SelText = FActiveSearchText) and
     ([ssoReplace, ssoReplaceAll] * FActiveSearchOptions <> [])
  then
    begin
      if (ssoBackwards in FActiveSearchOptions) then
        Pt.x := Max(SynEdit1.BlockBegin.X, SynEdit1.BlockEnd.X)
      else
        Pt.x := Min(SynEdit1.BlockBegin.X, SynEdit1.BlockEnd.X);
    end;

  Res := SynEdit1.SearchReplaceEx(FActiveSearchText, ReplaceText, FActiveSearchOptions, Pt);

  if (ssoReplace in FActiveSearchOptions) then
    Res := SynEdit1.SearchReplaceEx(FActiveSearchText, ReplaceText, FActiveSearchOptions - [ssoReplace], Pt);


  if (res = 0) then
    ShowMessage('"' + FActiveSearchText + '" not found!');
end;

procedure TEditorForm.FindActionExecute(Sender: TObject);
var
  S: String;
begin
  S := '';
  if SynEdit1.SelAvail then
    S := SynEdit1.SelText;

  StartSearch(S, FindDialog1);
end;

procedure TEditorForm.ReplaceActionExecute(Sender: TObject);
var
  S: String;
begin
  S := '';
  if SynEdit1.SelAvail then
    S := SynEdit1.SelText;

  StartSearch(S, ReplaceDialog1);
end;

procedure TEditorForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  S: String;
begin
  for S in FileNames do
    DoOpenPGM(S, true);
end;

procedure TEditorForm.InsertSetOptionsActionExecute(Sender: TObject);
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

        Delete(S, Length(S) - 2, 3);
      end;

    if (SO.LowRange <> '') then
      S := S + ' min = ' + SO.LowRange;

    if (SO.HighRange <> '') then
      S := S + ' max = ' + SO.HighRange;

    AList.Add(S);
  until (not Iter.Next);
  Iter.Free;

  SynEdit1.InsertTextAtCaret(AList.Text);
  AList.Free;
end;

procedure TEditorForm.FontChangeEvent(Sender: TObject);
begin
  SynEdit1.Font.BeginUpdate;
  SynEdit1.Font.Name := Executor.SetOptions[ANA_SO_EDITOR_FONT_NAME].Value;
  SynEdit1.Font.Size := StrToInt(Executor.SetOptions[ANA_SO_EDITOR_FONT_SIZE].Value);
  SynEdit1.Font.EndUpdate;
end;

procedure TEditorForm.SearchDlgShow(Sender: TObject);
var
  P: TPoint;
  MfBound: TRect;
begin
  MfBound   := BoundsRect;
  P.Y := MfBound.Top + (((MfBound.Bottom - MfBound.Top)  - FActiveDialog.Height) Div 2);
  P.X := MfBound.Left + (((MfBound.Right - MfBound.Left) - FActiveDialog.Width) Div 2);

  FActiveDialog.Position := P;
end;

function TEditorForm.CloseTab(Index: Integer): boolean;
var
  Res: TModalResult;
  Pr: TTabRecord;
  OldIndex: Integer;
begin
  result := false;

  OldIndex := TabControl1.TabIndex;
  TabControl1.TabIndex := Index;

  // For some odd reason OnChange is not fired
  if OldIndex <> Index then
    TabControl1Change(TabControl1);

  if SynEdit1.Modified then
    begin
      if (Self.CanFocus) then
        Self.SetFocus;

      Res := MessageDlg('Warning',
                        'Editor Content (EpiData Analysis pgm) not saved. Save before closing ?',
                        mtWarning,
                        mbYesNoCancel, 0
             );

      case Res of
        mrYes:
          if (not DoSavePGM(false)) then
            Exit;

        mrCancel:
          Exit;

        mrNo:
          ;
      end;
    end;

  Pr := CurrentTabRecord;
  TabControl1.Tabs.Delete(Index);

  PR.Stream.Free;
  Pr.Free;
//  SynEdit1.Modified := false;

  {$IFNDEF LINUX}
  if (Index >= TabControl1.Tabs.Count) then
    Index := TabControl1.Tabs.Count - 1;

  if (Index >= 0) and (Index < TabControl1.Tabs.Count) then
    begin
     TabControl1.TabIndex := Index;
      TabControl1Change(TabControl1);
    end;
  {$ENDIF}

  result := true
end;

function TEditorForm.NewTab: Integer;
var
  PR: TTabRecord;
  O: TControl;
begin
  result := TabControl1.Tabs.Add('Untitled');

  O := TControl(TabControl1.Tabs.Objects[result]);

  PR := TTabRecord.Create;
  PR.Stream := TMemoryStream.Create;
  PR.FileName := '';
  PR.Modified := false;

  O.Tag := PtrInt(PR);
  TabControl1.TabIndex := result;

  // For some odd reason OnChange is not fired (on linux at least)
  TabControl1Change(TabControl1);
end;

function TEditorForm.TabRecord(Index: integer): TTabRecord;
begin
  result := TTabRecord(TControl(TabControl1.Tabs.Objects[Index]).Tag);
end;

function TEditorForm.CurrentTabRecord: TTabRecord;
begin
  result := TabRecord(TabControl1.TabIndex);
end;

procedure TEditorForm.UpdateCaption;
var
  S: String;
begin
  if FFileName = '' then
    S := 'Untitled'
  else
    S := ExtractFileNameOnly(FFileName);

  if SynEdit1.Modified then
    S := '*' + S;

  TabControl1.Tabs[TabControl1.TabIndex] := S;
end;

procedure TEditorForm.LoadTutorials;
var
  FileList: TStringList;
  MenuItem: TMenuItem;
  i: Integer;
  P: String;
begin
  // First delete all previous tutorials.. (could be a change in tutorial dir).
  for i := TutorialSubMenu.Count - 1 downto 0 do
  begin
    MenuItem := TutorialSubMenu[i];
    TutorialSubMenu.Delete(i);
    MenuItem.Free;
  end;

  // Find all .pdf files in the directory set by TutorialsDirUTF8
  FileList := TStringListUTF8.Create;
  {$IFDEF DARWIN}
  P := ProgramDirectory + '../../../docs';
  {$ELSE}
  P := ProgramDirectory + DirectorySeparator + 'docs';
  {$ENDIF}
  FindAllFiles(FileList, P, '*.pdf', false);
  FindAllFiles(FileList, P, '*.html', false);
  FileList.CustomSort(@EpiStringListSortStr);

  if FileList.Count = 0 then
  begin
    TutorialSubMenu.Enabled := false;
    FileList.Free;
    Exit;
  end;

  for i := 0 to FileList.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(TutorialSubMenu);
    MenuItem.Name := 'TutorialMenuItem' + IntToStr(i);
    MenuItem.Caption := ExtractFileNameOnly(FileList[i]);
    MenuItem.OnClick := @MainForm.OpenTutorialMenuItemClick;

    TutorialSubMenu.Add(MenuItem);
  end;
  FileList.Free;
end;

procedure TEditorForm.SetExecutor(AValue: TExecutor);
begin
  if FExecutor = AValue then Exit;
  FExecutor := AValue;

  FExecutor.AddOnBeforeStatementHandler(@BeforeStatementHandler);
  FExecutor.AddOnAfterStatementHandler(@AfterStatementHandler);

  FExecutor.SetOptions[ANA_SO_EDITOR_FONT_SIZE].AddOnChangeHandler(@FontChangeEvent);
  FExecutor.SetOptions[ANA_SO_EDITOR_FONT_NAME].AddOnChangeHandler(@FontChangeEvent);

  UpdateFormActions;
end;

procedure TEditorForm.SetHistory(AValue: THistory);
begin
  if FHistory = AValue then Exit;
  FHistory := AValue;

  UpdateFormActions;
end;

procedure TEditorForm.SetOutputCreator(AValue: TOutputCreator);
begin
  if AValue = FOutputCreator then exit;
  FOutputCreator := AValue;
end;

procedure TEditorForm.UpdateFormActions;
begin
  RunAllAction.Enabled      := Assigned(FExecutor);
  RunSelectedAction.Enabled := Assigned(FExecutor);
end;

procedure TEditorForm.FormChanged(Sender: TObject; Form: TCustomForm);
begin
  if (Form = Self) then
    begin
      ActionList1.State := asNormal;
      RecentFilesActionList.State := asNormal;
    end
  else
    begin
      ActionList1.State := asSuspended;
      RecentFilesActionList.State := asSuspended;
    end;
end;

function TEditorForm.DoSavePGM(ForceDialog: boolean): boolean;
begin
  result := false;

  if ((ForceDialog) or (FFileName = '')) then
  begin
    SaveDialog1.FileName := FFileName;
    if (not SaveDialog1.Execute) then
      Exit
    else
      FFileName := SaveDialog1.FileName;
  end;

  try
    SynEdit1.Lines.SaveToFile(FFileName);
    SynEdit1.Modified := false;

    AddToRecent(FFileName, GetRecentPGMIniFileName, RecentPGMFiles);
    UpdateRecentFiles;
    result := true;
  except
    MessageDlg(
      'Error',
      'The file was not saved correctly. Try saving under a different filename!',
      mtError,
      [mbOK],
      0,
      mbOK
    );
  end;
end;

procedure TEditorForm.DoOpenPGM(const filename: UTF8String; ANewTab: boolean);
var
  S: String;
  L: TStrings;
begin
  if (not ANewTab) and
     (not CloseTab(TabControl1.TabIndex))
  then
    Exit;

  OpenDialog1.InitialDir := GetCurrentDirUTF8;
  if (filename = '') and
     (not OpenDialog1.Execute)
  then
    begin
      NewTab;
      UpdateCaption;
      Exit;
    end;

  if (filename <> '') then
    begin
      L := TStringList.Create;
      L.Add(filename);
    end
  else
    L := OpenDialog1.Files;

  for S in L do
    begin
      if (not FileExistsUTF8(S)) then
        begin
          MessageDlg('Error',
            'File not found: ' + S,
            mtError,
            [mbOK],
            0,
            mbOK
          );
          NewTab;
          Continue;
        end;

      NewTab;

      FFileName := S;
      SynEdit1.Lines.LoadFromFile(FFileName);
      SynEdit1.Modified := false;

      AddToRecent(FFileName, GetRecentPGMIniFileName, RecentPGMFiles);
      UpdateCaption;
    end;

  UpdateRecentFiles;

  if (filename <> '') then
    L.Free;
end;

procedure TEditorForm.DoParser(const S: UTF8String);
var
  P: TParser;
  Prgm: TStatementList;
begin
  FErrorToken := nil;
  SynEdit1.Invalidate;

  P := TParser.Create(FExecutor);
  P.OnCommentError := @CommentError;
  P.OnLexError     := @LexError;
  P.OnSyntaxError  := @SyntaxError;
  P.OnASTBuildError := @ASTBuildError;

  if P.ParseText(S, Prgm) then
    begin
      FOldLineNo := FHistory.Lines.Count;
      FHistory.AddLines(S);
      FExecuting := true;
      FExecutor.Execute(Prgm);
      FExecuting := false;
    end;
  P.Free;
end;

procedure TEditorForm.CommentError(Sender: TObject; ErrorToken: TToken);
var
  S: String;
begin
  FErrorToken := ErrorToken;

  S := Format('Line %d: Unexpected end of comment!', [ErrorToken.LineNum]);
  ShowMessage(S);

  SynEdit1.CaretY := ErrorToken.LineNum + (FParserStartPoint.Y - 1);
  SynEdit1.CaretX := ErrorToken.CaretNum + (FParserStartPoint.X - 1);
end;

procedure TEditorForm.LexError(Sender: TObject; ErrorToken: TToken);
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

  SynEdit1.CaretY := ErrorToken.LineNum + (FParserStartPoint.Y - 1);
  SynEdit1.CaretX := ErrorToken.CaretNum + (FParserStartPoint.X - 1);
end;

procedure TEditorForm.SyntaxError(Sender: TObject; ErrorToken: TToken;
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
        S := 'Line %d: Syntax error at pos %d  (%s: Reserved word)'
      else
        S := 'Line %d: Syntax error at pos %d  (%s)';

      ShowMessage(
        Format(S,
               [ErrorToken.LineNum + (FParserStartPoint.Y - 1),
                ErrorToken.CaretNum + (FParserStartPoint.X - 1),
                ErrorToken.DataVar]
              ) + LineEnding +
        'Expected tokens: ' + LineEnding +
        T
      );

      SynEdit1.CaretY := ErrorToken.LineNum + (FParserStartPoint.Y - 1);
      SynEdit1.CaretX := ErrorToken.CaretNum + (FParserStartPoint.X - 1);
    end;
end;

procedure TEditorForm.ASTBuildError(Sender: TObject; const Msg: UTF8String;
  ErrorToken: TToken);
begin
  FErrorToken := ErrorToken;

  ShowMessage(Msg);
  SynEdit1.CaretY := ErrorToken.LineNum + (FParserStartPoint.Y - 1);
  SynEdit1.CaretX := ErrorToken.CaretNum + (FParserStartPoint.X - 1);
end;

procedure TEditorForm.AfterStatementHandler(Statement: TCustomStatement);
begin
  if (not FExecuting) then exit;

  if (Statement.ExecResult <> csrSuccess) and
     (Executor.Cancelled)
  then
    begin
      FErrorToken := TToken.Create(Statement.LineNo, Statement.ColNo, Statement.ByteNo);

      SynEdit1.CaretY := FErrorToken.LineNum + (FParserStartPoint.Y - 1);
      SynEdit1.CaretX := FErrorToken.CaretNum + (FParserStartPoint.X - 1);
      SynEdit1.Invalidate;
    end;
end;

procedure TEditorForm.BeforeStatementHandler(Statement: TCustomStatement);
var
  Idx: Integer;
begin
  if (not FExecuting) then exit;
  if Assigned(Statement.FindCustomData(EDITOR_COMMAND_OUTPUT)) then exit;

  Statement.AddCustomData(EDITOR_COMMAND_OUTPUT, TObject(1));

  Idx := FOldLineNo + Statement.LineNo - 1;
  FOutputCreator.DoCommand('.' + StringsReplace(FHistory.Lines[Idx], ['{','}'], ['{{','}}'], [rfReplaceAll]));
end;

procedure TEditorForm.StartSearch(const SearchText: UTF8String; Dlg: TFindDialog
  );
begin
  if (Assigned(FActiveDialog)) and
     (FActiveDialog <> Dlg)
  then
    FActiveDialog.CloseDialog;

  FActiveDialog := Dlg;
  FActiveDialog.FindText := SearchText;
  FActiveDialog.OnFind := @SearchFind;
  FActiveDialog.OnShow := @SearchDlgShow;
  if (FActiveDialog is TReplaceDialog) then TReplaceDialog(FActiveDialog).OnReplace := @SearchFind;
  FActiveDialog.Execute;
end;

procedure TEditorForm.SBUpdateLineCol;
begin
  StatusBar1.Panels[0].Text := Format('%d: %d', [SynEdit1.CaretY, SynEdit1.CaretX]);
end;

procedure TEditorForm.SBUpdateModified;
begin
  if SynEdit1.Modified then
    StatusBar1.Panels[1].Text := 'Modified'
  else
    StatusBar1.Panels[1].Text := '';

  UpdateCaption;
end;

procedure TEditorForm.SBUpdateInsert;
begin
  if SynEdit1.InsertMode then
    StatusBar1.Panels[2].Text := 'Ins'
  else
    StatusBar1.Panels[2].Text := 'Ovr';
end;

procedure TEditorForm.SBUpdateFileName;
begin
  UpdateCaption;
  StatusBar1.Panels[3].Text := FFileName;
end;

procedure TEditorForm.AsyncOpenRecent(Data: PtrInt);
begin
  DoOpenPGM(TAction(Data).Caption, true);
end;

procedure TEditorForm.OpenRecentMenuItemClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@AsyncOpenRecent, PtrInt(Sender));
end;

procedure TEditorForm.BuildRecentFilesActions;
var
  i: Integer;
  A: TAction;
begin
  // Recent files actions
  for i := 1 to MaxRecentFiles do
  begin
    A := TAction.Create(RecentFilesActionList);
    A.ShortCut := KeyToShortCut(VK_1 + (i - 1), [ssShift, ssCtrlOS]);
    A.Enabled  := false;
    A.OnExecute := @OpenRecentMenuItemClick;
    A.ActionList := RecentFilesActionList;
  end;
end;

procedure TEditorForm.UpdateRecentFiles;
var
  i: Integer;
  A: TAction;
  Mi: TMenuItem;
begin
  LoadRecentFilesFromIni(GetRecentPGMIniFileName, RecentPGMFiles);

  RecentFilesSubMenu.Visible := RecentPGMFiles.Count > 0;
  RecentFilesSubMenu.Clear;

  for i := 0 to MaxRecentFiles - 1 do
  begin
    // Main menu
    A := TAction(RecentFilesActionList.Actions[i]);

    // Disable actions if the list of RecentPGMFiles is not long enough.
    if i >= RecentPGMFiles.Count then
    begin
      A.Enabled := false;
      Continue;
    end;

    A.Enabled := true;
    A.Caption := RecentPGMFiles[i];

    Mi := TMenuItem.Create(RecentFilesSubMenu);
    Mi.Name := 'recent' + inttostr(i);
    Mi.Action := A;
    RecentFilesSubMenu.Add(Mi);
  end;
end;

procedure TEditorForm.UpdateShortCuts;
begin
  QuitAction.ShortCut :=
    {$IFDEF linux}  KeyToShortCut(VK_Q, [ssCtrl]){$ENDIF}
    {$IFDEF darwin} KeyToShortCut(VK_Q, [ssMeta]){$ENDIF}
    {$IFDEF windows}KeyToShortCut(VK_F4, [ssAlt]){$ENDIF}
end;

constructor TEditorForm.Create(TheOwner: TComponent);
var
  KS: TSynEditKeyStroke;
begin
  inherited Create(TheOwner);

  BuildRecentFilesActions;
  NewTab;

  Screen.AddHandlerActiveFormChanged(@FormChanged);
  UpdateFormActions;
  SBUpdateInsert;
  SBUpdateLineCol;
  SBUpdateModified;

  SaveDialog1.Filter := GetEpiDialogFilter([dfPGM]);
  OpenDialog1.Filter := GetEpiDialogFilter([dfPGM, dfAll]);

  FExecuting := false;
end;

destructor TEditorForm.Destroy;
begin
  FExecutor.RemoveOnBeforeStatementHandler(@BeforeStatementHandler);
  FExecutor.RemoveOnAfterStatementHandler(@AfterStatementHandler);

  Screen.RemoveHandlerActiveFormChanged(@FormChanged);

  EditorForm := nil;
  inherited Destroy;
end;

procedure TEditorForm.OpenPgm(const Filename: UTF8String);
begin
  DoOpenPGM(Filename);
end;

class procedure TEditorForm.RestoreDefaultPos;
var
  AForm: TForm;
begin
  if (Assigned(EditorForm)) then
    AForm := EditorForm
  else
    AForm := TForm.Create(nil);

  Aform.Width := 500;
  Aform.Height := 500;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;

  SaveFormPosition(Aform, TEditorForm.ClassName);
  if (AForm <> EditorForm) then
    AForm.free;
end;

end.

