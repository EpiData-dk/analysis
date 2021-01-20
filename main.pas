unit main;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fgl, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ActnList, Menus, ExtCtrls, ComCtrls, HtmlView, VirtualTrees,
  SynEdit, Token, GOLDParser, executor, ast, outputcreator, epiv_datamodule,
  epidatafiles, outputgenerator_base, history, cmdedit, options_hashmap,
  epiv_projecttreeview_frame, epicustombase, analysis_statusbar, epidocument,
  epiopenfile, outputviewer_types, commandtree, history_form, varnames_form,
  projecttree_form, commandtree_form, stat_dialog_contribution, stat_dialog_action,
  script_runner, stat_dialog,
  {$IFDEF EPI_CHROMIUM_HTML}
  htmlviewer, htmlviewer_osr,
  {$ENDIF}
  textviewer,
  oldhtmlviewer;


type

  { TMainForm }

  TMainForm = class(TForm, IScriptMediator)
    AlwaysAvailableActionList: TActionList;
    CloseAllWindowsAction: TAction;
    ColorDialog1: TColorDialog;
    HistoryListBox: TListBox;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    StatisticsMainMenuItem: TMenuItem;
    OutputBGColourMenuItem: TMenuItem;
    OutputFontColourMenuItem: TMenuItem;
    CMDBGColourMenuItem: TMenuItem;
    CMDFontColourMenuItem: TMenuItem;
    SaveOutputAction: TAction;
    Label3: TLabel;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem39: TMenuItem;
    OutputFontMenuItem: TMenuItem;
    CommandLineFontMenuItem: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    LeftPanelSplitter: TSplitter;
    TutorialsWebAction: TAction;
    TutorialsWikiAction: TAction;
    ShowEditorStartupAction: TAction;
    ReadCBAction: TAction;
    DefaultWindowPositionAction: TAction;
    CancelExecAction: TAction;
    CopySelectedHistoryAction: TAction;
    CopyAllHistoryAction: TAction;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem21: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    TutorialSubMenu: TMenuItem;
    QuitAction: TAction;
    CmdEditFocusAction: TAction;
    CopyVersionInfoAction: TAction;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem8: TMenuItem;
    HistoryPopupMenu: TPopupMenu;
    LeftSidePanel: TPanel;
    ShowAboutAction: TAction;
    ActionList1: TActionList;
    Button2: TButton;
    FontDialog1: TFontDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel2: TPanel;
    LeftSideSplitter: TSplitter;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    RightSidePanel: TPanel;
    RightPanelSplitter: TSplitter;
    ToggleCmdTreeAction: TAction;
    ToggleVarnamesListAction: TAction;
    ToggleHistoryListAction: TAction;
    RightSideSplitter: TSplitter;
    VarnamesList: TVirtualStringTree;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    ReadAction: TAction;
    SaveAction: TAction;
    CloseAction: TAction;
    MenuItem17: TMenuItem;
    ClearOutputAction: TAction;
    ToggleProjectTree: TAction;
    MenuItem18: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem19: TMenuItem;
    ChangeDirAction: TAction;
    MenuItem20: TMenuItem;
    BrowseAction: TAction;
    MenuItem22: TMenuItem;
    ShowEditorAction: TAction;
    MenuItem27: TMenuItem;
    CommandsHelpAction: TAction;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    ClearHistoryAction: TAction;
    RecentFilesSubMenu: TMenuItem;
    RecentFilesActionList: TActionList;
    PageControl1: TPageControl;
    MenuItem4: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem38: TMenuItem;
    CheckVersionOnlineAction: TAction;
    ReleaseNotesAction: TAction;
    ShowShortcutAction: TAction;
    procedure Button2Click(Sender: TObject);
    procedure CancelExecActionExecute(Sender: TObject);
    procedure CloseAllWindowsActionExecute(Sender: TObject);
    procedure CMDBGColourMenuItemClick(Sender: TObject);
    procedure CmdEditFocusActionExecute(Sender: TObject);
    procedure CMDFontColourMenuItemClick(Sender: TObject);
    procedure CopyAllHistoryActionExecute(Sender: TObject);
    procedure CopySelectedHistoryActionExecute(Sender: TObject);
    procedure CopyVersionInfoActionExecute(Sender: TObject);
    procedure DefaultWindowPositionActionExecute(Sender: TObject);
    procedure OutputBGColourMenuItemClick(Sender: TObject);
    procedure OutputFontColourMenuItemClick(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HistoryListBoxDblClick(Sender: TObject);
    procedure HistoryListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FontChangeClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ReadCBActionExecute(Sender: TObject);
    procedure SaveOutputActionExecute(Sender: TObject);
    procedure ShowAboutActionExecute(Sender: TObject);
    procedure ShowEditorStartupActionExecute(Sender: TObject);
    procedure ToggleCmdTreeActionExecute(Sender: TObject);
    procedure ToggleVarnamesListActionExecute(Sender: TObject);
    procedure ToggleHistoryListActionExecute(Sender: TObject);
    procedure ReadActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure ClearOutputActionExecute(Sender: TObject);
    procedure ChangeDirActionExecute(Sender: TObject);
    procedure BrowseActionExecute(Sender: TObject);
    procedure ShowEditorActionExecute(Sender: TObject);
    procedure HistoryListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CommandsHelpActionExecute(Sender: TObject);
    procedure ToggleProjectTreeExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ClearHistoryActionExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure TutorialsWebActionExecute(Sender: TObject);
    procedure TutorialsWikiActionExecute(Sender: TObject);
    procedure OpenTutorialMenuItemClick(Sender: TObject);
    procedure CheckVersionOnlineActionExecute(Sender: TObject);
    procedure ReleaseNotesActionExecute(Sender: TObject);
    procedure ShowShortcutActionExecute(Sender: TObject);
  { Other internals }
  private
    Executor: TExecutor;
    FCommandTreeForm: TCommandTreeForm;
    procedure CloseWindows(CloseAll: boolean = false);
    procedure CommandTreeCommandDoubleClick(const CommandString: UTF8String);
    procedure CommandTreeFormLineAction(Sender: TObject;
      const LineText: UTF8String; ChangeFocus: boolean);
    procedure CommandTreePressEnterKey(const CommandString: UTF8String);
    procedure HistoryWindowClearHistory(Sender: TObject);
    procedure HistoryWindowLineAction(Sender: TObject; LineText: UTF8String);
    procedure LeftPanelChange(Sender: TObject);
    procedure ProjectTreeFormLineAction(Sender: TObject;
      const LineText: UTF8String);
    procedure RightPanelChange(Sender: TObject);
    procedure ShowEditor(Const Filename: UTF8String = '');
    procedure DoUpdateTitle;
    procedure DialogFilenameHack(const S: string);
    procedure ExecutorAfterStatement(Statement: TCustomStatement);
    procedure ExecutorStart(Sender: TObject);
    procedure ExecutorEnd(Sender: TObject);
    procedure SelectErrorText(ErrorToken: TToken);
    function  DoParseContent(Const S: UTF8String): boolean;
    procedure CommentError(Sender: TObject; ErrorToken: TToken);
    procedure LexError(Sender: TObject; ErrorToken: TToken);
    procedure SyntaxError(Sender: TObject; ErrorToken: TToken; TokenTable: TTokenStack);
    procedure ASTBuildError(Sender: TObject; const Msg: UTF8String; ErrorToken: TToken);
    procedure FormChanged(Sender: TObject; Form: TCustomForm);
    procedure UpdateSetOptions;
    procedure UpdateShortCuts;
    procedure ChangeWidth(Data: PtrInt);
    procedure LoadTutorials;
    procedure GUIInteraction(Sender: TExecutor; GUIAction: TGUIAction);
    procedure PrimaryKeyHandler(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OutputFontChange(Sender: TObject);
    procedure ProjectGetText(Sender: TObject; const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; const StaticText: boolean;
      var NodeText: string);
    procedure MainExceptionHandler(Sender: TObject; E: Exception);
    procedure CmdEditFontChangeEvent(Sender: TObject);
    procedure DelayCmdEditFocus(Data: PtrInt);
    procedure ApplicationActivate(Sender: TObject);
    procedure TutorialChange(Sender: TObject);

  { Variable List }
  private
    FVarnamesWindow: TVariablesForm;
    procedure VarnamesWindowLineAction(Sender: TObject;
      const LineText: UTF8String; ChangeFocus: boolean);
    procedure VarnamesListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VarnamesListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VarnamesListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VarnamesListNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure VarnamesListAfterGetMaxColumnWidth(Sender: TVTHeader;
      Column: TColumnIndex; var MaxWidth: Integer);

  { History / CmdEdit }
  private
    FHistoryWindow: THistoryForm;
    FHistory: THistory;
    FCmdEdit: TCmdEdit;
    function  CmdEditRunCommand(Sender: TObject; const S: UTF8String): boolean;
  protected
    procedure RunScript(Script: UTF8String);
    procedure PasteScript(Script: UTF8String);
  public
    procedure InterfaceRunCommand(const S: UTF8String);


  { LeftSidePanel }
  private
    // Project Tree
    FProjectTreeForm: TProjectTreeForm;
    FProjectTree: TEpiVProjectTreeViewFrame;
    procedure ProjectTreeHint(Sender: TObject; const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; var HintText: string);
    procedure ProjectTreeDoubleClick(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
  private
    // Command Tree
    FCommandTree: TCommandTree;
  { Sidebar }
  private
    type
      TToggleMode = (tmToggle, tmForceOpen, tmForceClose);
    procedure DisplayForm(AForm: TCustomForm; ToggleMode: TToggleMode = tmForceOpen);
    procedure ToggleSidebar(ToggleItem, Sibling: TWinControl; MidSplitter, MainSplitter: TSplitter; ToggleMode: TToggleMode = tmToggle);
    procedure DoUpdateVarnames;
    procedure DoUpdateHistory;
    procedure AddSetOptionHandlers;
  public
    procedure UpdateVarnamesList;
    procedure UpdateHistoryList;

  { Statusbar }
  private
    FStatusbar: TAnalysisStatusbar;

  { Progressbar }
  private
    FProgressbar: TProgressBar;
    FProgressbarInc: Cardinal;
    procedure ReadDataProgress(const Sender: TEpiCustomBase;
      ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
      var Canceled: Boolean);

  { Output }
  private
    FOutputViewer: IAnaOutputViewer;
    FLastCreatorCount: Integer;
    FOutputCreator: TOutputCreator;
    FOutputGenerator: TOutputGeneratorBase;
    procedure OutputRedrawRequest(Sender: TObject);
    function  CreateOutputGenerator(ST: TStream): TOutputGeneratorBase;
    procedure RedrawOutput;
    function  HTMLStream: TStream;
    procedure OutputViewerChanges(Sender: TObject);

  { Help }
  public
    procedure HelpLookup(ContextLookup: Boolean);

  { Recent Files }
  private
    procedure AsyncOpenRecent(Data: PtrInt);
    procedure OpenRecentMenuItemClick(Sender: TObject);
    procedure BuildRecentFilesActions;
  public
    procedure UpdateRecentFiles;

  { StatDialog }
  private
    FStatDialogFactory: TStatDialogFactory;
    procedure BuildStatDialogMenu;

  { Other }
  private
    FStartupFile: String;
    procedure ASyncRunStartup(Data: PtrInt);
    procedure ChangeColour(Const SetOptionName: UTF8String);

  public
    procedure RestoreDefaultPos;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, ast_builder, epiversionutils, parser, LazUTF8,
  outputgenerator_html, about, Clipbrd, epimiscutils, ast_types, epidatafilerelations,
  epiv_custom_statusbar, datamodule, editor_form, LCLIntf, Symbol,
  ana_procs, ana_documentfile, LazFileUtils, LazUTF8Classes, epistringutils, ana_globals,
  browse4, strutils, epifields_helper, options_utils, options_fontoptions, epiv_checkversionform,
  wizard_form, editor_form2, outputgenerator_txt;

{ TMainForm }

procedure TMainForm.CancelExecActionExecute(Sender: TObject);
begin
  Executor.Cancelled := true;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  Frm: TForm;
  EditorForm: TEditorForm;
begin
  //EditorForm := TEditorForm.Create(self);
  //EditorForm.Executor := Executor;
  //EditorForm.History := FHistory;
  //EditorForm.OutputCreator := FOutputCreator;
  //EditorForm.Show;
end;

procedure TMainForm.CloseAllWindowsActionExecute(Sender: TObject);
begin
  CloseWindows(true);
end;

procedure TMainForm.CMDBGColourMenuItemClick(Sender: TObject);
begin
  ChangeColour(ANA_SO_CMDEDIT_BG_COLOR);
end;

procedure TMainForm.CmdEditFocusActionExecute(Sender: TObject);
begin
  if (FCmdEdit.CanFocus) and
     (Self.CanFocus)
  then
    begin
      Self.SetFocus;
      FCmdEdit.SetFocus;
    end;
end;

procedure TMainForm.CMDFontColourMenuItemClick(Sender: TObject);
begin
  ChangeColour(ANA_SO_CMDEDIT_FONT_COLOR);
end;

procedure TMainForm.CopyAllHistoryActionExecute(Sender: TObject);
begin
  Clipboard.AsText := FHistory.Lines.Text;
end;

procedure TMainForm.CopySelectedHistoryActionExecute(Sender: TObject);
var
  S: UTF8String;
  i: Integer;
begin
  S := '';
  for i := 0 to HistoryListBox.Count - 1 do
    if HistoryListBox.Selected[i] then
      S := S + HistoryListBox.Items[i] + LineEnding;

  Clipboard.AsText := S;
end;

procedure TMainForm.CopyVersionInfoActionExecute(Sender: TObject);
var
  S: String;
begin
  S := GetProgramInfo;
  Clipboard.AsText := S;
  ShowMessage('Version info copied to clipboard!');
end;

procedure TMainForm.DefaultWindowPositionActionExecute(Sender: TObject);
begin
  FOutputCreator.DoInfoAll('Press F2, F3, F5, F6, F7, F8 and move windows to desired screen position');
  RedrawOutput;

  RestoreDefaultPos;
end;

procedure TMainForm.OutputBGColourMenuItemClick(Sender: TObject);
begin
  ChangeColour(ANA_SO_OUTPUT_BG_COLOR);
end;

procedure TMainForm.OutputFontColourMenuItemClick(Sender: TObject);
begin
  ChangeColour(ANA_SO_OUTPUT_FONT_COLOR);
end;

procedure TMainForm.QuitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FLastCreatorCount := 0;
  FOutputCreator := TOutputCreator.Create;
  FOutputCreator.OnRedrawRequest := @OutputRedrawRequest;

  Executor := TExecutor.Create(FOutputCreator);
  Executor.AddOnStartExecutingHandler(@ExecutorStart);
  Executor.AddOnEndExecutingHandler(@ExecutorEnd);
  Executor.AddOnAfterStatementHandler(@ExecutorAfterStatement);
  Executor.OnRunPgmCommentError := @CommentError;
  Executor.OnRunPgmLexError     := @LexError;
  Executor.OnRunPgmSyntaxError  := @SyntaxError;
  Executor.OnGUIInteraction     := @GUIInteraction;

  AddSetOptionHandlers;

  FOutputCreator.SetOptions := Executor.SetOptions;

  FProjectTree := TEpiVProjectTreeViewFrame.Create(Self);
  FProjectTree.Visible := false;
  FProjectTree.Parent := LeftSidePanel;
  FProjectTree.Align := alClient;
  FProjectTree.ShowProtected := true;
  FProjectTree.Tag := 0;
  FProjectTree.AllowSelectProject := false;
  FProjectTree.EditCaption := false;
  FProjectTree.EditStructure := false;
  FProjectTree.ShowCheckBoxes := false;
  FProjectTree.ShowRecordCount := true;
  FProjectTree.MinDocumentCount := 1;
  FProjectTree.MaxDocumentCount := 1;
  FProjectTree.ShowHint := true;
  FProjectTree.OnGetHint := @ProjectTreeHint;
  FProjectTree.OnTreeNodeDoubleClick := @ProjectTreeDoubleClick;
  FProjectTree.OnGetText := @ProjectGetText;

  FCommandTree := TCommandTree.Create(Self);
  FCommandTree.Visible := false;
  FCommandTree.Parent := LeftSidePanel;
  FCommandTree.Align := alBottom;
  FCommandTree.Tag := 1;
  FCommandTree.OnCommandDoubleClick := @CommandTreeCommandDoubleClick;
  FCommandTree.OnCommandPressEnterKey := @CommandTreePressEnterKey;

  with VarnamesList do
  begin
    Images := DM.Icons16;

    OnAfterGetMaxColumnWidth := @VarnamesListAfterGetMaxColumnWidth;
    OnGetText                := @VarnamesListGetText;
    OnKeyDown                := @VarnamesListKeyDown;
    OnNodeDblClick           := @VarnamesListNodeDblClick;
    OnGetImageIndex          := @VarnamesListGetImageIndex;
  end;

  FStatusbar := TAnalysisStatusbar.Create(Self, Executor);
  FStatusbar.Parent := Self;
  FStatusbar.Align := alBottom;
  FStatusbar.Update(sucDocFile);
  {$IFDEF DARWIN}
  SetCurrentDirUTF8(ResolveDots(ProgramDirectory + '../../..'));
  {$ENDIF}

  {$IFNDEF LINUX}
  // Do not show font colour items unless on linux
  CMDFontColourMenuItem.Visible := false;
  OutputFontColourMenuItem.Visible := false;
  {$ENDIF}


  FHistory := THistory.Create(Executor, FOutputCreator);

  FCmdEdit := TCmdEdit.Create(Self);
  FCmdEdit.OnRunCommand := @CmdEditRunCommand;
  FCmdEdit.Executor := Executor;
  FCmdEdit.History := FHistory;
  FCmdEdit.Align := alBottom;

  FHistoryWindow := THistoryForm.Create(Self, FHIstory);
  FHistoryWindow.OnClearHistoryAction := @HistoryWindowClearHistory;
  FHistoryWindow.OnLineAction := @HistoryWindowLineAction;

  FVarnamesWindow := TVariablesForm.Create(Self);
  FVarnamesWindow.OnLineAction   := @VarnamesWindowLineAction;

  FProjectTreeForm := TProjectTreeForm.Create(Self, Executor);
  FProjectTreeForm.OnLineAction := @ProjectTreeFormLineAction;

  FCommandTreeForm := TCommandTreeForm.Create(Self);
  FCommandTreeForm.OnLineAction := @CommandTreeFormLineAction;

  aDM.OnProgress := @ReadDataProgress;
  aDM.OutputCreator := FOutputCreator;
  aDM.OnDialogFilename := @DialogFilenameHack;

  BuildRecentFilesActions;
  BuildStatDialogMenu;

  Screen.AddHandlerActiveFormChanged(@FormChanged);
  Application.AddOnKeyDownBeforeHandler(@PrimaryKeyHandler);
  Application.AddOnExceptionHandler(@MainExceptionHandler);
  Application.AddOnActivateHandler(@ApplicationActivate);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FStatusbar);
 // FreeAndNil(EditorForm);
  FreeAndNil(EditorForm2);
  FreeAndNil(FHistory);
  Executor.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  Lst: TStringList;
  S: String;
  i: Integer;
  T: PString;
begin
  DoUpdateTitle;

  // This creates the output viewer (Html, Text)
  OutputViewerChanges(nil);
  UpdateSetOptions;
  RedrawOutput;
  UpdateShortCuts;
  OutputFontChange(nil);
  CmdEditFontChangeEvent(nil);

  ActiveControl := FCmdEdit;

  UpdateRecentFiles;
  LoadTutorials;

  LoadFormPosition(Self, 'MainForm');
//  LoadSplitterPosition(LeftSideSplitter, 'ProjectSplitter');
//  LoadSplitterPosition(RightSideSplitter, 'SidebarSplitter');
//  LoadSplitterPosition(RightPanelSplitter, 'SidebarBottomSplitter');

  // For Cocoa widget set, must add left margin to Main output window
  // and to bottom of command line to see the entire element

  {$IFDEF LCLCocoa}
     PageControl1.BorderSpacing.Left := 10;
     PageControl1.BorderSpacing.Right := 10;
     FCmdEdit.BorderSpacing.Bottom := 10;
  {$ENDIF}

   // At this point it is possible to run the first commands
  if Application.HasOption('i') then
    FStartupFile := ExpandFileNameUTF8(Application.GetOptionValue('i'))
  else
    FStartupFile := GetStartupPgm;

  // For some odd reason, the Statusbar has an incorrect height but changing the size
  // of the main form recalculates it all. This is only needed right after programstart.
//  Application.QueueAsyncCall(@ChangeWidth, 0);
  Application.QueueAsyncCall(@ASyncRunStartup, 0);

  {$IFDEF EPI_BETA}
  Panel2.Visible := true;
  {$ELSE}
  Panel2.Visible := false;
  {$ENDIF}
end;

procedure TMainForm.HistoryListBoxDblClick(Sender: TObject);
begin
  if HistoryListBox.ItemIndex <> -1 then
    begin
      FCmdEdit.Text := HistoryListBox.Items[HistoryListBox.ItemIndex];

      Application.QueueAsyncCall(@DelayCmdEditFocus, 0);
    end;
end;

procedure TMainForm.HistoryListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  S: String;
begin
  if (Key = VK_RETURN) and
     (Shift = [])
  then
    if HistoryListBox.ItemIndex <> -1 then
      begin
        FCmdEdit.Text := HistoryListBox.Items[HistoryListBox.ItemIndex];
        if FCmdEdit.CanFocus then
          FCmdEdit.SetFocus;
      end;

  if (Key = VK_C) and
     (Shift = [ssCtrlOS])
  then
    if HistoryListBox.ItemIndex <> -1 then
      begin
        S := '';
        for i := 0 to HistoryListBox.Count - 1 do
          if (HistoryListBox.Selected[i]) then
            S := S + LineEnding + HistoryListBox.Items[i];

        S := TrimLeft(S);
        Clipboard.AsText := S;
      end;
end;

procedure TMainForm.FontChangeClick(Sender: TObject);
var
  AFont: TFont;
  FN, FS, FC, FSt: String;
begin
  if (Sender = OutputFontMenuItem) then
    begin
      FN := ANA_SO_OUTPUT_FONT_NAME;
      FS := ANA_SO_OUTPUT_FONT_SIZE;
      FC := ANA_SO_OUTPUT_FONT_COLOR;
      FSt := ANA_SO_OUTPUT_FONT_STYLE;
    end;

  if (Sender = CommandLineFontMenuItem) then
    begin
      FN  := ANA_SO_CMDEDIT_FONT_NAME;
      FS  := ANA_SO_CMDEDIT_FONT_SIZE;
      FC  := ANA_SO_CMDEDIT_FONT_COLOR;
      FSt := ANA_SO_CMDEDIT_FONT_STYLE;
    end;

  AFont := FontFromSetOptions(FN, FS, FC, FSt, Executor.SetOptions);
  FontDialog1.Font.Assign(AFont);
  if FontDialog1.Execute then
    FontToSetOptions(FontDialog1.Font, FN, FS, FC, FSt, Executor.SetOptions);
  AFont.Free;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  S: TStream;
  FS: TFileStream;
begin
  if not SaveDialog1.Execute then exit;

  S := HTMLStream;
  FS := TFileStream.Create(SaveDialog1.FileName, fmCreate);
  FS.CopyFrom(S, S.Size);
  FS.Free;
  S.Free;
end;

procedure TMainForm.ReadCBActionExecute(Sender: TObject);
begin
  InterfaceRunCommand('read !cb;');
end;

procedure TMainForm.SaveOutputActionExecute(Sender: TObject);
begin
  InterfaceRunCommand('save !output;');
end;

procedure TMainForm.ShowAboutActionExecute(Sender: TObject);
var
  Frm: TAboutForm;
begin
  Frm := TAboutForm.Create(self);
  Frm.ShowModal;
  Frm.Free;
end;

procedure TMainForm.ShowEditorStartupActionExecute(Sender: TObject);
var
  S: String;
begin
  ShowEditor(GetStartupPgm);
end;

procedure TMainForm.ToggleCmdTreeActionExecute(Sender: TObject);
begin
  ToggleSidebar(FCommandTree, FProjectTree, LeftPanelSplitter, LeftSideSplitter);
  DisplayForm(FCommandTreeForm);
end;

procedure TMainForm.ToggleProjectTreeExecute(Sender: TObject);
begin
  ToggleSidebar(FProjectTree, FCommandTree, LeftPanelSplitter, LeftSideSplitter);
  DisplayForm(FProjectTreeForm);
end;

procedure TMainForm.ToggleVarnamesListActionExecute(Sender: TObject);
begin
  ToggleSidebar(VarnamesList, HistoryListBox, RightPanelSplitter, RightSideSplitter);
  DisplayForm(FVarnamesWindow);
end;

procedure TMainForm.ToggleHistoryListActionExecute(Sender: TObject);
begin
  ToggleSidebar(HistoryListBox, VarnamesList, RightPanelSplitter, RightSideSplitter);
  HistoryListBox.TopIndex := HistoryListBox.Count - 1;
  DisplayForm(FHistoryWindow);
end;

procedure TMainForm.VarnamesListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  F: TEpiField;
begin
  F := Executor.SortedFields[Node^.Index];

  case Column of
    0: CellText := F.Name + IfThen(F.IsKeyfield, '*', '');
    1: CellText := EpiTypeNamesShort[F.FieldType];
    2: CellText := F.Question.Text;
  end;
end;

procedure TMainForm.VarnamesListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Nodes: TVTVirtualNodeEnumeration;
  Node: PVirtualNode;
  S: UTF8String;
  F: TEpiField;
begin
  if (Key <> VK_RETURN) then
    Exit;

  S := '';
  for Node in VarnamesList.SelectedNodes() do
    begin
      F := Executor.SortedFields[Node^.Index];
      S := S + ' ' + F.Name;
    end;

  FCmdEdit.Text := FCmdEdit.Text + S;
  if FCmdEdit.CanFocus then
    FCmdEdit.SetFocus;
end;

procedure TMainForm.VarnamesListNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  F: TEpiField;
begin
  F := Executor.SortedFields[HitInfo.HitNode^.Index];
  FCmdEdit.Text := FCmdEdit.Text + ' ' + F.Name;
end;

procedure TMainForm.VarnamesListAfterGetMaxColumnWidth(Sender: TVTHeader;
  Column: TColumnIndex; var MaxWidth: Integer);
var
  W: Integer;
begin
  W := VarnamesList.Canvas.GetTextWidth(Sender.Columns[Column].Text) +
       // apparently Margin and TextMargin are cummulative...
       (VarnamesList.TextMargin * 2) +
       (VarnamesList.Margin * 2);

  if (W > MaxWidth) then
    MaxWidth := W;
end;

procedure TMainForm.ReadActionExecute(Sender: TObject);
begin
  InterfaceRunCommand('read;');
end;

procedure TMainForm.SaveActionExecute(Sender: TObject);
begin
  InterfaceRunCommand('save;');
end;

procedure TMainForm.CloseActionExecute(Sender: TObject);
begin
  InterfaceRunCommand('close;');
end;

procedure TMainForm.ClearOutputActionExecute(Sender: TObject);
begin
  InterfaceRunCommand('cls;');
end;

procedure TMainForm.ChangeDirActionExecute(Sender: TObject);
begin
  InterfaceRunCommand('cd;');
end;

procedure TMainForm.BrowseActionExecute(Sender: TObject);
begin
  InterfaceRunCommand('browse;');
end;

procedure TMainForm.ShowEditorActionExecute(Sender: TObject);
begin
  ShowEditor();
end;

procedure TMainForm.HistoryListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ACanvas: TCanvas;
  TS: TTextStyle;
begin
  //  FHistory.Lines.Objects[Index];//
  ACanvas := HistoryListBox.Canvas;
  ACanvas.FillRect(ARect);

  if (Index < 0) or (Index > HistoryListBox.Count) then exit;

  if FHistory.Custom[Index] then
    ACanvas.Font.Color := clBlue;

  if FHistory.Failed[Index] then
    ACanvas.Font.Color := clRed;

  TS := ACanvas.TextStyle;
  TS.Layout := tlCenter;
  ACanvas.TextStyle := TS;
  ACanvas.TextRect(ARect, ARect.Left + 2, ARect.Top, HistoryListBox.Items[Index]);
end;

procedure TMainForm.CommandsHelpActionExecute(Sender: TObject);
var
  S: String;
begin
  HelpLookup(false);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Res: TModalResult;
begin
  if Assigned(EditorForm2) then
    CanClose := EditorForm2.CloseQuery;

  if (not CanClose) then exit;

  if (Executor.Modified) and
     (Executor.SetOptionValue[ANA_SO_EXITSAVE] = 'YES')
  then
    begin
      Res := MessageDlg('Warning',
                        'Project has been been modified. You will loose all changes since last save.' + LineEnding +
                        'Are you sure you want to close?',
                        mtWarning,
                        mbYesNo, 0
             );

      CanClose := (Res = mrYes);
    end;

  if (CanClose) then
    begin
      SaveFormPosition(Self, 'MainForm');

      if LeftSideSplitter.Visible then
        SaveSplitterPosition(LeftSideSplitter, 'ProjectSplitter');

      if RightSideSplitter.Visible then
        SaveSplitterPosition(RightSideSplitter, 'SidebarSplitter');

      if RightPanelSplitter.Visible then
        SaveSplitterPosition(RightPanelSplitter, 'SidebarBottomSplitter');
    end;
end;

procedure TMainForm.ClearHistoryActionExecute(Sender: TObject);
begin
  DoParseContent('clh;');
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  S, Ext: String;
  ST: TCustomCommand;
  FT: TEpiDialogFilter;
begin
  if (Length(FileNames) = 0) then exit;


  for S in FileNames do
    begin
      if (not FilenameToFileType(S, FT, dfImport + [dfPGM])) then
      begin
        FOutputCreator.DoWarning('Unknown filetype "' + ExtractFileExt(S) + '" not supported!' + LineEnding +
          'File: ' + S
        );
        Continue;
      end;

      case FT of
        dfPGM:
          InterfaceRunCommand('run "' + S + '";');

        dfEPZ, dfEPX,
        dfDTA, dfREC,
        dfText:
          InterfaceRunCommand('read "' + S + '";');
      end;
    end;
  RedrawOutput;
end;

procedure TMainForm.TutorialsWebActionExecute(Sender: TObject);
begin
  OpenURL(Executor.SetOptionValue[ANA_SO_WEB_URL]);
end;

procedure TMainForm.TutorialsWikiActionExecute(Sender: TObject);
begin
  OpenURL('http://epidata.info/dokuwiki/doku.php?id=training:start');
end;

procedure TMainForm.ShowEditor(const Filename: UTF8String);
begin
  if (not Assigned(EditorForm2)) then
    begin
      EditorForm2 := TEditorForm2.Create(self);
      EditorForm2.Executor := Executor;
      EditorForm2.History := FHistory;
      EditorForm2.OutputCreator := FOutputCreator;
    end;

  EditorForm2.Show;

  if FileExistsUTF8(Filename) then
    EditorForm2.OpenFile(Filename);
end;

procedure TMainForm.LeftPanelChange(Sender: TObject);
var
  Value: TToggleMode;
begin
  case TSetOption(Sender).Value of
    'ON':  Value := tmForceOpen;
    'OFF': Value := tmForceClose;
  end;

  if (Sender = Executor.SetOptions[ANA_SO_DISPAY_COMMANDTREE]) then
    DisplayForm(FCommandTreeForm, Value)
  else
    DisplayForm(FProjectTreeForm, Value);
end;

procedure TMainForm.ProjectTreeFormLineAction(Sender: TObject;
  const LineText: UTF8String);
begin
  InterfaceRunCommand('use ' + LineText + ';');
  CmdEditFocusAction.Execute;
end;

procedure TMainForm.CommandTreeCommandDoubleClick(
  const CommandString: UTF8String);
begin
  FCmdEdit.Text := CommandString;
end;

procedure TMainForm.CloseWindows(CloseAll: boolean);
begin
  FCommandTreeForm.Close;
  FVarnamesWindow.Close;
  FProjectTreeForm.Close;
  FHistoryWindow.Close;

  if (CloseAll) then
    begin
      CloseBrowsers;
      if (Assigned(EditorForm2)) then
        EditorForm2.Close;
    end;
end;

procedure TMainForm.CommandTreeFormLineAction(Sender: TObject;
  const LineText: UTF8String; ChangeFocus: boolean);
begin
  FCmdEdit.Text := LineText;
  if (ChangeFocus) then
    CmdEditFocusAction.Execute;
end;

procedure TMainForm.CommandTreePressEnterKey(const CommandString: UTF8String);
begin
  FCmdEdit.Text := CommandString;
  if (FCmdEdit.CanFocus) then
    FCmdEdit.SetFocus;
end;

procedure TMainForm.HistoryWindowClearHistory(Sender: TObject);
begin
  ClearHistoryActionExecute(nil);
end;

procedure TMainForm.HistoryWindowLineAction(Sender: TObject;
  LineText: UTF8String);
begin
  // TODO : Not correct, but will do for now
  CommandTreePressEnterKey(LineText);
end;

procedure TMainForm.RightPanelChange(Sender: TObject);
var
  Value: TToggleMode;
begin
  case TSetOption(Sender).Value of
    'ON':  Value := tmForceOpen;
    'OFF': Value := tmForceClose;
  end;

  if (Sender = Executor.SetOptions[ANA_SO_DISPAY_HISTORY]) then
    DisplayForm(FHistoryWindow, Value)
  else
    DisplayForm(FVarnamesWindow, Value);
end;

procedure TMainForm.DoUpdateTitle;
var
  S, T: String;
  EpiDoc: TEpiDocument;
  DocFile: TAnaDocumentFile;
begin
  S := 'EpiData Analysis (v' + GetEpiVersionInfo(HINSTANCE) + ')'
    {$IFDEF EPIDATA_TEST_RELEASE}
    + 'test version'
    {$ENDIF}
    ;

  DocFile := TAnaDocumentFile(Executor.DocFile);
  if Assigned(DocFile) then
  begin
    S := S + ' - ' + ExtractFileName(DocFile.FileName);

    EpiDoc := DocFile.Document;
    if EpiDoc.Modified then
      S := S + '*';

    T := EpiDoc.Study.Version;
    if (T <> '') then
      S := S + ' Version: ' + T;

    T := EpiDoc.Study.Title.Text;
    if (T <> '') then
      S := S + ' [' + EpiCutString(T, 20) + ']';
  end;
  Caption := S;
end;

procedure TMainForm.SelectErrorText(ErrorToken: TToken);
begin
  if FCmdEdit.CanFocus then
    FCmdEdit.SetFocus;

  FCmdEdit.CaretPos := Point(ErrorToken.CaretNum, 1);
end;

procedure TMainForm.ExecutorAfterStatement(Statement: TCustomStatement);
begin
  DoUpdateVarnames;
  RedrawOutput;

  case Statement.StatementType of
    stNew:
      if (Statement.ExecResult = csrSuccess) and
         (TCustomNew(Statement).SubCommand = ccProject)
      then
        begin
          FProjectTree.AddDocument(Executor.Document);
          FProjectTreeForm.AddDocument(Executor.Document);
          FStatusbar.DocFile  := Executor.DocFile;
          FStatusbar.Datafile := Executor.DataFile;
          UpdateSetOptions;
        end;

    stRead:
      if (Statement.ExecResult = csrSuccess)
      then
        begin
          FProjectTree.AddDocument(Executor.Document);
          FProjectTreeForm.AddDocument(Executor.Document);
          FStatusbar.DocFile  := Executor.DocFile;
          FStatusbar.Datafile := Executor.DataFile;
          UpdateSetOptions;
          UpdateRecentFiles;
        end;

    stClose:
      if (Statement.ExecResult = csrSuccess)
      then
        begin
          FStatusbar.DocFile  := nil;
          FStatusbar.Datafile := nil;
          FStatDialogFactory.CloseAllDialogs();
        end;

    stUse:
      begin
        FStatusbar.Datafile := Executor.DataFile;
        FStatDialogFactory.ResetAllDialogs();
      end;

    stSave:
      begin
        FStatusbar.Update(sucSave);
        UpdateRecentFiles;
      end;

    stSet:
      begin
        UpdateSetOptions;
      end;
  end;

  FVarnamesWindow.DataFile := Executor.DataFile;
  FStatusbar.Update();
  LeftSideSplitter.Visible := LeftSidePanel.Visible;
end;

procedure TMainForm.DialogFilenameHack(const S: string);
var
  T1, T2: String;
  P, L: Integer;

  function PosAndLength(Const ST: String; out P, L: Integer): boolean;
  begin
    P := UTF8Pos(ST, T2, 1);
    L := UTF8Length(ST);
    result := (P > 0);
  end;

begin
  T1 := FHistory.Lines[FHistory.Lines.Count - 1];
  T2 := UTF8LowerCase(T1);

  if (not PosAndLength('read',    P, L)) then
  if (not PosAndLength('save',    P, L)) then
  if (not PosAndLength('runtest', P, L)) then
  if (not PosAndLength('run',     P, L)) then
  if (not PosAndLength('cd',      P, L)) then
    exit;

  // Include line content up-to-including "read/save/.."
  T2 := UTF8Copy(T1, 1, P + L - 1) +

        // Insert actual filename
        ' "' + S + '"' +

        // add rest of line
        UTF8Copy(T1, P + L, UTF8Length(T1));

  FHistory.Lines[FHistory.Lines.Count - 1] := T2;
end;

function TMainForm.CmdEditRunCommand(Sender: TObject; const S: UTF8String
  ): boolean;
begin
  if (S = 'fail!;') then
  begin
    Sender := nil;
    Sender.ToString;
    Exit;
  end;

  FOutputCreator.DoCommand('.' + OutputCreatorNormalizeText(S));

  result := DoParseContent(S);
  FOutputCreator.DoNormal('');
end;

procedure TMainForm.InterfaceRunCommand(const S: UTF8String);
begin
  FOutputCreator.DoCommand('.' + S);
  FHistory.AddLine(S);
  DoParseContent(S);
  FOutputCreator.DoNormal('');
end;

procedure TMainForm.RunScript(Script: UTF8String);
begin
  InterfaceRunCommand(Script);
end;

procedure TMainForm.PasteScript(Script: UTF8String);
begin
  FCmdEdit.Text := Script;
end;

procedure TMainForm.ProjectTreeHint(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  var HintText: string);
begin
  if (ObjectType <> otRelation) then
    Exit;

  HintText := TEpiMasterRelation(AObject).Datafile.Name;
end;

procedure TMainForm.ProjectTreeDoubleClick(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  if (ObjectType <> otRelation) then
    Exit;

  InterfaceRunCommand('use ' + TEpiMasterRelation(AObject).Datafile.Name + ';');
  DelayCmdEditFocus(0);

//  FCmdEdit.Text := FCmdEdit.Text + ' ' + TEpiMasterRelation(AObject).Datafile.Name;
end;

function TMainForm.DoParseContent(const S: UTF8String): boolean;
var
  P: TParser;
  TheProgram: TStatementList;
  T1, T2: TDateTime;
begin
  T1 := Now;
  P := TParser.Create(Executor);
  P.OnSyntaxError := @SyntaxError;
  P.OnLexError := @LexError;
  P.OnCommentError := @CommentError;
  P.OnASTBuildError := @ASTBuildError;

  Result := P.ParseText(S, TheProgram);

  if Result then
    Executor.Execute(TheProgram);

  P.Free;
  T2 := Now;

  FStatusbar.Update();

//  WriteLn('DoParseContent: ' + FormatDateTime('SS:ZZ', T2-T1));
//  writeln();
end;

procedure TMainForm.CommentError(Sender: TObject; ErrorToken: TToken);
begin
  FOutputCreator.DoError(Format('Line %d: Unexpected end of comment!', [ErrorToken.LineNum]));

  SelectErrorText(ErrorToken);
  FHistory.Failed[FHistory.Lines.Count - 1] := true;
  RedrawOutput;
end;

procedure TMainForm.LexError(Sender: TObject; ErrorToken: TToken);
var
  S: String;
  L: TOutputLine;
begin
  S := 'Line ' + IntToStr(ErrorToken.LineNum) +
       ': Unrecognised symbol at pos ' + IntToStr(ErrorToken.CaretNum) +
       ' - ' + ErrorToken.DataVar;

  FOutputCreator.DoError(S);

  SelectErrorText(ErrorToken);
  FHistory.Failed[FHistory.Lines.Count - 1] := true;
  RedrawOutput;
end;

procedure TMainForm.SyntaxError(Sender: TObject; ErrorToken: TToken;
  TokenTable: TTokenStack);
var
  T, S: UTF8String;
  i: Integer;
  L: TOutputLine;
  GP: TGOLDParser;
begin
  GP := TParser(Sender).GoldParser;
  S := ErrorToken.Name;
  if (Length(S) > 2) and
     (S[1] = 'o') and
     (S[2] = 'p')
  then
    FOutputCreator.DoError(Format('Line %d: Syntax error at pos %d: %s: Reserved word', [ErrorToken.LineNum, ErrorToken.CaretNum, ErrorToken.DataVar]))
  else
    begin
      T := OutputCreatorNormalizeText(ErrorToken.DataVar);
      FOutputCreator.DoError(Format('Line %d: Syntax error at pos %d: %s', [ErrorToken.LineNum, ErrorToken.CaretNum, T]));

      if (ErrorToken.ParentSymbol.Kind = SymbolTypeEnd) then
        begin
          FOutputCreator.DoError(
            'Unexpected end of line!' + LineEnding +
            'Perhaps a ";" is missing?'
          );
        end
      else
        if TokenTable.Count > 0 then
          begin
            T := '"' + TokenTable[0].Name + '"';
            for i := 1 to TokenTable.Count - 1 do
              T := T + ', ' + '"' + TokenTable[i].Name + '"';

            FOutputCreator.DoInfoAll('Expected: ' + OutputCreatorNormalizeText(T));
          end;
    end;

  SelectErrorText(ErrorToken);
  FHistory.Failed[FHistory.Lines.Count - 1] := true;
  RedrawOutput;
end;

procedure TMainForm.ASTBuildError(Sender: TObject; const Msg: UTF8String;
  ErrorToken: TToken);
var
  L: TOutputLine;
begin
  FOutputCreator.DoError(Msg);

  SelectErrorText(ErrorToken);
  FHistory.Failed[FHistory.Lines.Count - 1] := true;
  RedrawOutput;
end;

procedure TMainForm.FormChanged(Sender: TObject; Form: TCustomForm);
begin
  if (Form = Self) then
    ActionList1.State := asNormal
  else
    ActionList1.State := asSuspended;
end;

procedure TMainForm.UpdateSetOptions;

  // TODO: Remove when SetOptions object are properly implemented.
  function StringSetOption(Const S: UTF8String): UTF8String;
  begin
    result := UTF8UpperString(Executor.SetOptions.GetValue(S).Value);
  end;

begin
  BeginFormUpdate;
//  LeftSidePanel.Visible := (StringSetOption('DISPLAY PROJECTTREE')= 'ON');
  EndFormUpdate;
end;

procedure TMainForm.UpdateShortCuts;
begin
  QuitAction.ShortCut :=
    {$IFDEF linux}  KeyToShortCut(VK_Q, [ssCtrl]){$ENDIF}
    {$IFDEF darwin} KeyToShortCut(VK_Q, [ssMeta]){$ENDIF}
    {$IFDEF windows}KeyToShortCut(VK_F4, [ssAlt]){$ENDIF}
end;

procedure TMainForm.ChangeWidth(Data: PtrInt);
begin
  FLastCreatorCount := -1;
  Width := Width - 1;
  RedrawOutput;
end;

procedure TMainForm.LoadTutorials;
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
  P := Executor.SetOptionValue[ANA_SO_TUTORIAL_FOLDER];

  FindAllFiles(FileList, P, '*.pdf', false);
  FindAllFiles(FileList, P, '*.html', false);
  FileList.CustomSort(@EpiStringListSortStr);

  for i := 0 to FileList.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(TutorialSubMenu);
    MenuItem.Name := 'TutorialMenuItem' + IntToStr(i);
    MenuItem.Caption := ExtractFileName(FileList[i]);
    MenuItem.OnClick := @OpenTutorialMenuItemClick;

    TutorialSubMenu.Add(MenuItem);
  end;

  if FileList.Count = 0 then
    TutorialSubMenu.Enabled := false
  else
    TutorialSubMenu.Enabled := true;

  FileList.Free;
end;

procedure TMainForm.GUIInteraction(Sender: TExecutor; GUIAction: TGUIAction);
begin
  case GUIAction of
    gaClearScreen:
      begin
        FOutputCreator.Clear;
        {$IFDEF EPI_BETA}
        FOutputCreator.DoWarning('WARNING - Testversion. Confirm results with Public release!');
        FOutputCreator.DoNormal('');
        {$ENDIF}
        RedrawOutput;
      end;

    gaClearHistory:
      begin
        FHistory.Lines.Clear;
      end;

    gaProjectTree:
      begin
        FProjectTree.UpdateTree;
        FProjectTreeForm.UpdateProjectTree;
      end;

    gaVariableList:
      DoUpdateVarnames;

    gaTitle:
      DoUpdateTitle;
  end;
end;

procedure TMainForm.PrimaryKeyHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  aKey: Word;
begin
  if (not MainForm.CanFocus) then
    Exit;

  aKey := Key;
  Key := VK_UNKNOWN;
  case aKey of
    VK_H:  if (ssCtrlOS in Shift) then
             HelpLookup(true)
           else
             Key := aKey;
    VK_F1: HelpLookup(true);     // F1 will show context help
    VK_F2: ToggleCmdTreeActionExecute(nil);
    VK_F3: ToggleVarnamesListActionExecute(Nil);
    VK_F4: if (Shift = []) then CmdEditFocusActionExecute(Nil) else Key := aKey;
    VK_F5: ShowEditorActionExecute(nil);
    VK_F6: BrowseActionExecute(nil);
    VK_F7: ToggleHistoryListActionExecute(nil);
    VK_F8: ToggleProjectTreeExecute(nil);
    VK_F9: CloseWindows(true);
    VK_F12: ClearOutputActionExecute(nil);
  else
    Key := aKey;
  end;
end;

procedure TMainForm.OutputFontChange(Sender: TObject);
begin
  FOutputViewer.UpdateFontAndSize(Executor);
end;

procedure TMainForm.OutputViewerChanges(Sender: TObject);
var
  Sheet: TTabSheet;
  S: String;
begin
  DisableAutoSizing;

  if (PageControl1.PageCount > 0) and
     (Assigned(PageControl1.ActivePage))
  then
    PageControl1.ActivePage.Free;


  S := UTF8UpperCase(Executor.SetOptions[ANA_SO_OUTPUT_FORMAT].Value);
  case S of
    {$ifdef EPI_CHROMIUM_HTML}
    'HTML',
    'OSR':
      begin
        if (not IsHTMLSupported) then
          begin
            FOutputCreator.DoWarning(
              'HTML output is not supported!' + LineEnding +
              'Please install support library for HTML to work'
            );
            Sheet := TTextPanel.Create(Self);
          end
        else
          if S = 'HTML' then
            Sheet := TWebPanel.Create(Self)
          else
            Sheet := THTMLViewerOSR.Create(Self);
      end;
    {$endif}

    'TEXT':
      Sheet := TTextPanel.Create(Self);

    {$IFDEF EPI_CHROMIUM_HTML}
    'OLDHTML':
    {$ELSE}
    'HTML':
    {$ENDIF}
      Sheet := TOldHtmlSheet.Create(Self);
  end;

  FCmdEdit.Parent := Sheet;
  FCmdEdit.Align := alBottom;

  Sheet.Parent := PageControl1;
  PageControl1.ActivePage := Sheet;
  Supports(Sheet, IAnaOutputViewer, FOutputViewer);

  FOutputViewer.Initialize;
  FOutputViewer.UpdateFontAndSize(Executor);
  FOutputViewer.GetContextMenu.OnSaveOutputClick := @SaveOutputActionExecute;
  RedrawOutput;

  EnableAutoSizing;
end;

procedure TMainForm.ProjectGetText(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  const StaticText: boolean; var NodeText: string);
begin
  if (ObjectType <> otRelation) then exit;
  if (not StaticText) then exit;

  if (TEpiMasterRelation(AObject).Datafile = Executor.DataFile) then
    NodeText := NodeText + '*';
end;

procedure TMainForm.MainExceptionHandler(Sender: TObject; E: Exception);
begin
  ShowMessage(
    'An unhandled error occurred in the program!' + LineEnding +
    LineEnding +
    'Please verify that the error can be reproduced.' + LineEnding +
    'Report to the: EpiData-list (mail.....)' + LineEnding +
    '               and mail to info@epidata.dk' + LineEnding +
    LineEnding +
    'Include most recent 5-10 lines of history in your report.'
  );

  Screen.Cursor := crDefault;
end;

procedure TMainForm.CmdEditFontChangeEvent(Sender: TObject);
var
  AFont: TFont;
begin
  AFont := FontFromSetOptions(
             ANA_SO_CMDEDIT_FONT_NAME,
             ANA_SO_CMDEDIT_FONT_SIZE,
             ANA_SO_CMDEDIT_FONT_COLOR,
             ANA_SO_CMDEDIT_FONT_STYLE,
             Executor.SetOptions
  );

  FCmdEdit.Font.Assign(AFont);
  FCmdEdit.Color := TFontColorOption(Executor.SetOptions[ANA_SO_CMDEDIT_BG_COLOR]).GetBGRValue;
  AFont.Free;

{  FCmdEdit.Font.Name := Executor.SetOptions[ANA_SO_CMDEDIT_FONT_NAME].Value;
  FCmdEdit.Font.Size := StrToInt(Executor.SetOptions[ANA_SO_CMDEDIT_FONT_SIZE].Value);
  FCmdEdit.Font.Color := StrToInt(Executor.SetOptions[ANA_SO_CMDEDIT_FONT_COLOR].Value);
  FCmdEdit.Font.Style := TFontStyleOption(Executor.SetOptions[ANA_SO_CMDEDIT_FONT_STYLE]).GetAsStyle;
  FCmdEdit.Font.EndUpdate;}
end;

procedure TMainForm.DelayCmdEditFocus(Data: PtrInt);
begin
  if FCmdEdit.CanFocus then FCmdEdit.SetFocus;
end;

procedure TMainForm.ApplicationActivate(Sender: TObject);
begin
  FOutputViewer.InvalidateView;
end;

procedure TMainForm.TutorialChange(Sender: TObject);
begin
  LoadTutorials;
end;

procedure TMainForm.DisplayForm(AForm: TCustomForm; ToggleMode: TToggleMode);
begin
  if (not Assigned(AForm)) then
    Exit;

  Case ToggleMode of
    tmToggle: ;  // Not used?
    tmForceOpen:
      begin
        AForm.Show;
        AForm.BringToFront;
        AForm.SetFocus;
      end;
    tmForceClose:
      begin
        AForm.Close;
      end;
  end;
end;

procedure TMainForm.VarnamesWindowLineAction(Sender: TObject;
  const LineText: UTF8String; ChangeFocus: boolean);
begin
  FCmdEdit.Text := FCmdEdit.Text + ' ' + LineText;
  if (ChangeFocus) then
    CmdEditFocusActionExecute(Self);
end;

procedure TMainForm.ExecutorStart(Sender: TObject);
begin
  FStatusbar.Update();

  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
end;

procedure TMainForm.ExecutorEnd(Sender: TObject);
begin
  FStatusbar.Update();
  Screen.Cursor := crDefault;
  RedrawOutput;
  Application.ProcessMessages;
end;

procedure TMainForm.OpenTutorialMenuItemClick(Sender: TObject);
var
  P: String;
begin
  P := Executor.SetOptionValue[ANA_SO_TUTORIAL_FOLDER];

  OpenDocument(P + DirectorySeparator + TMenuItem(Sender).Caption);
end;

procedure TMainForm.CheckVersionOnlineActionExecute(Sender: TObject);
var
  F: TCheckVersionForm;
begin
  F := TCheckVersionForm.Create(Self);
  F.Caption := 'EpiData Analysis';
  F.ShowModal;
  F.Free;
end;

procedure TMainForm.ReleaseNotesActionExecute(Sender: TObject);
begin
  OpenURL('http://epidata.dk/epidataanalysis.changelog.txt');
end;

procedure TMainForm.ShowShortcutActionExecute(Sender: TObject);
var
  Fn: String;
begin
  Fn := Executor.SetOptionValue[ANA_SO_TUTORIAL_FOLDER] + '/epidataanalysisshortcuts.pdf';
  if FileExistsUTF8(Fn) then
    OpenURL(Fn)
  else
  begin
    ShowMessage(
      'Shortcut document was not found in tutorial folder:' + LineEnding +
      Executor.SetOptionValue[ANA_SO_TUTORIAL_FOLDER]
    );
    OpenURL('http://epidata.info/dokuwiki/doku.php?id=documentation:keyboard_shortcuts');
  end;
end;

procedure TMainForm.VarnamesListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  F: TEpiField;
begin
  if Column <> 1 then exit;
  Ghosted := false;

  F := Executor.SortedFields[Node^.Index];
  ImageIndex := DM.GetImageIndex(F.FieldType);
end;

procedure TMainForm.ReadDataProgress(const Sender: TEpiCustomBase;
  ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
  var Canceled: Boolean);
begin
  case ProgressType of
    eptInit:
      begin
        FStatusbar.Hide;
        FProgressbar := TProgressBar.Create(Self);
        FProgressbar.Parent := self;
        FProgressbar.Align := alBottom;
        FProgressbar.Position := 0;
        FProgressbar.Max := MaxPos;
        FProgressbar.Smooth := true;
        FProgressbar.BarShowText := true;
        Application.ProcessMessages;

        FProgressbarInc := MaxPos div 100;
        if FProgressbarInc = 0 then
          FProgressbarInc := 1;
      end;

    eptFinish:
      begin
        FStatusbar.Show;
        FreeAndNil(FProgressbar);
        Application.ProcessMessages;
      end;

    eptRecords:
      begin
        FProgressbar.Position := CurrentPos;

        if (CurrentPos mod FProgressbarInc) = 0 then
          Application.ProcessMessages;
      end;
  end;
end;

procedure TMainForm.ToggleSidebar(ToggleItem, Sibling: TWinControl;
  MidSplitter, MainSplitter: TSplitter; ToggleMode: TToggleMode);
var
  Tmp: Boolean;
  ParentPanel: TWinControl;

  procedure ToggleSplitterVisible(Splitter: TSplitter; Value: boolean);
  begin
    if (Splitter.Visible and (not Value)) then
      begin
        SaveSplitterPosition(Splitter, Splitter.Name);
        Splitter.Visible := Value;
      end;

    if ((not Splitter.Visible) and value) then
      begin
        Splitter.Visible := Value;
        LoadSplitterPosition(Splitter, Splitter.Name);
      end;
  end;

begin
  Exit;

  case ToggleMode of
    tmToggle:     Tmp := (not ToggleItem.Visible) or (not ToggleItem.Focused);
    tmForceOpen:  Tmp := true;
    tmForceClose: Tmp := false;
  end;

  ToggleItem.Visible := Tmp;

  ToggleSplitterVisible(MidSplitter,  ToggleItem.Visible and Sibling.Visible);
  ToggleSplitterVisible(MainSplitter, ToggleItem.Visible or  Sibling.Visible);

  ParentPanel          := ToggleItem.Parent;
  ParentPanel.Visible  := ToggleItem.Visible or Sibling.Visible;



  if (ToggleItem.CanSetFocus) then
    begin
      Self.SetFocus;
      ToggleItem.SetFocus;
    end;

  if (MidSplitter.Visible) then
    begin
      MidSplitter.Align := alNone;
      if (ToggleItem.Tag < Sibling.Tag) then
        begin
          ToggleItem.Align := alClient;
          Sibling.Align := alBottom;
        end
      else
        begin
          ToggleItem.Align := alBottom;
          Sibling.Align := alClient;
        end;
      MidSplitter.Align := alBottom;
    end
  else
    begin
      ToggleItem.Align := alClient;
      Sibling.Align := alClient;
    end;
end;

procedure TMainForm.DoUpdateVarnames;
begin
  if (not Assigned(Executor.DataFile)) then
    VarnamesList.RootNodeCount := 0
  else
    VarnamesList.RootNodeCount := Executor.SortedFields.Count;

  VarnamesList.InvalidateChildren(nil, true);
  VarnamesList.Header.AutoFitColumns(false);

//  FVarnamesWindow.UpdateVarNames;
end;

procedure TMainForm.DoUpdateHistory;
begin
  HistoryListBox.Items.BeginUpdate;

  HistoryListBox.Clear;
  HistoryListBox.Items.Assign(FHistory.Lines);

  HistoryListBox.Items.EndUpdate;
  HistoryListBox.TopIndex := HistoryListBox.Items.Count - 1;

  FHistoryWindow.UpdateHistory;
end;

procedure TMainForm.AddSetOptionHandlers;
begin
  // CmdEdit handlers
  Executor.SetOptions[ANA_SO_CMDEDIT_FONT_NAME].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_CMDEDIT_FONT_SIZE].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_CMDEDIT_FONT_COLOR].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_CMDEDIT_FONT_STYLE].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_CMDEDIT_BG_COLOR].AddOnChangeHandler(@CmdEditFontChangeEvent);

  // Output handlers
  Executor.SetOptions[ANA_SO_OUTPUT_FONT_SIZE].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_FONT_NAME].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_FONT_COLOR].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_FONT_STYLE].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_BG_COLOR].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_FORMAT].AddOnChangeHandler(@OutputViewerChanges);

  // Tutorial handlers
  Executor.SetOptions[ANA_SO_TUTORIAL_FOLDER].AddOnChangeHandler(@TutorialChange);

  // Display/Windows handlers
  Executor.SetOptions[ANA_SO_DISPAY_COMMANDTREE].AddOnChangeHandler(@LeftPanelChange);
  Executor.SetOptions[ANA_SO_DISPAY_DATASET].AddOnChangeHandler(@LeftPanelChange);
  Executor.SetOptions[ANA_SO_DISPAY_HISTORY].AddOnChangeHandler(@RightPanelChange);
  Executor.SetOptions[ANA_SO_DISPAY_VARIABLE].AddOnChangeHandler(@RightPanelChange);
end;

procedure TMainForm.UpdateVarnamesList;
begin
  DoUpdateVarnames;
end;

procedure TMainForm.UpdateHistoryList;
begin
  DoUpdateHistory;
end;

procedure TMainForm.OutputRedrawRequest(Sender: TObject);
begin
  RedrawOutput;
end;

function TMainForm.CreateOutputGenerator(ST: TStream): TOutputGeneratorBase;
begin
  case UTF8UpperString(Executor.SetOptionValue[ANA_SO_OUTPUT_FORMAT]) of
    'OSR',
    'OLDHTML',
    'HTML':
      begin
        Result := TOutputGeneratorHTML.Create(FOutputCreator, ST);
        TOutputGeneratorHTML(Result).CSSFileName := Executor.SetOptionValue[ANA_SO_OUTPUT_CSS_FILE];
        TOutputGeneratorHTML(Result).EmbedCSSFile := UTF8UpperString(Executor.SetOptionValue[ANA_SO_OUTPUT_CSS_INTERNAL]) = 'YES';
      end;
    'TEXT':
      Result := TOutputGeneratorTXT.Create(FOutputCreator, ST);
  end;
end;

procedure TMainForm.RedrawOutput;
var
  S: UTF8String;
  ST: TStream;
  T1, T2: TDateTime;
begin
  T1 := Now;
  if (FOutputCreator.Count <> FLastCreatorCount)
  then
    begin
      ST := TMemoryStreamUTF8.Create;

      FOutputGenerator := CreateOutputGenerator(ST);
      FOutputGenerator.GenerateReport;
      FOutputGenerator.Free;
      ST.Position := 0;

      (PageControl1.ActivePage as IAnaOutputViewer).LoadFromStream(ST);

      ST.Free;
      Application.ProcessMessages;

      FLastCreatorCount := FOutputCreator.Count;
    end;
  T2 := Now;

  Label2.Caption := FormatDateTime('SS:ZZZZ', T2 - T1);
  DoUpdateHistory;
end;

procedure TMainForm.UpdateRecentFiles;
var
  Mi: TMenuItem;
  i: Integer;
  A: TAction;
begin
  LoadRecentFilesFromIni(GetRecentDataIniFileName, RecentDataFiles);

  RecentFilesSubMenu.Visible := RecentDataFiles.Count > 0;
  RecentFilesSubMenu.Clear;

  for i := 0 to 19 do
  begin
    // Main menu
    A := TAction(RecentFilesActionList.Actions[i]);

    // Disable actions if the list of RecentDataFiles is not long enough.
    if i >= RecentDataFiles.Count then
    begin
      A.Enabled := false;
      Continue;
    end;

    A.Enabled := true;
    A.Caption := RecentDataFiles[i];

    Mi := TMenuItem.Create(RecentFilesSubMenu);
    Mi.Name := 'recent' + inttostr(i);
    Mi.Action := A;
    RecentFilesSubMenu.Add(Mi);
  end;
end;

procedure TMainForm.ASyncRunStartup(Data: PtrInt);
var
  Lst: TStringListUTF8;
begin
  if FileExistsUTF8(FStartupFile) then
    begin
      FHistory.LoadingStartup := true;
      Lst := TStringListUTF8.Create;
      Lst.LoadFromFile(FStartupFile);
      FHistory.AddLines(Lst);
      DoParseContent(Lst.Text);
      Lst.Free;
      FHistory.LoadingStartup := false;
      FStartupFile := '';
    end
  else
    InterfaceRunCommand('cls;');

  FOutputCreator.DoInfoAll(GetProgramInfo);
  FOutputCreator.DoNormal('');
end;

procedure TMainForm.ChangeColour(const SetOptionName: UTF8String);
var
  Colour: TColor;
  AFont: TFont;
begin
  Colour := TFontColorOption(Executor.SetOptions.GetValue(SetOptionName)).GetBGRValue;
  ColorDialog1.Color := Colour;

  if (ColorDialog1.Execute) then
    begin
      AFont := TFont.Create;
      AFont.Color := ColorDialog1.Color;
      FontToSetOptions(AFont, '', '', SetOptionName, '', Executor.SetOptions);
      AFont.Free;
    end;
end;

procedure TMainForm.BuildStatDialogMenu;
var
  ContributionList: TStatDialogContributionList;
  Contribution: IStatDialogContribution;
  MenuItem: TMenuItem;
begin
  FStatDialogFactory := TStatDialogFactory.Create(Self, Executor);
  ContributionList := GetStatDialogContributionList;

  for Contribution in ContributionList do
  begin
    MenuItem := TMenuItem.Create(MainMenu1);
    MenuItem.Caption := Contribution.GetCaption();
    MenuItem.Action  := FStatDialogFactory.CreateDialogAction(Contribution);

    StatisticsMainMenuItem.Add(MenuItem);
  end;
end;

procedure TMainForm.RestoreDefaultPos;
var
  W, H, T, L: Integer;
begin
  W := 700;
  H := 600;
  T := Monitor.Top + 5;
  L := Monitor.Left + 5;

  SetBounds(L, T, W, H);

  Application.ProcessMessages;

  TEditorForm2.RestoreDefaultPos(EditorForm2);
  TAboutForm.RestoreDefaultPos;
  TBrowseForm4.RestoreDefaultPos;

  CloseWindows();

  TCommandTreeForm.RestoreDefaultPos(FCommandTreeForm);
  THistoryForm.RestoreDefaultPos(FHistoryWindow);
  TProjectTreeForm.RestoreDefaultPos(FProjectTreeForm);
  TVariablesForm.RestoreDefaultPos(FVarnamesWindow);
  FStatDialogFactory.RestoreDefaultPos();

  SaveFormPosition(Self, 'MainForm');
end;

function TMainForm.HTMLStream: TStream;
var
  HtmlGenerator: TOutputGeneratorBase;
begin
  HtmlGenerator := TOutputGeneratorHTML.Create(FOutputCreator);
  HtmlGenerator.GenerateReport;

  Result := HtmlGenerator.Stream;
  Result.Position := 0;
  HtmlGenerator.Free;
end;

procedure TMainForm.HelpLookup(ContextLookup: Boolean);

  function WholeWordFromPos(Const Input: String; StartPos: Integer): string;
  var
    En, St: LongInt;
    SearchChars: TSysCharSet = ['a'..'z', 'A'..'Z'];
  begin
    St := StartPos;

    while (St > 0) and
          (St <= Length(Input)) and
          (Input[St] in SearchChars)
    do
     Dec(St);
    Inc(St);

    En := St + 1;
    while (En > 0) and
          (En <= Length(Input)) and
          (Input[En] in SearchChars)
    do
     Inc(En);

    Result := Copy(Input, St, En - St);
  end;

var
  S: String;
  P: TPoint;
  Txt: TCaption;
  Editor: TSynEdit;
begin
  S := '';
  Txt := '';
  P := Point(0,0);

  if FCmdEdit.Focused then
    begin
      S := FCmdEdit.SelText;
      Txt := FCmdEdit.Text;
      P := FCmdEdit.CaretPos;
      Inc(P.X); // because X,Y offset in the cmdedit is shifted by -1
    end;

  if FOutputViewer.IsFocused then
    begin
      S := FOutputViewer.GetSelectedText;
      Txt := FOutputViewer.GetLineAtCaret;
      P := FOutputViewer.GetCaretPos;
    end;

  if Assigned(EditorForm2) and
     EditorForm2.ActiveEditorPage.Editor.Focused
  then
    begin
      Editor := EditorForm2.ActiveEditorPage.Editor;
      S   := Editor.SelText;
      Txt := Editor.LineText;
      P   := Editor.CaretXY;
    end;

  if (HistoryListBox.Focused) and
     (HistoryListBox.ItemIndex > -1)
  then
    begin
      S := '';
      Txt := HistoryListBox.Items[HistoryListBox.ItemIndex];
      P   := Point(1,1);
    end;

  // Then try to grap a whole word (if caret is on top a word)
  if (S = '') then
    S := WholeWordFromPos(Txt, P.X);

  // Then grap the first word on the line
  if (S = '') then
    S := WholeWordFromPos(TrimLeft(Txt), 1);

  // If a word is found, then make it an index to search for.
  if (S <> '') and (ContextLookup) then
    S := '#' + S
  else
    S := '';

  {$IFDEF DARWIN}
  S := 'file://' + ResolveDots(ProgramDirectory + '../../../docs' + DirectorySeparator + 'commands.html' + S);
  {$ELSE}
  S := 'file://' + Executor.SetOptionValue[ANA_SO_TUTORIAL_FOLDER] + DirectorySeparator + 'commands.html' + S;
  {$ENDIF}
  OpenURL(S);
end;

procedure TMainForm.AsyncOpenRecent(Data: PtrInt);
begin
  InterfaceRunCommand('read "' + TAction(Data).Caption + '";');
end;

procedure TMainForm.OpenRecentMenuItemClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@AsyncOpenRecent, PtrInt(Sender));
end;

procedure TMainForm.BuildRecentFilesActions;
var
  i: Integer;
  A: TAction;
begin
  // Recent files actions
  for i := 1 to MaxRecentFiles do
  begin
    A := TAction.Create(RecentFilesActionList);
    if (i <= 9) then
      A.ShortCut := KeyToShortCut(VK_1 + (i - 1), [ssShift, ssCtrlOS]);
    A.Enabled  := false;
    A.OnExecute := @OpenRecentMenuItemClick;
    A.ActionList := RecentFilesActionList;
  end;
end;

end.

