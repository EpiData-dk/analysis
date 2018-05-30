unit main;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ActnList, Menus, ExtCtrls, ComCtrls, HtmlView, VirtualTrees,
  Token, GOLDParser, executor, ast, outputcreator,
  epiv_datamodule, epidatafiles, outputgenerator_base, history, cmdedit,
  options_hashmap, epiv_projecttreeview_frame, epicustombase,
  analysis_statusbar, epidocument, epiopenfile, outputviewer_types,
  {$IFDEF EPI_CHROMIUM_HTML}
  htmlviewer, htmlviewer_osr,
  {$ENDIF}
  textviewer,
  oldhtmlviewer;


type

  { TMainForm }

  TMainForm = class(TForm)
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    OutputFontMenuItem: TMenuItem;
    CommandLineFontMenuItem: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    TutorialsWebAction: TAction;
    TutorialsWikiAction: TAction;
    ShowEditorStartupAction: TAction;
    ReadCBAction: TAction;
    DefaultWindowPositionAction: TAction;
    CancelExecAction: TAction;
    CopySelectedHistoryAction: TAction;
    CopyAllHistoryAction: TAction;
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
    ShowAboutAction: TAction;
    ActionList1: TActionList;
    FontDialog1: TFontDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    SaveDialog1: TSaveDialog;
    ToggleCmdTreeAction: TAction;
    ToggleVarnamesListAction: TAction;
    ToggleHistoryListAction: TAction;
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
    MenuItem4: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem38: TMenuItem;
    CheckVersionOnlineAction: TAction;
    ReleaseNotesAction: TAction;
    ShowShortcutAction: TAction;
    procedure CancelExecActionExecute(Sender: TObject);
    procedure CmdEditFocusActionExecute(Sender: TObject);
    procedure CopyAllHistoryActionExecute(Sender: TObject);
    procedure CopySelectedHistoryActionExecute(Sender: TObject);
    procedure CopyVersionInfoActionExecute(Sender: TObject);
    procedure DefaultWindowPositionActionExecute(Sender: TObject);
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
    FVarnamesList: TVirtualStringTree;
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
  private
    procedure DoUpdateVarnames;
  public
    procedure UpdateVarnamesList;

  { History / CmdEdit }
  private
    FHistoryListBox: TListBox;
    FHistory: THistory;
    FCmdEdit: TCmdEdit;
    function  CmdEditRunCommand(Sender: TObject; const S: UTF8String): boolean;
    procedure DoUpdateHistory;
  public
    procedure UpdateHistoryList;
    procedure InterfaceRunCommand(const S: UTF8String);

  { ProjectPanel }
  private
    FProjectTree: TEpiVProjectTreeViewFrame;
    procedure ProjectTreeHint(Sender: TObject; const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; var HintText: string);
    procedure ProjectTreeDoubleClick(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);

  { Sidebar }
  private
    procedure ToggleSidebar(Item: Integer);

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
    FPageControl: TPageControl;
    FOutputViewer: IAnaOutputViewer;
    FLastCreatorCount: Integer;
    FOutputCreator: TOutputCreator;
    FOutputGenerator: TOutputGeneratorBase;
    procedure OutputRedrawRequest(Sender: TObject);
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

  { Other }
  public
    procedure RestoreDefaultPos;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, ast_builder, epiversionutils, parser, LazUTF8, outputgenerator_txt,
  outputgenerator_html, about, Clipbrd, epimiscutils, ast_types, epidatafilerelations,
  epiv_custom_statusbar, datamodule, introduction_form, editor_form, LCLIntf, Symbol,
  ana_procs, ana_documentfile, LazFileUtils, LazUTF8Classes, epistringutils, ana_globals,
  browse4, strutils, epifields_helper, options_utils, options_fontoptions, epiv_checkversionform,
  AnchorDocking, AnchorDockOptionsDlg, XMLPropStorage;
//, projecttree, history_form, varnames;

{ TMainForm }

procedure TMainForm.CancelExecActionExecute(Sender: TObject);
begin
  Executor.Cancelled := true;
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
  for i := 0 to FHistoryListBox.Count - 1 do
    if FHistoryListBox.Selected[i] then
      S := S + FHistoryListBox.Items[i] + LineEnding;

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
  RestoreDefaultPos;
end;


procedure TMainForm.QuitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DockMaster.MakeDockSite(Self, [akBottom], admrpChild);
  DockMaster.OnShowOptions := @ShowAnchorDockOptions;

//  ProjectTreeForm := TProjectTreeForm.Create(Self);
//  VarnamesForm := TVarnamesForm.Create(Self);

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

  Executor.SetOptions[ANA_SO_CMDEDIT_FONT_NAME].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_CMDEDIT_FONT_SIZE].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_CMDEDIT_FONT_COLOR].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_CMDEDIT_FONT_STYLE].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_CMDEDIT_BG_COLOR].AddOnChangeHandler(@CmdEditFontChangeEvent);
  Executor.SetOptions[ANA_SO_OUTPUT_FONT_SIZE].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_FONT_NAME].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_FONT_COLOR].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_FONT_STYLE].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_BG_COLOR].AddOnChangeHandler(@OutputFontChange);
  Executor.SetOptions[ANA_SO_OUTPUT_FORMAT].AddOnChangeHandler(@OutputViewerChanges);
  Executor.SetOptions[ANA_SO_TUTORIAL_FOLDER].AddOnChangeHandler(@TutorialChange);

  FOutputCreator.SetOptions := Executor.SetOptions;

  FProjectTree := TEpiVProjectTreeViewFrame.Create(Self);
//  FProjectTree.Visible := true;
//  FProjectTree.Parent := ProjectPanel;
//  FProjectTree.Align := alClient;
  FProjectTree.ShowProtected := true;

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

  DockMaster.MakeDockable(FProjectTree, false, false);

  FVarnamesList := TVirtualStringTree.Create(Self);
  with FVarnamesList do
    begin
      Name := 'VariableList';
      Colors.UnfocusedColor := clMedGray;
      Header.AutoSizeIndex := 2;
      with Header.Columns.Add do
        begin
          Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 0;
          Text := 'Name';
          Width := 248;
        end;

      with Header.Columns.Add do
        begin
          Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 1;
          Text := 'Type';
          Width := 208;
        end;

      with Header.Columns.Add do
        begin
          Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 2;
          Text := 'Label';
          Width := 10;
        end;

      Header.Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible, hoAutoSpring];
      TreeOptions.MiscOptions := [toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning];
      TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection];
      TreeOptions.SelectionOptions := [toFullRowSelect, toMultiSelect];
      Images := DM.Icons16;

      OnAfterGetMaxColumnWidth := @VarnamesListAfterGetMaxColumnWidth;
      OnGetText                := @VarnamesListGetText;
      OnKeyDown                := @VarnamesListKeyDown;
      OnNodeDblClick           := @VarnamesListNodeDblClick;
      OnGetImageIndex          := @VarnamesListGetImageIndex;
    end;
  DockMaster.MakeDockable(FVarnamesList, false, false);

  FHistoryListBox := TListBox.Create(Self);
  with FHistoryListBox do
    begin
      Name        := 'HistoryListBox';
      MultiSelect := True;
      OnDblClick  := @HistoryListBoxDblClick;
      OnDrawItem  := @HistoryListBoxDrawItem;
      OnKeyDown   := @HistoryListBoxKeyDown;
      PopupMenu   := HistoryPopupMenu;
      Style       := lbOwnerDrawFixed;
    end;
  DockMaster.MakeDockable(FHistoryListBox, false, false);


  FStatusbar := TAnalysisStatusbar.Create(Self, Executor);
  FStatusbar.Name := 'AnalysisStatusbar';
//  FStatusbar.Parent := Self;
//  FStatusbar.Align := alBottom;
  FStatusbar.Update(sucDocFile);
  DockMaster.MakeDockable(FStatusbar, true, true);


  FPageControl := TPageControl.Create(Self);
  FPageControl.Name := 'OutputPageControl';
  FPageControl.ShowTabs := false;
  DockMaster.MakeDockable(FPageControl, false, false);

  FHistory := THistory.Create(Executor, FOutputCreator);

  //HistoryForm := THistoryForm.Create(self, FHistory);

  FCmdEdit := TCmdEdit.Create(Self);
  FCmdEdit.Name := 'CmdEdit';
  FCmdEdit.Text := '';
  FCmdEdit.OnRunCommand := @CmdEditRunCommand;
  FCmdEdit.Executor := Executor;
  FCmdEdit.History := FHistory;
//  FCmdEdit.Parent := Self;//CenterPanel;
//  FCmdEdit.Align := alBottom;
  DockMaster.MakeDockable(FCmdEdit, true, true);

  aDM.OnProgress := @ReadDataProgress;
  aDM.OutputCreator := FOutputCreator;
  aDM.OnDialogFilename := @DialogFilenameHack;

  BuildRecentFilesActions;

  Screen.AddHandlerActiveFormChanged(@FormChanged);
  Application.AddOnKeyDownBeforeHandler(@PrimaryKeyHandler);
  Application.AddOnExceptionHandler(@MainExceptionHandler);
  Application.AddOnActivateHandler(@ApplicationActivate);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FStatusbar);
  FreeAndNil(EditorForm);
  FreeAndNil(FHistory);
  Executor.Free;
end;
procedure TMainForm.FormShow(Sender: TObject);
var
  Lst: TStringList;
  S: String;
  i: Integer;
  HS: TAnchorDockHostSite;
  XMLConfig: TXMLConfigStorage;
begin
  HS := DockMaster.GetAnchorSite(FStatusbar);
  HS.ChangeBounds(Self.Left, Self.Top + Self.Height, FStatusbar.Width, FStatusbar.Height, false);
  HS.Caption := '';

  HS := DockMaster.GetAnchorSite(FCmdEdit);
  HS.ChangeBounds(Self.Left, Self.Top + Self.Height, FCmdEdit.Width, FCmdEdit.Height, false);
  HS.Caption := '';

  DoUpdateTitle;
//  ActiveControl := FCmdEdit;

  // This creates the output viewer (Html, Text)
  OutputViewerChanges(nil);
  UpdateSetOptions;
  RedrawOutput;
  UpdateShortCuts;
  OutputFontChange(nil);
  CmdEditFontChangeEvent(nil);

  UpdateRecentFiles;
  LoadTutorials;

  {if FileExistsUTF8('layout.xml') then
    begin
      XMLConfig := TXMLConfigStorage.Create('layout.xml', true);
      DockMaster.LoadLayoutFromConfig(XMLConfig, true);
      XMLConfig.Free;
    end;   }

//  LoadFormPosition(Self, 'MainForm');
//  LoadSplitterPosition(ProjectSplitter, 'ProjectSplitter');
//  LoadSplitterPosition(SidebarSplitter, 'SidebarSplitter');
//  LoadSplitterPosition(SidebarBottomSplitter, 'SidebarBottomSplitter');

  // At this point it is possible to run the first commands
  if Application.HasOption('i') then
    S := Application.GetOptionValue('i')
  else
    {$IFDEF DARWIN}
  // One possible location of startup.pgm in folder with config files
  //  S := GetEnvironmentVariableUTF8('HOME')+'/.config/epidata/epidataanalysis/startup.pgm';
    S := ProgramDirectory + '../../../startup.pgm';
    {$ELSE}
    S := ProgramDirectory + DirectorySeparator + 'startup.pgm';
    {$ENDIF}

  S := ExpandFileNameUTF8(S);

  if FileExistsUTF8(s) then
    begin
      FHistory.LoadingStartup := true;
      Lst := TStringListUTF8.Create;
      Lst.LoadFromFile(S);
      FHistory.AddLines(Lst);
      DoParseContent(Lst.Text);
      Lst.Free;
      FHistory.LoadingStartup := false;
    end
  else
    InterfaceRunCommand('cls;');

  // For some odd reason, the Statusbar has an incorrect height but changing the size
  // of the main form recalculates it all. This is only needed right after programstart.
  Application.QueueAsyncCall(@ChangeWidth, 0);

  {$IFDEF RELEASE}
  Panel2.Visible := false;
  {$ENDIF}
end;

procedure TMainForm.HistoryListBoxDblClick(Sender: TObject);
begin
  if FHistoryListBox.ItemIndex <> -1 then
    begin
      FCmdEdit.Text := FHistoryListBox.Items[FHistoryListBox.ItemIndex];

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
    if FHistoryListBox.ItemIndex <> -1 then
      begin
        FCmdEdit.Text := FHistoryListBox.Items[FHistoryListBox.ItemIndex];
        if FCmdEdit.CanFocus then
          FCmdEdit.SetFocus;
      end;

  if (Key = VK_C) and
     (Shift = [ssCtrlOS])
  then
    if FHistoryListBox.ItemIndex <> -1 then
      begin
        S := '';
        for i := 0 to FHistoryListBox.Count - 1 do
          if (FHistoryListBox.Selected[i]) then
            S := S + LineEnding + FHistoryListBox.Items[i];

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

{  AFont := TFont.Create;
  AFont.Name := Executor.SetOptions[FN].Value;
  AFont.Size := StrToInt(Executor.SetOptions[FS].Value);
  AFont.Color := StrToInt(Executor.SetOptions[FC].Value);
  AFont.Style := ;                                       }

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
  {$IFDEF DARWIN}
// suggested location of startup.pgm
//  S := GetEnvironmentVariableUTF8('HOME')+'/.config/epidata/epidataanalysis/startup.pgm';
  S := ResolveDots(ProgramDirectory + '../../../startup.pgm');
  {$ELSE}
  S := ProgramDirectory + DirectorySeparator + 'startup.pgm';
  {$ENDIF}
  ShowEditor(S);
end;

procedure TMainForm.ToggleCmdTreeActionExecute(Sender: TObject);
begin
//  ToggleSidebar(1);
end;

procedure TMainForm.ToggleVarnamesListActionExecute(Sender: TObject);
begin
//  ToggleSidebar(2);
  DockMaster.MakeDockable(FVarnamesList, true, true);
end;

procedure TMainForm.ToggleHistoryListActionExecute(Sender: TObject);
begin
//  ToggleSidebar(3);
  DockMaster.MakeDockable(FHistoryListBox, true, true);
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
  for Node in FVarnamesList.SelectedNodes() do
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
  W := FVarnamesList.Canvas.GetTextWidth(Sender.Columns[Column].Text) +
       // apparently Margin and TextMargin are cummulative...
       (FVarnamesList.TextMargin * 2) +
       (FVarnamesList.Margin * 2);

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
  ACanvas := FHistoryListBox.Canvas;
  ACanvas.FillRect(ARect);

  if (Index < 0) or (Index > FHistoryListBox.Count) then exit;

  if FHistory.Custom[Index] then
    ACanvas.Font.Color := clBlue;

  if FHistory.Failed[Index] then
    ACanvas.Font.Color := clRed;

  TS := ACanvas.TextStyle;
  TS.Layout := tlCenter;
  ACanvas.TextStyle := TS;
  ACanvas.TextRect(ARect, ARect.Left + 2, ARect.Top, FHistoryListBox.Items[Index]);
end;

procedure TMainForm.CommandsHelpActionExecute(Sender: TObject);
var
  S: String;
begin
  HelpLookup(false);
end;

procedure TMainForm.ToggleProjectTreeExecute(Sender: TObject);
begin
//  ProjectPanel.Visible := not ProjectPanel.Visible;
//  ProjectSplitter.Visible := ProjectPanel.Visible;

  DockMaster.MakeDockable(FProjectTree, true, true);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Res: TModalResult;
  XMLConfig: TXMLConfigStorage;
begin
  if Assigned(EditorForm) then
    CanClose := EditorForm.CloseQuery;

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
//      SaveFormPosition(Self, 'MainForm');
   {   XMLConfig := TXMLConfigStorage.Create('layout.xml', false);
      DockMaster.SaveLayoutToConfig(XMLConfig);
      XMLConfig.WriteToDisk;
      XMLConfig.Free;  }

//      if ProjectSplitter.Visible then
//        SaveSplitterPosition(ProjectSplitter, 'ProjectSplitter');

//      if SidebarSplitter.Visible then
//        SaveSplitterPosition(SidebarSplitter, 'SidebarSplitter');

//      if SidebarBottomSplitter.Visible then
//        SaveSplitterPosition(SidebarBottomSplitter, 'SidebarBottomSplitter');
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
  if (not Assigned(EditorForm)) then
    begin
      EditorForm := TEditorForm.Create(self);
      EditorForm.Executor := Executor;
      EditorForm.History := FHistory;
      EditorForm.OutputCreator := FOutputCreator;
    end;

  EditorForm.Show;

  if (Filename <> '') then
    EditorForm.OpenPgm(Filename);
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
          FStatusbar.DocFile  := Executor.DocFile;
          FStatusbar.Datafile := Executor.DataFile;
          UpdateSetOptions;
        end;

    stRead:
      if (Statement.ExecResult = csrSuccess)
      then
        begin
          FProjectTree.AddDocument(Executor.Document);
          FStatusbar.DocFile  := Executor.DocFile;
          FStatusbar.Datafile := Executor.DataFile;
          UpdateSetOptions;
          UpdateRecentFiles;
        end;

    stClose:
      if (Statement.ExecResult = csrSuccess)
      then
        begin
          //ProjectPanel.Visible := false;

          FStatusbar.DocFile  := nil;
          FStatusbar.Datafile := nil;
        end;

    stUse:
      begin
        FStatusbar.Datafile := Executor.DataFile;
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

  FStatusbar.Update();
  //ProjectSplitter.Visible := ProjectPanel.Visible;
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
    FOutputCreator.DoError(Format('Line %d: Syntax error at pos %d  (%s: Reserved word)', [ErrorToken.LineNum, ErrorToken.CaretNum, ErrorToken.DataVar]))
  else
    begin
      T := OutputCreatorNormalizeText(ErrorToken.DataVar);
      FOutputCreator.DoError(Format('Line %d: Syntax error at pos %d  (%s)', [ErrorToken.LineNum, ErrorToken.CaretNum, T]));

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

            FOutputCreator.DoInfoAll('Expected tokens: ' + OutputCreatorNormalizeText(T));
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
//  ProjectPanel.Visible := (StringSetOption('DISPLAY PROJECTTREE')= 'ON');
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
(* suggested change for DARWIN
  {$IFDEF DARWIN}
  P := ResolveDots(ProgramDirectory + '../../../docs');
  {$ELSE}
  P := ProgramDirectory + DirectorySeparator + 'docs';
  {$ENDIF}
*)
  {$IFDEF DARWIN}
  P := ProgramDirectory + '../../../docs';
  {$ELSE}
  P := ProgramDirectory + DirectorySeparator + 'docs';
  {$ENDIF}

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
//        FOutputCreator.DoNormal('<a href="http://www.epidata.dk> TEST </a>');
        FOutputCreator.DoInfoAll(GetProgramInfo);
        FOutputCreator.DoNormal('');
        RedrawOutput;
      end;

    gaClearHistory:
      begin
        FHistory.Lines.Clear;
      end;

    gaProjectTree:
      FProjectTree.UpdateTree;

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

//  if (Shift <> []) then Exit;
//  if (not (Key in [VK_F1..VK_F7,VK_F12])) then exit;

  aKey := Key;
  Key := VK_UNKNOWN;
  case aKey of
    VK_H:  if (ssCtrlOS in Shift) then
             HelpLookup(true)
           else
             Key := aKey;
    VK_F1: HelpLookup(true);     // F1 will show context help
    VK_F2: ToggleProjectTreeExecute(nil);
    VK_F3: ToggleVarnamesListActionExecute(nil);
    VK_F4: if (Shift = []) then CmdEditFocusActionExecute(Nil) else Key := aKey;
    VK_F5: ShowEditorActionExecute(nil);
    VK_F6: BrowseActionExecute(nil);
    VK_F7: ToggleHistoryListActionExecute(nil);
    VK_F12: ClearOutputActionExecute(nil) ;
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
//  DisableAutoSizing;

  if (FPageControl.PageCount > 0) and
     (Assigned(FPageControl.ActivePage))
  then
    FPageControl.ActivePage.Free;


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

  Sheet.Parent := FPageControl;
  FPageControl.ActivePage := Sheet;
  Supports(Sheet, IAnaOutputViewer, FOutputViewer);

  FOutputViewer.Initialize;
  FOutputViewer.UpdateFontAndSize(Executor);
  RedrawOutput;

  DockMaster.MakeDockable(FPageControl, true, false);

//  EnableAutoSizing;
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
(* suggested change for DARWIN
  {$IFDEF DARWIN}
  P := ResolveDots(ProgramDirectory + '../../../docs');
  {$ELSE}
  P := ProgramDirectory + DirectorySeparator + 'docs';
  {$ENDIF}
*)
  {$IFDEF DARWIN}
  P := ProgramDirectory + '../../../docs';
  {$ELSE}
  P := ProgramDirectory + DirectorySeparator + 'docs';
  {$ENDIF}

  P := Executor.SetOptionValue[ANA_SO_TUTORIAL_FOLDER];

  OpenDocument(P + DirectorySeparator + TMenuItem(Sender).Caption);
end;

procedure TMainForm.CheckVersionOnlineActionExecute(Sender: TObject);
var
  F: TCheckVersionForm;
begin
  F := TCheckVersionForm.Create(Self);
  F.Caption := 'EpiData Analysis';
  F.CheckBoxValue := true;
  F.ShowModal;
//  ManagerSettings.CheckForUpdates := F.CheckBoxValue;
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

procedure TMainForm.ToggleSidebar(Item: Integer);
var
  Ctrl: TWinControl;
begin
{  // 1 = CmdTree
  // 2 = VarList
  // 3 = History
  // 4 = Project Tree

  case Item of
//    1: Ctrl := CmdTree;
    2: Ctrl := FVarnamesList;
    3: Ctrl := FHistoryListBox;
    4:
      begin

      end;
  end;

  if (not Ctrl.Visible) then
    begin
      if (not SidebarPanel.Visible) then
        SidebarPanel.Visible := true;
      Ctrl.Visible := true;
    end;

  if (Ctrl.Focused) then
    begin
      Ctrl.Visible := false;
    end
  else
    begin
      if (Ctrl.CanFocus) and
         (Self.CanFocus)
      then
        begin
          Self.SetFocus;
          Ctrl.SetFocus;
        end;
    end;

  if (FHistoryListBox.Visible) then
    begin
      if (not FVarnamesList.Visible) then
        FHistoryListBox.Align := alClient
      else
        FHistoryListBox.Align := alBottom;

      FHistoryListBox.TopIndex := FHistoryListBox.Count - 1;
    end;

  SidebarBottomSplitter.Visible := (FVarnamesList.Visible) and (FHistoryListBox.Visible);

  SidebarPanel.Visible := (FVarnamesList.Visible or FHistoryListBox.Visible);
  SidebarSplitter.Visible := SidebarPanel.Visible;        }
end;

procedure TMainForm.DoUpdateVarnames;
begin
  if (not Assigned(Executor.DataFile)) then
    FVarnamesList.RootNodeCount := 0
  else
    FVarnamesList.RootNodeCount := Executor.SortedFields.Count;

  FVarnamesList.InvalidateChildren(nil, true);
  FVarnamesList.Header.AutoFitColumns(false);
end;

procedure TMainForm.DoUpdateHistory;
begin
  FHistoryListBox.Items.BeginUpdate;

  FHistoryListBox.Clear;
  FHistoryListBox.Items.Assign(FHistory.Lines);

  FHistoryListBox.Items.EndUpdate;
  FHistoryListBox.TopIndex := FHistoryListBox.Items.Count - 1;

  //HistoryForm.UpdateHistory;
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

      FOutputGenerator := (FPageControl.ActivePage as IAnaOutputViewer).GetOutputGeneratorClass.Create(FOutputCreator, ST);
      FOutputGenerator.GenerateReport;
      FOutputGenerator.Free;
      ST.Position := 0;

      (FPageControl.ActivePage as IAnaOutputViewer).LoadFromStream(ST);

      ST.Free;
      Application.ProcessMessages;

      FLastCreatorCount := FOutputCreator.Count;
    end;
  T2 := Now;

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

  for i := 0 to 8 do
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

procedure TMainForm.RestoreDefaultPos;
begin
  TEditorForm.RestoreDefaultPos;
  TAboutForm.RestoreDefaultPos;
  TBrowseForm4.RestoreDefaultPos;

  BeginFormUpdate;
  Width := 700;
  Height := 600;
  Top := 5;
  Left := 5;
  EndFormUpdate;

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

  if Assigned(EditorForm) and
     EditorForm.SynEdit1.Focused
  then
    begin
      S   := EditorForm.SynEdit1.SelText;
      Txt := EditorForm.SynEdit1.LineText;
      P   := EditorForm.SynEdit1.CaretXY;
    end;

  if (FHistoryListBox.Focused) and
     (FHistoryListBox.ItemIndex > -1)
  then
    begin
      S := '';
      Txt := FHistoryListBox.Items[FHistoryListBox.ItemIndex];
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

{  {$IFDEF DARWIN}
  S := 'file://' + ResolveDots(ProgramDirectory + '../../../docs' + DirectorySeparator + 'commands.html' + S);
  {$ELSE}  }
  S := 'file://' + Executor.SetOptionValue[ANA_SO_TUTORIAL_FOLDER] + DirectorySeparator + 'commands.html' + S;
//  {$ENDIF}
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
    A.ShortCut := KeyToShortCut(VK_1 + (i - 1), [ssShift, ssCtrlOS]);
    A.Enabled  := false;
    A.OnExecute := @OpenRecentMenuItemClick;
    A.ActionList := RecentFilesActionList;
  end;
end;

end.

