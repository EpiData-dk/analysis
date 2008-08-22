unit Editor;
// Developer: FB 31st of March-June 2004.

interface

uses sysutils,Windows, Messages,Classes, Graphics, Forms,Dialogs, Controls, StdCtrls, ComCtrls,
  mwCustomEdit, Menus, UFRGODLG, ActnList, ImgList, SMUtils, ansdatatypes,
  ToolWin, UTranslation
  {,ToolWin}  ;

const
  cfCanCut=1;
  cfCanCopy=2;
  cfCanPaste=3;
  cfCanUnDo=4;
  cfCanRedo=5;

  cfCanFind=6;
  cfCanFindPrev=7;
  cfCanFindNext=8;
  cfCanReplace=9;

  cfCanSave=10;
  cfCanPrint=11;
  cfCanClose=12;
  cfCanSaveAs=13;

  cfCanSelectAll=14;
  cfCanGoTo=15;

  cfInsHist=16;

  SearchActionFind=1;
  SearchActionReFind=2;
  SearchActionReplace=3;

type
  TMDIChild = class(TForm)
    Memo1: TmwCustomEdit;
    pmnuEditor: TPopupMenu;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N3: TMenuItem;
    PrintSetup1: TMenuItem;
    N4: TMenuItem;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    New1: TMenuItem;
    Edit1: TMenuItem;
    N5: TMenuItem;
    GoTo1: TMenuItem;
    Replace1: TMenuItem;
    Find1: TMenuItem;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    N7: TMenuItem;
    Repeatcommand1: TMenuItem;
    Undo1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ActionList3: TActionList;
    FileNewCmd: TAction;
    FileOpenCmd: TAction;
    FileSaveCmd: TAction;
    FilePrintCmd: TAction;
    FileExitCmd: TAction;
    FileSaveAsCmd: TAction;
    AcEditundo: TAction;
    AcEditcut: TAction;
    AcEditcopy: TAction;
    AcEditpaste: TAction;
    AcEditFont: TAction;
    AcInsertHist: TAction;
    StatusBar: TStatusBar;
    AcEditRedo: TAction;
    AcGoto: TAction;
    AcSelectAll: TAction;
    Selectall1: TMenuItem;
    N8: TMenuItem;
    AcEditFind: TAction;
    AcEditReplace: TAction;
    StandardToolBar: TToolBar;
    ToolButton1: TToolButton;
    OpenButton: TToolButton;
    SaveButton: TToolButton;
    PrintButton: TToolButton;
    ToolButton5: TToolButton;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    UndoButton: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    BoldButton: TToolButton;
    ItalicButton: TToolButton;
    UnderlineButton: TToolButton;
    BulletsButton: TToolButton;
    Undo2: TMenuItem;
    Redo1: TMenuItem;
    N6: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    Selectall2: TMenuItem;
    N9: TMenuItem;
    Find2: TMenuItem;
    Replace2: TMenuItem;
    Gotoline1: TMenuItem;
    AcRunPgm: TAction;
    Run1: TMenuItem;
    Run2: TMenuItem;
    AcActivateVar: TAction;
    AcWindowCommands: TAction;
    ToolButton3: TToolButton;
    AcRefind: TAction;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    AcStopRun: TAction;
    Findagain1: TMenuItem;
    AcRunSelected: TAction;
    Runselected1: TMenuItem;
    InsertHistory1: TMenuItem;
    FileCloseCmd: TAction;
    Close1: TMenuItem;
    InsertHistory2: TMenuItem;
    Windows1: TMenuItem;
    CommandsF21: TMenuItem;
    VariablesF31: TMenuItem;
    SaveWindowPosition1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Memo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure memo1StatusChange(Sender: TObject;  Changes: TmwStatusChanges);
    procedure FormCreate(Sender: TObject);
    procedure memo1SpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure FileExitCmdExecute(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FilePrint(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure EditRedo(Sender: TObject);
    procedure EditFind(Sender: TObject);
    procedure EditGoToLin(Sender: TObject);
    procedure EditSelectAll(Sender: TObject);
    procedure EditInsertHistory(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure AcRunPgmExecute(Sender: TObject);
    procedure AcActivateVarExecute(Sender: TObject);
    procedure AcWindowCommandsExecute(Sender: TObject);
    procedure AcRefindExecute(Sender: TObject);
    procedure AcStopRunExecute(Sender: TObject);
    procedure AcRunSelectedExecute(Sender: TObject);
    procedure FileCloseCmdExecute(Sender: TObject);
    procedure AcEditReplaceExecute(Sender: TObject);
    procedure Savewindowsposition1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    fRow: Integer;
    fCol: Integer;
    fRowCount: Integer;
    fReadOnly: Boolean;
    fInsertMode: Boolean;
    fOnStatusChange: TNotifyEvent;
    fModified: Boolean;
    fFileName: string;
    St,Rt: string;
    Searchopt: TFrSearchOptions;
    EdSearchOptions: TmwSearchOptions;
    fCurrentLine: Integer;
    fCurrentErrorLine: Integer;
//    procedure WMIconEraseBkgnd(VAR Message: TWMIconEraseBkgnd); message WM_ICONERASEBKGND;
    function GetCanDo(Index: Integer): Boolean;
    function Empty: Boolean;
    procedure GoToLineExecute;
    procedure SetRow(const Value: Integer);
    procedure DoSearchReplaceText(AReplace, ABackwards: boolean);
    function ShowSearchDialog(Mode: Integer):TFrReplaceAction;
    procedure SetCurrentLine(Value: Integer);
    procedure SetCurrentErrorLine(const Value: Integer);
    //procedure CheckFileSave;
    Function CheckFileSave(): Boolean;
    procedure PerformFileOpen(const AFileName: string);
    procedure SetFileName(const FileName: string);
    procedure SetModified(Value: Boolean);
    function OpenFromStream(Stream: TStream; const pfilename: string; pReadonly: Boolean): Boolean;
    function RunPgm(Selected: Boolean): Boolean;
    function DoInsertHistory(): Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    function QueryForm(Query: Word; var Lparam,Wparam: Integer): Boolean;
    function ExecAction(Ac: word): Boolean;
    function OpenFile(const pfilename: string; pReadonly: Boolean): Boolean;
    property Col: Integer read fCol;
    property Row: Integer read fRow write SetRow;
    property RowCount: Integer read fRowCount;
    property ReadOnly: Boolean read fReadOnly;
    property InsertMode: Boolean read fInsertMode;
    property Modified: Boolean read fModified;
    property CanDo[Index: Integer]: Boolean read GetCanDo;
    property FileName: string read fFileName write fFileName;
    property OnStatusChange: TNotifyEvent read fOnStatusChange write fOnStatusChange;
    property CurrentLine: Integer read fCurrentLine write SetCurrentLine;
    property CurrentErrorLine: Integer read fCurrentErrorLine write SetCurrentErrorLine;
  end;

procedure ShowEditor(const FileName: string; Line: Integer; InsertHistory: Boolean=False);
procedure ShowEditorUsingStream(Stream: TStream; const pfilename: string; pReadonly: boolean);
function EditorHasFocus(): boolean;
function GetEditorHandle(): TMDIChild;
procedure AppendCommandToMemo(Cmd: string);
procedure s(mess: string);
function ShutdownEditor(): Boolean;

implementation

{$R *.DFM}
uses Umain,{ UToolWin, UVarWin,} UIniFile, UCmdProcessor;

var
  Child: TMDIChild;

procedure AppendCommandToMemo(Cmd: string); //FB: 31st of March-28rd of June 2004.
var ISelLine,ISelChar,ISelStart,IChar: Integer; TxtL,TxtR: string;
begin
  ISelLine:=0; //point to first line.
  ISelChar:=1; //point to first character.
  ISelStart:=Child.Memo1.GetSelStart; //cursor at char no. Each line ends with CR LF.
  IChar:=1;
  while IChar+Length(Child.Memo1.Lines.Strings[ISelLine])+1<ISelStart do
  begin
    IChar:=IChar+Length(Child.Memo1.Lines.Strings[ISelLine])+2;
    ISelLine:=ISelLine+1;
  end;
  if IChar<=ISelStart then
    ISelChar:=ISelChar+ISelStart-IChar;
  TxtL:=Copy(Child.Memo1.Lines.Strings[ISelLine],1,ISelChar-1);
  TxtR:=Copy(Child.Memo1.Lines.Strings[ISelLine],ISelChar,Length(Child.Memo1.Lines.Strings[ISelLine])+1-ISelChar);
  Child.Memo1.Lines.Strings[ISelLine]:=TxtL+' '+Cmd+' '+TxtR;
end;

procedure s(mess: string);
begin
  MessageDlg(mess,mtInformation,[mbOK],0);
end;

function GetEditorHandle(): TMDIChild;
begin
  result := child;
end;

function EditorHasFocus(): boolean;
begin
  if not Assigned(child) then
    result := false
  else
    result := child.Memo1.Focused;
end;

function Shutdowneditor(): Boolean;
begin
  Result:=False;
  if Assigned(Child) then
  begin
    Child.Show;
    try
      // Child.CheckFileSave;
      if not Child.CheckFileSave then exit;
    except
      Exit;
    end;
    FreeAndNil(Child);
    UMain.aMainForm.booUMainEditorFocused:=False;
  end;
  Result:=True;
end;

procedure ShowEditor(const FileName: string;Line: integer; InsertHistory:boolean=false);
begin
  { create a new MDI child window }
  if not assigned(child) then
  begin
    Child := TMDIChild.Create(Application);
    OTranslator.TranslateForm(Child);
    child.FileNew(Application);
  end;
    if InsertHistory then
      child.ExecAction(cfInsHist);
  child.Show;
//  child.SetFocus;
  if filename<>'' then child.PerformFileOpen(filename);// OpenFile(filename, false)
  //  child.OnStatusChange :=OnEditorStatusChange;
//    AcRunMeuExecute(Self);
end;

procedure ShowEditorUsingStream(Stream:TStream; const pfilename: string; pReadonly: boolean);
begin
  { create a new MDI child window }
  if not assigned(child) then
    Child := TMDIChild.Create(Application);
  child.Show;
  if Stream<>nil then
     child.OpenFromStream(Stream,pfilename, false);
//  child.OnStatusChange :=OnEditorStatusChange;
//    AcRunMeuExecute(Self);
end;



procedure TMDIChild.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

function TMDIChild.OpenFile(const pfilename: string; pReadonly: boolean): boolean;
const
 modes: array[boolean] of word=(fmOpenRead,fmOpenReadWrite);
var
 Stream : TFileStream;
begin
  if pfileName='' then exit;
  if not FileExists(pFileName) then exit;
  CheckFileSave;
try
try
  Stream := TFileStream.create(pfilename,modes[pReadonly]);
  OpenFromStream(Stream, pfilename, pReadonly);
except
  raise
end;
finally
  Stream.free;
end;
end;

function TMDIChild.OpenFromStream(Stream:TStream; const pfilename: string; pReadonly: boolean): boolean;
var
  backCursor: TCursor;
begin
  backCursor := Cursor;
  try
    CheckFileSave;
    Screen.Cursor := crHourGlass;
//    Windows.SetCursor(Screen.Cursors[crHourGlass]);
    Memo1.Lines.LoadFromStream(Stream);
    fReadonly := pReadonly;
    Filename :=pFilename;
    CurrentErrorLine:=-1;
  finally
    Screen.Cursor := backCursor;
  end;
end;

procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  aMainForm.SetFocus;
  aMainForm.booUMainEditorFocused:=True;
end;

procedure TMDIChild.Memo1Change(Sender: TObject);
begin
// fCurrentErrorLine:=-1;
 Statusbar.Panels[0].text:=format('Line:%d          Col:%d',[Row,col]);

//                           [ Memo1.Perform(EM_LINEFROMCHAR,-1,0),
//                             Abs(Memo1.Perform(EM_LINEINDEX,-1,0))]);
//-Memo1.SelStart
end;

procedure TMDIChild.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Memo1Change(self);
  CurrentErrorLine:=-1;
end;

procedure TMDIChild.Memo1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    Memo1Change(self);
    CurrentErrorLine:=-1;
end;

procedure TMDIChild.FormShow(Sender: TObject);
var
  opt: TEpiOption;
begin
  UMain.aMainForm.booUMainEditorFocused:=False;
  if dm.GetOptionValue('EDITOR FONT SIZE', opt) then
    Memo1.Font.Size := StrToInt(opt.Value);
  Memo1Change(self);
  memo1StatusChange(Self, [mwscCaretX, mwscCaretY, mwscLeftChar, mwscTopLine, mwscInsertMode]);
//  CurrentErrorLine := -1;
end;
{
procedure TMDIChild.WMIconEraseBkgnd(var Message: TWMIconEraseBkgnd);
begin
  TMainForm(Application.Mainform).PaintUnderIcon(Self, Message.DC);
  Message.Result := 0;
end;
}

function TMDIChild.QueryForm(Query: Word; var Lparam,
  Wparam: integer): boolean;
begin

end;

procedure TMDIChild.memo1StatusChange(Sender: TObject;Changes: TmwStatusChanges);
const
  ModifiedStrs: array[boolean] of string = ('', 'Modified');
  InsertModeStrs: array[boolean] of string = ('Overwrite', 'Insert');
var
  p: TPoint;
  Token: string;
//  Attri: TSynHighlighterAttributes;
begin
  // caret position has changed
//  if Changes * [mwscCaretX, mwscCaretY,mwscTopLine] <> [] then
//   begin
    p := memo1.CaretXY;
    fcol := p.X;
    frow := p.Y;
    fRowCount := memo1.Lines.Count;
    Memo1Change(self);
//  end;
  if Changes * [mwscInsertMode] <> [] then
  begin
    if memo1.ReadOnly then
        fReadOnly := True
    else
       fInsertMode := memo1.InsertMode
  end;
 fModified := memo1.Modified;
 Statusbar.Panels[1].Text := ModifiedStrs[fModified];
 Statusbar.Panels[2].Text := InsertModeStrs[fInsertMode];
 if assigned(fOnStatusChange) then
    fOnStatusChange(self);
  // Modified property has changed
(*  if Changes * [scAll, scModified] <> [] then
    Statusbar.Panels[1].Text := ModifiedStrs[SynEditor.Modified];
  // selection has changed
  if Changes * [scAll, scSelection] <> [] then
    cbExportSelected.Enabled := SynEditor.SelAvail;
  // select highlighter attribute at caret
  if (SynEditor.Highlighter <> nil) and (Changes * [scAll, scCaretX, scCaretY] <> [])
  then begin
    if not SynEditor.GetHighlighterAttriAtRowCol(SynEditor.CaretXY, Token,
      Attri)
    then
      Attri := SynEditor.Highlighter.WhitespaceAttribute;
    if Assigned(Attri) then begin
      cbxAttrSelect.ItemIndex := cbxAttrSelect.Items.IndexOf(Attri.Name);
      cbxAttrSelectChange(Self);
    end;
  end;  *)
end;

function TMDIChild.Empty: boolean;
var
  i,co : integer;
begin
  result:=true;
  co:=Memo1.Lines.Count;
//  if Assigned(Memo1.Lines[i]) then
    for i :=0 to  co - 1 do
      if Memo1.Lines[i] <> '' then
      begin
        result := FALSE;
        break;
      end;
end;


procedure TMDIChild.GoToLineExecute;
var
  tmpS:String;
  ln : integer;
begin
  tmpS:=InputBox('Goto line number...','Please enter line number','');
  ln := strtointdef(tmpS,-1);
  if ln =-1 then exit;
//  Memo1.Topline:=ln;
  Row:= ln;
 if assigned(fOnStatusChange) then
    fOnStatusChange(self);
end;

function TMDIChild.GetCanDo(Index: integer): boolean;
begin
  case index of
    cfCanCut        :  Result:=Memo1.SelAvail and not ReadOnly;
    cfCanCopy       :  Result:=Memo1.SelAvail;
    cfCanPaste      :  Result:=Memo1.CanPaste;
    cfCanUnDo       :  Result:=Memo1.CanUndo;
    cfCanRedo       :  Result:=Memo1.CanRedo;
    cfCanSelectAll,
    cfCanGoTo       :  Result:=not Empty;
    cfCanFind       :  Result:=not Empty;
    cfCanFindPrev,
    cfCanFindNext   :  Result:=not Empty  and (st <> '');
    cfCanReplace    :  Result:=not Empty and not Readonly;

    cfCanSave       :  Result:=Modified or (FileName = '') ;
    cfCanSaveAs     :  Result:=True;
    cfCanPrint      :  Result:=True;
    cfCanClose      : ;
    cfInsHist       :  Result:=True;
  end;//case
end;

procedure TMDIChild.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
//    FileNewCmd.Enabled:=
//    FileOpenCmd.Enabled:=
//    FileSaveCmd.Enabled:=
//    FilePrintCmd.Enabled:=
//    FileExitCmd.Enabled:=
//    FileSaveAsCmd.Enabled:=
    AcEditundo.Enabled:= GetCanDo(cfCanUndo);
    AcEditcut.Enabled:=GetCanDo(cfCanCut);
    AcEditcopy.Enabled:=GetCanDo(cfCanCopy);
    AcEditpaste.Enabled:=GetCanDo(cfCanPaste);
    AcInsertHist.Enabled:=GetCanDO(cfInsHist);
//    AcEditFont.Enabled:=
//    AcEditRedo.Enabled:=GetCanDo();
    AcGoto.Enabled:=GetCanDo(cfcanGoto);
//    AcSelectAll.Enabled:=
//    AcEditFind.Enabled:=GetCanDo(cfcanFind);
    AcReFind.Enabled:=GetCanDo(cfCanFindNext);
    AcEditReplace.Enabled:=GetCanDo(cfcanReplace);
    handled :=true;
end;




function TMDIChild.ExecAction(Ac: word): boolean;
begin
  Result:=true;
  case Ac of
    cfCanCut    :  Memo1.CutToClipboard;
    cfCanCopy   :  Memo1.CopyToClipboard;
    cfCanPaste  :  Memo1.PasteFromClipboard ;
    cfCanUnDo   :  Memo1.Undo ;
    cfCanRedo   :  Memo1.Redo;
    cfCanSelectAll : Memo1.SelectAll ;
    cfCanGoTo      : GoToLineExecute;
    cfInsHist   :  DoInsertHistory();

    cfCanFind   :  ShowSearchDialog(SearchActionFind);
    cfCanFindNext  : ShowSearchDialog(SearchActionReFind);
    cfCanReplace   : ShowSearchDialog(SearchActionReplace)
    (*
    cfCanFindPrev,
      Result:=not Empty { and (gsSearchText <> '')};
    cfCanReplace    :  Result:=not Empty and not Readonly;


    cfCanSave    :   Result:=Modified or (FileName = '') ;
    cfCanSaveAs  :   Result:=True;
    cfCanPrint   :   Result:=True;
    cfCanClose   : ;
    *)
  end;//case
end;

procedure TMDIChild.SetRow(const Value: integer);
begin
//Memo1.caretX:=1;
 Memo1.caretXY:=Point(1,Value);
end;

function TMDIChild.ShowSearchDialog(mode: integer):TFrReplaceAction;
const
 gbSearchTextAtCaret:boolean=true;
begin
//  SearchBackwards := gbSearchBackwards;
//  SearchCaseSensitive := gbSearchCaseSensitive;
//  SearchFromCursor := gbSearchFromCaret;
//  SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
//    SearchText := gsSearchText;
  if gbSearchTextAtCaret then
  begin
    // if something is selected search for that text
    if Memo1.SelAvail and (Memo1.BlockBegin.Y = Memo1.BlockEnd.Y)
    then
      St := Memo1.SelText
    else
//        St := Memo1.GetWordAtRowCol(Memo1.CaretXY);
  end;
//    SearchTextHistory := gsSearchTextHistory;
{    if AReplace then with dlg as TTextReplaceDialog do begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
    end;   }
//    SearchWholeWords := gbSearchWholeWords;
  while true do
  begin
    if mode = SearchActionFind then
      result:= ShowSearchDlg(St,Rt,Searchopt, 1)
    else if mode = SearchActionReplace then
      result:= ShowSearchDlg(St,Rt,Searchopt, 3);

    case result of
      fraCancel:     exit;
      fraFind:       mode := SearchActionFind;
      fraReplace,
      fraReplaceAll: mode := SearchActionReplace;
    end;

    if (frCommands in SearchOpt) or (frHowTo in SearchOpt) then
      aMainForm.SpecialSearch(St, SearchOpt)
    else
    begin
      EdSearchOptions:=[] ;
      if (frMatchCase in Searchopt)    then Include(EdSearchOptions, mwsoMatchCase);
      if (frWholeWord in Searchopt)    then Include(EdSearchOptions, mwsoWholeWord);
      if (frSelectedOnly in Searchopt) then Include(EdSearchOptions, mwsoSelectedOnly);
      if (frBackwards in Searchopt)    then Include(EdSearchOptions, mwsoBackwards);
      if (frEntireScope in Searchopt)  then Include(EdSearchOptions, mwsoEntireScope);
      if (result = fraReplace)         then Include(EdSearchOptions, mwsoReplace);
      if (result = fraReplaceAll)      then Include(EdSearchOptions, mwsoReplaceAll);
      DoSearchReplaceText(false,false);
    end;
  end;
end;

procedure TMDIChild.DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
var
  count: integer;
begin
{  if AReplace then
    Options := [mwsoPrompt, mwsoReplace, mwsoReplaceAll]
  else
    Options := [];  }
  count := Memo1.SearchReplace(St, Rt, EdSearchOptions);
  if count = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    showmsg('Search string not found');
    if mwsoBackwards in EdSearchOptions then
      Memo1.BlockEnd := Memo1.BlockBegin
    else
      Memo1.BlockBegin := Memo1.BlockEnd;
    Memo1.CaretXY := Memo1.BlockBegin;
    StatusBar.Panels[3].Text := 'Search string not found';
    exit;
  end;
  if mwsoReplaceAll in EdSearchOptions then
    StatusBar.Panels[3].Text := format('Replaced : %d', [count])
  else
    StatusBar.Panels[3].Text := '';
end;

procedure TMDIChild.FormCreate(Sender: TObject);
const
  def: TFormDefaults = (Section: 'Editor';
                        Top: 20; Left: 560;
                        Width: 500; Height: 500;
                        Maximize: false);
begin
  Searchopt :=[];
  st:='';
  Rt:='';
  OInifile.LoadForm(self, def);
end;

procedure TMDIChild.memo1SpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
//  if CurrentErrorLine< 0 then exit;
  Special:= (fCurrentErrorLine=Line);
  if special then
  begin
    FG := clWhite;
    BG := clRed
  end;
end;


procedure TMDIChild.SetCurrentLine(Value: integer);
begin
//  if fCurrentLine <> Value then
//  begin
    Memo1.InvalidateLine(fCurrentLine);
    fCurrentLine := Value;
    if (fCurrentLine > 0) and (Memo1.CaretY <> fCurrentLine) then
      Memo1.CaretXY := Point(1, fCurrentLine);
    Memo1.InvalidateLine(fCurrentLine);
//  end;
end;


procedure TMDIChild.SetCurrentErrorLine(const Value: integer);
begin
  fCurrentErrorLine := Value;
  if fCurrentErrorLine > -1 then
    CurrentLine:=Value;
  Memo1.Invalidate;
end;

procedure TMDIChild.FileExitCmdExecute(Sender: TObject);
begin
  try
    if not CheckFileSave then exit;
  except
    Exit;
  end;
  // if not OInifile.SectionExists('Editor') then
  OInifile.SaveCurrentForm(Self,'Editor');
  Child.Destroy;
  Child:=nil;
  UMain.aMainForm.booUMainEditorFocused:=True;
end;

procedure TMDIChild.FileCloseCmdExecute(Sender: TObject);
begin
  Close;
  UMain.aMainForm.booUMainEditorFocused:=True;
end;

procedure TMDIChild.SetFileName(const FileName: String);
begin
  FFileName := FileName;
  Caption := Format('%s - %s', [FileName, Application.Title]);
end;


//procedure TMDIChild.CheckFileSave;
Function TMDIChild.CheckFileSave(): Boolean;
var
  SaveResp: Integer;
begin
  Result := True;
  if not Memo1.Modified then Exit;
  SaveResp := MessageDlg(Format('Save changes to %s?', [FFileName]),
    mtConfirmation, mbYesNoCancel, 0);
  case SaveResp of
    idYes: FileSave(Self);
    idNo: {Nothing};
    idCancel: Result := False; //Abort;
  end;
end;


procedure TMDIChild.FileNew(Sender: TObject);
begin
  SetFileName('Untitled');
  Memo1.Lines.Clear;
  Memo1.Modified := False;
  SetModified(False);
end;

procedure TMDIChild.PerformFileOpen(const AFileName: string);
begin
  Memo1.Lines.LoadFromFile(AFileName);
  SetFileName(AFileName);
  Memo1.SetFocus;
  Memo1.Modified := False;
  SetModified(False);
end;

procedure TMDIChild.FileOpen(Sender: TObject);
var
 fn : string;
begin
  CheckFileSave;
   if GetOpenfilename(fn,EpiProgramFilter) then
  begin
    PerformFileOpen(fn);
//    Memo1.ReadOnly := ofReadOnly in OpenDialog.Options;
  end;
end;

procedure TMDIChild.FileSave(Sender: TObject);
begin
  if (FFileName = 'Untitled') or  (FFileName = '') then
    FileSaveAs(Sender)
  else
  begin
    Memo1.Lines.SaveToFile(FFileName);
    Memo1.Modified := False;
    SetModified(False);
  end;
end;

procedure TMDIChild.FileSaveAs(Sender: TObject);
var
 fn : string;
begin
  if GetSaveFileName(fn,EpiProgramFilter,'.prg') then
  begin
    if FileExists(fn) then
      if MessageDlg(Format('OK to overwrite %s', [fn]),
        mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;
    Memo1.Lines.SaveToFile(fn);
    SetFileName(fn);
    Memo1.Modified := False;
    SetModified(False);
  end else abort;
end;

procedure TMDIChild.FilePrint(Sender: TObject);
var
  options: TmwPrintOptions;
  old_print_options: TPrintDialogOptions;
  memo2 : Trichedit;
  opt: TEpiOption;

begin
  old_print_options := aMainForm.PrintDialog.Options;
  aMainForm.PrintDialog.Options := [poSelection, poWarning];

  if aMainform.PrintDialog.Execute then
  begin
    Options.Header := nil;
    Options.Footer := nil;
    if dm.GetOptionValue('EDITOR PRINT INFO', opt) and (opt.Value = 'ON') then
    begin
      Options.Header := TStringList.Create();
      Options.Footer := TStringList.Create();
      Options.Header.Add(GetCurrentDir + ' ' + Self.Caption);
      Options.Footer.Add(DateTimeToStr(Now))
    end;
    Options.SelectedOnly := aMainForm.PrintDialog.PrintRange = prSelection;
    Options.Highlighted := false;
    Options.WrapLongLines := True;
    Options.IgnoreColors := true;
    Options.Copies := aMainform.PrintDialog.Copies;
    Options.MarginUnits := muMillimeters;
    Options.Margins.Left := 20;
    Options.Margins.Top := 10;
    Options.Margins.Bottom := 10;
    Options.Margins.Right := 10;
    Options.PrintRange := Rect(0,0,0,0);
    memo1.Print(nil,options);
  end;
  aMainForm.PrintDialog.Options := old_print_options;
end;

procedure TMDIChild.SetModified(Value: Boolean);
begin
  if Value then StatusBar.Panels[1].Text := 'Modified'
  else StatusBar.Panels[1].Text := '';
end;


procedure TMDIChild.EditRedo(Sender: TObject);
begin
     ExecAction(cfCanRedo);
end;


procedure TMDIChild.EditUndo(Sender: TObject);
begin
     ExecAction(cfCanUnDo);
end;

procedure TMDIChild.EditCut(Sender: TObject);
begin
     ExecAction(cfCanCut);
end;

procedure TMDIChild.EditCopy(Sender: TObject);
begin
     ExecAction(cfCanCopy);
end;

procedure TMDIChild.EditPaste(Sender: TObject);
begin
     ExecAction(cfCanPaste);
end;

procedure TMDIChild.EditSelectAll(Sender: TObject);
begin
     ExecAction(cfCanSelectAll);
end;
procedure TMDIChild.EditGoToLin(Sender: TObject);
begin
     ExecAction(cfCanGoTo);
end;

procedure TMDIChild.EditFind(Sender: TObject);
begin
     ExecAction(cfCanFind);
end;

procedure TMDIChild.EditInsertHistory(Sender: TObject);
begin
   ExecAction(cfInsHist);
end;


procedure TMDIChild.AcWindowCommandsExecute(Sender: TObject);
var
 s : string;
begin
 aMainForm.AcShowWindowCommandsLeft(aMainForm.cmdTree);
 // insert here text from memo1.
 memo1.SelText := s+ ' ';
end;


procedure TMDIChild.AcActivateVarExecute(Sender: TObject);
var
 s : string;
begin
 if aMainForm.Lvars.Items.count=0 then exit;
   aMainForm.AcShowVariablesLeft(aMainForm.Lvars);
// s:= AcShowVariablesLeft(aMainForm.Lvars);
 memo1.SelText := trim(copy(s,1,10));
end;



(*
procedure TMDIChild.SetupRuler;
var
  I: Integer;
  S: String;
begin
  SetLength(S, 201);
  I := 1;
  while I < 200 do
  begin
    S[I] := #9;
    S[I+1] := '|';
    Inc(I, 2);
  end;
  Ruler.Caption := S;
end;

procedure TMDIChild.SetEditRect;
var
  R: TRect;
begin
  with Memo1 do
  begin
    R := Rect(GutterWid, 0, ClientWidth-GutterWid, ClientHeight);
    SendMessage(Handle, EM_SETRECT, 0, Longint(@R));
  end;
end;

{ Event Handlers }


procedure TMDIChild.ShowHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then
  begin
    StatusBar.SimplePanel := True;
    StatusBar.SimpleText := Application.Hint;
  end
  else StatusBar.SimplePanel := False;
end;


procedure TMDIChild.SelectFont(Sender: TObject);
begin
  FontDialog1.Font.Assign(Memo1.SelAttributes);
  if FontDialog1.Execute then
    CurrText.Assign(FontDialog1.Font);
  SelectionChange(Self);
  Memo1.SetFocus;
end;

procedure TMDIChild.RulerResize(Sender: TObject);
begin
  RulerLine.Width := Ruler.ClientWidth - (RulerLine.Left*2);
end;

procedure TMDIChild.FormResize(Sender: TObject);
begin
  SetEditRect;
  SelectionChange(Sender);
end;

procedure TMDIChild.FormPaint(Sender: TObject);
begin
  SetEditRect;
end;

procedure TMDIChild.BoldButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if BoldButton.Down then
    CurrText.Style := CurrText.Style + [fsBold]
  else
    CurrText.Style := CurrText.Style - [fsBold];
end;

procedure TMDIChild.ItalicButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if ItalicButton.Down then
    CurrText.Style := CurrText.Style + [fsItalic]
  else
    CurrText.Style := CurrText.Style - [fsItalic];
end;

procedure TMDIChild.FontSizeChange(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Size := StrToInt(FontSize.Text);
end;

procedure TMDIChild.AlignButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  Memo1.Paragraph.Alignment := TAlignment(TControl(Sender).Tag);
end;

procedure TMDIChild.FontNameChange(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Name := FontName.Items[FontName.ItemIndex];
end;

procedure TMDIChild.UnderlineButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if UnderlineButton.Down then
    CurrText.Style := CurrText.Style + [fsUnderline]
  else
    CurrText.Style := CurrText.Style - [fsUnderline];
end;

procedure TMDIChild.BulletsButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  Memo1.Paragraph.Numbering := TNumberingStyle(BulletsButton.Down);
end;

{ Ruler Indent Dragging }

procedure TMDIChild.RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragOfs := (TLabel(Sender).Width div 2);
  TLabel(Sender).Left := TLabel(Sender).Left+X-FDragOfs;
  FDragging := True;
end;

procedure TMDIChild.RulerItemMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging then
    TLabel(Sender).Left :=  TLabel(Sender).Left+X-FDragOfs
end;

procedure TMDIChild.FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  Memo1.Paragraph.FirstIndent := Trunc((FirstInd.Left+FDragOfs-GutterWid) / RulerAdj);
  LeftIndMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TMDIChild.LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  Memo1.Paragraph.LeftIndent := Trunc((LeftInd.Left+FDragOfs-GutterWid) / RulerAdj)-Memo1.Paragraph.FirstIndent;
  SelectionChange(Sender);
end;

procedure TMDIChild.RightIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  Memo1.Paragraph.RightIndent := Trunc((Ruler.ClientWidth-RightInd.Left+FDragOfs-2) / RulerAdj)-2*GutterWid;
  SelectionChange(Sender);
end;

procedure TMDIChild.UpdateCursorPos;
var
  CharPos: TPoint;
begin
  CharPos.Y := SendMessage(Memo1.Handle, EM_EXLINEFROMCHAR, 0,
    Memo1.SelStart);
  CharPos.X := (Memo1.SelStart -
    SendMessage(Memo1.Handle, EM_LINEINDEX, CharPos.Y, 0));
  Inc(CharPos.Y);
  Inc(CharPos.X);
  StatusBar.Panels[0].Text := Format('Line: %3d   Col: %3d', [CharPos.Y, CharPos.X]);
end;

procedure TMDIChild.FormShow(Sender: TObject);
begin
  UpdateCursorPos;
  DragAcceptFiles(Handle, True);
  RichEditChange(nil);
  Memo1.SetFocus;
  { Check if we should load a file from the command line }
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    PerformFileOpen(ParamStr(1));
end;

procedure TMDIChild.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
    begin
      CheckFileSave;
      PerformFileOpen(CFileName);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;


*)

procedure TMDIChild.AcRunSelectedExecute(Sender: TObject);
begin
  RunPgm(true);
end;


procedure TMDIChild.AcRunPgmExecute(Sender: TObject);
begin
  RunPgm(false);
end;


function TMDIChild.RunPgm(Selected:boolean):boolean;
var
fn, s :string;
i,co, LineAfter :integer;
fs : TFileStream;
begin
  if Empty then exit;
  fn := SMGetTempfileName('.Tmp');
  if fn='' then exit;
  try
    if Selected then
    begin
      if Memo1.SelAvail then
      begin
        S := Memo1.SelText;
        if S='' then exit;
      end else
        s := Memo1.LineText;
      fs := TFileStream.create(fn,fmOpenWrite);
      try
        fs.Write(pointer(s)^, length(s));
      finally
        fs.Free;
      end;
    end else
      Memo1.Lines.SaveToFile(fn);
  except
    raise;
    exit;
  end;

  if not selected then LineAfter :=1
     else LineAfter := memo1.CaretY;
  CurrentErrorLine :=-1;
  CurrentLine := 1;
  try
    aMainForm.doCommand('Run "'+ fn + '"');
    application.processmessages;
  finally
    CurrentErrorLine:=aMainform.CurrentErrorLine;
    Currentline := 1;
    Currentline := LineAfter;
    show;
  end;
end;




procedure TMDIChild.AcRefindExecute(Sender: TObject);
begin
 ExecAction(cfCanFindNext);
end;

procedure TMDIChild.AcStopRunExecute(Sender: TObject);
begin
  aMainform.AcCancelExecute(self);
end;

function TMDIChild.DoInsertHistory(): boolean;
var
  History: TStringList;
  HistString: String;
  i: integer;

begin
  History := aMainForm.CmdEdit.History;
  HistString := '';

  for i := 0 to History.Count -1 do
  begin
    HistString := HistString + #13 + History.Strings[i];
  end;
  memo1.DoCopyToClipboard(HistString);
  if HistString <> '' then
    memo1.PasteFromClipboard;
end;


procedure TMDIChild.AcEditReplaceExecute(Sender: TObject);
begin
     ExecAction(cfCanReplace);
end;

procedure TMDIChild.Savewindowsposition1Click(Sender: TObject);
begin
  OInifile.SaveCurrentForm(self, 'Editor');
end;

procedure TMDIChild.About1Click(Sender: TObject);
begin
  dm.help(memo1.SelText);
end;

end.
