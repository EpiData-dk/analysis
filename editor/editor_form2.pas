unit editor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, auto_position_form, ComCtrls, Dialogs, UITypes,
  editor_page, Forms, Menus, executor, history, outputcreator, ast, Token;

type

  { TEditorForm2 }

  TEditorForm2 = class(TCustomAutoPositionForm)
  private
    FExecutor: TExecutor;
    FHistory: THistory;
    FOutputCreator: TOutputCreator;
    procedure SetExecutor(AValue: TExecutor);
    procedure SetHistory(AValue: THistory);
    procedure SetOutputCreator(AValue: TOutputCreator);
    procedure TutorialChange(Sender: TObject);
  private
    // Form
    procedure EditorClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure EditorCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    // Actions
    procedure CloseTabActionExecute(Sender: TObject);
    procedure FontActionExecute(Sender: TObject);
    procedure NewActionExecute(Sender: TObject);
    procedure NoneActionExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure OpenRecentExecute(Sender: TObject);
    procedure RunAllActionExecute(Sender: TObject);
    procedure RunSelectedActionExecute(Sender: TObject);
  private
    // File I/O
    FOpenDialog: TOpenDialog;
    FSaveDialog: TSaveDialog;
    function  DoOpenDialog: TStrings;
    procedure DoOpenFile(FileName: UTF8String);
    procedure DoOpenFiles(FileNames: TStrings);
    function  DoSaveDialog: UTF8String;
    function  DoSaveFile(FileName: UTF8String): boolean;
  private
    // Recent files
    FRecentFilesSubMenu: TMenuItem;
    procedure UpdateRecentFiles;
    procedure AsyncOpenRecent(Data: PtrInt);
  private
    // Pagecontrol
    FPageControl: TPageControl;
    procedure PageChangeEvent(Sender: TObject);
    procedure PageChangingEvent(Sender: TObject; var AllowChange: Boolean);
    procedure PageStatusChange(Sender: TObject);
    procedure PageCloseTabEvent(Sender: TObject);
    function ActiveEditorPage: TEditorPage;
    function GetEditorPage(Index: Integer): TEditorPage;
    procedure AddNewTab;
    function CloseTab(EditorPage: TEditorPage): boolean;
  private
    // Statusbar
    FStatusBar: TStatusBar;
    procedure UpdateStatusBar;
  private
    // Create components
    procedure CreateComponents;
    procedure CreateMainMenu;
    procedure CreateStatusBar;
  public
    constructor Create(AOwner: TComponent); override;
    procedure OpenFiles(FileNames: TStrings);
    procedure OpenFile(FileName: UTF8String);
    property Executor: TExecutor read FExecutor write SetExecutor;
    property History: THistory read FHistory write SetHistory;
    property OutputCreator: TOutputCreator read FOutputCreator write SetOutputCreator;
  end;

implementation

uses
  Controls, SynEdit, ActnList, LCLType, epimiscutils, LazFileUtils, LazUTF8Classes,
  ana_procs, VirtualTrees, ana_globals, parser;

{ TEditorForm2 }

procedure TEditorForm2.CreateComponents;
begin
  CreateMainMenu;
  CreateStatusBar;

  FPageControl := TPageControl.Create(Self);
  FPageControl.Align := alClient;
  FPageControl.Parent := Self;
  FPageControl.Options := [nboShowCloseButtons, nboShowAddTabButton, nboDoChangeOnSetIndex];
  FPageControl.OnCloseTabClicked := @PageCloseTabEvent;
  FPageControl.OnChange := @PageChangeEvent;
  FPageControl.OnChanging := @PageChangingEvent;

  FOpenDialog := TOpenDialog.Create(self);
  FOpenDialog.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofViewDetail];
  FOpenDialog.Title := 'Open existing file';
  FOpenDialog.Filter := GetEpiDialogFilter([dfPGM, dfAll]);

  FSaveDialog := TSaveDialog.Create(self);
  FSaveDialog.Options := [ofOverwritePrompt, ofEnableSizing, ofViewDetail];
  FSaveDialog.Title := 'Save file as';
  FSaveDialog.Filter := GetEpiDialogFilter([dfPGM, dfAll]);
  FSaveDialog.DefaultExt := Copy(GetEpiDialogFilterExt([dfPGM]), 2, 4);
end;

procedure TEditorForm2.CreateMainMenu;
var
  MainMenu: TMainMenu;
  TopMenuItem: TMenuItem;

  function CreateActionAndMenuItem(Caption: TCaption; OnExecute: TNotifyEvent; ShortCut: TShortCut): TMenuItem;
  var
    TheAction: TAction;
  begin
    TheAction := TAction.Create(self);
    TheAction.Caption := Caption;
    TheAction.OnExecute := OnExecute;
    TheAction.ShortCut := ShortCut;

    Result := TMenuItem.Create(Self);
    Result.Action := TheAction;
  end;

  function CreateDivider(): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := '-';
  end;

begin
  MainMenu := TMainMenu.Create(Self);

  // File:
  TopMenuItem := TMenuItem.Create(Self);
  TopMenuItem.Caption := '&File';
  TopMenuItem.Add(CreateActionAndMenuItem('&New',        @NewActionExecute,  ShortCut(VK_N, [ssCtrlOs])));
  TopMenuItem.Add(CreateActionAndMenuItem('&Open...',    @OpenActionExecute, ShortCut(VK_O, [ssCtrlOs])));

  FRecentFilesSubMenu := TMenuItem.Create(Self);
  FRecentFilesSubMenu.Caption := 'Open Recent...';
  TopMenuItem.Add(FRecentFilesSubMenu);
  TopMenuItem.Add(CreateActionAndMenuItem('&Save',       @SaveActionExecute, ShortCut(VK_S, [ssCtrlOs])));
  TopMenuItem.Add(CreateActionAndMenuItem('Save &As...', @SaveAsActionExecute, ShortCut(VK_S, [ssCtrlOs, ssShift])));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('&Font...',    @NoneActionExecute, 0));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('&Close Tab',  @CloseTabActionExecute, ShortCut(VK_W, [ssCtrlOs])));
  TopMenuItem.Add(CreateActionAndMenuItem('&Quit',       @QuitActionExecute, ShortCut(VK_Q, [ssShift])));
  MainMenu.Items.Add(TopMenuItem);

  // Edit
  TopMenuItem := TMenuItem.Create(Self);
  TopMenuItem.Caption := 'Edit';
  TopMenuItem.Add(CreateActionAndMenuItem('Insert History',               @NoneActionExecute, ShortCut(VK_I, [ssAlt])));
  TopMenuItem.Add(CreateActionAndMenuItem('Insert Set Options',           @NoneActionExecute, 0));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('Cut',                          @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Copy',                         @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Paste',                        @NoneActionExecute, 0));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('Preferences... (startup.pgm)', @NoneActionExecute, 0));
  MainMenu.Items.Add(TopMenuItem);

  // Search
  TopMenuItem := TMenuItem.Create(Self);
  TopMenuItem.Caption := '&Search';
  TopMenuItem.Add(CreateActionAndMenuItem('Find...',       @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Find Next',     @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Find Previous', @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Replace...',    @NoneActionExecute, 0));
  MainMenu.Items.Add(TopMenuItem);

  // Run
  TopMenuItem := TMenuItem.Create(Self);
  TopMenuItem.Caption := '&Run';
  TopMenuItem.Add(CreateActionAndMenuItem('Run Selected', @RunSelectedActionExecute, ShortCut(VK_D, [ssCtrlOS])));
  TopMenuItem.Add(CreateActionAndMenuItem('Run All',      @RunAllActionExecute, ShortCut(VK_R, [ssCtrlOS])));
  MainMenu.Items.Add(TopMenuItem);

  // Window
  TopMenuItem := TMenuItem.Create(Self);
  TopMenuItem.Caption := '&Window';
  TopMenuItem.Add(CreateActionAndMenuItem('Show Command Tree', @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Variables',         @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Command Prompt',    @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Editor',            @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Browse',            @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('History',           @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Dataset List',      @NoneActionExecute, 0));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('Default Windowing', @NoneActionExecute, 0));
  MainMenu.Items.Add(TopMenuItem);

  // Help
  TopMenuItem := TMenuItem.Create(Self);
  TopMenuItem.Caption := '&Help';
  TopMenuItem.Add(CreateActionAndMenuItem('Tutorials (Local)',        @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Tutorials (EpiData Wiki)', @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Tutorials On Web',         @NoneActionExecute, 0));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('Commands Overview',        @NoneActionExecute, 0));
  MainMenu.Items.Add(TopMenuItem);

  Self.Menu := MainMenu;
end;

procedure TEditorForm2.AddNewTab;
var
  EditorPage: TEditorPage;
begin
  BeginFormUpdate;

  EditorPage := TEditorPage.Create(self);
  EditorPage.PageControl := FPageControl;
  EditorPage.Executor := FExecutor;
  EditorPage.History  := FHistory;
  EditorPage.OutputCreator := FOutputCreator;

  FPageControl.ActivePage := EditorPage;
  ActiveControl := EditorPage.Editor;
  UpdateStatusBar;

  EndFormUpdate;
end;

function TEditorForm2.CloseTab(EditorPage: TEditorPage): boolean;
var
  S: TCaption;
  Res: TModalResult;
begin
  if (EditorPage.Modified) then
    begin
      FPageControl.ActivePage := EditorPage;

      S := EditorPage.Caption;
      Delete(S, 1, 1);

      Res := MessageDlg('Warning',
                        'Editor Content (' + S + ') not saved. Save before closing ?',
                        mtWarning,
                        mbYesNoCancel, 0
             );

      case Res of
        mrYes:
          if (not DoSaveFile(EditorPage.FileName)) then
            Exit(false);

        mrCancel:
          Exit(false);

        mrNo:
          ;
      end;
    end;

  EditorPage.Free;
  if (FPageControl.PageCount = 0) then
    AddNewTab;
end;

procedure TEditorForm2.AsyncOpenRecent(Data: PtrInt);
begin
  DoOpenFile(TAction(Data).Caption);
end;

procedure TEditorForm2.SetExecutor(AValue: TExecutor);
var
  i: Integer;
begin
  if FExecutor = AValue then Exit;
  FExecutor := AValue;

  for i := 0 to FPageControl.PageCount - 1 do
    GetEditorPage(i).Executor := AValue;

  FExecutor.SetOptions[ANA_SO_TUTORIAL_FOLDER].AddOnChangeHandler(@TutorialChange);
end;

procedure TEditorForm2.RunAllActionExecute(Sender: TObject);
begin

end;

procedure TEditorForm2.RunSelectedActionExecute(Sender: TObject);
begin
end;

procedure TEditorForm2.SetHistory(AValue: THistory);
var
  i: Integer;
begin
  if FHistory = AValue then Exit;
  FHistory := AValue;

  for i := 0 to FPageControl.PageCount - 1 do
    GetEditorPage(i).History := AValue;
end;

procedure TEditorForm2.SetOutputCreator(AValue: TOutputCreator);
var
  i: Integer;
begin
  if FOutputCreator = AValue then Exit;
  FOutputCreator := AValue;

  for i := 0 to FPageControl.PageCount - 1 do
    GetEditorPage(i).OutputCreator := OutputCreator;
end;

procedure TEditorForm2.TutorialChange(Sender: TObject);
begin

end;

procedure TEditorForm2.EditorClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  case CloseAction of
    caNone: ;

    caHide,
    caFree:
      Application.ReleaseComponent(Self);

    caMinimize: ;
  end;
end;

procedure TEditorForm2.EditorCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  CanClose := true;

  for i := FPageControl.PageCount - 1 downto 0 do
    begin
      CanClose := CloseTab(TEditorPage(FPageControl.Pages[i]));

      if (not CanClose) then
        Exit;
    end;
end;

procedure TEditorForm2.OpenRecentExecute(Sender: TObject);
begin
  Application.QueueAsyncCall(@AsyncOpenRecent, PtrInt(Sender));
end;

procedure TEditorForm2.CloseTabActionExecute(Sender: TObject);
begin
  CloseTab(ActiveEditorPage);
end;

procedure TEditorForm2.FontActionExecute(Sender: TObject);
begin
  //
end;

procedure TEditorForm2.NewActionExecute(Sender: TObject);
begin
  AddNewTab;
end;

procedure TEditorForm2.NoneActionExecute(Sender: TObject);
begin
  ShowMessage('Action not implemented yet!');
end;

procedure TEditorForm2.OpenActionExecute(Sender: TObject);
var
  FileNames: TStrings;
begin
  FileNames := DoOpenDialog;
  DoOpenFiles(FileNames);
  FileNames.Free;
end;

procedure TEditorForm2.QuitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TEditorForm2.SaveActionExecute(Sender: TObject);
begin
  DoSaveFile(ActiveEditorPage.FileName);
end;

procedure TEditorForm2.SaveAsActionExecute(Sender: TObject);
begin
  DoSaveFile('');
end;

function TEditorForm2.DoOpenDialog: TStrings;
begin
  Result := TStringListUTF8.Create;

  FOpenDialog.InitialDir := GetCurrentDirUTF8;
  if (FOpenDialog.Execute) then
    Result.Assign(FOpenDialog.Files)
end;

procedure TEditorForm2.DoOpenFile(FileName: UTF8String);
var
  FileNames: TStringListUTF8;
begin
  if (FileName = '') then exit;
  if (not FileExistsUTF8(FileName)) then exit;

  FileNames := TStringListUTF8.Create;
  FileNames.Add(FileName);

  DoOpenFiles(FileNames);

  FileNames.Free;
end;

procedure TEditorForm2.DoOpenFiles(FileNames: TStrings);
var
  FileName: String;
begin
  if (not Assigned(FileNames)) then Exit;
  if (FileNames.Count = 0) then exit;

  for FileName in FileNames do
    begin
      if (not FileExistsUTF8(FileName)) then Exit;

      if (ActiveEditorPage.Modified) or
         (ActiveEditorPage.FileName <> '')
      then
        AddNewTab;

      ActiveEditorPage.LoadFromFile(FileName);
      AddToRecent(FileName, GetRecentPGMIniFileName, RecentPGMFiles);
    end;

  UpdateStatusBar;
  UpdateRecentFiles;
end;

function TEditorForm2.DoSaveDialog: UTF8String;
begin
  Result := '';

  FSaveDialog.InitialDir := GetCurrentDirUTF8;
  if (FSaveDialog.Execute) then
    Result := FSaveDialog.FileName;
end;

function TEditorForm2.DoSaveFile(FileName: UTF8String): boolean;
begin
  if (FileName = '') then
    FileName := DoSaveDialog;

  if (FileName = '') then
    Exit(false);

  ActiveEditorPage.SaveToFile(FileName);

  result := true;
end;

procedure TEditorForm2.UpdateRecentFiles;
var
  i: Integer;
  A: TAction;
  Mi: TMenuItem;
begin
  LoadRecentFilesFromIni(GetRecentPGMIniFileName, RecentPGMFiles);

  FRecentFilesSubMenu.Visible := RecentPGMFiles.Count > 0;
  FRecentFilesSubMenu.Clear;

  for i := 0 to MaxRecentFiles - 1 do
  begin
    // Main menu
    A := TAction.Create(self);
    if (i < 9) then
      A.ShortCut := KeyToShortCut(VK_1 + i, [ssShift, ssCtrlOs]);
    A.OnExecute := @OpenRecentExecute;

    // Disable actions if the list of RecentPGMFiles is not long enough.
    if i >= RecentPGMFiles.Count then
    begin
      A.Enabled := false;
      Continue;
    end;

    A.Enabled := true;
    A.Caption := RecentPGMFiles[i];

    Mi := TMenuItem.Create(FRecentFilesSubMenu);
    Mi.Name := 'recent' + inttostr(i);
    Mi.Action := A;
    FRecentFilesSubMenu.Add(Mi);
  end;
end;

function TEditorForm2.ActiveEditorPage: TEditorPage;
begin
  result := TEditorPage(FPageControl.ActivePage);
end;

function TEditorForm2.GetEditorPage(Index: Integer): TEditorPage;
begin
  result := TEditorPage(FPageControl.Pages[Index]);
end;

procedure TEditorForm2.PageChangeEvent(Sender: TObject);
begin
  ActiveEditorPage.OnStatusChange := @PageStatusChange;
  UpdateStatusBar;
end;

procedure TEditorForm2.PageChangingEvent(Sender: TObject;
  var AllowChange: Boolean);
begin
  ActiveEditorPage.OnStatusChange := nil;
  AllowChange := true;
end;

procedure TEditorForm2.PageStatusChange(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TEditorForm2.PageCloseTabEvent(Sender: TObject);
begin
  CloseTab(TEditorPage(Sender));
end;

procedure TEditorForm2.UpdateStatusBar;
var
  Editor: TSynEdit;
begin
  Editor := ActiveEditorPage.Editor;

  FStatusBar.Panels[0].Text := Format('%d: %d', [Editor.CaretY, Editor.CaretX]);
  if ActiveEditorPage.Modified then
    FStatusBar.Panels[1].Text := 'Modified'
  else
    FStatusBar.Panels[1].Text := '';
  if Editor.InsertMode then
    FStatusBar.Panels[2].Text := 'Ins'
  else
    FStatusBar.Panels[2].Text := 'Ovr';
  FStatusBar.Panels[3].Text := ActiveEditorPage.FileName;
end;

procedure TEditorForm2.CreateStatusBar;
var
  StatusBarPanel: TStatusPanel;
begin
  FStatusBar := TStatusBar.Create(self);
  FStatusBar.Align := alBottom;
  FStatusBar.Parent := Self;
  FStatusBar.SimplePanel := false;
  FStatusBar.Height := 20;

  // Line/Col
  StatusBarPanel := FStatusBar.Panels.Add;
  StatusBarPanel.Width := 100;

  // Modified
  StatusBarPanel := FStatusBar.Panels.Add;
  StatusBarPanel.Width := 75;

  // Insert/Overwrite
  StatusBarPanel := FStatusBar.Panels.Add;
  StatusBarPanel.Width := 75;

  // Filename
  StatusBarPanel := FStatusBar.Panels.Add;
  StatusBarPanel.Width := 75;
end;

constructor TEditorForm2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := 'PGM Editor';

  CreateComponents;
  AddNewTab;

  OnCloseQuery := @EditorCloseQuery;
  OnClose := @EditorClose;

  // Because first page never triggers the OnChangeEvent
  PageChangeEvent(FPageControl);
  UpdateRecentFiles;
end;

procedure TEditorForm2.OpenFiles(FileNames: TStrings);
begin
  DoOpenFiles(FileNames);
end;

procedure TEditorForm2.OpenFile(FileName: UTF8String);
begin
  DoOpenFile(FileName);
end;

end.

