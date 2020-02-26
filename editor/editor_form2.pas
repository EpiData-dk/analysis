unit editor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, auto_position_form, ComCtrls, Dialogs, UITypes,
  editor_page, Forms;

type

  { TEditorForm2 }

  TEditorForm2 = class(TCustomAutoPositionForm)
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
    // Accessors
    function ActiveEditorPage: TEditorPage;
  private
    // Tab
    procedure AddNewTab;
    function CloseTab(EditorPage: TEditorPage): boolean;
  private
    // Pagecontrol
    FPageControl: TPageControl;
    procedure PageChangeEvent(Sender: TObject);
    procedure PageChangingEvent(Sender: TObject; var AllowChange: Boolean);
    procedure PageStatusChange(Sender: TObject);
    procedure PageCloseTabEvent(Sender: TObject);
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
  end;

implementation

uses
  Controls, SynEdit, Menus, ActnList, LCLType, epimiscutils, LazFileUtils, LazUTF8Classes;

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
  TopMenuItem.Add(CreateActionAndMenuItem('&New',        @NewActionExecute,  ShortCut(VK_N, [ssCtrl])));
  TopMenuItem.Add(CreateActionAndMenuItem('&Open...',    @OpenActionExecute, ShortCut(VK_O, [ssCtrl])));
  // Open Recent....
  TopMenuItem.Add(CreateActionAndMenuItem('&Save',       @SaveActionExecute, ShortCut(VK_S, [ssCtrl])));
  TopMenuItem.Add(CreateActionAndMenuItem('Save &As...', @SaveAsActionExecute, ShortCut(VK_S, [ssCtrl, ssShift])));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('&Font...',    @NoneActionExecute, 0));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('&Close Tab',  @CloseTabActionExecute, ShortCut(VK_W, [ssCtrl])));
  TopMenuItem.Add(CreateActionAndMenuItem('&Quit',       @NoneActionExecute, ShortCut(VK_Q, [ssShift])));
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
  TopMenuItem.Add(CreateActionAndMenuItem('Run Selected', @NoneActionExecute, 0));
  TopMenuItem.Add(CreateActionAndMenuItem('Run All',      @NoneActionExecute, 0));
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
  //
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
    end;

  UpdateStatusBar;
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

function TEditorForm2.ActiveEditorPage: TEditorPage;
begin
  result := TEditorPage(FPageControl.ActivePage);
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

