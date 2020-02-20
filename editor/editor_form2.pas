unit editor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, auto_position_form, ComCtrls, Dialogs, editor_page;

type

  { TEditorForm2 }

  TEditorForm2 = class(TCustomAutoPositionForm)
  private
    // Actions
    procedure AddNewTab;
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
    function  DoOpenDialog: TStrings;
    procedure DoOpenFile(FileName: UTF8String);
    procedure DoOpenFiles(FileNames: TStrings);
  private
    // Accessors
    function ActiveEditorPage: TEditorPage;
  private
    // Components
    FPageControl: TPageControl;
    FStatusBar: TStatusBar;
    procedure CreateComponents;
    procedure CreateMainMenu;
    procedure CreateStatusBar;
  public
    constructor Create(AOwner: TComponent); override;
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

  FOpenDialog := TOpenDialog.Create(self);
  FOpenDialog.Options := [ofAllowMultiSelect,ofFileMustExist,ofEnableSizing,ofViewDetail];
  FOpenDialog.Title := 'Open existing file';
  FOpenDialog.Filter := GetEpiDialogFilter([dfPGM, dfAll]);
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
  TopMenuItem.Add(CreateActionAndMenuItem('&Open...',    @NoneActionExecute, ShortCut(VK_O, [ssCtrl])));
  // Open Recent....
  TopMenuItem.Add(CreateActionAndMenuItem('&Save',       @NoneActionExecute, ShortCut(VK_S, [ssCtrl])));
  TopMenuItem.Add(CreateActionAndMenuItem('Save &As...', @NoneActionExecute, ShortCut(VK_S, [ssCtrl, ssShift])));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('&Font...',    @NoneActionExecute, 0));
  TopMenuItem.Add(CreateDivider());
  TopMenuItem.Add(CreateActionAndMenuItem('&Close Tab',  @NoneActionExecute, ShortCut(VK_W, [ssCtrl])));
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

  EndFormUpdate;
end;

procedure TEditorForm2.CloseTabActionExecute(Sender: TObject);
begin
  //
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
begin
  //
end;

procedure TEditorForm2.QuitActionExecute(Sender: TObject);
begin
  //
end;

procedure TEditorForm2.SaveActionExecute(Sender: TObject);
begin
  //
end;

procedure TEditorForm2.SaveAsActionExecute(Sender: TObject);
begin
  //
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
begin
  if (not Assigned(FileNames)) then Exit;
  if (FileNames.Count = 0) then exit;


end;

function TEditorForm2.ActiveEditorPage: TEditorPage;
begin
  result := TEditorPage(FPageControl.ActivePage);
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

  CreateComponents;
  AddNewTab;
end;

end.

