unit editor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, auto_position_form, ComCtrls;

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
    // Components
    PageControl: TPageControl;
    StatusBar: TStatusBar;
    procedure CreateComponents;
    procedure CreateMainMenu;
    procedure CreateStatusBar;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Controls, editor_page, SynEdit, Menus, ActnList, LCLType, Dialogs;

{ TEditorForm2 }

procedure TEditorForm2.CreateComponents;
begin
  CreateMainMenu;
  CreateStatusBar;

  PageControl := TPageControl.Create(Self);
  PageControl.Align := alClient;
  PageControl.Parent := Self;
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
  Sheet: TTabSheet;
  EditorPage: TEditorPage;
  Editor: TSynEdit;
begin
  BeginFormUpdate;

  EditorPage := TEditorPage.Create;

  Sheet := PageControl.AddTabSheet;
  Sheet.Caption := EditorPage.Caption;

  Editor := EditorPage.Editor;
  Editor.Parent := Sheet;

  PageControl.ActivePage := Sheet;

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

procedure TEditorForm2.CreateStatusBar;
var
  StatusBarPanel: TStatusPanel;
begin
  StatusBar := TStatusBar.Create(self);
  StatusBar.Align := alBottom;
  StatusBar.Parent := Self;
  StatusBar.SimplePanel := false;
  StatusBar.Height := 20;

  // Line/Col
  StatusBarPanel := StatusBar.Panels.Add;
  StatusBarPanel.Width := 100;

  // Modified
  StatusBarPanel := StatusBar.Panels.Add;
  StatusBarPanel.Width := 75;

  // Insert/Overwrite
  StatusBarPanel := StatusBar.Panels.Add;
  StatusBarPanel.Width := 75;

  // Filename
  StatusBarPanel := StatusBar.Panels.Add;
  StatusBarPanel.Width := 75;
end;

constructor TEditorForm2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  CreateComponents;
  AddNewTab;
end;

end.

