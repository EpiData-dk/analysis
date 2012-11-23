unit UHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HtmlView, ToolWin, ComCtrls, ActnList, ExtCtrls, Menus;

type
  THelpForm = class(TForm)
    ViewerPanel: TPanel;
    ActionList1: TActionList;
    hlpPrint: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Print1: TMenuItem;
    ToolBar1: TToolBar;
    tbPrint: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tbOpen: TToolButton;
    ToolButton5: TToolButton;
    tbPrev: TToolButton;
    tbNext: TToolButton;
    tbReload: TToolButton;
    hlpOpen: TAction;
    hlpPrev: TAction;
    hlpNext: TAction;
    hlpReload: TAction;
    hlpRepeatSearch: TAction;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    lphPopUp: TPopupMenu;
    Print2: TMenuItem;
    Open1: TMenuItem;
    Previous1: TMenuItem;
    Next1: TMenuItem;
    Reload1: TMenuItem;
    hlpCopy: TAction;
    Copy2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure hlpPrintExecute(Sender: TObject);
    procedure hlpRepeatSearchExecute(Sender: TObject);
    procedure hlpOpenExecute(Sender: TObject);
    procedure hlpPrevExecute(Sender: TObject);
    procedure hlpReloadExecute(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure hlpCopyExecute(Sender: TObject);
  private
    { Private declarations }
    OldHelpText: String;
    Active: boolean;
    Viewer: THTMLViewer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure LoadHelp(HelpText: STring);
    procedure LoadHelpFrom(HelpText, FileName: STring);
    property IsActive: boolean read Active;
    { Public declarations }
  end;

procedure ShowHelpForm(HelpText: String); overload;
procedure ShowHelpForm(HelpText, FileName: String); overload;
procedure ViewHelpForm(FileName: String);
procedure CloseHelpForm();
function HelpWindowHasFocus(): boolean;
function GetHelpformHandle(): TForm;

implementation

uses
  UMain, UInifile, UCmdProcessor, AnsDatatypes, UTranslation {, AgOpenDialog};

var
  HelpForm: THelpForm;

{$R *.dfm}

function GetHelpformHandle(): TForm;
begin
  result := HelpForm;
end;

procedure CloseHelpForm();
begin
  if Assigned(HelpForm) then
    FreeAndNil(HelpForm);
end;

procedure ShowHelpForm(HelpText: String);
var
  opt: TEpiOption;
begin
  if not assigned(HelpForm) then
  begin
    HelpForm := THelpForm.Create(Application);
  end;
  HelpForm.Show;
  if dm.GetOptionValue('VIEWER FONT SIZE', opt) then
    helpform.Viewer.DefFontSize:= StrToInt(opt.value)
  else
    helpform.Viewer.DefFontSize:= 10;
  if dm.GetOptionValue('VIEWER FONT NAME', opt) then
    helpform.Viewer.DefFontName := opt.Value
  else
    helpform.Viewer.DefFontName := 'Verdana,Courier';
  HelpForm.SetFocus;
  if (helptext <> '') and ( pos(' ',helptext) > 0) then helptext := copy(helptext,1,pos(' ',helptext)-1);
  HelpForm.LoadHelp(helptext);
end;


procedure ViewHelpForm(FileName: String);
begin
  if not assigned(HelpForm) then
  begin
    HelpForm := THelpForm.Create(Application);
  end;
  HelpForm.Show;
  HelpForm.SetFocus;
  if not FileExists(filename) then
    dm.error('File %s not found', [filename], 103013);
  HelpForm.Viewer.LoadFromFile(filename);
  HelpForm.Caption  := Filename;
end;

procedure ShowHelpForm(HelpText, FileName: String);
begin
  if not assigned(HelpForm) then
  begin
    HelpForm := THelpForm.Create(Application);
  end;
  HelpForm.Show;
  HelpForm.SetFocus;
  HelpForm.LoadHelpFrom(helptext, FileName);
end;

function HelpWindowHasFocus(): boolean;
begin
  if not Assigned(HelpForm) then
    result := false
  else
    result := HelpForm.IsActive;
end;

procedure THelpForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure THelpForm.FormCreate(Sender: TObject);
const
  def: TFormDefaults = (Section: 'Help';
                        Top: 100; Left: 600;
                        Width: 600; Height: 600;
                        Maximize: false);
begin
  OInifile.LoadForm(self, def);
  Viewer := THTMLViewer.Create(self);
  Viewer.parent := ViewerPanel;
  Viewer.Align := alClient;
  Viewer.OnKeyDown := self.OnKeyDown;
  Viewer.DefBackground := clWhite;
  Viewer.vscrollbar.visible := True;
  Viewer.NoSelect := false;
  Viewer.HistoryMaxCount := 25;
//  Viewer.OnHistoryChange :=HistoryChange;
  Viewer.OnHotSpotClick := aMainForm.HotSpotClick;
end;

procedure THelpForm.LoadHelpFrom(HelpText, FileName: STring);
var
  loadfile, lang, sysdir: string;
  opt: TEpiOption;
  found : Boolean;
begin
  sysdir := ExtractFilePath(Application.ExeName) + 'languages' + PathDelim;

  if dm.GetOptionValue('LANGUAGE', opt) then
    lang := AnsiLowerCase(opt.value);

  if FileExists(sysdir + lang + PathDelim + FileName + '.htm') then
    loadfile := sysdir + lang + PathDelim + FileName + '.htm'
  else
    loadfile :=sysdir + 'en' + PathDelim + FileName + '.htm';
  Viewer.LoadFromFile(loadfile);
  Self.Caption := Viewer.DocumentTitle;
  Found:= Viewer.Find(HelpText, false);
  if (not found) then dm.info('Not found: %s', [helptext], 25001);
      OldHelpText := HelpText;
end;

procedure THelpForm.LoadHelp(HelpText: String);
var
  loadfile: string;
  sysdir: string;
  lang: string;
  opt: TEpiOption;

  function test(path: string): boolean;
  begin
    result := false;
    if (loadfile = '') and fileexists(path) then
      result := true;
  end;

begin
  sysdir := ExtractFilePath(Application.ExeName) + 'languages' + PathDelim;
  if dm.GetOptionValue('LANGUAGE', opt) then
    if AnsiUpperCase(opt.Value) = 'ENGLISH' then lang := 'en'
      else  lang := OTranslator.Translate(105,'en');  // was lang := AnsiLowerCase(opt.value);

  loadfile := '';

  // notice that as soon as loadfile is <> '', then the test(      )   will fail.
  // 1. search in {sysdir}\languages\{language}\xxxx.htm[l] for the language specific extended file: (e.g. read.htm og read.html)
  if test(sysdir + lang + PathDelim + HelpText + '.htm') then
    loadfile := sysdir + lang + PathDelim + helptext + '.htm';

  if test(sysdir + lang + PathDelim + HelpText + '.html') then
    loadfile := sysdir + lang + PathDelim + helptext + '.html';

  // 2.search in {sysdir}\languages\en\ xxxx.htm[l]  for the file. (fx read.htm og read.html)
  if test(sysdir + 'en' + PathDelim + HelpText + '.htm') then
    loadfile := sysdir + 'en' + PathDelim + helptext + '.htm';
  if test(sysdir + 'en' + PathDelim + HelpText + '.html') then
    loadfile := sysdir + 'en' + PathDelim + helptext + '.html';

  // 3. search for translated  {sysdir}\languages\{language}\commands.htm
  if test(sysdir + lang + PathDelim + 'commands.htm') then
    loadfile := sysdir + lang + PathDelim + 'commands.htm';

  // 4. finally if none of 1-3 apply show the english version file:  {sysdir}\languages\en\commands.htm
  if LoadFile = '' then
    loadfile := sysdir + 'en' + PathDelim + 'commands.htm';

  Viewer.LoadFromFile(loadfile);
  Self.Caption := Viewer.DocumentTitle;
  
//  Viewer.positionto('#top');
  if (helptext <> '') and ((loadfile = sysdir + 'en' + PathDelim + 'commands.htm') or
     (loadfile = sysdir + lang + PathDelim + 'commands.htm')) then
    Viewer.positionto('#' + helptext)      // find relevant #xx  in the html file
  else
    Viewer.positionto('#top');

  OldHelpText := HelpText;
end;

procedure THelpForm.FormDeactivate(Sender: TObject);
begin
  active := false;
end;

procedure THelpForm.FormActivate(Sender: TObject);
begin
  active := true;
end;

procedure THelpForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    OInifile.SaveCurrentForm(Self, 'Help');
end;

procedure THelpForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('P')) and (ssCtrl in Shift) then   hlpPrintExecute(nil)
  {else if (Key = Key_F10) then aMainform.CmdEdit.SetFocus}       {// malfunction of this one}
  else if ((Key = VK_Prior) or (Key = VK_Next)) then
          Viewer.KeyDown(key, shift)
  else
    aMainForm.OnKeyDown(sender, key, shift);
end;

procedure THelpForm.hlpPrintExecute(Sender: TObject);
var
  PrintOptions: TPrintDialogOptions;
begin
  with aMainForm.PrintDialog do
  begin
    PrintOptions := Options;
    Options := Options + [poPageNums];
    if Execute then
    begin
      if PrintRange = prAllPages then
        viewer.Print(1, 9999)
      else
        Viewer.Print(FromPage, ToPage);
    end;
    Options := PrintOptions;
  end
end;


procedure THelpForm.hlpRepeatSearchExecute(Sender: TObject);
begin
  Viewer.Find(OldHelpText, false);
end;

procedure THelpForm.hlpOpenExecute(Sender: TObject);
begin
  with dm.OD do
  begin
    FileName:='';
    Filter:= EpiLogFileFilter;
    FilterIndex := 1;
    if not Execute then exit;
    Viewer.LoadFromFile(FileName);
    Self.Caption := Viewer.DocumentTitle;
  end;
end;

procedure THelpForm.hlpPrevExecute(Sender: TObject);
begin
  with Viewer do
  begin
    if Sender = hlpPrev then
      HistoryIndex := HistoryIndex +1
    else
      HistoryIndex := HistoryIndex -1;
    Self.Caption := DocumentTitle;
  end;
end;

procedure THelpForm.hlpReloadExecute(Sender: TObject);
begin
  with Viewer do
  begin
    hlpReload.Enabled := False;
    ReLoad;
    hlpReload.Enabled := CurrentFile <> '';
    Viewer.SetFocus;
  end;
end;

procedure THelpForm.Exit1Click(Sender: TObject);
begin
  close;
end;

procedure THelpForm.hlpCopyExecute(Sender: TObject);
begin
  Viewer.CopyToClipboard;
end;

end.
