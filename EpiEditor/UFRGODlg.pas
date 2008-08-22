unit UFRGODlg;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, MRUCombo;

type


  TFRSearchOption = (frMatchCase, frWholeWord, frBackwards, frEntireScope, frSelectedOnly,
    frReplace, frReplaceAll, frPrompt, frLocalWindow, frStartPage, frHowTo, frCommands);
  TFrSearchOptions = set of TfrSearchOption;
  TFrShowOptions = set of TFrSearchOption;
  TFrReplaceAction = (fraCancel, fraFind, fraReplace, fraReplaceAll);

  TFRGODlg = class(TForm)
    findbtn: TButton;
    CancelBtn: TButton;
    Pg: TPageControl;
    GroupBox1: TGroupBox;
    MatchCase: TCheckBox;
    WholeWord: TCheckBox;
    Prompt: TCheckBox;
    GroupBox2: TGroupBox;
    Forward: TRadioButton;
    Backward: TRadioButton;
    GroupBox3: TGroupBox;
    Global: TRadioButton;
    SelectedOnly: TRadioButton;
    GroupBox4: TGroupBox;
    FromCursor: TRadioButton;
    EntireScope: TRadioButton;
    RadioButton1: TRadioButton;
    cbFind: TMRUComboBox;
    cbReplace: TMRUComboBox;
    TaFind: TTabSheet;
    TaReplace: TTabSheet;
    Label4: TLabel;
    TaGoto: TTabSheet;
    Label1: TLabel;
    repAllbtn: TButton;
    repBtn: TButton;
    ListBox1: TListBox;
    EpageNo: TEdit;
    GotoLabel: TLabel;
    cbShowoptions: TCheckBox;
    LocalWindow: TRadioButton;
    Label2: TLabel;
    Commands: TRadioButton;
    HowTo: TRadioButton;
    Startpage: TRadioButton;
    procedure PgChange(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure findbtnClick(Sender: TObject);
    procedure repAllbtnClick(Sender: TObject);
    procedure repBtnClick(Sender: TObject);
    procedure cbFindChange(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbShowoptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fSearchOptions: TFrSearchOptions;
    fResultCode: TFrReplaceAction;
    ForgRec      : TRect;
    fmode:Word;
    function GetReplaceText: string;
    function GetReplaceTextHistory: string;
    function GetSearchText: string;
    function GetSearchTextHistory: string;
    procedure SetReplaceText(const Value: string);
    procedure SetReplaceTextHistory(const Value: string);
    procedure SetSearchText(const Value: string);
    procedure SetSearchTextHistory(const Value: string);
    procedure SetSearchOptions(const Value: TFrSearchOptions);
    function GetSearchOptions: TFrSearchOptions;
    procedure SetShowOptions(const Value: TFrShowOptions);
    function GetShowOptions: TFrShowOptions;
    procedure ShowOptions(show:boolean);
    { Private declarations }
  public
    function Initialize:boolean;
    property SearchOptions:TFrSearchOptions read GetSearchOptions write SetSearchOptions;
    property ShowOpts: TFrShowOptions  read GetShowOptions write SetShowOptions;
    property SearchText: string read GetSearchText write SetSearchText;
    property SearchTextHistory: string read GetSearchTextHistory write SetSearchTextHistory;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ReplaceTextHistory: string read GetReplaceTextHistory write SetReplaceTextHistory;
    property ResultCode : TFrReplaceAction  read  fResultCode write fResultCode;
  end;


function ShowSearchDlg(var FindStr,ReplStr:string; var options :TFrSearchOptions;mode:Word=1;ShowOptions: TFrShowOptions=[frMatchCase..frCommands]):TFrReplaceAction;

implementation

{$R *.DFM}

uses
  UCmdProcessor, ansDatatypes;

var
  FRGODlg: TFRGODlg;


{ TODO:  Get tab stops for replace to work properly !!!
  function Settaborder(mode : Integer): Boolean;
 var i : integer;
begin

  // set all taborders off:
     FRGODlg.cbReplace.tabstop := False;
     FRGODlg.repbtn.Tabstop := False;
     FRGODlg.repAllbtn.Tabstop := False;
     FRGODlg.GroupBox1.Tabstop := False;
     FRGODlg.GroupBox2.Tabstop := False;
    FRGODlg.GroupBox3.Tabstop := False;
    FRGODlg.GroupBox4.Tabstop := False;
     FRGODlg.GroupBox1.Taborder :=  0;
     FRGODlg.GroupBox2.Taborder :=  0;
    FRGODlg.GroupBox3.Taborder :=  0;
    FRGODlg.GroupBox4.Taborder :=  0;

  //for i := 0 to FRGODlg.ComponentCount -1 do
  //           begin FRGODlg.Components[i].t .TabStop := False;

   FRGODlg.cbFind.TabORder := 1;
   FRGODlg.findbtn.TabOrder := 2;
  // turn selected ones on again:  3: replace
  if mode = 3 then
   begin
     FRGODlg.cbReplace.AddItemToList(' ');
     FRGODlg.cbFind.TabORder := 1;
     FRGODlg.cbReplace.tabOrder := 2;
     FRGODlg.repbtn.TabOrder := 3;
     FRGODlg.repAllbtn.TabOrder := 4;
     FRGODlg.findbtn.TabOrder := 5;
   end;
end;
.. this attempt also no effect !! JL Apr 2008
}

function ShowSearchDlg(var FindStr,ReplStr:string; var options :TFrSearchOptions; mode:Word=1; ShowOptions: TFrShowOptions=[frMatchCase..frCommands]):TFrReplaceAction;
var
  opt: TEpiOption;
begin
    if FRGODlg = nil then
         FRGODlg:= TFRGODlg.Create(application) ;
    result:= fraCancel;
    FRGODlg.SearchOptions := options;
    FRGODlg.searchText := Findstr;
    FRGODlg.ReplaceText := ReplStr;
    FRGODlg.ShowOpts := ShowOptions;
    case mode of
      1,3: FRGODlg.LocalWindow.Caption := '&Editor';
      2: FRGODlg.LocalWindow.Caption := '&Output Window';
    end;
    case mode of
      //1,2: begin settaborder(mode); FRGODlg.Pg.ActivePage := FRGODlg.TaFind; end;
      //3: begin settaborder(mode); FRGODlg.Pg.ActivePage := FRGODlg.TaReplace; end;
      1,2: FRGODlg.Pg.ActivePage := FRGODlg.TaFind;
      3: FRGODlg.Pg.ActivePage := FRGODlg.TaReplace;

    end;
    if (dm.GetOptionValue('DISPLAY COMMAND PROMPT', opt) and (opt.value = 'OFF')) then
    begin
      FRGODlg.Commands.Visible := false;
      FRGODlg.HowTo.Visible := false;
    end else begin
      FRGODlg.Commands.Visible := true;
      //FRGODlg.HowTo.Visible := true;
    end;


    if FRGODlg.showmodal=mrCancel then exit;
    result:= FRGODlg.ResultCode;
    options:= FRGODlg.SearchOptions ;
    Findstr := FRGODlg.SearchText ;
    ReplStr :=FRGODlg.ReplaceText ;
end;

function TFRGODlg.GetReplaceText: string;
begin
  Result:=cbReplace.Text ;
end;

function TFRGODlg.GetReplaceTextHistory: string;
begin

end;

function TFRGODlg.GetSearchOptions: TFrSearchOptions;
begin
//frMatchCase, frWholeWord, frBackwards, frEntireScope, frSelectedOnly,
//    frReplace, frReplaceAll, frPrompt
      Result:=[];
      if matchcase.Checked then include(Result,frMatchCase);
      if wholeWord.Checked then include(Result,frWholeWord);
      if Prompt.Checked then include(Result,frPrompt);
      if SelectedOnly.Checked then include(Result,frSelectedOnly);
      if Backward.Checked then include(Result,frBackwards);
      if EntireScope.Checked then include(Result,frEntireScope);
      if Startpage.Checked then include(Result, frStartPage);
      if LocalWindow.Checked then include(Result, frLocalWindow);
      if Commands.Checked then include(Result, frCommands);
      if HowTo.Checked then include(Result, frHowTo);
end;

procedure TFRGODlg.SetSearchOptions(const Value: TFrSearchOptions);
begin
      matchcase.Checked :=                        frMatchCase in value;
      wholeWord.Checked :=                        frWholeWord in value;
      Prompt.Checked :=                         frPrompt in value;
      SelectedOnly.Checked  :=                       frSelectedOnly in value ;
      Backward.Checked :=                        frBackwards in value ;
      EntireScope.Checked :=                        frEntireScope in value ;
end;

function TFRGODlg.GetSearchText: string;
begin
 Result:=cbFind.Text ;
end;

function TFRGODlg.GetSearchTextHistory: string;
begin

end;

procedure TFRGODlg.PgChange(Sender: TObject);
begin
if ((Sender as TPageControl).ActivePage = TaFind) then
 begin
    cbFind.Visible :=true;
 end
 else if ((Sender as TPageControl).ActivePage = TaReplace) then
 begin
     cbFind.Visible :=true;
 end
 else if ((Sender as TPageControl).ActivePage = TaGoTo) then
 begin
    cbFind.Visible :=false;
 end
end;

procedure TFRGODlg.SetReplaceText(const Value: string);
begin
 cbReplace.Text := Value;
end;

procedure TFRGODlg.SetReplaceTextHistory(const Value: string);
begin

end;


procedure TFRGODlg.SetSearchText(const Value: string);
begin
   cbFind.Text := Value;
end;

procedure TFRGODlg.SetSearchTextHistory(const Value: string);
begin

end;

procedure TFRGODlg.CancelBtnClick(Sender: TObject);
begin
fResultCode:= fraCancel;
end;

procedure TFRGODlg.findbtnClick(Sender: TObject);
begin
fResultCode:=  fraFind;
modalresult:=mrok;
end;

procedure TFRGODlg.repAllbtnClick(Sender: TObject);
begin
fResultCode:= fraReplaceAll;
modalresult:=mrok;
end;

procedure TFRGODlg.repBtnClick(Sender: TObject);
begin
fResultCode:= fraReplace;
modalresult:=mrok;
end;

procedure TFRGODlg.cbFindChange(Sender: TObject);
begin
findbtn.Enabled := cbfind.text <>'';
if findbtn.Enabled then  cbfind.items.add(cbfind.text);
end;

procedure TFRGODlg.cbReplaceChange(Sender: TObject);
begin
repbtn.Enabled := length(cbReplace.text) > 0   ; // <>'';
repAllbtn.Enabled :=repbtn.Enabled;
if repbtn.Enabled then  cbReplace.items.add(cbReplace.text);
end;

function TFRGODlg.Initialize: boolean;
begin
   cbFindChange(self);
   cbReplaceChange(self);
   cbShowoptionsClick(self);
{   PgChange(pg);
   case fmode of
   1:  pg.ActivePage := TaFind;
   2:  pg.ActivePage := TaReplace;
   3:  pg.ActivePage := TaGoTO;
   end;
   if fmode < 3 then }
   activecontrol:= cbfind;
end;

procedure TFRGODlg.FormShow(Sender: TObject);
begin
Initialize ;
end;

procedure TFRGODlg.ShowOptions(show:boolean);
var
  r : TRect;
  p : Tpoint;
begin
 if show then
    SetBounds(self.Left,self.top,ForgRec.right-ForgRec.left,
      ForgRec.bottom-ForgRec.top)
 else
 begin
    r :=Cancelbtn.BoundsRect;
    p:= r.BottomRight;
    p:= Cancelbtn.ClientToScreen(p);
    SetBounds(self.Left,self.top,ForgRec.right-ForgRec.left,
      groupbox2.Top+ self.height-self.ClientHeight )
 end;
end;

procedure TFRGODlg.cbShowoptionsClick(Sender: TObject);
begin
  showoptions(cbShowoptions.State=cbChecked);
end;

procedure TFRGODlg.FormCreate(Sender: TObject);
begin
 ForgRec := BoundsRect;
end;

function TFRGODlg.GetShowOptions: TFrShowOptions;
begin
  // This functione is not used!
end;

procedure TFRGODlg.SetShowOptions(const Value: TFrShowOptions);
begin
  MatchCase.Enabled := (frMatchCase in Value);
  WholeWord.Enabled := (frWholeWord in value);
  LocalWindow.Enabled := (frLocalWindow in value);
  HowTo.Enabled := (frHowTo in value);
  Commands.Enabled := (frCommands in value);
  Prompt.Enabled := (frPrompt in value);
  Forward.Enabled := (frBackwards in value);
  Backward.Enabled := (frBackwards in value);
  Global.Enabled := (frEntireScope in value);
  SelectedOnly.Enabled := (frEntireScope in value);
  FromCursor.Enabled := (frSelectedOnly in value);
  EntireScope.Enabled := (frSelectedOnly in value);
  TaReplace.TabVisible := (frReplace in value) or (frReplaceAll in value);
end;


end.

