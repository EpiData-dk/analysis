unit UTableDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, SMUtils, Umain, AnsDatatypes, Mask,
  UVectors, CheckLst, Grids, ValEdit, ColorGrd;

type
  TTableDlg = class(TForm)
    CmdPanel: TPanel;
    CancelBtn: TButton;
    RunBtn: TButton;
    ResetBtn: TButton;
    PasteBtn: TButton;
    Panel1: TPanel;
    ExpandBtn: TBitBtn;
    MinimizeBtn: TBitBtn;
    LabelBox: TGroupBox;
    ValueChk: TCheckBox;
    FieldsGroup: TGroupBox;
    VarLabel: TLabel;
    DotsLabel: TLabel;
    GetVar: TCheckListBox;
    TablePanel: TPanel;
    FreqPanel: TPanel;
    GroupBox2: TGroupBox;
    CIChk: TCheckBox;
    GroupBox6: TGroupBox;
    RowFPct: TCheckBox;
    CumPct: TCheckBox;
    AllBtn: TButton;
    AdvTablePanel: TPanel;
    EstimatesGrp: TGroupBox;
    ColumnVar: TLabel;
    RowVar: TLabel;
    Variables: TLabel;
    Label2: TLabel;
    VarLabelChk: TCheckBox;
    HelpBtn: TButton;
    PercentageGrp: TGroupBox;
    RowPct: TCheckBox;
    ColumnPct: TCheckBox;
    TotalPct: TCheckBox;
    HelpPanel: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    CloseHelpBtn: TButton;
    Label1: TLabel;
    GroupBox3: TGroupBox;
    LineClass: TRadioButton;
    BoxClass: TRadioButton;
    FilledClass: TRadioButton;
    ShadedClass: TRadioButton;
    FMisChk: TCheckBox;
    TblShowGrp: TGroupBox;
    SortSA: TRadioButton;
    SortSAT: TRadioButton;
    SortSDT: TRadioButton;
    Label9: TLabel;
    GroupBox1: TGroupBox;
    StdBtn: TRadioButton;
    ARBtn: TRadioButton;
    OrBtn: TRadioButton;
    CompactBtn: TRadioButton;
    RRBtn: TRadioButton;
    ARCIChk: TCheckBox;
    StatsGrp: TGroupBox;
    ChiSQChk: TCheckBox;
    ExactChk: TCheckBox;
    MissingChk: TCheckBox;
    SortSD: TRadioButton;
    FVbtn: TRadioButton;
    GammaChk: TCheckBox;
    ExecBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure ExpandBtnClick(Sender: TObject);
    procedure GetVarClick(Sender: TObject);
    procedure AllBtnClick(Sender: TObject);
    procedure GetVarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure HelpBtnClick(Sender: TObject);
    procedure CloseHelpBtnClick(Sender: TObject);
    procedure GetVarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AdvTablePanelClick(Sender: TObject);
    procedure ExecBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
 Private
    CmdName :string;
    CmdString : string;
    Mainform: TaMainForm;
    DlgResult : integer;
    Expanded: boolean;
    procedure SetupDlg();
    procedure ParseString(const ps: string);
    function makeCmdString(var cmd: string): boolean;
    procedure InitializeDlg(const ps: string);
  public
    { Public declarations }
  end;

  function ShowTableDlg(frm: TaMainForm; ParseString: string; var CmdString: string):integer;

implementation

uses
  UCmdProcessor, GeneralUtils, UTranslation, UIniFile;

{$R *.DFM}
const
  DialogWidth: integer        = 407;
  DialogHeight: integer       = 325;
  DialogExpandWidth: integer  = 657;
  DialogExpandHeight: integer = 325;

var
  TableDlg: TTableDlg;

function ShowTableDlg(frm: TaMainForm; ParseString: string; var CmdString: string):integer;
begin
  if not assigned(TableDlg) then
  begin
    TableDlg:= TTableDlg.Create(application);
    TableDlg.Mainform :=frm;
  end;
  TableDlg.InitializeDlg(ParseString);
  OTranslator.TranslateForm(TableDlg);
  TableDlg.ShowModal;
  Result := TableDlg.DlgResult;
  CmdString := TableDlg.CmdString;
  FreeAndNil(TableDlg);
end;

procedure TTableDlg.FormCreate(Sender: TObject);
begin
{  CancelBtn.tag := DlgResCancel;
  ResetBtn.tag := DlgResReset;
  PasteBtn.tag :=  DlgResPaste;
  RunBtn.tag :=  DlgResRun;   }
end;

procedure TTableDlg.InitializeDlg(const ps: string);
begin
  if not dm.CheckDataOpen() then exit; //dm.CheckDataOpen();
  self.Width := DialogWidth;
  self.Height := DialogHeight;
  ParseString(ps);
  if ps = 'freq varlist' then
  begin
    TablePanel.Visible := False;
    AdvTablePanel.Visible := False;
  end
  else
  begin
    FreqPanel.Visible := False;

  end;
  ResetBtnClick(nil);
  SetupDlg();
  Expanded := True;
  ExpandBtnClick(Self);
end;

procedure TTableDlg.SetupDlg();
begin
  GetVar.Items := dm.dataframe.GetVectorNames(Nil);
  Caption := CmdName;
  AdvTablePanelClick(Self);
end;


procedure TTableDlg.ParseString(const ps: string);
var
 s,remain, v : string;
 c : char;
begin
  c:= #32;
  s := AnsiUpperCase(ps);
  SplitFirst(s,[#32,'('],v,remain,c);
  if v='' then exit;
  CmdName:= v;
  SplitFirst(remain,[#32,'('],v,remain,c);
  if v <> 'VARLIST' then exit;
end;

function TTableDlg.makeCmdString(var cmd:string):boolean;
var i : integer;
    //s: String;
  function addoption(box :TCheckBox; const option: string):boolean;
  begin
    if box.Checked then cmd := cmd + option + ' ';
  end;

  function btnoption(btn :TRadioButton; const option: string):boolean;
  begin
    if btn.Checked then cmd := cmd + option + ' ';
  end;

begin
  result := true;
  Cmd:= tabledlg.Caption;
  //s := CmdName;
  if DlgResult in [DlgResCancel, DlgResReset] then exit;

  if TablePanel.Visible then
  begin
    cmd := trim(cmd + ' ' + ColumnVar.Caption + ' ' + RowVar.Caption + ' ' + Variables.caption);
    for i := 0 to GetVar.Count-1 do
      if (GetVar.State[i] = cbChecked) and (pos(GetVar.Items[i],cmd) = 0) then
        cmd := cmd + ' ' + GetVar.Items[i] ;
    // options
    addoption(ColumnPct,' /C');
    addoption(RowPct,' /R');
    addoption(TotalPct,' /TP');
    addoption(CHISqChk,' /T');
    addoption(ExactChk,' /EX');
    addoption(MissingCHK,' /M');
    addoption(GammaChk,' /Gam');

    if CompactBtn.Checked then
    begin
      btnoption(ORbtn,' /CT /O');
      btnoption(ARbtn,' /CT /AR');
      btnoption(RRbtn,' /CT /RR');
      addoption(ARCIChk,' /CI');
    end
    else {default or FV}
    begin
      btnoption(ORbtn,' /O');
      btnoption(RRBtn,' /RR');
      btnoption(FVBtn,' /FV');
    end;
  end
  else
  begin // frequencies
    cmd := 'Tables ';
    for i := 0 to GetVar.Count-1 do
      if (GetVar.State[i] = cbChecked) then
        cmd := cmd + GetVar.Items[i] + ' ';
    // options
    addoption(CumPct,' /CUM');
    addoption(RowFPct,' /R');
    addoption(CIChk,' /CI');

    // add freq option:
    cmd := cmd + ' /F';
    addoption(FMisChk,' /M');
  end;

  //common options: e.g. shov values etc.
  addoption(ValueCHK,' /VL');

  //Sorting
  btnoption(SortSAT,' /SCAT');
  btnoption(SortSAT,' /SRAT');
  btnoption(SortSDT,' /SCDT');
  btnoption(SortSDT,' /SRDT');
  btnoption(SortSA,' /SA');
  btnoption(SortSD,' /SD');

  if LineClass.checked or BoxClass.Checked or ShadedClass.Checked or FilledClass.Checked then
  begin    // run set command:
    if pos('FRE',TableDlg.Caption) > 0
       then CmdName := 'Set table design freq='
       else CmdName := 'Set table design=';
    if BoxClass.Checked then CmdName:=CmdName + 'box'
    else if ShadedClass.Checked then CmdName:=CmdName + 'shaded'
    else if FilledClass.Checked then CmdName:=CmdName + 'filled'
    else CmdName:=CmdName + 'line';
    cmd := CmdName + ' ; ' + Cmd;
  end;  // end set design
end;

procedure TTableDlg.CancelBtnClick(Sender: TObject);
  function mr2dr(mr: integer): integer;
  begin
    case mr of
      mrOk: result := DlgResRun;
      mrYes: result := DlgResPaste;
      mrCancel: result := DlgResCancel;
    end;
  end;

begin
  DlgResult := mr2dr(TButton(sender).ModalResult);
  //if makeCmdString(CmdString) then close;
  makeCmdString(CmdString);
  close;
end;

procedure TTableDlg.ExecBtnClick(Sender: TObject);
begin
  DlgResult := DlgResRun;
  makeCmdString(CmdString);
  aMainForm.doCommand(CmdString);
  DlgResult := DlgResCancel;
  CmdString := '';
end;


procedure TTableDlg.ResetBtnClick(Sender: TObject);
var
  i: integer;
begin
   // checkboxes
  for i := 0 to TableDlg.ComponentCount -1 do
    if (TableDlg.Components[i] is TCheckBox) then
      (TableDlg.Components[i] as TCheckBox).Checked := false;

  for i := 0 to GetVar.Count -1 do
    GetVar.State[i] := cbUnchecked;

  if GetVar.Count > 0 then GetVarClick(Self);
       HelpPanel.Visible := False;
end;

procedure TTableDlg.ExpandBtnClick(Sender: TObject);
begin
  // if not expanded then
  begin
    self.Width := DialogExpandWidth;
    self.Height := DialogExpandHeight;
    expanded := true;
    MinimizeBtn.BringToFront;
    RESETbtn.Visible := True;
    PASTEbtn.Visible := True;
    if pos('FRE',TableDlg.Caption) = 0 then
     AdvTablePanel.Visible := True;
  end {else
  begin
    self.Width := DialogWidth;
    self.Height := DialogHeight;
    expanded := false;
    ExpandBtn.BringToFront;
    RESETbtn.Visible := True;
    PASTEbtn.Visible := False;
  end;
  }
end;


procedure TTableDlg.GetVarClick(Sender: TObject);
var
  i,j: integer;
   function stripvarname(s,varlist: string):string;
   begin
      result := varlist;
      if pos(s,varlist) > 0 then
         result := copy(varlist,1,pos(s,varlist)-1)
                + copy(varlist,(pos(s,varlist)+length(s)),length(varlist)-length(s));
   end;
begin

  if (cmdname = 'FREQ') then exit;

     // check if clear for any of the three labels is needed:
   for i := 0 to GetVar.Count-1 do
   begin
     if (GetVar.State[i] = cbUnChecked) and (ColumnVar.Caption = GetVar.Items[i]) then ColumnVar.Caption := '';
     if (GetVar.State[i] = cbUnChecked) and (RowVar.Caption = GetVar.Items[i]) then RowVar.Caption := '';
     if (GetVar.State[i] = cbUnChecked) and (pos(GetVar.Items[i],Variables.Caption) > 0) then
         Variables.Caption := stripvarname( GetVar.Items[i]+' ',Variables.Caption);
   end;

  if(GetVar.State[GetVar.ItemIndex] = cbUnChecked) then exit;   // do not do anything

  // else how many checked now ?
  j := 0;
  for i := 0 to GetVar.Count-1 do
    if (GetVar.State[i] = cbChecked) and (pos(GetVar.Items[i],Variables.Caption) = 0)  then inc(j);

    if j = 0 then exit;   // do not do anything

    // add var name to appropriate place for checked options:
    if (j = 1) and (trim(ColumnVar.Caption) = '')
      then ColumnVar.Caption := GetVar.Items[Getvar.ItemIndex]
        else if (j = 2) and (trim(ColumnVar.Caption) = '') then ColumnVar.Caption := GetVar.Items[GetVar.ItemIndex]
        else if (j = 2) and (trim(RowVar.Caption) = '') then Rowvar.Caption := GetVar.Items[GetVar.ItemIndex]
        else if (j > 2) and (pos(GetVar.Items[GetVar.ItemIndex],Variables.Caption) = 0)
          then  Variables.Caption := Variables.Caption + GetVar.Items[GetVar.ItemIndex] + ' ';

    // dm.info(columnvar.caption + '<br>' + rowvar.caption + '<br>' + variables.caption);
end;

procedure TTableDlg.AllBtnClick(Sender: TObject);
VAR
  n:Integer;
begin
  FOR n:=0 TO GetVar.Items.Count-1 DO
    GetVar.State[n] := cbChecked;
    GetVarClick(Getvar);
end;

procedure TTableDlg.GetVarMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
VAR
  pos: TPoint;
  n: Integer;
begin
  pos.x:=X; pos.y:=Y;
  n:= GetVar.ItemAtPos(Pos,True);
  IF (n<>-1) and (n < GetVar.Count) then
    VarLabel.Caption:= dm.dataframe.VectorByName[GetVar.Items[n]].GetVariableLabel
  ELSE
    VarLabel.Caption:='';
end;

procedure TTableDlg.HelpBtnClick(Sender: TObject);
begin
     HelpPanel.Visible := True;
     if pos('FRE',TableDlg.Caption) > 0 then
       begin
       label3.Caption := '1: Select variables';
       Label4.Caption := '  A frequency table will be made';
       Label5.Caption := '  for all marked variables';
       end
     else
       begin
       label3.Caption := '1: Select variables';
       Label4.Caption := '  First is Column, Second Row variable';
       if (StdBtn.Checked and Orbtn.checked) or Orbtn.checked or Arbtn.Checked or ArCICHK.Checked then Label4.Caption := ' First: Outcome Second: Exposure';
       Label5.Caption := '  Third+Fourth.... stratifying variables';
       if stdbtn.checked then Label5.Caption := '  Third+Fourth.... exposure variables';
       end;
       Label6.Caption := '2: Click percentage, test, sorting etc.';
       Label7.Caption := '3: Click OK        (arrow: more options)';
end;

procedure TTableDlg.CloseHelpBtnClick(Sender: TObject);
begin
     HelpPanel.Visible := False;
end;


procedure TTableDlg.GetVarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  var n:Integer;
begin
  n := GetVar.ItemIndex;
  case key of
    VK_SPACE   :      GetVarClick(Getvar);
    VK_INSERT :
        begin
          if GetVar.State[n] =  cbChecked
            then GetVar.State[n] := cbUnChecked
            else GetVar.State[n] := cbChecked;
          GetVarClick(Getvar);
          IF n < (GetVar.Count-1) then
            GetVar.ItemIndex := GetVar.ItemIndex + 1;
        end;
  end;
end;


procedure TTableDlg.AdvTablePanelClick(Sender: TObject);
begin
  // options and buttons depend on table type
  RRBtn.enabled := CompactBtn.Checked or StdBtn.Checked;
  Arbtn.enabled := Compactbtn.Checked;
  if (Sender = CompactBtn) then
    ARBtn.Checked := true;

  // clear buttons
  if Not ARBtn.Enabled then ARBtn.checked := False;
  ArCICHK.Enabled := ArBtn.Checked;
  if Not ARBtn.Checked then ARCiChk.checked := False;
  if Not RRBtn.Enabled then RRBtn.checked := False;
end;



procedure TTableDlg.FormShow(Sender: TObject);
const
  def: TFormDefaults = (Section: 'TableDlg';
                        Top: 60; Left: 300;
                        Width: 407; Height: 325;
                        Maximize: false);
begin
  OIniFile.LoadForm(self, def);
end;

procedure TTableDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OInifile.SaveCurrentForm(Self, 'TableDlg');
end;

end.
