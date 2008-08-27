unit UGraphDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, SMUtils, Umain, AnsDatatypes, Mask,
  UVectors;

type
  TGraphDlg = class(TForm)
    CmdPanel: TPanel;
    CancelBtn: TButton;
    RunBtn: TButton;
    ResetBtn: TButton;
    PasteBtn: TButton;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit6: TEdit;
    TxtGRpBox: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    xpos1: TMaskEdit;
    ypos1: TMaskEdit;
    Edit3: TEdit;
    btyp1: TCheckBox;
    AxisGrpBox: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    XHideChk: TCheckBox;
    YHideChk: TCheckBox;
    XInvChk: TCheckBox;
    XLogChk: TCheckBox;
    YInvChk: TCheckBox;
    YLogChk: TCheckBox;
    GenGrpBox: TGroupBox;
    LegendChk: TCheckBox;
    FrameChk: TCheckBox;
    HGridChk: TCheckBox;
    VGridChk: TCheckBox;
    xtickchk: TCheckBox;
    YtickChk: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    xmin: TMaskEdit;
    xmax: TMaskEdit;
    Label8: TLabel;
    Label11: TLabel;
    ymax: TMaskEdit;
    ymin: TMaskEdit;
    EditChk: TCheckBox;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    xinc: TMaskEdit;
    Label20: TLabel;
    ExpandBtn: TBitBtn;
    MinimizeBtn: TBitBtn;
    ComboBox5: TComboBox;
    YINC: TMaskEdit;
    ExecBtn: TButton;
    SpcGrpBox: TPanel;
    Label7: TLabel;
    Label21: TLabel;
    Break1: TMaskEdit;
    Break2: TMaskEdit;
    SPCTest: TCheckBox;
    NChk: TCheckBox;
    GroupBox1: TGroupBox;
    SizeX: TMaskEdit;
    SizeY: TMaskEdit;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    WeigthBox: TGroupBox;
    ComboBox6: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBoxSelect(Sender: TObject);
    procedure ExpandBtnClick(Sender: TObject);
    procedure ExecBtnClick(Sender: TObject);
 Private
    CmdName :string;
    CmdString : string;
    MAinform: TaMainForm;
    DlgResult : integer;
    VarCount: Integer;
    Expanded: boolean;
    procedure SetupDlg(legalxtype, legalytype: array of integer;by: Boolean = False; weigth: Boolean = False);
    procedure ParseString(const ps: string);
    function makeCmdString(var cmd: string): boolean;
    procedure InitializeDlg(const ps: string;xvars, yvars: integer; legalxtype, legalytype:
                array of integer; AdvType: TEpiAdvOptions; BY: Boolean; Weigth: Boolean;
                t1:string='X Variable'; t2:string='Y Variable 1';
                      t3:string='Y Variable 2';t4:string='Y Variable 3';
                      tb:string='Y Variable 4';byreq:Boolean=False);
    function LegalValue(testtype: integer; legalvalues: array of integer): boolean;
    procedure ShowAdvOpt(AdvType: TEpiAdvOptions);
    procedure BreakFormat(Vec: TEpiVector; YBox: boolean = false);

//       procedure FormCreate(Sender: TObject);

  public
    { Public declarations }
  end;

  function ShowGraphDlg(frm: TaMainForm; ParseString: string; var CmdString: string;
                      Xvars, Yvars: Integer; LegalXtype, LegalYType: array of integer;
                      AdvType: TEpiAdvOptions; By: Boolean=False; Weigth: boolean=false;
                      t1:string='X Variable'; t2:string='Y Variable 1';
                      t3:string='Y Variable 2';t4:string='Y Variable 3';
                      tb:string='Y Variable 4'):integer;

implementation

uses
  UCmdProcessor, UTranslation;

{$R *.DFM}

var
  GraphDlg: TGraphDlg;

function ShowGraphDlg(frm: TaMainForm; ParseString: string; var CmdString: string;
                      Xvars, Yvars: Integer; LegalXtype, LegalYType: array of integer;
                      AdvType: TEpiAdvOptions; By:Boolean=False; Weigth: boolean=false;
                      t1:string='X Variable'; t2:string='Y Variable 1';
                      t3:string='Y Variable 2';t4:string='Y Variable 3';
                      tb:string='Y Variable 4'):integer;
begin
  if not assigned( GraphDlg) then
  begin
    GraphDlg:= TGraphDlg.Create(application);
    GraphDlg.MAinform :=frm;
    OTranslator.TranslateForm(GraphDlg);     // translate form ?
  end;
  GraphDlg.InitializeDlg(ParseString, Xvars, Yvars, legalxtype, legalytype, AdvType, By, Weigth,
                         t1,t2,t3,t4,tb);
  GraphDlg.showmodal;
  Result:=GraphDlg.DlgResult;
  CmdString := GraphDlg.CmdString;
  GraphDlg.Free;
  GRaphDlg := nil;
end;

function TGraphDlg.LegalValue(testtype: integer; legalvalues: array of integer): boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to high(legalvalues) do
    if testtype = legalvalues[i] then exit;
  result := false;
end;

procedure TGraphDlg.FormCreate(Sender: TObject);
begin
  CancelBtn.tag:=DlgResCancel;
  ResetBtn.tag:= DlgResReset;
  PasteBtn.tag:=  DlgResPaste;
  RunBtn.tag:=  DlgResRun;
end;

procedure TGraphDlg.InitializeDlg(
const ps: string;
xvars, yvars: integer; legalxtype, legalytype: array of integer; AdvType: TEpiAdvOptions; By:boolean; Weigth: boolean;
t1:string='X Variable'; t2:string='Y Variable 1';
                      t3:string='Y Variable 2';t4:string='Y Variable 3';
                      tb:string='Y Variable 4';byreq:Boolean=False);
begin
  if not dm.CheckDataOpen() then exit; //m.CheckDataOpen();
  self.Width := 400;
  self.Height := 195;
  ParseString(ps);
  VarCount := yvars;
  ResetBtnClick(Self);
  SetupDlg(legalxtype,legalytype,By, weigth);
  ShowAdvOpt(AdvType);
//  Expanded := false;
  Expanded := True;
  ExpandBtnClick(Self);
end;

procedure TGraphDlg.SetupDlg(legalxtype, legalytype: array of integer;by:Boolean=false; weigth: Boolean = False);
var
  i,co : integer;

begin
  if high(legalxtype)<0 then
    raise exception.Create('GrapgDlg: Array not initialized');
  Combobox1.Items.Clear;
  Combobox2.Items.Clear;
  Combobox3.Items.Clear;
  Combobox4.Items.Clear;
  Combobox5.Items.Clear;
  combobox1.Items.BeginUpdate;
  combobox2.Items.BeginUpdate;
  combobox3.Items.BeginUpdate;
  combobox4.Items.BeginUpdate;
  combobox5.Items.BeginUpdate;
  combobox6.Items.BeginUpdate;
  co := dm.dataframe.VectorCount;
  for i:= 0 to co-1 do
  begin
    // Skip std. vectors
    //if (dm.dataframe.Vectors[i].Internal) then continue;
    if LegalValue(dm.dataframe.Vectors[i].DataType, legalxtype) then
       combobox1.Items.Add(dm.dataframe.Vectors[i].Name);
    if LegalValue(dm.dataframe.Vectors[i].DataType, legalytype) then
    begin
      combobox2.Items.Add(dm.dataframe.Vectors[i].Name);
      combobox3.Items.Add(dm.dataframe.Vectors[i].Name);
      combobox4.Items.Add(dm.dataframe.Vectors[i].Name);
      combobox6.Items.Add(dm.dataframe.Vectors[i].Name);
    end;
    if (by){ and (LegalValue(dm.dataframe.Vectors[i].DataType,)} then
      combobox5.Items.Add(dm.dataframe.Vectors[i].Name);
  end;
  combobox1.Items.EndUpdate;
  combobox2.Items.EndUpdate;
  combobox3.Items.EndUpdate;
  combobox4.Items.EndUpdate;
  combobox5.Items.EndUpdate;
  combobox6.Items.EndUpdate;
  if (not by) then Combobox5.Visible := False;
  if (not weigth) then WeigthBox.Visible := False;

//  SetButtons;
  Caption := CmdName;
end;


procedure TGraphDlg.ParseString(const ps: string);
var
 s,remain, v : string;
 c : char;
begin
  c:= #32;
  s :=AnsiUpperCase(ps);
  SplitFirst(s,[#32,'('],v,remain,c);
  if v='' then exit;
  CmdName:= v;
  SplitFirst(remain,[#32,'('],v,remain,c);
  if v <> 'VARLIST' then exit;
end;

function TGraphDlg.makeCmdString(var cmd:string):boolean;
const
  c = ',';

  function ChkBxRes(CB: TCheckBox): string;
  begin
    if CB.Checked then result := '1'
    else result := '0';
  end;

begin
  result := true;
  Cmd:=CmdName;
  if Combobox1.ItemIndex <> -1 then Cmd := cmd + ' ' + Combobox1.Text;
  if Combobox2.ItemIndex <> -1 then Cmd := cmd + ' ' + Combobox2.Text;
  if Combobox3.ItemIndex <> -1 then Cmd := cmd + ' ' + Combobox3.Text;
  if Combobox4.ItemIndex <> -1 then Cmd := cmd + ' ' + Combobox4.Text;
  if Combobox5.ItemIndex <> -1 then Cmd := cmd + ' ' + '/by='+ Combobox5.Text;
  if Combobox6.ItemIndex <> -1 then Cmd := cmd + ' ' + '/W='+ Combobox6.Text;

  if trim(Edit1.Text) <> 'Title' then
    Cmd := cmd + ' /TI="' + trim(edit1.Text) + '"';
  if trim(Edit2.Text) <> 'Subtitle' then
    Cmd := cmd + ' /SUB="' + trim(edit2.Text) + '"';
  if trim(Edit6.Text) <> 'Save File as' then
    Cmd := cmd + ' /SAVE="' + trim(edit6.Text) + '"';

  if not ((trim(break1.Text) = '-  -') or (trim(break1.Text) = '')) then cmd := cmd + ' /B="' + Break1.Text + '"';
  if not ((trim(break2.Text) = '-  -') or (trim(break2.Text) = '')) then cmd := cmd + ' /B="' + Break2.Text + '"';

  if not (trim(SizeX.Text) = '') then cmd := cmd + ' /SIZEX="' + SizeX.Text + '"';
  if not (trim(SizeY.Text) = '') then cmd := cmd + ' /SIZEY="' + SizeY.Text + '"';

  // Show options:
  if FrameChk.Checked then cmd := cmd + ' /FRAME';
  if HGridChk.Checked then cmd := cmd + ' /HGRID';
  if VGridChk.Checked then cmd := cmd + ' /VGRID';
  if NChk.Checked then cmd := cmd + ' /N';

  if (CmdName = 'Line') or (CmdName = 'Scatter') then
    begin if not LegendChk.Checked then cmd := cmd + ' /NOLEGEND'; end
  else
    if LegendChk.Checked then cmd := cmd + ' /LEGEND';
  if EditChk.Checked then cmd := cmd + ' /EDIT';
  // Axis option:
  if not XHideChk.Checked then cmd := cmd + ' /XHIDE';
  if not YHideChk.Checked then cmd := cmd + ' /YHIDE';
  if XInvChk.Checked then cmd := cmd + ' /XINV';
  if YInvChk.Checked then cmd := cmd + ' /YINV';
  if XLogChk.Checked then cmd := cmd + ' /XLOG';
  if YLogChk.Checked then cmd := cmd + ' /YLOG';
  if not XTickChk.Checked then cmd := cmd + ' /NOXTICK';
  if not YTickChk.Checked then cmd := cmd + ' /NOYTICK';
  if not ((trim(xmin.Text) = '-  -') or (trim(xmin.Text) = '')) then
    cmd := cmd + ' /XMIN="' + xmin.Text + '"';
  if not ((trim(xmax.Text) = '-  -') or (trim(xmax.Text) = '')) then
    cmd := cmd + ' /XMAX="' + xmax.Text + '"';
  if not ((trim(ymin.Text) = '-  -') or (trim(ymin.Text) = '')) then
    cmd := cmd + ' /YMIN="' + ymin.Text + '"';
  if not ((trim(ymax.Text) = '-  -') or (trim(ymax.Text) = '')) then
    cmd := cmd + ' /YMAX="' + ymax.Text + '"';
  if not ({(trim(xinc.Text) = '-  -') or} (trim(xinc.Text) = '')) then
    cmd := cmd + ' /XINC=' + xinc.Text + '';
  if not ({(trim(xinc.Text) = '-  -') or} (trim(Yinc.Text) = '')) then
    cmd := cmd + ' /YINC=' + xinc.Text + '';

  // Textbox options:
  if trim(Edit3.Text) <> '' then cmd := cmd +
     ' /TEXT="' + xpos1.Text + c + ypos1.Text + c + edit3.Text + c + ChkBxRes(btyp1) + '"';
 { if trim(Edit4.Text) <> '' then cmd := cmd +
     ' /TEXT="' + xpos2.Text + c + ypos2.Text + c + edit4.Text + c + ChkBxRes(btyp2) + '"';
 }
  if (((CmdName = 'PCHART') or (CmdName = 'RUNCHART') or (CmdName = 'ICHART'))
     and (spctest.checked)) then cmd := cmd + ' /T'; // as default do test for special causes

end;

procedure TGraphDlg.CancelBtnClick(Sender: TObject);
begin
  DlgResult:=Tcomponent(sender).tag;
  makeCmdString(CmdString);
  close;
end;


procedure TGraphDlg.ResetBtnClick(Sender: TObject);
var
  i: integer;

begin

  for i := 0 to GraphDlg.ComponentCount -1 do
  begin
    if (GraphDlg.Components[i] is TMaskEdit) then
      (GraphDlg.Components[i] as TMaskEdit).Clear;
    if (GraphDlg.Components[i] is TCheckBox) then
      (GraphDlg.Components[i] as TCheckBox).Checked := false;
    if (GraphDlg.Components[i] is TEdit) then
      (GraphDlg.Components[i] as TEdit).clear;
    if (GraphDlg.Components[i] is TComboBox) then
      (GraphDlg.Components[i] as TComboBox).ItemIndex := -1;
  end;

  Combobox3.text := 'Y Variable (optional)';
  Combobox4.text := 'Y Variable (optional)';
  Combobox5.text := 'by Variable (optional)';
  Combobox6.text := 'Weigth variable (optional)';

  SPCtest.checked := False;

  if pos(CmdName,'PCHART RUNCHART ICHART XCHART') > 0 then
        begin
          Combobox1.text := 'Time variable';
          Combobox2.text := 'Count';
          Combobox3.text := 'Total';
          SPCtest.checked := True;
        end
    else if CmdName = 'EPICURVE' then
        begin
          Combobox1.text := 'Outcome';
          Combobox2.text := 'Time variable';
        end
    else if CmdName = 'CIPLOT' then
        begin
          Combobox1.text := 'Variable';
          Combobox2.text := 'Variable (optional)';
          Combobox3.text := 'Variable (optional)';
          Combobox4.text := 'Variable (optional)';
          Combobox5.text := 'by Variable';
       end
        else
          begin
          Combobox1.text := 'Choose X Variable';
          Combobox2.text := 'Choose Y Variable';
        end;

  Edit1.Text := 'Title';
  Edit2.Text := 'Subtitle';
  Edit6.Text := 'Save File as';
  Label1.Caption := '';
  Label2.Caption := '';
  Label3.Caption := '';
  Label4.Caption := '';
  Label24.Caption := '';
  if VarCount < 1 then Combobox2.Visible := false;
  if VarCount < 2 then Combobox3.Visible := false;
  if VarCount < 3 then Combobox4.Visible := false;
  if (CmdName = 'Scatter') or (CmdName = 'Line') then
    LegendChk.Checked := true;
  XHideChk.Checked := true;
  YHideChk.Checked := true;
  EditChk.Checked := False;
  XTickChk.Checked := true;
  YTickChk.Checked := true;
end;

procedure TGraphDlg.FormShow(Sender: TObject);
begin
//   showadvopt(AdvOpt);
   combobox1.SetFocus;
end;

procedure TGraphDlg.ComboBoxSelect(Sender: TObject);
var
  i : integer;
  v,s : string;
begin
   i:= (sender as tcombobox).ItemIndex;
   v := trim(copy((sender as tcombobox).Items[i], 1, 20));
   s := dm.dataframe.VectorByName[v].GetVariableLabel;
   case (Sender as TComboBox).Tag of
     1: begin
          BreakFormat(dm.dataframe.VectorByName[v]);
          Label1.Caption := S;
        end;
     2: begin
          BreakFormat(dm.dataframe.VectorByName[v], true);
          Label2.Caption := S;
        end;
     3: Label3.Caption := S;
     4: Label4.Caption := S;
     5: Label24.Caption := S;
   end;
end;

procedure TGraphDlg.ShowAdvOpt(AdvType: TEpiAdvOptions);
begin
  if not (GrpSpcOpt in AdvType) then
    SpcGrpBox.Visible := false;
  if not (GrpStdOpt in AdvType) then
  begin
    GenGrpBox.Visible := false;
    AxisGrpBox.Visible := false;
    TxtGRpBox.Visible := false;
  end;
end;

procedure TGraphDlg.BreakFormat(Vec: TEpiVector; YBox: boolean = false);
var
  mask: string;
begin
  if Vec.DataType = EpiTyDate then
  begin
    if (Vec.FieldFormat = '%DMY') or (Vec.FieldFormat = '%MDY') then
      mask := '00\-00\-0000;1;_';
    if (Vec.FieldFormat = '%YMD') then
      mask := '0000\-00\-00;1;_';
  end
  else
    mask := '';
  if ybox then
  begin
    ymin.EditMask := mask;
    ymax.EditMask := mask;
  end else begin
    break1.EditMask := mask;
    break2.EditMask := mask;
    xmin.EditMask := mask;
    xmax.EditMask := mask;
  end;
end;

procedure TGraphDlg.ExpandBtnClick(Sender: TObject);
begin
  if not expanded then
  begin
    self.Width := 718;
    self.Height := 323;
    expanded := true;
    MinimizeBtn.BringToFront;
    RESETbtn.Visible := True;
    // PASTEbtn.Visible := True;
  end else begin
    self.Width := 400;
    self.Height := 210;
    expanded := false;
    ExpandBtn.BringToFront;
    RESETbtn.Visible := False;
    PASTEbtn.Visible := False;
  end;
end;

procedure TGraphDlg.ExecBtnClick(Sender: TObject);
begin
  DlgResult := DlgResRun;
  makeCmdString(CmdString);
  aMainForm.doCommand(CmdString);
  DlgResult := DlgResCancel;
  CmdString := '';
end;

end.
