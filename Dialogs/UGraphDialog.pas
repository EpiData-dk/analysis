unit UGraphDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, ComCtrls, ansDataTypes;

type

  TGraphDlgDefault = (goSPC, goBy, goWeight, goTestAll, goTest, goLegend);
  TGraphDlgDefaults = set of TGraphDlgDefault;

  TGraphDlgOptions = Class(TObject)
  private
    fVarCount: integer;
    fLabels: TStrings;
    fByLabel: string;
    fWLabel: string;
    fTypes: array of TEpiDatatypes;
    fByTypes: TEpiDatatypes;
    fWTypes: TEpiDatatypes;
    fTitle: string;
    fCmd: string;
    fCmdOptions: string;
    fVarNames: TStrings;
    fDefaults: TGraphDlgDefaults;
    procedure SetVarCount(const count: integer);
    function GetLabel(const index: integer): string;
    procedure SetLabel(const index: integer; const aLabel: string);
    function GetVarName(const index: integer): string;
    procedure SetVarName(const index: integer; const aLabel: string);
    function GetTypes(const index: integer): TEpiDatatypes;
    procedure SetTypes(const index: integer; const aTypes: TEpiDatatypes);
  public
    constructor Create();
    destructor Destroy(); override;
    property VarCount: integer read fVarCount write SetVarCount;
    property BoxLabels[const index: integer]: string read GetLabel write SetLabel;
    property ByLabel: string read fByLabel write fByLabel;
    property WeightLabel: string read fWLabel write fWLabel;
    property BoxTypes[const index: integer]: TEpiDatatypes read GetTypes write SetTypes;
    property ByTypes: TEpiDatatypes read fByTypes write fByTypes;
    property WeightTypes: TEpiDatatypes read fWTypes write fWTypes;
    property Title: string read fTitle write fTitle;
    property Cmd: string read fCmd write fCmd;
    property CmdOptions: string read fCmdOptions write fCmdOptions;
    property VarNames[const index: integer]: string read GetVarName write SetVarName;
    property Defaults: TGraphDlgDefaults read fDefaults write fDefaults;
  end;


  TGraphDialog = class(TForm)
    PageControl1: TPageControl;
    tabVariables: TTabSheet;
    cbVar1: TComboBox;
    cbVar2: TComboBox;
    cbVar3: TComboBox;
    lblVar1: TLabel;
    lblVar2: TLabel;
    lblVar3: TLabel;
    tabAxis: TTabSheet;
    tabSPC: TTabSheet;
    ShowGrpBox: TGroupBox;
    chkLegend: TCheckBox;
    chkFrame: TCheckBox;
    chkGridH: TCheckBox;
    chkGridV: TCheckBox;
    chkN: TCheckBox;
    SizeGrpBox: TGroupBox;
    Label22: TLabel;
    Label23: TLabel;
    edSizeX: TMaskEdit;
    edSizeY: TMaskEdit;
    AxisGrpBox: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    chkHideX: TCheckBox;
    chkHideY: TCheckBox;
    chkInvX: TCheckBox;
    chkLogX: TCheckBox;
    chkInvY: TCheckBox;
    chkLogY: TCheckBox;
    edMinX: TMaskEdit;
    edMaxX: TMaskEdit;
    edMaxY: TMaskEdit;
    edMinY: TMaskEdit;
    edIncX: TMaskEdit;
    edIncY: TMaskEdit;
    Panel1: TPanel;
    CancelBtn: TButton;
    RunBtn: TButton;
    ResetBtn: TButton;
    PasteBtn: TButton;
    ExecBtn: TButton;
    GrpTests: TGroupBox;
    chkTest1: TCheckBox;
    chkTest2: TCheckBox;
    chkTest3: TCheckBox;
    chkTest4: TCheckBox;
    chkTest5: TCheckBox;
    chkCheckAll: TCheckBox;
    GrpBreaks: TGroupBox;
    edBreak1: TMaskEdit;
    brkLabel: TLabel;
    edBreak2: TMaskEdit;
    Label4: TLabel;
    edBreak3: TMaskEdit;
    Label7: TLabel;
    tabTitle: TTabSheet;
    edTitle: TEdit;
    edSubtitle: TEdit;
    cbVar4: TComboBox;
    cbBy: TComboBox;
    tabMisc: TTabSheet;
    edSaveAs: TEdit;
    Label8: TLabel;
    lblBy: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    lblWeight: TLabel;
    Bevel1: TBevel;
    edFoot: TEdit;
    cbXLabel: TComboBox;
    Label26: TLabel;
    chkCombiTest: TCheckBox;
    Bevel2: TBevel;
    TxtGRpBox: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edTxtX: TMaskEdit;
    edTxtY: TMaskEdit;
    chkTxtBox: TCheckBox;
    mmTxt: TMemo;
    edSubFoot: TEdit;
    edXtext: TEdit;
    edYText: TEdit;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    cbWeight: TComboBox;
    lblVar4: TLabel;
    edFontSz: TMaskEdit;
    GrpLines: TGroupBox;
    chkSigmaLine: TCheckBox;
    chkInfo: TCheckBox;
    chkEdit: TCheckBox;
    GrpFreeze: TGroupBox;
    edFreeze: TMaskEdit;
    Label1: TLabel;
    Label17: TLabel;
    chkTickX: TCheckBox;
    chkTickY: TCheckBox;
    Label2: TLabel;
    TicksGrpBox: TGroupBox;
    btnXAngle45: TRadioButton;
    btnXAngle90: TRadioButton;
    btnXAngleXA: TRadioButton;
    BtnXAngleDefault: TRadioButton;
    Label3: TLabel;
    chkLabelsX: TCheckBox;
    chkLabelsY: TCheckBox;
    procedure chkTest1Click(Sender: TObject);
    procedure chkCombiTestClick(Sender: TObject);
    procedure chkCheckAllClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure ComboBoxSelect(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ExecBtnClick(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure editEnter(Sender: TObject);
    procedure editClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fDlgOptions: TGraphDlgOptions;
    fCmdString: string;
    procedure UpdateInduvidualTests();
    procedure UpdateAllTests();
    procedure UpdateCombiTests();
    function MakeCmdString(): string;
  public
    { Public declarations }
    procedure Initialize(const DialogOptions: TGraphDlgOptions);
    property CmdString: string read fCmdString;
  end;

implementation

{$R *.dfm}

uses
  UTranslation, Masks, UCmdProcessor, UMain, UInifile;

const
  rsTitle    = 'Title';
  rsSubTitle = 'Subtitle';
  rsFoot     = 'Footnote';
  rsSubFoot  = 'Subfoot';
  rsXText    = 'XText';
  rsYText    = 'YText';
  rsXLabel   = 'XLabel';
  rsSaveAs   = 'Save File As';

{ TGraphDlgOptions }

constructor TGraphDlgOptions.Create;
begin
  fLabels := TStringList.Create;
  fLabels.Add('X Variable');
  fLabels.Add('Y Variable');
  fVarCount := 2;
  fVarNames := TStringList.Create;
  fVarNames.Add('');
  fVarNames.Add('');
  fByLabel  := 'By variable (optional)';
  fWLabel   := 'Weight variable (optional)';
  SetLength(fTypes, 2);
  fDefaults := [];
  fTitle := '';
  fCmd := '';
  fCmdOptions := '';
end;

destructor TGraphDlgOptions.Destroy;
begin
  FreeAndNil(fLabels);
  FreeAndNil(fVarNames);
  inherited;
end;

function TGraphDlgOptions.GetLabel(const index: integer): string;
begin
  result := '';
  if (index < 0) or (index > fLabels.Count-1) then exit;
  result := fLabels[index];
end;

function TGraphDlgOptions.GetTypes(const index: integer): TEpiDatatypes;
begin
  result := [];
  if (index < 0) or (index > fVarCount-1) then exit;
  result := fTypes[index];
end;

function TGraphDlgOptions.GetVarName(const index: integer): string;
begin
  result := '';
  if (index < 0) or (index > fVarNames.Count-1) then exit;
  result := fVarNames[index];
end;

procedure TGraphDlgOptions.SetLabel(const index: integer;
  const aLabel: string);
begin
  if (index < 0) or (index > fLabels.Count-1) then exit;
  fLabels[index] := aLabel;
end;

procedure TGraphDlgOptions.SetTypes(const index: integer;
  const aTypes: TEpiDatatypes);
begin
  if (index < 0) or (index > fVarCount-1) then exit;
  fTypes[index] := aTypes;
end;

procedure TGraphDlgOptions.SetVarCount(const count: integer);
var
  i: integer;
begin
  if Count = fVarCount then exit;
  for i := 1 to (Count - fVarCount) do
  begin
    fVarNames.Add('');
    fLabels.Add('');
  end;
  SetLength(fTypes, count);
  fVarCount := count;
end;

procedure TGraphDlgOptions.SetVarName(const index: integer;
  const aLabel: string);
begin
  if (index < 0) or (index > fVarNames.Count-1) then exit;
  fVarNames[index] := aLabel;
end;

{ TGraphDialog }

procedure TGraphDialog.chkTest1Click(Sender: TObject);
begin
  UpdateAllTests();
  UpdateCombiTests();
end;

procedure TGraphDialog.chkCombiTestClick(Sender: TObject);
begin
  UpdateInduvidualTests();
  UpdateAllTests();
end;

procedure TGraphDialog.chkCheckAllClick(Sender: TObject);
begin
  UpdateInduvidualTests();
//  UpdateCombiTests();
end;


procedure TGraphDialog.UpdateAllTests;
begin
  chkCheckAll.OnClick := nil;
  chkCheckAll.Checked :=
     chkTest1.Checked and
     chkTest2.Checked and
     chkTest3.Checked and
     chkTest4.Checked and
     chkTest5.Checked;
  chkCheckAll.OnClick := chkCheckAllClick;
end;

procedure TGraphDialog.UpdateCombiTests;
begin
  chkCombiTest.OnClick := nil;
  chkCombiTest.Checked :=
     chkTest1.Checked and
     chkTest2.Checked and
     chkTest3.Checked;
  chkCombiTest.OnClick := chkCombiTestClick;
end;

procedure TGraphDialog.UpdateInduvidualTests;
begin
  chkTest1.OnClick := nil;
  chkTest2.OnClick := nil;
  chkTest3.OnClick := nil;
  chkTest4.OnClick := nil;
  chkTest5.OnClick := nil;
  chkTest1.Checked := chkCheckAll.Checked or chkCombiTest.Checked;
  chkTest2.Checked := chkCheckAll.Checked or chkCombiTest.Checked;
  chkTest3.Checked := chkCheckAll.Checked or chkCombiTest.Checked;
  chkTest4.Checked := chkCheckAll.Checked;
  chkTest5.Checked := chkCheckAll.Checked;
  chkTest1.OnClick := chkTest1Click;
  chkTest2.OnClick := chkTest1Click;
  chkTest3.OnClick := chkTest1Click;
  chkTest4.OnClick := chkTest1Click;
  chkTest5.OnClick := chkTest1Click;
end;

procedure TGraphDialog.Initialize(const DialogOptions: TGraphDlgOptions);

  procedure InitCombo(cb: TComboBox; AllowTypes: TEpiDatatypes);
  var
    i: integer;
  begin
    cb.Items.BeginUpdate;
    for i := 0 to Dm.Dataframe.VectorCount -1 do
      if Dm.Dataframe.Vectors[i].DataType in AllowTypes then
        cb.Items.Add(Dm.Dataframe.Vectors[i].Name);
    cb.Items.EndUpdate;
  end;

begin
  fDlgOptions := DialogOptions;
  fCmdString := '';
  Self.Caption := fDlgOptions.Title;

  // Enable only boxes we need.
  if fDlgOptions.VarCount < 4 then cbVar4.Enabled := false
  else InitCombo(cbVar4, fDlgOptions.BoxTypes[3]);
  if fDlgOptions.VarCount < 3 then cbVar3.Enabled := false
  else InitCombo(cbVar3, fDlgOptions.BoxTypes[2]);
  if fDlgOptions.VarCount < 2 then cbVar2.Enabled := false
  else InitCombo(cbVar2, fDlgOptions.BoxTypes[1]);
  InitCombo(cbVar1, fDlgOptions.BoxTypes[0]);
  cbBy.Enabled := (goBy in fDlgOptions.Defaults);
  if cbBy.Enabled then InitCombo(cbBy, fDlgOptions.ByTypes);
  cbWeight.Enabled := (goWeight in fDlgOptions.Defaults);
  if cbWeight.Enabled then InitCombo(cbWeight, fDlgOptions.WeightTypes);

  InitCombo(cbXLabel, [EpiTyBoolean, EpiTyString, EpiTyUppercase, EpiTyDate, EpiTyInteger, EpiTyFloat]);

  // Show SPC
  if not (goSPC in fDlgOptions.Defaults) then
    tabSPC.PageControl := nil;

  ResetBtnClick(self);
end;

procedure TGraphDialog.ResetBtnClick(Sender: TObject);

  procedure UpdateCombo(cb: TComboBox; const aLabel: string; const VarName: string);
  begin
    if not cb.Enabled then exit;
    cb.ItemIndex := -1;
    cb.Text := aLabel;
    if trim(VarName) <> '' then
    begin
      cb.ItemIndex := cb.Items.IndexOf(VarName);
      ComboBoxSelect(cb);
    end;
  end;

begin
  // Variables page:
  lblVar1.Caption := '';
  lblVar2.Caption := '';
  lblVar3.Caption := '';
  lblVar4.Caption := '';
  lblBy.Caption   := '';
  lblWeight.Caption := '';
  UpdateCombo(cbVar1, fDlgOptions.BoxLabels[0], fDlgOptions.VarNames[0]);
  UpdateCombo(cbVar2, fDlgOptions.BoxLabels[1], fDlgOptions.VarNames[1]);
  UpdateCombo(cbVar3, fDlgOptions.BoxLabels[2], fDlgOptions.VarNames[2]);
  UpdateCombo(cbVar4, fDlgOptions.BoxLabels[3], fDlgOptions.VarNames[3]);
  UpdateCombo(cbBy,   fDlgOptions.ByLabel, '');
  UpdateCombo(cbWeight, fDlgOptions.WeightLabel, '');

  // SPC Page:
  if Assigned(tabSPC.PageControl) then
  begin
    chkCheckAll.Checked := (goTestAll in fDlgOptions.Defaults);
    if not chkCheckAll.Checked then
      chkCombiTest.Checked := (goTest in fDlgOptions.Defaults);
    edBreak1.Clear;
    edBreak2.Clear;
    edBreak3.Clear;
    chkSigmaLine.Checked := false;
    chkInfo.Checked := True;
  end;

  // Graph/Axis page:
  // - show...
  chkLegend.Checked := (goLegend in fDlgOptions.Defaults);
  chkFrame.Checked := false;
  chkN.Checked := false;
  chkGridH.Checked := false;
  chkGridV.Checked := false;
  // - graph size
  edSizeX.Clear;
  edSizeY.Clear;
  // - axis...
  chkHideX.Checked := false;
  chkHideY.Checked := false;
  chkInvX.Checked := false;
  chkInvY.Checked := false;
  chkLogX.Checked := false;
  chkLogY.Checked := false;
  chkTickX.Checked := True;
  chkTickY.Checked := True;
  edMinX.Clear;
  edMaxX.Clear;
  edIncX.Clear;
  edMinY.Clear;
  edMaxY.Clear;
  edIncY.Clear;
  chkEdit.Checked := false;
  btnXangleDefault.Checked := True;

  // Titles page:
  edTitle.Text := rsTitle;
  edSubtitle.Text := rsSubTitle;
  edFoot.Text := rsFoot;
  edSubFoot.Text := rsSubFoot;
  edXtext.Text := rsXText;
  edYText.Text := rsYText;
  edFontSz.Clear;
  mmTxt.Clear;
  chkTxtBox.Checked := false;
  edTxtX.Clear;
  edTxtY.Clear;

  // Misc. page:
  cbXLabel.Text := rsXLabel;
  edSaveAs.Text := rsSaveAs;
end;

procedure TGraphDialog.ComboBoxSelect(Sender: TObject);
var
  i : integer;
  v,s : string;
begin
   i:= (Sender as TComboBox).ItemIndex;
   v := Trim((Sender as TComboBox).Items[i]);
   s := DM.Dataframe.VectorByName[v].GetVariableLabel;
   case (Sender as TComboBox).Tag of
     1: lblVar1.Caption := S;
     2: lblVar2.Caption := S;
     3: lblVar3.Caption := S;
     4: lblVar4.Caption := S;
     5: lblBy.Caption   := S;
     6: lblWeight.Caption := S;
   end;
end;

function TGraphDialog.MakeCmdString: string;
var
  s: string;

  procedure AddText(var Cmd: string; const OptionText: string);
  begin
    Cmd := Cmd + ' ' + OptionText;
  end;

  procedure AddChkBox(chkBox: TCheckBox; var Cmd: string; const OptionText: string);
  begin
    if chkBox.Checked then AddText(cmd, OptionText);
  end;

  procedure AddUnChkBox(chkBox: TCheckBox; var Cmd: string; const OptionText: string);
  begin
    if not chkBox.Checked then AddText(cmd, OptionText);
  end;

  procedure AddCombo(cb: TComboBox; var Cmd: string; const OptionText: string);
  begin
    if (cb.Visible) and (cb.ItemIndex <> -1) and (Trim(cb.Text) <> '') then AddText(cmd, OptionText);
  end;

  procedure AddEdit(ed: TCustomEdit; var Cmd: string; const OptionText: string; const Compare: string = '');
  begin
    if Trim(ed.Text) <> Compare then AddText(Cmd, OptionText);
  end;

  function BtnOption(btn :TRadioButton; var Cmd: String; const optiontext: string):boolean;
  begin
    if btn.Checked then AddText(Cmd, OptionText);
  end;


begin
  result := fDlgOptions.Cmd;

  // Variables page:
  AddCombo(cbVar1, result, cbVar1.Text);
  AddCombo(cbVar2, result, cbVar2.Text);
  AddCombo(cbVar3, result, cbVar3.Text);
  AddCombo(cbVar4, result, cbVar4.Text);
  AddCombo(cbBy, result, '/By=' + cbBy.Text);
  AddCombo(cbWeight, result, '/W=' + cbWeight.Text);

  // Addition options specified beforehand. Allways applied.
  AddText(result, fDlgOptions.CmdOptions);

  // SPC Page:
  if Assigned(tabSPC.PageControl) then
  begin
    AddChkBox(chkTest1, result, '/T1');
    AddChkBox(chkTest2, result, '/T2');
    AddChkBox(chkTest3, result, '/T3');
    AddChkBox(chkTest4, result, '/T4');
    AddChkBox(chkTest5, result, '/T5');
    AddChkBox(chkSigmaLine, result, '/Sl');
    AddUnChkBox(chkInfo, result, '/Noinf');
    AddEdit(edBreak1, result, '/B=' + edBreak1.Text);
    AddEdit(edBreak2, result, '/B=' + edBreak2.Text);
    AddEdit(edBreak3, result, '/B=' + edBreak3.Text);
    AddEdit(edFreeze, result, '/F=' + Trim(edFreeze.Text));
  end;

  // Graph/Axis page:
  // - Show box...
  AddChkBox(chkLegend, result, '/Legend');
  AddChkBox(chkFrame, result, '/Frame');
  AddChkBox(chkN, result, '/N');
  AddChkBox(chkGridH, result, '/Hgrid');
  AddChkBox(chkGridV, result, '/Vgrid');
  // - Graph size...
  AddEdit(edSizeX, result, '/SizeX=' + Trim(edSizeX.Text));
  AddEdit(edSizeY, result, '/SizeY=' + Trim(edSizeY.Text));
  // - Axis...
  AddChkBox(chkHideX, result, '/Xhide');
  AddChkBox(chkHideY, result, '/Yhide');
  AddChkBox(chkInvX, result, '/Xinv');
  AddChkBox(chkInvY, result, '/Yinv');
  AddChkBox(chkLogX, result, '/Xlog');
  AddChkBox(chkLogY, result, '/Ylog');
  AddUnChkBox(chkTickX, result, '/Noxtick');
  AddUnChkBox(chkTickY, result, '/Noytick');
  AddUnChkBox(chkLabelsX, result, '/Noxlabel');
  AddUnChkBox(chkLabelsY, result, '/Noylabel');
  AddEdit(edMinX, result, '/Xmin=' + Trim(edMinX.Text));
  AddEdit(edMaxX, result, '/Xmax=' + Trim(edMaxX.Text));
  AddEdit(edMinY, result, '/Ymin=' + Trim(edMinY.Text));
  AddEdit(edMaxY, result, '/Ymax=' + Trim(edMaxY.Text));
  AddEdit(edIncX, result, '/Xinc=' + Trim(edIncX.Text));
  AddEdit(edIncY, result, '/Yinc=' + Trim(edIncY.Text));
  // - Ticks
  BtnOption(btnXangleXA, result, ' /XA');
  BtnOption(btnXangle45, result, ' /X45');
  BtnOption(btnXangle90, result, ' /X90');
  // - Others
  AddChkBox(chkEdit, result, '/Edit');

  // Titles page:
  AddEdit(edTitle, result, '/Ti="' + edTitle.Text + '"', rsTitle);
  AddEdit(edSubtitle, result, '/Sub="' + edSubtitle.Text + '"', rsSubTitle);
  AddEdit(edFoot, result, '/Fn="' + edFoot.Text + '"', rsFoot);
  AddEdit(edSubFoot, result, '/Subfoot="' + edSubFoot.Text + '"', rsSubFoot);
  AddEdit(edXtext, result, '/Xtext="' + edXtext.Text + '"', rsXText);
  AddEdit(edYtext, result, '/Ytext="' + edYtext.Text + '"', rsYText);
  AddEdit(edFontSz, result, '/Fontsize=' + edFontSz.Text);
  // - Text box.
  // TODO -o Torsten: Support multiple lines in text box
  if mmTxt.Lines.Count > 0 then
  begin
    s := '/Text="' + Trim(edTxtX.Text) + ',' + Trim(edTxtY.Text);
    mmTxt.Lines.BeginUpdate;
    mmTxt.Lines.Delimiter := #13;
//    mmTxt.Lines.QuoteChar := ' ';
    s := s + ',' + Trim(mmTxt.Lines.DelimitedText) + ',';
    mmTxt.Lines.EndUpdate;
    if chkTxtBox.Checked then
      s := s + '1'
    else
      s := s + '0';
    s := s + '"';
    AddText(result, s);
  end;

  // Misc. page:
  AddCombo(cbXLabel, result, '/Xlabel=' + cbXLabel.Text);
  AddEdit(edSaveAs, result, '/Save="' + edSaveAs.Text + '"', rsSaveAs);
end;

procedure TGraphDialog.RunBtnClick(Sender: TObject);
begin
  fCmdString := MakeCmdString;
end;

procedure TGraphDialog.FormShow(Sender: TObject);
const
  def: TFormDefaults = (Section: 'GraphDlg';
                        Top: 60; Left: 300;
                        Width: 515; Height: 295;
                        Maximize: false);
begin
  OIniFile.LoadForm(self, def);
  PageControl1.ActivePage := tabVariables;
  cbVar1.SetFocus;
end;

procedure TGraphDialog.ExecBtnClick(Sender: TObject);
begin
  aMainForm.doCommand(MakeCmdString);
end;

procedure TGraphDialog.PasteBtnClick(Sender: TObject);
begin
  aMainForm.CmdEdit.Text := MakeCmdString;
end;

procedure TGraphDialog.editEnter(Sender: TObject);
begin
  if not (Sender is TMaskEdit) then exit;  
  TMaskEdit(Sender).SelStart := Length(Trim(TMaskEdit(Sender).Text));
  TMaskEdit(Sender).SelLength := 0;
end;

procedure TGraphDialog.editClick(Sender: TObject);
begin
  if not (Sender is TMaskEdit) then exit;  
  if (Length(Trim(TMaskEdit(Sender).Text)) > 0) and
     (TMaskEdit(Sender).SelStart < Length(Trim(TMaskEdit(Sender).Text))) then
   exit;
  TMaskEdit(Sender).SelStart := Length(Trim(TMaskEdit(Sender).Text));
  TMaskEdit(Sender).SelLength := 0;
end;

procedure TGraphDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  OInifile.SaveCurrentForm(Self, 'GraphDlg');
end;

end.
