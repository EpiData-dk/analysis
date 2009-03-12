unit USpcDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, ComCtrls;

type
  TfrmSPCDialog = class(TForm)
    PageControl1: TPageControl;
    tabVariables: TTabSheet;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    tabGraph: TTabSheet;
    tabSPC: TTabSheet;
    GenGrpBox: TGroupBox;
    LegendChk: TCheckBox;
    FrameChk: TCheckBox;
    HGridChk: TCheckBox;
    VGridChk: TCheckBox;
    NChk: TCheckBox;
    GroupBox1: TGroupBox;
    Label22: TLabel;
    Label23: TLabel;
    SizeX: TMaskEdit;
    SizeY: TMaskEdit;
    AxisGrpBox: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    XHideChk: TCheckBox;
    YHideChk: TCheckBox;
    XInvChk: TCheckBox;
    XLogChk: TCheckBox;
    YInvChk: TCheckBox;
    YLogChk: TCheckBox;
    xtickchk: TCheckBox;
    YtickChk: TCheckBox;
    xmin: TMaskEdit;
    xmax: TMaskEdit;
    ymax: TMaskEdit;
    ymin: TMaskEdit;
    xinc: TMaskEdit;
    YINC: TMaskEdit;
    EditChk: TCheckBox;
    Panel1: TPanel;
    CancelBtn: TButton;
    RunBtn: TButton;
    ResetBtn: TButton;
    PasteBtn: TButton;
    ExecBtn: TButton;
    GroupBox2: TGroupBox;
    cbTest1: TCheckBox;
    cbTest2: TCheckBox;
    cbTest3: TCheckBox;
    cbCheckAll: TCheckBox;
    cbTest4: TCheckBox;
    cbTest5: TCheckBox;
    GroupBox3: TGroupBox;
    edBreak1: TMaskEdit;
    brkLabel: TLabel;
    TxtGRpBox: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    xpos1: TMaskEdit;
    ypos1: TMaskEdit;
    Edit3: TEdit;
    btyp1: TCheckBox;
    MaskEdit1: TMaskEdit;
    Label4: TLabel;
    MaskEdit2: TMaskEdit;
    Label7: TLabel;
    tabTitle: TTabSheet;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit6: TEdit;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    procedure cbTest1Click(Sender: TObject);
    procedure cbCheckAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function ShowSPCDlg():integer;

implementation

{$R *.dfm}

uses
  UTranslation, Masks;

var
  frmSPCDialog: TfrmSPCDialog;

  
function ShowSPCDlg():integer;
begin
  if not assigned(frmSPCDialog) then
  begin
    frmSPCDialog:= TfrmSPCDialog.Create(application);
    OTranslator.TranslateForm(frmSPCDialog); 
  end;
  frmSPCDialog.ShowModal;
  FreeAndNil(frmSPCDialog);
end;



procedure TfrmSPCDialog.cbTest1Click(Sender: TObject);
begin
  cbCheckAll.OnClick := nil;
  cbCheckAll.Checked :=
     cbTest1.Checked and
     cbTest2.Checked and
     cbTest3.Checked and
     cbTest4.Checked and
     cbTest5.Checked;
  cbCheckAll.OnClick := cbCheckAllClick;
end;

procedure TfrmSPCDialog.cbCheckAllClick(Sender: TObject);
begin
  cbTest1.OnClick := nil;
  cbTest2.OnClick := nil;
  cbTest3.OnClick := nil;
  cbTest4.OnClick := nil;
  cbTest5.OnClick := nil;
  cbTest1.Checked := cbCheckAll.Checked;
  cbTest2.Checked := cbCheckAll.Checked;
  cbTest3.Checked := cbCheckAll.Checked;
  cbTest4.Checked := cbCheckAll.Checked;
  cbTest5.Checked := cbCheckAll.Checked;
  cbTest1.OnClick := cbTest1Click;
  cbTest2.OnClick := cbTest1Click;
  cbTest3.OnClick := cbTest1Click;
  cbTest4.OnClick := cbTest1Click;
  cbTest5.OnClick := cbTest1Click;
end;

end.
