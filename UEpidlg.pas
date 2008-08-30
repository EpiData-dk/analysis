unit UEpidlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, SMUtils, Umain, AnsDatatypes,
  CheckLst;


type
  TEpiDlg = class(TForm)
    CmdPanel: TPanel;
    CancelBtn: TButton;
    RunBtn: TButton;
    PasteBtn: TButton;
    ResetBtn: TButton;
    FieldsGroup: TGroupBox;
    VarLabel: TLabel;
    DotsLabel: TLabel;
    GetVar: TCheckListBox;
    AllPanel: TPanel;
    AllBtn: TButton;
    StatPanel: TPanel;
    MeanVar: TLabel;
    ByVar: TLabel;
    MeansBox: TGroupBox;
    TestChk: TCheckBox;
    NoneBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MissingChk: TCheckBox;
    DesignBox: TGroupBox;
    LineClass: TRadioButton;
    BoxClass: TRadioButton;
    FilledClass: TRadioButton;
    ShadedClass: TRadioButton;
    LabelBox: TGroupBox;
    ValueChk: TCheckBox;
    VLabel: TLabel;
    ExecBtn: TButton;
    TimeVar: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetVarClick(Sender: TObject);
    procedure AllBtnClick(Sender: TObject);
    procedure NoneBtnClick(Sender: TObject);
    procedure GetVarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GetVarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ExecBtnClick(Sender: TObject);
  private
    MinVars,MaxVars: integer;
    CmdName :string;
    CmdString : string;
    MAinfrm: TaMainForm;
    ShowVars: boolean;
    DlgResult : integer;
    procedure SetupDlg(const ps: string);
    procedure ParseString(const ps: string);
    function makeCmdString(var cmd: string): boolean;
    procedure InitializeDlg(const ps: string);
  public
    { Public declarations }
  end;

function ShowDlg(frm: TaMainForm;ParseString:string;var CmdString:string):integer;

implementation

uses
  UCmdProcessor, UTranslation;

{$R *.DFM}

var
  EpiDlg: TEpiDlg;

function ShowDlg(frm: TaMainForm;ParseString:string;var CmdString:string):integer;
begin
 if not assigned( EpiDlg) then
 begin
    EpiDlg:= TEpiDlg.Create(application);
    EpiDlg.MAinfrm :=frm;
 end;
  EpiDlg.InitializeDlg(ParseString);
  OTranslator.TranslateForm(EpiDlg);
  EpiDlg.showmodal;
  Result:=EpiDlg.DlgResult;
  CmdString := EpiDlg.CmdString;
end;

procedure TEpiDlg.FormCreate(Sender: TObject);
begin
{  CancelBtn.tag:=DlgResCancel;
  ResetBtn.tag:= DlgResReset;
  PasteBtn.tag:=  DlgResPaste;
  RunBtn.tag:=  DlgResRun;    }
end;


procedure TEpiDlg.InitializeDlg(const ps: string);
begin
  if not dm.CheckDataOpen() then exit; //dm.CheckDataOpen();
  ParseString(ps);
  if pos(ps,' correlate varlist means varlist kwallis varlist regress varlist lifetable varlist ') > 0 then
  begin
    AllPanel.Visible := False;
    StatPanel.Visible := True;
    MeansBox.Visible := False;
    if (pos(ps,' means varlist ') > 0) or (pos(ps,' kwallis varlist ') > 0) then
      begin
           MeansBox.Visible := True;
           testchk.Visible := True;
      end;
    if pos(ps,' kwallis varlist ') > 0 then testchk.Visible := False;
  end
  else
  begin
    AllPanel.Visible := True;
    StatPanel.Visible := False;
    DesignBox.Visible := False;
  end;
  ResetBtnClick(nil);
  SetupDlg(ps);
end;


procedure TEpiDlg.SetupDlg(const ps: string);
begin
  GetVar.Items := dm.dataframe.GetVectorNames(Nil);
  Caption := CmdName;

  if pos(CmdName, ' LIFETABLE' ) > 0 then
  begin
    Label2.Caption := 'Outcome:';
    Label3.Caption := 'Time:(+ by)';
    timevar.visible := True;
  end
  else
    timevar.visible := False;
end;


procedure TEpiDlg.ParseString(const ps: string);
var
 s,remain, v : string;
 c : char;
begin
  c:= #32;
  s := ''; s :=AnsiUpperCase(ps);
  MinVars :=0;
  MaxVars :=MaxInt;
  SplitFirst(s,[#32,'('],v,remain,c);
  if v='' then exit;
  CmdName:= v;
  SplitFirst(remain,[#32,'('],v,remain,c);
  if v='VARLIST' then
  begin
     ShowVars:=true;
  end
  else
  begin
    MinVars :=0;
    MaxVars :=0;
  end;
end;


function TEpiDlg.MakeCmdString(var cmd:string):boolean;
var i : integer;
  function addoption(box :TCheckBox; const option: string):boolean;
     begin
       if box.Checked then cmd := cmd + option + ' ';
     end;
 { function btnoption(btn :TRadioButton; const option: string):boolean;
     begin
       if btn.Checked then cmd := cmd + option + ' ';
     end;
 }
begin
  result := true;
  Cmd:=EpiDlg.Caption;
  if DlgResult in [DlgResCancel, DlgResReset] then exit;
  //GetVarClick(Self);
  if (Cmd = 'REGRESS') or (Cmd = 'CORRELATE') or (Cmd = 'LIFETABLE')then
    begin
      cmd := EpiDlg.caption + ' ' + MeanVar.Caption + ' ' + ByVar.caption;
      if (length(TimeVar.Caption) > 0) then
        cmd := trim(cmd + ' /By=' + TimeVar.Caption);
    end
  else  if (pos(trim(uppercase(Cmd)),'MEANS KWALLIS') > 0)  then
      begin
          if meanvar.caption = '' then
            begin
              if CmdName = 'MEANS' then
                dm.info('Select at least one variable', [], 104001)
              else dm.error('Select two variables', [], 104002);
              exit;
            end;

           if length(ByVar.Caption) > 0 then
             cmd := trim(cmd + ' ' + MeanVar.Caption + ' /By=' + ByVar.Caption)
           else cmd := trim(cmd + ' ' + MeanVar.Caption + ' ' + ByVar.Caption);
           addoption(TestChk,' /T');
           addoption(MissingCHK,' /M');
           //common options: e.g. shov values etc.
           addoption(ValueCHK,' /VL');
      end
 else
      begin // list describe
        cmd := EpiDlg.caption + ' ';
        for i := 0 to GetVar.Count-1 do
         if (GetVar.State[i] = cbChecked) then cmd := cmd + GetVar.Items[i] + ' ';
      end;

  if LineClass.checked or BoxClass.Checked or ShadedClass.Checked or FilledClass.Checked then
    begin    // run set command:
      CmdName := 'Set table design=';
     if BoxClass.Checked then CmdName:=CmdName + 'box'
       else if ShadedClass.Checked then CmdName:=CmdName + 'shaded'
       else if FilledClass.Checked then CmdName:=CmdName + 'filled'
       else CmdName:=CmdName + 'line';
    cmd := CmdName + ' ; ' + Cmd;

  end;  // end set design
end;

procedure TEpiDlg.CancelBtnClick(Sender: TObject);
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
  makeCmdString(CmdString);
  close;
end;

procedure TEpiDlg.ResetBtnClick(Sender: TObject);
var
  i: integer;
begin
   // checkboxes
  for i := 0 to EpiDlg.ComponentCount -1 do
    if (EpiDlg.Components[i] is TCheckBox) then
      (EpiDlg.Components[i] as TCheckBox).Checked := false;

      Byvar.Caption := '';
      Meanvar.Caption := '';

  for i := 0 to GetVar.Count -1 do
    GetVar.State[i] := cbUnchecked;

  if GetVar.Count > 0 then GetVarClick(Self);
end;

procedure TEpiDlg.FormShow(Sender: TObject);
begin
   GetVar.SetFocus;
end;

procedure TEpiDlg.GetVarClick(Sender: TObject);
var
  i,j: integer;
     function stripvarname(s,varlist: string):string;
   begin
      result := varlist;
      if pos(s+' ',varlist+' ') > 0 then
         result := copy(varlist,1,pos(s,varlist)-1)
                + copy(varlist,(pos(s,varlist)+length(s)),length(varlist)-length(s));
   end;

begin

  if pos(cmdname,' REGRESS MEANS CORRELATE KWALLIS LIFETABLE ') = 0 then exit;

     // check if clear for any of the two labels is needed:
   for i := 0 to GetVar.Count-1 do
   begin
     if (GetVar.State[i] = cbUnChecked) and (MeanVar.Caption = GetVar.Items[i]) then MeanVar.Caption := '';
     if (GetVar.State[i] = cbUnChecked) then ByVar.Caption := StripVarName(GetVar.Items[i],ByVar.Caption);
   end;

  if  ((GetVar.ItemIndex = -1) or (GetVar.State[GetVar.ItemIndex] = cbUnChecked)) then exit;   // do not do anything

  // else how many checked now ?
  j := 0;
  for i := 0 to GetVar.Count-1 do
    if (GetVar.State[i] = cbChecked)  then inc(j);

    if j = 0 then exit;   // do not do anything

    // add var name to appropriate place for checked options:
    if (j = 1) and (trim(MeanVar.Caption) = '')
      then MeanVar.Caption := GetVar.Items[Getvar.ItemIndex]
        else if (j = 2) and (trim(ByVar.Caption) = '') then ByVar.Caption := GetVar.Items[GetVar.ItemIndex]
        {    // dm.info(columnvar.caption + '<br>' + rowvar.caption + '<br>' + variables.caption);}
        else if (j > 2) then
          if (cmdname = 'REGRESS') or (cmdname = 'CORRELATE' ) then
            ByVar.Caption := ByVar.Caption + ' ' + GetVar.Items[GetVar.ItemIndex]
          else if (cmdname = 'MEANS' ) then
            dm.error('Max two variables', [], -1, 104003)
          else if (cmdname = 'LIFETABLE') and (j = 3) then
            TimeVar.Caption := GetVar.Items[GetVar.ItemIndex]
          else
            dm.error('Max three variables', [], -1, 104003);
end;


procedure TEpiDlg.AllBtnClick(Sender: TObject);
VAR
  n:Integer;
begin
  FOR n:=0 TO GetVar.Items.Count-1 DO
    GetVar.State[n] := cbChecked;
    GetVarClick(Getvar);
end;

procedure TEpiDlg.NoneBtnClick(Sender: TObject);
VAR
  n:Integer;
begin
  FOR n:=0 TO GetVar.Items.Count-1 DO
    GetVar.State[n] := cbUnChecked;
    GetVarClick(Getvar);
end;


procedure TEpiDlg.GetVarKeyDown(Sender: TObject; var Key: Word;
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

procedure TEpiDlg.GetVarMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
VAR
  pos: TPoint;
  n: Integer;
begin
  pos.x:=X; pos.y:=Y;
  n:= GetVar.ItemAtPos(Pos,True);
  IF (n<>-1) and (n < GetVar.Count) then
    VLabel.Caption:= dm.dataframe.VectorByName[GetVar.Items[n]].GetVariableLabel
  ELSE
    VLabel.Caption:='';
end;


procedure TEpiDlg.ExecBtnClick(Sender: TObject);
begin
  DlgResult := DlgResRun;
  makeCmdString(CmdString);
  if (pos(' ',CmdString) > 0)
      or (pos('Des',CmdString) > 0 )
      then aMainForm.doCommand(CmdString);
  DlgResult := DlgResCancel;
  CmdString := '';
end;

end.
