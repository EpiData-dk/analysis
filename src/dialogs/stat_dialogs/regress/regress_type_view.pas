unit regress_type_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls, ExtCtrls, spin,
  StdCtrls, regress_type_model, fields_combobox, stat_dialog_custom_view;

type

  { TRegressTypeView }

  TRegressTypeView = class(TCustomStatDialogView)
  private
    FDataModel: TRegressTypeModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    FTypeRadios: TRadioGroup;
    FDegreeBox: TGroupBox;
    FDegreeLabel: TLabel;
    FDegreeEdit: TSpinEdit;
    FMultiVarSelect: Boolean; 
    FOnModified: IStatDiaglogViewModified;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombos();
    procedure CreateTypeRadios(RadioGroup: TRadioGroup);
    procedure TypeSelectionChanged(Sender: TObject);
    procedure CreateDegreeEdit(Field: TSpinEdit);
    procedure DegreeChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent);
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: TRegressTypeModel);
  end;

implementation

uses
  epidatafiles, epidatafilestypes;

const
  XVARIABLE_TAG = Ord(rvX);
  YVARIABLE_TAG = Ord(rvY);
  FITVARIABLE_TAG = Ord(rvF);

{ TRegressTypeView }

procedure TRegressTypeView.CreateTypeRadios(RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Simple linear regression (one independent variable)');
  RadioGroup.Items.Add('Multivariable linear regression');
  RadioGroup.Items.Add('Polynomial regression (one independent variable)');
  RadioGroup.Items.Add('Logistic regression');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @TypeSelectionChanged;
end;

procedure TRegressTypeView.CreateDegreeEdit(Field: TSpinEdit);
var
  i: Integer;
begin
  Field.OnChange := @DegreeChanged;
end;

procedure TRegressTypeView.VariableSelect(Sender: TObject);
var
  Field: TEpiField;
  ComboBox: TCustomComboBox;
begin
  ComboBox := TCustomComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);

  case ComboBox.Tag of
    XVARIABLE_TAG:
      FDataModel.XVariable := Field;
    YVARIABLE_TAG:
      FDataModel.YVariable := Field;
    FITVARIABLE_TAG:
      FDataModel.FitVariable := Field;
  end;

  UpdateCombos();
  DoModified();
end;

procedure TRegressTypeView.UpdateCombos();
var
  Field: TEpiField;
  ComboBox: TEpiFieldsComboBox;
  i: Integer;
begin
  for i := Low(FComboBoxes) to High(FComboBoxes) do
    begin
      ComboBox := FComboBoxes[i];

      Field := ComboBox.SelectedField;
      ComboBox.Fields.Free;
      ComboBox.Fields := nil;
      ComboBox.Fields := FDataModel.GetComboFields(TRegressVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

constructor TRegressTypeView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  FTypeRadios := TRadioGroup.Create(self);
  CreateTypeRadios(FTypeRadios);

  FDegreeBox := TGroupBox.Create(self);
  FDegreeLabel := TLabel.Create(self);

  FDegreeEdit := TSpinEdit.Create(self);
  CreateDegreeEdit(FDegreeEdit);
  FDegreeEdit.MinValue := 2;
  FDegreeEdit.MaxValue := 10;

  SetLength(FComboBoxes, 3);

  FTypeRadios.Parent := self;
  FTypeRadios.Caption := 'Regression Type';
  FTypeRadios.Anchors := [];
  FTypeRadios.AnchorParallel(akTop, 5, Self);
  FTypeRadios.AnchorParallel(akLeft, 5, self);
  FTypeRadios.AnchorParallel(akRight, 5, self);

  FDegreeBox.Parent := self;
  FDegreeBox.Anchors := [];
  FDegreeBox.AnchorToNeighbour(akTop, 0, FTypeRadios);
  FDegreeBox.AnchorParallel(akLeft, 5, self);
  FDegreeBox.AnchorParallel(akRight, 5, self);

  FDegreeLabel.Parent := FDegreeBox;
  // This is an ugly way to position the label
  // There are 2 problems
  // 1 Setting FDegreeBox caption triggers the change action (yields error)
  // 2 The two controls within FDegreeBox do not follow anchors at all,
  //   whether to each other or their parent. Thare are always placed one on top of the other
  FDegreeLabel.Caption := '                Degree of polynomial';
//  FDegreeLabel.Anchors := [];
//  FDegreeLabel.AnchorParallel(akTop, 5, self);
//  FDegreeLabel.AnchorParallel(akleft, 5, self);

  FDegreeEdit.Parent := FDegreeBox;
//  FDegreeEdit.Anchors := [];
//  FDegreeEdit.AnchorParallel(akTop, 35, self);
//  FDegreeEdit.AnchorParallel(akLeft, 10, self);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := [ftInteger, ftFloat];
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, FDegreeBox);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'Dependent Variable';
  FComboBoxes[YVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'Single independent Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := FITVARIABLE_TAG;
  ComboBox.NoItemText := 'Save fitted values to variable';
  FComboBoxes[FITVARIABLE_TAG] := ComboBox;

end;

procedure TRegressTypeView.TypeSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: begin
      FDataModel.RegressType := rtSimple;
      FDegreeBox.Visible := false;
      FComboBoxes[XVARIABLE_TAG].Visible := true;
      FMultiVarSelect := false;
    end;
    1: begin
      FDataModel.RegressType := rtLinear;
      FDegreeBox.Visible := false;
      FComboBoxes[XVARIABLE_TAG].Visible := false;
      FMultiVarSelect := true;
    end;
    2: begin
      FDataModel.RegressType := rtPolynomial;
      FDegreeBox.Visible := true;
      FComboBoxes[XVARIABLE_TAG].Visible := true;
      FMultiVarSelect := false;
    end;
    3: begin
      FDataModel.RegressType := rtLogistic;
      FDegreeBox.Visible := false;
      FComboBoxes[XVARIABLE_TAG].Visible := false;
      FMultiVarSelect := true;
    end;
  end;
  DoModified();
end;

procedure TRegressTypeView.DegreeChanged(Sender: TObject);
begin
  FDataModel.Degree := FDegreeEdit.Value;
  DoModified();
end;

procedure TRegressTypeView.EnterView();
begin
end;

function TRegressTypeView.ExitView(): boolean;
begin
  result := true;
end;

function TRegressTypeView.GetViewCaption(): UTF8String;
begin
  result := 'Type and Dependent Variable';
end;

procedure TRegressTypeView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.YVariable := nil;
  FDataModel.FitVariable := nil;
  UpdateCombos();
  FTypeRadios.ItemIndex := 1;
  FComboBoxes[XVARIABLE_TAG].Visible := false;
  FDegreeBox.Visible := false;
  FDataModel.RegressType := rtLinear;
  FDataModel.Degree := 2;
  DoModified();
end;

function TRegressTypeView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TRegressTypeView.SetModel(DataModel: TRegressTypeModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;


end.

