unit survival_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, epidatafilestypes,
  stat_dialog_contribution, survival_model, fields_combobox, stat_dialog_custom_view;

type

  { TSurvivalStatDialogVariablesView }

  TSurvivalStatDialogVariablesView = class(TCustomStatDialogView)
  private
    FDataModel: TSurvivalStatDialogVariableModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    FOnModified: IStatDiaglogViewModified;
    FFailureGroup: TRadioGroup;
    FVerticalDivider: TBevel;
    procedure VariableSelect(Sender: TObject);
    procedure CreateFailureRadios(RadioGroup: TRadioGroup);
    procedure FailureSelectionChanged(Sender: TObject);
    procedure UpdateCombos();
  public
    constructor Create(TheOwner: TComponent);
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: TSurvivalStatDialogVariableModel);
  end;

implementation

uses
  epidatafiles;

const
  XVARIABLE_TAG   = Ord(tvX);
  TVARIABLE_TAG   = Ord(tvT);
  T2VARIABLE_TAG  = Ord(tvT2);
  WVARIABLE_TAG   = Ord(tvW);
  ByVariable_TAG  = Ord(tvBy);

{ TSurvivalStatDialogVariablesView }

procedure TSurvivalStatDialogVariablesView.VariableSelect(Sender: TObject);
var
  Field: TEpiField;
  ComboBox: TCustomComboBox;
begin
  ComboBox := TCustomComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);

  case ComboBox.Tag of
    XVARIABLE_TAG:
      begin
        FDataModel.XVariable := Field;
        if (Assigned(Field)) then
          FDataModel.FailureType := FDataModel.XVariable.FieldType
        else
          FDataModel.Failure := '';
        CreateFailureRadios(FFailureGroup);
      end;

    TVARIABLE_TAG:
      begin
        FDataModel.TVariable := Field;
        if (Field.FieldType in DateFieldTypes) then
          FComboBoxes[T2VARIABLE_TAG].Visible := true
        else
          with (FComboBoxes[T2VARIABLE_TAG]) do
          begin
            Visible := false;
            Fields.Free;
            Fields := nil;
          end;
      end;

    T2VARIABLE_TAG:
      FDataModel.T2Variable := Field;

    WVARIABLE_TAG:
      FDataModel.WVariable := Field;

    ByVariable_TAG:
      FDataModel.ByVariable := Field;

  end;

  UpdateCombos();
  DoModified();
end;

procedure TSurvivalStatDialogVariablesView.UpdateCombos();
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
      ComboBox.Fields := FDataModel.GetComboFields(TSurvivalStatDialogVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

constructor TSurvivalStatDialogVariablesView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, Ord(High(TSurvivalStatDialogVariable)) + 1);

  FVerticalDivider := TBevel.Create(self);
  FVerticalDivider.Parent := self;
  FVerticalDivider.Style := bsLowered;
  FVerticalDivider.Shape := bsSpacer;
  FVerticalDivider.Width := 5;
  FVerticalDivider.Anchors := [];
  FVerticalDivider.AnchorParallel(akTop, 5, Self);
  FVerticalDivider.AnchorParallel(akBottom, 0, Self);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := [ftInteger, ftString, ftUpperString];
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorToNeighbour(akRight, 10, FVerticalDivider);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'Outcome Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := [ftInteger] + DateFieldTypes;
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorToNeighbour(akRight, 10, FVerticalDivider);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := TVARIABLE_TAG;
  ComboBox.NoItemText := 'Time Variable (integer time or date)';
  FComboBoxes[TVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := DateFieldTypes;
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorToNeighbour(akRight, 10, FVerticalDivider);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := T2VARIABLE_TAG;
  ComboBox.NoItemText := 'Time Variable (optional date)';
  FComboBoxes[T2VARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.AnchorToNeighbour(akLeft, 10, FVerticalDivider);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := ByVariable_TAG;
  ComboBox.NoItemText := 'By Variable (optional)';
  FComboBoxes[ByVariable_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorToNeighbour(akLeft, 10, FVerticalDivider);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := WVARIABLE_TAG;
  ComboBox.NoItemText := 'Weight Variable (optional)';
  FComboBoxes[WVARIABLE_TAG] := ComboBox;

  FFailureGroup := TRadioGroup.Create(TheOwner);
  FFailureGroup.Parent := self;
  FFailureGroup.Caption := 'Outcome value indicating death / failure';
  FFailureGroup.Anchors := [];
  FFailureGroup.AnchorParallel(akLeft, 10, Self);
  FFailureGroup.AnchorToNeighbour(akTop, 10, FComboBoxes[T2VARIABLE_TAG]);
  FFailureGroup.AnchorToNeighbour(akRight, 10, FVerticalDivider);
  FFailureGroup.AnchorParallel(akBottom, 10, Self);

  EnterView(); // Must do this to get combo boxes aligned and visible
end;

procedure TSurvivalStatDialogVariablesView.CreateFailureRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items := FDatamodel.OutcomeValues;
  RadioGroup.Visible := true;
  RadioGroup.OnSelectionChanged := @FailureSelectionChanged;
end;

procedure TSurvivalStatDialogVariablesView.FailureSelectionChanged(
  Sender: TObject);
begin
  FDataModel.Failure := TRadioGroup(Sender).Items[TRadioGroup(Sender).ItemIndex];
  DoModified();
end;

procedure TSurvivalStatDialogVariablesView.EnterView();
begin
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function TSurvivalStatDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TSurvivalStatDialogVariablesView.GetViewCaption(): UTF8String;
begin
  result := 'Variables';
end;

procedure TSurvivalStatDialogVariablesView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.TVariable := nil;
  FDataModel.T2Variable := nil;
  FDataModel.WVariable := nil;
  FDataModel.ByVariable := nil;

  UpdateCombos();

  if (FFailureGroup.Items.Count > 0) then
    FFailureGroup.Visible := false;       // easier than emptying the group

  DoModified();
end;

function TSurvivalStatDialogVariablesView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TSurvivalStatDialogVariablesView.SetModel(
  DataModel: TSurvivalStatDialogVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

end.
