unit survival_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls,
  survival_model, fields_combobox, stat_dialog_custom_view;

type

  { TSurvivalDialogVariablesView }

  TSurvivalDialogVariablesView = class(TCustomStatDialogView)
  private
    FDataModel: TSurvivalDialogVariableModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    FFailureGroup: TRadioGroup;
    FStrataGroup: TRadioGroup;
    FVerticalDivider: TBevel;
    procedure VariableSelect(Sender: TObject);
    procedure CreateFailureRadios(RadioGroup: TRadioGroup);
    procedure FailureSelectionChanged(Sender: TObject);
    procedure CreateRefStratumRadios(RadioGroup: TRadioGroup);
    procedure RefStratumSelectionChanged(Sender: TObject);
    procedure UpdateCombos();
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: TSurvivalDialogVariableModel);
  end;

implementation

uses
  epidatafiles, epidatafilestypes;

const
  OUTCOMEV_TAG   = Ord(svOutcome);
  TIME1V_TAG     = Ord(svTime1);
  TIME2V_TAG     = Ord(svTime2);
  WEIGHTV_TAG    = Ord(svW);
  BYV_TAG        = Ord(svBy);

{ TSurvivalDialogVariablesView }

procedure TSurvivalDialogVariablesView.VariableSelect(Sender: TObject);
var
  Field: TEpiField;
  ComboBox: TCustomComboBox;
begin
  ComboBox := TCustomComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);
  case ComboBox.Tag of
    OUTCOMEV_TAG:
      begin
        FDataModel.OutcomeVariable := Field;
        if not (Assigned(Field)) then
          FDataModel.Failure := '';
        CreateFailureRadios(FFailureGroup);
      end;

    TIME1V_TAG:
      begin
        FDataModel.Time1Variable := Field;
        if Assigned(Field) then
          if (Field.FieldType in DateFieldTypes) then
            FComboBoxes[TIME2V_TAG].Visible := true
          else
            with (FComboBoxes[TIME2V_TAG]) do
            begin
              Visible := false;
              Fields.Free;
              Fields := nil;
            end;
      end;

    TIME2V_TAG:
      FDataModel.Time2Variable := Field;

    WEIGHTV_TAG:
      FDataModel.WVariable := Field;

    BYV_TAG:
      begin
        FDataModel.ByVariable := Field;
        if (not Assigned(Field)) then
          FDataModel.RefStratum := '';
        CreateRefStratumRadios(FStrataGroup);
      end;
  end;

  UpdateCombos();
  DoModified();
end;

procedure TSurvivalDialogVariablesView.UpdateCombos();
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
      ComboBox.Fields := FDataModel.GetComboFields(TSurvivalDialogVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

constructor TSurvivalDialogVariablesView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, Ord(High(TSurvivalDialogVariable)) + 1);

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
  ComboBox.Tag := OUTCOMEV_TAG;
  ComboBox.NoItemText := 'Outcome Variable';
  FComboBoxes[OUTCOMEV_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := [ftInteger] + DateFieldTypes;
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorToNeighbour(akRight, 10, FVerticalDivider);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := TIME1V_TAG;
  ComboBox.NoItemText := 'Time Variable (integer time or date)';
  FComboBoxes[TIME1V_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := DateFieldTypes;
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorToNeighbour(akRight, 10, FVerticalDivider);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := TIME2V_TAG;
  ComboBox.NoItemText := 'Time Variable (optional date)';
  FComboBoxes[TIME2V_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.AnchorToNeighbour(akLeft, 10, FVerticalDivider);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := BYV_TAG;
  ComboBox.NoItemText := 'By Variable (optional)';
  FComboBoxes[BYV_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorToNeighbour(akLeft, 10, FVerticalDivider);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := WEIGHTV_TAG;
  ComboBox.NoItemText := 'Weight Variable (optional)';
  FComboBoxes[WEIGHTV_TAG] := ComboBox;

  FFailureGroup := TRadioGroup.Create(TheOwner);
  FFailureGroup.Parent := self;
  FFailureGroup.Caption := 'Outcome value indicating death / failure';
  FFailureGroup.Anchors := [];
  FFailureGroup.AnchorParallel(akLeft, 10, Self);
  FFailureGroup.AnchorToNeighbour(akTop, 10, FComboBoxes[TIME2V_TAG]);
  FFailureGroup.AnchorToNeighbour(akRight, 10, FVerticalDivider);
  FFailureGroup.AnchorParallel(akBottom, 10, Self);

  FStrataGroup := TRadioGroup.Create(TheOwner);
  FStrataGroup.Parent := self;
  FStrataGroup.Caption := 'Reference stratum (only used with !t)';
  FStrataGroup.Anchors := [];
  FStrataGroup.AnchorParallel(akRight, 10, Self);
  FStrataGroup.AnchorToNeighbour(akTop, 10, FComboBoxes[WEIGHTV_TAG]);
  FStrataGroup.AnchorToNeighbour(akLeft, 10, FVerticalDivider);
  FStrataGroup.AnchorParallel(akBottom, 10, Self);

  EnterView(); // Must do this to get combo boxes aligned and visible
end;

procedure TSurvivalDialogVariablesView.CreateFailureRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items := FDatamodel.OutcomeValues;
  RadioGroup.Visible := true;
  RadioGroup.OnSelectionChanged := @FailureSelectionChanged;
end;

procedure TSurvivalDialogVariablesView.FailureSelectionChanged(
  Sender: TObject);
begin
  FDataModel.Failure := TRadioGroup(Sender).Items[TRadioGroup(Sender).ItemIndex];
  DoModified();
end;

procedure TSurvivalDialogVariablesView.CreateRefStratumRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items := FDatamodel.StrataValues;
  RadioGroup.Visible := true;
  RadioGroup.OnSelectionChanged := @RefStratumSelectionChanged;
end;

procedure TSurvivalDialogVariablesView.RefStratumSelectionChanged(
  Sender: TObject);
begin
  FDataModel.RefStratum := TRadioGroup(Sender).Items[TRadioGroup(Sender).ItemIndex];
  DoModified();
end;

procedure TSurvivalDialogVariablesView.EnterView();
begin
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function TSurvivalDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TSurvivalDialogVariablesView.GetViewCaption(): UTF8String;
begin
  result := 'Variables';
end;

procedure TSurvivalDialogVariablesView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.OutcomeVariable := nil;
  FDataModel.Time1Variable := nil;
  FDataModel.Time2Variable := nil;
  FDataModel.WVariable := nil;
  FDataModel.ByVariable := nil;

  UpdateCombos();

  if (FFailureGroup.Items.Count > 0) then
    FFailureGroup.Visible := false;       // easier than emptying the group

  if (FStrataGroup.Items.Count > 0) then
    FStrataGroup.Visible := false;

  DoModified();
end;

function TSurvivalDialogVariablesView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TSurvivalDialogVariablesView.SetModel(
  DataModel: TSurvivalDialogVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

end.
