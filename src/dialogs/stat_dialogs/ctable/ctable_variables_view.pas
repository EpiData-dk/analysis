unit ctable_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls, ExtCtrls,
  StdCtrls, ctable_model, fields_combobox, stat_dialog_custom_view;

type

  { TCtableStatDialogVariablesView }

  TCtableStatDialogVariablesView = class(TCustomStatDialogView)
  private
    FDataModel: TCtableStatDialogVariableModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    FOnModified: IStatDiaglogViewModified;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombos();
  public
    constructor Create(TheOwner: TComponent);
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: TCtableStatDialogVariableModel);
  end;

implementation

uses
  epidatafiles, epidatafilestypes;

const
  XVARIABLE_TAG   = Ord(tvX);
  YVARIABLE_TAG   = Ord(tvY);
  ZVARIABLE_TAG   = Ord(tvZ);
  WVARIABLE_TAG   = Ord(tvW);
  BYVARIABLE1_TAG = Ord(tvBy1);
  BYVARIABLE2_TAG = Ord(tvBy2);

{ TCtableStatDialogVariablesView }

procedure TCtableStatDialogVariablesView.VariableSelect(Sender: TObject);
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

    ZVARIABLE_TAG:
      FDataModel.ZVariable := Field;

    WVARIABLE_TAG:
      FDataModel.WVariable := Field;

    BYVARIABLE1_TAG:
      FDataModel.ByVariable1 := Field;

    BYVARIABLE2_TAG:
      FDataModel.ByVariable2 := Field;
  end;

  UpdateCombos();
  DoModified();
end;

procedure TCtableStatDialogVariablesView.UpdateCombos();
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
      ComboBox.Fields := FDataModel.GetComboFields(TCtableStatDiaglogVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

constructor TCtableStatDialogVariablesView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, Ord(High(TCtableStatDiaglogVariable)) + 1);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := [ftInteger, ftFloat, ftString];
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'Outcome Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'First Exposure Variable';
  FComboBoxes[YVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := ZVARIABLE_TAG;
  ComboBox.NoItemText := 'Last Exposure Variable';
  FComboBoxes[ZVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := BYVARIABLE1_TAG;
  ComboBox.NoItemText := 'By Variable (optional)';
  FComboBoxes[BYVARIABLE1_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := BYVARIABLE2_TAG;
  ComboBox.NoItemText := 'By Variable (optional)';
  FComboBoxes[BYVARIABLE2_TAG] := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := WVARIABLE_TAG;
  ComboBox.NoItemText := 'Weight Variable (optional)';
  FComboBoxes[WVARIABLE_TAG] := ComboBox;
end;

procedure TCtableStatDialogVariablesView.EnterView();
begin
end;

function TCtableStatDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TCtableStatDialogVariablesView.GetViewCaption(): UTF8String;
begin
  result := 'Variables';
end;

procedure TCtableStatDialogVariablesView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.YVariable := nil;
  FDataModel.ZVariable := nil;
  FDataModel.WVariable := nil;
  FDataModel.ByVariable1 := nil;
  FDataModel.ByVariable2 := nil;

  UpdateCombos();
  DoModified();
end;

function TCtableStatDialogVariablesView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TCtableStatDialogVariablesView.SetModel(
  DataModel: TCtableStatDialogVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

end.
