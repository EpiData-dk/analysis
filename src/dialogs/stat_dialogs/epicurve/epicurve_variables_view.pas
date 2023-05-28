unit epicurve_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls,
  stat_dialog_contribution, epicurve_model, fields_combobox, stat_dialog_custom_view;

type

  { TEpicurveDialogVariablesView }

  TEpicurveDialogVariablesView = class(TCustomStatDialogView)
  private
    FDataModel: TEpicurveDialogVariableModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombos();
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: TEpicurveDialogVariableModel);
  end;

implementation

uses
  epidatafiles, epidatafilestypes;

const
  XVARIABLE_TAG   = Ord(tvX);
  YVARIABLE_TAG   = Ord(tvY);

{ TEpicurveDialogVariablesView }

procedure TEpicurveDialogVariablesView.VariableSelect(Sender: TObject);
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

  end;

  UpdateCombos();
  DoModified();
end;

procedure TEpicurveDialogVariablesView.UpdateCombos();
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
      ComboBox.Fields := FDataModel.GetComboFields(TEpicurveDialogVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

constructor TEpicurveDialogVariablesView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, Ord(High(TEpicurveDialogVariable)) + 1);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := DateFieldTypes + [ftInteger];
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'Time Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'Stratifying Variable';
  FComboBoxes[YVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  EnterView(); // Must do this to get combo boxes aligned and visible
end;

procedure TEpicurveDialogVariablesView.EnterView();
begin

end;

function TEpicurveDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TEpicurveDialogVariablesView.GetViewCaption(): UTF8String;
begin
  result := 'Variables';
end;

procedure TEpicurveDialogVariablesView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.YVariable := nil;

  UpdateCombos();
  DoModified();
end;

function TEpicurveDialogVariablesView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TEpicurveDialogVariablesView.SetModel(
  DataModel: TEpicurveDialogVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

end.
