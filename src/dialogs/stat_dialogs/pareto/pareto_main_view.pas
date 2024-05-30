unit pareto_main_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_custom_view, fields_combobox, epidatafiles,
  pareto_variables_model;

type

  { TParetoMainDialogView }

  TParetoMainDialogView = class(TCustomStatDialogView)
  private
    FDataModel: TParetoStatVariableModel;
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
    procedure SetModel(DataModel: TParetoStatVariableModel);
  end;

implementation

uses
  epidatafilestypes, Controls, ExtCtrls, StdCtrls;

const
  XVARIABLE_TAG  = Ord(tvX);
  BYVARIABLE_TAG = Ord(tvBy);

  { TParetoMainDialogView }

procedure TParetoMainDialogView.VariableSelect(Sender: TObject);
var
  ComboBox: TEpiFieldsComboBox;
  Field: TEpiField;
begin
  ComboBox := TEpiFieldsComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);

  case ComboBox.Tag of
    XVARIABLE_TAG:
      FDataModel.XVariable := Field;

    BYVARIABLE_TAG:
      FDataModel.ByVariable := Field;
  end;

  UpdateCombos();
  DoModified();
end;

procedure TParetoMainDialogView.UpdateCombos();
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
      ComboBox.Fields := FDataModel.GetComboFields(TParetoStatDiaglogVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

constructor TParetoMainDialogView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, Ord(High(TParetoStatDiaglogVariable)) + 1);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := DateFieldTypes + [ftInteger];
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := BYVARIABLE_TAG;
  ComboBox.NoItemText := 'Stratifying Variable';
  FComboBoxes[BYVARIABLE_TAG] := ComboBox;

  EnterView();
  end;

procedure TParetoMainDialogView.EnterView();
begin

end;

function TParetoMainDialogView.ExitView(): boolean;
begin
  Result := true;
end;

function TParetoMainDialogView.GetViewCaption(): UTF8String;
begin
  Result := 'Variables';
end;

procedure TParetoMainDialogView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.BYVariable := nil;

  UpdateCombos();
  DoModified();
end;

function TParetoMainDialogView.IsDefined(): boolean;
begin
  Result := FDataModel.IsDefined();
end;

procedure TParetoMainDialogView.SetModel(DataModel: TParetoStatVariableModel);
begin
  FDataModel := DataModel;
  UpdateCombos();
end;

end.
