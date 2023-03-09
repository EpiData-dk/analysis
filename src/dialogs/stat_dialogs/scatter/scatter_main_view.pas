unit scatter_main_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_custom_view, fields_combobox, epidatafiles,
  StdCtrls, scatter_variables_model;

type

  { TScatterMainDialogView }

  TScatterMainDialogView = class(TCustomStatDialogView)
  private
    FDataModel: TScatterStatVariableModel;
    FXVariableCombo: TEpiFieldsComboBox;
    FYVariableCombo: TEpiFieldsComboBox;
    FByVariableCombo: TEpiFieldsComboBox;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombo(Combo: TEpiFieldsComboBox; Variable: TScatterStatDiaglogVariable);
    procedure UpdateCombos();
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: TScatterStatVariableModel);
  end;

implementation

uses
  epidatafilestypes, Controls;

const
  XVARIABLE_TAG  = Ord(tvX);
  YVARIABLE_TAG  = Ord(tvY);
  BYVARIABLE_TAG = Ord(tvBy);

  { TScatterMainDialogView }

procedure TScatterMainDialogView.VariableSelect(Sender: TObject);
var
  ComboBox: TCustomComboBox;
  Field: TEpiField;
begin
  ComboBox := TCustomComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);

  case ComboBox.Tag of
    XVARIABLE_TAG:
      FDataModel.XVariable := Field;
    YVARIABLE_TAG:
      FDataModel.YVariable := Field;
    BYVARIABLE_TAG:
      FDataModel.ByVariable := Field;
  end;

  UpdateCombos();
  DoModified();
end;

procedure TScatterMainDialogView.UpdateCombo(Combo: TEpiFieldsComboBox;
  Variable: TScatterStatDiaglogVariable);
var
  Field: TEpiField;
begin
  Field := Combo.SelectedField;
  Combo.Fields.Free;
  Combo.Fields := nil;
  Combo.Fields := FDataModel.GetComboFields(Variable);
  Combo.ItemIndex := Combo.Items.IndexOfObject(Field);
end;

procedure TScatterMainDialogView.UpdateCombos();
begin
  UpdateCombo(FXVariableCombo, tvX);
  UpdateCombo(FYVariableCombo, tvY);
  UpdateCombo(FByVariableCombo, tvBy);
end;

constructor TScatterMainDialogView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FXVariableCombo := TEpiFieldsComboBox.Create(TheOwner);
  FXVariableCombo.Parent := self;
  FXVariableCombo.AnchorParallel(akLeft, 10, Self);
  FXVariableCombo.AnchorParallel(akRight, 10, Self);
  FXVariableCombo.AnchorParallel(akTop, 10, Self);
  FXVariableCombo.OnSelect := @VariableSelect;
  FXVariableCombo.Tag := XVARIABLE_TAG;
  FXVariableCombo.NoItemText := 'X Variable';
  FXVariableCombo.Filter := IntFieldTypes + FloatFieldTypes + DateFieldTypes;

  FYVariableCombo := TEpiFieldsComboBox.Create(TheOwner);
  FYVariableCombo.Parent := self;
  FYVariableCombo.AnchorParallel(akLeft, 10, Self);
  FYVariableCombo.AnchorParallel(akRight, 10, Self);
  FYVariableCombo.AnchorToNeighbour(akTop, 10, FXVariableCombo);
  FYVariableCombo.OnSelect := @VariableSelect;
  FYVariableCombo.Tag := YVARIABLE_TAG;
  FYVariableCombo.NoItemText := 'Y Variable';
  FYVariableCombo.Filter := IntFieldTypes + FloatFieldTypes + DateFieldTypes;

  FByVariableCombo := TEpiFieldsComboBox.Create(TheOwner);
  FByVariableCombo.Parent := self;
  FByVariableCombo.AnchorParallel(akLeft, 10, Self);
  FByVariableCombo.AnchorParallel(akRight, 10, Self);
  FByVariableCombo.AnchorToNeighbour(akTop, 10, FYVariableCombo);
  FByVariableCombo.OnSelect := @VariableSelect;
  FByVariableCombo.Tag := ByVARIABLE_TAG;
  FByVariableCombo.NoItemText := 'By Variable';
  FByVariableCombo.Filter := IntFieldTypes + FloatFieldTypes + DateFieldTypes;
end;

procedure TScatterMainDialogView.EnterView();
begin

end;

function TScatterMainDialogView.ExitView(): boolean;
begin
  Result := true;
end;

function TScatterMainDialogView.GetViewCaption(): UTF8String;
begin
  Result := 'Variables';
end;

procedure TScatterMainDialogView.ResetView();
begin
  FXVariableCombo.ItemIndex := 0;
  FYVariableCombo.ItemIndex := 0;
  FByVariableCombo.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.YVariable := nil;
  FDataModel.ByVariable := nil;

  UpdateCombos();
  DoModified();
end;

function TScatterMainDialogView.IsDefined(): boolean;
begin
  Result := FDataModel.IsDefined();
end;

procedure TScatterMainDialogView.SetModel(DataModel: TScatterStatVariableModel);
begin
  FDataModel := DataModel;
  UpdateCombos();
end;

end.
