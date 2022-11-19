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
    FXVariableCombo: TEpiFieldsComboBox;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombo(Combo: TEpiFieldsComboBox; Variable: TParetoStatDiaglogVariable);
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
  epidatafilestypes, Controls;

{ TParetoMainDialogView }

procedure TParetoMainDialogView.VariableSelect(Sender: TObject);
var
  ComboBox: TEpiFieldsComboBox;
  Field: TEpiField;
begin
  ComboBox := TEpiFieldsComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);

  if ComboBox = FXVariableCombo then
    FDataModel.XVariable := Field;

  UpdateCombos();
  DoModified();
end;

procedure TParetoMainDialogView.UpdateCombo(Combo: TEpiFieldsComboBox;
  Variable: TParetoStatDiaglogVariable);
var
  Field: TEpiField;
begin
  Field := Combo.SelectedField;
  Combo.Fields.Free;
  Combo.Fields := nil;
  Combo.Fields := FDataModel.GetComboFields(Variable);
  Combo.ItemIndex := Combo.Items.IndexOfObject(Field);
end;

procedure TParetoMainDialogView.UpdateCombos();
begin
  UpdateCombo(FXVariableCombo, tvX);
end;

constructor TParetoMainDialogView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FXVariableCombo := TEpiFieldsComboBox.Create(TheOwner);
  FXVariableCombo.Parent := self;
  FXVariableCombo.AnchorParallel(akLeft, 10, Self);
  FXVariableCombo.AnchorParallel(akRight, 10, Self);
  FXVariableCombo.AnchorParallel(akTop, 10, Self);
  FXVariableCombo.OnSelect := @VariableSelect;
  FXVariableCombo.NoItemText := 'X Variable';
  FXVariableCombo.Filter := IntFieldTypes + FloatFieldTypes + DateFieldTypes;

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
begin
  FXVariableCombo.ItemIndex := 0;

  FDataModel.XVariable := nil;

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
