unit scatter_main_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_custom_view, fields_combobox, epidatafiles,
  scatter_variables_model;

type

  { TScatterMainDialogView }

  TScatterMainDialogView = class(TCustomStatDialogView)
  private
    FDataModel: TScatterStatVariableModel;
    FXVariableCombo: TEpiFieldsComboBox;
    FYVariableCombo: TEpiFieldsComboBox;
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

{ TScatterMainDialogView }

procedure TScatterMainDialogView.VariableSelect(Sender: TObject);
var
  ComboBox: TEpiFieldsComboBox;
  Field: TEpiField;
begin
  ComboBox := TEpiFieldsComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);

  if ComboBox = FXVariableCombo then
    FDataModel.XVariable := Field
  else
    FDataModel.YVariable := Field;

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
  FXVariableCombo.NoItemText := 'X Variable';
  FXVariableCombo.Filter := IntFieldTypes + FloatFieldTypes + DateFieldTypes;

  FYVariableCombo := TEpiFieldsComboBox.Create(TheOwner);
  FYVariableCombo.Parent := self;
  FYVariableCombo.AnchorParallel(akLeft, 10, Self);
  FYVariableCombo.AnchorParallel(akRight, 10, Self);
  FYVariableCombo.AnchorToNeighbour(akTop, 10, FXVariableCombo);
  FYVariableCombo.OnSelect := @VariableSelect;
  FYVariableCombo.NoItemText := 'Y Variable';
  FYVariableCombo.Filter := IntFieldTypes + FloatFieldTypes + DateFieldTypes;
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

  FDataModel.XVariable := nil;
  FDataModel.YVariable := nil;

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
