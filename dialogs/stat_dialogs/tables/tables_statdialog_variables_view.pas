unit tables_statdialog_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls, ExtCtrls,
  StdCtrls, tables_statdialog_model, fields_combobox, stat_dialog_custom_view;

type

  { TTableStatDialogVariablesView }

  TTableStatDialogVariablesView = class(TCustomStatDialogView)
  private
    FDataModel: TTableStatDialogVariableModel;
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
    procedure SetModel(DataModel: TTableStatDialogVariableModel);
  end;

implementation

uses
  epidatafiles;

const
  XVARIABLE_TAG   = Ord(tvX);
  YVARIABLE_TAG   = Ord(tvY);
  BYVARIABLE1_TAG = Ord(tvBy1);
  BYVARIABLE2_TAG = Ord(tvBy2);

{ TTableStatDialogVariablesView }

procedure TTableStatDialogVariablesView.VariableSelect(Sender: TObject);
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

    BYVARIABLE1_TAG:
      FDataModel.ByVariable1 := Field;

    BYVARIABLE2_TAG:
      FDataModel.ByVariable2 := Field;
  end;

  UpdateCombos();
  DoModified();
end;

procedure TTableStatDialogVariablesView.UpdateCombos();
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
      ComboBox.Fields := FDataModel.GetComboFields(TTableStatDiaglogVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

constructor TTableStatDialogVariablesView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, Ord(High(TTableStatDiaglogVariable)) + 1);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'Column Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'Row Variable';
  FComboBoxes[YVARIABLE_TAG] := ComboBox;
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
end;

procedure TTableStatDialogVariablesView.EnterView();
begin
end;

function TTableStatDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TTableStatDialogVariablesView.GetViewCaption(): UTF8String;
begin
  result := 'Variables';
end;

procedure TTableStatDialogVariablesView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.YVariable := nil;
  FDataModel.ByVariable1 := nil;
  FDataModel.ByVariable2 := nil;

  UpdateCombos();
  DoModified();
end;

function TTableStatDialogVariablesView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TTableStatDialogVariablesView.SetModel(
  DataModel: TTableStatDialogVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

end.


