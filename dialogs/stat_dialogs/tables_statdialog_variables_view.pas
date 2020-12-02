unit tables_statdialog_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls, ExtCtrls,
  StdCtrls, tables_statdialog_model, fields_combobox, executor;

type

  { TTableStatDialogVariablesView }

  TTableStatDialogVariablesView = class(TPanel, IStatDialogView)
  private
    FDataModel: TTableStatDialogVariableModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    FOnModified: IStatDiaglogViewModified;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombos();
  protected
    procedure DoModified();
  public
    constructor Create(TheOwner: TComponent; Executor: TExecutor);
    procedure EnterView();
    function ExitView(): boolean;
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    procedure ResetView();
    procedure SetModel(DataModel: TTableStatDialogVariableModel);
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
  end;

implementation

uses
  epidatafilestypes, epidatafiles;

const
  XVARIABLE_TAG = 0;
  YVARIABLE_TAG = 1;
  WEIGHTVARIABLE_TAG = 2;

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

    WEIGHTVARIABLE_TAG:
      FDataModel.WeightVariable := Field;
  end;

  DoModified();
end;

procedure TTableStatDialogVariablesView.UpdateCombos();
var
  Field: TEpiField;
begin
  Field := TEpiField(FComboBoxes[0].Items[FComboBoxes[0].ItemIndex]);
  FComboBoxes[0].Fields.Free;
  FComboBoxes[0].Fields := FDataModel.GetComboFields(tvX);
  FComboBoxes[0].ItemIndex := FComboBoxes[0].Items.IndexOfObject(Field);
end;

procedure TTableStatDialogVariablesView.DoModified();
begin
  if (Assigned(FOnModified)) then
    FOnModified.OnViewModified(FDataModel);
end;

constructor TTableStatDialogVariablesView.Create(TheOwner: TComponent;
  Executor: TExecutor);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, 3);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'X Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'Y Variable (optional)';
  FComboBoxes[YVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := WEIGHTVARIABLE_TAG;
  ComboBox.NoItemText := 'Weight Variable (optional)';
  FComboBoxes[WEIGHTVARIABLE_TAG] := ComboBox;
end;

procedure TTableStatDialogVariablesView.EnterView();
begin
end;

function TTableStatDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TTableStatDialogVariablesView.GetControl(): TControl;
begin
  result := self;
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

  DoModified();
end;

procedure TTableStatDialogVariablesView.SetModel(
  DataModel: TTableStatDialogVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

procedure TTableStatDialogVariablesView.SetOnModified(
  OnModified: IStatDiaglogViewModified);
begin
  FOnModified := OnModified;
end;

end.


