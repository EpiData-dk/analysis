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
    FOnModified: IStatDiaglogViewModified;
    procedure VariableSelect(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent; Executor: TExecutor);
    procedure EnterView();
    function ExitView(): boolean;
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    procedure SetModel(DataModel: TTableStatDialogVariableModel);
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
  end;

implementation

uses
  epidatafilestypes, epidatafiles;

const
  XVARIABLE_TAG = 1;
  YVARIABLE_TAG = 2;
  WEIGHTVARIABLE_TAG = 3;

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
      FDataModel.WeightVariable
      := Field;
  end;

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

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Filter := AllFieldTypes;
  ComboBox.Fields := Executor.SortedFields;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'X Variable';
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Filter := AllFieldTypes;
  ComboBox.Fields := Executor.SortedFields;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'Y Variable (optional)';
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Filter := AllFieldTypes;
  ComboBox.Fields := Executor.SortedFields;
  ComboBox.Tag := WEIGHTVARIABLE_TAG;
  ComboBox.NoItemText := 'Weight Variable (optional)';
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

procedure TTableStatDialogVariablesView.SetModel(
  DataModel: TTableStatDialogVariableModel);
begin
  FDataModel := DataModel;
end;

procedure TTableStatDialogVariablesView.SetOnModified(
  OnModified: IStatDiaglogViewModified);
begin
  FOnModified := OnModified;
end;

end.


