unit describe_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls, ExtCtrls,
  StdCtrls, describe_model, fields_combobox, stat_dialog_custom_view;

type

  { TDescribeVariablesView }

  TDescribeVariablesView = class(TCustomStatDialogView)
  private
    FDataModel: TDescribeVariableModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    FOnModified: IStatDialogViewModified;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombos();
  public
    constructor Create(TheOwner: TComponent);
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: TDescribeVariableModel);
  end;

implementation

uses
  epidatafiles;

const
  XVARIABLE_TAG   = Ord(tvX);
  YVARIABLE_TAG   = Ord(tvY);

{ TDescribeVariablesView }

procedure TDescribeVariablesView.VariableSelect(Sender: TObject);
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

procedure TDescribeVariablesView.UpdateCombos();
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
      ComboBox.Fields := FDataModel.GetComboFields(TDescribeStatDialogVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

constructor TDescribeVariablesView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, Ord(High(TDescribeStatDialogVariable)) + 1);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'First Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'Last Variable';
  FComboBoxes[YVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  end;

procedure TDescribeVariablesView.EnterView();
begin
end;

function TDescribeVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TDescribeVariablesView.GetViewCaption(): UTF8String;
begin
  result := 'Select a range of variables';
end;

procedure TDescribeVariablesView.ResetView();
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

function TDescribeVariablesView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TDescribeVariablesView.SetModel(
  DataModel: TDescribeVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

end.
