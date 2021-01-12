unit tables_statdialog_primaryoption_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, stat_dialog_contribution,
  tables_statdialog_primaryoption_model;

type

  { TTableStatPrimaryOptionsView }

  TTableStatPrimaryOptionsView = class(TPanel, IStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TTableStatDialogPrimaryOptionModel;
    FHorizontalDivider: TBevel;
    FVerticalDivider: TBevel;
    procedure CreateValueLabelsRadios(RadioGroup: TRadioGroup);
    procedure CreateVariableLabelsRadios(RadioGroup: TRadioGroup);
    procedure CreatePercentCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateSortingRadios(RadioGroup: TRadioGroup);
    procedure PercentageItemChecked(Sender: TObject; Index: integer);
    procedure SortingSelectionChanged(Sender: TObject);
    procedure ValueLabelSelectionChanged(Sender: TObject);
    procedure VariableLabelSelectionChanged(Sender: TObject);
  protected
    procedure DoModified();
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView();
    function ExitView(): boolean;
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    function IsDefined(): boolean;
    procedure ResetView();
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
    procedure SetModel(DataModel: TTableStatDialogPrimaryOptionModel);
  end;

implementation

uses
  StdCtrls, epifields_helper;

{ TTableStatPrimaryOptionsView }

procedure TTableStatPrimaryOptionsView.CreateValueLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Value');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Value + Label');
  RadioGroup.Items.Add('Label + Value');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @ValueLabelSelectionChanged;
end;

procedure TTableStatPrimaryOptionsView.CreateVariableLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Variable Name');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Variable Name + Label');
  RadioGroup.Items.Add('Label + Variable Name');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @VariableLabelSelectionChanged;
end;

procedure TTableStatPrimaryOptionsView.CreatePercentCheckboxes(
  CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Row');
  CheckGroup.Items.Add('Column');
  CheckGroup.Items.Add('Total');
  CheckGroup.OnItemClick := @PercentageItemChecked;
end;

procedure TTableStatPrimaryOptionsView.CreateSortingRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Ascending');
  RadioGroup.Items.Add('Descending');
  RadioGroup.Items.Add('Totals Ascending');
  RadioGroup.Items.Add('Totals Descending');
  RadioGroup.ItemIndex := 0;
  RadioGroup.OnSelectionChanged := @SortingSelectionChanged;
end;

procedure TTableStatPrimaryOptionsView.PercentageItemChecked(Sender: TObject;
  Index: integer);
var
  NewState: TPercentages;
begin
  case Index of
    0: NewState := [pRow];
    1: NewState := [pCol];
    2: NewState := [pTotal];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.Percentages := FDataModel.Percentages + NewState
  else
    FDataModel.Percentages := FDataModel.Percentages - NewState;

  DoModified();
end;

procedure TTableStatPrimaryOptionsView.SortingSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.Sorting := sortAsc;
    1: FDataModel.Sorting := sortDesc;
    2: FDataModel.Sorting := sortAscTotal;
    3: FDataModel.Sorting := sortDescTotal;
  end;
  DoModified();
end;

procedure TTableStatPrimaryOptionsView.ValueLabelSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.ValueLabelType := gvtValue;
    1: FDataModel.ValueLabelType := gvtLabel;
    2: FDataModel.ValueLabelType := gvtValueLabel;
    3: FDataModel.ValueLabelType := gvtLabelValue;
  end;
  DoModified();
end;

procedure TTableStatPrimaryOptionsView.VariableLabelSelectionChanged(
  Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.VariableLabelType := gvtVarName;
    1: FDataModel.VariableLabelType := gvtVarLabel;
    2: FDataModel.VariableLabelType := gvtVarNameLabel;
    3: FDataModel.VariableLabelType := gvtVarLabelName;
  end;
  DoModified();
end;

procedure TTableStatPrimaryOptionsView.DoModified();
begin
  if (Assigned(FOnModified)) then
    FOnModified.OnViewModified(Self);
end;

constructor TTableStatPrimaryOptionsView.Create(TheOwner: TComponent);
var
  CheckBox: TCheckBox;
  PrevControl: TControl;
  PercentGroup: TCheckGroup;
  SortingGroup, VariableLabelsGroup, ValueLabelsGroup: TRadioGroup;
begin
  inherited Create(TheOwner);

  ValueLabelsGroup := TRadioGroup.Create(self);
  VariableLabelsGroup := TRadioGroup.Create(self);
  PercentGroup := TCheckGroup.Create(self);
  SortingGroup := TRadioGroup.Create(self);

  FHorizontalDivider := TBevel.Create(self);
  FHorizontalDivider.Parent := self;
  FHorizontalDivider.Style := bsLowered;
  FHorizontalDivider.Shape := bsSpacer;
  FHorizontalDivider.Height := 5;
  FHorizontalDivider.Anchors := [];
  FHorizontalDivider.AnchorParallel(akLeft, 0, Self);
  FHorizontalDivider.AnchorParallel(akRight, 0, Self);

  FVerticalDivider := TBevel.Create(self);
  FVerticalDivider.Parent := self;
  FVerticalDivider.Style := bsLowered;
  FVerticalDivider.Shape := bsSpacer;
  FVerticalDivider.Width := 5;
  FVerticalDivider.Anchors := [];
  FVerticalDivider.AnchorParallel(akTop, 5, Self);
  FVerticalDivider.AnchorParallel(akBottom, 0, Self);

  ValueLabelsGroup.Parent := self;
  ValueLabelsGroup.Caption := 'Value Labels';
  ValueLabelsGroup.Anchors := [];
  ValueLabelsGroup.AnchorParallel(akTop, 5, Self);
  ValueLabelsGroup.AnchorParallel(akLeft, 5, self);
  ValueLabelsGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  ValueLabelsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  VariableLabelsGroup.Parent := self;
  VariableLabelsGroup.Caption := 'Variable Labels';
  VariableLabelsGroup.Anchors := [];
  VariableLabelsGroup.AnchorParallel(akTop, 5, Self);
  VariableLabelsGroup.AnchorParallel(akRight, 5, self);
  VariableLabelsGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  VariableLabelsGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  PercentGroup.Parent := self;
  PercentGroup.Caption := 'Percents';
  PercentGroup.Anchors := [];
  PercentGroup.AnchorParallel(akLeft, 5, self);
  PercentGroup.AnchorParallel(akBottom, 5, Self);
  PercentGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  PercentGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  SortingGroup.Parent := self;
  SortingGroup.Caption := 'Sorting';
  SortingGroup.Anchors := [];
  SortingGroup.AnchorParallel(akRight, 5, self);
  SortingGroup.AnchorParallel(akBottom, 5, Self);
  SortingGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  SortingGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  CreateValueLabelsRadios(ValueLabelsGroup);
  CreateVariableLabelsRadios(VariableLabelsGroup);
  CreatePercentCheckboxes(PercentGroup);
  CreateSortingRadios(SortingGroup);
end;

procedure TTableStatPrimaryOptionsView.EnterView();
begin
  FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function TTableStatPrimaryOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TTableStatPrimaryOptionsView.GetControl(): TControl;
begin
  result := self;
end;

function TTableStatPrimaryOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Options';
end;

function TTableStatPrimaryOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TTableStatPrimaryOptionsView.ResetView();
begin
  FDataModel.Percentages := [];
  FDataModel.Sorting := sortAsc;
  FDataModel.ValueLabelType := gvtLabel;
  FDataModel.VariableLabelType := gvtVarLabel;


end;

procedure TTableStatPrimaryOptionsView.SetOnModified(
  OnModified: IStatDiaglogViewModified);
begin
  FOnModified := OnModified;
end;

procedure TTableStatPrimaryOptionsView.SetModel(
  DataModel: TTableStatDialogPrimaryOptionModel);
begin
  FDataModel := DataModel;
end;

end.

