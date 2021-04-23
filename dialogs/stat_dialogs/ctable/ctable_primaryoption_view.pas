unit ctable_primaryoption_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, stat_dialog_contribution,
  ctable_primaryoption_model, stat_dialog_custom_view;

type

  { TCtableStatPrimaryOptionsView }

  TCtableStatPrimaryOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TCtableStatDialogPrimaryOptionModel;
    FHorizontalDivider: TBevel;
    FPercentGroup: TCheckGroup;
    FSortingGroup: TRadioGroup;
    FValueLabelsGroup: TRadioGroup;
    FVariableLabelsGroup: TRadioGroup;
    FVerticalDivider: TBevel;
    procedure CreateValueLabelsRadios(RadioGroup: TRadioGroup);
    procedure CreateVariableLabelsRadios(RadioGroup: TRadioGroup);
    procedure CreatePercentCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateSortingRadios(RadioGroup: TRadioGroup);
    procedure PercentageItemChecked(Sender: TObject; Index: integer);
    procedure SortingSelectionChanged(Sender: TObject);
    procedure ValueLabelSelectionChanged(Sender: TObject);
    procedure VariableLabelSelectionChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TCtableStatDialogPrimaryOptionModel);
  end;

implementation

uses
  StdCtrls, epifields_helper;

{ TCtableStatPrimaryOptionsView }

procedure TCtableStatPrimaryOptionsView.CreateValueLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Value');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Value + Label');
  RadioGroup.Items.Add('Label + Value');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @ValueLabelSelectionChanged;
end;

procedure TCtableStatPrimaryOptionsView.CreateVariableLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Variable Name');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Variable Name + Label');
  RadioGroup.Items.Add('Label + Variable Name');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @VariableLabelSelectionChanged;
end;

procedure TCtableStatPrimaryOptionsView.CreatePercentCheckboxes(
  CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Row');
  CheckGroup.Items.Add('Column');
  CheckGroup.Items.Add('Total');
  CheckGroup.OnItemClick := @PercentageItemChecked;
end;

procedure TCtableStatPrimaryOptionsView.CreateSortingRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Ascending');
  RadioGroup.Items.Add('Descending');
  RadioGroup.Items.Add('Totals Ascending');
  RadioGroup.Items.Add('Totals Descending');
  RadioGroup.ItemIndex := 0;
  RadioGroup.OnSelectionChanged := @SortingSelectionChanged;
end;

procedure TCtableStatPrimaryOptionsView.PercentageItemChecked(Sender: TObject;
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

procedure TCtableStatPrimaryOptionsView.SortingSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.Sorting := sortAsc;
    1: FDataModel.Sorting := sortDesc;
    2: FDataModel.Sorting := sortAscTotal;
    3: FDataModel.Sorting := sortDescTotal;
  end;
  DoModified();
end;

procedure TCtableStatPrimaryOptionsView.ValueLabelSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.ValueLabelType := gvtValue;
    1: FDataModel.ValueLabelType := gvtLabel;
    2: FDataModel.ValueLabelType := gvtValueLabel;
    3: FDataModel.ValueLabelType := gvtLabelValue;
  end;
  DoModified();
end;

procedure TCtableStatPrimaryOptionsView.VariableLabelSelectionChanged(
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

constructor TCtableStatPrimaryOptionsView.Create(TheOwner: TComponent);
var
  CheckBox: TCheckBox;
  PrevControl: TControl;
begin
  inherited Create(TheOwner);

  FValueLabelsGroup := TRadioGroup.Create(self);
  FVariableLabelsGroup := TRadioGroup.Create(self);
  FPercentGroup := TCheckGroup.Create(self);
  FSortingGroup := TRadioGroup.Create(self);

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

  FValueLabelsGroup.Parent := self;
  FValueLabelsGroup.Caption := 'Value Labels';
  FValueLabelsGroup.Anchors := [];
  FValueLabelsGroup.AnchorParallel(akTop, 5, Self);
  FValueLabelsGroup.AnchorParallel(akLeft, 5, self);
  FValueLabelsGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  FValueLabelsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FVariableLabelsGroup.Parent := self;
  FVariableLabelsGroup.Caption := 'Variable Labels';
  FVariableLabelsGroup.Anchors := [];
  FVariableLabelsGroup.AnchorParallel(akTop, 5, Self);
  FVariableLabelsGroup.AnchorParallel(akRight, 5, self);
  FVariableLabelsGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  FVariableLabelsGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  FPercentGroup.Parent := self;
  FPercentGroup.Caption := 'Percents';
  FPercentGroup.Anchors := [];
  FPercentGroup.AnchorParallel(akLeft, 5, self);
  FPercentGroup.AnchorParallel(akBottom, 5, Self);
  FPercentGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FPercentGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FSortingGroup.Parent := self;
  FSortingGroup.Caption := 'Sorting';
  FSortingGroup.Anchors := [];
  FSortingGroup.AnchorParallel(akRight, 5, self);
  FSortingGroup.AnchorParallel(akBottom, 5, Self);
  FSortingGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FSortingGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  CreateValueLabelsRadios(FValueLabelsGroup);
  CreateVariableLabelsRadios(FVariableLabelsGroup);
  CreatePercentCheckboxes(FPercentGroup);
  CreateSortingRadios(FSortingGroup);
end;

procedure TCtableStatPrimaryOptionsView.EnterView();
begin
  FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function TCtableStatPrimaryOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TCtableStatPrimaryOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Options';
end;

function TCtableStatPrimaryOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TCtableStatPrimaryOptionsView.ResetView();
var
  i: Integer;
begin
  FDataModel.Percentages := [];
  FDataModel.Sorting := sortAsc;
  FDataModel.ValueLabelType := gvtLabel;
  FDataModel.VariableLabelType := gvtVarLabel;

  for i := 0 to FPercentGroup.Items.Count - 1 do
    FPercentGroup.Checked[i] := false;

  FSortingGroup.ItemIndex := 0;
  FValueLabelsGroup.ItemIndex := 1;
  FVariableLabelsGroup.ItemIndex := 1;
end;

procedure TCtableStatPrimaryOptionsView.SetModel(
  DataModel: TCtableStatDialogPrimaryOptionModel);
begin
  FDataModel := DataModel;
end;

end.

