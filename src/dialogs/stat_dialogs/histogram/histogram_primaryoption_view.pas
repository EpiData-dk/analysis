unit histogram_primaryoption_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, stat_dialog_contribution,
  histogram_primaryoption_model, stat_dialog_custom_view;

type

  { ThistogramStatPrimaryOptionsView }

  ThistogramStatPrimaryOptionsView = class(TCustomStatDialogView)
  private
    FDataModel: ThistogramStatDialogPrimaryOptionModel;
    FHorizontalDivider: TBevel;
    FValueLabelsGroup: TRadioGroup;
    FVariableLabelsGroup: TRadioGroup;
    FOptionGroup: TCheckGroup;
    FVerticalDivider: TBevel;
    procedure CreateValueLabelsRadios(RadioGroup: TRadioGroup);
    procedure CreateVariableLabelsRadios(RadioGroup: TRadioGroup);
    procedure ValueLabelSelectionChanged(Sender: TObject);
    procedure VariableLabelSelectionChanged(Sender: TObject);
    procedure OptionGroupCheck(Sender: TObject; Index: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: ThistogramStatDialogPrimaryOptionModel);
  end;

implementation

uses
  StdCtrls, epifields_helper;

{ ThistogramStatPrimaryOptionsView }

procedure ThistogramStatPrimaryOptionsView.CreateValueLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Value');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Value + Label');
  RadioGroup.Items.Add('Label + Value');
  RadioGroup.OnSelectionChanged := @ValueLabelSelectionChanged;
end;

procedure ThistogramStatPrimaryOptionsView.CreateVariableLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Variable Name');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Variable Name + Label');
  RadioGroup.Items.Add('Label + Variable Name');
  RadioGroup.OnSelectionChanged := @VariableLabelSelectionChanged;
end;

procedure ThistogramStatPrimaryOptionsView.VariableLabelSelectionChanged(
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

procedure ThistogramStatPrimaryOptionsView.ValueLabelSelectionChanged(
  Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.ValueLabelType := gvtValue;
    1: FDataModel.ValueLabelType := gvtLabel;
    2: FDataModel.ValueLabelType := gvtValueLabel;
    3: FDataModel.ValueLabelType := gvtLabelValue;
  end;
  DoModified();
end;

procedure ThistogramStatPrimaryOptionsView.OptionGroupCheck(Sender: TObject; Index: integer);
var
  Value: Boolean;
begin
  Value := TCheckGroup(Sender).Checked[Index];
  case Index of
    0:  FDataModel.Stack := Value;
    1:  FDataModel.SortD := Value;
  end;
end;

constructor ThistogramStatPrimaryOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FValueLabelsGroup := TRadioGroup.Create(self);
  FVariableLabelsGroup := TRadioGroup.Create(self);
  FOptionGroup := TCheckGroup.Create(self);

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

  FOptionGroup.Parent := self;
  FOptionGroup.Caption := 'Output options';
  FOptionGroup.Anchors := [];
  FOptionGroup.AnchorParallel(akLeft, 5, self);
  FOptionGroup.AnchorParallel(akBottom, 5, Self);
  FOptionGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FOptionGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);
  FOptionGroup.Items.Add('Stack bars');
  FOptionGroup.Items.Add('Strata in descending order');
  FOptionGroup.OnItemClick:= @OptionGroupCheck;

  CreateValueLabelsRadios(FValueLabelsGroup);
  CreateVariableLabelsRadios(FVariableLabelsGroup);
end;

procedure ThistogramStatPrimaryOptionsView.EnterView();
begin
  FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function ThistogramStatPrimaryOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function ThistogramStatPrimaryOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Output';
end;

function ThistogramStatPrimaryOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure ThistogramStatPrimaryOptionsView.ResetView();
begin
  FDataModel.ValueLabelType := gvtLabel;
  FDataModel.VariableLabelType := gvtVarLabel;

  FValueLabelsGroup.ItemIndex := FDataModel.ValueLabelsDefault;
  FVariableLabelsGroup.ItemIndex := FDataModel.VariableLabelsDefault;

  FDataModel.Stack := false;

end;

procedure ThistogramStatPrimaryOptionsView.SetModel(
  DataModel: ThistogramStatDialogPrimaryOptionModel);
begin
  FDataModel := DataModel;
  FValueLabelsGroup.ItemIndex := FDataModel.ValueLabelsDefault;
  FVariableLabelsGroup.ItemIndex := FDataModel.VariableLabelsDefault;
end;

end.
