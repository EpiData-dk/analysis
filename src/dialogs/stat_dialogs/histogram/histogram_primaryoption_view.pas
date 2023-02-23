unit histogram_primaryoption_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, stat_dialog_contribution,
  histogram_primaryoption_model, stat_dialog_custom_view;

type

  { THistogramPrimaryOptionsView }

  THistogramPrimaryOptionsView = class(TCustomStatDialogView)
  private
    FDataModel: THistogramDialogPrimaryOptionModel;
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
    procedure SetModel(DataModel: THistogramDialogPrimaryOptionModel);
  end;

implementation

uses
  StdCtrls, epifields_helper;

{ THistogramPrimaryOptionsView }

procedure THistogramPrimaryOptionsView.CreateValueLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Value');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Value + Label');
  RadioGroup.Items.Add('Label + Value');
  RadioGroup.OnSelectionChanged := @ValueLabelSelectionChanged;
end;

procedure THistogramPrimaryOptionsView.CreateVariableLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Variable Name');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Variable Name + Label');
  RadioGroup.Items.Add('Label + Variable Name');
  RadioGroup.OnSelectionChanged := @VariableLabelSelectionChanged;
end;

procedure THistogramPrimaryOptionsView.VariableLabelSelectionChanged(
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

procedure THistogramPrimaryOptionsView.ValueLabelSelectionChanged(
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

procedure THistogramPrimaryOptionsView.OptionGroupCheck(Sender: TObject; Index: integer);
var
  Value: Boolean;
begin
  Value := TCheckGroup(Sender).Checked[Index];
  case Index of
    0:  FDataModel.Stack := Value;
    1:  FDataModel.SortD := Value;
  end;
end;

constructor THistogramPrimaryOptionsView.Create(TheOwner: TComponent);
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

procedure THistogramPrimaryOptionsView.EnterView();
begin
  FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function THistogramPrimaryOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function THistogramPrimaryOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Output';
end;

function THistogramPrimaryOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure THistogramPrimaryOptionsView.ResetView();
begin
  FDataModel.ValueLabelType := gvtLabel;
  FDataModel.VariableLabelType := gvtVarLabel;

  FValueLabelsGroup.ItemIndex := FDataModel.ValueLabelsDefault;
  FVariableLabelsGroup.ItemIndex := FDataModel.VariableLabelsDefault;

  FDataModel.Stack := false;

end;

procedure THistogramPrimaryOptionsView.SetModel(
  DataModel: THistogramDialogPrimaryOptionModel);
begin
  FDataModel := DataModel;
  FValueLabelsGroup.ItemIndex := FDataModel.ValueLabelsDefault;
  FVariableLabelsGroup.ItemIndex := FDataModel.VariableLabelsDefault;
end;

end.
