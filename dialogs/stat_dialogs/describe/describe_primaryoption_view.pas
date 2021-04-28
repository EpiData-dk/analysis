unit describe_primaryoption_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, stat_dialog_contribution,
  describe_primaryoption_model, stat_dialog_custom_view;

type

  { TDescribeStatPrimaryOptionsView }

  TDescribeStatPrimaryOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TDescribePrimaryOptionModel;
    FValueLabelsGroup: TRadioGroup;
    FVariableLabelsGroup: TRadioGroup;
    FVerticalDivider: TBevel;
    procedure CreateValueLabelsRadios(RadioGroup: TRadioGroup);
    procedure CreateVariableLabelsRadios(RadioGroup: TRadioGroup);
    procedure ValueLabelSelectionChanged(Sender: TObject);
    procedure VariableLabelSelectionChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TDescribePrimaryOptionModel);
  end;

implementation

uses
  StdCtrls, epifields_helper;

{ TDescribeStatPrimaryOptionsView }

procedure TDescribeStatPrimaryOptionsView.CreateValueLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Value');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Value + Label');
  RadioGroup.Items.Add('Label + Value');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @ValueLabelSelectionChanged;
end;

procedure TDescribeStatPrimaryOptionsView.CreateVariableLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Variable Name');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Variable Name + Label');
  RadioGroup.Items.Add('Label + Variable Name');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @VariableLabelSelectionChanged;
end;

procedure TDescribeStatPrimaryOptionsView.ValueLabelSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.ValueLabelType := gvtValue;
    1: FDataModel.ValueLabelType := gvtLabel;
    2: FDataModel.ValueLabelType := gvtValueLabel;
    3: FDataModel.ValueLabelType := gvtLabelValue;
  end;
  DoModified();
end;

procedure TDescribeStatPrimaryOptionsView.VariableLabelSelectionChanged(
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

constructor TDescribeStatPrimaryOptionsView.Create(TheOwner: TComponent);
var
  CheckBox: TCheckBox;
  PrevControl: TControl;
begin
  inherited Create(TheOwner);

  FValueLabelsGroup := TRadioGroup.Create(self);
  FVariableLabelsGroup := TRadioGroup.Create(self);

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
  FValueLabelsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FVariableLabelsGroup.Parent := self;
  FVariableLabelsGroup.Caption := 'Variable Labels';
  FVariableLabelsGroup.Anchors := [];
  FVariableLabelsGroup.AnchorParallel(akTop, 5, Self);
  FVariableLabelsGroup.AnchorParallel(akRight, 5, self);
  FVariableLabelsGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  CreateValueLabelsRadios(FValueLabelsGroup);
  CreateVariableLabelsRadios(FVariableLabelsGroup);
end;

procedure TDescribeStatPrimaryOptionsView.EnterView();
begin
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function TDescribeStatPrimaryOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TDescribeStatPrimaryOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Label options';
end;

function TDescribeStatPrimaryOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TDescribeStatPrimaryOptionsView.ResetView();
var
  i: Integer;
begin
  FDataModel.ValueLabelType := gvtLabel;
  FDataModel.VariableLabelType := gvtVarLabel;

  FValueLabelsGroup.ItemIndex := 1;
  FVariableLabelsGroup.ItemIndex := 1;
end;

procedure TDescribeStatPrimaryOptionsView.SetModel(
  DataModel: TDescribePrimaryOptionModel);
begin
  FDataModel := DataModel;
end;

end.
