unit means_primaryoption_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, stat_dialog_contribution,
  Means_primaryoption_model, stat_dialog_custom_view;

type

  { TMeansStatPrimaryOptionsView }

  TMeansStatPrimaryOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDialogViewModified;
    FDataModel: TMeansStatDialogPrimaryOptionModel;
    FHorizontalDivider: TBevel;
    FVariableLabelsGroup: TRadioGroup;
    FValueLabelsGroup: TRadioGroup;
    FVerticalDivider: TBevel;
    procedure CreateVariableLabelsRadios(RadioGroup: TRadioGroup);
    procedure VariableLabelSelectionChanged(Sender: TObject);
    procedure CreateValueLabelsRadios(RadioGroup: TRadioGroup);
    procedure ValueLabelSelectionChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TMeansStatDialogPrimaryOptionModel);
  end;

implementation

uses
  StdCtrls, epifields_helper;

{ TMeansStatPrimaryOptionsView }

procedure TMeansStatPrimaryOptionsView.CreateVariableLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Variable Name');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Variable Name + Label');
  RadioGroup.Items.Add('Label + Variable Name');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @VariableLabelSelectionChanged;
end;

procedure TMeansStatPrimaryOptionsView.CreateValueLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Value');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Value + Label');
  RadioGroup.Items.Add('Label + Value');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @ValueLabelSelectionChanged;
end;

procedure TMeansStatPrimaryOptionsView.VariableLabelSelectionChanged(
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

procedure TMeansStatPrimaryOptionsView.ValueLabelSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.ValueLabelType := gvtValue;
    1: FDataModel.ValueLabelType := gvtLabel;
    2: FDataModel.ValueLabelType := gvtValueLabel;
    3: FDataModel.ValueLabelType := gvtLabelValue;
  end;
  DoModified();
end;

constructor TMeansStatPrimaryOptionsView.Create(TheOwner: TComponent);
var
  CheckBox: TCheckBox;
  PrevControl: TControl;
begin
  inherited Create(TheOwner);

  FVariableLabelsGroup := TRadioGroup.Create(self);
  FValueLabelsGroup := TRadioGroup.Create(self);

  FVerticalDivider := TBevel.Create(self);
  FVerticalDivider.Parent := self;
  FVerticalDivider.Style := bsLowered;
  FVerticalDivider.Shape := bsSpacer;
  FVerticalDivider.Width := 5;
  FVerticalDivider.Anchors := [];
  FVerticalDivider.AnchorParallel(akTop, 5, Self);
  FVerticalDivider.AnchorParallel(akBottom, 0, Self);

  FVariableLabelsGroup.Parent := self;
  FVariableLabelsGroup.Caption := 'Variable Labels';
  FVariableLabelsGroup.Anchors := [];
  FVariableLabelsGroup.AnchorParallel(akTop, 5, Self);
  FVariableLabelsGroup.AnchorParallel(akRight, 5, self);
  FVariableLabelsGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  FValueLabelsGroup.Parent := self;
  FValueLabelsGroup.Caption := 'Value Labels';
  FValueLabelsGroup.Anchors := [];
  FValueLabelsGroup.AnchorParallel(akTop, 5, Self);
  FValueLabelsGroup.AnchorParallel(akLeft, 5, self);
  FValueLabelsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  CreateVariableLabelsRadios(FVariableLabelsGroup);
  CreateValueLabelsRadios(FValueLabelsGroup);
end;

procedure TMeansStatPrimaryOptionsView.EnterView();
begin
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function TMeansStatPrimaryOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TMeansStatPrimaryOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Options';
end;

function TMeansStatPrimaryOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TMeansStatPrimaryOptionsView.ResetView();
var
  i: Integer;
begin
  FDataModel.VariableLabelType := gvtVarLabel;
  FVariableLabelsGroup.ItemIndex := 1;
  FDataModel.ValueLabelType := gvtLabel;
  FVariableLabelsGroup.ItemIndex := 1;
end;

procedure TMeansStatPrimaryOptionsView.SetModel(
  DataModel: TMeansStatDialogPrimaryOptionModel);
begin
  FDataModel := DataModel;
end;

end.

