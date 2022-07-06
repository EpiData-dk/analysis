unit survival_primaryoption_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, stat_dialog_contribution,
  survival_primaryoption_model, stat_dialog_custom_view;

type

  { TSurvivalStatPrimaryOptionsView }

  TSurvivalStatPrimaryOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TSurvivalStatDialogPrimaryOptionModel;
    FHorizontalDivider: TBevel;
    FDecimalGroup: TRadioGroup;
    FValueLabelsGroup: TRadioGroup;
    FVariableLabelsGroup: TRadioGroup;
    FOutputGroup: TCheckGroup;
    FVerticalDivider: TBevel;
    procedure CreateValueLabelsRadios(RadioGroup: TRadioGroup);
    procedure CreateVariableLabelsRadios(RadioGroup: TRadioGroup);
    procedure CreateDecimalRadios(RadioGroup: TRadioGroup);
    procedure ValueLabelSelectionChanged(Sender: TObject);
    procedure VariableLabelSelectionChanged(Sender: TObject);
    procedure DecimalSelectionChanged(Sender: TObject);
    procedure OutputGroupCheck(Sender: TObject; Index: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TSurvivalStatDialogPrimaryOptionModel);
  end;

implementation

uses
  StdCtrls, epifields_helper;

{ TSurvivalStatPrimaryOptionsView }

procedure TSurvivalStatPrimaryOptionsView.CreateValueLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Value');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Value + Label');
  RadioGroup.Items.Add('Label + Value');
  RadioGroup.OnSelectionChanged := @ValueLabelSelectionChanged;
end;

procedure TSurvivalStatPrimaryOptionsView.CreateVariableLabelsRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Variable Name');
  RadioGroup.Items.Add('Label');
  RadioGroup.Items.Add('Variable Name + Label');
  RadioGroup.Items.Add('Label + Variable Name');
  RadioGroup.OnSelectionChanged := @VariableLabelSelectionChanged;
end;

procedure TSurvivalStatPrimaryOptionsView.CreateDecimalRadios(
  RadioGroup: TRadioGroup);
var
  i: integer;
begin
  for i := 0 to 5 do
    RadioGroup.Items.Add(IntToStr(i));
  RadioGroup.ItemIndex := 3;
  RadioGroup.OnSelectionChanged := @DecimalSelectionChanged;
end;

procedure TSurvivalStatPrimaryOptionsView.VariableLabelSelectionChanged(
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

procedure TSurvivalStatPrimaryOptionsView.ValueLabelSelectionChanged(
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

procedure TSurvivalStatPrimaryOptionsView.DecimalSelectionChanged(
  Sender: TObject);
begin
  FDataModel.Decimals := IntToStr(TRadioGroup(Sender).ItemIndex);
  DoModified();
end;

procedure TSurvivalStatPrimaryOptionsView.OutputGroupCheck(Sender: TObject; Index: integer);
var
  Value: Boolean;
begin
  Value := TCheckGroup(Sender).Checked[Index];
  case Index of
    0:  FDataModel.OutputTable := not Value;
    1:  FDataModel.OutputSummary := not Value;
    2:  FDataModel.OutputClipboard := Value;
    3:  FDataModel.OutputCIBand := Value;
  end;
end;

constructor TSurvivalStatPrimaryOptionsView.Create(TheOwner: TComponent);
var
  CheckBox: TCheckBox;
  PrevControl: TControl;
begin
  inherited Create(TheOwner);

  FValueLabelsGroup := TRadioGroup.Create(self);
  FVariableLabelsGroup := TRadioGroup.Create(self);
  FDecimalGroup := TRadioGroup.Create(self);
  FOutputGroup := TCheckGroup.Create(self);

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

  FDecimalGroup.Parent := self;
  FDecimalGroup.Caption := 'Decimals to show';
  FDecimalGroup.Anchors := [];
  FDecimalGroup.AnchorParallel(akRight, 5, self);
  FDecimalGroup.AnchorParallel(akBottom, 5, Self);
  FDecimalGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FDecimalGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  FOutputGroup.Parent := self;
  FOutputGroup.Caption := 'Output options';
  FOutputGroup.Anchors := [];
  FOutputGroup.AnchorParallel(akLeft, 5, self);
  FOutputGroup.AnchorParallel(akBottom, 5, Self);
  FOutputGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FOutputGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);
  FOutputGroup.Items.Add('No survival tables');
  FOutputGroup.Items.Add('No summary table');
  FOutputGroup.Items.Add('Save plot to clipboard');
  FOutputGroup.Items.Add('Show CI as band');
  FOutputGroup.OnItemClick:= @OutputGroupCheck;

  CreateValueLabelsRadios(FValueLabelsGroup);
  CreateVariableLabelsRadios(FVariableLabelsGroup);
  CreateDecimalRadios(FDecimalGroup);
end;

procedure TSurvivalStatPrimaryOptionsView.EnterView();
begin
  FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
end;

function TSurvivalStatPrimaryOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TSurvivalStatPrimaryOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Output';
end;

function TSurvivalStatPrimaryOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TSurvivalStatPrimaryOptionsView.ResetView();
var
  i: Integer;
begin
  FDataModel.ValueLabelType := gvtLabel;
  FDataModel.VariableLabelType := gvtVarLabel;
  FDataModel.Decimals := '3';

  FValueLabelsGroup.ItemIndex := FDataModel.ValueLabelsDefault;
  FVariableLabelsGroup.ItemIndex := FDataModel.VariableLabelsDefault;
  FDecimalGroup.ItemIndex := 3;

  FDataModel.OutputTable := true;
  FDataModel.OutputSummary := true;
  FDataModel.OutputClipboard := false;
  FDataModel.OutputCIBand := false;;

end;

procedure TSurvivalStatPrimaryOptionsView.SetModel(
  DataModel: TSurvivalStatDialogPrimaryOptionModel);
begin
  FDataModel := DataModel;
  FValueLabelsGroup.ItemIndex := FDataModel.ValueLabelsDefault;
  FVariableLabelsGroup.ItemIndex := FDataModel.VariableLabelsDefault;
end;

end.
