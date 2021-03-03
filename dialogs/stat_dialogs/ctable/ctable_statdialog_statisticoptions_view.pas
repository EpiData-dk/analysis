unit ctable_statdialog_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, ctable_statdialog_statisticoptions_model,
  stat_dialog_custom_view;

type

  { TCtableStatDialogStatisticOptionsView }

  TCtableStatDialogStatisticOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TCtableStatDialogStatisticOptionsModel;
    FHorizontalDivider: TBevel;
    FVerticalDivider: TBevel;
    FStatisticsGroup: TCheckGroup;
    FAttackRateGroup: TCheckGroup;
    FIncludeGroup:    TCheckGroup;
    FSortResultsGroup: TRadioGroup;

    procedure CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateAttackRateCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateIncludeCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateSortResultsRadio(RadioGroup: TRadioGroup);
    procedure StatisticsItemChecked(Sender: TObject; Index: integer);
    procedure AttackRateItemChecked(Sender: TObject; Index: integer);
    procedure IncludeItemChecked(Sender: TObject; Index: integer);
    procedure SortResultsSelectionChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TCtableStatDialogStatisticOptionsModel);
  end;

implementation

{ TCtableStatDialogStatisticOptionsView }

constructor TCtableStatDialogStatisticOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

//  Caption := 'Statistics Options';
  FStatisticsGroup  := TCheckGroup.Create(self);
  FAttackRateGroup  := TCheckGroup.Create(self);
  FIncludeGroup     := TCheckGroup.Create(self);
  FSortResultsGroup := TRadioGroup.Create(self);

  FVerticalDivider := TBevel.Create(self);
  FVerticalDivider.Parent := self;
  FVerticalDivider.Style := bsLowered;
  FVerticalDivider.Shape := bsSpacer;
  FVerticalDivider.Width := 5;
  FVerticalDivider.Anchors := [];
  FVerticalDivider.AnchorParallel(akTop, 5, Self);
  FVerticalDivider.AnchorParallel(akBottom, 0, Self);

  FHorizontalDivider := TBevel.Create(self);
  FHorizontalDivider.Parent := self;
  FHorizontalDivider.Style := bsLowered;
  FHorizontalDivider.Shape := bsSpacer;
  FHorizontalDivider.Height := 5;
  FHorizontalDivider.Anchors := [];
  FHorizontalDivider.AnchorParallel(akLeft, 0, Self);
  FHorizontalDivider.AnchorParallel(akRight, 0, Self);

  FStatisticsGroup.Parent := self;
  FStatisticsGroup.Caption := 'Statistics';
  FStatisticsGroup.Anchors := [];
  FStatisticsGroup.AnchorParallel(akTop, 5, Self);
  FStatisticsGroup.AnchorParallel(akLeft, 5, self);
  FStatisticsGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  FStatisticsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FSortResultsGroup.Parent := self;
  FSortResultsGroup.Caption := 'Sort Results';
  FSortResultsGroup.Anchors := [];
  FSortResultsGroup.AnchorParallel(akTop, 5, Self);
  FSortResultsGroup.AnchorParallel(akRight, 5, self);
  FSortResultsGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  FSortResultsGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  FAttackRateGroup.Parent := self;
  FAttackRateGroup.Caption := 'Attack Rate Tables';
  FAttackRateGroup.Anchors := [];
  FAttackRateGroup.AnchorParallel(akLeft, 5, Self);
  FAttackRateGroup.AnchorParallel(akBottom, 5, self);
  FAttackRateGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FAttackRateGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FIncludeGroup.Parent := self;
  FIncludeGroup.Caption := 'Include non 2x2 tables';
  FIncludeGroup.Anchors := [];
  FIncludeGroup.AnchorParallel(akRight, 5, Self);
  FIncludeGroup.AnchorParallel(akBottom, 5, self);
  FIncludeGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FIncludeGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  CreateStatisticsCheckBoxes(FStatisticsGroup);
  CreateAttackRateCheckBoxes(FAttackRateGroup);
  CreateIncludeCheckBoxes(FIncludeGroup);
  CreateSortResultsRadio(FSortResultsGroup);
end;

procedure TCtableStatDialogStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Chi-square Test');
  CheckGroup.Items.Add('Fisher Exact Test');
  CheckGroup.Items.Add('Odds Ratio');
  CheckGroup.Items.Add('Risk Ratio');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
end;

procedure TCtableStatDialogStatisticOptionsView.CreateAttackRateCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Attack rate table plus Risk Ratios');
  CheckGroup.Items.Add('Attack rate table only');
  CheckGroup.OnItemClick := @AttackRateItemChecked;
end;

procedure TCtableStatDialogStatisticOptionsView.CreateIncludeCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Include tables that are not 2x2');
  CheckGroup.OnItemClick := @IncludeItemChecked;
end;

procedure TCtableStatDialogStatisticOptionsView.CreateSortResultsRadio(RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('by Variable name');
  RadioGroup.Items.Add('by Variable label');
  RadioGroup.Items.Add('by key statistic');
  RadioGroup.OnSelectionChanged := @SortResultsSelectionChanged;
end;

procedure TCtableStatDialogStatisticOptionsView.StatisticsItemChecked(Sender: TObject; Index: integer);
var
  NewState: TStatisticTypes;
begin
  case Index of
    0: NewState := [pChi];
    1: NewState := [pFET];
    2: NewState := [pOR];
    3: NewState := [pRR];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.StatisticTypes := FDataModel.StatisticTypes + NewState
  else
    FDataModel.StatisticTypes := FDataModel.StatisticTypes - NewState;
  DoModified();
end;

procedure TCtableStatDialogStatisticOptionsView.AttackRateItemChecked(Sender: TObject; Index: integer);
var
  NewState: TAttackRateTypes;
begin
  case Index of
    0: NewState := [pAR];
    1: NewState := [pEN];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.AttackRateTypes := FDataModel.AttackRateTypes + NewState
  else
    FDataModel.AttackRateTypes := FDataModel.AttackRateTypes - NewState;
  DoModified();
end;

procedure TCtableStatDialogStatisticOptionsView.IncludeItemChecked(Sender: TObject; Index: integer);
var
  NewState: TIncludeTypes;
begin
  case Index of
    0: NewState := [pInclude];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.IncludeTypes := FDataModel.IncludeTypes + NewState
  else
    FDataModel.IncludeTypes := FDataModel.IncludeTypes - NewState;
  DoModified();
end;

procedure TCtableStatDialogStatisticOptionsView.SortResultsSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.TableSort := sortName;
    1: FDataModel.TableSort := sortLabel;
    2: FDataModel.TableSort := sortStatistic;
  end;
  DoModified();
end;

procedure TCtableStatDialogStatisticOptionsView.EnterView();
begin
    FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
    FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
end;

function TCtableStatDialogStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TCtableStatDialogStatisticOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Statistics'
end;

function TCtableStatDialogStatisticOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TCtableStatDialogStatisticOptionsView.ResetView();
var i: Integer;
begin
  FDataModel.StatisticTypes := [];
  FDataModel.TableSort := sortName;

  for i := 0 to FStatisticsGroup.Items.Count - 1 do
    FStatisticsGroup.Checked[i] := false;

  FSortResultsGroup.ItemIndex := 0;
end;

procedure TCtableStatDialogStatisticOptionsView.SetModel(
  DataModel: TCtableStatDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.

