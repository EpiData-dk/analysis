unit ctable_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, ctable_statisticoptions_model,
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
    FSortingGroup: TRadioGroup;
    FOutputSortGroup: TRadioGroup;

    procedure CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateAttackRateCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateSortingRadios(RadioGroup: TRadioGroup);
    procedure CreateOutputSortRadio(RadioGroup: TRadioGroup);
    procedure StatisticsItemChecked(Sender: TObject; Index: integer);
    procedure AttackRateItemChecked(Sender: TObject; Index: integer);
    procedure SortingSelectionChanged(Sender: TObject);
    procedure OutputSortSelectionChanged(Sender: TObject);
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
  FSortingGroup := TRadioGroup.Create(self);
  FOutputSortGroup := TRadioGroup.Create(self);

  FVerticalDivider := TBevel.Create(self);
  with FVerticalDivider do
  begin
    Parent := self;
    Style := bsLowered;
    Shape := bsSpacer;
    Width := 5;
    Anchors := [];
    AnchorParallel(akTop, 5, Self);
    AnchorParallel(akBottom, 0, Self);
  end;

  FHorizontalDivider := TBevel.Create(self);
  with FHorizontalDivider do
  begin
    Parent := self;
    Style := bsLowered;
    Shape := bsSpacer;
    Height := 5;
    Anchors := [];
    AnchorParallel(akLeft, 0, Self);
    AnchorParallel(akRight, 0, Self);
  end;

  FStatisticsGroup.Parent := self;
  with FStatisticsGroup do
  begin
    Caption := 'Statistics';
    Anchors := [];
    AnchorParallel(akTop, 5, Self);
    AnchorParallel(akLeft, 5, self);
    AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
    AnchorToNeighbour(akRight, 0, FVerticalDivider);
  end;

  FOutputSortGroup.Parent := self;
  with FOutputSortGroup do
  begin
    Caption := 'Sort results';
    Anchors := [];
    AnchorParallel(akRight, 5, self);
    AnchorParallel(akBottom, 5, Self);
    AnchorToNeighbour(akTop, 0, FHorizontalDivider);
    AnchorToNeighbour(akLeft, 0, FVerticalDivider);
  end;

  FAttackRateGroup.Parent := self;
  with FAttackRateGroup do
  begin
    Caption := 'Attack rate tables';
    Anchors := [];
    AnchorParallel(akLeft, 5, Self);
    AnchorParallel(akBottom, 5, self);
    AnchorToNeighbour(akTop, 0, FHorizontalDivider);
    AnchorToNeighbour(akRight, 0, FVerticalDivider);
  end;

  FSortingGroup.Parent := self;
  with FSortingGroup do
  begin
    Caption := 'Sort outcome and exposure data (important for RR)';
    Anchors := [];
    AnchorParallel(akTop, 5, Self);
    AnchorParallel(akRight, 5, self);
    AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
    AnchorToNeighbour(akLeft, 0, FVerticalDivider);
  end;

  CreateStatisticsCheckBoxes(FStatisticsGroup);
  CreateAttackRateCheckBoxes(FAttackRateGroup);
  CreateSortingRadios(FSortingGroup);
  CreateOutputSortRadio(FOutputSortGroup);
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

procedure TCtableStatDialogStatisticOptionsView.CreateSortingRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Ascending (example: for RR, 0=no, 1=yes)');
  RadioGroup.Items.Add('Descending (example: for RR, N=no, Y=yes)');
//  RadioGroup.Items.Add('Totals Ascending');
//  RadioGroup.Items.Add('Totals Descending');
  RadioGroup.ItemIndex := 0;
  RadioGroup.OnSelectionChanged := @SortingSelectionChanged;
end;

procedure TCtableStatDialogStatisticOptionsView.CreateOutputSortRadio(RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('by Variable name');
  RadioGroup.Items.Add('by Variable label');
  RadioGroup.Items.Add('by key statistic');
  RadioGroup.OnSelectionChanged := @OutputSortSelectionChanged;
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

procedure TCtableStatDialogStatisticOptionsView.SortingSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.Sorting := sortAsc;
    1: FDataModel.Sorting := sortDesc;
 //   2: FDataModel.Sorting := sortAscTotal;
 //   3: FDataModel.Sorting := sortDescTotal;
  end;
  DoModified();
end;

procedure TCtableStatDialogStatisticOptionsView.OutputSortSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.OutputSort := sortName;
    1: FDataModel.OutputSort := sortLabel;
    2: FDataModel.OutputSort := sortStatistic;
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
  FDataModel.OutputSort := sortName;
  FDataModel.Sorting := sortAsc;
  FSortingGroup.ItemIndex := 0;
  FOutputSortGroup.ItemIndex := 0;

  for i := 0 to FStatisticsGroup.Items.Count - 1 do
    FStatisticsGroup.Checked[i] := false;

end;

procedure TCtableStatDialogStatisticOptionsView.SetModel(
  DataModel: TCtableStatDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.

