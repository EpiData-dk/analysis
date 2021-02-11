unit ctable_statdialog_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, ctable_statdialog_statisticoptions_model;

type

  { TCtableStatDialogStatisticOptionsView }

  TCtableStatDialogStatisticOptionsView = class(TPanel, IStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TCtableStatDialogStatisticOptionsModel;
    FHorizontalDivider: TBevel;
    FVerticalDivider: TBevel;
    FStatisticsGroup: TCheckGroup;
    FSortResultsGroup: TRadioGroup;
    procedure CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateSortResultsRadio(RadioGroup: TRadioGroup);
    procedure StatisticsItemChecked(Sender: TObject; Index: integer);
    procedure SortResultsSelectionChanged(Sender: TObject);
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
    procedure SetModel(DataModel: TCtableStatDialogStatisticOptionsModel);
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
  end;

implementation

{ TCtableStatDialogStatisticOptionsView }

procedure TCtableStatDialogStatisticOptionsView.DoModified();
begin
  if (Assigned(FOnModified)) then
    FOnModified.OnViewModified(Self);
end;

constructor TCtableStatDialogStatisticOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

//  Caption := 'Statistics Options';
  FStatisticsGroup := TCheckGroup.Create(self);
  FSortResultsGroup := TRadioGroup.Create(self);

  FVerticalDivider := TBevel.Create(self);
  FVerticalDivider.Parent := self;
  FVerticalDivider.Style := bsLowered;
  FVerticalDivider.Shape := bsSpacer;
  FVerticalDivider.Width := 5;
  FVerticalDivider.Anchors := [];
  FVerticalDivider.AnchorParallel(akTop, 5, Self);
  FVerticalDivider.AnchorParallel(akBottom, 0, Self);

  FStatisticsGroup.Parent := self;
  FStatisticsGroup.Caption := 'Statistics';
  FStatisticsGroup.Anchors := [];
  FStatisticsGroup.AnchorParallel(akTop, 5, Self);
  FStatisticsGroup.AnchorParallel(akLeft, 5, self);
  FStatisticsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FSortResultsGroup.Parent := self;
  FSortResultsGroup.Caption := 'Sort Results';
  FSortResultsGroup.Anchors := [];
  FSortResultsGroup.AnchorParallel(akTop, 5, Self);
  FSortResultsGroup.AnchorParallel(akRight, 5, self);
  FSortResultsGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  CreateStatisticsCheckBoxes(FStatisticsGroup);
  CreateSortResultsRadio(FSortResultsGroup);
end;

procedure TCtableStatDialogStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Chi-square Test');
  CheckGroup.Items.Add('Fisher Exact Test');
  CheckGroup.Items.Add('Odds Ratio');
  CheckGroup.Items.Add('Risk Ratio');
  CheckGroup.Items.Add('Attack rate table plus Risk Ratios');
  CheckGroup.Items.Add('Attack rate table only');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
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
    4: NewState := [pAR];
    5: NewState := [pEN];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.StatisticTypes := FDataModel.StatisticTypes + NewState
  else
    FDataModel.StatisticTypes := FDataModel.StatisticTypes - NewState;
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
end;

function TCtableStatDialogStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TCtableStatDialogStatisticOptionsView.GetControl(): TControl;
begin
  result := self;
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

procedure TCtableStatDialogStatisticOptionsView.SetOnModified(
  OnModified: IStatDiaglogViewModified);
begin
  FOnModified := OnModified;
end;

end.

