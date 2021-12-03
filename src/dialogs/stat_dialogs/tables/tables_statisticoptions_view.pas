unit tables_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, tables_statisticoptions_model,
  stat_dialog_custom_view;

type

  { TTableStatDialogStatisticOptionsView }

  TTableStatDialogStatisticOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TTableStatDialogStatisticOptionsModel;
    FStatisticsGroup: TCheckGroup;
    FSortResultsGroup: TRadioGroup;
    procedure CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
//    procedure CreateSortResultsRadio(RadioGroup: TRadioGroup);
    procedure StatisticsItemChecked(Sender: TObject; Index: integer);
//    procedure SortResultsSelectionChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TTableStatDialogStatisticOptionsModel);
  end;

implementation

{ TTableStatDialogStatisticOptionsView }

constructor TTableStatDialogStatisticOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

//  Caption := 'Statistics Options';
  FStatisticsGroup := TCheckGroup.Create(self);
//  FSortResultsGroup := TRadioGroup.Create(self);

  FStatisticsGroup.Parent := self;
  FStatisticsGroup.Caption := 'Statistics';
  FStatisticsGroup.Anchors := [];
  FStatisticsGroup.AnchorParallel(akTop, 5, Self);
  FStatisticsGroup.AnchorParallel(akLeft, 5, self);
//  FStatisticsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

{  FSortResultsGroup.Parent := self;
  FSortResultsGroup.Caption := 'Sort Results';
  FSortResultsGroup.Anchors := [];
  FSortResultsGroup.AnchorParallel(akTop, 5, Self);
  FSortResultsGroup.AnchorParallel(akRight, 5, self);
  FSortResultsGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);
}
  CreateStatisticsCheckBoxes(FStatisticsGroup);
//  CreateSortResultsRadio(FSortResultsGroup);
end;

procedure TTableStatDialogStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Chi-square Test');
  CheckGroup.Items.Add('Fisher Exact Test');
  CheckGroup.Items.Add('Odds Ratio');
  CheckGroup.Items.Add('Risk Ratio');
//  CheckGroup.Items.Add('Attack rate table plus Risk Ratios');
//  CheckGroup.Items.Add('Attack rate table without Risk Ratios');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
end;

{procedure TTableStatDialogStatisticOptionsView.CreateSortResultsRadio(RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('by Variable name');
  RadioGroup.Items.Add('by Variable label');
  RadioGroup.Items.Add('by key statistic');
  RadioGroup.OnSelectionChanged := @SortResultsSelectionChanged;
end;
}
procedure TTableStatDialogStatisticOptionsView.StatisticsItemChecked(Sender: TObject; Index: integer);
var
  NewState: TStatisticTypes;
begin
  case Index of
    0: NewState := [pChi];
    1: NewState := [pFET];
    2: NewState := [pOR];
    3: NewState := [pRR];
//    4: NewState := [pAR];
//    5: NewState := [pEN];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.StatisticTypes := FDataModel.StatisticTypes + NewState
  else
    FDataModel.StatisticTypes := FDataModel.StatisticTypes - NewState;
  DoModified();
end;
{
procedure TTableStatDialogStatisticOptionsView.SortResultsSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.TableSort := sortName;
    1: FDataModel.TableSort := sortLabel;
    2: FDataModel.TableSort := sortStatistic;
  end;
  DoModified();
end;
}
procedure TTableStatDialogStatisticOptionsView.EnterView();
begin

end;

function TTableStatDialogStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TTableStatDialogStatisticOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Statistics'
end;

function TTableStatDialogStatisticOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TTableStatDialogStatisticOptionsView.ResetView();
var i: Integer;
begin
  FDataModel.StatisticTypes := [];
//  FDataModel.TableSort := sortName;

  for i := 0 to FStatisticsGroup.Items.Count - 1 do
    FStatisticsGroup.Checked[i] := false;

//  FSortResultsGroup.ItemIndex := 0;
end;

procedure TTableStatDialogStatisticOptionsView.SetModel(
  DataModel: TTableStatDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.

