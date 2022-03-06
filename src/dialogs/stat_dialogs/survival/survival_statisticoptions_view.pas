unit survival_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, survival_statisticoptions_model,
  stat_dialog_custom_view;

type

  { TSurvivalStatDialogStatisticOptionsView }

  TSurvivalStatDialogStatisticOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDialogViewModified;
    FDataModel: TSurvivalStatDialogStatisticOptionsModel;
    FStatisticsGroup: TCheckGroup;
    procedure CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
    procedure StatisticsItemChecked(Sender: TObject; Index: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TSurvivalStatDialogStatisticOptionsModel);
  end;

implementation

{ TSurvivalStatDialogStatisticOptionsView }

constructor TSurvivalStatDialogStatisticOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FStatisticsGroup := TCheckGroup.Create(self);
  FStatisticsGroup.Parent := self;
  FStatisticsGroup.Caption := 'Statistics';
  FStatisticsGroup.Anchors := [];
  FStatisticsGroup.AnchorParallel(akTop, 5, Self);
  FStatisticsGroup.AnchorParallel(akLeft, 5, self);

  CreateStatisticsCheckBoxes(FStatisticsGroup);

end;

procedure TSurvivalStatDialogStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Log rank test');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
end;

procedure TSurvivalStatDialogStatisticOptionsView.StatisticsItemChecked(Sender: TObject; Index: integer);
var
  NewState: TStatisticTypes;
begin
  case Index of
    0: NewState := [pChi];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.StatisticTypes := FDataModel.StatisticTypes + NewState
  else
    FDataModel.StatisticTypes := FDataModel.StatisticTypes - NewState;
  DoModified();
end;

procedure TSurvivalStatDialogStatisticOptionsView.EnterView();
begin

end;

function TSurvivalStatDialogStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TSurvivalStatDialogStatisticOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Statistics'
end;

function TSurvivalStatDialogStatisticOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TSurvivalStatDialogStatisticOptionsView.ResetView();
var i: Integer;
begin
  FDataModel.StatisticTypes := [];

  for i := 0 to FStatisticsGroup.Items.Count - 1 do
    FStatisticsGroup.Checked[i] := false;

end;

procedure TSurvivalStatDialogStatisticOptionsView.SetModel(
  DataModel: TSurvivalStatDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.
