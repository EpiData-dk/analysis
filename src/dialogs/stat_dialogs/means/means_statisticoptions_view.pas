unit means_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, Means_statisticoptions_model,
  stat_dialog_custom_view;

type

  { TMeansStatDialogStatisticOptionsView }

  TMeansStatDialogStatisticOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDialogViewModified;
    FDataModel: TMeansStatDialogStatisticOptionsModel;
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
    procedure SetModel(DataModel: TMeansStatDialogStatisticOptionsModel);
  end;

implementation

{ TMeansStatDialogStatisticOptionsView }

constructor TMeansStatDialogStatisticOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

//  Caption := 'Statistics Options';
  FStatisticsGroup  := TCheckGroup.Create(self);

  FStatisticsGroup.Parent := self;
  FStatisticsGroup.Caption := 'Statistics';
  FStatisticsGroup.Anchors := [];
  FStatisticsGroup.AnchorParallel(akTop, 5, Self);
  FStatisticsGroup.AnchorParallel(akLeft, 5, self);

  CreateStatisticsCheckBoxes(FStatisticsGroup);
end;

procedure TMeansStatDialogStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Analysis of Variance');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
end;

procedure TMeansStatDialogStatisticOptionsView.StatisticsItemChecked(Sender: TObject; Index: integer);
var
  NewState: TStatisticTypes;
begin
  case Index of
    0: NewState := [pT];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.StatisticTypes := FDataModel.StatisticTypes + NewState
  else
    FDataModel.StatisticTypes := FDataModel.StatisticTypes - NewState;
  DoModified();
end;

procedure TMeansStatDialogStatisticOptionsView.EnterView();
begin

end;

function TMeansStatDialogStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TMeansStatDialogStatisticOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Anova'
end;

function TMeansStatDialogStatisticOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TMeansStatDialogStatisticOptionsView.ResetView();
var i: Integer;
begin
  FDataModel.StatisticTypes := [];

  for i := 0 to FStatisticsGroup.Items.Count - 1 do
    FStatisticsGroup.Checked[i] := false;

end;

procedure TMeansStatDialogStatisticOptionsView.SetModel(
  DataModel: TMeansStatDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.

