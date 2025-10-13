unit regress_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, regress_statisticoptions_model,
  stat_dialog_custom_view;

type

  { TRegressStatisticOptionsView }

  TRegressStatisticOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TRegressStatisticOptionsModel;
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
    procedure SetModel(DataModel: TRegressStatisticOptionsModel);
  end;

implementation

{ TRegressStatisticOptionsView }

constructor TRegressStatisticOptionsView.Create(TheOwner: TComponent);
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

procedure TRegressStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('No Constant Term');
  CheckGroup.Items.Add('Analysis of Variance');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
end;

procedure TRegressStatisticOptionsView.StatisticsItemChecked(Sender: TObject; Index: integer);
var
  NewState: TStatisticTypes;
begin
  case Index of
    0: NewState := [pC];
    1: NewState := [pA];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.StatisticTypes := FDataModel.StatisticTypes + NewState
  else
    FDataModel.StatisticTypes := FDataModel.StatisticTypes - NewState;
  DoModified();
end;

procedure TRegressStatisticOptionsView.EnterView();
begin

end;

function TRegressStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TRegressStatisticOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Regress options'
end;

function TRegressStatisticOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TRegressStatisticOptionsView.ResetView();
var i: Integer;
begin
  FDataModel.StatisticTypes := [];

  for i := 0 to FStatisticsGroup.Items.Count - 1 do
    FStatisticsGroup.Checked[i] := false;

end;

procedure TRegressStatisticOptionsView.SetModel(
  DataModel: TRegressStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.

