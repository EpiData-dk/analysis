unit survival_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, survival_statisticoptions_model,
  stat_dialog_custom_view;

type

  { TSurvivalDialogStatisticOptionsView }

  TSurvivalDialogStatisticOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TSurvivalDialogStatisticOptionsModel;
    FStatisticsGroup: TCheckGroup;
    FCIGroup: TRadioGroup;
    procedure CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
    procedure StatisticsItemChecked(Sender: TObject; Index: integer);
    procedure CreateCIRadios(RadioGroup: TRadioGroup);
    procedure CISelectionChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TSurvivalDialogStatisticOptionsModel);
  end;

implementation

{ TSurvivalDialogStatisticOptionsView }

constructor TSurvivalDialogStatisticOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FStatisticsGroup := TCheckGroup.Create(self);
  FStatisticsGroup.Parent := self;
  FStatisticsGroup.Caption := 'Statistics';
  FStatisticsGroup.Anchors := [];
  FStatisticsGroup.AnchorParallel(akTop, 5, Self);
  FStatisticsGroup.AnchorParallel(akLeft, 5, self);
  FStatisticsGroup.AnchorParallel(akRight, 5, self);

  FCIGroup := TRadioGroup.Create(Self);
  FCIGroup.Parent := self;
  FCIGroup.Caption := 'Confidence interval style';
  FCIGroup.Anchors := [];
  FCIGroup.AnchorToNeighbour(akTop, 10, FStatisticsGroup);
  FCIGroup.AnchorParallel(akLeft, 5, self);
  FCIGroup.AnchorParallel(akRight, 5, self);

  CreateStatisticsCheckBoxes(FStatisticsGroup);
  CreateCIRadios(FCIGroup);
end;

procedure TSurvivalDialogStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Log rank test');
  CheckGroup.Items.Add('Use time intervals' + LineEnding + '(set "survival intervals")');
  CheckGroup.Items.Add('Adjust survival for intervals');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
end;

procedure TSurvivalDialogStatisticOptionsView.CreateCIRadios(RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Default (vertical bars)');
  RadioGroup.Items.Add('Upper and Lower lines');
  RadioGroup.Items.Add('CI band');
  RadioGroup.Items.Add('No CI');
  RadioGroup.OnClick := @CISelectionChanged;
end;

procedure TSurvivalDialogStatisticOptionsView.StatisticsItemChecked(Sender: TObject; Index: integer);
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

procedure TSurvivalDialogStatisticOptionsView.CISelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.CIType := ciDefault;
    1: FDataModel.CIType := ciLine;
    2: FDataModel.CIType := ciBand;
    3: FDataModel.CIType := ciNone;
  end;
  DoModified();
end;

procedure TSurvivalDialogStatisticOptionsView.EnterView();
begin

end;

function TSurvivalDialogStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TSurvivalDialogStatisticOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Statistics'
end;

function TSurvivalDialogStatisticOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TSurvivalDialogStatisticOptionsView.ResetView();
var i: Integer;
begin
  FDataModel.StatisticTypes := [];

  for i := 0 to FStatisticsGroup.Items.Count - 1 do
    FStatisticsGroup.Checked[i] := false;

  FDataModel.CIType := ciDefault;
end;

procedure TSurvivalDialogStatisticOptionsView.SetModel(
  DataModel: TSurvivalDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.
