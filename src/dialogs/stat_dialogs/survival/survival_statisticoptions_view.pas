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
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TSurvivalStatDialogStatisticOptionsModel;
    FStatisticsGroup: TCheckGroup;
    FCIGroup: TRadioGroup;
    FCITypes: TCITypes;
//    FVerticalDivider: TBevel;
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
    procedure SetModel(DataModel: TSurvivalStatDialogStatisticOptionsModel);
  end;

implementation

{ TSurvivalStatDialogStatisticOptionsView }

constructor TSurvivalStatDialogStatisticOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

{  FVerticalDivider := TBevel.Create(self);
  FVerticalDivider.Parent := self;
  FVerticalDivider.Style := bsLowered;
  FVerticalDivider.Shape := bsSpacer;
  FVerticalDivider.Width := 5;
  FVerticalDivider.Anchors := [];
  FVerticalDivider.AnchorParallel(akTop, 5, Self);
  FVerticalDivider.AnchorParallel(akBottom, 0, Self);
}
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

//  FStatisticsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  CreateStatisticsCheckBoxes(FStatisticsGroup);
  CreateCIRadios(FCIGroup);
end;

procedure TSurvivalStatDialogStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Log rank test');
  CheckGroup.Items.Add('Use time intervals' + LineEnding + '(set "survival intervals")');
  CheckGroup.Items.Add('Adjust survival for intervals');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
end;

procedure TSurvivalStatDialogStatisticOptionsView.CreateCIRadios(RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Default (vertical bars)');
  RadioGroup.Items.Add('Upper and Lower lines');
  RadioGroup.Items.Add('CI band');
  RadioGroup.Items.Add('No CI');
  RadioGroup.OnClick := @CISelectionChanged;
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

procedure TSurvivalStatDialogStatisticOptionsView.CISelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.CIType := ciDefault;
    1: FDataModel.CIType := ciLine;
    2: FDataModel.CIType := ciBand;
    3: FDataModel.CIType := ciNone;
  end;
  DoModified();
end;

procedure TSurvivalStatDialogStatisticOptionsView.EnterView();
begin
  FCITypes := [ciDefault, ciLine, ciBand, ciNone];
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

  FDataModel.CIType := ciDefault;
end;

procedure TSurvivalStatDialogStatisticOptionsView.SetModel(
  DataModel: TSurvivalStatDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.
