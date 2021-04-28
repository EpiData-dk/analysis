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
    FSortingGroup: TRadioGroup;
    FVerticalDivider: TBevel;
    procedure CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateSortingRadios(RadioGroup: TRadioGroup);
    procedure StatisticsItemChecked(Sender: TObject; Index: integer);
    procedure SortingSelectionChanged(Sender: TObject);
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

  FVerticalDivider := TBevel.Create(self);
  FVerticalDivider.Parent := self;
  FVerticalDivider.Style := bsLowered;
  FVerticalDivider.Shape := bsSpacer;
  FVerticalDivider.Width := 5;
  FVerticalDivider.Anchors := [];
  FVerticalDivider.AnchorParallel(akTop, 5, Self);
  FVerticalDivider.AnchorParallel(akBottom, 0, Self);

  FStatisticsGroup := TCheckGroup.Create(self);
  FStatisticsGroup.Parent := self;
  FStatisticsGroup.Caption := 'Statistics';
  FStatisticsGroup.Anchors := [];
  FStatisticsGroup.AnchorParallel(akTop, 5, Self);
  FStatisticsGroup.AnchorParallel(akLeft, 5, self);
  FStatisticsGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FSortingGroup := TRadioGroup.Create(self);
  FSortingGroup.Parent := self;
  FSortingGroup.Caption := 'Sorting';
  FSortingGroup.Anchors := [];
  FSortingGroup.AnchorParallel(akRight, 5, self);
  FSortingGroup.AnchorParallel(akTop, 5, Self);
  FSortingGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  CreateStatisticsCheckBoxes(FStatisticsGroup);
  CreateSortingRadios(FSortingGroup);
end;

procedure TTableStatDialogStatisticOptionsView.CreateStatisticsCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Chi-square Test');
  CheckGroup.Items.Add('Fisher Exact Test');
  CheckGroup.Items.Add('Odds Ratio');
  CheckGroup.Items.Add('Risk Ratio');
  CheckGroup.OnItemClick := @StatisticsItemChecked;
end;

procedure TTableStatDialogStatisticOptionsView.CreateSortingRadios(
  RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Ascending (example: for RR, 0=no, 1=yes)');
  RadioGroup.Items.Add('Descending (example: for RR, N=no, Y=yes)');
  RadioGroup.Items.Add('Totals Ascending');
  RadioGroup.Items.Add('Totals Descending');
  RadioGroup.ItemIndex := 0;
  RadioGroup.OnSelectionChanged := @SortingSelectionChanged;
end;

procedure TTableStatDialogStatisticOptionsView.StatisticsItemChecked(Sender: TObject; Index: integer);
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

procedure TTableStatDialogStatisticOptionsView.SortingSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: FDataModel.Sorting := sortAsc;
    1: FDataModel.Sorting := sortDesc;
    2: FDataModel.Sorting := sortAscTotal;
    3: FDataModel.Sorting := sortDescTotal;
  end;
  DoModified();
end;

procedure TTableStatDialogStatisticOptionsView.EnterView();
begin
  FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
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
  FDataModel.Sorting := sortAsc;
  for i := 0 to FStatisticsGroup.Items.Count - 1 do
    FStatisticsGroup.Checked[i] := false;

  FSortingGroup.ItemIndex := 0;
end;

procedure TTableStatDialogStatisticOptionsView.SetModel(
  DataModel: TTableStatDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.

