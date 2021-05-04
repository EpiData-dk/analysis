unit describe_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, ana_globals,
  Controls, describe_statisticoptions_model, executor,
  stat_dialog_custom_view;

type

  { TDescribeStatisticOptionsView }

  TDescribeStatisticOptionsView = class(TCustomStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TDescribeStatisticOptionsModel;
    FHorizontalDivider: TBevel;
    FVerticalDivider: TBevel;
    FMeansGroup: TCheckGroup;
    FFreqGroup: TCheckGroup;
    FRangeGroup:    TCheckGroup;
    FOtherGroup: TCheckGroup;

    procedure CreateMeansCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateFreqCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateRangeCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateOtherCheckboxes(CheckGroup: TCheckGroup);
    procedure MeansItemChecked(Sender: TObject; Index: integer);
    procedure FreqItemChecked(Sender: TObject; Index: integer);
    procedure RangeItemChecked(Sender: TObject; Index: integer);
    procedure OtherItemChecked(Sender: TObject; Index: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TDescribeStatisticOptionsModel);
  end;

implementation

{ TDescribeStatisticOptionsView }

constructor TDescribeStatisticOptionsView.Create(TheOwner: TComponent);

begin
  inherited Create(TheOwner);

  FMeansGroup   := TCheckGroup.Create(self);
  FFreqGroup    := TCheckGroup.Create(self);
  FRangeGroup := TCheckGroup.Create(self);
  FOtherGroup := TCheckGroup.Create(self);

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

  FMeansGroup.Parent := self;
  FMeansGroup.Caption := 'Means options';
  FMeansGroup.Anchors := [];
  FMeansGroup.AnchorParallel(akTop, 5, Self);
  FMeansGroup.AnchorParallel(akLeft, 5, self);
  FMeansGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  FMeansGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FOtherGroup.Parent := self;
  FOtherGroup.Caption := 'Other options';
  FOtherGroup.Anchors := [];
  FOtherGroup.AnchorParallel(akRight, 5, Self);
  FOtherGroup.AnchorParallel(akBottom, 5, self);
  FOtherGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FOtherGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  FFreqGroup.Parent := self;
  FFreqGroup.Caption := 'Freq options';
  FFreqGroup.Anchors := [];
  FFreqGroup.AnchorParallel(akLeft, 5, Self);
  FFreqGroup.AnchorParallel(akBottom, 5, self);
  FFreqGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FFreqGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FRangeGroup.Parent := self;
  FRangeGroup.Caption := 'Range options';
  FRangeGroup.Anchors := [];
  FRangeGroup.AnchorParallel(akTop, 5, Self);
  FRangeGroup.AnchorParallel(akRight, 5, self);
  FRangeGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  FRangeGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  CreateMeansCheckBoxes(FMeansGroup);
  CreateFreqCheckBoxes(FFreqGroup);
  CreateRangeCheckBoxes(FRangeGroup);
  CreateOtherCheckboxes(FOtherGroup);

end;

procedure TDescribeStatisticOptionsView.CreateMeansCheckboxes(CheckGroup: TCheckGroup);

begin
  CheckGroup.Items.Add('means, sd, sum');
  CheckGroup.Items.Add('mean and confidence interval');
  CheckGroup.OnItemClick := @MeansItemChecked;
end;

procedure TDescribeStatisticOptionsView.CreateFreqCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('5 most frequent values');
  CheckGroup.Items.Add('5 least frequent values');
  CheckGroup.OnItemClick := @FreqItemChecked;
end;

procedure TDescribeStatisticOptionsView.CreateRangeCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('min, median, max');
  CheckGroup.Items.Add('interdecile range');
  CheckGroup.Items.Add('interquartile range');
  CheckGroup.OnItemClick := @RangeItemChecked;
end;

procedure TDescribeStatisticOptionsView.CreateOtherCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('missing value count');
  CheckGroup.Items.Add('results in separate tables');
  CheckGroup.OnItemClick := @OtherItemChecked;
//  CheckGroup.Checked[1] := true;  // default to pCT on startup

end;

procedure TDescribeStatisticOptionsView.MeansItemChecked(Sender: TObject; Index: integer);
var
  NewState: TMeansTypes;
begin
  case Index of
    0: NewState := [pMSD];
    1: NewState := [pMCI];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.MeanTypes := FDataModel.MeanTypes + NewState
  else
    FDataModel.MeanTypes := FDataModel.MeanTypes - NewState;
  DoModified();
end;

procedure TDescribeStatisticOptionsView.FreqItemChecked(Sender: TObject; Index: integer);
var
  NewState: TFreqTypes;
begin
  case Index of
    0: NewState := [pFH];
    1: NewState := [pFL];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.FreqTypes := FDataModel.FreqTypes + NewState
  else
    FDataModel.FreqTypes := FDataModel.FreqTypes - NewState;
  DoModified();
end;

procedure TDescribeStatisticOptionsView.RangeItemChecked(Sender: TObject; Index: integer);
var
  NewState: TRangeTypes;
begin
  case Index of
    0: NewState := [pRM];
    1: NewState := [pIDR];
    2: NewState := [pIQR];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.RangeTypes := FDataModel.RangeTypes + NewState
  else
    FDataModel.RangeTypes := FDataModel.RangeTypes - NewState;
  DoModified();
end;

procedure TDescribeStatisticOptionsView.OtherItemChecked(Sender: TObject; Index: integer);
var
  NewState: TOtherTypes;
begin
  case Index of
    0: NewState := [pMissing];
    1: NewState := [pST];
  end;

    if (TCheckGroup(Sender).Checked[Index]) then
      FDataModel.OtherTypes := FDataModel.OtherTypes + NewState
    else
      FDataModel.OtherTypes := FDataModel.OtherTypes - NewState;
  DoModified();
end;

procedure TDescribeStatisticOptionsView.EnterView();
begin
    FVerticalDivider.Left := ((Self.Width - FVerticalDivider.Width) div 2);
    FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
end;

function TDescribeStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TDescribeStatisticOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Statistics'
end;

function TDescribeStatisticOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TDescribeStatisticOptionsView.ResetView();
var i: Integer;
begin
  FDataModel.MeanTypes := [];
  FDataModel.FreqTypes := [];
  FDataModel.OtherTypes := [];
  FDataModel.RangeTypes := [];

  for i := 0 to FMeansGroup.Items.Count - 1 do
    FMeansGroup.Checked[i] := false;

  for i := 0 to FFreqGroup.Items.Count - 1 do
    FFreqGroup.Checked[i] := false;

  for i := 0 to FRangeGroup.Items.Count - 1 do
    FRangeGroup.Checked[0] := false;

  for i := 0 to FOtherGroup.Items.Count - 1 do
    FOtherGroup.Checked[0] := false;
end;

procedure TDescribeStatisticOptionsView.SetModel(
  DataModel: TDescribeStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.
