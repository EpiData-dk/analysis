unit describe_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, describe_statisticoptions_model,
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
    FMissingGroup:    TCheckGroup;
    FCompactGroup: TCheckGroup;

    procedure CreateMeansCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateFreqCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateMissingCheckboxes(CheckGroup: TCheckGroup);
    procedure CreateCompactCheckboxes(CheckGroup: TCheckGroup);
    procedure MeansItemChecked(Sender: TObject; Index: integer);
    procedure FreqItemChecked(Sender: TObject; Index: integer);
    procedure MissingItemChecked(Sender: TObject; Index: integer);
    procedure CompactItemChecked(Sender: TObject; Index: integer);
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

//  Caption := 'Statistics Options';
  FMeansGroup   := TCheckGroup.Create(self);
  FFreqGroup    := TCheckGroup.Create(self);
  FMissingGroup := TCheckGroup.Create(self);
  FCompactGroup := TCheckGroup.Create(self);

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

  FCompactGroup.Parent := self;
  FCompactGroup.Caption := 'Compact table';
  FCompactGroup.Anchors := [];
  FCompactGroup.AnchorParallel(akTop, 5, Self);
  FCompactGroup.AnchorParallel(akRight, 5, self);
  FCompactGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  FCompactGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  FFreqGroup.Parent := self;
  FFreqGroup.Caption := 'Freq options';
  FFreqGroup.Anchors := [];
  FFreqGroup.AnchorParallel(akLeft, 5, Self);
  FFreqGroup.AnchorParallel(akBottom, 5, self);
  FFreqGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FFreqGroup.AnchorToNeighbour(akRight, 0, FVerticalDivider);

  FMissingGroup.Parent := self;
  FMissingGroup.Caption := 'Show missing';
  FMissingGroup.Anchors := [];
  FMissingGroup.AnchorParallel(akRight, 5, Self);
  FMissingGroup.AnchorParallel(akBottom, 5, self);
  FMissingGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FMissingGroup.AnchorToNeighbour(akLeft, 0, FVerticalDivider);

  CreateMeansCheckBoxes(FMeansGroup);
  CreateFreqCheckBoxes(FFreqGroup);
  CreateMissingCheckBoxes(FMissingGroup);
  CreateCompactCheckboxes(FCompactGroup);
end;

procedure TDescribeStatisticOptionsView.CreateMeansCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('means, sd, sum');
  CheckGroup.Items.Add('mean and CI');
  CheckGroup.Items.Add('min, median, max');
  CheckGroup.Items.Add('interdecile range');
  CheckGroup.Items.Add('interquartile range');
  CheckGroup.OnItemClick := @MeansItemChecked;
end;

procedure TDescribeStatisticOptionsView.CreateFreqCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('5 most frequent values');
  CheckGroup.Items.Add('5 least frequent values');
  CheckGroup.OnItemClick := @FreqItemChecked;
end;

procedure TDescribeStatisticOptionsView.CreateMissingCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('Missing value count');
  CheckGroup.OnItemClick := @MissingItemChecked;
end;

procedure TDescribeStatisticOptionsView.CreateCompactCheckboxes(CheckGroup: TCheckGroup);
begin
  CheckGroup.Items.Add('one row per variable if possible');
  CheckGroup.OnItemClick := @CompactItemChecked;
end;

procedure TDescribeStatisticOptionsView.MeansItemChecked(Sender: TObject; Index: integer);
var
  NewState: TMeansTypes;
begin
  case Index of
    0: NewState := [pMSD];
    1: NewState := [pMCI];
    2: NewState := [pRM];
    3: NewState := [pIDR];
    4: NewState := [pIQR];
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

procedure TDescribeStatisticOptionsView.MissingItemChecked(Sender: TObject; Index: integer);
var
  NewState: TMissingTypes;
begin
  case Index of
    0: NewState := [pMissing];
  end;

  if (TCheckGroup(Sender).Checked[Index]) then
    FDataModel.MissingTypes := FDataModel.MissingTypes + NewState
  else
    FDataModel.MissingTypes := FDataModel.MissingTypes - NewState;
  DoModified();
end;

procedure TDescribeStatisticOptionsView.CompactItemChecked(Sender: TObject; Index: integer);
var
  NewState: TCompactTypes;
begin
  case Index of
    0: NewState := [pCT];
  end;

    if (TCheckGroup(Sender).Checked[Index]) then
      FDataModel.CompactTypes := FDataModel.CompactTypes + NewState
    else
      FDataModel.CompactTypes := FDataModel.CompactTypes - NewState;
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
  FDataModel.CompactTypes := [];
  FDataModel.MissingTypes := [];

  for i := 0 to FMeansGroup.Items.Count - 1 do
    FMeansGroup.Checked[i] := false;

  for i := 0 to FFreqGroup.Items.Count - 1 do
    FFreqGroup.Checked[i] := false;

  FMissingGroup.Checked[0] := false;
  FCompactGroup.Checked[0] := false;
end;

procedure TDescribeStatisticOptionsView.SetModel(
  DataModel: TDescribeStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

end.
