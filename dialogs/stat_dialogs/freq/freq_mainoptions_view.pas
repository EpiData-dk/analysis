unit freq_mainoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, freq_mainoptions_model,
  Controls;

type

  { TFreqMainOptionsView }

  TFreqMainOptionsView = class(TPanel, IStatDialogView)
  private
    FHorizontalDivider: TBevel;
    FOnModified: IStatDiaglogViewModified;
    FDataModel: TFreqMainOptionsModel;
    FOtherGroup: TCheckGroup;
    FPercentageGroup: TCheckGroup;
    procedure OtherGroupItemCheck(Sender: TObject; Index: integer);
    procedure PercentageGroupClick(Sender: TObject; Index: integer);
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
    procedure SetDatamodel(DataModel: TFreqMainOptionsModel);
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
  end;

implementation

{ TFreqMainOptionsView }

procedure TFreqMainOptionsView.PercentageGroupClick(Sender: TObject;
  Index: integer);
begin
  case Index of
    0: FDataModel.ShowRowPercentage        := TCheckGroup(Sender).Checked[Index];
    1: FDataModel.ShowCumulativePercentage := TCheckGroup(Sender).Checked[Index];
  end;

  DoModified();
end;

procedure TFreqMainOptionsView.OtherGroupItemCheck(Sender: TObject;
  Index: integer);
begin
  case Index of
    0: FDataModel.ShowMissing            := TCheckGroup(Sender).Checked[Index];
    1: FDataModel.ShowConfidenceInterval := TCheckGroup(Sender).Checked[Index];
  end;

  DoModified();
end;

procedure TFreqMainOptionsView.DoModified();
begin
  if (Assigned(FOnModified)) then
    FOnModified.OnViewModified(self);
end;

constructor TFreqMainOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FHorizontalDivider := TBevel.Create(self);
  FHorizontalDivider.Parent := self;
  FHorizontalDivider.Style := bsLowered;
  FHorizontalDivider.Shape := bsSpacer;
  FHorizontalDivider.Height := 5;
  FHorizontalDivider.Anchors := [];
  FHorizontalDivider.AnchorParallel(akLeft, 0, Self);
  FHorizontalDivider.AnchorParallel(akRight, 0, Self);

  FPercentageGroup := TCheckGroup.Create(self);
  FPercentageGroup.Parent := self;
  FPercentageGroup.Anchors := [];
  FPercentageGroup.AnchorParallel(akTop, 5, Self);
  FPercentageGroup.AnchorParallel(akLeft, 5, Self);
  FPercentageGroup.AnchorParallel(akRight, 5, Self);
  FPercentageGroup.AnchorToNeighbour(akBottom, 0, FHorizontalDivider);
  FPercentageGroup.Caption := 'Percentages';
  FPercentageGroup.Items.Add('Row');
  FPercentageGroup.Items.Add('Cumulative');
  FPercentageGroup.OnItemClick := @PercentageGroupClick;

  FOtherGroup := TCheckGroup.Create(self);
  FOtherGroup.Parent := self;
  FOtherGroup.Anchors := [];
  FOtherGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FOtherGroup.AnchorParallel(akBottom, 5, Self);
  FOtherGroup.AnchorParallel(akLeft, 5, Self);
  FOtherGroup.AnchorParallel(akRight, 5, Self);
  FOtherGroup.Caption := 'Other';
  FOtherGroup.Items.Add('Show Missing');
  FOtherGroup.Items.Add('Confidence Interval');
  FOtherGroup.OnItemClick := @OtherGroupItemCheck;
end;

procedure TFreqMainOptionsView.EnterView();
begin
  FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
end;

function TFreqMainOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TFreqMainOptionsView.GetControl(): TControl;
begin
  result := self
end;

function TFreqMainOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Options';
end;

function TFreqMainOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TFreqMainOptionsView.ResetView();
var
  i: Integer;
begin
  for i := 0 to FPercentageGroup.Items.Count - 1 do
    FPercentageGroup.Checked[i] := false;

  for i := 0 to FOtherGroup.Items.Count - 1 do
    FOtherGroup.Checked[i] := false;

  FDataModel.ShowRowPercentage        := false;
  FDataModel.ShowCumulativePercentage := false;
  FDataModel.ShowMissing              := false;
  FDataModel.ShowConfidenceInterval   := false;
end;

procedure TFreqMainOptionsView.SetDatamodel(DataModel: TFreqMainOptionsModel);
begin
  FDataModel := DataModel;
end;

procedure TFreqMainOptionsView.SetOnModified(OnModified: IStatDiaglogViewModified);
begin
  FOnModified := OnModified;
end;

end.

