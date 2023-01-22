unit chart_options_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, StdCtrls, ExtCtrls, EditBtn,
  Controls, chart_options_model, executor, epidatafilestypes,
  stat_dialog_custom_view;

type

  { TChartOptionsView }

  TChartOptionsView = class(TCustomStatDialogView)
  private
    FDataModel: TChartOptionsModel;
    FLabel: array of TLabel;
    FText: array of TCustomEdit;
    FDate: array of TDateEdit;
    FOnModified: IStatDiaglogViewModified;
    FSetXmin,
    FSetXmax,
    FSetYmin,
    FSetYmax:  Boolean;
  
    procedure SetText(Sender: TObject);
    procedure SetDate(Sender: TObject);
//    procedure UpdateTexts();
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TChartOptionsModel);
  end;

implementation

const
  TITLE_TAG    = Ord(cbT);
  FOOTNOTE_TAG = Ord(cbF);
  XTITLE_TAG   = Ord(cbXT);
  YTITLE_TAG   = Ord(cbYT);
  COLOR_TAG    = Ord(cbC);
  XMIN_TAG     = Ord(cbnXMin);
  XDMIN_TAG    = Ord(cbdXMin);
  XMAX_TAG     = Ord(cbnXMax);
  XDMAX_TAG    = Ord(cbdXMax);
  YMIN_TAG     = Ord(cbnYMin);
  YDMIN_TAG    = Ord(cbdYMin);
  YMAX_TAG     = Ord(cbnYMax);
  YDMAX_TAG    = Ord(cbdYMax);

{ TChartOptionsView }

constructor TChartOptionsView.Create(TheOwner: TComponent);
const
  entryBox = 150;
  dateSize = 120;
var
  EditText: TCustomEdit;
  DateText: TDateEdit;
//  PrevEditText: TCustomEdit;
//  PrevDateText: TDateEdit;
  LabelText: TLabel;
  PrevLabel: TLabel;
begin
  inherited Create(TheOwner);

  SetLength(FText,  Ord(High(TChartOptionBox)) + 1);
  SetLength(FDate,  Ord(High(TChartMinMaxBox)) + 1);
  SetLength(FLabel, Ord(High(TChartOptionBox)) + 1);

  LabelText := TLabel.Create(TheOwner);
  LabelText.Caption := 'Main Title';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorParallel(akTop,   10, Self);
  LabelText.Parent := self;

  EditText := TCustomEdit.Create(TheOwner);
  EditText.AnchorParallel(akTop,  10, Self);
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := TITLE_TAG;
  EditText.OnChange := @SetText;
  EditText.Parent := self;
  FText[TITLE_TAG] := EditText;
  FLabel[TITLE_TAG] := LabelText;
  PrevLabel := LabelText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Footnote';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevLabel);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevLabel);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := FOOTNOTE_TAG;
  EditText.OnChange := @SetText;
  FText[FOOTNOTE_TAG] := EditText;
  FLabel[FOOTNOTE_TAG] := LabelText;
  PrevLabel := LabelText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'X-axis Title';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevLabel);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevLabel);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := XTITLE_TAG;
  EditText.OnChange := @SetText;
  FText[XTITLE_TAG] := EditText;
  FLabel[XTITLE_TAG] := LabelText;
  PrevLabel := LabelText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Y-axis Title';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevLabel);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevLabel);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := YTITLE_TAG;
  EditText.OnChange := @SetText;
  FText[YTITLE_TAG] := EditText;
  FLabel[YTITLE_TAG] := LabelText;
  PrevLabel := LabelText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Color selection';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevLabel);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevLabel);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := COLOR_TAG;
  EditText.OnChange := @SetText;
  FText[COLOR_TAG] := EditText;
  FLabel[COLOR_TAG] := LabelText;
  PrevLabel := LabelText;

  // TODO:  Show 10 color button controls with the standard colors. User can click
  //        on a button to change it? Maybe a bit too complicated
  //        Problem is: once a color is set, you have to use the hex colours for all 10
  //        For this Dialog, just show the colors; don't make them clickable

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'X-axis minumum';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevLabel);
  LabelText.Visible := false;
  FLabel[XMIN_TAG] := LabelText;

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevLabel);
  EditText.Tag := XMIN_TAG;
  EditText.OnChange := @SetText;
  EditText.Visible := false;
  FText[XMIN_TAG] := EditText;

  DateText := TDateEdit.Create(TheOwner);
  DateText.Parent := self;
  DateText.AnchorParallel(akLeft, entryBox, Self);
  DateText.AnchorToNeighbour(akTop, 10, PrevLabel);
  DateText.Width := DateSize;
  DateText.Tag := XDMIN_TAG;
  DateText.OnChange := @SetDate;
  DateText.Visible := false;
  FDate[XDMIN_TAG] := DateText;
  PrevLabel := LabelText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'X-axis maximum';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevLabel);
  LabelText.Visible := false;
  FLabel[XMAX_TAG] := LabelText;

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevLabel);
  EditText.Tag := XMAX_TAG;
  EditText.OnChange := @SetText;
  FText[XMAX_TAG] := EditText;

  DateText := TDateEdit.Create(TheOwner);
  DateText.Parent := self;
  DateText.AnchorParallel(akLeft, entryBox, Self);
  DateText.AnchorToNeighbour(akTop, 10, PrevLabel);
  DateText.Width := DateSize;
  DateText.Tag := XDMAX_TAG;
  DateText.OnChange := @SetDate;
  DateText.Visible := false;
  FDate[XDMAX_TAG] := DateText;
  PrevLabel := LabelText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Y-axis minumum';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevLabel);
  LabelText.Visible := false;
  FLabel[YMIN_TAG] := LabelText;

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevLabel);
  EditText.Tag := YMIN_TAG;
  EditText.OnChange := @SetText;
  FText[YMIN_TAG] := EditText;

  DateText := TDateEdit.Create(TheOwner);
  DateText.Parent := self;
  DateText.AnchorParallel(akLeft, entryBox, Self);
  DateText.AnchorToNeighbour(akTop, 10, PrevLabel);
  DateText.Width := DateSize;
  DateText.Tag := YDMIN_TAG;
  DateText.OnChange := @SetDate;
  DateText.Visible := false;
  FDate[YDMIN_TAG] := DateText;
  PrevLabel := LabelText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Y-axis maximum';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevLabel);
  LabelText.Visible := false;
  FLabel[YMAX_TAG] := LabelText;

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevLabel);
  EditText.Tag := YMAX_TAG;
  EditText.OnChange := @SetText;
  FText[YMAX_TAG] := EditText;

  DateText := TDateEdit.Create(TheOwner);
  DateText.Parent := self;
  DateText.AnchorParallel(akLeft, entryBox, Self);
  DateText.AnchorToNeighbour(akTop, 10, PrevLabel);
  DateText.Width := DateSize;
  DateText.Tag := YDMAX_TAG;
  DateText.OnChange := @SetDate;
  DateText.Visible := false;
  FDate[YDMAX_TAG] := DateText;

end;

procedure TChartOptionsView.SetText(Sender: TObject);
var
  EditText: TCustomEdit;
begin
  EditText := TCustomEdit(Sender);
  case EditText.Tag of
  TITLE_TAG:
    FDataModel.Title := EditText.Caption;
  FOOTNOTE_TAG:
    FDataModel.Footnote := EditText.Caption;
  XTITLE_TAG:
    FDataModel.XTitle := EditText.Caption;
  YTITLE_TAG:
    FDataModel.YTitle := EditText.Caption;
  COLOR_TAG:
    FDataModel.Colors := EditText.Caption;
  XMIN_TAG:
    FDataModel.XMin := EditText.Caption;
  XMAX_TAG:
    FDataModel.XMax := EditText.Caption;
  YMIN_TAG:
    FDataModel.YMin := EditText.Caption;
  YMAX_TAG:
    FDataModel.YMax := EditText.Caption;
  end;
  DoModified;
end;

procedure TChartOptionsView.SetDate(Sender: TObject);
var
  DateText: TDateEdit;
begin
  DateText := TDateEdit(Sender);
  case DateText.Tag of
  XDMIN_TAG:
    FDataModel.XMin := DateText.Caption;
  XDMAX_TAG:
    FDataModel.XMax := DateText.Caption;
  YDMIN_TAG:
    FDataModel.YMin := DateText.Caption;
  YDMAX_TAG:
    FDataModel.YMax := DateText.Caption;
  end;
  DoModified;
end;

procedure TChartOptionsView.EnterView();
var
  i: integer;
begin
  // reset all min/max fields
  for i := XMIN_TAG to YMAX_TAG do
    begin
      FText[i].Visible := false;
      FLabel[i].Visible := false;
    end;
  for i := XDMIN_TAG to YDMAX_TAG do
    FDate[i].Visible := false;
  FDataModel.GetVars;

  if (Assigned(FDataModel.XVariable)) then
    begin
      if ((FDataModel.MinMax and 1) = 1) then
        begin
          if (FDataModel.XVariable.FieldType in DateFieldTypes) then
            FDate[XDMIN_TAG].Visible := true
          else
            FText[XMIN_TAG].Visible := true;
          FLabel[XMIN_TAG].Visible := true;
        end;
      if ((FDataModel.MinMax and 2) = 2) then
        begin
          if (FDataModel.XVariable.FieldType in DateFieldTypes) then
            FDate[XDMAX_TAG].Visible := true
          else
            FText[XMAX_TAG].Visible := true;
          FLabel[XMAX_TAG].Visible := true;
        end;
    end;

  if (Assigned(FDataModel.YVariable)) then
    begin
      if ((FDataModel.MinMax and 4) = 4) then
        begin
          if (FDataModel.YVariable.FieldType in DateFieldTypes) then
            FDate[YDMIN_TAG].Visible := true
          else
            FText[YMIN_TAG].Visible := true;
          FLabel[YMIN_TAG].Visible := true;
        end;
      if ((FDataModel.MinMax and 8) = 8) then
        begin
          if (FDataModel.YVariable.FieldType in DateFieldTypes) then
            FDate[YDMAX_TAG].Visible := true
          else
            FText[YMAX_TAG].Visible := true;
          FLabel[YMAX_TAG].Visible := true;
        end;
    end;
end;

function TChartOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TChartOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Chart Options'
end;

function TChartOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TChartOptionsView.ResetView();
var 
  EditText: TCustomEdit;
begin
  FDataModel.Title := '';
  FDataModel.Footnote := '';
  FDataModel.XTitle := '';
  FDataModel.YTitle := '';
  FDataModel.Colors := '';

  FDataModel.XMin := '';
  FDataModel.XMax := '';
  FDataModel.YMin := '';
  FDataModel.YMax := '';

  for EditText in FText do
    EditText.Text := '';

  DoModified;
end;

procedure TChartOptionsView.SetModel(
  DataModel: TChartOptionsModel);
begin
  FDataModel := DataModel;
end;

end.
