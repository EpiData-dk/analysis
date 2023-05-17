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
    FOptLabel: array of TLabel;
    FMinMaxLabel: array of TLabel;
    FOptText: array of TCustomEdit;
    FMinMaxValue: array of TCustomEdit;
    FMinMaxDate: array of TDateEdit;
    FOnModified: IStatDiaglogViewModified;
{    FSetXmin,
    FSetXmax,
    FSetYmin,
    FSetYmax:  Boolean;}
  
    procedure SetText(Sender: TObject);
    procedure SetMinMaxValue(Sender: TObject);
    procedure SetMinMaxDate(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TChartOptionsModel);
    procedure ResetMinMax;
  end;

implementation

const
  TITLE_TAG    = Ord(cbT);
  FOOTNOTE_TAG = Ord(cbF);
  XTITLE_TAG   = Ord(cbXT);
  YTITLE_TAG   = Ord(cbYT);
  COLOR_TAG    = Ord(cbC);

  XMIN_TAG     = Ord(cbnXMin);
  XMAX_TAG     = Ord(cbnXMax);
  YMIN_TAG     = Ord(cbnYMin);
  YMAX_TAG     = Ord(cbnYMax);

{ TChartOptionsView }

constructor TChartOptionsView.Create(TheOwner: TComponent);
const
  // not guaranteed to fit every language, but calculating it from the labels
  // is not simple (LabelText.Width does not work)
  labelWidth = 120;
  dateSize   = 120;
  inputSpacing = 10;

var
  EditText:  TCustomEdit;
  DateText:  TDateEdit;
  LabelText: TLabel;
  OptLabels:    array of UTF8String = ('Main Title', 'Footnote',
                      'X-Axis Title', 'Y-Axis Title', 'Colour Selection');
  MinMaxLabels: array of UTF8String = ('X-Axis Minumum', 'X-Axis Maximum',
                      'Y-Axis Minumum', 'Y-Axis Maximum');
  i:         Integer;
begin
  inherited Create(TheOwner);

  SetLength(FOptText,  Ord(High(TChartOptionEdit)) + 1);
  SetLength(FOptLabel, Ord(High(TChartOptionEdit)) + 1);
  SetLength(FMinMaxValue,  Ord(High(TChartMinMax)) + 1);
  SetLength(FMinMaxDate,   Ord(High(TChartMinMax)) + 1);
  SetLength(FMinMaxLabel,  Ord(High(TChartMinMax)) + 1);

  // create options
  for i := Low(FOptText) to High(FOptText) do
    begin
      EditText  := TCustomEdit.Create(TheOwner);
      FOptText[i] := EditText;
      LabelText := TLabel.Create(TheOwner);
      FOptLabel[i] := LabelText;

      LabelText.Caption := OptLabels[i];
      LabelText.AutoSize := false;
      LabelText.Width := labelWidth;
      LabelText.Alignment := taRightJustify;
      LabelText.AnchorParallel(akLeft, 0, Self);
      LabelText.AnchorParallel(akTop, 0, EditText);
      LabelText.Parent := self;

      if (i=0) then
        EditText.AnchorParallel(akTop, 0, Self)
      else
        EditText.AnchorToNeighbour(akTop, inputSpacing, FOptText[i - 1]);
      EditText.AnchorToNeighbour(akLeft, 10, LabelText);
      EditText.OnChange := @SetText;
      EditText.Tag := i;
      EditText.AnchorParallel(akRight, 10, Self);
      EditText.Parent := self;
    end;

  // create MinMax Date and Edit controls without top alignment
  // controls are aligned in EnterView
  for i := Low(FMinMaxValue) to High(FMinMaxValue) do
    begin
      EditText := TCustomEdit.Create(TheOwner);
      FMinMaxValue[i] := EditText;
      DateText := TDateEdit.Create(TheOwner);
      FMinMaxDate[i] := DateText;
      LabelText := TLabel.Create(TheOwner);
      FMinMaxLabel[i] := LabelText;

      LabelText.Caption := MinMaxLabels[i];
      LabelText.AutoSize := false;
      LabelText.Width := labelWidth;
      LabelText.Alignment := taRightJustify;
      LabelText.AnchorParallel(akLeft, 0, Self);
      LabelText.Parent := self;

      EditText.AnchorToNeighbour(akLeft, 10, LabelText);
      EditText.Tag := i;
      EditText.OnChange := @SetMinMaxValue;
      EditText.Visible := false;
      EditText.Parent := self;
      FMinMaxValue[i] := EditText;

      DateText.AnchorToNeighbour(akLeft, 10, LabelText);
      DateText.Width := DateSize;
      DateText.Tag := i;
      DateText.OnChange := @SetMinMaxDate;
      DateText.Visible := false;
      DateText.Parent := self;
      FMinMaxDate[i] := DateText;
    end;

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
  end;
  DoModified;
end;

procedure TChartOptionsView.SetMinMaxValue(Sender: TObject);
var
  EditText: TCustomEdit;
begin
  EditText := TCustomEdit(Sender);
  case EditText.Tag of
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

procedure TChartOptionsView.SetMinMaxDate(Sender: TObject);
var
  DateText: TDateEdit;
begin
  DateText := TDateEdit(Sender);
  case DateText.Tag of
  XMIN_TAG:
    FDataModel.XMin := DateText.Caption;
  XMAX_TAG:
    FDataModel.XMax := DateText.Caption;
  YMIN_TAG:
    FDataModel.YMin := DateText.Caption;
  YMAX_TAG:
    FDataModel.YMax := DateText.Caption;
  end;
  DoModified;
end;

procedure TChartOptionsView.EnterView();
var
  prevEntry: TWinControl; // used to line up controls

  procedure setMinMaxEntry(isDate: Boolean; ix: Integer);
  begin
    if (isDate) then
      begin
        FMinMaxDate[ix].Visible := true;
        FMinMaxDate[ix].AnchorToNeighbour(akTop, 10, prevEntry);
        prevEntry := FMinMaxDate[ix];
      end
    else
      begin
        FMinMaxValue[ix].Visible := true;
        FMinMaxValue[ix].AnchorToNeighbour(akTop, 10, prevEntry);
        prevEntry := FMinMaxValue[ix];
      end;
    FMinMaxLabel[ix].AnchorParallel(akTop, 0, prevEntry);
    FminMaxLabel[ix].Visible := true;
  end;

begin
  ResetMinMax;
  prevEntry := FOptText[high(FOptText)];
  if (FDataModel.UseX) and (mmtXMin in FDataModel.MinMax) then
    setMinMaxEntry(FDataModel.XDate, XMIN_TAG);
  if (FDataModel.UseX) and (mmtXMax in FDataModel.MinMax) then
    setMinMaxEntry(FDataModel.XDate, XMAX_TAG);
  if (FDataModel.UseY) and (mmtYMin in FDataModel.MinMax) then
    setMinMaxEntry(FDataModel.YDate, YMIN_TAG);
  if (FDataModel.UseY) and (mmtYMax in FDataModel.MinMax) then
    setMinMaxEntry(FDataModel.YDate, YMAX_TAG);
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
  DateText: TDateEdit;
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

  for EditText in FOptText do
    EditText.Text := '';
  for EditText in FMinMaxValue do
    EditText.Text := '';
  for DateText in FMinMaxDate do
    DateText.Text := '';

  ResetMinMax;
  DoModified;
end;

procedure TChartOptionsView.SetModel(
  DataModel: TChartOptionsModel);
begin
  FDataModel := DataModel;
end;

procedure TChartOptionsView.ResetMinMax;
var
  i: Integer;
begin
  for i := XMIN_TAG to YMAX_TAG do
    begin
      FMinMaxValue[i].Visible := false;
      FMinMaxLabel[i].Visible := false;
      FMinMaxDate[i].Visible := false;
    end;
end;

end.
