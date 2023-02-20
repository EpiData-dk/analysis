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

  XDMIN_TAG    = Ord(cbdXMin);
  XDMAX_TAG    = Ord(cbdXMax);
  YDMIN_TAG    = Ord(cbdYMin);
  YDMAX_TAG    = Ord(cbdYMax);

{ TChartOptionsView }

constructor TChartOptionsView.Create(TheOwner: TComponent);
const
  entryBox = 150;
  dateSize = 120;
var
  EditText:  TCustomEdit;
  DateText:  TDateEdit;
  LabelText: TLabel;
  PrevLabel: TLabel;
  Labels:    array of UTF8String = ('Main Title', 'Footnote',
             'X-Axis Title', 'Y-Axis Title', 'Colour Selection',
             'X-Axis Minumum', 'X-Axis Maximum', 'Y-Axis Minumum', 'Y-Axis Maximum');
  i:         Integer;
begin
  inherited Create(TheOwner);

  SetLength(FText,  Ord(High(TChartOptionEdit)) + 1);
  SetLength(FDate,  Ord(High(TChartMinMaxDate)) + 1);
  SetLength(FLabel, Ord(High(TChartOptionEdit)) + 1);

  // create labels

  for i:= Low(FLabel) to High(FLabel) do
    begin
      LabelText := TLabel.Create(TheOwner);
      with LabelText do
        begin
          Caption := Labels[i];
          AnchorParallel(akLeft, 10, Self);
          if (i = 0) then
            AnchorParallel(akTop, 20, Self)
          else
            AnchorToNeighbour(akTop, 10, FLabel[i-1]);
          Parent := self;
        end;
      FLabel[i] := LabelText;
    end;

  // create CustomEdit controls

  for i := Low(FText) to High(FText) do
    begin
      EditText := TCustomEdit.Create(TheOwner);
      with EditText do
        begin
          AnchorParallel(akTop, 0, FLabel[i]);
          AnchorParallel(akLeft, entryBox, Self);
          OnChange := @SetText;
          Tag := i;
          if (i < XMIN_TAG) then
            AnchorParallel(akRight, 10, Self)
          else
            Visible := false;
          Parent := self;
        end;
      FText[i] := EditText;
     end;

  // create DateEdit controls in same position as corresponding CustomEdit controls

  for i := Low(FDate) to High(FDate) do
    begin
      DateText := TDateEdit.Create(TheOwner);
      with DateText do
        begin
          AnchorParallel(akLeft, entryBox, Self);
          AnchorParallel(akTop, 0, FLabel[i + XMIN_TAG]);
          Width := DateSize;
          Tag := i;
          OnChange := @SetDate;
          Visible := false;
          Parent := self;
        end;
      FDate[i] := DateText;
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
begin
  // reset all min/max fields before checking variable types
  ResetMinMax;
  if ((FDataModel.MinMax and 1) = 1) then
    begin
      if (Assigned(FDataModel.XVariable)) then
      begin
        if (FDataModel.XVariable.FieldType in DateFieldTypes) then
          FDate[XDMIN_TAG].Visible := true
        else
          FText[XMIN_TAG].Visible := true;
        FLabel[XMIN_TAG].Visible := true;
      end;
    end;

  if ((FDataModel.MinMax and 2) = 2) then
    begin
      if (Assigned(FDataModel.XVariable)) then
      begin
        if (FDataModel.XVariable.FieldType in DateFieldTypes) then
          FDate[XDMAX_TAG].Visible := true
        else
          FText[XMAX_TAG].Visible := true;
        FLabel[XMAX_TAG].Visible := true;
      end;
     end;

  if ((FDataModel.MinMax and 4) = 4) then
    begin
      if (Assigned(FDataModel.YVariable)) then
      begin
        if (FDataModel.YVariable.FieldType in DateFieldTypes) then
          FDate[YDMIN_TAG].Visible := true
        else
          FText[YMIN_TAG].Visible := true;
        FLabel[YMIN_TAG].Visible := true;
        end;
    end;

  if ((FDataModel.MinMax and 8) = 8) then
    begin
      if (Assigned(FDataModel.YVariable)) then
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

  for EditText in FText do
    EditText.Text := '';
  for DateText in FDate do
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
      FText[i].Visible := false;
      FLabel[i].Visible := false;
    end;
  for i := XDMIN_TAG to YDMAX_TAG do
    FDate[i].Visible := false;
end;

end.
