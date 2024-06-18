unit chart_options_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, StdCtrls, ExtCtrls, EditBtn,
  Controls, chart_options_model,
  stat_dialog_custom_view;

type

  { TChartOptionsView }

  TChartOptionsView = class(TCustomStatDialogView)
  private
    FDataModel: TChartOptionsModel;
    FOptLabel: array of TLabel;
    FOptText: array of TCustomEdit;
    FMinMaxDate: array of TDateEdit;
    FReplace: TCheckBox;
    FOnModified: IStatDiaglogViewModified;
    procedure SetText(Sender: TObject);
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
    procedure ReplaceBoxClicked(Sender: TObject);
  end;

implementation

uses Graphics, math;

const
  TITLE_TAG    = Ord(cbT);
  FOOTNOTE_TAG = Ord(cbF);
  XTITLE_TAG   = Ord(cbXT);
  YTITLE_TAG   = Ord(cbYT);
  COLOR_TAG    = Ord(cbC);
  EXPORT_TAG   = Ord(cbE);
  REPLACE_TAG  = Ord(cbR);
  XMIN_TAG     = Ord(cbnXMin);
  XMAX_TAG     = Ord(cbnXMax);
  YMIN_TAG     = Ord(cbnYMin);
  YMAX_TAG     = Ord(cbnYMax);
  inputSpacing = 10;

{ TChartOptionsView }

constructor TChartOptionsView.Create(TheOwner: TComponent);
var
  prevControl: TWinControl;
  LabelWidth,
  DateWidth: Integer;
  OptLabels:    array of UTF8String = ('Main Title', 'Footnote',
                      'X-Axis Title', 'Y-Axis Title', 'Colour Selection',
                      'Export to file', 'Replace graph file',
                      'X-Axis Minumum', 'X-Axis Maximum',
                      'Y-Axis Minumum', 'Y-Axis Maximum');
  i:         Integer;

function MaxWidth(AStrings: array of UTF8String): integer;
var
  bmp: Graphics.TBitmap;
  i: integer;
begin
  result := 0;
  for i := 0 to High(AStrings) do
    begin
      bmp := Graphics.TBitmap.Create;
      result := max(bmp.Canvas.TextWidth(AStrings[i]), result);
      bmp.Free;
    end;
end;

begin
  inherited Create(TheOwner);

  LabelWidth := MaxWidth(OptLabels) + 5;
  // add width for the calendar icon
  DateWidth  := MaxWidth(['0000-00-00']) + 36;

  SetLength(FOptText,  Ord(High(TChartOptionEdit)) + 1);
  SetLength(FOptLabel, Length(FOptText));
  SetLength(FMinMaxDate, Length(FOptText));

  // create option labels and edit fields
  for i := TITLE_TAG to YMAX_TAG do
    begin
      FOptLabel[i] := TLabel.Create(TheOwner);

      case i of
        REPLACE_TAG:
          begin
          // create checkbox for replace export file
            FReplace := TCheckBox.Create(TheOwner);
            with FReplace do
            begin
              OnClick := @ReplaceBoxClicked;
              AnchorToNeighbour(akTop, inputSpacing, prevControl);
              AnchorToNeighbour(akLeft, 10, FOptLabel[i]);
              AnchorParallel(akRight, 10, Self);
              Parent := Self;
              Tag := i;
            end;
            prevControl := FReplace;
          end;
        else
          begin
          // create a text box for all other options
            FOptText[i] := TCustomEdit.Create(TheOwner);
            with FOptText[i] do
            begin
              Parent := self;
              OnChange := @SetText;
              Tag := i;
              if (i=TITLE_TAG) then
                AnchorParallel(akTop, 0, Self)
              else if (i<REPLACE_TAG) then
                AnchorToNeighbour(akTop, inputSpacing, prevControl);
              // MinMax entries do not have top alignment
              AnchorToNeighbour(akLeft, 10, FOptLabel[i]);
              AnchorParallel(akRight, 10, Self);
            end;
            prevControl := FOptText[i];
          end;
      end;

      with FOptLabel[i] do
      begin
        Parent := self;
        Caption := OptLabels[i];
        AutoSize := false;
        Width := LabelWidth;
        Alignment := taRightJustify;
        AnchorParallel(akTop,0, prevControl);
        AnchorParallel(akLeft, 0, Self);
      end;
    end;

  // MinMax Date controls do not have top alignment
  // controls are aligned in EnterView
  for i := XMIN_TAG to YMAX_TAG do
    begin
      FMinMaxDate[i] := TDateEdit.Create(TheOwner);
      with FMinMaxDate[i] do
      begin
        AnchorParallel(akLeft, 0, FOptLabel[i]);
        Width := DateWidth;
        Tag := i;
        OnChange := @SetMinMaxDate;
        Parent := self;
      end;
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
  EXPORT_TAG:
    FDataModel.ExportName := EditText.Caption;
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

procedure TChartOptionsView.ReplaceBoxClicked(Sender: TObject);
begin
  FDataModel.ExportReplace := TCheckBox(Sender).Checked;
end;

procedure TChartOptionsView.EnterView();
var
  prevLabel: TLabel; // used to line up controls

  procedure setMinMaxEntry(isDate: Boolean; ix: Integer);
  begin
    FOptLabel[ix].AnchorToNeighbour(akTop, inputSpacing, prevLabel);
    FOptLabel[ix].Visible := true;
    prevLabel := FOptLabel[ix];
    if (isDate) then
      begin
        FMinMaxDate[ix].Visible := true;
        FMinMaxDate[ix].AnchorParallel(akTop, 0, prevLabel);
        FMinMaxDate[ix].AnchorToNeighbour(akLeft, 10, prevLabel);
      end
    else
      begin
        FOptText[ix].Visible := true;
        FOptText[ix].AnchorParallel(akTop, 0, prevLabel);
        FOptText[ix].AnchorToNeighbour(akLeft, 10, prevLabel);
      end;
  end;

begin
  ResetMinMax;
  prevLabel := FOptLabel[REPLACE_TAG];
  if (FDataModel.UseX) then
    begin
      if (mmtXMin in FDataModel.MinMax) then
        setMinMaxEntry(FDataModel.XDate, XMIN_TAG);
      if (mmtXMax in FDataModel.MinMax) then
        setMinMaxEntry(FDataModel.XDate, XMAX_TAG);
    end;
  if (FDataModel.UseY) then
    begin
      if (mmtYMin in FDataModel.MinMax) then
        setMinMaxEntry(FDataModel.YDate, YMIN_TAG);
      if (mmtYMax in FDataModel.MinMax) then
        setMinMaxEntry(FDataModel.YDate, YMAX_TAG);
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
  i: integer;
begin
  FDataModel.Title := '';
  FDataModel.Footnote := '';
  FDataModel.XTitle := '';
  FDataModel.YTitle := '';
  FDataModel.Colors := '';
  FDataModel.ExportName := '';
  FDataModel.ExportReplace := false;
  FDataModel.XMin := '';
  FDataModel.XMax := '';
  FDataModel.YMin := '';
  FDataModel.YMax := '';

  for i := TITLE_TAG to EXPORT_TAG do
    FOptText[i].Text := '';
  FReplace.Checked := false;
  for i := XMIN_TAG to YMAX_TAG do
    begin
      FOptText[i].Text := '';
      FMinMaxDate[i].Text := '';
    end;
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
      FOptText[i].Visible := false;
      FOptLabel[i].Visible := false;
      FMinMaxDate[i].Visible := false;
    end;
end;

end.
