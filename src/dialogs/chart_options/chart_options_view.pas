unit chart_options_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, StdCtrls, ExtCtrls,
  Controls, chart_options_model, executor,
  stat_dialog_custom_view;

type

  { TChartOptionsView }

  TChartOptionsView = class(TCustomStatDialogView)
  private
    FDataModel: TChartOptionsModel;
    FLabel: array of TLabel;
    FText: array of TCustomEdit;
    FOnModified: IStatDiaglogViewModified;
  
    procedure SetText(Sender: TObject);
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
  XMIN_TAG     = Ord(cbXMin);
  XMAX_TAG     = Ord(cbXMax);
  YMIN_TAG     = Ord(cbYMin);
  YMAX_TAG     = Ord(cbYMax);
  
{ TChartOptionsView }

constructor TChartOptionsView.Create(TheOwner: TComponent);
const
  entryBox = 140;
var
  EditText: TCustomEdit;
  PrevEditText: TCustomEdit;
  LabelText: TLabel;
begin
  inherited Create(TheOwner);

  SetLength(FText, Ord(High(TChartOptionsBox)) + 1);
  SetLength(FLabel, Ord(High(TChartOptionsBox)) + 1);

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
  PrevEditText := EditText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Footnote';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevEditText);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevEditText);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := FOOTNOTE_TAG;
  EditText.OnChange := @SetText;
  FText[FOOTNOTE_TAG] := EditText;
  FLabel[FOOTNOTE_TAG] := LabelText;
  PrevEditText := EditText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'X-axis Title';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevEditText);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevEditText);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := XTITLE_TAG;
  EditText.OnChange := @SetText;
  FText[XTITLE_TAG] := EditText;
  FLabel[XTITLE_TAG] := LabelText;
  PrevEditText := EditText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Y-axis Title';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevEditText);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevEditText);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := YTITLE_TAG;
  EditText.OnChange := @SetText;
  FText[YTITLE_TAG] := EditText;
  FLabel[YTITLE_TAG] := LabelText;
  PrevEditText := EditText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Color selection';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevEditText);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevEditText);
  EditText.AnchorParallel(akRight, 10, Self);
  EditText.Tag := COLOR_TAG;
  EditText.OnChange := @SetText;
  FText[COLOR_TAG] := EditText;
  FLabel[COLOR_TAG] := LabelText;
  PrevEditText := EditText;

  // TODO:  Show 10 color button controls with the standard colors. User can click
  //        on a button to change it? Maybe a bit too complicated
  //        Problem is: once a color is set, you have to use the hex colours for all 10
  //        For this Dialog, just show the colors; don't make them clickable

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'X-axis minumum';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevEditText);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevEditText);
  EditText.Tag := XMIN_TAG;
  EditText.OnChange := @SetText;
  FText[XMIN_TAG] := EditText;
  FLabel[XMIN_TAG] := LabelText;
  PrevEditText := EditText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'X-axis maximum';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevEditText);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevEditText);
  EditText.Tag := XMAX_TAG;
  EditText.OnChange := @SetText;
  FText[XMAX_TAG] := EditText;
  FLabel[XMAX_TAG] := LabelText;
  PrevEditText := EditText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Y-axis minumum';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevEditText);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevEditText);
  EditText.Tag := YMIN_TAG;
  EditText.OnChange := @SetText;
  FText[YMIN_TAG] := EditText;
  FLabel[YMIN_TAG] := LabelText;
  PrevEditText := EditText;

  LabelText := TLabel.Create(TheOwner);
  LabelText.Parent := self;
  LabelText.Caption := 'Y-axis maximum';
  LabelText.AnchorParallel(akLeft,  10, Self);
  LabelText.AnchorToNeighbour(akTop, 10, PrevEditText);

  EditText := TCustomEdit.Create(TheOwner);
  EditText.Parent := self;
  EditText.AnchorParallel(akLeft, entryBox, Self);
  EditText.AnchorToNeighbour(akTop, 10, PrevEditText);
  EditText.Tag := YMAX_TAG;
  EditText.OnChange := @SetText;
  FText[YMAX_TAG] := EditText;
  FLabel[YMAX_TAG] := LabelText;

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

procedure TChartOptionsView.EnterView();
begin

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
