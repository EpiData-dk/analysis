unit chart_options_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, chart_options, executor,
  epidatafiles;

type

  TChartOptionEdit = (cbT, cbF, cbXT, cbYT, cbC, cbnXMin, cbnXMax, cbnYMin, cbnYMax);
  TChartMinMaxDate = (cbdXMin, cbdXMax, cbdYMin, cbdYMax);
  TVarModelTypes   = (mtNoVars, mtScatter, mtFBarChart, mtEpicurve, mtHistogram, mtOther);

  { TChartOptionsModel }
  TChartOptionsModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FTitle:    UTF8String;
    FFootnote: UTF8String;
    FXTitle:   UTF8String;
    FYTitle:   UTF8String;
    FColors:   UTF8String;
    FXMin, FXMax, FYMin, FYMax: UTF8String;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FMinMax: Integer;   // lower bytes refer to ymax, ymin, xmax, xmin
    procedure SetTitle(AValue: UTF8String);
    procedure SetFootnote(AValue: UTF8String);
    procedure SetXTitle(AValue: UTF8String);
    procedure SetYTitle(AValue: UTF8String);
    procedure SetColors(AValue: UTF8String);
    procedure SetXVar(AValue: TEpiField);
    procedure SetYVar(AValue: TEpiField);
  public
    constructor Create();
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    property Title: UTF8String read FTitle write SetTitle;
    property Footnote: UTF8String read FFootnote write SetFootnote;
    property XTitle: UTF8String read FXTitle write SetXTitle;
    property YTitle: UTF8String read FYTitle write SetYTitle;
    property Colors: UTF8String read FColors write SetColors;
    property XMin: UTF8String read FXMin write FXMin;
    property XMax: UTF8String read FXMax write FXMax;
    property YMin: UTF8String read FYMin write FYMin;
    property YMax: UTF8String read FYMax write FYMax;
    property XVariable: TEpiField read FXVariable write SetXVar;
    property YVariable: TEpiField read FYVariable write SetYVar;
    property MinMax: Integer read FMinMax write FMinMax;
  end;


implementation

uses
  LazUTF8;

{ TChartOptionsModel }

constructor TChartOptionsModel.Create();
begin
  FXVariable := nil;
  FYVariable := nil;
  FMinMax    := 0;
end;

procedure TChartOptionsModel.SetTitle(AValue: UTF8String);
begin
  if (AValue = FTitle) then exit;
  FTitle := Avalue
end;

procedure TChartOptionsModel.SetFootnote(AValue: UTF8String);
begin
  if (AValue = FFootnote) then exit;
  FFootnote := Avalue
end;

procedure TChartOptionsModel.SetXTitle(AValue: UTF8String);
begin
  if (AValue = FXTitle) then exit;
  FXTitle := Avalue
end;

procedure TChartOptionsModel.SetYTitle(AValue: UTF8String);
begin
  if (AValue = FYTitle) then exit;
  FYTitle := Avalue
end;

procedure TChartOptionsModel.SetColors(AValue: UTF8String);
begin
  if (AValue = FColors) then exit;
  FColors := Avalue
end;

procedure TChartOptionsModel.SetXVar(AValue: TEpiField);
begin
  if (AValue = FXVariable) then exit;
  FXVariable := AValue;
end;

procedure TChartOptionsModel.SetYVar(AValue: TEpiField);
begin
  if (AValue = FYVariable) then exit;
  FYVariable := AValue;
end;

function TChartOptionsModel.GenerateScript(): UTF8String;

  function rangeFmt(Value: UTF8String): UTF8String;
  var
    i: Integer;
  begin
    if (tryStrToInt(Value, i)) then
      Result := Value
    else
      Result := 'createDate("' + Value + '")';
  end;
begin
  result := '';
  if (FTitle <> '') then
    result += ' !ti:="' + FTitle + '"';
  if (FFootnote <> '') then
    result += ' !fn:="' + FFootnote + '"';
  if (FXTitle <> '') then
    result += ' !xt:="' + FXTitle + '"';
  if (FYTitle <> '') then
    result += ' !yt:="' + FYTitle + '"';
  if (FColors <> '') then
    result += ' !colors:="' + FColors + '"';
  if (FXMin <> '') then
    result += ' !xmin:=' + rangeFmt(FXMin);
  if (FXMax <> '') then
    result += ' !xmax:=' + rangeFmt(FXMax);
  if (FYMin <> '') then
    result += ' !ymin:=' + rangeFmt(FYMin);
  if (FYMax <> '') then
    result += ' !ymax:=' + rangeFmt(FYMax);
end;

function TChartOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
