unit chart_options_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, chart_options, executor,
  epidatafiles;

type

  TChartOptionEdit  = (cbT, cbF, cbXT, cbYT, cbC, cbE, cbR, cbnXMin, cbnXMax, cbnYMin, cbnYMax);
//  TVarModelTypes    = (mtNoVars, mtScatter, mtFBarChart, mtEpicurve, mtHistogram, mtOther);
  TMinMaxType       = (mmtXMin, mmtXMax, mmtYMin, mmtYMax);
  TMinMaxTypes      = set of TMinMaxType;

  { TChartOptionsModel }
  TChartOptionsModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FTitle:    UTF8String;
    FFootnote: UTF8String;
    FXTitle:   UTF8String;
    FYTitle:   UTF8String;
    FColors:   UTF8String;
    FExportName: UTF8String;
    FExportReplace: Boolean;
    FXMin, FXMax, FYMin, FYMax: UTF8String;
    FUseX, FUseY,
    FXDate,FYDate: Boolean;
    FMinMax: TMinMaxTypes;
    procedure SetTitle(AValue: UTF8String);
    procedure SetFootnote(AValue: UTF8String);
    procedure SetXTitle(AValue: UTF8String);
    procedure SetYTitle(AValue: UTF8String);
    procedure SetColors(AValue: UTF8String);
    procedure SetExportName(AValue: UTF8String);
    procedure SetExportReplace(AValue: Boolean);
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
    property XDate: Boolean read FXDate write FXDate;
    property YDate: Boolean read FYDate write FYDate;
    property UseX: Boolean read FUseX write FUseX;
    property UseY: Boolean read FUseY write FUseY;
    property MinMax: TMinMaxTypes read FMinMax write FMinMax;
    property ExportName: UTF8String read FExportName write SetExportName;
    property ExportReplace: Boolean read FExportReplace write SetExportReplace;
  end;


implementation

uses
  LazUTF8;

{ TChartOptionsModel }

constructor TChartOptionsModel.Create();
begin
  XDate  := false;
  YDate  := false;
  UseX   := false;
  UseY   := false;
  MinMax := [];
  ExportName := 'graph.jpg';
  ExportReplace := false;
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

procedure TChartOptionsModel.SetExportName(AValue: UTF8String);
begin
  if (AValue = FExportName) then exit;
  FExportName := Avalue
end;

procedure TChartOptionsModel.SetExportReplace(AValue: Boolean);
begin
  if (AValue = FExportReplace) then exit;
  FExportReplace := Avalue
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
  if (FExportName <> '') then
      result += ' !e:="' + FExportName + '"';
  if (FExportReplace) then
      result += ' !replace'
end;

function TChartOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
