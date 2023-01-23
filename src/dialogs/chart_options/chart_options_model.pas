unit chart_options_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, chart_options, executor,
  scatter_variables_model, barchart_model, epicurve_model, histogram_model,
  epidatafiles;

type

  TChartOptionEdit = (cbT, cbF, cbXT, cbYT, cbC, cbnXMin, cbnXMax, cbnYMin, cbnYMax);
  TChartMinMaxDate = (cbdXMin, cbdXMax, cbdYMin, cbdYMax);
  TVarModelTypes   = (mtNoVars, mtScatter, mtBarChart, mtEpicurve, mtHistogram, mtOther);

  { TChartOptionsModel }
  TChartOptionsModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FTitle:    UTF8String;
    FFootnote: UTF8String;
    FXTitle:   UTF8String;
    FYTitle:   UTF8String;
    FColors:   UTF8String;
    FVarModelType: Integer;
    FXMin, FXMax, FYMin, FYMax: UTF8String;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FScatterModel: TScatterStatVariableModel;
    FBarchartModel: TBarchartStatDialogVariableModel;
    FEpicurveModel: TEpicurveStatDialogVariableModel;
    FHistogramModel: THistogramStatDialogVariableModel;
    FMinMax: Integer;   // lower bytes refer to ymax, ymin, xmax, xmin
    procedure SetTitle(AValue: UTF8String);
    procedure SetFootnote(AValue: UTF8String);
    procedure SetXTitle(AValue: UTF8String);
    procedure SetYTitle(AValue: UTF8String);
    procedure SetColors(AValue: UTF8String);

  public
    constructor Create(Executor: TExecutor; Flags: Integer = 0);
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
    procedure GetVars;
    procedure SetVariableModel(AValue: TScatterStatVariableModel);
    procedure SetVariableModel(AValue: TBarchartStatDialogVariableModel);
    procedure SetVariableModel(AValue: TEpicurveStatDialogVariableModel);
    procedure SetVariableModel(AValue: THistogramStatDialogVariableModel);
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
    property XVariable: TEpiField read FXVariable;
    property YVariable: TEpiField read FYVariable;
    property MinMax: Integer read FMinMax write FMinMax;
  end;


implementation

uses
  LazUTF8;

const
  GScatter   = Ord(mtScatter);
  GBarchart  = Ord(mtBarchart);
  GEpicurve  = Ord(mtEpicurve);
  GHistogram = Ord(mtHistogram);
  GNoVars    = Ord(mtNoVars);

{ TChartOptionsModel }

constructor TChartOptionsModel.Create(Executor: TExecutor; Flags: Integer = 0);
begin
  FExecutor  := Executor;
  FMinMax    := Flags;
  FVarModelType := GNoVars;
end;

procedure TChartOptionsModel.SetVariableModel(AValue: TScatterStatVariableModel);
begin
  if (FScatterModel = AValue) then
    exit;
  FScatterModel := AValue;
  FVarModelType := GScatter;
end;

procedure TChartOptionsModel.SetVariableModel(AValue: TBarchartStatDialogVariableModel);
begin
  if (FBarchartModel = AValue) then
    exit;
  FBarchartModel := AValue;
  FVarModelType := GBarchart;
end;

procedure TChartOptionsModel.SetVariableModel(AValue: TEpicurveStatDialogVariableModel);
begin
  if (FEpicurveModel = AValue) then
    exit;
  FEpicurveModel := AValue;
  FVarModelType := GEpicurve;
end;

procedure TChartOptionsModel.SetVariableModel(AValue: THistogramStatDialogVariableModel);
begin
  if (FHistogramModel = AValue) then
    exit;
  FHistogramModel := AValue;
  FVarModelType := GHistogram;
end;

procedure TChartOptionsModel.GetVars;
begin
  case FVarModelType of
    GNoVars:
      exit;
    GScatter:
      begin
        FXVariable := FScatterModel.XVariable;
        FYVariable := FSCatterModel.YVariable;
      end;
    GBarchart:
      begin
        FXVariable := FBarchartModel.XVariable;
      end;
    GEpicurve:
      begin
        FXVariable := FEpicurveModel.XVariable;
      end;
    GHistogram:
      begin
        FXVariable := FHistogramModel.XVariable;
      end;
  end;
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
