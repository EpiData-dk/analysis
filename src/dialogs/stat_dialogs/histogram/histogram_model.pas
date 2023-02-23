unit histogram_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epicustombase, chart_options_model;

type

  THistogramDialogVariable = (tvX, tvY, tvW);

  { THistogramDialogVariableModel }

  THistogramDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FWVariable: TEpiField;
    FChartOptionsModel: TChartOptionsModel;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; histogramVariable: THistogramDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(histogramVariable: THistogramDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
    property ChartOptions: TChartOptionsModel read FChartOptionsModel write FChartOptionsModel;
  end;

implementation

uses
  epidatafilestypes;

{ THistogramDialgoModel }

procedure THistogramDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
  FChartOptionsModel.UseX := (AValue <> nil);
  if (AValue <> nil) then
    FChartOptionsModel.XDate := AValue.FieldType in DateFieldTypes;
end;

procedure THistogramDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure THistogramDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

function THistogramDialogVariableModel.IsUsed(Field: TEpiField;
  histogramVariable: THistogramDialogVariable): boolean;
begin
  result := (not (histogramVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (histogramVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (histogramVariable = tvW)) and (Field = FWVariable));
end;

function THistogramDialogVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;

end;

function THistogramDialogVariableModel.IsDefined(): boolean;
begin
  result :=
    Assigned(FXVariable);
end;

constructor THistogramDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function THistogramDialogVariableModel.GetComboFields(
  histogramVariable: THistogramDialogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, histogramVariable)) then
      Result.AddItem(Field);
end;

end.
