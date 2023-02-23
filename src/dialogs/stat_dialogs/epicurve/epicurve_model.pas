unit epicurve_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epicustombase, chart_options_model;

type

  TEpicurveDialogVariable = (tvX, tvY);

  { TEpicurveDialogVariableModel }

  TEpicurveDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FWVariable: TEpiField;
    FChartOptionsModel: TChartOptionsModel;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; EpicurveVariable: TEpicurveDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(EpicurveVariable: TEpicurveDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
    property ChartOptions: TChartOptionsModel read FChartOptionsModel write FChartOptionsModel;
  end;

implementation

uses
  epidatafilestypes;

{ TEpicurveDialgoModel }

procedure TEpicurveDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
  FChartOptionsModel.UseX := (AValue <> nil);
  if (AValue <> nil) then
    FChartOptionsModel.XDate := AValue.FieldType in DateFieldTypes;
end;

procedure TEpicurveDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TEpicurveDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

function TEpicurveDialogVariableModel.IsUsed(Field: TEpiField;
  EpicurveVariable: TEpicurveDialogVariable): boolean;
begin
  result := (not (EpicurveVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (EpicurveVariable = tvY)) and (Field = FYVariable));
end;

function TEpicurveDialogVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

end;

function TEpicurveDialogVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable);
end;

constructor TEpicurveDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TEpicurveDialogVariableModel.GetComboFields(
  EpicurveVariable: TEpicurveDialogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, EpicurveVariable)) then
      Result.AddItem(Field);
end;

end.
