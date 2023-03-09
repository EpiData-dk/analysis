unit scatter_variables_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles, chart_options_model;

type

  TScatterStatDiaglogVariable = (tvX, tvY, tVBy);

  { TScatterStatVariableModel }

  TScatterStatVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FByVariable: TEpiField;
    FChartOptionsModel: TChartOptionsModel;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetByVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; TableVariable: TScatterStatDiaglogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(TableVariable: TScatterStatDiaglogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property ByVariable: TEpiField read FByVariable write SetByVariable;
    property ChartOptions: TChartOptionsModel read FChartOptionsModel write FChartOptionsModel;
  end;


implementation

uses
  epidatafilestypes;

{ TScatterStatVariableModel }

procedure TScatterStatVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
  FChartOptionsModel.UseX := (AValue <> nil);
  if (AValue <> nil) then
    FChartOptionsModel.XDate := AValue.FieldType in DateFieldTypes;
end;

procedure TScatterStatVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
  FChartOptionsModel.UseY := (AValue <> nil);
  if (AValue <> nil) then
    FChartOptionsModel.YDate := AValue.FieldType in DateFieldTypes;
end;

procedure TScatterStatVariableModel.SetByVariable(AValue: TEpiField);
begin
  if FByVariable = AValue then Exit;
  FByVariable := AValue;
end;

function TScatterStatVariableModel.IsUsed(Field: TEpiField;
  TableVariable: TScatterStatDiaglogVariable): boolean;
begin
  result := (not (TableVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (TableVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (TableVariable = tvBy)) and (Field = FByVariable));
end;

function TScatterStatVariableModel.GenerateScript(): UTF8String;
begin
  Result := FXVariable.Name + ' ' + FYVariable.Name;
  if Assigned(FByVariable) then
    result += ' !by := ' + FByVariable.Name;
end;

function TScatterStatVariableModel.IsDefined(): boolean;
begin
  Result := Assigned(FXVariable) and Assigned(FYVariable);
end;

constructor TScatterStatVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TScatterStatVariableModel.GetComboFields(
  TableVariable: TScatterStatDiaglogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, TableVariable)) then
      Result.AddItem(Field);
end;

end.
