unit fbarchart_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epicustombase;

type

  TFBarChartStatDialogVariable = (tvX, tvY, tvW);

  { TFBarChartStatDialogVariableModel }

  TFBarChartStatDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FWVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; barchartVariable: TFBarChartStatDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(barchartVariable: TFBarChartStatDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
  end;

implementation

{ TFBarChartStatDialgoModel }

procedure TFBarChartStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TFBarChartStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TFBarChartStatDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

function TFBarChartStatDialogVariableModel.IsUsed(Field: TEpiField;
  barchartVariable: TFBarChartStatDialogVariable): boolean;
begin
  result := (not (barchartVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (barchartVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (barchartVariable = tvW)) and (Field = FWVariable));
end;

function TFBarChartStatDialogVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;

end;

function TFBarChartStatDialogVariableModel.IsDefined(): boolean;
begin
  result :=
    Assigned(FXVariable);
end;

constructor TFBarChartStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TFBarChartStatDialogVariableModel.GetComboFields(
  barchartVariable: TFBarChartStatDialogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, barchartVariable)) then
      Result.AddItem(Field);
end;

end.
