unit scatter_variables_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles;

type

  TScatterStatDiaglogVariable = (tvX, tvY);

  { TScatterStatVariableModel }

  TScatterStatVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; TableVariable: TScatterStatDiaglogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(TableVariable: TScatterStatDiaglogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
  end;


implementation

{ TScatterStatVariableModel }

procedure TScatterStatVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TScatterStatVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

function TScatterStatVariableModel.IsUsed(Field: TEpiField;
  TableVariable: TScatterStatDiaglogVariable): boolean;
begin
  result := (not (TableVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (TableVariable = tvY)) and (Field = FYVariable));
end;

function TScatterStatVariableModel.GenerateScript(): UTF8String;
begin
  Result := FXVariable.Name + ' ' + FYVariable.Name;
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
