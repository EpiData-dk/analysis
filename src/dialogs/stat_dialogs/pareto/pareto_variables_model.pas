unit pareto_variables_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles;

type

  TParetoStatDiaglogVariable = (tvX, tvBy);

  { TParetoStatVariableModel }

  TParetoStatVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FByVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetByVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; TableVariable: TParetoStatDiaglogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(TableVariable: TParetoStatDiaglogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property ByVariable: TEpiField read FByVariable write SetByVariable;
  end;


implementation

uses
  epidatafilestypes;

{ TParetoStatVariableModel }

procedure TParetoStatVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TParetoStatVariableModel.SetByVariable(AValue: TEpiField);
begin
  if FByVariable = AValue then Exit;
  FByVariable := AValue;
end;

function TParetoStatVariableModel.IsUsed(Field: TEpiField;
  TableVariable: TParetoStatDiaglogVariable): boolean;
begin
  result := (not (TableVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (TableVariable = tvBy)) and (Field = FByVariable));
end;

function TParetoStatVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FByVariable) then
      result += ' !by:=' + FByVariable.Name;
end;

function TParetoStatVariableModel.IsDefined(): boolean;
begin
  Result := Assigned(FXVariable);
end;

constructor TParetoStatVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TParetoStatVariableModel.GetComboFields(
  TableVariable: TParetoStatDiaglogVariable): TEpiFields;
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
