unit pareto_variables_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles;

type

  TParetoStatDiaglogVariable = (tvX, tvY);

  { TParetoStatVariableModel }

  TParetoStatVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; TableVariable: TParetoStatDiaglogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(TableVariable: TParetoStatDiaglogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
  end;


implementation

{ TParetoStatVariableModel }

procedure TParetoStatVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

function TParetoStatVariableModel.IsUsed(Field: TEpiField;
  TableVariable: TParetoStatDiaglogVariable): boolean;
begin
  result := (not (TableVariable = tvX)) and (Field = FXVariable);
end;

function TParetoStatVariableModel.GenerateScript(): UTF8String;
begin
  Result := FXVariable.Name;
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
