unit tables_statdialog_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution;

type

  TTableStatDiaglogVariable = (tvX, tvY, tvWeight, tvBy);

  { TTableStatDialogVariableModel }

  TTableStatDialogVariableModel = class(IStatDialogModel)
  private
    FByVariables: TEpiFields;
    FExecutor: TExecutor;
    FWeightVariable: TEpiField;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    procedure SetWeightVariable(AValue: TEpiField);
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    procedure AddByVariable(Field: TEpiField);
    function GetComboFields(TableVariable: TTableStatDiaglogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WeightVariable: TEpiField read FWeightVariable write SetWeightVariable;
  end;

implementation

{ TTableStatDialgoModel }

procedure TTableStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TTableStatDialogVariableModel.SetWeightVariable(AValue: TEpiField);
begin
  if FWeightVariable = AValue then Exit;
  FWeightVariable := AValue;
end;

procedure TTableStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

function TTableStatDialogVariableModel.IsUsed(Field: TEpiField): boolean;
begin
  result :=
    (Field = FXVariable) or
    (Field = FYVariable) or
    (Field = FWeightVariable) or
    (FByVariables.FieldExists(Field));
end;

function TTableStatDialogVariableModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if Assigned(FWeightVariable) then
    result += ' !w := ' + FWeightVariable.Name;

  for Field in FByVariables do
    result += ' !by := ' + Field.Name;
end;

function TTableStatDialogVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable);
end;

constructor TTableStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
  FByVariables := TEpiFields.Create(nil);
  FByVariables.Sorted := false;
end;

procedure TTableStatDialogVariableModel.AddByVariable(Field: TEpiField);
begin
  FByVariables.AddItem(Field);
end;

function TTableStatDialogVariableModel.GetComboFields(
  TableVariable: TTableStatDiaglogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field)) then
      Result.AddItem(Field);
end;

end.

