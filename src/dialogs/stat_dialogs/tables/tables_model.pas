unit tables_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution;

type

  TTableStatDialogVariable = (tvX, tvY, tvBy1, tvBy2);

  { TTableStatDialogVariableModel }

  TTableStatDialogVariableModel = class(IStatDialogModel)
  private
    FByVariable1: TEpiField;
    FByVariable2: TEpiField;
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    procedure SetByVariable1(AValue: TEpiField);
    procedure SetByVariable2(AValue: TEpiField);
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; TableVariable: TTableStatDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(TableVariable: TTableStatDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property ByVariable1: TEpiField read FByVariable1 write SetByVariable1;
    property ByVariable2: TEpiField read FByVariable2 write SetByVariable2;
  end;

implementation

{ TTableStatDialgoModel }

procedure TTableStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TTableStatDialogVariableModel.SetByVariable1(AValue: TEpiField);
begin
  if FByVariable1 = AValue then Exit;
  FByVariable1 := AValue;
end;

procedure TTableStatDialogVariableModel.SetByVariable2(AValue: TEpiField);
begin
  if FByVariable2 = AValue then Exit;
  FByVariable2 := AValue;
end;

procedure TTableStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

function TTableStatDialogVariableModel.IsUsed(Field: TEpiField;
  TableVariable: TTableStatDialogVariable): boolean;
begin
  result := (not (TableVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (TableVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (TableVariable = tvBy1)) and (Field = FByVariable1));
  result := result or ((not (TableVariable = tvBy2)) and (Field = FByVariable2));
end;

function TTableStatDialogVariableModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if Assigned(FByVariable1) then
    result += ' !by := ' + FByVariable1.Name;

  if Assigned(FByVariable2) then
    result += ' !by := ' + FByVariable2.Name;
end;

function TTableStatDialogVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable) and Assigned(FYVariable);
end;

constructor TTableStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TTableStatDialogVariableModel.GetComboFields(
  TableVariable: TTableStatDialogVariable): TEpiFields;
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

