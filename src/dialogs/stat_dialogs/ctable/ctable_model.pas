unit ctable_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution;

type

  TCtableStatDiaglogVariable = (tvX, tvY, tvZ, tvW, tvBy1, tvBy2);

  { TCtableStatDialogVariableModel }

  TCtableStatDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FZVariable: TEpiField;
    FWVariable: TEpiField;
    FByVariable1: TEpiField;
    FByVariable2: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetZVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    procedure SetByVariable1(AValue: TEpiField);
    procedure SetByVariable2(AValue: TEpiField);
    function IsUsed(Field: TEpiField; TableVariable: TCtableStatDiaglogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(TableVariable: TCtableStatDiaglogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property ZVariable: TEpiField read FYVariable write SetZVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
    property ByVariable1: TEpiField read FByVariable1 write SetByVariable1;
    property ByVariable2: TEpiField read FByVariable2 write SetByVariable2;
  end;

implementation

{ TCtableStatDialogModel }

procedure TCtableStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TCtableStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TCtableStatDialogVariableModel.SetZVariable(AValue: TEpiField);
begin
  if FZVariable = AValue then Exit;
  FZVariable := AValue;
end;

procedure TCtableStatDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

procedure TCtableStatDialogVariableModel.SetByVariable1(AValue: TEpiField);
begin
  if FByVariable1 = AValue then Exit;
  FByVariable1 := AValue;
end;

procedure TCtableStatDialogVariableModel.SetByVariable2(AValue: TEpiField);
begin
  if FByVariable2 = AValue then Exit;
  FByVariable2 := AValue;
end;

function TCtableStatDialogVariableModel.IsUsed(Field: TEpiField;
  TableVariable: TCtableStatDiaglogVariable): boolean;
begin
  result := (not (TableVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (TableVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (TableVariable = tvZ)) and (Field = FZVariable));
  result := result or ((not (TableVariable = tvW)) and (Field = FWVariable));
  result := result or ((not (TableVariable = tvBy1)) and (Field = FByVariable1));
  result := result or ((not (TableVariable = tvBy2)) and (Field = FByVariable2));
end;

function TCtableStatDialogVariableModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if Assigned(FZVariable) then
      result += '-' + FZVariable.Name;

  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;

  if Assigned(FByVariable1) then
    result += ' !by := ' + FByVariable1.Name;

  if Assigned(FByVariable2) then
    result += ' !by := ' + FByVariable2.Name;
end;

function TCtableStatDialogVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable) and Assigned(FYVariable);
end;

constructor TCtableStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TCtableStatDialogVariableModel.GetComboFields(
  TableVariable: TCtableStatDiaglogVariable): TEpiFields;
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
