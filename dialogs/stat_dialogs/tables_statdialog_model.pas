unit tables_statdialog_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution;

type

  { TTableStatDialogVariableModel }

  TTableStatDialogVariableModel = class(IStatDialogModel)
  private
    FByVariables: TEpiFields;
    FWeightVariable: TEpiField;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    procedure SetWeightVariable(AValue: TEpiField);
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create;
    procedure AddByVariable(Field: TEpiField);
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

constructor TTableStatDialogVariableModel.Create;
begin
  FByVariables := TEpiFields.Create(nil);
  FByVariables.Sorted := false;
end;

procedure TTableStatDialogVariableModel.AddByVariable(Field: TEpiField);
begin
  FByVariables.AddItem(Field);
end;

end.

