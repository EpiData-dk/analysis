unit describe_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution;

type

  TDescribeStatDiaglogVariable = (tvX, tvY);

  { TDescribeVariableModel }

  TDescribeVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; DialogVariable: TDescribeStatDiaglogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(DialogVariable: TDescribeStatDiaglogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
  end;

implementation

{ TDescribeModel }

procedure TDescribeVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TDescribeVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

function TDescribeVariableModel.IsUsed(Field: TEpiField;
  DialogVariable: TDescribeStatDiaglogVariable): boolean;
begin
  result := (not (DialogVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (DialogVariable = tvY)) and (Field = FYVariable));
end;

function TDescribeVariableModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += '-' + FYVariable.Name;

end;

function TDescribeVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable);
end;

constructor TDescribeVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TDescribeVariableModel.GetComboFields(
  DialogVariable: TDescribeStatDiaglogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, DialogVariable)) then
      Result.AddItem(Field);
end;

end.
