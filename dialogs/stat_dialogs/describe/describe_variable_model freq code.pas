unit describe_variable_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution;

type

  TDescribeVariable = (tvX, tvY);

  { TDescribeVariableModel }

  TDescribeVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; DialogVariable: TDescribeVariable): boolean;
    FListFields: TEpiFields;
    FSelectedFields: TBits;
    function GetSelectVariable(Index: Integer): boolean;
    procedure SetSelectVariable(Index: Integer; AValue: boolean);
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(DialogVariable: TDescribeVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    function GetListFields(): TEpiFields;
    function GetSelectState: VariableSelectState;
    property SelectVariable[Index: Integer]: boolean read GetSelectVariable write SetSelectVariable;
  end;

implementation

uses
  LazUTF8;

{ TDescribeModel }

procedure TDescribeVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
  // clear all variable bits
end;

procedure TDescribeVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
  // clear all variable bits
end;

function TDescribeVariableModel.IsUsed(Field: TEpiField;
  DialogVariable: TDescribeVariable): boolean;
begin
  result := (not (DialogVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (DialogVariable = tvY)) and (Field = FYVariable));
end;

function TDescribeVariableModel.GetSelectVariable(Index: Integer): boolean;
begin
  Result := FSelectedFields.Bits[Index];
end;

procedure TDescribeVariableModel.SetSelectVariable(Index: Integer; AValue: boolean);
begin
  FSelectedFields.Bits[Index] := AValue;
  // clear FXVariable and FYVariable
end;

function TDescribeVariableModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
  i: Integer;
begin
  if Assigned(FXVariable) then
     result := FXVariable.Name;
     if Assigned(FYVariable) then
        result += '-' + FYVariable.Name;
  else
     result := '';
     for i := 0 to FListFields.Count - 1 do
        if (FSelectedFields.Bits[i]) then
           result += FListFields[i].Name + ' ';
     result := UTF8Trim(result);
  endif;
end;

function TDescribeVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable);
  result := result or (FSelectedFields.FindFirstBit(true) > -1);
end;

constructor TDescribeVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TDescribeVariableModel.GetComboFields(
  DialogVariable: TDescribeVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, DialogVariable)) then
      Result.AddItem(Field);
end;

function TDescribeVariableModel.GetListFields(): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    Result.AddItem(Field);

  FreeAndNil(FSelectedFields);
  FSelectedFields := TBits.Create(Result.Count);
  FListFields := Result;
end;

function TDescribeVariableModel.GetSelectState: VariableSelectState;
var
  AllSelected: Boolean;
  i: Integer;
begin
  Result := vssNone;

  if (FSelectedFields.FindFirstBit(true) > -1) then Result := vssMultiple;

  AllSelected := true;
  for i := 0 to FSelectedFields.Size - 1 do
    AllSelected := AllSelected and FSelectedFields.Bits[i];

  if (AllSelected) then Result := vssAll;
end;

end.
