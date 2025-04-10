unit regress_variables_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles;


type

  TRegressType = (rtSimple, rtLinear, rtPolynomial, rtLogistic);
  TRegressVariable = (rvX, rvY, rvF);
  VariableSelectState = (vssNone, vssMultiple, vssAll);

  { TRegressVariableModel }

  TRegressVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FRegressType: TRegressType;
    FDegree: Integer;
    FYVariable: TEpiField;
    FXVariable: TEpiField;
    FFitVariable: TEpiField;
    FListFields: TEpiFields;
    FSelectedFields: TBits;
    procedure SetRegressType(AValue: TRegressType);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetXVariable(AValue: TEpiField);
    procedure SetFitVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; Variable: TRegressVariable): boolean;
    function GetSelectVariable(Index: Integer): boolean;
    procedure SetSelectVariable(Index: Integer; AValue: boolean);
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(RegressVariable: TRegressVariable): TEpiFields;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property FitVariable: TEpiField read FFitVariable write SetFitVariable;
    property RegressType: TRegressType read FRegressType write SetRegressType;
    property Degree: Integer read FDegree write FDegree;
    function GetListFields(): TEpiFields;
    function GetSelectState: VariableSelectState;
    property SelectVariable[Index: Integer]: boolean read GetSelectVariable write SetSelectVariable;
  end;

implementation

uses
  LazUTF8;

{ TRegressVariableModel }

procedure TRegressVariableModel.SetRegressType(AValue: TRegressType);
begin
  if FRegressType = AValue then exit;
  FRegressType := AValue;
end;

procedure TRegressVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TRegressVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TRegressVariableModel.SetFitVariable(AValue: TEpiField);
begin
  if FFitVariable = AValue then Exit;
  FFitVariable := AValue;
end;

function TRegressVariableModel.IsUsed(Field: TEpiField;
  Variable: TRegressVariable): boolean;
begin
  result := (not (Variable = rvY)) and (Field = FYVariable);
  result := result or ((not (Variable = rvX)) and (Field = FXVariable));
  result := result or ((not (Variable = rvF)) and (Field = FFitVariable));
end;

function TRegressVariableModel.GetSelectVariable(Index: Integer): boolean;
begin
  Result := FSelectedFields.Bits[Index];
end;

procedure TRegressVariableModel.SetSelectVariable(Index: Integer; AValue: boolean);
begin
  FSelectedFields.Bits[Index] := AValue;
end;

function TRegressVariableModel.GenerateScript(): UTF8String;
var
  i: Integer;
begin
  result := '';
  result := FYVariable.Name;
  if Assigned(FXVariable) then
    result += ' ' + FXVariable.Name
  else
    for i := 0 to FListFields.Count - 1 do
      if (FSelectedFields.Bits[i]) then
        result += ' ' + FListFields[i].Name;

  case FRegressType of
    rtPolynomial: result += ' !poly:=' + FDegree.ToString;
    rtLogistic:   result += ' !logit';
  end;

  if Assigned(FFitVariable) then
    result += ' !fit:=' + FFitVariable.Name;

  result := UTF8Trim(result);
end;

function TRegressVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FYVariable);
  if (not Assigned(FXVariable)) then
    result := result and (FSelectedFields.FindFirstBit(true) > -1);
end;

constructor TRegressVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TRegressVariableModel.GetListFields(): TEpiFields;
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

function TRegressVariableModel.GetSelectState: VariableSelectState;
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

function TRegressVariableModel.GetComboFields(
  RegressVariable: TRegressVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, RegressVariable)) then
      Result.AddItem(Field);
end;

end.

