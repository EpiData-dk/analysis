unit regress_type_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution;

type

  TRegressType = (rtSimple, rtLinear, rtPolynomial, rtLogistic);
  TRegressVariable = (rvX, rvY, rvF);

  { RegressTypeModel }

  TRegressTypeModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FRegressType: TRegressType;
    FDegree: Integer;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FFitVariable: TEpiField;
    procedure SetRegressType(AValue: TRegressType);
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetFitVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; Variable: TRegressVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
    
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(RegressVariable: TRegressVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property FitVariable: TEpiField read FFitVariable write SetFitVariable;
    property RegressType: TRegressType read FRegressType write SetRegressType;
    property Degree: Integer read FDegree write FDegree;
  end;

implementation

{ TRegressTypeModel }

procedure TRegressTypeModel.SetRegressType(AValue: TRegressType);
begin
  if FRegressType = AValue then exit;
  FRegressType := AValue;
end;

procedure TRegressTypeModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TRegressTypeModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TRegressTypeModel.SetFitVariable(AValue: TEpiField);
begin
  if FFitVariable = AValue then Exit;
  FFitVariable := AValue;
end;

function TRegressTypeModel.IsUsed(Field: TEpiField;
  Variable: TRegressVariable): boolean;
begin
  result := (not (Variable = rvY)) and (Field = FYVariable);
  result := result or ((not (Variable = rvX)) and (Field = FXVariable));
  result := result or ((not (Variable = rvF)) and (Field = FFitVariable));
end;

function TRegressTypeModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
begin
  result := FYVariable.Name;
  if Assigned(FXVariable) then
    result += ' ' + FXVariable.Name
  else
    result += ' %indepvars% ';
  // type
  case FRegressType of
    rtPolynomial: result += ' !poly:=' + FDegree.ToString;
    rtLogistic:   result += ' !logit';
  end;
  if Assigned(FFitVariable) then
    result += ' !fit:=' + FFitVariable.Name;
end;

function TRegressTypeModel.IsDefined(): boolean;
begin
  result := Assigned(FYVariable);
  result := result and
         (  ((FRegressType in [rtPolynomial, rtSimple]) and
             Assigned(FXVariable)) or
            (FRegressType in [rtLinear, rtLogistic])
         );
end;

constructor TRegressTypeModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TRegressTypeModel.GetComboFields(
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
