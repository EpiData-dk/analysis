unit epicurve_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epicustombase;

type

  TEpicurveStatDialogVariable = (tvX, tvY);

  { TEpicurveStatDialogVariableModel }

  TEpicurveStatDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FWVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; EpicurveVariable: TEpicurveStatDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(EpicurveVariable: TEpicurveStatDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
  end;

implementation

{ TEpicurveStatDialgoModel }

procedure TEpicurveStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TEpicurveStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TEpicurveStatDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

function TEpicurveStatDialogVariableModel.IsUsed(Field: TEpiField;
  EpicurveVariable: TEpicurveStatDialogVariable): boolean;
begin
  result := (not (EpicurveVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (EpicurveVariable = tvY)) and (Field = FYVariable));
end;

function TEpicurveStatDialogVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

end;

function TEpicurveStatDialogVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable);
end;

constructor TEpicurveStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TEpicurveStatDialogVariableModel.GetComboFields(
  EpicurveVariable: TEpicurveStatDialogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, EpicurveVariable)) then
      Result.AddItem(Field);
end;

end.
