unit means_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, epidatafilestypes, stat_dialog_contribution;

type

  TMeansStatDiaglogVariable = (tvX, tvBy); //, tvW);

  { TMeansStatDialogVariableModel }

  TMeansStatDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
//    FWVariable: TEpiField;
    FByVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
//    procedure SetWVariable(AValue: TEpiField);
    procedure SetByVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; MeansVariable: TMeansStatDiaglogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(MeansVariable: TMeansStatDiaglogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
//    property WVariable: TEpiField read FWVariable write SetWVariable;
    property ByVariable: TEpiField read FByVariable write SetByVariable;
  end;

implementation

{ TMeansStatDialogModel }

procedure TMeansStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

{
procedure TMeansStatDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;
}

procedure TMeansStatDialogVariableModel.SetByVariable(AValue: TEpiField);
begin
  if FByVariable = AValue then Exit;
  FByVariable := AValue;
end;

function TMeansStatDialogVariableModel.IsUsed(Field: TEpiField;
  MeansVariable: TMeansStatDiaglogVariable): boolean;
begin
  result := (not (MeansVariable = tvX)) and (Field = FXVariable);
//  result := result or ((not (MeansVariable = tvW)) and (Field = FWVariable));
  result := result or ((not (MeansVariable = tvBy)) and (Field = FByVariable));
end;

function TMeansStatDialogVariableModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
begin
  result := FXVariable.Name;
{
  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;
 }
  if Assigned(FByVariable) then
    result += ' !by := ' + FByVariable.Name;

end;

function TMeansStatDialogVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable);
end;

constructor TMeansStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TMeansStatDialogVariableModel.GetComboFields(
  MeansVariable: TMeansStatDiaglogVariable): TEpiFields;
var
  Field: TEpiField;

begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, MeansVariable)) then
      if (MeansVariable = tvBy) or (Field.FieldType in [ftInteger, ftAutoInc, ftFloat]) then
        Result.AddItem(Field);
end;

end.

