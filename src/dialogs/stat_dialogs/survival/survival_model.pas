unit survival_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epidatafilestypes,epivaluelabels;

type

  TSurvivalStatDialogVariable = (tvX, tvY, tvW, tvBy1);

  { TSurvivalStatDialogVariableModel }

  TSurvivalStatDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;    // Outcome
    FYVariable: TEpiField;    // Time
    FWVariable: TEpiField;
    FByVariable1: TEpiField;
    FFailure: UTF8String;
    FFailureType: TEpiFieldType;
    FOutcomeValues: TStringList;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    procedure SetByVariable1(AValue: TEpiField);
    procedure SetFailure(AValue: UTF8String);
    procedure SetOutcomeValues(Field: TEpiField);
    function IsUsed(Field: TEpiField; SurvivalVariable: TSurvivalStatDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(SurvivalVariable: TSurvivalStatDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
    property ByVariable1: TEpiField read FByVariable1 write SetByVariable1;
    property Failure: UTF8String read FFailure write SetFailure;
    property FailureType: TEpiFieldType read FFailureType write FFailureType;
    property OutcomeValues: TStringList read FOutcomeValues;
  end;

implementation

{ TSurvivalStatDialgoModel }

procedure TSurvivalStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
//  FOutcomeValues.Clear;
  SetOutcomeValues(FXVariable);
end;

procedure TSurvivalStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetByVariable1(AValue: TEpiField);
begin
  if FByVariable1 = AValue then Exit;
  FByVariable1 := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetFailure(
  AValue: UTF8String);
begin
  if FFailure = AValue then Exit;
  FFailure := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetOutcomeValues(Field: TEpiField);
var
  i: Integer;
  l: TEpiValueLabelSet;
begin
  l := Field.ValueLabelSet;
  FOutcomeValues.Clear;
  if (l = nil) then
    for i := 0 to 5 do
      FOutcomeValues.Add(IntToStr(i))
  else
  for i := 0 to l.Count - 1 do
    FOutcomeValues.Add(l.ValueLabels[i].ValueAsString);
end;

function TSurvivalStatDialogVariableModel.IsUsed(Field: TEpiField;
  SurvivalVariable: TSurvivalStatDialogVariable): boolean;
begin
  result := (not (SurvivalVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (SurvivalVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (SurvivalVariable = tvW)) and (Field = FWVariable));
  result := result or ((not (SurvivalVariable = tvBy1)) and (Field = FByVariable1));
end;

function TSurvivalStatDialogVariableModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;

  if Assigned(FByVariable1) then
    result += ' !by:=' + FByVariable1.Name;

  if (FFailure <> '0') then
    if (FFailureType = ftInteger) then
      Result += ' !o:=' + FFailure
    else
      Result += ' !o:="' + FFailure + '"';

end;

function TSurvivalStatDialogVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FXVariable) and Assigned(FYVariable);
end;

constructor TSurvivalStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
  FOutcomeValues := TStringList.Create;
end;

function TSurvivalStatDialogVariableModel.GetComboFields(
  SurvivalVariable: TSurvivalStatDialogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, SurvivalVariable)) then
      Result.AddItem(Field);
end;

end.
