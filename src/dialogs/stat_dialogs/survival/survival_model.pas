unit survival_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epidatafilestypes,epivaluelabels, options_utils, epicustombase;

type

  TSurvivalDialogVariable = (svOutcome, svTime1, svTime2, svBy, svW);

  { TSurvivalDialogVariableModel }

  TSurvivalDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FOutcomeVariable: TEpiField;
    FTime1Variable: TEpiField;
    FTime2Variable: TEpiField;
    FWVariable: TEpiField;
    FByVariable: TEpiField;
    FFailure: UTF8String;
    FRefStratum: UTF8String;
    FOutcomeValues: TStringList;
    FStrataValues:  TStringList;
    procedure SetOutcomeVariable(AValue: TEpiField);
    procedure SetTime1Variable(AValue: TEpiField);
    procedure SetTime2Variable(AValue: TEpiField);
    procedure SetByVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    procedure SetFailure(AValue: UTF8String);
    procedure SetOutcomeValues(Field: TEpiField);
    procedure SetStrataValues(Field: TEpiField);
    procedure SetRefStratum(AValue: UTF8String);
    function IsUsed(Field: TEpiField; SurvivalVariable: TSurvivalDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(SurvivalVariable: TSurvivalDialogVariable): TEpiFields;
    property OutcomeVariable: TEpiField read FOutcomeVariable write SetOutcomeVariable;
    property Time1Variable: TEpiField read FTime1Variable write SetTime1Variable;
    property Time2Variable: TEpiField read FTime2Variable write SetTime2Variable;
    property ByVariable: TEpiField read FByVariable write SetByVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
    property Failure: UTF8String read FFailure write SetFailure;
    property OutcomeValues: TStringList read FOutcomeValues;
    property StrataValues: TStringList read FStrataValues;
    property RefStratum: UTF8String read FRefStratum write SetRefStratum;
  end;

implementation

{ TSurvivalStatDialgoModel }

procedure TSurvivalDialogVariableModel.SetOutcomeVariable(AValue: TEpiField);
begin
  if FOutcomeVariable = AValue then Exit;
  FOutcomeVariable := AValue;
  SetOutcomeValues(FOutcomeVariable);
end;

procedure TSurvivalDialogVariableModel.SetTime1Variable(AValue: TEpiField);
begin
  if FTime1Variable = AValue then Exit;
  FTime1Variable := AValue;
end;

procedure TSurvivalDialogVariableModel.SetTime2Variable(AValue: TEpiField);
begin
  if FTime2Variable = AValue then Exit;
  FTime2Variable := AValue;
end;

procedure TSurvivalDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

procedure TSurvivalDialogVariableModel.SetByVariable(AValue: TEpiField);
begin
  if FByVariable = AValue then Exit;
  FByVariable := AValue;
  SetStrataValues(FByVariable);
end;

procedure TSurvivalDialogVariableModel.SetOutcomeValues(Field: TEpiField);
begin
  FOutcomeValues.Clear;
  if (Field = nil) then
    exit;
  FOutcomeValues := GetFieldValues(Field);
end;

procedure TSurvivalDialogVariableModel.SetFailure(AValue: UTF8String);
begin
  if FFailure = AValue then Exit;
  FFailure := AValue;
end;

procedure TSurvivalDialogVariableModel.SetStrataValues(Field: TEpiField);
begin
  FStrataValues.Clear;
  if (Field = nil) then
    exit;
  FStrataValues := GetFieldValues(Field);
end;

procedure TSurvivalDialogVariableModel.SetRefStratum(AValue: UTF8String);
begin
  if FRefStratum = AValue then Exit;
  FRefStratum := AValue;
end;

function TSurvivalDialogVariableModel.IsUsed(Field: TEpiField;
  SurvivalVariable: TSurvivalDialogVariable): boolean;
begin
  result := (not (SurvivalVariable = svOutcome)) and (Field = FOutcomeVariable);
  result := result or ((not (SurvivalVariable = svTime1)) and (Field = FTime1Variable));
  result := result or ((not (SurvivalVariable = svTime2)) and (Field = FTime2Variable));
  result := result or ((not (SurvivalVariable = svBy)) and (Field = FByVariable));
  result := result or ((not (SurvivalVariable = svW)) and (Field = FWVariable));
end;

function TSurvivalDialogVariableModel.GenerateScript(): UTF8String;
var
p:   Integer;
begin
  result := FOutcomeVariable.Name;

  if Assigned(FTime1Variable) then
    result += ' ' + FTime1Variable.Name;

  if Assigned(FTime2Variable) then
    result += ' ' + FTime2Variable.Name;

  if ((FFailure <> '0') and (FFailure <> '')) then
    begin
      p := Pos('=',FFailure) - 1;
      if (p < 0) then
        p := length(FFailure);
      if (FOutcomeVariable.FieldType = ftInteger) then
        Result += ' !o:=' + copy(FFailure,0,p)
      else
        Result += ' !o:="' + copy(FFailure,0,p) + '"';
    end;

  if Assigned(FByVariable) then
    begin
      result += ' !by:=' + FByVariable.Name;

      if (FRefStratum <> '') then
        begin
          p := Pos('=',FRefStratum) - 1;
          if (p < 0) then
            p := length(FRefStratum);
          if (FByVariable.FieldType = ftInteger) then
            Result += ' !ref:=' + Copy(FRefStratum,0,p)
          else
            Result += ' !ref:="' + Copy(FRefStratum,0,p) + '"';
        end;
    end;
  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;

end;

function TSurvivalDialogVariableModel.IsDefined(): boolean;
begin
  result :=
    (Assigned(FOutcomeVariable) and (FFailure <> '')) and
    (((Assigned(FTime1Variable) and
     (FTime1Variable.FieldType in [ftInteger, ftString, ftUpperString]))
     ) or
     ((Assigned(FTime1Variable) and (FTime1Variable.FieldType in DateFieldTypes)) and
      (Assigned(FTime2Variable) and (FTime2Variable.FieldType in DateFieldTypes))
    ));
end;

constructor TSurvivalDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
  FOutcomeValues := TStringList.Create;
  FStrataValues  := TStringList.Create;
end;

function TSurvivalDialogVariableModel.GetComboFields(
  SurvivalVariable: TSurvivalDialogVariable): TEpiFields;
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
