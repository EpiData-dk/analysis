unit survival_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epidatafilestypes,epivaluelabels, options_utils, epicustombase;

type

  TSurvivalStatDialogVariable = (tvX, tvT, tvT2, tvBy, tvW);

  { TSurvivalStatDialogVariableModel }

  TSurvivalStatDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;    // Outcome
    FTVariable: TEpiField;    // Time
    FT2Variable: TEpiField;    // Time 2
    FWVariable: TEpiField;
    FByVariable: TEpiField;
    FFailure: UTF8String;
//    FFailureType: TEpiFieldType;
    FRefStratum: UTF8String;
//    FRefStratumType: TEpiFieldType;
    FOutcomeValues: TStringList;
    FStrataValues:  TStringList;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetTVariable(AValue: TEpiField);
    procedure SetT2Variable(AValue: TEpiField);
    procedure SetByVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    procedure SetFailure(AValue: UTF8String);
    procedure SetOutcomeValues(Field: TEpiField);
    procedure SetStrataValues(Field: TEpiField);
    procedure SetRefStratum(AValue: UTF8String);
    function IsUsed(Field: TEpiField; SurvivalVariable: TSurvivalStatDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(SurvivalVariable: TSurvivalStatDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property TVariable: TEpiField read FTVariable write SetTVariable;
    property T2Variable: TEpiField read FT2Variable write SetT2Variable;
    property ByVariable: TEpiField read FByVariable write SetByVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
    property Failure: UTF8String read FFailure write SetFailure;
//    property FailureType: TEpiFieldType read FFailureType write FFailureType;
    property OutcomeValues: TStringList read FOutcomeValues;
    property StrataValues: TStringList read FStrataValues;
    property RefStratum: UTF8String read FRefStratum write SetRefStratum;
  end;

implementation

{ TSurvivalStatDialgoModel }

procedure TSurvivalStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
  SetOutcomeValues(FXVariable);
end;

procedure TSurvivalStatDialogVariableModel.SetTVariable(AValue: TEpiField);
begin
  if FTVariable = AValue then Exit;
  FTVariable := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetT2Variable(AValue: TEpiField);
begin
  if FT2Variable = AValue then Exit;
  FT2Variable := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetByVariable(AValue: TEpiField);
begin
  if FByVariable = AValue then Exit;
  FByVariable := AValue;
  SetStrataValues(FByVariable);
end;

procedure TSurvivalStatDialogVariableModel.SetOutcomeValues(Field: TEpiField);
begin
  FOutcomeValues.Clear;
  if (Field = nil) then
    exit;
  FOutcomeValues := GetFieldValues(Field);
end;

procedure TSurvivalStatDialogVariableModel.SetFailure(AValue: UTF8String);
begin
  if FFailure = AValue then Exit;
  FFailure := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetStrataValues(Field: TEpiField);
begin
  FStrataValues.Clear;
  if (Field = nil) then
    exit;
  FStrataValues := GetFieldValues(Field);
end;

procedure TSurvivalStatDialogVariableModel.SetRefStratum(AValue: UTF8String);
begin
  if FRefStratum = AValue then Exit;
  FRefStratum := AValue;
end;

function TSurvivalStatDialogVariableModel.IsUsed(Field: TEpiField;
  SurvivalVariable: TSurvivalStatDialogVariable): boolean;
begin
  result := (not (SurvivalVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (SurvivalVariable = tvT)) and (Field = FTVariable));
  result := result or ((not (SurvivalVariable = tvT2)) and (Field = FT2Variable));
  result := result or ((not (SurvivalVariable = tvBy)) and (Field = FByVariable));
  result := result or ((not (SurvivalVariable = tvW)) and (Field = FWVariable));
end;

function TSurvivalStatDialogVariableModel.GenerateScript(): UTF8String;
var
p:   Integer;
begin
  result := FXVariable.Name;

  if Assigned(FTVariable) then
    result += ' ' + FTVariable.Name;

  if Assigned(FT2Variable) then
    result += ' ' + FT2Variable.Name;

  if ((FFailure <> '0') and (FFailure <> '')) then
    begin
      p := Pos('=',FFailure) - 1;
      if (p < 0) then
        p := length(FFailure);
      if (FXVariable.FieldType = ftInteger) then
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

function TSurvivalStatDialogVariableModel.IsDefined(): boolean;
begin
  result :=
    (Assigned(FXVariable) and (FFailure <> '')) and
    (((Assigned(FTVariable) and
     (FTVariable.FieldType in [ftInteger, ftString, ftUpperString]))
     ) or
     ((Assigned(FTVariable) and (FTVariable.FieldType in DateFieldTypes)) and
      (Assigned(FT2Variable) and (FT2Variable.FieldType in DateFieldTypes))
    ));
end;

constructor TSurvivalStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
  FOutcomeValues := TStringList.Create;
  FStrataValues  := TStringList.Create;
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
