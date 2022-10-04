unit survival_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epidatafilestypes,epivaluelabels, outputcreator, freq, epicustombase;

type

  TSurvivalStatDialogVariable = (tvX, tvT, tvT2, tvW, tvBy);

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
    FFailureType: TEpiFieldType;
    FOutcomeValues: TStringList;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetTVariable(AValue: TEpiField);
    procedure SetT2Variable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    procedure SetByVariable(AValue: TEpiField);
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
    property TVariable: TEpiField read FTVariable write SetTVariable;
    property T2Variable: TEpiField read FT2Variable write SetT2Variable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
    property ByVariable: TEpiField read FByVariable write SetByVariable;
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
end;

procedure TSurvivalStatDialogVariableModel.SetFailure(
  AValue: UTF8String);
begin
  if FFailure = AValue then Exit;
  FFailure := AValue;
end;

procedure TSurvivalStatDialogVariableModel.SetOutcomeValues(Field: TEpiField);
var
  i:             Integer;
  l:             TEpiValueLabelSet;
  DF:            TEpiDataFile;
  F:             TFreqCommand;
  O:             TEpiReferenceMap;
  CategV:        TEpiField;
  OutcomeFreq:   TFreqDatafile;
  AVar:          TStringList;
  OutputCreator: TOutputCreator;
begin
  FOutcomeValues.Clear;
  if (Field = nil) then
    exit;

// check for value labels
  l := Field.ValueLabelSet;
  if (l <> nil) then
    begin
      for i := 0 to l.Count - 1 do
        FOutcomeValues.Add(l.ValueLabels[i].ValueAsString);
      exit;
    end;

// get frequencies to identify possible values
  F :=           TFreqCommand.Create(FExecutor, OutputCreator);
  AVar :=        TStringList.Create;
  AVar.Add(Field.Name);
  DF :=          FExecutor.PrepareDatafile(AVar, AVar);
  OutcomeFreq := F.CalcFreq(DF, Field.Name, O);
  CategV :=      OutcomeFreq.Categ;
  for i:= 0 to OutcomeFreq.Size - 1 do
    FOutcomeValues.Add(CategV.AsString[i]);
  DF.Free;
  F.Free;
  OutcomeFreq.Free;
  AVar.Free;
end;

function TSurvivalStatDialogVariableModel.IsUsed(Field: TEpiField;
  SurvivalVariable: TSurvivalStatDialogVariable): boolean;
begin
  result := (not (SurvivalVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (SurvivalVariable = tvT)) and (Field = FTVariable));
  result := result or ((not (SurvivalVariable = tvT2)) and (Field = FT2Variable));
  result := result or ((not (SurvivalVariable = tvW)) and (Field = FWVariable));
  result := result or ((not (SurvivalVariable = tvBy)) and (Field = FByVariable));
end;

function TSurvivalStatDialogVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FTVariable) then
    result += ' ' + FTVariable.Name;

  if Assigned(FT2Variable) then
    result += ' ' + FT2Variable.Name;

  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;

  if Assigned(FByVariable) then
    result += ' !by:=' + FByVariable.Name;

  if ((FFailure <> '0') and (FFailure <> '')) then
    if (FFailureType = ftInteger) then
      Result += ' !o:=' + FFailure
    else
      Result += ' !o:="' + FFailure + '"';

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
