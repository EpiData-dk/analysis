unit freq_variable_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles;


const
  FREQ_VARIABLES_COUNT = 4;

type

  { TFreqVariableModel }

  TFreqVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FVariables: array[0..FREQ_VARIABLES_COUNT-1] of TEpiField;
    function GetVariables(const Index: Integer): TEpiField;
    procedure SetVariables(const Index: Integer; AValue: TEpiField);
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(): TEpiFields;
    property Variables[const Index: Integer]: TEpiField read GetVariables write SetVariables;
  end;

implementation

uses
  LazUTF8;

{ TFreqVariableModel }

function TFreqVariableModel.GetVariables(const Index: Integer): TEpiField;
begin
  result := FVariables[Index];
end;

procedure TFreqVariableModel.SetVariables(const Index: Integer;
  AValue: TEpiField);
begin
  if FVariables[Index] = AValue then Exit;
  FVariables[Index] := AValue;
end;

function TFreqVariableModel.GenerateScript(): UTF8String;
var
  i: Integer;
begin
  result := '';

  for i := 0 to FREQ_VARIABLES_COUNT - 1 do
    if (Assigned(FVariables[i])) then
      result := result + FVariables[i].Name + ' ';

  result := UTF8Trim(result);
end;

function TFreqVariableModel.IsDefined(): boolean;
begin
  result := Assigned(FVariables[0]);
end;

constructor TFreqVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TFreqVariableModel.GetComboFields(): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    Result.AddItem(Field);
end;

end.

