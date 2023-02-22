unit barchart_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epicustombase;

type

  TBarchartDialogVariable = (tvX, tvY, tvW);

  { TBarchartDialogVariableModel }

  TBarchartDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FWVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; barchartVariable: TBarchartDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(barchartVariable: TBarchartDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
  end;

implementation

{ TBarchartDialgoModel }

procedure TBarchartDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TBarchartDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TBarchartDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

function TBarchartDialogVariableModel.IsUsed(Field: TEpiField;
  barchartVariable: TBarchartDialogVariable): boolean;
begin
  result := (not (barchartVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (barchartVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (barchartVariable = tvW)) and (Field = FWVariable));
end;

function TBarchartDialogVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;

end;

function TBarchartDialogVariableModel.IsDefined(): boolean;
begin
  result :=
    Assigned(FXVariable);
end;

constructor TBarchartDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TBarchartDialogVariableModel.GetComboFields(
  barchartVariable: TBarchartDialogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, barchartVariable)) then
      Result.AddItem(Field);
end;

end.
