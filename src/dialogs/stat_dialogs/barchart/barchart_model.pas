unit barchart_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epicustombase;

type

  TBarchartStatDialogVariable = (tvX, tvY, tvW);

  { TBarchartStatDialogVariableModel }

  TBarchartStatDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    FWVariable: TEpiField;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; barchartVariable: TBarchartStatDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(barchartVariable: TBarchartStatDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
  end;

implementation

{ TBarchartStatDialgoModel }

procedure TBarchartStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TBarchartStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure TBarchartStatDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

function TBarchartStatDialogVariableModel.IsUsed(Field: TEpiField;
  barchartVariable: TBarchartStatDialogVariable): boolean;
begin
  result := (not (barchartVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (barchartVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (barchartVariable = tvW)) and (Field = FWVariable));
end;

function TBarchartStatDialogVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if Assigned(FWVariable) then
      result += ' !w := ' + FWVariable.Name;

end;

function TBarchartStatDialogVariableModel.IsDefined(): boolean;
begin
  result :=
    Assigned(FXVariable);
end;

constructor TBarchartStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TBarchartStatDialogVariableModel.GetComboFields(
  barchartVariable: TBarchartStatDialogVariable): TEpiFields;
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
