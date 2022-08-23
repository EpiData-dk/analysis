unit histogram_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles, stat_dialog_contribution,
  epidatafilestypes,epivaluelabels, outputcreator, freq, epicustombase;

type

  THistogramStatDialogVariable = (tvX, tvY, tvW);

  { THistogramStatDialogVariableModel }

  THistogramStatDialogVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FXVariable: TEpiField;    // Time
    FYVariable: TEpiField;    // Strata
    FWVariable: TEpiField;
    FEpicurve: Boolean;
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
    procedure SetWVariable(AValue: TEpiField);
    function IsUsed(Field: TEpiField; histogramVariable: THistogramStatDialogVariable): boolean;
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(histogramVariable: THistogramStatDialogVariable): TEpiFields;
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property WVariable: TEpiField read FWVariable write SetWVariable;
    property Epicurve: boolean read FEpicurve write FEpicurve;
  end;

implementation

{ THistogramStatDialgoModel }

procedure THistogramStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure THistogramStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

procedure THistogramStatDialogVariableModel.SetWVariable(AValue: TEpiField);
begin
  if FWVariable = AValue then Exit;
  FWVariable := AValue;
end;

function THistogramStatDialogVariableModel.IsUsed(Field: TEpiField;
  histogramVariable: THistogramStatDialogVariable): boolean;
begin
  result := (not (histogramVariable = tvX)) and (Field = FXVariable);
  result := result or ((not (histogramVariable = tvY)) and (Field = FYVariable));
  result := result or ((not (histogramVariable = tvW)) and (Field = FWVariable));
end;

function THistogramStatDialogVariableModel.GenerateScript(): UTF8String;
begin
  result := FXVariable.Name;

  if Assigned(FYVariable) then
    result += ' ' + FYVariable.Name;

  if (Assigned(FWVariable) and (not FEpicurve)) then
      result += ' !w := ' + FWVariable.Name;

end;

function THistogramStatDialogVariableModel.IsDefined(): boolean;
begin
  result :=
    Assigned(FXVariable);
end;

constructor THistogramStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
  FEpicurve := false;
end;

function THistogramStatDialogVariableModel.GetComboFields(
  histogramVariable: THistogramStatDialogVariable): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    if (not IsUsed(Field, histogramVariable)) then
      Result.AddItem(Field);
end;

end.
