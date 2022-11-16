unit scatter_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper, executor;

type


   { TScatterStatDialogPrimaryOptionModel }

   TScatterStatDialogPrimaryOptionModel = class(IStatDialogModel)
   private
     FExecutor: TExecutor;
     FValueLabelType: TEpiGetValueLabelType;
     FVariableLabelType: TEpiGetVariableLabelType;
     FValueLabelsDefault: Integer;
     FVariableLabelsDefault: Integer;
     FLine: Boolean;
     FPoints:  Boolean;
     procedure SetValueLabelType(AValue: TEpiGetValueLabelType);
     procedure SetVariableLabelType(AValue: TEpiGetVariableLabelType);
   public
     constructor Create(Executor: TExecutor);
     function GenerateScript(): UTF8String;
     function IsDefined(): boolean;
   public
     property VariableLabelType: TEpiGetVariableLabelType read FVariableLabelType write SetVariableLabelType;
     property ValueLabelType: TEpiGetValueLabelType read FValueLabelType write SetValueLabelType;
     property ValueLabelsDefault: Integer read FValueLabelsDefault write FValueLabelsDefault;
     property VariableLabelsDefault: Integer read FVariableLabelsDefault write FVariableLabelsDefault;
     property Line: boolean read FLine write FLine;
     property Points: boolean read FPoints write FPoints;
   end;


implementation

uses
  LazUTF8;

{ TScatterStatDialogPrimaryOptionModel }

procedure TScatterStatDialogPrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

procedure TScatterStatDialogPrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

constructor TScatterStatDialogPrimaryOptionModel.Create(Executor: TExecutor);
var
    defaults: TStringList;
begin
  FExecutor := Executor;
  defaults := TStringList.Create;
  defaults.AddStrings(['v', 'l', 'vl', 'lv']);
  ValueLabelsDefault := defaults.indexOf(LowerCase(FExecutor.GetSetOptionValue('STATISTICS VALUE LABEL')));
  defaults.Clear;
  defaults.AddStrings(['vn', 'vla', 'vnl', 'vlv']);
  VariableLabelsDefault := defaults.indexOf(LowerCase(FExecutor.GetSetOptionValue('STATISTICS VARIABLE LABEL')));
  defaults.Clear;
  defaults.Free;
end;

function TScatterStatDialogPrimaryOptionModel.GenerateScript(): UTF8String;
var
  compareOption: UTF8String;
begin
  Result := '';

  case FValueLabelType of
    gvtValue:      compareOption := 'v';
    gvtLabel:      compareOption := 'l';
    gvtValueLabel: compareOption := 'vl';
    gvtLabelValue: compareOption := 'lv';
  end;
  if (LowerCase(FExecutor.GetSetOptionValue('STATISTICS VALUE LABEL')) <> compareOption) then
    Result := ' !' + compareOption;

  case FVariableLabelType of
    gvtVarName:      compareOption := 'vn';
    gvtVarLabel:     compareOption := 'vla';
    gvtVarNameLabel: compareOption := 'vnl';
    gvtVarLabelName: compareOption := 'vln';
  end;
  if (LowerCase(FExecutor.GetSetOptionValue('STATISTICS VARIABLE LABEL')) <> compareOption) then
    Result += ' !' + compareOption;

  if (Fline) then
    Result += ' !l';
  if (FPoints) then
    Result += ' !p';
end;

function TScatterStatDialogPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
