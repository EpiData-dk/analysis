unit histogram_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper, executor;

type


   { ThistogramStatDialogPrimaryOptionModel }

   THistogramStatDialogPrimaryOptionModel = class(IStatDialogModel)
   private
     FExecutor: TExecutor;
     FValueLabelType: TEpiGetValueLabelType;
     FVariableLabelType: TEpiGetVariableLabelType;
     FValueLabelsDefault: Integer;
     FVariableLabelsDefault: Integer;
     FDecimals: UTF8String;
     FStack: Boolean;
     FEpicurve: Boolean;
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
     property Epicurve: Boolean read FEpicurve write FEpicurve;
     property Stack: boolean read FStack write FStack;
   end;


implementation

uses
  LazUTF8;

{ THistogramStatDialogPrimaryOptionModel }

procedure THistogramStatDialogPrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

procedure THistogramStatDialogPrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

constructor THistogramStatDialogPrimaryOptionModel.Create(Executor: TExecutor);
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
  FEpicurve := false;
end;

function THistogramStatDialogPrimaryOptionModel.GenerateScript(): UTF8String;
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

  if (FStack and (not FEpicurve)) then
    Result += ' !stack';
//  if (FInterval) then
//    Result += ' !interval:=' + FInterval;

//  Result := UTF8Trim(Result);
end;

function THistogramStatDialogPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
