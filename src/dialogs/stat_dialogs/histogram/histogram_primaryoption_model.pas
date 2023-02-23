unit histogram_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper, executor;

type


   { THistogramDialogPrimaryOptionModel }

   THistogramDialogPrimaryOptionModel = class(IStatDialogModel)
   private
     FExecutor: TExecutor;
     FValueLabelType: TEpiGetValueLabelType;
     FVariableLabelType: TEpiGetVariableLabelType;
     FValueLabelsDefault: Integer;
     FVariableLabelsDefault: Integer;
     FStack: Boolean;
     FSortD:  Boolean;
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
     property Stack: boolean read FStack write FStack;
     property SortD: boolean read FSortD write FSortD;
   end;


implementation

uses
  LazUTF8;

{ THistogramDialogPrimaryOptionModel }

procedure THistogramDialogPrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

procedure THistogramDialogPrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

constructor THistogramDialogPrimaryOptionModel.Create(Executor: TExecutor);
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

function THistogramDialogPrimaryOptionModel.GenerateScript(): UTF8String;
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

  if (FStack) then
    Result += ' !stack';
  if (FSortD) then
    Result += ' !sd';
end;

function THistogramDialogPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
