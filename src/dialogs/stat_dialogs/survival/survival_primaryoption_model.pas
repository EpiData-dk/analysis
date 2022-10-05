unit survival_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper, executor;

type


   { TSurvivalStatDialogPrimaryOptionModel }

   TSurvivalStatDialogPrimaryOptionModel = class(IStatDialogModel)
   private
     FExecutor: TExecutor;
     FValueLabelType: TEpiGetValueLabelType;
     FVariableLabelType: TEpiGetVariableLabelType;
     FValueLabelsDefault: Integer;
     FVariableLabelsDefault: Integer;
     FDecimals: UTF8String;
     FOutputTable,
     FOutputSummary,
     FOutputClipboard:  Boolean;
     procedure SetValueLabelType(AValue: TEpiGetValueLabelType);
     procedure SetVariableLabelType(AValue: TEpiGetVariableLabelType);
     procedure SetDecimals(AValue: UTF8String);
   public
     constructor Create(Executor: TExecutor);
     function GenerateScript(): UTF8String;
     function IsDefined(): boolean;
   public
     property VariableLabelType: TEpiGetVariableLabelType read FVariableLabelType write SetVariableLabelType;
     property ValueLabelType: TEpiGetValueLabelType read FValueLabelType write SetValueLabelType;
     property Decimals: UTF8String read FDecimals write SetDecimals;
     property ValueLabelsDefault: Integer read FValueLabelsDefault write FValueLabelsDefault;
     property VariableLabelsDefault: Integer read FVariableLabelsDefault write FVariableLabelsDefault;
     property OutputTable: boolean read FOutputTable write FOutputTable;
     property OutputSummary: boolean read FOutputSummary write FOutputSummary;
     property OutputClipboard: boolean read FOutputClipboard write FOutputClipboard;
   end;


implementation

uses
  LazUTF8;

{ TSurvivalStatDialogPrimaryOptionModel }

procedure TSurvivalStatDialogPrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

procedure TSurvivalStatDialogPrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

procedure TSurvivalStatDialogPrimaryOptionModel.SetDecimals(
  AValue: UTF8String);
begin
  if FDecimals = AValue then Exit;
  FDecimals := AValue;
end;

constructor TSurvivalStatDialogPrimaryOptionModel.Create(Executor: TExecutor);
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

function TSurvivalStatDialogPrimaryOptionModel.GenerateScript(): UTF8String;
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

  if (FDecimals <> '3') then
    Result += ' !d' + FDecimals;

  if (not FOutputTable) then
    Result += ' !nt';
  if (not FOutputSummary) then
    Result += ' !ns';
  if (FOutputClipboard) then
    Result += ' !cb';

//  Result := UTF8Trim(Result);
end;

function TSurvivalStatDialogPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
