unit regress_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper;

type

   { TRegressPrimaryOptionModel }

   TRegressPrimaryOptionModel = class(IStatDialogModel)
   private
     FVariableLabelType: TEpiGetVariableLabelType;
     FValueLabelType: TEpiGetValueLabelType;
     procedure SetVariableLabelType(AValue: TEpiGetVariableLabelType);
     procedure SetValueLabelType(AValue: TEpiGetValueLabelType);
   public
     constructor Create();
     function GenerateScript(): UTF8String;
     function IsDefined(): boolean;
   public
     property VariableLabelType: TEpiGetVariableLabelType read FVariableLabelType write SetVariableLabelType;
     property ValueLabelType: TEpiGetValueLabelType read FValueLabelType write SetValueLabelType;
   end;


implementation

uses
  LazUTF8;

{ TRegressPrimaryOptionModel }

procedure TRegressPrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

procedure TRegressPrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

constructor TRegressPrimaryOptionModel.Create();
begin
  //
end;

function TRegressPrimaryOptionModel.GenerateScript(): UTF8String;
begin
  Result := '';

  case FVariableLabelType of
    gvtVarName:      Result += '!vn';
    gvtVarLabel:     Result += '!vla';
    gvtVarNameLabel: Result += '!vnl';
    gvtVarLabelName: Result += '!vln';
  end;

    result += ' ';

  case FValueLabelType of
    gvtValue:      Result += '!v';
    gvtLabel:      Result += '!l';
    gvtValueLabel: Result += '!vl';
    gvtLabelValue: Result += '!lv';
  end;

end;

function TRegressPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.

