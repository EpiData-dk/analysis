unit means_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper;

type

   { TMeansStatDialogPrimaryOptionModel }

   TMeansStatDialogPrimaryOptionModel = class(IStatDialogModel)
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

{ TMeansStatDialogPrimaryOptionModel }

procedure TMeansStatDialogPrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

procedure TMeansStatDialogPrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

constructor TMeansStatDialogPrimaryOptionModel.Create();
begin
  //
end;

function TMeansStatDialogPrimaryOptionModel.GenerateScript(): UTF8String;
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

function TMeansStatDialogPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.

