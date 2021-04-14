unit describe_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper;

type

   TPercentage = (pRow, pCol, pTotal);
   TPercentages = set of TPercentage;

   TSorting = (sortAsc, sortDesc, sortAscTotal, sortDescTotal);

   { TDescribePrimaryOptionModel }

   TDescribePrimaryOptionModel = class(IStatDialogModel)
   private
     FValueLabelType: TEpiGetValueLabelType;
     FVariableLabelType: TEpiGetVariableLabelType;
     procedure SetValueLabelType(AValue: TEpiGetValueLabelType);
     procedure SetVariableLabelType(AValue: TEpiGetVariableLabelType);
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

{ TDescribePrimaryOptionModel }

procedure TDescribePrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

procedure TDescribePrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

constructor TDescribePrimaryOptionModel.Create();
begin
  //
end;

function TDescribePrimaryOptionModel.GenerateScript(): UTF8String;
begin
  Result := '';

  case FValueLabelType of
    gvtValue:      Result += '!v';
    gvtLabel:      Result += '!l';
    gvtValueLabel: Result += '!vl';
    gvtLabelValue: Result += '!lv';
  end;

  result += ' ';

  case FVariableLabelType of
    gvtVarName:      Result += '!vn';
    gvtVarLabel:     Result += '!vla';
    gvtVarNameLabel: Result += '!vnl';
    gvtVarLabelName: Result += '!vln';
  end;

  Result := UTF8Trim(Result);
end;

function TDescribePrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
