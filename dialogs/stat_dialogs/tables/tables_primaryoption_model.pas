unit tables_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper;

type

   TPercentage = (pRow, pCol, pTotal);
   TPercentages = set of TPercentage;

   { TTableStatDialogPrimaryOptionModel }

   TTableStatDialogPrimaryOptionModel = class(IStatDialogModel)
   private
     FPercentages: TPercentages;
     FValueLabelType: TEpiGetValueLabelType;
     FVariableLabelType: TEpiGetVariableLabelType;
     FShowMissing: boolean;
     procedure SetPercentages(AValue: TPercentages);
     procedure SetValueLabelType(AValue: TEpiGetValueLabelType);
     procedure SetVariableLabelType(AValue: TEpiGetVariableLabelType);
     procedure SetShowMissing(AValue: boolean);
   public
     constructor Create();
     function GenerateScript(): UTF8String;
     function IsDefined(): boolean;
   public
     property VariableLabelType: TEpiGetVariableLabelType read FVariableLabelType write SetVariableLabelType;
     property ValueLabelType: TEpiGetValueLabelType read FValueLabelType write SetValueLabelType;
     property Percentages: TPercentages read FPercentages write SetPercentages;
     property ShowMissing: boolean read FShowMissing write SetShowMissing;
   end;


implementation

uses
  LazUTF8;

{ TTableStatDialogPrimaryOptionModel }

procedure TTableStatDialogPrimaryOptionModel.SetPercentages(AValue: TPercentages
  );
begin
  if FPercentages = AValue then Exit;
  FPercentages := AValue;
end;

procedure TTableStatDialogPrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

procedure TTableStatDialogPrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

procedure TTableStatDialogPrimaryOptionModel.SetShowMissing(AValue: boolean);
begin
  if FShowMissing = AValue then Exit;
  FShowMissing := AValue;
end;

constructor TTableStatDialogPrimaryOptionModel.Create();
begin
  //
end;

function TTableStatDialogPrimaryOptionModel.GenerateScript(): UTF8String;
begin
  Result := '';

  case FValueLabelType of
    gvtValue:      Result += '!v ';
    gvtLabel:      Result += '!l ';
    gvtValueLabel: Result += '!vl ';
    gvtLabelValue: Result += '!lv ';
  end;

  case FVariableLabelType of
    gvtVarName:      Result += '!vn ';
    gvtVarLabel:     Result += '!vla ';
    gvtVarNameLabel: Result += '!vnl ';
    gvtVarLabelName: Result += '!vln ';
  end;

  if (pCol   in FPercentages) then Result += '!pc ';
  if (pRow   in FPercentages) then Result += '!pr ';
  if (pTotal in FPercentages) then Result += '!pt ';

  if (FShowMissing) then result += '!m ';

end;

function TTableStatDialogPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.

