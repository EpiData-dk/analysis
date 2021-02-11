unit tables_statdialog_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper;

type

   TPercentage = (pRow, pCol, pTotal);
   TPercentages = set of TPercentage;

   TSorting = (sortAsc, sortDesc, sortAscTotal, sortDescTotal);
   
   { TTableStatDialogPrimaryOptionModel }

   TTableStatDialogPrimaryOptionModel = class(IStatDialogModel)
   private
     FPercentages: TPercentages;
     FSorting: TSorting;
     FValueLabelType: TEpiGetValueLabelType;
     FVariableLabelType: TEpiGetVariableLabelType;
     procedure SetPercentages(AValue: TPercentages);
     procedure SetSorting(AValue: TSorting);
     procedure SetValueLabelType(AValue: TEpiGetValueLabelType);
     procedure SetVariableLabelType(AValue: TEpiGetVariableLabelType);
   public
     constructor Create();
     function GenerateScript(): UTF8String;
     function IsDefined(): boolean;
   public
     property VariableLabelType: TEpiGetVariableLabelType read FVariableLabelType write SetVariableLabelType;
     property ValueLabelType: TEpiGetValueLabelType read FValueLabelType write SetValueLabelType;
     property Percentages: TPercentages read FPercentages write SetPercentages;
     property Sorting: TSorting read FSorting write SetSorting;
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

procedure TTableStatDialogPrimaryOptionModel.SetSorting(AValue: TSorting);
begin
  if FSorting = AValue then Exit;
  FSorting := AValue;
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

constructor TTableStatDialogPrimaryOptionModel.Create();
begin
  //
end;

function TTableStatDialogPrimaryOptionModel.GenerateScript(): UTF8String;
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

  result += ' ';

  case FSorting of
    sortAsc:       Result += '!sa';
    sortDesc:      Result += '!sd';
    sortAscTotal:  Result += '!scta !srta';
    sortDescTotal: Result += '!sctd !srtd';
  end;

  result += ' ';

  if (pCol   in FPercentages) then Result += '!pc ';
  if (pRow   in FPercentages) then Result += '!pr ';
  if (pTotal in FPercentages) then Result += '!pt ';

  Result := UTF8Trim(Result);
end;

function TTableStatDialogPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.

