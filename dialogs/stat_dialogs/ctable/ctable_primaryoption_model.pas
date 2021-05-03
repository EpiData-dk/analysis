unit ctable_primaryoption_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, epifields_helper;

type

   TIncludeType     = (pInclude);
   TIncludeTypes    = set of TIncludeType ;

   { TCtableStatDialogPrimaryOptionModel }

   TCtableStatDialogPrimaryOptionModel = class(IStatDialogModel)
   private
     FValueLabelType: TEpiGetValueLabelType;
     FVariableLabelType: TEpiGetVariableLabelType;
     FIncludeTypes:    TIncludeTypes;
     procedure SetValueLabelType(AValue: TEpiGetValueLabelType);
     procedure SetVariableLabelType(AValue: TEpiGetVariableLabelType);
     procedure SetInclude(AValue: TIncludeTypes);
   public
     constructor Create();
     function GenerateScript(): UTF8String;
     function IsDefined(): boolean;
   public
     property VariableLabelType: TEpiGetVariableLabelType read FVariableLabelType write SetVariableLabelType;
     property ValueLabelType: TEpiGetValueLabelType read FValueLabelType write SetValueLabelType;
     property IncludeTypes: TIncludeTypes read FIncludeTypes write SetInclude;
   end;


implementation

uses
  LazUTF8;

{ TCtableStatDialogPrimaryOptionModel }

procedure TCtableStatDialogPrimaryOptionModel.SetValueLabelType(
  AValue: TEpiGetValueLabelType);
begin
  if FValueLabelType = AValue then Exit;
  FValueLabelType := AValue;
end;

procedure TCtableStatDialogPrimaryOptionModel.SetVariableLabelType(
  AValue: TEpiGetVariableLabelType);
begin
  if FVariableLabelType = AValue then Exit;
  FVariableLabelType := AValue;
end;

procedure TCtableStatDialogPrimaryOptionModel.SetInclude(AValue: TIncludeTypes);
begin
  if FIncludeTypes = AValue then Exit;
  FIncludeTypes := AValue;
end;

constructor TCtableStatDialogPrimaryOptionModel.Create();
begin
  //
end;

function TCtableStatDialogPrimaryOptionModel.GenerateScript(): UTF8String;
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

  if (pInclude in FIncludeTypes) then Result += '!inc ';

  Result := UTF8Trim(Result);
end;

function TCtableStatDialogPrimaryOptionModel.IsDefined(): boolean;
begin
  result := true;
end;

end.

