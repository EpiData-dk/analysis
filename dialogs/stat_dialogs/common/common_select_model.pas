unit common_select_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ExtCtrls,
           executor, epidatafiles, epidatafilestypes,
           stat_dialog_contribution;

type

  TMatchCriteria = (
    mcEq,
    mcNEq,
    mcLEq,
    mcLT,
    mcGT,
    mcGEq,
    mcTrue,
    mcFalse,
    mcMissing
  );
  TMatchCriterias = set of TMatchCriteria;

const
  MatchCriteriaCaption: Array[TMatchCriteria] of string =  (
    '=',
    '<>',
    '<=',
    '<',
    '>',
    '>=',
    'true',
    'false',
    'missing'
  );

  MatchCriteriaAll = [mcEq, mcNEq, mcLEq, mcLT, mcGT, mcGEq, mcTrue, mcFalse, mcMissing];
  MatchCriteriaNoTextSearch = [mcTrue, mcFalse, mcMissing];
  MatchCriteriaStrings = [mcEq, mcNEq, mcMissing];
  MatchCriteriaNumber = MatchCriteriaAll - [mcTrue, mcFalse];
  MatchCriteriaBoolean = [mcTrue, mcFalse];

  type

  { TCommonSelectModel }

  TCommonSelectModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FSelectVariable: TEpiField;
    FMatchCriteria: TMatchCriteria;
    FExpression: UTF8String;
    procedure SetField(AValue: TEpiField);
    procedure SetMatchCriteria(AValue: TMatchCriteria);
    procedure SetExpression(AValue: UTF8String);
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetComboFields(): TEpiFields;
    procedure GetComboCriteria(List: TListBox; Ft: TEpiFieldType);
    property SelectVariable: TEpiField read FSelectVariable write SetField;
    property MatchCriteria: TMatchCriteria read FMatchCriteria write SetMatchCriteria;
    property Expression: UTF8String read FExpression write SetExpression;
  end;

implementation

uses
  LazUTF8;

{ TCommonSelectModel }

procedure TCommonSelectModel.SetField(AValue: TEpiField);
begin
  if FSelectVariable = AValue then Exit;
  FSelectVariable := AValue;
end;

procedure TCommonSelectModel.SetMatchCriteria(AValue: TMatchCriteria);
begin
  if FMatchCriteria = AValue then Exit;
  FMatchCriteria := AValue;
end;

procedure TCommonSelectModel.SetExpression(AValue: UTF8String);
begin
  if FExpression = AValue then Exit;
  FExpression := AValue;
end;

function TCommonSelectModel.GenerateScript(): UTF8String;
var
  Field: TEpiField;
  i: Integer;
  FieldName: String;
  Compare: String;
 begin
  result := '';
  if FSelectVariable = nil then exit;

  FieldName := FSelectVariable.Name;
  result := 'select ';

  Compare := MatchCriteriaCaption[FMatchCriteria];
  if FMatchCriteria = mcMissing then
    begin
      result += FieldName + ' = .';
    end
  else
    begin
    case FSelectVariable.FieldType of
      ftInteger,
      ftAutoInc,
      ftFloat:
        result += Fieldname + ' ' + Compare + ' ' + FExpression;

      ftBoolean:
        begin
        if FMatchCriteria = mcFalse then result += ' not ';
        result += Fieldname;
        end
      else
        result += Fieldname + ' ' + Compare + ' "' + FExpression + '"';
    end;
  end;
  result += ' do';
end;

function TCommonSelectModel.IsDefined(): boolean;
begin
  result := true;
  if (FSelectVariable = nil) then exit;
  if FMatchCriteria in MatchCriteriaNoTextSearch then exit;
  result := (FExpression <> '');
end;

constructor TCommonSelectModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TCommonSelectModel.GetComboFields(): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;
  for Field in FExecutor.SortedFields do
      Result.AddItem(Field);
end;

procedure TCommonSelectModel.GetComboCriteria(
  List: TListBox; Ft: TEpiFieldType);
var
  CriteriaSet: TMatchCriterias;
  Criteria: TMatchCriteria;
  ACriteria: String;
begin
  List.Items.BeginUpdate;
  List.Clear;
  if (Ft in StringFieldTypes) then
     CriteriaSet := MatchCriteriaStrings
  else
    if (Ft in [ftInteger, ftAutoInc, ftFloat]) then
     CriteriaSet := MatchCriteriaNumber
    else
      if (Ft = ftBoolean) then
        CriteriaSet := MatchCriteriaBoolean
      else
        CriteriaSet := MatchCriteriaStrings;

  for Criteria in CriteriaSet do
    begin
      ACriteria := MatchCriteriaCaption[Criteria];
      List.AddItem(ACriteria, TObject(PtrUInt(Criteria)));
    end;
  List.Items.EndUpdate;
end;

end.
