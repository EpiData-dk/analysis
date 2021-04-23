unit describe_variable_list_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles;
//  describe_variable_model;

{
TODO: remove all the cross-references
      have only one describe_variables_model and _view
      structure the view to have two columns; one with dropdowns, the other with a list
      ideally, check boxes for vars when range is set
}
type

//  VariableSelectState = (vssNone, vssMultiple, vssAll);

  { TDescribeVariableListModel }

  TDescribeVariableListModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FListFields: TEpiFields;
    FSelectedFields: TBits;
//    FVariableModel: TDescribeVariableModel;
    function GetSelectVariable(Index: Integer): boolean;
    procedure SetSelectVariable(Index: Integer; AValue: boolean);
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetListFields(): TEpiFields;
//    function GetSelectState: VariableSelectState;
    property SelectVariable[Index: Integer]: boolean read GetSelectVariable write SetSelectVariable;
//    property VariableModel: TDescribeVariableModel read FVariableModel write FVariableModel;
  end;

implementation

uses
  LazUTF8;

{ TDescribeVariableListModel }

function TDescribeVariableListModel.GetSelectVariable(Index: Integer): boolean;
begin
  Result := FSelectedFields.Bits[Index];
end;

procedure TDescribeVariableListModel.SetSelectVariable(Index: Integer; AValue: boolean);
begin
  FSelectedFields.Bits[Index] := AValue;
end;

function TDescribeVariableListModel.GenerateScript(): UTF8String;
var
  i: Integer;
begin
  result := '';

  for i := 0 to FListFields.Count - 1 do
    if (FSelectedFields.Bits[i]) then
      result += FListFields[i].Name + ' ';

  result := UTF8Trim(result);
end;

function TDescribeVariableListModel.IsDefined(): boolean;
begin
  result := FSelectedFields.FindFirstBit(true) > -1;
//  result := result or FVariableModel.IsDefined();
end;

constructor TDescribeVariableListModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TDescribeVariableListModel.GetListFields(): TEpiFields;
var
  Field: TEpiField;
begin
  Result := TEpiFields.Create(nil);
  Result.Sorted := false;

  for Field in FExecutor.SortedFields do
    Result.AddItem(Field);

  FreeAndNil(FSelectedFields);
  FSelectedFields := TBits.Create(Result.Count);
  FListFields := Result;
end;

{function TDescribeVariableListModel.GetSelectState: VariableSelectState;
var
  AllSelected: Boolean;
  i: Integer;
begin
  Result := vssNone;

  if (FSelectedFields.FindFirstBit(true) > -1) then Result := vssMultiple;

  AllSelected := true;
  for i := 0 to FSelectedFields.Size - 1 do
    AllSelected := AllSelected and FSelectedFields.Bits[i];

  if (AllSelected) then Result := vssAll;
end; }

end.

