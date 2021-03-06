unit freq_variable_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles;


type

  VariableSelectState = (vssNone, vssMultiple, vssAll);

  { TFreqVariableModel }

  TFreqVariableModel = class(IStatDialogModel)
  private
    FExecutor: TExecutor;
    FListFields: TEpiFields;
    FSelectedFields: TBits;
    function GetSelectVariable(Index: Integer): boolean;
    procedure SetSelectVariable(Index: Integer; AValue: boolean);
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create(Executor: TExecutor);
    function GetListFields(): TEpiFields;
    function GetSelectState: VariableSelectState;
    property SelectVariable[Index: Integer]: boolean read GetSelectVariable write SetSelectVariable;
  end;

implementation

uses
  LazUTF8;

{ TFreqVariableModel }

function TFreqVariableModel.GetSelectVariable(Index: Integer): boolean;
begin
  Result := FSelectedFields.Bits[Index];
end;

procedure TFreqVariableModel.SetSelectVariable(Index: Integer; AValue: boolean);
begin
  FSelectedFields.Bits[Index] := AValue;
end;

function TFreqVariableModel.GenerateScript(): UTF8String;
var
  i: Integer;
begin
  result := '';

  for i := 0 to FListFields.Count - 1 do
    if (FSelectedFields.Bits[i]) then
      result += FListFields[i].Name + ' ';

  result := UTF8Trim(result);
end;

function TFreqVariableModel.IsDefined(): boolean;
begin
  result := FSelectedFields.FindFirstBit(true) > -1;
end;

constructor TFreqVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

function TFreqVariableModel.GetListFields(): TEpiFields;
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

function TFreqVariableModel.GetSelectState: VariableSelectState;
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
end;

end.

