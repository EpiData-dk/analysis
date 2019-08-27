unit epi_script_function_observations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ast, epidatafilestypes, ast_types, options_hashmap;

type

  { TEpiScriptFunction_ObsFunctions }

  // deleted ([index: integer]) : boolean
  // verified ([index: integer]) : boolean

  TEpiScriptFunction_ObsFunctions = class(TFunctionCall)
  private
    FOp: TParserOperationType;
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(Const AOperation: TParserOperationType; const ParamList: TParamList);
    function ResultType: TASTResultType; override;
    function Evaluate: boolean; override;
  end;

implementation

{ TEpiScriptFunction_ObsFunctions }

function TEpiScriptFunction_ObsFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  SetLength(Result, 2);
  Result[1] := 1;
end;

function TEpiScriptFunction_ObsFunctions.ParamAcceptType(ParamNo: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited ParamAcceptType(ParamNo);

  if (ParamNo = 0) then result.ResultTypes := [rtInteger];
end;

constructor TEpiScriptFunction_ObsFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_ObsFunctions.ResultType: TASTResultType;
begin
  Result := rtBoolean;
end;

function TEpiScriptFunction_ObsFunctions.Evaluate: boolean;
begin
  Result := inherited Evaluate;

  if (Assigned(FParamList)) and
     (FParamList.Count = 1)
  then
    Idx := Param[0].AsInteger
  else
    Idx := FExecutor.GetCurrentRecordNo;

  case FOp of
    otFuncDeleted:
      FEvalValue.BoolVal := FExecutor.DataFile.Deleted[Idx];

    otFuncVerified:
      FEvalValue.BoolVal := FExecutor.DataFile.Verified[Idx];
  end;
end;
{
function TEpiScriptFunction_ObsFunctions.AsBoolean: Boolean;
var
  Idx: ASTInteger;
begin
  Result := inherited AsBoolean;

  if (Assigned(FParamList)) and
     (FParamList.Count = 1)
  then
    Idx := Param[0].AsInteger
  else
    Idx := FExecutor.GetCurrentRecordNo;

  case FOp of
    otFuncDeleted:
      Result := FExecutor.DataFile.Deleted[Idx];

    otFuncVerified:
      Result := FExecutor.DataFile.Verified[Idx];
  end;
end;
}
end.

