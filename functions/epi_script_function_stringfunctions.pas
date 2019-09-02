unit epi_script_function_stringfunctions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ast, epidatafilestypes, ast_types, options_hashmap;

type

  { TEpiScriptFunction_StringFunctions }

  TEpiScriptFunction_StringFunctions = class(TFunctionCall)
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

  EEpiScriptFunction_StringFunctions = class(Exception);

implementation

uses
  LazUTF8, epi_script_function_resourcestrings;

{ TEpiScriptFunction_StringFunctions }

function TEpiScriptFunction_StringFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  case FOp of
    otFuncLength,
    otFuncLower,
    otFuncUpper,
    otFuncTrim:
      result[0] := 1;
    otFuncPos:
      result[0] := 2;
    otFuncSubString:
      result[0] := 3;
    otFuncConcat:
      result[0] := -1;
  end;
end;

function TEpiScriptFunction_StringFunctions.ParamAcceptType(ParamNo: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited ParamAcceptType(ParamNo);

  case FOp of
    otFuncLength,
    otFuncLower,
    otFuncUpper,
    otFuncTrim:
      Result.ResultTypes := [rtAny, rtString];
    otFuncPos:
      Result.ResultTypes := [rtAny, rtString];
    otFuncSubString:
      if ParamNo = 0 then
        Result.ResultTypes := [rtAny, rtString]
      else
        Result.ResultTypes := [rtInteger];
    otFuncConcat:
      if ParamNo = 0 then
        Result.ResultTypes := AllResultDataTypes
      else
        Result.ResultTypes := AllResultDataTypes + [rtAny]
  end;
end;

constructor TEpiScriptFunction_StringFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_StringFunctions.ResultType: TASTResultType;
begin
  case FOp of
    otFuncPos,
    otFuncLength:
      result := rtInteger;
    otFuncLower,
    otFuncUpper,
    otFuncSubString,
    otFuncTrim,
    otFuncConcat:
      result := rtString;
  end;
end;

function TEpiScriptFunction_StringFunctions.Evaluate: boolean;
var
  T: EpiString;
  S: String;
  i: Integer;
begin
  Result := inherited Evaluate;

  if (not Result) then
    Exit;

  FEvalValue.Missing := false;

  case FOp of
    otFuncPos:
      begin
        if (Param[0].IsMissing or Param[1].IsMissing) then
          begin
            FEvalValue.IntVal := 0;
            Exit;
          end;

        FEvalValue.IntVal := UTF8Pos(Param[1].AsString, Param[0].AsString);
      end;

    otFuncLength:
      if Param[0].IsMissing then
        FEvalValue.IntVal := 0
      else
        FEvalValue.IntVal := UTF8Length(Param[0].AsString);


    otFuncLower:
      FEvalValue.StringVal := UTF8LowerCase(Param[0].AsString);

    otFuncUpper:
      FEvalValue.StringVal := UTF8UpperCase(Param[0].AsString);

    otFuncSubString:
      begin
        if Param[0].IsMissing then Exit;

        if Param[1].IsMissing or Param[2].IsMissing then
          RuntimeError(EEpiScriptFunction_StringFunctions, 'Substring: Pos or Len value is missing!') // TODO : Error in execution
        else
          FEvalValue.StringVal := UTF8Copy(Param[0].AsString, Param[1].AsInteger, Param[2].AsInteger);
      end;

    otFuncTrim:
      if Param[0].IsMissing then
        FEvalValue.StringVal := Param[0].AsString
      else
        FEvalValue.StringVal := UTF8Trim(Param[0].AsString);

    otFuncConcat:
      begin
        T := Param[0].AsString;

        S := '';
        for i := 1 to FParamList.Count - 1 do
          if Param[i].IsMissing then
            S := S + T
          else
            S := S + Param[i].AsString;

        FEvalValue.StringVal := S;
      end;
  end;
end;

{

function TEpiScriptFunction_StringFunctions.AsInteger: EpiInteger;
begin
  result := inherited;

  case FOp of
    otFuncPos:
      begin
        if Param[0].IsMissing then exit(0);
        if Param[1].IsMissing then exit(0);
        result := UTF8Pos(Param[1].AsString, Param[0].AsString);
      end;
    otFuncLength:
      if Param[0].IsMissing then
        result := 0
      else
        result := UTF8Length(Param[0].AsString);
  end;
end;

function TEpiScriptFunction_StringFunctions.AsString: EpiString;
var
  S: EpiString;
  i: Integer;
begin
  result := inherited;

  case FOp of
    otFuncLower:
      result := UTF8LowerCase(Param[0].AsString);

    otFuncUpper:
      result := UTF8UpperCase(Param[0].AsString);

    otFuncSubString:
      begin
        if Param[0].IsMissing then Exit;

        if Param[1].IsMissing or Param[2].IsMissing then
          RuntimeError(EEpiScriptFunction_StringFunctions, 'Substring: Pos or Len value is missing!') // TODO : Error in execution
        else
          result := UTF8Copy(Param[0].AsString, Param[1].AsInteger, Param[2].AsInteger);
      end;

    otFuncTrim:
      if Param[0].IsMissing then
        Exit(inherited AsString)
      else
        Result := UTF8Trim(Param[0].AsString);

    otFuncConcat:
      begin
        S := Param[0].AsString;

        result := '';
        for i := 1 to FParamList.Count - 1 do
          if Param[i].IsMissing then
            Result := Result + S
          else
            Result := Result + Param[i].AsString;
      end;
  end;
end;
}
end.

