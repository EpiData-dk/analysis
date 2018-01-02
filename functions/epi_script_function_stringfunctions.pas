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
    function AsInteger: EpiInteger; override;
    function AsString: EpiString; override;
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
    otFuncTrim:
      result := rtString;
  end;
end;

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
  end;
end;

end.

