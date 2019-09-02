unit epi_script_function_timefunctions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ast, epidatafilestypes, ast_types, options_hashmap;

type

  { TEpiScriptFunction_TimeFunctions }

  TEpiScriptFunction_TimeFunctions = class(TFunctionCall)
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

uses
  epi_script_function_resourcestrings, dateutils;

{ TEpiScriptFunction_TimeFunctions }

function TEpiScriptFunction_TimeFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  case FOp of
    otFuncNow:    ;                      // Current time
    otFuncHour,                          // Hour part of a time
    otFuncMinut,                         // Minut part of a time
    otFuncSecond: result[0] := 1;        // Seconds part of a time
  end;
end;

function TEpiScriptFunction_TimeFunctions.ParamAcceptType(ParamNo: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited ParamAcceptType(ParamNo);

  case FOp of
    otFuncNow:
      result.ResultTypes := [];  // Current time

    otFuncHour,                  // Hour part of a time
    otFuncMinut,                 // Minut part of a time
    otFuncSecond:                // Seconds part of a time
      result.ResultTypes := [rtAny, rtInteger, rtDate, rtFloat, rtTime];
  end;
end;

constructor TEpiScriptFunction_TimeFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_TimeFunctions.ResultType: TASTResultType;
begin
  case FOp of
    otFuncNow:
      result := rtTime;
    otFuncHour,
    otFuncMinut,
    otFuncSecond:
      result := rtInteger;
  end;
end;

function TEpiScriptFunction_TimeFunctions.Evaluate: boolean;
begin
  Result := inherited Evaluate;

  if (not Result) then
    Exit;

  FEvalValue.Missing := false;

  case FOp of
    otFuncHour:
      FEvalValue.IntVal := HourOf(Param[0].AsTime);
    otFuncMinut:
      FEvalValue.IntVal := MinuteOf(Param[0].AsTime);
    otFuncSecond:
      FEvalValue.IntVal := SecondOf(Param[0].AsTime);
    otFuncNow:
      FEvalValue.TimeVal := frac(Now);
  end;
end;
{
function TEpiScriptFunction_TimeFunctions.AsInteger: EpiInteger;
begin
  Result := inherited;

  case FOp of
    otFuncHour:
      result := HourOf(Param[0].AsTime);
    otFuncMinut:
      result := MinuteOf(Param[0].AsTime);
    otFuncSecond:
      result := SecondOf(Param[0].AsTime);
  end;
end;

function TEpiScriptFunction_TimeFunctions.AsDate: EpiDate;
begin
  Result := AsInteger;
end;

function TEpiScriptFunction_TimeFunctions.AsFloat: EpiFloat;
begin
  Result := inherited;

  case FOp of
    otFuncNow:
      result := frac(Now);
  end;
end;

function TEpiScriptFunction_TimeFunctions.AsTime: EpiDateTime;
begin
  Result := AsFloat;
end;

function TEpiScriptFunction_TimeFunctions.AsString: EpiString;
begin
  Result := inherited;

{  case FOp of
    otFuncNow:
      Result := TimeToStr(AsTime);
  end;}
end;
}
end.

