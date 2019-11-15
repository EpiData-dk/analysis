unit epi_script_function_createtime;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ast, epidatafilestypes, options_hashmap,
    ast_types;

type

  { TEpiScriptFunction_CreateTime }

  TEpiScriptFunction_CreateTime = class(TFunctionCall)
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; override;
  public
    function ResultType: TASTResultType; override;
    function AsInteger: EpiInteger; override;
    function AsTime: EpiTime; override;
    function IsMissing: Boolean; override;
  end;

  EEpiScriptFunction_CreateTime = class(Exception);

implementation

uses
  epidatafiles, epiconvertutils;

{ TEpiScriptFunction_CreateTime }

function TEpiScriptFunction_CreateTime.ParamCounts: TBoundArray;
begin
  SetLength(Result, 2);
  Result[0] := 1;
  Result[1] := 3;
end;

function TEpiScriptFunction_CreateTime.ParamAcceptType(ParamNo: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited ParamAcceptType(ParamNo);

  if FParamList.Count = 1 then
    result.ResultTypes := [rtString]
  else
    Result.ResultTypes := [rtAny, rtInteger];
end;

function TEpiScriptFunction_CreateTime.ResultType: TASTResultType;
begin
  Result := rtTime;
end;

function TEpiScriptFunction_CreateTime.AsInteger: EpiInteger;
begin
  Result := trunc(AsFloat);
end;

function TEpiScriptFunction_CreateTime.AsTime: EpiTime;
var
  Hour, Minut, Sec: TExpr;
  M, S: EpiInteger;
  ResultTime: EpiTime;
  Msg: string;
begin
  result := inherited;

  if FParamList.Count = 1 then
  begin
    if not EpiStrToTimeGues(Param[0].AsString, Result, Msg) then
      RuntimeError(EEpiScriptFunction_CreateTime, Msg);
    Exit;
  end;

  if FParamList.Count = 3 then
  begin
    Hour := Param[0];
    Minut := Param[1];
    Sec   := Param[2];

    if Sec.IsMissing then
      S := 0
    else
      S := Sec.AsInteger;

    if Minut.IsMissing then
      M := 0
    else
      M := Minut.AsInteger;

    if Hour.IsMissing then
      result := inherited AsTime
    else
      if (not TryEncodeTime(Hour.AsInteger, M, S, 0, result)) then
        result := inherited AsTime;

    Exit;
  end;
end;

function TEpiScriptFunction_CreateTime.IsMissing: Boolean;
var
  S, M: Integer;
  Dummy: TDateTime;
  Hour, Minute, Sec: TExpr;
  Msg: string;
begin
  Result := inherited IsMissing;

  if FParamList.Count = 1 then
  begin
    result := not EpiStrToTimeGues(Param[0].AsString, Dummy, Msg);
    Exit;
  end;

  if FParamList.Count = 3 then
  begin
    Hour := Param[0];
    Minute := Param[1];
    Sec   := Param[2];

    if Sec.IsMissing then
      S := 0
    else
      S := Sec.AsInteger;

    if Minute.IsMissing then
      M := 0
    else
      M := Minute.AsInteger;

    if Hour.IsMissing then
      result := true
    else
      result := not TryEncodeTime(Hour.AsInteger, M, S, 0, Dummy);
  end;
end;

end.

