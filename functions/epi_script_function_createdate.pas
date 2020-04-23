unit epi_script_function_createdate;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ast, epidatafilestypes, options_hashmap,
    ast_types;

type

  { TEpiScriptFunction_CreateDate }

  TEpiScriptFunction_CreateDate = class(TFunctionCall)
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; override;
  public
    function ResultType: TASTResultType; override;
    function AsDate: EpiDate; override;
    function AsInteger: ASTInteger; override;
  end;

  EEpiScriptFunction_CreateDate = class(Exception);

implementation

uses
  epidatafiles, epiconvertutils, scandate_from_fpc;

{ TEpiScriptFunction_CreateDate }

function TEpiScriptFunction_CreateDate.ParamCounts: TBoundArray;
begin
  SetLength(Result, 3);
  Result[0] := 1;
  Result[1] := 2;
  Result[2] := 3;
end;

function TEpiScriptFunction_CreateDate.ParamAcceptType(ParamNo: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited ParamAcceptType(ParamNo);
  case FParamList.Count of
    1, 2:
      result.ResultTypes := [rtString];
    3:
      result.ResultTypes := [rtAny, rtInteger];
  end;
end;

function TEpiScriptFunction_CreateDate.ResultType: TASTResultType;
begin
  Result := rtDate;
end;

function TEpiScriptFunction_CreateDate.AsDate: EpiDate;
var
  Day, Month, Year: TExpr;
  D, M: Integer;
  S: String;
  Ft: TEpiFieldType;
  Msg: string;
  DateResult: EpiDate;
begin
  result := inherited;

  if FParamList.Count = 1 then
  begin
    if (not EpiStrToDateGuess(Param[0].AsString, DateResult, Msg)) then
      RuntimeError(EEpiScriptFunction_CreateDate, Msg);

    Result := DateResult;
    Exit;
  end;

  if FParamList.Count = 2 then
  begin
    S := LowerCase(Param[1].AsString);
    case S of
      'dmy': Ft := ftDMYDate;
      'mdy': Ft := ftMDYDate;
      'ymd': Ft := ftYMDDate;
    else
      begin
        try
          Result := Trunc(epi_scandatetime(S, Param[0].AsString));
        except
          Result := TEpiDateField.DefaultMissing;
        end;
        Exit;
      end;
    end;

    if not (
         (EpiStrToDate(Param[0].AsString, '-', ft, DateResult, Msg)) or
         (EpiStrToDate(Param[0].AsString, '/', ft, DateResult, Msg)) or
         (EpiStrToDate(Param[0].AsString, '.', ft, DateResult, Msg))
       )
    then
      RuntimeError(EEpiScriptFunction_CreateDate, Msg);

    Result := DateResult;
    Exit;
  end;

  if FParamList.Count = 3 then
  begin
    Day   := Param[0];
    Month := Param[1];
    Year  := Param[2];

    if Day.IsMissing then
      D := 1
    else
      D := Day.AsInteger;

    if Month.IsMissing then
      M := 1
    else
      M := Month.AsInteger;

    if Year.IsMissing then
      Result := inherited AsDate
    else
      Result := Trunc(EncodeDate(Year.AsInteger, M, D));

    Exit;
  end;
end;

function TEpiScriptFunction_CreateDate.AsInteger: ASTInteger;
begin
  Result := AsDate;
end;

end.

