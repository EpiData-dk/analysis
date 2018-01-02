unit epi_script_function_datefunctions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ast, epidatafilestypes, ast_types, options_hashmap;

type

  { TEpiScriptFunction_DateFunctions }

  TEpiScriptFunction_DateFunctions = class(TFunctionCall)
  private
    FOp: TParserOperationType;
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(Const AOperation: TParserOperationType; const ParamList: TParamList);
    function ResultType: TASTResultType; override;
    function AsInteger: EpiInteger; override;
    function AsDate: EpiDate; override;
    function AsFloat: ASTFloat; override;
    function AsTime: EpiDateTime; override;
    function AsString: EpiString; override;
  end;

implementation

uses
  epi_script_function_resourcestrings, dateutils;

{ TEpiScriptFunction_DateFunctions }

function TEpiScriptFunction_DateFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  case FOp of
    otFuncToday:       ;                      // Current date
    otFuncDay,                                // Day part of a date
    otFuncMonth,                              // Month part of a date
    otFuncYear,                               // Year part of a date
    otFuncDayOfWeek,                          // Day of the week, result is a number between 1-7
    otFuncWeek:        Result[0] := 1;        // Week number
  end;
end;

function TEpiScriptFunction_DateFunctions.ParamAcceptType(ParamNo: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited ParamAcceptType(ParamNo);

  with Result do
    case FOp of
      otFuncToday:     ResultTypes := [];                  // Current date
      otFuncDay,                                           // Day part of a date
      otFuncMonth,                                         // Month part of a date
      otFuncYear,                                          // Year part of a date
      otFuncDayOfWeek,                                     // Day of the week, ResultTypes is a number between 1-7
      otFuncWeek:      ResultTypes := [rtAny, rtInteger,   // Week number
                                       rtDate];
    end;
end;

constructor TEpiScriptFunction_DateFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_DateFunctions.ResultType: TASTResultType;
begin
  case FOp of
    otFuncToday:
      result := rtDate;

    otFuncDay,
    otFuncMonth,
    otFuncYear,
    otFuncDayOfWeek,
    otFuncWeek:
      result := rtInteger;
  end;
end;

function TEpiScriptFunction_DateFunctions.AsInteger: EpiInteger;
begin
  result := inherited;

  case FOp of
    otFuncToday:
      result := Trunc(Today);
    otFuncDay:
      result := DayOf(Param[0].AsInteger);
    otFuncMonth:
      result := MonthOf(Param[0].AsInteger);
    otFuncYear:
      result := YearOf(Param[0].AsInteger);
    otFuncDayOfWeek:
      begin
        Result := ((Param[0].AsInteger - 1) mod 7);
        If (Result<=0) then
          Inc(Result,7);
      end;
    otFuncWeek:
      result := WeekOf(Param[0].AsInteger);
  end;
end;

function TEpiScriptFunction_DateFunctions.AsDate: EpiDate;
begin
  result := AsInteger;
end;

function TEpiScriptFunction_DateFunctions.AsFloat: ASTFloat;
begin
  Result := AsInteger;
end;

function TEpiScriptFunction_DateFunctions.AsTime: EpiDateTime;
begin
  Result := AsInteger;
end;

function TEpiScriptFunction_DateFunctions.AsString: EpiString;
begin
  if FOp = otFuncToday then
    Result := DateToStr(AsInteger)
  else
    Result := inherited;
end;

end.

