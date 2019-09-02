unit epi_script_function_mathfunctions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ast, epidatafilestypes, options_hashmap,
    ast_types;

type

  { TEpiScriptFunction_MathFunctions }

{
  otFuncAbs,
  otFuncExp,
  otFuncFraction,
  otFuncLn,
  otFuncLog,
  otFuncSqrt,
  otFuncRandom,
  otFuncTan,
  otFuncArcTan,
  otFuncSin,
  otFuncArcSin,
  otFuncCos,
  otFuncArcCos:
  otFuncRound,
  otFuncLRE:
  otFuncSameValue:
  otFuncSum:
}

  TEpiScriptFunction_MathFunctions = class(TFunctionCall)
  private
    FOp: TParserOperationType;
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; override;
    function DoEvaluate: boolean; override;
  public
    constructor Create(Const AOperation: TParserOperationType; const ParamList: TParamList);
    function ResultType: TASTResultType; override;
    function IsMissing: Boolean; override;
  end;

implementation

uses
  math, epi_script_function_resourcestrings;

{ TEpiScriptFunction_MathFunctions }

function TEpiScriptFunction_MathFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;

  case FOp of
    otFuncAbs,
    otFuncExp,
    otFuncFraction,
    otFuncLn,
    otFuncLog,
    otFuncSqrt,
    otFuncRandom,
    otFuncTan,
    otFuncArcTan,
    otFuncSin,
    otFuncArcSin,
    otFuncCos,
    otFuncArcCos:
      Result[0] := 1;
    otFuncRound,
    otFuncLRE:
      Result[0] := 2;
    otFuncSameValue:
      begin
        SetLength(Result, 2);
        Result[0] := 2;
        Result[1] := 3;
      end;
    otFuncSum:
      Result[0] := -1;
  end;
end;

function TEpiScriptFunction_MathFunctions.ParamAcceptType(ParamNo: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited ParamAcceptType(ParamNo);

  case FOp of
    otFuncAbs,
    otFuncExp,
    otFuncLn,
    otFuncLog,
    otFuncSqrt,
    otFuncTan,
    otFuncArcTan,
    otFuncSin,
    otFuncArcSin,
    otFuncCos,
    otFuncArcCos,
    otFuncLRE:
      Result.ResultTypes := [rtAny, rtInteger, rtFloat];
    otFuncFraction:
      Result.ResultTypes := [rtAny, rtInteger, rtDate, rtFloat, rtTime];
    otFuncRandom:
      Result.ResultTypes := [rtAny, rtInteger];
    otFuncRound:
      if ParamNo = 0 then
        Result.ResultTypes := [rtAny, rtInteger, rtDate, rtFloat, rtTime]
      else
        Result.ResultTypes := [rtInteger];
    otFuncSameValue:
      if ParamNo < 2 then
        Result.ResultTypes := [rtInteger, rtFloat, rtDate, rtTime]
      else
        Result.ResultTypes := [rtInteger];
    otFuncSum:
      Result.ResultTypes := [rtInteger, rtFloat];
  end;
end;

constructor TEpiScriptFunction_MathFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_MathFunctions.ResultType: TASTResultType;
begin
  case FOp of
    otFuncAbs:
      Result := Param[0].ResultType;
    otFuncExp,
    otFuncFraction,
    otFuncLn,
    otFuncLog,
    otFuncSqrt,
    otFuncTan,
    otFuncArcTan,
    otFuncSin,
    otFuncArcSin,
    otFuncCos,
    otFuncArcCos,
    otFuncRound,
    otFuncLRE:
      Result := rtFloat;
    otFuncRandom:
      result := rtInteger;
    otFuncSameValue:
      result := rtBoolean;
    otFuncSum:
      result := Param[0].ResultType;
  end;
end;

function TEpiScriptFunction_MathFunctions.DoEvaluate: boolean;
var
  E: float;
  i: Integer;
  P: TExpr;
  c, t: ASTFloat;
begin
  Result := inherited DoEvaluate;

  if (not Result) then
    Exit;

  FEvalValue.Missing := false;

  case ResultType of
    rtBoolean:
      case FOp of
        otFuncSameValue:
          if FParamList.Count =  2 then
            FEvalValue.BoolVal := SameValue(Param[0].AsFloat, param[1].AsFloat)
          else
            begin
              E := power(10, -Param[2].AsInteger);
              FEvalValue.BoolVal := SameValue(Param[0].AsFloat, param[1].AsFloat, E);
            end;
      end;

    rtInteger:
      case FOp of
        otFuncRandom:
          FEvalValue.IntVal := Random(Param[0].AsInteger);

        otFuncSum:
          begin
            FEvalValue.IntVal := 0;

            for i := 0 to FParamList.Count - 1 do
              begin
                P := Param[i];
                if (not (P.IsMissing or P.IsUserMissing)) then
                  FEvalValue.IntVal := FEvalValue.IntVal + P.AsInteger;
              end;
          end;

        otFuncAbs:
          FEvalValue.IntVal := Abs(Param[0].AsInteger);
      end;

    rtFloat:
      case FOp of
        otFuncSum:
          begin
            FEvalValue.FloatVal := 0;

            for i := 0 to FParamList.Count - 1 do
              begin
                P := Param[i];
                if (not (P.IsMissing or P.IsUserMissing)) then
                  FEvalValue.FloatVal := FEvalValue.FloatVal + P.AsFloat;
              end;
          end;

        otFuncAbs:
          FEvalValue.FloatVal := Abs(Param[0].AsFloat);

        otFuncExp:
          FEvalValue.FloatVal := exp(Param[0].AsFloat);

        otFuncFraction:
          FEvalValue.FloatVal := frac(Param[0].AsFloat);

        otFuncLn:
          FEvalValue.FloatVal := ln(Param[0].AsFloat);

        otFuncLog:
          FEvalValue.FloatVal := log10(Param[0].AsFloat);

        otFuncSqrt:
          FEvalValue.FloatVal := sqrt(Param[0].AsFloat);

        otFuncRound:
          FEvalValue.FloatVal := RoundTo(Param[0].AsFloat, -Param[1].AsInteger);

        otFuncTan:
          FEvalValue.FloatVal := tan(Param[0].AsFloat);

        otFuncArcTan:
          FEvalValue.FloatVal := arctan(Param[0].AsFloat);

        otFuncSin:
          FEvalValue.FloatVal := sin(Param[0].AsFloat);

        otFuncArcSin:
          FEvalValue.FloatVal := arcsin(Param[0].AsFloat);

        otFuncCos:
          FEvalValue.FloatVal := cos(Param[0].AsFloat);

        otFuncArcCos:
          FEvalValue.FloatVal := arccos(Param[0].AsFloat);

        otFuncLRE:
          begin
    {       From Stata: http://www.stata.com/support/cert/nist/

            The National Institute of Standards and Technology (NIST) writes,

            In response to industrial concerns about the numerical accuracy of computations from statistical software,
            the Statistical Engineering and Mathematical and Computational Sciences Divisions of NIST’s Information Technology Laboratory
            are providing datasets with certified values for a variety of statistical methods.

            These datasets are known as the NIST StRD—Standard Reference Data. See the NIST StRD web page for detailed
            descriptions of these datasets and tests.

            Below are presented the results of running these tests on Stata.

            In reporting comparisons, it is popular to report the LRE—the log relative error.
            Let c represent a calculated result and t the answer supplied by NIST. The formal definition of this comparison is

                LRE = min( 15, -log10(|c-t|/t) ) if |t|!=0
                LRE = min( 15, -log10(|c-t|) ) otherwise.

            The result of this calculation is then called “Digits of Accuracy” or, more precisely, “Decimal Digits of Accuracy”;
            it counts the number of digits in common with the true value (higher values are obviously better).
            Note that LRE cannot exceed 15.
    }

            // c = Param[0]
            // t = Param[1]
            c := Param[0].AsFloat;
            t := Param[1].AsFloat;

            if (t <> 0) then
              FEvalValue.FloatVal := min(15, -log10(abs((c - t) / t)))
            else
              FEvalValue.FloatVal := min(15, -log10(abs(c - t)));
          end;
      end;
  end;
end;

{
function TEpiScriptFunction_MathFunctions.AsBoolean: Boolean;
var
  E: Extended;
begin
  Result := inherited AsBoolean;

  case FOp of
    otFuncSameValue:
      if FParamList.Count =  2 then
        result := SameValue(Param[0].AsFloat, param[1].AsFloat)
      else
        begin
          E := power(10, -Param[2].AsInteger);
          result := SameValue(Param[0].AsFloat, param[1].AsFloat, E);
        end;


  end;
end;

function TEpiScriptFunction_MathFunctions.AsInteger: EpiInteger;
var
  i: Integer;
  P: TExpr;
begin
  result := inherited AsInteger;

  case FOp of
    otFuncAbs:
      Result := Abs(Param[0].AsInteger);
    otFuncRandom:
      result := Random(Param[0].AsInteger);
    otFuncSum:
      begin
        result := 0;

        for i := 0 to FParamList.Count - 1 do
          begin
            P := Param[i];
            if (not (P.IsMissing or P.IsUserMissing)) then
              Result := Result + P.AsInteger;
          end;
      end;
  end;
end;

function TEpiScriptFunction_MathFunctions.AsFloat: EpiFloat;
var
  c, t: EpiFloat;
  i: Integer;
  P: TExpr;
begin
  // Do not call inherited here, because we may end up calling Abs()
  // twice - one time in AsInteger and one time here. This is NOT allowed
  // if the calling of Param[0]... result in computational changes.
  // Hence we need to set ASTCurrentExecutionObject manually!
  ASTCurrentExecutionObject := self;

  case FOp of
    otFuncAbs:
      Result := Abs(Param[0].AsFloat);

    otFuncExp:
      Result := exp(Param[0].AsFloat);

    otFuncFraction:
      Result := frac(Param[0].AsFloat);

    otFuncLn:
      result := ln(Param[0].AsFloat);

    otFuncLog:
      result := log10(Param[0].AsFloat);

    otFuncSqrt:
      Result := sqrt(Param[0].AsFloat);

    otFuncRound:
      result := RoundTo(Param[0].AsFloat, -Param[1].AsInteger);

    otFuncTan:
      result := tan(Param[0].AsFloat);

    otFuncArcTan:
      result := arctan(Param[0].AsFloat);

    otFuncSin:
      result := sin(Param[0].AsFloat);

    otFuncArcSin:
      result := arcsin(Param[0].AsFloat);

    otFuncCos:
      result := cos(Param[0].AsFloat);

    otFuncArcCos:
      result := arccos(Param[0].AsFloat);

    otFuncLRE:
      begin
{       From Stata: http://www.stata.com/support/cert/nist/

        The National Institute of Standards and Technology (NIST) writes,

        In response to industrial concerns about the numerical accuracy of computations from statistical software,
        the Statistical Engineering and Mathematical and Computational Sciences Divisions of NIST’s Information Technology Laboratory
        are providing datasets with certified values for a variety of statistical methods.

        These datasets are known as the NIST StRD—Standard Reference Data. See the NIST StRD web page for detailed
        descriptions of these datasets and tests.

        Below are presented the results of running these tests on Stata.

        In reporting comparisons, it is popular to report the LRE—the log relative error.
        Let c represent a calculated result and t the answer supplied by NIST. The formal definition of this comparison is

            LRE = min( 15, -log10(|c-t|/t) ) if |t|!=0
            LRE = min( 15, -log10(|c-t|) ) otherwise.

        The result of this calculation is then called “Digits of Accuracy” or, more precisely, “Decimal Digits of Accuracy”;
        it counts the number of digits in common with the true value (higher values are obviously better).
        Note that LRE cannot exceed 15.
}

        // c = Param[0]
        // t = Param[1]
        c := Param[0].AsFloat;
        t := Param[1].AsFloat;

        if (t <> 0) then
          result := min(15, -log10(abs((c - t) / t)))
        else
          result := min(15, -log10(abs(c - t)));
      end;

    otFuncSum:
      begin
        result := 0;

        for i := 0 to FParamList.Count - 1 do
          begin
            P := Param[i];
            if (not (P.IsMissing or P.IsUserMissing)) then
              Result := Result + P.AsFloat;
          end;
      end;
  else
    result := inherited AsFloat;
  end;
end;
  }
function TEpiScriptFunction_MathFunctions.IsMissing: Boolean;
begin
  ASTCurrentExecutionObject := self;

  case FOp of
    otFuncAbs,
    otFuncExp,
    otFuncFraction,
    otFuncSqrt,
    otFuncRandom,
    otFuncRound,
    otFuncTan,
    otFuncArcTan,
    otFuncSin,
    otFuncCos:
      result := Param[0].IsMissing;
    otFuncArcCos,
    otFuncArcSin:
      result := (Param[0].AsFloat < -1) or (Param[0].AsFloat > 1);
    otFuncLn,
    otFuncLog:
      result := Param[0].IsMissing or
                (Param[0].AsFloat = 0);
    otFuncSameValue,
    otFuncLRE:
      result := Param[0].IsMissing or Param[1].IsMissing;
    otFuncSum:
      Result := false;
  end;
end;

end.

