unit epi_script_function_systemfunctions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ast, epidatafilestypes, ast_types, options_hashmap;

type

  { TEpiScriptFunction_SystemFunctions }

  // exist( <var> )
  // idtype( <var> )
  // datatype( <var> )
  // size( <var> )
  // label( <var> )
  // iif( <bool-expr>, <result true>, <result false>)
  // cwd()

  TEpiScriptFunction_SystemFunctions = class(TFunctionCall)
  private
    FOp: TParserOperationType;
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(Const AOperation: TParserOperationType; const ParamList: TParamList);
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    function ResultType: TASTResultType; override;
    function Evaluate: boolean; override;
    function AsInteger: ASTInteger; override;
    function AsString: EpiString; override;
    function AsDate: EpiDate; override;
    function AsTime: EpiDateTime; override;
    function AsFloat: ASTFloat; override;
    function AsBoolean: Boolean; override;
  end;

implementation

uses
  result_variables, typinfo, epimiscutils, LazFileUtils;

type

  { TSystemFunctionParameterList }

  TSystemFunctionParameterList = class(TParamList)
  private
    FFunc: TEpiScriptFunction_SystemFunctions;
  public
    constructor Create(Func: TEpiScriptFunction_SystemFunctions);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
  end;

{ TSystemFunctionParameterList }

constructor TSystemFunctionParameterList.Create(
  Func: TEpiScriptFunction_SystemFunctions);
begin
  inherited Create;
  FFunc := Func;
end;

function TSystemFunctionParameterList.TypeCheck(Parser: IEpiTypeChecker
  ): boolean;
var
  i: Integer;
  P: TExpr;
  EV: TCustomExecutorVariable;
begin
  result := true;

  for i := 0 to Count - 1 do
  begin
    P := Param[i];

    if P.InheritsFrom(TVariable) then
    begin
      EV := FFunc.FExecutor.GetExecVariable(TVariable(P).Ident);
      if (not Assigned(EV)) then
      begin
        DoTypeCheckError('Identifier "%s" not found!', [TVariable(P).Ident], Parser);
        result := false;
        Exit;
      end;
    end;

    if P.InheritsFrom(TReferencedVariable)
    then
      begin
        result := result and P.TypeCheck(Parser);
      end
    else if P.InheritsFrom(TIndexVariable)
    then
      begin
        DoTypeCheckError('Can not read an indexed variable as an identifier!' + LineEnding +
                         'Add "@" before the identifier if you want to use the content of the index as an identifier' + LineEnding +
                         'eg.: @' + P.AsIdent + '[...]',
                         Parser);
        result := false;
      end
    else if P.InheritsFrom(TVariable) then
      result := true
    else
      begin
        result := result and
                  P.TypeCheck(Parser) and
                  (P.ResultType = rtString);
      end;
  end;
end;


{ TEpiScriptFunction_SystemFunctions }

function TEpiScriptFunction_SystemFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;

  case FOp of
    otFuncIif: result[0] := 3;

    otFuncIdentExists,
    otFuncIdentType,
    otFuncSize,
    otFuncDataType,
    otFuncLabel:
      Result[0] := 1;

    otFuncCwd:
      Result[0] := 0;
  end;
end;

function TEpiScriptFunction_SystemFunctions.ParamAcceptType(ParamNo: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited ParamAcceptType(ParamNo);

  case FOp of
    otFuncIif:
      begin
        if (ParamNo = 0) then
          result.ResultTypes := [rtBoolean]
        else
          result.ResultTypes := AllResultDataTypes + [rtAny];
      end;

    otFuncIdentExists:
      begin
        result.ExecutorVariableTypes := ExecutorVariableTypesAll;
        result.ResultTypes           := [rtObject];
        result.Flags                 := [evfInternal, evfExternal, evfAsObject];
      end;

    otFuncIdentType,
    otFuncSize,
    otFuncDataType:
      begin
        result.ExecutorVariableTypes := ExecutorVariableTypesAll;
        result.ResultTypes           := [rtObject];
        result.Flags                 := [evfInternal, evfAsObject];
      end;

    otFuncLabel:
      begin
        result.ExecutorVariableTypes := [evtDataset, evtField];
        result.ResultTypes           := [rtObject];
        result.Flags                 := [evfInternal, evfAsObject];
      end;

    otFuncCwd:
      begin
        result.ExecutorVariableTypes := [];
        result.ResultTypes           := [];
        Result.ResultTypes           := [rtUndefined];
      end;
  end;
end;

constructor TEpiScriptFunction_SystemFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_SystemFunctions.TypeCheck(
  TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean;
begin
  Result := inherited TypeCheck(TypeChecker, TypesAndFlags);
  if (not Result) then Exit;

  case FOp of
    otFuncIif:
      begin
        if (Param[1].ResultType <> Param[2].ResultType) and
           (Param[1].ResultType <> rtAny) and
           (Param[2].ResultType <> rtAny)
        then
          begin
            DoTypeCheckError(
              'True value and False value must have the same result type:' + LineEnding +
                'true: '  + ASTResultTypeString[Param[1].ResultType] + LineEnding +
                'false: ' + ASTResultTypeString[Param[2].ResultType],
               TypeChecker
            );
            Result := false;
            Exit;
          end;
      end;

    otFuncLabel,
    otFuncIdentExists,
    otFuncIdentType,
    otFuncSize,
    otFuncDataType:
      ;
  end;
end;

function TEpiScriptFunction_SystemFunctions.ResultType: TASTResultType;
begin
  case FOp of
    otFuncIdentExists:
      result := rtBoolean;

    otFuncIdentType,
    otFuncDataType,
    otFuncSize:
      result := rtInteger;

    otFuncIif:
      if Param[1].ResultType = rtAny then
        result := Param[2].ResultType
      else
        result := Param[1].ResultType;

    otFuncLabel,
    otFuncCwd:
      result := rtString;
  end;
end;

function TEpiScriptFunction_SystemFunctions.Evaluate: boolean;
var
  lParam: TExpr;
  ExecVar: TCustomExecutorVariable;
begin
  Result := inherited Evaluate;

  case FOp of
    otFuncIdentExists:
      FEvalValue.BoolVal := Assigned(FExecutor.GetExecVariable(Param[0].AsIdent));

    otFuncIdentType:
      begin
        lParam := Param[0];
        FEvalValue.IntVal := Integer(FExecutor.GetVariableExecType(lParam.AsIdent));
        FEvalValue.StringVal := ExecutorVariableTypeString[TExecutorVariableType(FEvalValue.IntVal)];
      end;

    otFuncDataType:
      begin
        lParam := Param[0];
        ExecVar := FExecutor.GetExecVariable(lParam.AsIdent);

        case ExecVar.VarType of
          evtGlobal,
          evtGlobalVector,
          evtField,
          evtResultConst,
          evtResultVector,
          evtResultMatrix:
            FEvalValue.IntVal := Integer(TCustomExecutorDataVariable(ExecVar).DataType);

          evtValuelabel:
            FEvalValue.IntVal := Integer(TExecutorValuelabelsetVariable(ExecVar).Valuelabelset.LabelType);
        else
          FEvalValue.IntVal := -1;
        end;

        if (FEvalValue.IntVal >= 0) then
          FEvalValue.StringVal := EpiTypeNames[TEpiFieldType(I)]
        else
          FEvalValue.StringVal := 'Identifier has no data type';
      end;

    otFuncSize:
      begin
        lParam := Param[0];
        ExecVar := FExecutor.GetExecVariable(lParam.AsIdent);

        case ExecVar.VarType of
          evtGlobal:
            FEvalValue.IntVal := 1;

          evtGlobalVector:
            FEvalValue.IntVal := TExecVarGlobalVector(ExecVar).Length;

          evtField:
            FEvalValue.IntVal := TExecVarField(ExecVar).Length;

          evtDataset:
            FEvalValue.IntVal := TExecutorDatasetVariable(ExecVar).DataFile.Size;

          evtValuelabel:
            FEvalValue.IntVal := TExecutorValuelabelsetVariable(ExecVar).Valuelabelset.Count;

          evtResultConst:
            FEvalValue.IntVal := 1;

          evtResultVector:
            FEvalValue.IntVal := TExecVarVector(ExecVar).Length;

          evtResultMatrix:
           FEvalValue.IntVal result := -1; // TExecVarMatrix(ExecVar).Rows;
        end;
      end;

    otFuncIif:
      if Param[0].AsBoolean then
        begin
          FEvalValue.IntVal := Param[1].AsInteger;
          FEvalValue.StringVal := Param[1].AsString;
          FEvalValue.FloatVal := Param[1].AsFloat;
          FEvalValue.DateVal := Param[1].AsDate;
          FEvalValue.TimeVal := Param[1].AsTime;
          FEvalValue.BoolVal := Param[1].AsBoolean;
        end
      else
        begin
          FEvalValue.IntVal := Param[2].AsInteger;
          FEvalValue.StringVal := Param[2].AsString;
          FEvalValue.FloatVal := Param[2].AsFloat;
          FEvalValue.DateVal := Param[2].AsDate;
          FEvalValue.TimeVal := Param[2].AsTime;
          FEvalValue.BoolVal := Param[2].AsBoolean;
        end;

    otFuncLabel:
      begin
        lParam := Param[0];
        ExecVar := FExecutor.GetExecVariable(lParam.AsIdent);

        case ExecVar.VarType of
          evtField:
            result := TExecVarField(ExecVar).Field.Question.Text;

          evtDataset:
            result := TExecutorDatasetVariable(ExecVar).DataFile.Caption.Text;

        else
          result := '';
        end;
      end;

    otFuncCwd:
      result := GetCurrentDirUTF8;
  end;
end;
{
function TEpiScriptFunction_SystemFunctions.AsInteger: ASTInteger;
var
  lParam: TExpr;
  ExecVar: TCustomExecutorVariable;
begin
  Result := inherited AsInteger;

  case FOp of
    otFuncIdentType:
      begin
        lParam := Param[0];
        result := Integer(FExecutor.GetVariableExecType(lParam.AsIdent));
      end;

    otFuncDataType:
      begin
        lParam := Param[0];
        ExecVar := FExecutor.GetExecVariable(lParam.AsIdent);

        case ExecVar.VarType of
          evtGlobal,
          evtGlobalVector,
          evtField,
          evtResultConst,
          evtResultVector,
          evtResultMatrix:
            result := Integer(TCustomExecutorDataVariable(ExecVar).DataType);

          evtValuelabel:
            result := Integer(TExecutorValuelabelsetVariable(ExecVar).Valuelabelset.LabelType);
        else
          result := -1;
        end;
      end;

    otFuncSize:
      begin
        lParam := Param[0];
        ExecVar := FExecutor.GetExecVariable(lParam.AsIdent);

        case ExecVar.VarType of
          evtGlobal:
            result := 1;

          evtGlobalVector:
            result := TExecVarGlobalVector(ExecVar).Length;

          evtField:
            result := TExecVarField(ExecVar).Length;

          evtDataset:
            result := TExecutorDatasetVariable(ExecVar).DataFile.Size;

          evtValuelabel:
            result := TExecutorValuelabelsetVariable(ExecVar).Valuelabelset.Count;

          evtResultConst:
            result := 1;

          evtResultVector:
            result := TExecVarVector(ExecVar).Length;

          evtResultMatrix:
            result := -1; // TExecVarMatrix(ExecVar).Rows;
        end;
      end;

    otFuncIif:
      if Param[0].AsBoolean then
        result := Param[1].AsInteger
      else
        result := Param[2].AsInteger;
  end;
end;

function TEpiScriptFunction_SystemFunctions.AsString: EpiString;
var
  I: ASTInteger;
  lParam: TExpr;
  ExecVar: TCustomExecutorVariable;
begin
  Result := inherited AsString;

  case FOp of
    otFuncIdentType:
      result := ExecutorVariableTypeString[TExecutorVariableType(AsInteger)];

    otFuncDataType:
      begin
        I := AsInteger;
        if (i >= 0) then
          result := EpiTypeNames[TEpiFieldType(I)]
        else
          result := 'Identifier has no data type';
      end;

    otFuncIif:
      if Param[0].AsBoolean then
        result := Param[1].AsString
      else
        result := Param[2].AsString;

    otFuncLabel:
      begin
        lParam := Param[0];
        ExecVar := FExecutor.GetExecVariable(lParam.AsIdent);

        case ExecVar.VarType of
          evtField:
            result := TExecVarField(ExecVar).Field.Question.Text;

          evtDataset:
            result := TExecutorDatasetVariable(ExecVar).DataFile.Caption.Text;

        else
          result := '';
        end;
      end;

    otFuncCwd:
      result := GetCurrentDirUTF8;
  end;
end;

function TEpiScriptFunction_SystemFunctions.AsDate: EpiDate;
begin
  Result := inherited AsDate;

  case FOp of
    otFuncIif:
      if Param[0].AsBoolean then
        result := Param[1].AsDate
      else
        result := Param[2].AsDate;
  end
end;

function TEpiScriptFunction_SystemFunctions.AsTime: EpiDateTime;
begin
  Result := inherited AsTime;

  case FOp of
    otFuncIif:
      if Param[0].AsBoolean then
        result := Param[1].AsTime
      else
        result := Param[2].AsTime;
  end
end;

function TEpiScriptFunction_SystemFunctions.AsFloat: ASTFloat;
begin
  Result := inherited AsFloat;

  case FOp of
    otFuncIif:
      if Param[0].AsBoolean then
        result := Param[1].AsFloat
      else
        result := Param[2].AsFloat;
  end
end;

function TEpiScriptFunction_SystemFunctions.AsBoolean: Boolean;
begin
  Result := inherited AsBoolean;

  case FOp of
    otFuncIif:
      if Param[0].AsBoolean then
        result := Param[1].AsBoolean
      else
        result := Param[2].AsBoolean;

    otFuncIdentExists:
      Result := Assigned(FExecutor.GetExecVariable(Param[0].AsIdent));
  end
end;
}
end.

