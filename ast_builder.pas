unit ast_builder;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, ast_types, Token, epidatafilestypes;

type

  { TASTBuilder }

  TASTBuilder = class
  private
    FExecutor:    IEpiScriptExecutor;
    FError:       boolean;
    FErrorToken:  TToken;
    FErrorMsg:    UTF8String;
    procedure DoError(Const Msg: UTF8String; Token: TToken);
  private
    // Common
    function FamilyIdents(R: TReduction; Supported: TCrudCommands = AllCrudCommands): TCrudCommand;

    // Statements
    function DoStatementList(R: TReduction; OwnerList: TStatementList): TStatementList;
    function DoStatement(R: TReduction): TCustomStatement;
    function DoIfThen(R: TReduction): TIfThen;
    function DoOptionalElse(R: TReduction): TCustomStatement;
    function DoSelect(R: TReduction): TSelect;
    function DoFor(R: TReduction): TFor;
    function DoUse(R: TReduction): TUse;
    function DoEvalExpression(R: TReduction): TCustomStatement;
    function DoAssertCommand(R: TReduction): TAssertCommand;
    function DoSetCommand(R: TReduction): TSetCommand;
    function DoAssignment(R: TReduction): TAssignment;
    function DoFunctionCall(R: TReduction): TFunctionCall;
    function DoCrudCmds(R: TReduction): TCustomStatement;
    function DoCheckCmd(R: TReduction): TCustomStatement;
    function DoReportCmd(R: TReduction): TCustomStatement;
    function DoRecodeCommand(R: TReduction): TCustomCommand;
    function DoOptionalAssignment(R: TReduction): TExpr;
  private
    // Recode
    function DoOptionalRecodeTo(R: TReduction): TCustomVariable;
    function DoRecodeIntervaList(R: TReduction): TValueLabelPairs;
    function DoRecodeInterval(R: TReduction): TExpr;
    function DoInterval(R: TReduction): TExpr;

  private
    FCrudErrorToken: TToken;
    // Crud commands
    function DoSimpleCrud(R: TReduction): TCustomStatement;
    // - new
    function DoNew(R: TReduction): TCustomNew;
    function DoNewVarOrGlobal(SubCmd: TCrudCommand; R: TReduction; OptionList: TOptionList): TCustomNewValued;
    function DoNewValueLabel(R: TReduction; OptionList: TOptionList): TNewValuelabel;
    function DoValueLabelPairs(R: TReduction; OwnerList: TValueLabelPairs): TValueLabelPairs;
    function DoNewDataset(R: TReduction; OptionList: TOptionList): TNewDataset;
    function DoNewProject(R: TReduction; OptionList: TOptionList): TNewProject;
    // - edit
    function DoEdit(R: TReduction): TCustomCrudCommand;
    function DoEditProject(R: TReduction; OptList: TOptionList): TEditProject;
    function DoEditDataset(R: TReduction; OptList: TOptionList): TEditDataset;
    function DoEditVariable(R: TReduction; OptList: TOptionList): TEditVariable;
    function DoEditValuelabel(R: TReduction; OptList: TOptionList): TEditValueLabel;
    function DoEditData(R: TReduction; OptList: TOptionList): TEditData;

  private
    // Commands
    function DoVariableCommand(R: TReduction): TCustomVariableCommand;
    function DoStringCommand(R: TReduction): TCustomStringCommand;
    function DoEmptyCommand(R: TReduction): TCustomEmptyCommand;
    function DoSystemCommand(R: TReduction): TCustomStringCommand;

    function DoCommandName(R: TReduction): UTF8String;

    // Reference Or String
    function DoOptionalReferenceOrString(R: TReduction): TExpr;
    function DoReferenceOrString(R: TReduction): TExpr;
    function DoOptionalString(R: TReduction): TExpr;

    // Variables
    function DoOptionalIndexedVariableList(R: TReduction; OwnerList: TVariableList = nil): TVariableList;
    function DoIndexedVariableList(R: TReduction; OwnerList: TVariableList): TVariableList;
    function DoOptionalVariableList(R: TReduction; OwnerList: TVariableList = nil): TVariableList;
    function DoVariableList(R: TReduction; OwnerList: TVariableList): TVariableList;
    function DoIndexVariable(R: TReduction): TCustomVariable;
    function DoReferencedVariable(R: TReduction): TCustomVariable;
    function DoVariable(R: TReduction): TCustomVariable;

    // Options
    function DoOptionList(R: TReduction; OwnerList: TOptionList): TOptionList;
    function DoOption(R: TReduction): TOption;

    // expressions
    function DoOptionalExpressionList(R: TReduction; OwnerList: TParamList): TParamList;
    function DoExpressionList(R: TReduction; OwnerList: TParamList): TParamList;
    function DoOptionalExpression(R: TReduction): TExpr;
    function DoExpression(R: TReduction): TExpr;
    function DoAddExpression(R: TReduction): TExpr;
    function DoMultExpression(R: TReduction): TExpr;
    function DoExponentialExpression(R: TReduction): TExpr;
    function DoNegateExpression(R: TReduction): TExpr;
    function DoValue(R: TReduction): TExpr;
    function DoArray(R: TReduction): TExpr;

  public
    constructor Create(AExecutor: IEpiScriptExecutor);
    destructor Destroy; override;
    function BuildAST(TheProgram: TReduction; out AST: TStatementList): boolean;
    function BuildOptionList(TheProgram: TReduction; out AOptionList: TOptionList): boolean;
    property Error: boolean read FError;
    property ErrorMsg: UTF8String read FErrorMsg;
    property ErrorToken: TToken read FErrorToken;
  end;

implementation

uses
  Symbol, LazUTF8;

const
  ASTFormatSettings: TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator:  '.';
    CurrencyDecimals:    2;
    DateSeparator:     '-';
    TimeSeparator:     ':';
    ListSeparator:     ',';
    CurrencyString:    '$';
    ShortDateFormat:   'd/m/y';
    LongDateFormat:    'dd" "mmmm" "yyyy';
    TimeAMString:      'AM';
    TimePMString:      'PM';
    ShortTimeFormat:   'hh:nn';
    LongTimeFormat:    'hh:nn:ss';
    ShortMonthNames:   ('Jan','Feb','Mar','Apr','May','Jun',
                        'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames:    ('January','February','March','April','May','June',
                        'July','August','September','October','November','December');
    ShortDayNames:     ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:      ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );

{ TASTBuilder }

procedure TASTBuilder.DoError(const Msg: UTF8String; Token: TToken);
begin
  FError := true;
  FErrorMsg := Msg;
  FErrorToken := Token;
end;

function TASTBuilder.FamilyIdents(R: TReduction; Supported: TCrudCommands
  ): TCrudCommand;
var
  CC: TCrudCommand;
  T: TToken;
  S: String;
begin
  // R := <Variable>
  T := R.Tokens[0];
  S := UTF8LowerString(T.DataVar);

  case S of
    'd', 'data':
      result := ccData;

    'g', 'global':
      result := ccGlobal;

    'v', 'var', 'variable':
      result := ccVariable;

    'ds', 'dataset':
      result := ccDataset;

    'p', 'project':
      result := ccProject;

    'vl', 'valuelabel':
      result := ccValuelabel;

    'r', 'res', 'result':
      result := ccResult;
  else
    DoError('Unknown sub-command: ' + T.DataVar, T);
    Exit;
  end;

  if (not (result in Supported)) then
    begin
      S := '';

      for CC in Supported do
        S := S + ', ' + CrudCommandString[CC];

      Delete(S, 1, 2);

      DoError('Unsupported sub-command: ' + CrudCommandString[Result] + LineEnding +
              'Supported sub-commands are: ' + S,
              T
             );
    end;
end;

function TASTBuilder.DoStatementList(R: TReduction; OwnerList: TStatementList
  ): TStatementList;
begin
//  <Statement List>  ::= <Statement> ';' <Statement List>
//                     |
  result := nil;

  if Assigned(OwnerList) then
    Result := OwnerList
  else
    Result := TStatementList.Create;

  if R.TokenCount > 0 then
    Result.AddStatement(DoStatement(R.Tokens[0].Reduction));

  if FError then exit;

  if (R.TokenCount = 3) then
    Result := DoStatementList(R.Tokens[2].Reduction, Result);
end;

function TASTBuilder.DoStatement(R: TReduction): TCustomStatement;
begin
{
<Statement>      ::= 'begin' <Statement List> 'end'
                 |   'if' <Expression> 'then' <Statement> <Optional Else>
                 |   'select' <Expression> <Statement>
                 |   'for' <Variable> ':=' <Expression> <For Direction> <Expression> 'do' <Statement>
                 |   <Function Call>
                 |   <Crud Commands>
                 |   opUse <Referenced Variable> <Option List>
                 |   '?' <Expression>
                 |   opAssert <Expression> <Option List>
                 |   opSet <Optional Reference Or String> <Optional Assignment>
                 |   opRecode <Referenced Variable> <Optional Recode To> <Recode By Or Intervals> <Option List>
                 |   <Indexed Variable> ':=' <Expression>
                 |   <Variable Command> <Optional Referenced Variable List> <Option List>
                 |   <String Command> <Optional Expression> <Option List>
                 |   <Empty Command> <Option List>
                 |   <System Command> <Optional String>
                 |   ! This is to accomodate empty commands
}
  result := nil;

  // The empty command ";"
  if (R.TokenCount = 0) then
    begin
      Result := TEmptyStatement.Create;
      Exit;
    end;


  case R.Tokens[0].Name of
    'begin':
      begin
        Result := DoStatementList(R.Tokens[1].Reduction, nil);
        Result.AssignToken(R.Tokens[0]);
      end;

    'if':
      Result := DoIfThen(R);

    'select':
      Result := DoSelect(R);

    'for':
      Result := DoFor(R);

    'Function Call':
      Result := DoFunctionCall(R.Tokens[0].Reduction);

    'Crud Commands':
      result := DoCrudCmds(R.Tokens[0].Reduction);

    'Check Command':
      result := DoCheckCmd(R.Tokens[0].Reduction);

    'Report Command':
      result := DoReportCmd(R.Tokens[0].Reduction);

    'opUse':
      Result := DoUse(R);

    '?':
      Result := DoEvalExpression(R);

    'opAssert':
      Result := DoAssertCommand(R);

    'opSet':
      Result := DoSetCommand(R);

    'opRecode':
      Result := DoRecodeCommand(R);

    'Indexed Variable':
      Result := DoAssignment(R);

    'Variable Command':
      Result := DoVariableCommand(R);

    'String Command':
      Result := DoStringCommand(R);

    'Empty Command':
      Result := DoEmptyCommand(R);

    'System Command':
      Result := DoSystemCommand(R);
  else
    DoError('AST_Builder: Failed DoStatement', R.Tokens[0]);
    result := nil;
  end;
end;

function TASTBuilder.DoIfThen(R: TReduction): TIfThen;
var
  Expression: TExpr;
  ThenStatement, ElseStatement: TCustomStatement;
begin
// <Statement>       ::= 'if' <Expression> 'then' <Statement> <Optional Else>
  result := nil;

  Expression    := DoExpression(R.Tokens[1].Reduction);
  ThenStatement := DoStatement(R.Tokens[3].Reduction);
  ElseStatement := DoOptionalElse(R.Tokens[4].Reduction);

  Result := TIfThen.Create(Expression, ThenStatement, ElseStatement);

  if (Assigned(Result)) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoOptionalElse(R: TReduction): TCustomStatement;
begin
//  <Optional Else>   ::= 'else' <Statement>
//                     |
  Result := nil;

  if (R.TokenCount > 0) then
    begin
      Result := DoStatement(R.Tokens[1].Reduction);

      if Assigned(Result) then
        Result.AssignToken(R.Tokens[0]);
    end;
end;

function TASTBuilder.DoSelect(R: TReduction): TSelect;
var
  Expr: TExpr;
  Statement: TCustomStatement;
begin
//  <Statement>       ::= 'select' <Expression> 'do' <Statement>
  result := nil;

  Result := TSelect.Create(
    DoExpression(R.Tokens[1].Reduction),
    DoStatement(R.Tokens[3].Reduction)
  );

  if (Assigned(Result)) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoFor(R: TReduction): TFor;
begin
//   Token Idx:        0           1            2        3              4            5        6       7
//  <Statement> ::=  'for' <Indexed Variable> ':=' <Expression> <For Direction> <Expression> 'do' <Statement>
//              |    'for' <Indexed Variable> 'in'    <Array>          'do'      <Statement>
  result := nil;

  if (R.TokenCount = 6) then
    begin
      Result := TFor.Create(
        DoIndexVariable(R.Tokens[1].Reduction),
        TArray(DoArray(R.Tokens[3].Reduction)),
        DoStatement(R.Tokens[5].Reduction)
      );
    end
  else
    begin
      Result := TFor.Create(
        DoIndexVariable(R.Tokens[1].Reduction),
        DoExpression(R.Tokens[3].Reduction),
        DoExpression(R.Tokens[5].Reduction),
        R.Tokens[4].Reduction.Tokens[0].DataVar,
        DoStatement(R.Tokens[7].Reduction)
      );
    end;

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoNew(R: TReduction): TCustomNew;
var
  ft: TEpiFieldType;
  VL: TVariableList;
  IdxVar: TCustomVariable;
  OptList: TOptionList;
  IdxToken: TToken;
  SubCmd: TCrudCommand;
  S: String;
begin
//    R                     0       1            2              3
//  <Crud Commands>   ::= opNew <Variable> <New SubSystem> <Option List>

  result := nil;

  IdxVar   := DoVariable(R.Tokens[1].Reduction);
  SubCmd   := FamilyIdents(R.Tokens[1].Reduction, [ccProject, ccDataset, ccVariable, ccGlobal, ccValuelabel]);
  OptList  := DoOptionList(R.Tokens[3].Reduction, nil);
  FCrudErrorToken := TToken.Create(IdxVar.LineNo, IdxVar.ColNo, IdxVar.ByteNo);

  case SubCmd of
    ccProject:
      result := DoNewProject(R.Tokens[2].Reduction, OptList);

    ccDataset:
      result := DoNewDataset(R.Tokens[2].Reduction, OptList);

    ccVariable,
    ccGlobal:
      result := DoNewVarOrGlobal(SubCmd, R.Tokens[2].Reduction, OptList);

    ccValuelabel:
      result := DoNewValueLabel(R.Tokens[2].Reduction, OptList);
  else
    Exit;
  end;

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoNewVarOrGlobal(SubCmd: TCrudCommand; R: TReduction;
  OptionList: TOptionList): TCustomNewValued;
var
  TypeVar, NameVar: TCustomVariable;
  Ft: TEpiFieldType;
begin
  {                                   0               1              2
                                     name            type         ---------
  <New SubSystem>    ==>   ::= <Indexed Variable> <Variable> <Optional Assignment> ! New global (vector), variable
                           |   <Indexed Variable> <Variable> <Value Label Pairs>   ! New value label
                           |   <Indexed Variable>                                  ! New dataset
                           |                                                       ! New project
  }
  result := nil;

  // Project
  if R.TokenCount = 0 then
    begin
      DoError(Format('New %s: missing datatype and variable name!' + LineEnding +
                     'Correct syntax: new %s <name> <type> [:= <value>] [!options...];',
                     [CrudCommandString[SubCmd], CrudCommandString[SubCmd]]),
              FCrudErrorToken); // DoError
      Exit;
    end;

  NameVar := DoIndexVariable(R.Tokens[0].Reduction);

  // Dataset
  if R.TokenCount = 1 then
    begin
      DoError(Format('New %s: missing datatype or variable name!' + LineEnding +
                     'Correct syntax: new %s <name> <type> [:= <value>] [!options...];',
                     [CrudCommandString[SubCmd], CrudCommandString[SubCmd]]),
                     TToken.Create(NameVar.LineNo, NameVar.ColNo, NameVar.ByteNo)
      );
      Exit;
    end;

  // Valuelabels
  if (R.Tokens[2].Name = 'Value Label Pairs') then
    begin
      DoError(Format('New %s: incorrect syntax!' + LineEnding +
                     'Correct syntax: new %s <name> <type> [:= <value>] [!options...];',
                     [CrudCommandString[SubCmd], CrudCommandString[SubCmd]]),
                     TToken.Create(NameVar.LineNo, NameVar.ColNo, NameVar.ByteNo)
      );
      Exit;
    end;

  TypeVar := DoVariable(R.Tokens[1].Reduction);
  if (not EpiFieldTypeFromString(TypeVar.Ident, Ft)) then
    begin
      DoError(Format('Unknown %s type: %s',
                     [CrudCommandString[SubCmd], TypeVar.Ident]),
                     TToken.Create(TypeVar.LineNo, TypeVar.ColNo, TypeVar.ByteNo)
      );
      Exit;
    end;

  case SubCmd of
    ccVariable:
      begin
        if (NameVar is TIndexVariable) then
          begin
            DoError(Format('New %s: Cannot use index on variables! ' + LineEnding +
                           'Corrent syntax: new %s <name> <type> [:= <value>] [!options...];',
                           [CrudCommandString[SubCmd], CrudCommandString[SubCmd]]),
                           TToken.Create(NameVar.LineNo, NameVar.ColNo, NameVar.ByteNo)
            );
            Exit;
          end;

        Result := TNewVariable.Create(
                    DoOptionalAssignment(R.Tokens[2].Reduction),
                    ft,
                    NameVar,
                    OptionList
                  );

      end;

    ccGlobal:
      begin
        if (NameVar is TIndexVariable) then
          begin
            Result := TNewGlobalVector.Create(
                        TIndexVariable(NameVar).Expr[0],
                        DoOptionalAssignment(R.Tokens[2].Reduction),
                        Ft,
                        NameVar,
                        OptionList
                      );
          end
        else
          Result := TNewGlobal.Create(
                      DoOptionalAssignment(R.Tokens[2].Reduction),
                      Ft,
                      NameVar,
                      OptionList
                    );
      end;
  end;
end;

function TASTBuilder.DoNewValueLabel(R: TReduction; OptionList: TOptionList
  ): TNewValuelabel;
var
  TypeVar, NameVar, V: TCustomVariable;
  Ft: TEpiFieldType;
  Expr: TExpr;
begin
  {                                   0               1              2
                                     name            type         ---------
  <New SubSystem>          ::= <Indexed Variable> <Variable> <Optional Assignment> ! New global (vector), variable
                    ==>    |   <Indexed Variable> <Variable> <Value Label Pairs>   ! New value label
                           |   <Indexed Variable>                                  ! New dataset
                           |                                                       ! New project
  }
  result := nil;

  if R.TokenCount = 0 then
    begin
      DoError(Format('New %s: missing datatype and variable name!' + LineEnding +
                     'Corrent syntax: new %s <name> <type> (<value> "<label text>" ...) [!options ...];',
                     [CrudCommandString[ccValuelabel], CrudCommandString[ccValuelabel]]),
              FCrudErrorToken); // DoError
      Exit;
    end;

  NameVar := DoIndexVariable(R.Tokens[0].Reduction);
  if (R.TokenCount = 1) then
    begin
      DoError(Format('New %s: missing datatype or variable name!' + LineEnding +
                     'Corrent syntax: new %s <name> <type> (<value> "<label text>" ...) [!options ...];',
                     [CrudCommandString[ccValuelabel], CrudCommandString[ccValuelabel]]),
                     TToken.Create(NameVar.LineNo, NameVar.ColNo, NameVar.ByteNo)
      );
      Exit;
    end;

  if (R.Tokens[2].Name = 'Optional Assignment') then
    begin
      DoError(Format('New %s: incorrect syntax!' + LineEnding +
                     'Corrent syntax: new %s <name> <type> (<value> "<label text>" ...) [!options ...];',
                     [CrudCommandString[ccValuelabel], CrudCommandString[ccValuelabel]]),
                     TToken.Create(NameVar.LineNo, NameVar.ColNo, NameVar.ByteNo)
      );
      Exit;
    end;

  if (NameVar is TIndexVariable) then
    begin
      DoError(Format('New %s: Index not allowed on variable name!' + LineEnding +
                     'Correct syntax: new %s <name> [!options...];',
                     [CrudCommandString[ccDataset], CrudCommandString[ccDataset]]),
              TToken.Create(V.LineNo, V.ColNo, V.ByteNo)
      );
      Exit;
    end;

  TypeVar := DoVariable(R.Tokens[1].Reduction);
  if (not EpiFieldTypeFromString(TypeVar.Ident, Ft)) then
    begin
      DoError(Format('Unknown %s type: %s',
                     [CrudCommandString[ccValuelabel], TypeVar.Ident]),
              TToken.Create(TypeVar.LineNo, TypeVar.ColNo, TypeVar.ByteNo)
      );
      Exit;
    end;

  result := TNewValuelabel.Create(
              DoValueLabelPairs(R.Tokens[2].Reduction, TValueLabelPairs.Create(FieldTypeToASTTypeTable[Ft])),
              Ft,
              NameVar,
              OptionList
            );
end;

function TASTBuilder.DoValueLabelPairs(R: TReduction;
  OwnerList: TValueLabelPairs): TValueLabelPairs;
var
  Expr: TExpr;
  S: UTF8String;
begin
{                                                  0           1             2
<Value Label Pairs>                        ::= <Expression> String <Value Label Pairs>
                                           |   <Expression> String

}
  if Assigned(OwnerList) then
    result := OwnerList
  else
    result := TValueLabelPairs.Create(rtUndefined);

  // <Expression>
  Expr := DoExpression(R.Tokens[0].Reduction);
  // <String>
  S := R.Tokens[1].DataVar;
  S := Copy(S, 2, Length(S) - 2);

  result.AddPair(Expr, S);

  if R.TokenCount = 3 then
    result := DoValueLabelPairs(R.Tokens[2].Reduction, result);
end;

function TASTBuilder.DoNewDataset(R: TReduction; OptionList: TOptionList
  ): TNewDataset;
var
  V: TCustomVariable;
begin
  {                                                  0           1             2
  <New SubSystem>          ::= <Indexed Variable> <Variable> <Optional Assignment> ! New global (vector), variable
                           |   <Indexed Variable> <Variable> <Value Label Pairs>   ! New value label
                      ==>  |   <Indexed Variable>                                  ! New dataset
                           |                                                       ! New project
  }

  result := nil;

  if R.TokenCount < 1 then
    begin
      DoError(Format('New %s: missing name!' + LineEnding +
                     'Corrent syntax: new %s <name> [!options...];',
                     [CrudCommandString[ccDataset], CrudCommandString[ccDataset]]),
              FCrudErrorToken);
      Exit;
    end;

  if R.TokenCount > 1 then
    begin
      V := DoVariable(R.Tokens[1].Reduction);
      DoError(Format('New %s: too many specifiers! Only 1 required!!' + LineEnding +
                     'Correct syntax: new %s <name> [!options...];',
                     [CrudCommandString[ccDataset], CrudCommandString[ccDataset]]),
              TToken.Create(V.LineNo, V.ColNo, V.ByteNo)
      );
      Exit;
    end;

  V := DoIndexVariable(R.Tokens[0].Reduction);

  if (V is TIndexVariable) then
    begin
      DoError(Format('New %s: Index not allowed on variable name!' + LineEnding +
                     'Correct syntax: new %s <name> [!options...];',
                     [CrudCommandString[ccDataset], CrudCommandString[ccDataset]]),
              TToken.Create(V.LineNo, V.ColNo, V.ByteNo)
      );
      Exit;
    end;

  Result := TNewDataset.Create(V, OptionList);
end;

function TASTBuilder.DoNewProject(R: TReduction; OptionList: TOptionList
  ): TNewProject;
var
  V: TCustomVariable;
begin
{
<New SubSystem>                            ::= <Indexed Variable> <Variable> <Optional Assignment> ! New global (vector), variable
                                           |   <Indexed Variable> <Variable> <Value Label Pairs>   ! New value label
                                           |   <Indexed Variable>                                  ! New dataset
                     ==>>                  |                                                       ! New project
}

  result := nil;

  if R.TokenCount > 0 then
    begin
      V := DoIndexVariable(R.Tokens[0].Reduction);
      DoError(Format('New %s does not accept specifiers or assigment!' + LineEnding +
                     'Correct syntax: new %s [!options...];',
                     [CrudCommandString[ccProject], CrudCommandString[ccProject]]),
              TToken.Create(V.LineNo, V.ColNo, V.ByteNo)
              );
      Exit;
    end;

  Result := TNewProject.Create(OptionList);
end;

function TASTBuilder.DoEdit(R: TReduction): TCustomCrudCommand;
var
  SubCmd: TCrudCommand;
  OptList: TOptionList;
  V: TCustomVariable;
begin
{                        0        1              2              3
  <Crud Commands>  ::= opEdit <Variable> <Edit SubSystem> <Option List>
}
  Result := nil;

  SubCmd := FamilyIdents(R.Tokens[1].Reduction, [ccProject, ccDataset, ccVariable, ccValuelabel, ccData]);
  OptList := DoOptionList(R.Tokens[3].Reduction, nil);
  V := DoVariable(R.Tokens[1].Reduction);
  FCrudErrorToken := TToken.Create(V.LineNo, V.ColNo, V.ByteNo);

  case SubCmd of
    ccProject:
      result := DoEditProject(R.Tokens[2].Reduction, OptList);

    ccDataset:
      result := DoEditDataset(R.Tokens[2].Reduction, OptList);

    ccVariable:
      result := DoEditVariable(R.Tokens[2].Reduction, OptList);

    ccValuelabel:
      result := DoEditValuelabel(R.Tokens[2].Reduction, OptList);

    ccData:
      result := DoEditData(R.Tokens[2].Reduction, OptList);
  end;
end;

function TASTBuilder.DoEditProject(R: TReduction; OptList: TOptionList
  ): TEditProject;
var
  V: TCustomVariable;
begin
{                               0                  1
  <Edit SubSystem>       ::= <Indexed Variable> <Value Label Pairs>        ! Edit value label
                         |   <Indexed Variable>                            ! Edit dataset, variable
                   ==>>  |                                                 ! Edit project, data
}
  result := nil;

  if (R.TokenCount > 0) then
    begin
      V := DoIndexVariable(R.Tokens[0].Reduction);
      DoError(Format('Edit %s: incorrect syntax!' + LineEnding +
                     'Corrent syntax: edit %s [!options...];',
                     [CrudCommandString[ccProject], CrudCommandString[ccProject]]),
              TToken.Create(V.LineNo, V.ColNo, V.ByteNo)
      );
      Exit;
    end;

  result := TEditProject.Create(OptList);
end;

function TASTBuilder.DoEditDataset(R: TReduction; OptList: TOptionList
  ): TEditDataset;
var
  V: TCustomVariable;
begin
  {                               0                       1
    <Edit SubSystem>       ::= <Indexed Variable> <Value Label Pairs>        ! Edit value label
                     ==>>  |   <Indexed Variable>                            ! Edit dataset, variable
                           |                                                 ! Edit project
  }
  Result := nil;

  if (R.TokenCount = 0) then
    begin
      DoError(Format('Edit %s: incorrect syntax!' + LineEnding +
                     'Corrent syntax: edit %s <name> [!options...];',
                     [CrudCommandString[ccDataset], CrudCommandString[ccDataset]]),
              FCrudErrorToken
      );
      Exit;
    end;

  if (R.TokenCount > 1) then
    begin
      V := DoIndexVariable(R.Tokens[0].Reduction);
      DoError(Format('Edit %s: incorrect syntax!' + LineEnding +
                     'Corrent syntax: edit %s <name> [!options...];',
                     [CrudCommandString[ccDataset], CrudCommandString[ccDataset]]),
              TToken.Create(V.LineNo, V.ColNo, V.ByteNo)
      );
      Exit;
    end;

  result := TEditDataset.Create(
    DoIndexVariable(R.Tokens[0].Reduction),
    OptList
  );
end;

function TASTBuilder.DoEditVariable(R: TReduction; OptList: TOptionList
  ): TEditVariable;
var
  V: TCustomVariable;
begin
  {                               0                       1
    <Edit SubSystem>       ::= <Indexed Variable> <Value Label Pairs>        ! Edit value label
                     ==>>  |   <Indexed Variable>                            ! Edit dataset, variable
                           |                                                    ! Edit project
  }
  Result := nil;

  if (R.TokenCount = 0) then
    begin
      DoError(Format('Edit %s: incorrect syntax!' + LineEnding +
                     'Corrent syntax: edit %s <name> [!options...];',
                     [CrudCommandString[ccVariable], CrudCommandString[ccVariable]]),
              FCrudErrorToken
      );
      Exit;
    end;

  if (R.TokenCount > 1) then
    begin
      V := DoIndexVariable(R.Tokens[0].Reduction);
      DoError(Format('Edit %s: incorrect syntax!' + LineEnding +
                     'Corrent syntax: edit %s <name> [!options...];',
                     [CrudCommandString[ccVariable], CrudCommandString[ccVariable]]),
              TToken.Create(V.LineNo, V.ColNo, V.ByteNo)
      );
      Exit;
    end;

  result := TEditVariable.Create(
    DoIndexVariable(R.Tokens[0].Reduction),
    OptList
  );
end;

function TASTBuilder.DoEditValuelabel(R: TReduction; OptList: TOptionList
  ): TEditValueLabel;
begin
  {                               0                       1
    <Edit SubSystem> ==>>  ::= <Indexed Variable> <Value Label Pairs>        ! Edit value label
                     ==>>  |   <Indexed Variable>                            ! Edit dataset, variable, value label
                           |                                                    ! Edit project
  }
  Result := nil;

  if (R.TokenCount = 0) then
    begin
      DoError(Format('Edit %s: incorrect syntax!' + LineEnding +
                     'Corrent syntax: edit %s <name> (<value> "<label text>" ...) [!options ...];',
                     [CrudCommandString[ccValuelabel], CrudCommandString[ccValuelabel]]),
              FCrudErrorToken
      );
      Exit;
    end;

  if (R.TokenCount = 1) then
    begin
      result := TEditValueLabel.Create(
        nil,
        DoIndexVariable(R.Tokens[0].Reduction),
        OptList
      );
    end
  else
    result := TEditValueLabel.Create(
      DoValueLabelPairs(R.Tokens[1].Reduction, nil),
      DoIndexVariable(R.Tokens[0].Reduction),
      OptList
    );
end;

function TASTBuilder.DoEditData(R: TReduction; OptList: TOptionList): TEditData;
var
  V: TCustomVariable;
begin
  {                               0                  1
    <Edit SubSystem>       ::= <Indexed Variable> <Value Label Pairs>        ! Edit value label
                           |   <Indexed Variable>                            ! Edit dataset, variable
                     ==>>  |                                                 ! Edit project, data
  }
    result := nil;

    if (R.TokenCount > 0) then
      begin
        V := DoIndexVariable(R.Tokens[0].Reduction);
        DoError(Format('Edit %s: incorrect syntax!' + LineEnding +
                       'Corrent syntax: edit %s [!options...];',
                       [CrudCommandString[ccData], CrudCommandString[ccData]]),
                TToken.Create(V.LineNo, V.ColNo, V.ByteNo)
        );
        Exit;
      end;

    result := TEditData.Create(OptList);
end;

function TASTBuilder.DoCrudCmds(R: TReduction): TCustomStatement;
var
  FI: TCrudCommand;
  S: UTF8String;
  VL: TVariableList;
  OptList: TOptionList;
begin
{
<Crud Commands>   ::= opNew  <Indexed Variable> <New SubSystem>                                <Option List>
                  |   opEdit <Variable>         <Edit SubSystem>                               <Option List>
                  |   opDrop <Variable>         <Optional Variable List>                       <Option List>
                  |   opList <Variable>         <Optional Variable List>                       <Option List>
}
  result := nil;
  S := UTF8LowerString(DoCommandName(R));

  case S of
    'new':
      result := DoNew(R);

    'edit':
      result := DoEdit(R);

    'drop',
    'list':
      result := DoSimpleCrud(R);
  end;

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoCheckCmd(R: TReduction): TCustomStatement;
var
  V: TCustomVariable;
  SubCmd: UTF8String;
  OptList: TOptionList;
  VarList: TVariableList;
begin
{                       0         1                      2                        3
  <Check Command> ::= opCheck <Variable> <Optional Indexed Variable List> <Option List>
}
  result := nil;

  V := DoVariable(R.Tokens[1].Reduction);
  VarList := DoOptionalIndexedVariableList(R.Tokens[2].Reduction);
  OptList := DoOptionList(R.Tokens[3].Reduction, nil);

  SubCmd := UTF8LowerString(V.Ident);
  case SubCmd of
    'd',
    'data':
      result := TCheckDataCommand.Create(VarList, OptList);

    'k',
    'key':
      result := TCheckKeyCommand.Create(VarList, OptList);

    'r',
    'relate':
      result := TCheckRelateCommand.Create(VarList, OptList);

    's',
    'study':
      result := TCheckStudyCommand.Create(VarList, OptList);
  else
    DoError('Unknown check sub-command: ' + V.Ident, R.Tokens[1]);
    Exit;
  end;

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoReportCmd(R: TReduction): TCustomStatement;
var
  V: TCustomVariable;
  SubCmd: UTF8String;
  OptList: TOptionList;
  VarList: TVariableList;
begin
{                       0         1                      2                        3
  <Report Command> ::= opReport <Variable> <Optional Indexed Variable List> <Option List>
}
  result := nil;

  V := DoVariable(R.Tokens[1].Reduction);
  VarList := DoOptionalIndexedVariableList(R.Tokens[2].Reduction);
  OptList := DoOptionList(R.Tokens[3].Reduction, nil);

  SubCmd := UTF8LowerString(V.Ident);
  case SubCmd of
    'cby':
      result := TReportCountById.Create(VarList, OptList);

    'u',
    'users':
      result := TReportUsers.Create(VarList, OptList);

    'val':
      result := TReportValidateDoubleEntry.Create(VarList, OptList);
  else
    DoError(
      'Unknown report sub-command: ' + V.Ident + LineEnding +
      'Possible sub-commands are: cby, users (u), val',
      R.Tokens[1]
    );
    Exit;
  end;

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoSimpleCrud(R: TReduction): TCustomStatement;
var
  S: UTF8String;
begin
{                      0        1                    2                          3
<Crud Commands> ::=  opDrop <Variable>  <Optional Indexed Variable List>  <Option List>
                |    opList <Variable>  <Optional Indexed Variable List>  <Option List>
}

  Result := nil;
  S := DoCommandName(R);
  case UTF8LowerCase(S) of
    'drop':
      result := TDropCommand.Create(
                  DoOptionalIndexedVariableList(R.Tokens[2].Reduction),
                  DoOptionList(R.Tokens[3].Reduction, nil),
                  FamilyIdents(R.Tokens[1].Reduction, [ccData, ccGlobal, ccVariable, ccValuelabel, ccDataset])
                );

    'list':
      result := TListCommand.Create(
                  DoOptionalIndexedVariableList(R.Tokens[2].Reduction),
                  DoOptionList(R.Tokens[3].Reduction, nil),
                  FamilyIdents(R.Tokens[1].Reduction)
                );
  end;

  if FError then
    FreeAndNil(result);

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoUse(R: TReduction): TUse;
begin
//  <Statement>       ::= opUse <Indexed Variable> <Option List>
  result := nil;

  result := TUse.Create(
    DoIndexVariable(R.Tokens[1].Reduction),
    DoOptionList(R.Tokens[2].Reduction, nil)
  );

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoOptionalAssignment(R: TReduction): TExpr;
begin
{ <Optional Assignment> ::= ':=' <Expression>
                         |
}
  result := nil;

  if (R.TokenCount > 0) then
    begin
      result := DoExpression(R.Tokens[1].Reduction);

      if (Assigned(Result)) then
        Result.AssignToken(R.Tokens[0]);
    end;
end;

function TASTBuilder.DoOptionalRecodeTo(R: TReduction): TCustomVariable;
begin

end;

function TASTBuilder.DoRecodeIntervaList(R: TReduction): TValueLabelPairs;
begin

end;

function TASTBuilder.DoRecodeInterval(R: TReduction): TExpr;
begin

end;

function TASTBuilder.DoInterval(R: TReduction): TExpr;
begin

end;

function TASTBuilder.DoFunctionCall(R: TReduction): TFunctionCall;
var
  T: TToken;
begin
//  <Function Call>   ::= Identifier '(' <Parameter List> ')'
  T := R.Tokens[0];
  Result := TFunctionCall.CreateFunction(
    T.DataVar,
    DoOptionalExpressionList(R.Tokens[2].Reduction, nil),
    FExecutor
  );

  if Assigned(result) then
    Result.AssignToken(T)
  else
    DoError('Function does not exists: ' + T.DataVar, R.Tokens[0]);
end;

function TASTBuilder.DoAssignment(R: TReduction): TAssignment;
begin
//  <Statement>       ::=  <Indexed Variable> ':=' <Expression>
  result := nil;

  result := TAssignment.Create(
    DoIndexVariable(R.Tokens[0].Reduction),
    DoExpression(R.Tokens[2].Reduction)
  );

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[1]);
end;

function TASTBuilder.DoEvalExpression(R: TReduction): TCustomStatement;
begin
//  <Statement>       ::=  '?' <Expression>
  result := nil;

  result := TEvalExpression.Create(
              DoExpression(R.Tokens[1].Reduction)
            );

  if Assigned(Result) then
    result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoAssertCommand(R: TReduction): TAssertCommand;
begin
{                            0         1                      2
  <Statement>       ::=   opAssert <Expression>        <Option List>
                             0      1      2        3         4
                          opAssert '(' <Statement> ')' <Option List>

}
  result := nil;

  if R.TokenCount = 3 then
    begin
      result := TAssertCommand.Create(
        DoExpression(R.Tokens[1].Reduction),
        DoOptionList(R.Tokens[2].Reduction, nil)
      );
    end
  else
    begin
      result := TAssertCommand.Create(
        DoStatement(R.Tokens[2].Reduction),
        DoOptionList(R.Tokens[4].Reduction, nil)
      );
    end;

  if (Assigned(result)) then
    result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoSetCommand(R: TReduction): TSetCommand;
begin
  //  <Statement>       ::=  opSet <Optional Expression> <Optional Assignment>
  result := nil;

  result := TSetCommand.Create(
    DoOptionalExpression(R.Tokens[1].Reduction),
    DoOptionalAssignment(R.Tokens[2].Reduction)
  );

  if Assigned(Result) then
    result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoRecodeCommand(R: TReduction): TCustomCommand;
var
  Variable: TCustomVariable;
begin
{                     0             1                     2                       3                    4
  <Statement>  ::= opRecode <Referenced Variable> <Optional Recode To> <Recode By Or Intervals> <Option List>
}
  //Variable := DoReferencedVariable(R.Tokens[1].Reduction);
//  RecodeTo := DoOptionalRecodeTo(R.Tokens[2].Reduction);

end;

function TASTBuilder.DoIndexVariable(R: TReduction): TCustomVariable;
begin
{                              0              1          2          3
<Indexed Variable> ::= <Referenced Variable> '[' <Expression List> ']'
                   |   <Referenced Variable>
}
  Result := nil;

  if R.TokenCount = 1 then
    Result := DoReferencedVariable(R.Tokens[0].Reduction)
  else
    begin
      Result := TIndexVariable.Create(
        DoReferencedVariable(R.Tokens[0].Reduction),
        FExecutor,
        DoExpressionList(R.Tokens[2].Reduction, nil)
      );

      if Assigned(Result) then
        Result.AssignToken(R.Tokens[0]);
    end;
end;

function TASTBuilder.DoReferencedVariable(R: TReduction): TCustomVariable;
begin
{                          0   1      2         3
<Referenced Variable> ::= '@' '{' <Expression> '}'
                      |   <Variable>
}
  Result := nil;

  if R.TokenCount = 4 then
    begin
      result := TReferencedVariable.Create(
                  DoExpression(R.Tokens[2].Reduction),
                  FExecutor
                );

      if Assigned(Result) then
        Result.AssignToken(R.Tokens[0]);
    end
  else
    result := DoVariable(R.Tokens[0].Reduction);
end;

function TASTBuilder.DoVariable(R: TReduction): TCustomVariable;
begin
//  <Variable>        ::= Identifier
  Result := nil;

  result := TVariable.Create(R.Tokens[0].DataVar, FExecutor);

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

function TASTBuilder.DoVariableList(R: TReduction; OwnerList: TVariableList
  ): TVariableList;
var
  OptReduction: TReduction;
begin
{                         0       1      2                  3
  <Variable List> ::= <Variable> '-' <Variable> <Optional Variable List>
                          0                 1
                  |   <Variable> <Optional Variable List>
}
  Result := nil;

  if Assigned(OwnerList) then
    Result := OwnerList
  else
    Result := TVariableList.Create;


  // <Referenced Variable> '-' <Referenced Variable> <Referenced Variable List>
  if R.TokenCount = 4 then
    begin
      result.Add(
        TVariableRange.Create(
          DoVariable(R.Tokens[0].Reduction),
          DoVariable(R.Tokens[2].Reduction),
          Result,
          FExecutor
        )
      );

      OptReduction := R.Tokens[3].Reduction
    end
  else
    begin
      result.Add(
        DoVariable(R.Tokens[0].Reduction)
      );

      OptReduction := R.Tokens[1].Reduction;
    end;

  result := DoOptionalVariableList(OptReduction, result);
end;

function TASTBuilder.DoCommandName(R: TReduction): UTF8String;
begin
//  <Command> ::=  Identifier
  result := R.Tokens[0].DataVar;
end;

function TASTBuilder.DoOptionalReferenceOrString(R: TReduction): TExpr;
begin
{
<Optional Reference Or String>             ::= <Reference Or String>
                                           |
}
  result := nil;

  if (R.TokenCount = 1) then
    result := DoReferenceOrString(R.Tokens[0].Reduction);
end;

function TASTBuilder.DoReferenceOrString(R: TReduction): TExpr;
begin
{
<Reference Or String>                      ::= <Referenced Variable>
                                           |   String
}
  Result := nil;

  case R.Tokens[0].Name of
    'String':
      result := DoValue(R);

    'Referenced Variable':
      result := DoReferencedVariable(R.Tokens[0].Reduction);
  end;
end;

function TASTBuilder.DoOptionalString(R: TReduction): TExpr;
begin
{
<Optional String>                          ::= String
                                           |
}
  result := nil;

  if (R.TokenCount = 1) then
    result := DoValue(R);
end;


function TASTBuilder.DoOptionList(R: TReduction; OwnerList: TOptionList
  ): TOptionList;
begin
{  <Option List>     ::= <Option> <Option List>
                     |
}
  Result := nil;

  if Assigned(OwnerList) then
    result := OwnerList
  else
    result := TOptionList.Create;

  if (R.TokenCount = 0) then exit;

  result.Add(DoOption(R.Tokens[0].Reduction));

  DoOptionList(R.Tokens[1].Reduction, Result);
end;

function TASTBuilder.DoOption(R: TReduction): TOption;
var
  Expr: TExpr;
  V: TCustomVariable;
begin
{
<Option>          ::= '!' <Indexed Variable>
                   |  '!' <Variable> '=' <Expression>
}
  Result := nil;
  Expr := nil;

  if R.TokenCount = 4 then
    begin
      Expr := DoExpression(R.Tokens[3].Reduction);
      V    := DoVariable(R.Tokens[1].Reduction);
    end
  else
    V := DoIndexVariable(R.Tokens[1].Reduction);

  result := TOption.Create(
              V,
              Expr
            );
end;

function TASTBuilder.DoOptionalVariableList(R: TReduction; OwnerList: TVariableList): TVariableList;
begin
{
<Optional Variable List> ::= <Variable List>
                         |
}
  Result := nil;

  if Assigned(OwnerList) then
    Result := OwnerList
  else
    Result := TVariableList.Create;

  if R.TokenCount = 1 then
    Result := DoVariableList(R.Tokens[0].Reduction, result);
end;

function TASTBuilder.DoOptionalIndexedVariableList(R: TReduction;
  OwnerList: TVariableList): TVariableList;
begin
{
<Optional Indexed Variable List>        ::= <Indexed Variable List>
                                        |
}
  Result := nil;

  if Assigned(OwnerList) then
    Result := OwnerList
  else
    Result := TVariableList.Create;

  if R.TokenCount = 1 then
    result := DoIndexedVariableList(R.Tokens[0].Reduction, result);
end;

function TASTBuilder.DoIndexedVariableList(R: TReduction;
  OwnerList: TVariableList): TVariableList;
var
  OptReduction: TReduction;
begin
{                                    0                        1
<Indexed Variable List>  ::= <Indexed Variable> <Optional Indexed Variable List>
                                     0           1          2                        3
                         |   <Indexed Variable> '-' <Indexed Variable> <Optional Indexed Variable List>
}
  Result := nil;

  if Assigned(OwnerList) then
    Result := OwnerList
  else
    Result := TVariableList.Create;

  // <Indexed Variable> '-' <Indexed Variable> <Optional Indexed Variable List>
  if R.TokenCount = 4 then
    begin
      result.Add(
        TVariableRange.Create(
          DoIndexVariable(R.Tokens[0].Reduction),
          DoIndexVariable(R.Tokens[2].Reduction),
          Result,
          FExecutor
        )
      );

      OptReduction := R.Tokens[3].Reduction
    end
  else
    begin
      result.Add(
        DoIndexVariable(R.Tokens[0].Reduction)
      );

      OptReduction := R.Tokens[1].Reduction;
    end;

  result := DoOptionalIndexedVariableList(OptReduction, result);
end;

function TASTBuilder.DoOptionalExpressionList(R: TReduction; OwnerList: TParamList
  ): TParamList;
begin
{
  <Optional Expression List>  ::= <Expression List>
                              |
}
  result := nil;

  if R.TokenCount = 0 then exit;

  result := DoExpressionList(R.Tokens[0].Reduction, OwnerList);
end;

function TASTBuilder.DoExpressionList(R: TReduction; OwnerList: TParamList
  ): TParamList;
begin
{
<Expression List>                          ::= <Expression> ',' <Expression List>
                                           |   <Expression>
}
  Result := nil;

  if (not Assigned(OwnerList)) then
    begin
      OwnerList := TParamList.Create;
      OwnerList.AssignToken(R.Tokens[0]);
    end;

  result := OwnerList;
  result.Add(DoExpression(R.Tokens[0].Reduction));

  if R.TokenCount = 3 then
    result := DoExpressionList(R.Tokens[2].Reduction, result);
end;

function TASTBuilder.DoOptionalExpression(R: TReduction): TExpr;
begin
{
<Optional Expression>                      ::= <Expression>
                                           |
}
  result := nil;

  if (R.TokenCount = 1) then
    result := DoExpression(R.Tokens[0].Reduction);
end;

function TASTBuilder.DoVariableCommand(R: TReduction): TCustomVariableCommand;
begin
//  <Statement>       ::=  <Variable Command> <Optional Indexed Variable List> <Option List>
  Result := nil;

  result := TCustomVariableCommand.CreateCustomVariableCommand(
    DoOptionalIndexedVariableList(R.Tokens[1].Reduction),
    DoOptionList(R.Tokens[2].Reduction, nil),
    TCustomCommand.CommandNameToStatementType(DoCommandName(R.Tokens[0].Reduction))
  );

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0].Reduction.Tokens[0]);
end;

function TASTBuilder.DoStringCommand(R: TReduction): TCustomStringCommand;
var
  S: UTF8String;
begin
//  <Statement>       ::= <String Command> <Optional Expression> <Option List>
  Result := nil;

  S := UTF8LowerString(DoCommandName(R.Tokens[0].Reduction));

  case S of
    'read':
      Result := TReadCommand.Create(
        DoOptionalExpression(R.Tokens[1].Reduction),
        DoOptionList(R.Tokens[2].Reduction, nil)
      );

    'save':
      Result := TSaveCommand.Create(
        DoOptionalExpression(R.Tokens[1].Reduction),
        DoOptionList(R.Tokens[2].Reduction, nil)
      );

    'erase':
      Result := TEraseCommand.Create(
        DoOptionalExpression(R.Tokens[1].Reduction),
        DoOptionList(R.Tokens[2].Reduction, nil)
      );

  else
    result := TStringCommand.Create(
      DoOptionalExpression(R.Tokens[1].Reduction),
      DoOptionList(R.Tokens[2].Reduction, nil),
      S
    );
  end;

  if Assigned(result) then
    Result.AssignToken(R.Tokens[0].Reduction.Tokens[0]);
end;

function TASTBuilder.DoEmptyCommand(R: TReduction): TCustomEmptyCommand;
begin
//  <Statement>       ::= <Empty Command>  <Option List>
  Result := nil;

  result := TCustomEmptyCommand.CreateCustomEmptyCommand(
    DoOptionList(R.Tokens[1].Reduction, nil),
    TCustomCommand.CommandNameToStatementType(DoCommandName(R.Tokens[0].Reduction))
  );

  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0].Reduction.Tokens[0]);
end;

function TASTBuilder.DoSystemCommand(R: TReduction): TCustomStringCommand;
begin
  //  <Statement>       ::= <System Command> <Optional Expression>
  Result := nil;

  result := TStringCommand.Create(
    DoOptionalExpression(R.Tokens[1].Reduction),
    nil,
    DoCommandName(R.Tokens[0].Reduction)
  );

  if Assigned(result) then
    Result.AssignToken(R.Tokens[0].Reduction.Tokens[0]);
end;

function TASTBuilder.DoExpression(R: TReduction): TExpr;
var
  LeftExpr, RightExpr: TExpr;
begin
{
<Expression>      ::= <Expression> '>'  <Add Exp>
                   |  <Expression> '<'  <Add Exp>
                   |  <Expression> '<=' <Add Exp>
                   |  <Expression> '>=' <Add Exp>
                   |  <Expression> '='  <Add Exp>    !Equal
                   |  <Expression> '<>' <Add Exp>    !Not equal
                   |  <Add Exp>
}
  Result := nil;

  if R.TokenCount = 1 then
    begin
      Result := DoAddExpression(R.Tokens[0].Reduction);
      Exit;
    end
  else
    begin
      LeftExpr  := DoExpression(R.Tokens[0].Reduction);
      RightExpr := DoAddExpression(R.Tokens[2].Reduction);
      case R.Tokens[1].Name of
        '>':  Result := TRelationalExpr.Create(otGT,  LeftExpr, RightExpr);
        '>=': Result := TRelationalExpr.Create(otGTE, LeftExpr, RightExpr);
        '<':  Result := TRelationalExpr.Create(otLT,  LeftExpr, RightExpr);
        '<=': Result := TRelationalExpr.Create(otLTE, LeftExpr, RightExpr);
        '=':  Result := TRelationalExpr.Create(otEQ,  LeftExpr, RightExpr);
        '<>': Result := TRelationalExpr.Create(otNEQ, LeftExpr, RightExpr);
      end;

      if Assigned(Result) then
        Result.AssignToken(R.Tokens[1]);
    end;
end;

function TASTBuilder.DoAddExpression(R: TReduction): TExpr;
var
  LeftExpr, RightExpr: TExpr;
begin
{
<Add Exp>         ::= <Add Exp> '+' <Mult Exp>
                   |  <Add Exp> '-' <Mult Exp>
                   |  <Add Exp> 'or' <Mult Exp>
                   |  <Add Exp> 'xor' <Mult Exp>
                   |  <Mult Exp>
}
  Result := nil;

  if R.TokenCount = 1 then
    begin
      Result := DoMultExpression(R.Tokens[0].Reduction);
      Exit;
    end
  else
    begin
      LeftExpr  := DoAddExpression(R.Tokens[0].Reduction);
      RightExpr := DoMultExpression(R.Tokens[2].Reduction);
      case R.Tokens[1].Name of
        '+':   Result := TBinaryExpr.Create(otPlus,  LeftExpr, RightExpr);
        '-':   Result := TBinaryExpr.Create(otMinus, LeftExpr, RightExpr);
        'or':  Result := TBinaryExpr.Create(otOr,    LeftExpr, RightExpr);
        'xor': Result := TBinaryExpr.Create(otXor,   LeftExpr, RightExpr);
      end;

      if Assigned(Result) then
        Result.AssignToken(R.Tokens[1]);
    end;
end;

function TASTBuilder.DoMultExpression(R: TReduction): TExpr;
var
  LeftExpr, RightExpr: TExpr;
begin
{
<Mult Exp>        ::= <Mult Exp> '*' <Exponential Exp>
                   |  <Mult Exp> '/' <Exponential Exp>
                   |  <Mult Exp> 'div' <Exponential Exp>
                   |  <Mult Exp> 'mod' <Exponential Exp>
                   |  <Mult Exp> 'and' <Exponential Exp>
                   |  <Mult Exp> 'shl' <Exponential Exp>
                   |  <Mult Exp> 'shr' <Exponential Exp>
                   |  <Exponential Exp>
}
  Result := nil;

  if R.TokenCount = 1 then
    begin
      Result := DoExponentialExpression(R.Tokens[0].Reduction);
      Exit;
    end
  else
    begin
      LeftExpr  := DoMultExpression(R.Tokens[0].Reduction);
      RightExpr := DoExponentialExpression(R.Tokens[2].Reduction);
      case R.Tokens[1].Name of
        '*':   Result := TBinaryExpr.Create(otMult,   LeftExpr, RightExpr);
        '/':   Result := TBinaryExpr.Create(otDivide, LeftExpr, RightExpr);
        'div': Result := TBinaryExpr.Create(otDiv,    LeftExpr, RightExpr);
        'mod': Result := TBinaryExpr.Create(otMod,    LeftExpr, RightExpr);
        'and': Result := TBinaryExpr.Create(otAnd,    LeftExpr, RightExpr);
        'shl': Result := TBinaryExpr.Create(otShl,    LeftExpr, RightExpr);
        'shr': Result := TBinaryExpr.Create(otShr,    LeftExpr, RightExpr);
      end;

      if Assigned(Result) then
        Result.AssignToken(R.Tokens[1]);
    end;
end;

function TASTBuilder.DoExponentialExpression(R: TReduction): TExpr;
var
  LeftExpr, RightExpr: TExpr;
begin
{
<Exponential Exp> ::= <Exponential Exp> '^' <Negate Exp>
                   |  <Negate Exp>
}
  Result := nil;

  if R.TokenCount = 1 then
    begin
      Result := DoNegateExpression(R.Tokens[0].Reduction);
      Exit;
    end
  else
    begin
      LeftExpr  := DoExponentialExpression(R.Tokens[0].Reduction);
      RightExpr := DoNegateExpression(R.Tokens[2].Reduction);
      Result    := TBinaryExpr.Create(otExponential, LeftExpr, RightExpr);

      if Assigned(Result) then
        Result.AssignToken(R.Tokens[1]);
    end;
end;

function TASTBuilder.DoNegateExpression(R: TReduction): TExpr;
var
  LeftExpr, RightExpr: TExpr;
begin
{
<Negate Exp>      ::= '-' <Negate Exp>
                   |  'not' <Negate Exp>
                   |  <Value>
}
  Result := nil;

  if R.TokenCount = 1 then
    begin
      Result := DoValue(R.Tokens[0].Reduction);
      Exit;
    end
  else
    begin
      LeftExpr  := DoNegateExpression(R.Tokens[1].Reduction);
      RightExpr := nil;
      case R.Tokens[0].Name of
        '-':   Result := TUnaryExpr.Create(otMinus, LeftExpr, RightExpr);
        'not': Result := TUnaryExpr.Create(otNot,   LeftExpr, RightExpr);
      end;

      if Assigned(Result) then
        Result.AssignToken(R.Tokens[0]);
    end;
end;

function TASTBuilder.DoValue(R: TReduction): TExpr;
var
  S: UTF8String;
begin
{
<Value>      ::= <Referenced Variable>
             |   Integer
             |   Float
             |   String
             |   Missing
             |   Pi
             |   'true'
             |   'false'
             |   <Array>
             |   '(' <Expression> ')'
             |   <Function Call>
}
  Result := nil;

  case R.Tokens[0].Name of
    'Indexed Variable':
        result := DoIndexVariable(R.Tokens[0].Reduction);

    'Integer':
      begin
        Result := TIntegerLiteral.Create(StrToInt64(R.Tokens[0].DataVar));
        if Assigned(Result) then
          Result.AssignToken(R.Tokens[0]);
      end;

    'Float':
      begin
        Result := TFloatLiteral.Create(StrToFloat(R.Tokens[0].DataVar, ASTFormatSettings));
        if Assigned(Result) then
          Result.AssignToken(R.Tokens[0]);
      end;

    'String':
      begin
        S := R.Tokens[0].DataVar;
        S := Copy(S, 2, Length(S) - 2);

        Result := TStringLiteral.Create(S);
        if Assigned(Result) then
          Result.AssignToken(R.Tokens[0]);
      end;

    'Missing':
      begin
        result := TMissingLiteral.Create;
        if Assigned(Result) then
          Result.AssignToken(R.Tokens[0]);
      end;

    'true':
      begin
        result := TBooleanLiteral.Create(true);
        if Assigned(Result) then
          Result.AssignToken(R.Tokens[0]);
      end;

    'false':
      begin
        result := TBooleanLiteral.Create(false);
        if Assigned(Result) then
          Result.AssignToken(R.Tokens[0]);
      end;

    'Array':
      result := DoArray(R.Tokens[0].Reduction);

    'Pi':
      begin
        result := TFloatLiteral.Create(pi);
        if Assigned(Result) then
          Result.AssignToken(R.Tokens[0]);
      end;

    'RecNumber':
      begin
        result := TRecNumberLiteral.Create(FExecutor);
        if Assigned(Result) then
          Result.AssignToken(R.Tokens[0]);
      end;

    '(':
      Result := DoExpression(R.Tokens[1].Reduction);

    'Function Call':
      result := DoFunctionCall(R.Tokens[0].Reduction);
  end;
end;

function TASTBuilder.DoArray(R: TReduction): TExpr;
begin
{                0         1           2
  <Array>   ::= '[' <Expression List> ']'
}
  Result := TArray.Create(
    DoExpressionList(R.Tokens[1].Reduction, nil)
  );
  if Assigned(Result) then
    Result.AssignToken(R.Tokens[0]);
end;

constructor TASTBuilder.Create(AExecutor: IEpiScriptExecutor);
begin
  FExecutor := AExecutor;
end;

destructor TASTBuilder.Destroy;
begin
  inherited Destroy;
end;

function TASTBuilder.BuildAST(TheProgram: TReduction; out AST: TStatementList
  ): boolean;
begin
  FError := false;

//  <Program>         ::= <Statement List>
  AST := DoStatementList(TheProgram.Tokens[0].Reduction, nil);
  Result := not FError;
end;

function TASTBuilder.BuildOptionList(TheProgram: TReduction; out
  AOptionList: TOptionList): boolean;
begin
  FError := false;

  AOptionList := DoOptionList(TheProgram, nil);
  Result := not FError;
end;

end.

