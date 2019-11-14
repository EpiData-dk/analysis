unit ast;

{$codepage UTF-8}
{$mode objfpc}{$H+}
{$INTERFACES CORBA}

  interface

uses
  Classes, SysUtils, fgl, result_variables,
  ast_types, options_hashmap,
  epidatafilestypes, epiopenfile, epidocument,
  epidatafiles, contnrs,
  Token;

type
  TCustomStatement = class;
  TAbstractSyntaxTreeBase = class;
  TExpr = class;
  TArray = class;
  TCustomVariable = class;
  TParamList = class;
  TFunctionCall = class;
  TOption = class;
  TOptionList = class;
  TVariableList = class;
  TFunctionDefinition = class;
  TFunctionList = class;
  TParamDeclerationTypeListEnumerator = class;
  TReturn = class;

  ASTFloat = EpiFloat;
  ASTInteger = EpiInteger;

  TExprValue = record
    Missing: boolean;
    BoolVal: boolean;
    IntVal: ASTInteger;
    DateVal: EpiDate;
    FloatVal: ASTFloat;
    TimeVal: EpiTime;
    StringVal: EpiString;
  end;
  PExprValue = ^TExprValue;

var
  ASTCurrentExecutionObject: TAbstractSyntaxTreeBase;

type

{   IEpiTypeChecker = interface ['IEpiTypeChecker']
     procedure TypeCheckError(Const Msg: string;
       Const LineNo, ColNo, BytePos: integer
     );
   end;      }

  IVariableCheck = interface ['IVariableCheck']
    function GetVariableList: TVariableList;
    // GetAcceptedVariableCount must return an array of posible variable count for a given statement.
    // Positive values specifies an exact match - eg. result[0] := 2 , means exactly 2 variable
    // Negative values specifies "at least"     - eg. result[0] := -1, means at least 1 variable
    // If any number of variables are allowed, return a 1-length result with the value 0 ie.: result[0] := 0;
    // If NO variables are accepted, return an empty length array.
    function GetAcceptedVariableCount: TBoundArray;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec;
  end;


  { IEpiScriptExecutor }

  IEpiScriptExecutor = interface ['IEpiScriptExecutor']
    function GetDataFile: TEpiDataFile;
    function GetExecVariable(Const Ident: UTF8String): TCustomExecutorVariable;
    function GetVariableExecType(Const Ident: UTF8String): TExecutorVariableType;
    function GetVariableValues(Const Sender: TCustomVariable; Values: PExprValue): boolean;
    function GetVariableValueBool(Const Sender: TCustomVariable): Boolean;
    function GetVariableValueInt(Const Sender: TCustomVariable): ASTInteger;
    function GetVariableValueFloat(Const Sender: TCustomVariable): ASTFloat;
    function GetVariableValueDate(Const Sender: TCustomVariable): EpiDate;
    function GetVariableValueTime(Const Sender: TCustomVariable): EpiDateTime;
    function GetVariableValueString(Const Sender: TCustomVariable): EpiString;
    function GetVariableValueMissing(Const Sender: TCustomVariable): boolean;
    function GetVariableValueUserMissing(Const Sender: TCustomVariable): boolean;
    function GetVariableType(Const Ident: TCustomVariable): TEpiFieldType;
    function GetCurrentRecordNo: Integer;
    function TypeCheckVariable(Const Sender: TCustomVariable;
      TypesAndFlags: TTypesAndFlagsRec): boolean;
    function ExpandVariableList(Const Sender: TCustomVariable; VariableChecker: IVariableCheck;
      Index: Integer; out AVariableList: TVariableList): boolean;
    function CreateFunction(Const FunctionName: string; Const ParamList: TParamList): TFunctionCall;
    procedure TypeCheckError(Const Msg: string;
      Const LineNo, ColNo, BytePos: integer);
    procedure SetTypeCheckErrorOutput(Active: boolean);
    property DataFile: TEpiDataFile read GetDataFile;
  // Functions
    procedure ExecStatement(St: TCustomStatement);
  end;

  IEpiTypeChecker = IEpiScriptExecutor;

  IOptionCheck = interface ['IOptionCheck']
    // The result should return a map of <Name, Types> which represent what options
    // the command supports.
    // The types use the TASTResultTypes set, where:
    //  - rtUndefined: represents an options that does not require an expression
    //  - rtObject:    represents an options that requires a variable/identifier
    //                 (when using rtObject, also add the executor type on .insert)
    function GetAcceptedOptions: TStatementOptionsMap;
    function GetOptionList: TOptionList;
  end;

  { TAbstractSyntaxTreeBase }

  TAbstractSyntaxTreeBase = class(TObject, IFPObserver)
  private
    FLineNo: integer;
    FColNo: integer;
    FLine: string;
    FObservedList: TFpList;
    FByteNo: integer;
  protected
    procedure DoTypeCheckError(Const Msg: String; Parser: IEpiTypeChecker);
    procedure DoTypeCheckError(Const Msg: String; Const Args: Array of const; Parser: IEpiTypeChecker);

  { Observer / IFPObserver }
  protected
    procedure ObserveObject(O: TObject);
    procedure DoObservedChange(Sender: TObject); virtual; abstract;
  public
    procedure FPOObservedChanged(ASender: TObject;
       Operation: TFPObservedOperation; Data: Pointer);
  public
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; virtual;
    procedure AssignToken(T: TToken);
    property LineNo: integer read FLineNo;
    property ColNo: integer read FColNo;
    property ByteNo: integer read FByteNo;
    property Line: string read FLine;

  { Custom Data }
  // CustomData is a custom property that can be used freely to store some data
  // along with the object.
  private
    FCustomData: TFPObjectHashTable;
  public
    procedure AddCustomData(Const Key: UTF8String; Data: TObject);
    function FindCustomData(Const Key: UTF8String): TObject;
    function RemoveCustomData(Const Key: UTF8String): TObject;
  end;

  TCustomStatementExecutionResult = (
    csrSuccess,   // The statement completed successfully
    csrOptionList,// The statement completed by showing the list of options
    csrFailed,    // The statement failed
    csrCustom     // The statement returned a custom result. See actual implementation for details.
  );

  TCustomStatementExecutionFlag = (
    sefNoSelect,  // Cannot execute during an active select
    sefNoLoop,    // Cannot execure during a loop
    sefInternal   // Command called within another command, eg. adding Valuelabel set during merge
                  // hence should not be added to output/history.
  );
  TCustomStatementExecutionFlags = set of TCustomStatementExecutionFlag;

  { TCustomStatement }

  TCustomStatement = class(TAbstractSyntaxTreeBase)
  private
    FStatementType: TASTStatementType;
    FExecResult: TCustomStatementExecutionResult;
  protected
    constructor Create(StatementType: TASTStatementType); virtual;
    function DoOptionsCheck(OptionsChecker: IOptionCheck; TypeChecker: IEpiTypeChecker): Boolean; virtual;
    function DoVarialeCheck(VariableChecker: IVariableCheck; TypeChecker: IEpiTypeChecker): Boolean; virtual;
    function GetRequireOpenProject: Boolean; virtual;
    function GetExecFlags: TCustomStatementExecutionFlags; virtual;
    function GetExecFlagsErrorMsg(Flag: TCustomStatementExecutionFlag): UTF8String; virtual;
  public
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property StatementType: TASTStatementType read FStatementType;
    property RequireOpenProject: Boolean read GetRequireOpenProject;
    property ExecResult: TCustomStatementExecutionResult read FExecResult write FExecResult;
    property ExecFlags: TCustomStatementExecutionFlags read GetExecFlags;
    property ExecFlagsErrorMsg[Flag: TCustomStatementExecutionFlag]: UTF8String read GetExecFlagsErrorMsg;
  end;
  TCustomStatementClass = class of TCustomStatement;

  TStatementList = class;

  { TEmptyStatement }

  TEmptyStatement = class(TCustomStatement)
  protected
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create;
  end;

  { TAssignment }

  TAssignment = class(TCustomStatement)
  private
    FVAriable: TCustomVariable;
    FExpr: TExpr;
  protected
    procedure DoObservedChange(Sender: TObject); override;
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create(Const Variable: TCustomVariable; Const Expr: TExpr);
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Variable: TCustomVariable read FVAriable;
    property Expr: TExpr read FExpr;
  end;

  { TIfThen }

  TIfThen = class(TCustomStatement)
  private
    FExpr: TExpr;
    FThenStatement: TCustomStatement;
    FElseStatement: TCustomStatement;
  protected
    procedure DoObservedChange(Sender: TObject); override;
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create(Const Expr: TExpr; Const ThenStatement: TCustomStatement;
      Const ElseStatement: TCustomStatement);
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Expr: TExpr read FExpr;
    property ThenStatement: TCustomStatement read FThenStatement;
    property ElseStatement: TCustomStatement read FElseStatement;
  end;

  { TSelect }

  TSelect = class(TCustomStatement)
  private
    FExpr: TExpr;
    FStatement: TCustomStatement;
  protected
    procedure DoObservedChange(Sender: TObject); override;
  public
    constructor Create(Const Expr: TExpr; Const Statement: TCustomStatement);
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Expr: TExpr read FExpr;
    property Statement: TCustomStatement read FStatement;
  end;

  { TFor }

  TFor = class(TCustomStatement, IVariableCheck)
  private
    type
      TForDirection = (fdTo, fdDownTo);
      TForType      = (ftRange, ftArray);
  private
    FVariable: TCustomVariable;
    FStartExpr: TExpr;
    FDirection: TForDirection;
    FEndExpr: TExpr;
    FStatement: TCustomStatement;
    FForType: TForType;
    FArrayVal: TArray;
  protected
    function GetRequireOpenProject: Boolean; override;
    procedure DoObservedChange(Sender: TObject); override;
  public // IVariableCheck
    function GetVariableList: TVariableList;
    function GetAcceptedVariableCount: TBoundArray;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec;
  public
    constructor Create(Const Variable: TCustomVariable;
      Const StartExpr, EndExpr: TExpr;
      ADirection: UTF8String;
      Statement: TCustomStatement);
    constructor Create(Const AVariable: TCustomVariable;
      Const AArrayVal: TArray;
      AStatement: TCustomStatement);
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Variable: TCustomVariable read FVariable;
    property StartExpr: TExpr read FStartExpr;
    property EndExpr: TExpr read FEndExpr;
    property ArrayVal: TArray read FArrayVal;
    property Statement: TCustomStatement read FStatement;
    property Direction: TForDirection read FDirection;
    property ForType: TForType read FForType;
  end;

  { TParamPair }

  TParamPair = class
  private
    FIdent: UTF8String;
    FParamType: TASTResultType;
  public
    constructor Create(AIdent: UTF8String; AParamType: TASTResultType);
    property Ident: UTF8String read FIdent;
    property ParamType: TASTResultType read FParamType;
  end;

  { TParamDeclerationTypeList }

  TParamDeclerationTypeList = class
  private
    type
      TParamPairList = specialize TFPGList<TParamPair>;
  private
    FParamPairList: TParamPairList;
    function GetCount: Integer;
    function GetItem(Ident: UTF8String): TParamPair;
    function GetItems(Index: Integer): TParamPair;
  public
    constructor Create;
    procedure Add(ParamPair: TParamPair);
    function GetEnumerator: TParamDeclerationTypeListEnumerator;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TParamPair read GetItems; default;
    property Item[Ident: UTF8String]: TParamPair read GetItem;
  end;

  { TParamDeclerationTypeListEnumerator }

  TParamDeclerationTypeListEnumerator = class
  private
    FCurrentIndex: Integer;
    FParamDeclerationTypeList: TParamDeclerationTypeList;
    FListCount: Integer;
    procedure RaiseCountError;
    procedure CheckListCount;
  protected
    function GetCurrent: TParamPair;
  public
    constructor Create(ParamDeclerationTypeList: TParamDeclerationTypeList);
    function MoveNext: Boolean;
    property Current: TParamPair read GetCurrent;
  end;

  { TFunctionListEnumerator }

  TFunctionListEnumerator = class
    FCurrentIndex: Integer;
    FFunctionList: TFunctionList;
    FListCount: Integer;
    procedure RaiseCountError;
    procedure CheckListCount;
  protected
    function GetCurrent: TFunctionDefinition;
  public
    constructor Create(FunctionList: TFunctionList);
    function MoveNext: Boolean;
    property Current: TFunctionDefinition read GetCurrent;
  end;

  { TFunctionList }

  TFunctionList = class(TAbstractSyntaxTreeBase)
  private
    type
      TFunctionDefinitionList = specialize TFPGList<TFunctionDefinition>;
  private
    FFunctionList: TFunctionDefinitionList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TFunctionDefinition;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure Add(AFunction: TFunctionDefinition);
    function GetEnumerator: TFunctionListEnumerator;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFunctionDefinition read GetItems; default;
  end;

  { TFunctionDefinition }

  TReturnStatementList = specialize TFPGList<TReturn>;

  TFunctionDefinition = class(TCustomStatement)
  private
    FIdent: UTF8String;
    FParameterTypeList: TParamDeclerationTypeList;
    FReturnType: TASTResultType;
    FStatements: TStatementList;
    FReturnStatements: TReturnStatementList;
    FActualdReturnStatement: TReturn;
  protected
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create(Const Ident: UTF8String;
      ParameterTypeList: TParamDeclerationTypeList;
      ReturnType: TASTResultType;
      Statements: TStatementList;
      ReturnStatements: TReturnStatementList);
    property Ident: UTF8String read FIdent;
    property ParameterTypeList: TParamDeclerationTypeList read FParameterTypeList;
    property ReturnType: TASTResultType read FReturnType;
    property Statements: TStatementList read FStatements;
    property ReturnStatements: TReturnStatementList read FReturnStatements;
    property ActualdReturnStatement: TReturn read FActualdReturnStatement write FActualdReturnStatement;
  end;

  { TReturn }

  TReturn = class(TCustomStatement)
  private
    FParentFunction: TFunctionDefinition;
    FReturnExpression: TExpr;
  public
    constructor Create(AReturnExpression: TExpr);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property ReturnExpression: TExpr read FReturnExpression;
    property ParentFunction: TFunctionDefinition read FParentFunction;
  end;

  { TEvalExpression }

  TEvalExpression = class(TCustomStatement)
  private
    FExpr: TExpr;
  protected
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create(AExpr: TExpr);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Expr: TExpr read FExpr;
  end;

  { TExpr }

  TExpr = class(TCustomStatement)
  private
    FOp: TParserOperationType;
    FL:  TExpr;
    FR:  TExpr;
    function GetAsBoolean: Boolean;
    function GetAsDate: EpiDate;
    function GetAsFloat: ASTFloat;
    function GetAsInteger: ASTInteger;
    function GetAsString: EpiString;
    function GetAsTime: EpiDateTime;
    procedure ResetEvalValue;
  protected
    FEvalValue: TExprValue;
    function CommonType(Const A, B: TExpr): TASTResultType;
    function DoEvaluate: boolean; virtual;
    procedure DoObservedChange(Sender: TObject); override;
    procedure RuntimeError(EClass: TExceptionClass; Const Msg: string);
  public
    constructor Create(Const Op: TParserOperationType; Const L, R: TExpr); virtual;
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override; overload;
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; virtual; overload;
    function ResultType: TASTResultType; virtual;
    property Operation: TParserOperationType read FOp;
    property Left: TExpr read FL;
    property Right: TExpr read FR;
  private
    FEvaluated: boolean;
  public
    function Evaluate:  TExpr;
    procedure ResetEvaluation;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsInteger: ASTInteger read GetAsInteger;
    property AsFloat:   ASTFloat read GetAsFloat;
    property AsDate:    EpiDate read GetAsDate;
    property AsTime:    EpiDateTime read GetAsTime;
    property AsString:  EpiString read GetAsString;
    function AsIdent:   UTF8String; virtual;
    function IsMissing: Boolean; virtual;
    function IsUserMissing: Boolean; virtual;
  end;

  { TLiteral }

  TLiteral = class(TExpr)
  protected
    function DoEvaluate: boolean; override;
  end;

  { TBooleanLiteral }

  TBooleanLiteral = class(TLiteral)
  private
    FOriginalVal: Boolean;
  protected
    function DoEvaluate: boolean; override;
  public
    constructor Create(Const Value: Boolean); overload;
    function ResultType: TASTResultType; override;
  end;

  { TIntegerLiteral }

  TIntegerLiteral = class(TLiteral)
  private
    FOriginalVal: ASTInteger;
  protected
    function DoEvaluate: boolean; override;
  public
    constructor Create(const Value: ASTInteger);
    function ResultType: TASTResultType; override;
  end;

  { TFloatLiteral }

  TFloatLiteral = class(TLiteral)
  private
    FOriginalVal: ASTFloat;
  protected
    function DoEvaluate: boolean; override;
  public
    constructor Create(const Value: ASTFloat);
    function ResultType: TASTResultType; override;
  end;

  { TStringLiteral }

  TStringLiteral = class(TLiteral)
  private
    FOriginalVal: EpiString;
  protected
    function DoEvaluate: boolean; override;
  public
    constructor Create(const Value: EpiString);
    function ResultType: TASTResultType; override;
  end;

  { TMissingLiteral }

  TMissingLiteral = class(TLiteral)
  protected
    function DoEvaluate: boolean; override;
  public
    constructor Create;
    function ResultType: TASTResultType; override;
  end;

  { TRecNumberLiteral }

  TRecNumberLiteral = class(TLiteral)
  private
    FExecutor: IEpiScriptExecutor;
  protected
    function DoEvaluate: boolean; override;
  public
    constructor Create(Executor: IEpiScriptExecutor);
    function ResultType: TASTResultType; override;
  end;

  { TUnaryExpr }

  TUnaryExpr = class(TExpr)
  protected
    function DoEvaluate: boolean; override;
  public
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    function ResultType: TASTResultType; override;
  end;

  { TBinaryExpr }

  TBinaryExpr = class(TExpr)
  protected
    function DoEvaluate: boolean; override;
  public
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    function ResultType: TASTResultType; override;
  end;

  { TRelationalExpr }

  TRelationalExpr = class(TExpr)
  protected
    function DoEvaluate: boolean; override;
  public
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    function ResultType: TASTResultType; override;
  end;

  { TParamList }

  TParamList = class(TAbstractSyntaxTreeBase)
  private
    type
      TExprList = specialize TFPGList<TExpr>;
  private
    FExprList: TExprList;
    function GetCount: Integer;
    function GetParam(const Index: Integer): TExpr;
  protected
    procedure DoObservedChange(Sender: TObject); override;
  public
    constructor Create;
    destructor  Destroy; override;
    function    TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    procedure   Add(Param: TExpr);
    property    Param[Const Index: Integer]: TExpr read GetParam; default;
    property    Count: Integer read GetCount;
  end;

  { TFunctionCall }

  TFunctionCall = class(TExpr)
  private
    function GetParam(const Index: integer): TExpr;
  protected
    FExecutor: IEpiScriptExecutor;
    FParamList: TParamList;
    constructor Create(Const ParamList: TParamList); virtual;
    function ParamCounts: TBoundArray; virtual;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; virtual;
    function DoEvaluate: boolean; override;
  public
    class function CreateFunction(Const FunctionName: string;
      Const ParamList: TParamList;
      Executor: IEpiScriptExecutor): TFunctionCall;
    destructor Destroy; override;
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    property Param[Const Index: integer]: TExpr read GetParam;
  end;

  { TTypeCast }

  TTypeCast = class(TFunctionCall)
  private
    procedure DoTypeCastError(Const Msg: UTF8String);
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; override;
    function DoEvaluate: Boolean; override;
  public
    function ResultType: TASTResultType; override;
  public
    constructor Create(Const AOperation: TParserOperationType; Const ParamList: TParamList);
    function IsMissing: Boolean; override;
  end;

  { TUserFunction }

  TUserFunction = class(TFunctionCall)
  private
    FFunctionDefinition: TFunctionDefinition;
    procedure ExecuteStatements;
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec; override;
    function DoEvaluate: boolean; override;
  public
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    function ResultType: TASTResultType; override;
  public
    constructor Create(FunctionDefinition: TFunctionDefinition; const ParamList: TParamList);
    function IsMissing: Boolean; override;
  end;

  { TArray }

  TArray = class(TExpr)
  private
    FExprList: TParamList;
    FResultSubType: TASTResultType;
    function GetCount: Integer;
  protected
    function DoEvaluate: boolean; override;
  public
    constructor Create(Const AExprList: TParamList);
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    function ResultType: TASTResultType; override;
    property ResultSubType: TASTResultType read FResultSubType;
    property ExprList: TParamList read FExprList;
    property Count: Integer read GetCount;
  end;

  { TCustomVariable }

  TVariableType = (vtVariable, vtIndexed, vtReferenced);

  TCustomVariable = class(TExpr, IFPObserved)
  private
    FExecutor: IEpiScriptExecutor;
  protected
    FIdent: UTF8String;
    FVarType: TVariableType;
    class function FieldTypeToParserType(FieldType: TEpiFieldType): TASTResultType;
    function GetIdent: UTF8String; virtual;
    function DoEvaluate: boolean; override;
    property Executor: IEpiScriptExecutor read FExecutor;
  public
    constructor Create(Const AIdent: UTF8String; AExecutor: IEpiScriptExecutor);
    destructor Destroy; override;
    function  ResultType: TASTResultType; override;
    function  TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    function  AsIdent: UTF8String; override;
    property  Ident: UTF8String read GetIdent;
    property  VarType: TVariableType read FVarType;

  public
    function  IsMissing: Boolean; override;
    function  IsUserMissing: Boolean; override;

  { IFPObserved }
  private
    FObservers: TFPList;
  public
    procedure FPOAttachObserver(AObserver: TObject);
    procedure FPODetachObserver(AObserver: TObject);
    procedure FPONotifyObservers(ASender: TObject;
      AOperation: TFPObservedOperation; Data: Pointer);
  end;

  { TVariable }

  TVariable = class(TCustomVariable)
  public
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
  end;

  { TIndexVariable }

  TIndexVariable = class(TCustomVariable)
  private
    FVariable: TCustomVariable;
    FExprList: TParamList;
  protected
    function GetParamCount: Integer; virtual;
    function GetExpr(const Index: Integer): TExpr; virtual;
    function DoEvaluate: boolean; override;
  public
    constructor Create(AVariable: TCustomVariable; AExecutor: IEpiScriptExecutor; AExprList: TParamList);
    destructor Destroy; override;
    function  TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    property  Expr[const Index: Integer]: TExpr read GetExpr;
    property  ParamCount: Integer read GetParamCount;
  end;

  { TReferencedVariable }

  TReferencedVariable = class(TCustomVariable)
  private
    FExpr: TExpr;
    FReferenceVariable: TCustomVariable;
    function GetReferenceVariable: TCustomVariable;
  protected
    property ReferenceVariable: TCustomVariable read GetReferenceVariable;
    function GetIdent: UTF8String; override;
    function DoEvaluate: boolean; override;
  public
    constructor Create(AExpr: TExpr; AExecutor: IEpiScriptExecutor);
    destructor Destroy; override;
    function ResultType: TASTResultType; override;
    function TypeCheck(TypeChecker: IEpiTypeChecker; ATypesAndFlags: TTypesAndFlagsRec): boolean; override;
    function AsIdent: UTF8String; override;
    function IsMissing: Boolean; override;
  end;

  { TVariableRange }

  TVariableRange = class(TCustomVariable)
  private
    FStartVariable: TCustomVariable;
    FEndVariable: TCustomVariable;
  public
    constructor Create(AStartVariable, AEndVariable: TCustomVariable; AOwnerList: TVariableList; AExecutor: IEpiScriptExecutor);
    function TypeCheck(TypeChecker: IEpiTypeChecker; TypesAndFlags: TTypesAndFlagsRec): boolean; override;
    property StartVariable: TCustomVariable read FStartVariable;
    property EndVariable: TCustomVariable read FEndVariable;
  end;

  { TVariableListEnumerator }

  TVariableListEnumerator = class
  private
    FList: TVariableList;
    FIdx: Integer;
    function GetCurrent: TCustomVariable;
  public
    constructor Create(List: TVariableList);
    function MoveNext: boolean;
    property Current: TCustomVariable read GetCurrent;
  end;

  { TVariableList }

  TVariableList = class(TAbstractSyntaxTreeBase)
  private
    type
      TVarList = specialize TFPGList<TCustomVariable>;
  private
    FExpanded: boolean;
    FVarList: TVarList;
    function GetCount: Integer;
    function GetVariable(const Index: Integer): TCustomVariable;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Add(Variable: TCustomVariable);
    procedure   Delete(Index: Integer);
    function    TypeCheck(VariableChecker: IVariableCheck; TypeChecker: IEpiTypeChecker): boolean; reintroduce;
    function    GetIdentsAsList: TStrings;
    function    GetEnumerator: TVariableListEnumerator;
    // Expands any Range variables to the complete list, must be done before Count and TypeChecking.
    function    ExpandVariableList(VariableChecker: IVariableCheck): boolean;
    property    Variable[Const Index: Integer]: TCustomVariable read GetVariable; default;
    property    Count: Integer read GetCount;
  end;

  TCrudCommand =
    (ccData, ccVariable, ccDataset, ccProject, ccValuelabel, ccGlobal, ccResult);

  TCrudCommands = set of TCrudCommand;

const
  CrudCommandString: array[TCrudCommand] of UTF8String =
    ('data', 'variable', 'dataset', 'project', 'valuelabel', 'global', 'result');

  AllCrudCommands = [ccData..ccResult];

type

  { TCustomCrudCommand }

  TCustomCrudCommand = class(TCustomStatement, IOptionCheck)
  private
    FOptionList: TOptionList;
    FSubCommand: TCrudCommand;
  protected
    // IOptionCheck
    function GetAcceptedOptions: TStatementOptionsMap; virtual;
    function GetOptionList: TOptionList;
  public
    constructor Create(AOptionList: TOptionList; AStatementType: TASTStatementType;
      ASubCommand: TCrudCommand); virtual;
    property OptionList: TOptionList read GetOptionList;
    property SubCommand: TCrudCommand read FSubCommand;
    function HasOption(Const Ident: UTF8String; out AOption: TOption): boolean; overload;
    function HasOption(Const Ident: UTF8String): boolean; overload;
  end;

  { TCustomNew }

  TCustomNew = class(TCustomCrudCommand)
  protected
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create(AOptionList: TOptionList; ASubCommand: TCrudCommand); virtual;
  end;

  { TNewProject }

  TNewProject = class(TCustomNew)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AOptionList: TOptionList); virtual;
  end;

  { TCustomNewNamed }

  TCustomNewNamed = class(TCustomNew)
  private
    FVariable: TCustomVariable;
  public
    constructor Create(Const AVariable: TCustomVariable; AOptionList: TOptionList; ASubCommand: TCrudCommand); virtual;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Variable: TCustomVariable read FVariable;
  end;

  { TNewDataset }

  TNewDataset = class(TCustomNewNamed)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(Const AVariable: TCustomVariable; AOptionList: TOptionList); virtual;
  end;

  { TCustomNewTyped }

  TCustomNewTyped = class(TCustomNewNamed)
  private
    FNewType: TEpiFieldType;
  public
    constructor Create(ANewType: TEpiFieldType; const AVariable: TCustomVariable; AOptionList: TOptionList; ASubCommand: TCrudCommand); virtual;
    property NewType: TEpiFieldType read FNewType;
  end;

  { TValueLabelPairs }

  TValueLabelPairs = class(TAbstractSyntaxTreeBase)
  private
    FValues: TObjectList;
    FTexts: TObjectList;
    FValueType: TASTResultType;
    function GetCount: Integer;
    function GetText(const Index: Integer): TExpr;
    function GetValues(const Index: Integer): TExpr;
  public
    constructor Create(AValueType: TASTResultType); virtual;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    function Find(Const S: UTF8String; out Index: Integer): boolean;
    procedure AddPair(Value, Text: TExpr);
    property Count: Integer read GetCount;
    property ValueType: TASTResultType read FValueType;
    property Values[Const Index: Integer]: TExpr read GetValues;
    property Text[Const Index: Integer]: TExpr read GetText;
  end;


  { TNewValuelabel }

  TNewValuelabel = class(TCustomNewTyped)
  private
    FValueLabelPairs: TValueLabelPairs;
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AValueLabelPairs: TValueLabelPairs; ANewType: TEpiFieldType;
      const AVariable: TCustomVariable; AOptionList: TOptionList); virtual;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
  published
    property ValueLabelPairs: TValueLabelPairs read FValueLabelPairs;
  end;

  { TCustomNewValued }

  TCustomNewValued = class(TCustomNewTyped)
  private
    FValueExpr: TExpr;
  public
    constructor Create(AValueExpr: TExpr;
      ANewType: TEpiFieldType; const AVariable: TCustomVariable;
      AOptionList: TOptionList; ASubCommand: TCrudCommand); virtual;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property ValueExpr: TExpr read FValueExpr;
  end;

  { TNewVariable }

  TNewVariable = class(TCustomNewValued)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AValueExpr: TExpr;
      ANewType: TEpiFieldType; const AVariable: TCustomVariable; AOptionList: TOptionList); virtual;
  end;

  { TCustomNewGlobal }

  TCustomNewGlobal = class(TCustomNewValued)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AValueExpr: TExpr; ANewType: TEpiFieldType;
      const AVariable: TCustomVariable; AOptionList: TOptionList);
  end;

  { TNewGlobal }

  TNewGlobal = class(TCustomNewGlobal)
  public
    constructor Create(AValueExpr: TExpr; ANewType: TEpiFieldType;
      const AVariable: TCustomVariable; AOptionList: TOptionList); virtual;
  end;

  { TNewGlobalVector }

  TNewGlobalVector = class(TCustomNewGlobal)
  private
    FVectorExpr: TExpr;
  public
    constructor Create(AVectorExpr, AValueExpr: TExpr; ANewType: TEpiFieldType;
      const AVariable: TCustomVariable; AOptionList: TOptionList); virtual;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property VectorExpr: TExpr read FVectorExpr;
  end;

  { TEditProject }

  TEditProject = class(TCustomCrudCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AOptionList: TOptionList);
  end;

  { TEditData }

  TEditData = class(TCustomCrudCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AOptionList: TOptionList);
  end;

  { TCustomVariablesCrudCommand }

  TCustomVariablesCrudCommand = class(TCustomCrudCommand, IVariableCheck)
  private
    FVariableList: TVariableList;
  protected
    // IVariableCheck
    function GetVariableList: TVariableList;
    function GetAcceptedVariableCount: TBoundArray; virtual;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; virtual;
  public
    constructor Create(AVariables: TVariableList; AOptionList: TOptionList;
      AStatementType: TASTStatementType; ASubCommand: TCrudCommand); virtual;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Variables: TVariableList read GetVariableList;
  end;

  { TCustomEditCommand }

  TCustomEditCommand = class(TCustomVariablesCrudCommand)
  private
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetVariable: TCustomVariable;
  public
    constructor Create(AVariable: TCustomVariable; AOptionList: TOptionList;
      ASubCommand: TCrudCommand);
    property Variable: TCustomVariable read GetVariable;
  end;

  { TEditDataset }

  TEditDataset = class(TCustomEditCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AVariable: TCustomVariable; AOptionList: TOptionList);
  end;

  { TEditVariable }

  TEditVariable = class(TCustomEditCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AVariable: TCustomVariable; AOptionList: TOptionList);
  end;

  { TEditValueLabel }

  TEditValueLabel = class(TCustomEditCommand)
  private
    FValueLabelPairs: TValueLabelPairs;
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AValueLabelPairs: TValueLabelPairs; AVariable: TCustomVariable; AOptionList: TOptionList); virtual;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
  published
    property ValueLabelPairs: TValueLabelPairs read FValueLabelPairs;
  end;

  { TListCommand }

  TListCommand = class(TCustomVariablesCrudCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetRequireOpenProject: Boolean; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AVariables: TVariableList; AOptionList: TOptionList;
      ASubCommand: TCrudCommand); virtual;
  end;

  { TEditCommand }

  TEditCommand = class(TCustomVariablesCrudCommand)
  private
    function GetVariable: TCustomVariable;
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AVariable: TCustomVariable; AOptionList: TOptionList; ASubCommand: TCrudCommand);
    property Variable: TCustomVariable read GetVariable;
  end;

  { TDropCommand }

  TDropCommand = class(TCustomVariablesCrudCommand)
  protected
    function GetRequireOpenProject: Boolean; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AVariables: TVariableList; AOptionList: TOptionList; ASubCommand: TCrudCommand);
    property Variables: TVariableList read FVariableList;
  end;

  { TUse }

  TUse = class(TCustomStatement, IOptionCheck, IVariableCheck)
  private
    FOptionList: TOptionList;
    FTmpVariableList: TVariableList;
    FVariable: TCustomVariable;
//    FExpr: TExpr;
  protected
    // IOptionCheck
    function GetAcceptedOptions: TStatementOptionsMap;
    function GetOptionList: TOptionList;
    // IVariableCheck
    function GetVariableList: TVariableList;
    function GetAcceptedVariableCount: TBoundArray;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec;
    function DoVarialeCheck(VariableChecker: IVariableCheck;
      TypeChecker: IEpiTypeChecker): Boolean; override;
  public
    constructor Create(AVariable: TCustomVariable; AOptions: TOptionList);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Options: TOptionList read GetOptionList;
    property Variable: TCustomVariable read FVariable;
//    property Expr: TExpr read FExpr;
  end;

  { TOption }

  TOption = class(TAbstractSyntaxTreeBase)
  private
    FExpr: TExpr;
    FVariable: TCustomVariable;
    function GetIdent: UTF8String;
  public
    constructor Create(AVariable: TCustomVariable; AExpr: TExpr);
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Ident: UTF8String read GetIdent;
    property Variable: TCustomVariable read FVariable;
    property Expr: TExpr read FExpr;
  end;


  TOptionListEnumerator = class;

  { TOptionList }

  TOptionList = class(TAbstractSyntaxTreeBase)
  private
    type
      TOptList = specialize TFPGList<TOption>;
  private
    FOptList: TOptList;
    function GetCount: Integer;
    function GetOption(const Ident: UTF8String): TOption;
    function GetOptions(const Index: Integer): TOption;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Add(Option: TOption);
    procedure   Remove(Option: TOption);
    procedure   EvaluateOptions;
    function    HasOption(Const Ident: UTF8String; out AOption: TOption): boolean; overload;
    function    HasOption(Const Ident: UTF8String): boolean; overload;
    function    GetEnumerator: TOptionListEnumerator;
    property    Options[Const Index: Integer]: TOption read GetOptions; default;
    property    Option[Const Ident: UTF8String]: TOption read GetOption;
    property    Count: Integer read GetCount;
  end;

  { TOptionListEnumerator }

  TOptionListEnumerator = class
  private
    FCurrentIndex: Integer;
    FOptionList: TOptionList;
    FListCount: Integer;
    procedure RaiseCountError;
    procedure CheckListCount;
  protected
    function GetCurrent: TOption;
  public
    constructor Create(OptionList: TOptionList);
    function MoveNext: Boolean;
    property Current: TOption read GetCurrent;
  end;

  { TCustomCommand }

  TCustomCommand = class(TCustomStatement)
  private
    FCommand: UTF8String;
  public
    constructor Create(Const ACommand: UTF8String);
    constructor Create(ST: TASTStatementType); override;
    class function CommandNameToStatementType(const ACommand: UTF8String): TASTStatementType;
    property Command: UTF8String read FCommand;
  end;

  { TCustomOptionsCommand }

  TCustomOptionsCommand = class(TCustomCommand, IOptionCheck)
  private
    FOptions: TOptionList;
  protected
    // IOptionCheck
    function GetAcceptedOptions: TStatementOptionsMap; virtual;
    function GetOptionList: TOptionList;
  public
    constructor Create(AOptionList: TOptionList; Const ACommand: UTF8String);
    constructor Create(AOptionList: TOptionList; ST: TASTStatementType);
    function HasOption(Const Ident: UTF8String; out AOption: TOption): boolean; overload;
    function HasOption(Const Ident: UTF8String): boolean; overload;
    property Options: TOptionList read GetOptionList;
  end;

  { TAssertCommand }

  TAssertCommand = class(TCustomOptionsCommand)
  private
    FStatement: TCustomStatement;
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create(AStatement: TCustomStatement; AOptionList: TOptionList);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Statement: TCustomStatement read FStatement;
  end;

  { TSetCommand }

  TSetCommand = class(TCustomCommand)
  private
    FOptionExpr: TExpr;
    FAssignmentExpr: TExpr;
  protected
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create(AOptionExpr, AAssignmentExpr: TExpr);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property OptionExpr: TExpr read FOptionExpr;
    property AssignmentExpr: TExpr read FAssignmentExpr;
  end;

  { TCustomVariableCommand }

  TCustomVariableCommand = class(TCustomOptionsCommand, IVariableCheck)
  private
    FVariableList: TVariableList;
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  protected
    // IVariableCheck
    function GetVariableList: TVariableList; virtual;
    function GetAcceptedVariableCount: TBoundArray; virtual;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; virtual;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList; ST: TASTStatementType); virtual;
    class function CreateCustomVariableCommand(AVariableList: TVariableList;
      AOptionList: TOptionList; ST: TASTStatementType): TCustomVariableCommand;
    property VariableList: TVariableList read GetVariableList;
  end;

  { TTablesCommand }

  TTablesCommand = class(TCustomVariableCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TCTableCommand }

  TCTableCommand = class(TCustomVariableCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

 { TMeansCommand }

  TMeansCommand = class(TCustomVariableCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TAggregateCommand }

  TAggregateCommand = class(TCustomVariableCommand)
  protected
    function GetExecFlags: TCustomStatementExecutionFlags; override;
    function GetExecFlagsErrorMsg(Flag: TCustomStatementExecutionFlag): UTF8String; override;
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TBrowseCommand }

  TBrowseCommand = class(TCustomVariableCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TFreqCommand }

  TFreqCommand = class(TCustomVariableCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TDescribeCommand }

  TDescribeCommand = class(TCustomVariableCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TRecodeInterval }

  TRecodeInterval = class
  private
    FFromValue: TExpr;
    FLabelValue: TExpr;
    FToValue: TExpr;
    FValueLabel: TExpr;
  public
    constructor Create(AFromValue, AToValue, ALabelValue, AValueLabel: TExpr);
    property FromValue:  TExpr read FFromValue;
    property ToValue:    TExpr read FToValue;
    property LabelValue: TExpr read FLabelValue;
    property ValueLabel: TExpr read FValueLabel;
  end;

  TRecodeIntervalList = specialize TFPGList<TRecodeInterval>;

  { TRecodeCommand }

  TRecodeCommand = class(TCustomVariableCommand)
  private
    FFromVariable: TCustomVariable;
    FRecodeIntervalList: TRecodeIntervalList;
    FToVariable: TCustomVariable;
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec; override;
  public
    constructor Create(AFromVariable, AToVariable: TCustomVariable;
      ARecodeIntervalList: TRecodeIntervalList; AOptionList: TOptionList);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property FromVariable: TCustomVariable read FFromVariable;
    property ToVariable: TCustomVariable read FToVariable;
    property RecodeIntervalList: TRecodeIntervalList read FRecodeIntervalList;
  end;

  { TSortCommand }

  TSortCommand = class(TCustomVariableCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TCustomMergeCommand }

  TCustomMergeCommand = class(TCustomVariableCommand)
  private
    FFilename: UTF8String;
  protected
    function GetExecFlags: TCustomStatementExecutionFlags; override;
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    property Filename: UTF8String read FFilename write FFilename;
  end;

  { TAppendCommand }

  // append [<append vars>...] [!ds := <id>]
  TAppendCommand = class(TCustomMergeCommand)
  protected
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetExecFlagsErrorMsg(Flag: TCustomStatementExecutionFlag): UTF8String; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TMergeCommand }

  // merge [<key vars>...] [!filename] [!combine] [!update] [!replace] [!ds := <id>]
  TMergeCommand = class(TCustomMergeCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetExecFlagsErrorMsg(Flag: TCustomStatementExecutionFlag): UTF8String; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  TCheckSubCommand = (cscRelate, cscKey, cscStudy, cscData);

  { TCustomCheckCommand }

  TCustomCheckCommand = class(TCustomVariableCommand)
  private
    FSubCmd: TCheckSubCommand;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList; ASubCmd: TCheckSubCommand); virtual;
    property SubCmd: TCheckSubCommand read FSubCmd;
  end;

  { TCustomCheckCommandNoVariables }

  TCustomCheckCommandNoVariables = class(TCustomCheckCommand)
  protected
    function GetAcceptedVariableCount: TBoundArray; override;
  end;

  { TCheckRelateCommand }

  TCheckRelateCommand = class(TCustomCheckCommandNoVariables)
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TCheckStudyCommand }

  TCheckStudyCommand = class(TCustomCheckCommandNoVariables)
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TCheckKeyCommand }

  TCheckKeyCommand = class(TCustomCheckCommand)
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TCheckDataCommand }

  TCheckDataCommand = class(TCustomCheckCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  TReportSubCommand = (
    rscCountById,                     // Count Keys across datasets
    rscValidateDoubleEntry,           // Validates 2 datasets/projects to each other
    rscUsers                          // Created a report on login data
  );

  { TCustomReportCommand }

  TCustomReportCommand = class(TCustomVariableCommand)
  private
    FSubCmd: TReportSubCommand;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList; ASubCmd: TReportSubCommand); virtual;
    property SubCmd: TReportSubCommand read FSubCmd;
  end;

  { TReportCountById }

  TReportCountById = class(TCustomReportCommand)
  protected
    function GetRequireOpenProject: Boolean; override;
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec;
      override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TReportValidateDoubleEntry }

  TReportValidateDoubleEntry = class(TCustomReportCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
    function GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec;
      override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TReportUsers }

  TReportUsers = class(TCustomReportCommand)
  protected
    function GetAcceptedVariableCount: TBoundArray; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TReorderCommand }

  TReorderCommand = class(TCustomVariableCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetAcceptedVariableCount: TBoundArray; override;
  public
    constructor Create(AVariableList: TVariableList; AOptionList: TOptionList);
  end;

  { TCustomStringCommand }

  TCustomStringCommand = class(TCustomOptionsCommand)
  private
    FStringExpr: TExpr;
  protected
    function GetRequireOpenProject: Boolean; override;
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(Const AStringExpr: TExpr; AOptions: TOptionList;
      ACommand: UTF8String);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    destructor Destroy; override;
    property StringExpr: TExpr read FStringExpr;
  end;

  { TStringCommand }

  TStringCommand = class(TCustomStringCommand)
  public
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
  end;

  { TCustomFilecommand }

  TCustomFilecommand = class(TCustomStringCommand)
  public
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
  end;

  { TReadCommand }

  TReadCommand = class(TCustomFilecommand)
  protected
    function GetRequireOpenProject: Boolean; override;
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(Const AStringExpr: TExpr; AOptionList: TOptionList);
  end;

  { TSaveCommand }

  TSaveCommand = class(TCustomFilecommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create(Const AStringExpr: TExpr; AOptionList: TOptionList);
  end;

  { TEraseCommand }

  TEraseCommand = class(TCustomFilecommand)
  protected
    function GetRequireOpenProject: Boolean; override;
    function GetAcceptedOptions: TStatementOptionsMap; override;
  public
    constructor Create(const AStringExpr: TExpr; AOptions: TOptionList);
  end;

  { TCustomEmptyCommand }

  TCustomEmptyCommand = class(TCustomOptionsCommand)
  protected
    function GetAcceptedOptions: TStatementOptionsMap; override;
    function GetRequireOpenProject: Boolean; override;
  public
    class function CreateCustomEmptyCommand(AOptions: TOptionList; ST: TASTStatementType): TCustomEmptyCommand;
    destructor Destroy; override;
  end;
  TCustomEmptyCommandClass = class of TCustomEmptyCommand;

  { TStatementList }

  TStatementList = class(TCustomStatement)
  private
    type
      TStatements = specialize TFPGList<TCustomStatement>;
  private
    FStatements: TStatements;
    function GetCount: Integer;
    function GetStatements(Index: Integer): TCustomStatement;
    function GetRequireOpenProject: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddStatement(AStatement: TCustomStatement);
    function TypeCheck(Parser: IEpiTypeChecker): boolean; override;
    property Statements[Index: Integer]: TCustomStatement read GetStatements;
    property Count: Integer read GetCount;
  end;

  { TProgram }

  TProgram = class
  private
    FFunctions: TFunctionList;
    FStatements: TStatementList;
  public
    constructor Create(AStatements: TStatementList; AFunctions: TFunctionList);
    property Functions: TFunctionList read FFunctions;
    property Statements: TStatementList read FStatements;
  end;

implementation

uses
  LazUTF8Classes, epiconvertutils, typinfo, datamodule,
  options_utils, parser,

  // SCRIPT FUNCTIONS (placed ind ./functions/epi_script_function_<name>.pas
  epi_script_function_mathfunctions,
  epi_script_function_createdate,
  epi_script_function_createtime,
  epi_script_function_datefunctions,
  epi_script_function_timefunctions,
  epi_script_function_stringfunctions,
  epi_script_function_systemfunctions,
  epi_script_function_observations,
  math, variants, LazUTF8, LazFileUtils;

{ TProgram }

constructor TProgram.Create(AStatements: TStatementList;
  AFunctions: TFunctionList);
begin
  FStatements := AStatements;
  FFunctions  := AFunctions;
end;

{ TParamPair }

{ TRecodeInterval }

constructor TRecodeInterval.Create(AFromValue, AToValue, ALabelValue,
  AValueLabel: TExpr);
begin
  FFromValue  := AFromValue;
  FToValue    := AToValue;
  FLabelValue := ALabelValue;
  FValueLabel := AValueLabel;
end;

{ TOptionListEnumerator }

procedure TOptionListEnumerator.RaiseCountError;
begin
  raise Exception.CreateFmt(
      'Enumeration error!' + LineEnding +
      'Expected %d items in list, but MoveNext found %d!' + LineEnding +
      'Deleting/Inserting items during iteration is NOT supported!',
      [FListCount, FOptionList.Count]
    );
end;

procedure TOptionListEnumerator.CheckListCount;
begin
  if FOptionList.Count <> FListCount then
    RaiseCountError;
end;

function TOptionListEnumerator.GetCurrent: TOption;
begin
  result := FOptionList[FCurrentIndex];
end;

constructor TOptionListEnumerator.Create(OptionList: TOptionList);
begin
  FOptionList := OptionList;
  FCurrentIndex := -1;
  FListCount := OptionList.Count;
end;

function TOptionListEnumerator.MoveNext: Boolean;
begin
  CheckListCount;
  Inc(FCurrentIndex);
  Result := (FCurrentIndex < FOptionList.Count);
end;

{ TTablesCommand }

function TTablesCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  AddDecimalOptions(Result);
  AddVariableLabelOptions(Result);
  AddValueLabelOptions(Result);
  AddSortingOptions(Result);

  Result.Insert('by', AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('w',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('m', [rtUndefined]);
  Result.Insert('cs', AllResultDataTypes);

  // Output silencing options
  Result.Insert('q',  [rtUndefined]);
  Result.Insert('nc', [rtUndefined]);
  Result.Insert('nb', [rtUndefined]);
  Result.Insert('ns', [rtUndefined]);

  // Percents options
  Result.Insert('pr', [rtUndefined]);
  Result.Insert('pc', [rtUndefined]);
  Result.Insert('pt', [rtUndefined]);

  // Statistics
  Result.Insert('debug', [rtUndefined]);  // Special debug option for statistics
  Result.Insert('t',     [rtUndefined]);  // Chi2
  Result.Insert('ex',    [rtUndefined]);  // Exact tests for 2x2 tables
  Result.Insert('odds',  [rtUndefined]);  // Odds ratio, including M-H adjusted
  Result.Insert('rr',    [rtUndefined]);  // Risk ratio, including M-H adjusted
  Result.Insert('ci90',  [rtUndefined]);  // for OR and RR confidence intervals
  Result.Insert('ci95',  [rtUndefined]);
  Result.Insert('ci99',  [rtUndefined]);
end;

function TTablesCommand.GetAcceptedVariableCount: TBoundArray;
begin
  result := inherited GetAcceptedVariableCount;
  result[0] := 2;
end;

function TTablesCommand.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);
//  result.ResultTypes := [rtFloat, rtInteger];
end;

constructor TTablesCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stTables);
end;

{TCTableCommand}

function TCTableCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  AddDecimalOptions(Result);
  AddVariableLabelOptions(Result);
  AddValueLabelOptions(Result);
  AddSortingOptions(Result);

  Result.Insert('by',   AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('w',    AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('m',    [rtUndefined]);
  Result.Insert('cs',   AllResultDataTypes);

  // Output silencing options
  Result.Insert('q',    [rtUndefined]);

  // Sort table by variable name or label
  Result.Insert('sn',   [rtUndefined]);  // sort summary table by variable name
  Result.Insert('sl',   [rtUndefined]);  // sort summary table by variable label
  Result.Insert('ss',   [rtUndefined]);  // sort summary table by key statistic
  // Statistics
  Result.Insert('ar',   [rtUndefined]);  // Show attack rates
  Result.Insert('en',   [rtUndefined]);  // Minimized ar output... (contains less)
  Result.Insert('t',    [rtUndefined]);  // Chi2
  Result.Insert('ex',   [rtUndefined]);  // Exact tests for 2x2 tables
  Result.Insert('odds', [rtUndefined]);  // Odds ratio, including M-H adjusted
  Result.Insert('rr',   [rtUndefined]);  // Risk ratio, including M-H adjusted
//  Result.Insert('ci90',  [rtUndefined]);  // for OR and RR confidence intervals
//  Result.Insert('ci95',  [rtUndefined]);
//  Result.Insert('ci99',  [rtUndefined]);
end;

function TCTableCommand.GetAcceptedVariableCount: TBoundArray;
begin
  result := inherited GetAcceptedVariableCount;
  result[0] := -1;
end;

constructor TCTableCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stCTable);
end;

{ TAggregateCommand }

function TAggregateCommand.GetExecFlags: TCustomStatementExecutionFlags;
begin
  Result := inherited GetExecFlags;

  if HasOption('u') then
    Result := Result + [sefNoSelect];
end;

function TAggregateCommand.GetExecFlagsErrorMsg(
  Flag: TCustomStatementExecutionFlag): UTF8String;
begin
  Result := inherited GetExecFlagsErrorMsg(Flag);

  case Flag of
    sefNoSelect:
      Result := 'It is not possible to use the option !u in aggregate during an active select!';
  end;
end;

function TAggregateCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('q',       [rtUndefined]);
  Result.Insert('m',       [rtUndefined]);
  Result.Insert('nc',      [rtUndefined]);
  Result.Insert('nt',      [rtUndefined]);
  Result.Insert('replace', [rtUndefined]);
  Result.Insert('label',   AllResultDataTypes);
  Result.Insert('h',       [rtUndefined, rtObject],  [evtGlobalVector], [evfInternal, evfAsObject]);
  Result.Insert('ds',      [rtObject],               [evtDataset],      [evfInternal, evfExternal, evfAsObject]);
  Result.Insert('u',       [rtUndefined]);
  Result.Insert('full',    [rtUndefined]);

  AddDecimalOptions(Result);
  AddVariableLabelOptions(Result);
  AddValueLabelOptions(Result);

  // Summary statistics
  Result.Insert('des',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('iqr',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('idr',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('isr',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('mci',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('mv',   AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('mean', AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('sd',   AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('sv',   AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('min',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p1',   AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p5',   AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p10',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p25',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p50',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('med',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p75',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p90',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p95',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('p99',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('max',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
  Result.Insert('sum',  AllResultDataTypes, [evtField], [evfInternal, evfAsObject]);
end;

function TAggregateCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
end;

function TAggregateCommand.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);
end;

constructor TAggregateCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stAggregate);
end;

{ TReportValidateDoubleEntry }

function TReportValidateDoubleEntry.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('fn',   [rtString, rtUndefined]);
  Result.Insert('ds',   [rtObject], [evtDataset], [evfInternal, evfExternal, evfAsObject]);
  result.Insert('nol',  [rtUndefined]);
  Result.Insert('nos', [rtUndefined]);
  Result.Insert('nodt', [rtUndefined]);
  Result.Insert('noc', [rtUndefined]);
  Result.Insert('noauto', [rtUndefined]);
  Result.Insert('val', [rtUndefined]);
end;

function TReportValidateDoubleEntry.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  Result[0] := 0;
end;

function TReportValidateDoubleEntry.GetAcceptedVariableTypesAndFlags(
  Index: Integer): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);
  Result.Flags := [evfInternal, evfExternal, evfAsObject];
end;

constructor TReportValidateDoubleEntry.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, rscValidateDoubleEntry);
end;

{ TReportUsers }

function TReportUsers.GetAcceptedVariableCount: TBoundArray;
begin
  Result := nil;
end;

constructor TReportUsers.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, rscUsers);
end;

{ TEditData }

function TEditData.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('md',   [rtUndefined]);
  Result.Insert('nomd', [rtUndefined]);
  Result.Insert('mv',   [rtUndefined]);
  Result.Insert('nomv', [rtUndefined]);
end;

constructor TEditData.Create(AOptionList: TOptionList);
begin
  inherited Create(AOptionList, stEdit, ccData);
end;

{ TEraseCommand }

function TEraseCommand.GetRequireOpenProject: Boolean;
begin
  Result := false;
end;

function TEraseCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
end;

constructor TEraseCommand.Create(const AStringExpr: TExpr; AOptions: TOptionList
  );
begin
  inherited Create(AStringExpr, AOptions, 'erase');
end;

{ TCustomFilecommand }

function TCustomFilecommand.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  EV: TCustomExecutorVariable;
begin
  Result := inherited TypeCheck(Parser);

  if (Result) and
     Assigned(StringExpr)
  then
    begin
      result := StringExpr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData));

      if (not result) and
         (StringExpr is TCustomVariable)
      then
        begin
          DoTypeCheckError(
            'Remember " " and extension!' + LineEnding,
            Parser
          );
            Exit;
        end;

      if Result and
         (StringExpr.ResultType <> rtString)
      then
      begin
        DoTypeCheckError('Only String expressions allowed!', Parser);
        result := false;
      end;
    end;
end;

{ TEmptyStatement }

function TEmptyStatement.GetRequireOpenProject: Boolean;
begin
  Result := false;
end;

constructor TEmptyStatement.Create;
begin
  inherited Create(stNone);
end;

{ TSaveCommand }

function TSaveCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('format',  [rtString]);
  Result.Insert('replace', [rtUndefined]);
  Result.Insert('force',   [rtUndefined]);
  Result.Insert('ds',      [rtObject], [evtDataset], [evfInternal, evfAsObject]);

  // Save Output
  result.Insert('output',   [rtString, rtUndefined]);

  // Stata Options
  // - version
  Result.Insert('version', [rtInteger]);

  // CSV options
  // - header/varnames
  Result.Insert('varn',    [rtBoolean]);
  // - delimiter
  Result.Insert('d',       [rtString]);
  // - quotechar
  result.Insert('q',       [rtString]);
  // - datesep
  result.Insert('dated',   [rtString]);
  // - timesep
  result.Insert('timed',   [rtString]);
  // - decimalsep
  result.Insert('decd',    [rtString]);
  // - newline
  result.Insert('nl',      [rtString]);
  // - memo newline converts to
  result.Insert('memoln',  [rtString]);
  // - fixed
  result.Insert('fixed',   [rtUndefined]);
  // - ByteOrderMark
  result.Insert('bom',     [rtUndefined]);
end;

function TSaveCommand.GetRequireOpenProject: Boolean;
begin
  if (HasOption('output')) then
    result := false
  else
    Result := inherited GetRequireOpenProject;
end;

constructor TSaveCommand.Create(const AStringExpr: TExpr;
  AOptionList: TOptionList);
begin
  inherited Create(AStringExpr, AOptionList, 'save');
end;

{ TReportCountById }

function TReportCountById.GetRequireOpenProject: Boolean;
begin
  Result := false;
end;

function TReportCountById.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  result.Insert('fn', [rtObject], [evtGlobalVector], [evfInternal, evfAsObject]);
  result.Insert('ds', [rtObject], [evtGlobalVector], [evfInternal, evfAsObject]);
  result.Insert('nol', [rtUndefined]);
end;

function TReportCountById.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  Result[0] := -1;
end;

function TReportCountById.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);
  Result.Flags := [evfInternal, evfExternal, evfAsObject];
end;

constructor TReportCountById.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, rscCountById);
end;

{ TCustomReportCommand }

constructor TCustomReportCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList; ASubCmd: TReportSubCommand);
begin
  inherited Create(AVariableList, AOptionList, stReport);
  FSubCmd := ASubCmd;
end;

{ TReorderCommand }

function TReorderCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('before', [rtObject], [evtField], [evfInternal, evfAsObject]);
  Result.Insert('after',  [rtObject], [evtField], [evfInternal, evfAsObject]);
  Result.Insert('last',   [rtUndefined]);
end;

function TReorderCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  Result[0] := -1;
end;

constructor TReorderCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stReorder);
end;

{ TVariableRange }

constructor TVariableRange.Create(AStartVariable,
  AEndVariable: TCustomVariable; AOwnerList: TVariableList;
  AExecutor: IEpiScriptExecutor);
begin
  inherited Create('', AExecutor);
  FStartVariable := AStartVariable;
  FEndVariable   := AEndVariable;
end;

function TVariableRange.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
begin
  result := StartVariable.TypeCheck(TypeChecker, TypesAndFlags) and
            EndVariable.TypeCheck(TypeChecker, TypesAndFlags);

  if (not Result) then
    Exit;

  if (TypeChecker.GetVariableExecType(StartVariable.Ident) <> evtField) or
     (TypeChecker.GetVariableExecType(EndVariable.Ident) <> evtField)
  then
    begin
      DoTypeCheckError('Ranges of identifiers is only allowed for variables!', TypeChecker);
      result := false;
      Exit;
    end;

  if (TypesAndFlags.ExecutorVariableTypes <> [evtField]) then
  begin
    DoTypeCheckError('Ranges of identifiers is only allowed for variables!', TypeChecker);
    result := false;
    Exit;
  end;
end;

{ TStringCommand }

function TStringCommand.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if Assigned(StringExpr) then
    begin
      result := result and StringExpr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData));

      if Result and
         (StringExpr.ResultType <> rtString)
      then
      begin
        DoTypeCheckError('Only String expressions allowed!', Parser);
        result := false;
      end;
    end;
end;

{ TReadCommand }

function TReadCommand.GetRequireOpenProject: Boolean;
begin
  Result := false;
end;

function TReadCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  result.Insert('d',      [rtString]);
  result.Insert('q',      [rtString]);
  result.Insert('vn',     [rtBoolean]);
  result.Insert('pw',     [rtString]);
  result.Insert('login',  [rtString]);
  result.Insert('c',      [rtUndefined]);
  result.Insert('cb',     [rtUndefined]);
  result.Insert('force',  [rtUndefined]);
end;

constructor TReadCommand.Create(const AStringExpr: TExpr;
  AOptionList: TOptionList);
begin
  inherited Create(AStringExpr, AOptionList, 'read');
end;

{ TCheckDataCommand }

function TCheckDataCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('detail',  [rtUndefined]);
  Result.Insert('v',  [rtUndefined]);
  Result.Insert('l',  [rtUndefined]);
  Result.Insert('vl', [rtUndefined]);
  Result.Insert('lv', [rtUndefined]);
  Result.Insert('lst',   [rtUndefined]);
  Result.Insert('sysmis', [rtUndefined]);
end;

constructor TCheckDataCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, cscData);
end;

{ TCheckKeyCommand }

constructor TCheckKeyCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, cscKey);
end;

{ TCheckStudyCommand }

constructor TCheckStudyCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, cscStudy);
end;

{ TCheckRelateCommand }

constructor TCheckRelateCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, cscRelate);
end;

{ TCustomCheckCommandNoVariables }

function TCustomCheckCommandNoVariables.GetAcceptedVariableCount: TBoundArray;
begin
  result := nil;
end;

{ TCustomCheckCommand }

constructor TCustomCheckCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList; ASubCmd: TCheckSubCommand);
begin
  inherited Create(AVariableList, AOptionList, stCheck);
  FSubCmd := ASubCmd;
end;

{ TCheckRelateCommand }


{ TMergeCommand }

function TMergeCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  result.Insert('combine', [rtUndefined]);
  result.Insert('update',  [rtUndefined]);
  result.Insert('replace', [rtUndefined]);
  result.Insert('table',   [rtUndefined]);
  result.Insert('nu',      [rtUndefined]);
  result.Insert('label',   AllResultDataTypes);
  result.Insert('r',       [rtUndefined, rtObject], [], [evfAsObject, evfExternal]);
end;

function TMergeCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  if HasOption('filename') then
    result[0] := -1
  else
    result[0] := 0;
end;

function TMergeCommand.GetExecFlagsErrorMsg(Flag: TCustomStatementExecutionFlag
  ): UTF8String;
begin
  Result := inherited GetExecFlagsErrorMsg(Flag);

  case Flag of
    sefNoSelect:
      Result := 'Cannot merge with an active select!';
  end;
end;

constructor TMergeCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stMerge);
end;

{ TCustomMergeCommand }

function TCustomMergeCommand.GetExecFlags: TCustomStatementExecutionFlags;
begin
  Result := [sefNoSelect];
end;

function TCustomMergeCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  result.Insert('ds',       [rtObject], [evtDataset], [evfInternal, evfExternal, evfAsObject]);

  // Common options used for loading files - mostly when importing CSV files.
  AddReadOptions(Result);
end;

{ TMeansCommand }

function TMeansCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('by', [rtObject], [evtField], [evfInternal, evfAsObject]);
  Result.Insert('t',  [rtUndefined]);
  Result.Insert('q',  [rtUndefined]);
  AddDecimalOptions(Result);
end;

function TMeansCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  Result[0] := 1;
end;

function TMeansCommand.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited GetAcceptedVariableTypesAndFlags(Index);
  result.ResultTypes := [rtFloat, rtInteger];
end;

constructor TMeansCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stMeans);
end;

{ TBrowseCommand }

function TBrowseCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('caption', [rtString]);
  Result.Insert('c',       [rtUndefined]);
  Result.Insert('a',       [rtUndefined]);
  Result.Insert('del',     [rtUndefined]);
end;

constructor TBrowseCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stBrowse);
end;

{ TFreqCommand }

function TFreqCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('m',   [rtUndefined]);
  Result.Insert('cum', [rtUndefined]);
  Result.Insert('pr',  [rtUndefined]);
  Result.Insert('ci',  [rtUndefined]);
  Result.Insert('q',   [rtUndefined]);
  AddDecimalOptions(Result);
end;

function TFreqCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  result[0] := -1;
end;

constructor TFreqCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stFreq);
end;

{ TDescribeCommand }

function TDescribeCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('m',   [rtUndefined]);  // include missing
  Result.Insert('mci', [rtUndefined]);  // mean, conf. interval
  Result.Insert('msd', [rtUndefined]);  // sum, mean, sd
  Result.Insert('rm',  [rtUndefined]);  // range (min, max), median
  Result.Insert('iqr', [rtUndefined]);  // interquartile range, median
  Result.Insert('idr', [rtUndefined]);  // intrdecile range, median
  Result.Insert('q',   [rtUndefined]);  // no output
  Result.Insert('fl',  [rtUndefined]);  // lowest frequencies
  Result.Insert('fh',  [rtUndefined]);  // highest frequencies
  Result.Insert('fb',  [rtUndefined]);  // lowest and highest
  Result.Insert('pc',   [rtUndefined]); // show percent rather than counts
  Result.Insert('nf',  [rtUndefined]);  // no footer (not relevant)
  Result.Insert('ct',  [rtUndefined]);  // force compact table
  AddDecimalOptions(Result);
  AddVariableLabelOptions(Result);
  AddValueLabelOptions(Result);
end;
function TDescribeCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  result[0] := -1;
end;
constructor TDescribeCommand.Create(AVariableList: TVariableList; AOptionList: TOptionList);
begin
    inherited Create(AVariableList, AOptionList, stDescribe);
end;

{ TRecodeCommand }

function TRecodeCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  // Group size
  Result.Insert('by',   [rtInteger]);
  // Custom starting value
  Result.Insert('min',  [rtFloat, rtInteger]);
  // Custom starting value
  Result.Insert('max',  [rtFloat, rtInteger]);
  // Do not create valuelabel for new groups
  Result.Insert('nvl',  [rtUndefined]);
  // Replace variable & valuelabel set if already exists
  Result.Insert('replace', [rtUndefined]);
  // Grouping values are integers (if value is assigned, start from this and count up with 1)
  Result.Insert('i', [rtUndefined, rtInteger]);
  // Sets a specific value to be a missing value
  Result.Insert('m', [rtInteger]);
  // Assign a label to ?
  Result.Insert('label', AllResultDataTypes);
end;

function TRecodeCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  Result[0] := 1;
end;

function TRecodeCommand.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);
  Result.ResultTypes := [rtInteger, rtFloat];
end;

constructor TRecodeCommand.Create(AFromVariable, AToVariable: TCustomVariable;
  ARecodeIntervalList: TRecodeIntervalList; AOptionList: TOptionList);
var
  AVariableList: TVariableList;
begin
  AVariableList := TVariableList.Create;
  AVariableList.Add(AFromVariable);

  inherited Create(AVariableList, AOptionList, stRecode);

  FFromVariable       := AFromVariable;
  FToVariable         := AToVariable;
  FRecodeIntervalList := ARecodeIntervalList;
end;

function TRecodeCommand.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if (not Result) then
    exit;

//  if (RecodeIntervalList.Count > 0) then
//    Parser.TypeCheckError('Recode intervals not supported!', LineNo, ColNo, ByteNo);
end;

{ TSortCommand }

function TSortCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('descending', [rtUndefined]);
  Result.Insert('d',          [rtUndefined]);
end;

function TSortCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  result[0] := -1;
end;

constructor TSortCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stSort);
end;

{ TAppendCommand }

function TAppendCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  Result[0] := 0;
end;

function TAppendCommand.GetExecFlagsErrorMsg(Flag: TCustomStatementExecutionFlag
  ): UTF8String;
begin
  Result := inherited GetExecFlagsErrorMsg(Flag);

  case Flag of
    sefNoSelect:
      Result := 'Cannot append with an active select!';
  end;
end;

constructor TAppendCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList);
begin
  inherited Create(AVariableList, AOptionList, stAppend);
end;

{ TArray }

function TArray.GetCount: Integer;
begin
  result := FExprList.Count;
end;

constructor TArray.Create(const AExprList: TParamList);
begin
  inherited Create(otFunction, nil, nil);
  FExprList := AExprList;
  FResultSubType := rtUndefined;
end;

function TArray.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
var
  i: Integer;
  E: TExpr;
begin
  Result := inherited TypeCheck(TypeChecker, TypesAndFlags);

  FResultSubType := rtUndefined;

  for i := 0 to ExprList.Count - 1 do
    begin
      E := ExprList[i];
      Result := E.TypeCheck(TypeChecker, options_hashmap.TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData)) and result;

      if (FResultSubType = rtUndefined) then
        begin
          FResultSubType := E.ResultType;
          Continue;
        end;

      if (E.ResultType <> FResultSubType) then
        begin
          DoTypeCheckError(
            'Ambigious array items (no: %d)' + LineEnding +
              'Expected datatype: %s' + LineEnding +
              'Found datatype: %s',
            [i + 1, ASTResultTypeString[FResultSubType], ASTResultTypeString[ExprList[i].ResultType]],
            TypeChecker
          );
          Result := false;
          Exit;
        end;
    end;
end;

function TArray.ResultType: TASTResultType;
begin
  Result := rtArray;
end;

function TArray.DoEvaluate: boolean;
var
  i: Integer;
begin
  Result := inherited DoEvaluate;

  if (not Result) then
    Exit;

  for i := 0 to Count - 1 do
    Result := Result and ExprList[i].DoEvaluate;
end;

{ TCustomNewGlobal }

function TCustomNewGlobal.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('replace', [rtUndefined]);
end;

constructor TCustomNewGlobal.Create(AValueExpr: TExpr; ANewType: TEpiFieldType;
  const AVariable: TCustomVariable; AOptionList: TOptionList);
begin
  inherited Create(AValueExpr, ANewType, AVariable, AOptionList, ccGlobal);
end;

{ TVariableListEnumerator }

constructor TVariableListEnumerator.Create(List: TVariableList);
begin
  FList := List;
  FIdx := -1;
end;

function TVariableListEnumerator.MoveNext: boolean;
begin
  Inc(FIdx);
  result := (FIdx < FList.Count);
end;

function TVariableListEnumerator.GetCurrent: TCustomVariable;
begin
  result := FList.Variable[FIdx];
end;

{ TEditValueLabel }

function TEditValueLabel.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  result.Insert('d',    AllResultDataTypes);
  result.Insert('m',   AllResultDataTypes);
  result.Insert('nom', AllResultDataTypes);
end;

function TEditValueLabel.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);
  Result.ExecutorVariableTypes := [evtValuelabel];
end;

constructor TEditValueLabel.Create(AValueLabelPairs: TValueLabelPairs;
  AVariable: TCustomVariable; AOptionList: TOptionList);
begin
  inherited Create(AVariable, AOptionList, ccValuelabel);
  FValueLabelPairs := AValueLabelPairs;
end;

function TEditValueLabel.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);


  if Assigned(ValueLabelPairs) then
  begin
    // Since the variable type was not established at build time, we need
    // to extract it now and add the type to the ValueLabelPairs, then
    // do the typecheck.
    ValueLabelPairs.FValueType := Variable.ResultType;
    result := result and ValueLabelPairs.TypeCheck(Parser);
  end;
end;

{ TEditVariable }

function TEditVariable.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  result.Insert('label',        [rtString]);
  result.Insert('l',            [rtInteger]);
  result.Insert('d',            [rtInteger]);
  result.Insert('vl',           [rtObject], [evtValuelabel], [evfInternal, evfAsObject]);
  result.Insert('novl',         [rtUndefined]);
  result.Insert('max',          AllResultDataTypes);
  result.Insert('min',          AllResultDataTypes);
  result.Insert('norange',      [rtUndefined]);
  result.Insert('entry',        [rtInteger]);
  result.Insert('confirm',      [rtUndefined]);
  result.Insert('noconfirm',    [rtUndefined]);
  result.Insert('key',          [rtUndefined]);
  result.Insert('nokey',        [rtUndefined]);
  result.Insert('nocmp',        [rtUndefined]);
  result.Insert('cmpEQ',        [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpNE',        [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpGT',        [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpLT',        [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpGE',        [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpLE',        [rtObject], [evtField], [evfInternal, evfAsObject]);
end;

constructor TEditVariable.Create(AVariable: TCustomVariable;
  AOptionList: TOptionList);
begin
  inherited Create(AVariable, AOptionList, ccVariable);
end;

{ TEditDataset }

function TEditDataset.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  Result.Insert('label',     [rtString]);
  Result.Insert('childobs',  [rtInteger]);
  Result.Insert('afterobs',  [rtInteger]);
  Result.Insert('statusbar', [rtString]);
  Result.Insert('size',      [rtInteger]);
  Result.Insert('noparent',  [rtUndefined]);
end;

function TEditDataset.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  result := inherited GetAcceptedVariableTypesAndFlags(Index);
  Result.ExecutorVariableTypes := [evtDataset]
end;

constructor TEditDataset.Create(AVariable: TCustomVariable;
  AOptionList: TOptionList);
begin
  inherited Create(AVariable, AOptionList, ccDataset);
end;

{ TCustomEditCommand }

function TCustomEditCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
  result.Insert('r', [rtObject], ExecutorVariableTypesAll, [evfExternal, evfAsObject]);
end;

function TCustomEditCommand.GetVariable: TCustomVariable;
begin
  result := FVariableList[0];
end;

constructor TCustomEditCommand.Create(AVariable: TCustomVariable;
  AOptionList: TOptionList; ASubCommand: TCrudCommand);
var
  VarList: TVariableList;
begin
  VarList := TVariableList.Create;
  if Assigned(AVariable) then
    VarList.Add(AVariable);

  inherited Create(VarList, AOptionList, stEdit, ASubCommand);
end;

{ TEditProject }

function TEditProject.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  Result.Insert('title', [rtString]);
  Result.Insert('pw',    [rtString]);
end;

constructor TEditProject.Create(AOptionList: TOptionList);
begin
  inherited Create(AOptionList, stEdit, ccProject);
end;

{ TValueLabelPairs }

function TValueLabelPairs.GetCount: Integer;
begin
  result := FValues.Count;
end;

function TValueLabelPairs.GetText(const Index: Integer): TExpr;
begin
  result := TExpr(FTexts[Index]);
end;

function TValueLabelPairs.GetValues(const Index: Integer): TExpr;
begin
  result := TExpr(FValues[Index]);
end;

constructor TValueLabelPairs.Create(AValueType: TASTResultType);
begin
  FValues := TObjectList.Create(false);
  FTexts  := TObjectList.Create(false);
  FValueType := AValueType;
end;

function TValueLabelPairs.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  Expr: TExpr;
  i: Integer;
begin
  Result := inherited TypeCheck(Parser);

  if result then
    for i := 0 to FValues.Count -1 do
    begin
      Expr := TExpr(FValues[I]);
      Result := Expr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData));

      if (Expr.ResultType <> FValueType) then
      begin
        Parser.TypeCheckError(
          Format('Value does not have the correct type! ' + LineEnding +
                 'Expected %s but got %s', [ASTResultTypeString[FValueType], ASTResultTypeString[Expr.ResultType]]),
          Expr.LineNo,
          Expr.ColNo,
          Expr.ByteNo
        );
        result := false;
      end;

      if (not result) then
        break;

      Expr := TExpr(FTexts[I]);
      Result := Expr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData));

      if (not result) then break;
    end;
end;

function TValueLabelPairs.Find(const S: UTF8String; out Index: Integer
  ): boolean;
begin
  Index := 0;
  while (Index < FValues.Count) do
    begin
      if TExpr(FValues[Index]).AsString = S then
        break;
      Inc(Index);
    end;
  result := (Index < FValues.Count);
end;

procedure TValueLabelPairs.AddPair(Value, Text: TExpr);
begin
  FValues.Add(Value);
  FTexts.Add(Text);
end;

{ TNewGlobalVector }

constructor TNewGlobalVector.Create(AVectorExpr, AValueExpr: TExpr;
  ANewType: TEpiFieldType; const AVariable: TCustomVariable;
  AOptionList: TOptionList);
begin
  inherited Create(AValueExpr, ANewType, AVariable, AOptionList);
  FVectorExpr := AVectorExpr;
end;

function TNewGlobalVector.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser) and
            VectorExpr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData));

  if Result then
  begin
    if (not (VectorExpr.ResultType = rtInteger)) then
      DoTypeCheckError('New global - size expressions must return an integer!', Parser);
  end;
end;

{ TNewGlobal }

constructor TNewGlobal.Create(AValueExpr: TExpr; ANewType: TEpiFieldType;
  const AVariable: TCustomVariable; AOptionList: TOptionList);
begin
  inherited Create(AValueExpr, ANewType, AVariable, AOptionList);
end;

{ TNewVariable }

function TNewVariable.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  result.Insert('label',      AllResultDataTypes);
  result.Insert('l',          [rtInteger]);
  result.Insert('d',          [rtInteger]);
  result.Insert('vl',         [rtObject], [evtValuelabel], [evfInternal, evfAsObject]);
  result.Insert('min',        AllResultDataTypes);
  result.Insert('max',        AllResultDataTypes);
  result.Insert('entry',      [rtInteger]);
  result.Insert('confirm',    [rtUndefined]);
  result.Insert('key',        [rtUndefined]);
  result.Insert('cmpEQ',      [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpNE',      [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpGT',      [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpLT',      [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpGE',      [rtObject], [evtField], [evfInternal, evfAsObject]);
  result.Insert('cmpLE',      [rtObject], [evtField], [evfInternal, evfAsObject]);

  case NewType of
    ftBoolean: ;  // No extra options for boolean

    ftFloat: ;    // No extra options for float

    ftInteger:
      result.Insert('auto', [rtUndefined]);

    ftDMYDate:
      begin
        result.Insert('auto', [rtUndefined, rtInteger]);
        result.Insert('dmy',  [rtUndefined]);
        result.Insert('mdy',  [rtUndefined]);
        result.Insert('ymd',  [rtUndefined]);
      end;

    ftTime:
      result.Insert('auto', [rtUndefined, rtInteger]);

    ftString:
      begin
        result.Insert('u',    [rtUndefined]);
        result.Insert('memo', [rtUndefined]);
      end;
  end;
end;

constructor TNewVariable.Create(AValueExpr: TExpr; ANewType: TEpiFieldType;
  const AVariable: TCustomVariable; AOptionList: TOptionList);
begin
  inherited Create(AValueExpr, ANewType, AVariable, AOptionList, ccVariable);
end;

{ TCustomNewValued }

constructor TCustomNewValued.Create(AValueExpr: TExpr; ANewType: TEpiFieldType;
  const AVariable: TCustomVariable; AOptionList: TOptionList;
  ASubCommand: TCrudCommand);
begin
  inherited Create(ANewType, AVariable, AOptionList, ASubCommand);
  FValueExpr := AValueExpr;
end;

function TCustomNewValued.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  VarT, ExpT: TASTResultType;
  EFlags: TTypesAndFlagsRec;
begin
  Result := inherited TypeCheck(Parser);

  EFlags := TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData);

  // Since assignment TO a variable should also accept a variable.
  if (SubCommand = ccVariable) then
    Include(EFlags.Flags, evfAsObject);

  if Assigned(ValueExpr) then
    result := result and ValueExpr.TypeCheck(Parser, EFlags);

  if Result and
     (Assigned(ValueExpr))
  then
    begin
      // TODO: Account for rtArray and vectors.

      if (Self is TNewGlobalVector) then
        begin
          ExpT := ValueExpr.ResultType;

          if (ExpT = rtArray) then
            ExpT := TArray(ValueExpr).ResultSubType;

        end
      else
        ExpT := ValueExpr.ResultType;

      VarT := FieldTypeToASTTypeTable[NewType];
      Result := Ord(VarT) >= Ord(ExpT);

      if not result then
        DoTypeCheckError(
          'Incompatible types: ' + LineEnding +
          'Variable ' + Variable.Ident + ' expect result to be of type ' +  ASTResultTypeString[VarT] + LineEnding +
          'but expression is of type ' + ASTResultTypeString[ExpT],
          Parser
        );
    end;
end;

{ TNewValuelabel }

function TNewValuelabel.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  result.Insert('m', AllResultDataTypes);
  result.Insert('replace', [rtUndefined]);
end;

constructor TNewValuelabel.Create(AValueLabelPairs: TValueLabelPairs;
  ANewType: TEpiFieldType; const AVariable: TCustomVariable;
  AOptionList: TOptionList);
begin
  inherited Create(ANewType, AVariable, AOptionList, ccValuelabel);
  FValueLabelPairs := AValueLabelPairs;
end;

function TNewValuelabel.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if Assigned(ValueLabelPairs) then
    result := Result and ValueLabelPairs.TypeCheck(Parser);
end;

{ TNewDataset }

function TNewDataset.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  Result.Insert('parent',    [rtObject], [evtDataset], [evfInternal, evfAsObject]);
  Result.Insert('label',     [rtString]);
  Result.Insert('childobs',  [rtInteger]);
  Result.Insert('afterobs',  [rtInteger]);
  Result.Insert('statusbar', [rtString]);
  Result.Insert('size',      [rtInteger]);
end;

constructor TNewDataset.Create(const AVariable: TCustomVariable;
  AOptionList: TOptionList);
begin
  inherited Create(AVariable, AOptionList, ccDataset);
end;

{ TReferencedVariable }

function TReferencedVariable.GetReferenceVariable: TCustomVariable;
begin
  if (not Assigned(FReferenceVariable)) then
    FReferenceVariable := TVariable.Create(FExpr.Evaluate.AsString, Executor);

  result := FReferenceVariable;
end;

function TReferencedVariable.GetIdent: UTF8String;
begin
  Result := AsIdent;
end;

constructor TReferencedVariable.Create(AExpr: TExpr;
  AExecutor: IEpiScriptExecutor);
begin
  inherited Create('', AExecutor);
  FExpr    := AExpr;
  FVarType := vtReferenced;
  FReferenceVariable := nil;
end;

destructor TReferencedVariable.Destroy;
begin
  if Assigned(FReferenceVariable) then
    FReferenceVariable.Free;
  inherited Destroy;
end;

function TReferencedVariable.ResultType: TASTResultType;
begin
  result := ReferenceVariable.ResultType;
end;

function TReferencedVariable.TypeCheck(TypeChecker: IEpiTypeChecker;
  ATypesAndFlags: TTypesAndFlagsRec): boolean;
var
  VarTypeAndFlags: TTypesAndFlagsRec;
begin
  VarTypeAndFlags := TypesAndFlags(AllResultTypes, ExecutorVariableTypesData);

  Result := FExpr.TypeCheck(TypeChecker, VarTypeAndFlags) and
            ReferenceVariable.TypeCheck(TypeChecker, ATypesAndFlags);

  if result and
     (FExpr.ResultType <> rtString)
  then
    begin
      DoTypeCheckError('The datatype of the expression MUST be of type %s', [ASTResultTypeString[rtString]], TypeChecker);
      result := false;
      Exit;
    end;
end;

function TReferencedVariable.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;{ and
            ReferenceVariable.DoEvaluate;

  if (not Result) then
    Exit;

  FEvalValue.BoolVal   := ReferenceVariable.AsBoolean;
  FEvalValue.IntVal    := ReferenceVariable.AsInteger;
  FEvalValue.DateVal   := ReferenceVariable.AsDate;
  FEvalValue.FloatVal  := ReferenceVariable.AsFloat;
  FEvalValue.TimeVal   := ReferenceVariable.AsTime;
  FEvalValue.StringVal := ReferenceVariable.AsString; }
end;

{function TReferencedVariable.AsBoolean: Boolean;
begin
  Result := ReferenceVariable.AsBoolean;
end;

function TReferencedVariable.AsInteger: ASTInteger;
begin
  Result := ReferenceVariable.AsInteger;
end;

function TReferencedVariable.AsFloat: ASTFloat;
begin
  Result := ReferenceVariable.AsFloat;
end;

function TReferencedVariable.AsDate: EpiDate;
begin
  Result := ReferenceVariable.AsDate;
end;

function TReferencedVariable.AsTime: EpiDateTime;
begin
  Result := ReferenceVariable.AsTime;
end;

function TReferencedVariable.AsString: EpiString;
begin
  Result := ReferenceVariable.AsString;
end;                             }

function TReferencedVariable.AsIdent: UTF8String;
begin
  Result := ReferenceVariable.AsIdent;
end;

function TReferencedVariable.IsMissing: Boolean;
begin
  Result := ReferenceVariable.IsMissing;
end;

{ TDropCommand }

function TDropCommand.GetRequireOpenProject: Boolean;
begin
  result := true;

  case SubCommand of
    ccData: ;
    ccVariable: ;
    ccDataset: ;
    ccProject: ;
    ccValuelabel: ;

    ccGlobal:
      result := false;

    ccResult: ;
  end;
end;

function TDropCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;
  Result[0] := 0;

  case SubCommand of
    ccData:
      result := nil;

    ccVariable: ;
    ccDataset: ;
    ccValuelabel: ;
    ccGlobal:
      if HasOption('all') then
        Result := nil;
  end;

end;

function TDropCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  case SubCommand of
    ccData:
      result.Insert('del', [rtUndefined]);

    ccVariable: ;
    ccDataset: ;
    ccProject: ;
    ccValuelabel: ;
    ccGlobal:
      begin
        result.Insert('all', [rtUndefined, rtString]);
      end;

    ccResult: ;
  end;
end;

function TDropCommand.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);

  // This ensures that drop doesn't flunk on non-existing identifiers
  Include(Result.Flags, evfExternal);

  case SubCommand of
    ccDataset:
      result.ExecutorVariableTypes := [evtDataset];

    ccGlobal:
      result.ExecutorVariableTypes := [evtGlobal, evtGlobalVector];

    ccVariable:
      result.ExecutorVariableTypes := [evtField];

    ccData:
      result.ExecutorVariableTypes := [];

    ccValuelabel:
      result.ExecutorVariableTypes := [evtValuelabel];
  end;
end;

constructor TDropCommand.Create(AVariables: TVariableList;
  AOptionList: TOptionList; ASubCommand: TCrudCommand);
begin
  inherited Create(AVariables, AOptionList, stDrop, ASubCommand);
end;

{ TNewProject }

function TNewProject.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  Result.Insert('size',  [rtInteger]);
  Result.Insert('title', [rtString]);
  Result.Insert('c',     [rtUndefined]);
  Result.Insert('pw',    [rtString]);
end;

constructor TNewProject.Create(AOptionList: TOptionList);
begin
  inherited Create(AOptionList, ccProject);
end;

{ TCustomNewNamed }

constructor TCustomNewNamed.Create(const AVariable: TCustomVariable;
  AOptionList: TOptionList; ASubCommand: TCrudCommand);
begin
  inherited Create(AOptionList, ASubCommand);
  FVariable := AVariable;
end;

function TCustomNewNamed.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result :=
    inherited TypeCheck(Parser) and
    FVariable.TypeCheck(Parser, TypesAndFlags(AllResultTypes, ExecutorVariableTypesAll, [evfExternal, evfAsObject]));
end;

{ TCustomVariablesCrudCommand }

function TCustomVariablesCrudCommand.GetVariableList: TVariableList;
begin
  result := FVariableList;
end;

function TCustomVariablesCrudCommand.GetAcceptedVariableCount: TBoundArray;
begin
  SetLength(Result, 1);
  Result[0] := 1;
end;

function TCustomVariablesCrudCommand.GetAcceptedVariableTypesAndFlags(
  Index: Integer): TTypesAndFlagsRec;
begin
  result.Flags                 := [evfInternal, evfAsObject];
  result.ExecutorVariableTypes := [evtField];
  result.ResultTypes           := AllResultDataTypes;
end;

constructor TCustomVariablesCrudCommand.Create(AVariables: TVariableList;
  AOptionList: TOptionList; AStatementType: TASTStatementType;
  ASubCommand: TCrudCommand);
begin
  inherited Create(AOptionList, AStatementType, ASubCommand);
  FVariableList := AVariables;
end;

function TCustomVariablesCrudCommand.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);
end;

{ TListCommand }

function TListCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  if SubCommand in [ccData] then
    begin
      AddValueLabelOptions(Result);
      Result.Insert('del', [rtUndefined]);
    end;

  if SubCommand in [ccData, ccDataset] then
    AddVariableLabelOptions(Result);

  if SubCommand = ccDataset then
    Result.Insert('all', [rtUndefined]);

  if SubCommand = ccProject then
    Result.Insert('info', [rtUndefined]);
end;

function TListCommand.GetRequireOpenProject: Boolean;
begin
  case SubCommand of
    ccVariable,
    ccDataset,
    ccProject,
    ccValuelabel,
    ccData:
      result := true;

    ccGlobal,
    ccResult:
      result := false;
  end;
end;

function TListCommand.GetAcceptedVariableCount: TBoundArray;
begin
  Result := inherited GetAcceptedVariableCount;

  case SubCommand of
    ccGlobal,
    ccData,
    ccResult,
    ccValuelabel,
    ccVariable:
      result[0] := 0;
  else
    {ccVariable: ;
    ccDataset: ;
    ccProject: ;
    ccValuelabel: ;
    ccGlobal: ;
    ccResult: ; }
    result := nil;
  end;
end;

function TListCommand.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);

  case SubCommand of
    ccGlobal:
      Result.ExecutorVariableTypes := [evtGlobal, evtGlobalVector];

    ccVariable:
      Result.ExecutorVariableTypes := [evtField];

    ccDataset:
      Result.ExecutorVariableTypes := [evtDataset];

    ccValuelabel:
      Result.ExecutorVariableTypes := [evtValuelabel];

    ccResult:
      result.ExecutorVariableTypes := [evtResultConst, evtResultVector, evtResultMatrix];
  end;
end;

constructor TListCommand.Create(AVariables: TVariableList;
  AOptionList: TOptionList; ASubCommand: TCrudCommand);
begin
  inherited Create(AVariables, AOptionList, stList, ASubCommand);
end;

{ TEditCommand }

function TEditCommand.GetVariable: TCustomVariable;
begin
  result := Variables[0];
end;

function TEditCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  case SubCommand of
    ccProject:
      begin
        Result.Insert('title', [rtString]);
      end;

    ccVariable:
      begin
        result.Insert('label',      [rtString]);
        result.Insert('length',     [rtInteger]);
        result.Insert('decimal',    [rtInteger]);
        result.Insert('valuelabel', [rtObject], [evtValuelabel], [evfInternal, evfAsObject]);
        result.Insert('rangelow',   AllResultDataTypes);
        result.Insert('rangehigh',  AllResultDataTypes);
        result.Insert('entrymode',  [rtInteger]);
        result.Insert('confirm',    [rtBoolean]);
      end;

    ccDataset:
      begin
        Result.Insert('label',     [rtString]);
        Result.Insert('childrec',  [rtInteger]);
        Result.Insert('afterrec',  [rtInteger]);
        Result.Insert('statusbar', [rtString]);
        Result.Insert('size',      [rtInteger]);
      end;

    ccValuelabel: ;
    ccGlobal: ;
    ccResult: ;
  end;
end;

function TEditCommand.GetAcceptedVariableCount: TBoundArray;
begin
  result := inherited GetAcceptedVariableCount;

  if (SubCommand = ccProject) then
    SetLength(Result, 0);
end;

function TEditCommand.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result := inherited GetAcceptedVariableTypesAndFlags(Index);
  case SubCommand of
    ccVariable:
      Result.ExecutorVariableTypes := [evtField];

    ccDataset:
      Result.ExecutorVariableTypes := [evtDataset];

    ccProject:
      result.ExecutorVariableTypes := [];

    ccValuelabel:
      result.ExecutorVariableTypes := [evtValuelabel];
  end;
end;

constructor TEditCommand.Create(AVariable: TCustomVariable;
  AOptionList: TOptionList; ASubCommand: TCrudCommand);
var
  AVariables: TVariableList;
begin
  AVariables := TVariableList.Create;

  if (Assigned(AVariable)) then
    AVariables.Add(AVariable);

  inherited Create(AVariables, AOptionList, stEdit, ASubCommand);
end;

{ TUse }

function TUse.GetAcceptedOptions: TStatementOptionsMap;
begin
  result := TStatementOptionsMap.Create;
end;

function TUse.GetOptionList: TOptionList;
begin
  result := FOptionList;
end;


function TUse.GetVariableList: TVariableList;
begin
  result := TVariableList.Create;
  result.Add(Variable);
//  FTmpVariableList := Result;
end;

function TUse.GetAcceptedVariableCount: TBoundArray;
begin
  SetLength(Result, 1);
  Result[0] := 1;
end;

function TUse.GetAcceptedVariableTypesAndFlags(Index: Integer): TTypesAndFlagsRec;
begin
  Result.ExecutorVariableTypes := [evtDataset];
  Result.ResultTypes := [];
  Result.Flags := [evfInternal, evfAsObject];
end;

function TUse.DoVarialeCheck(VariableChecker: IVariableCheck;
  TypeChecker: IEpiTypeChecker): Boolean;
begin
  Result := inherited DoVarialeCheck(VariableChecker, TypeChecker);
  FreeAndNil(FTmpVariableList);
end;

constructor TUse.Create(AVariable: TCustomVariable; AOptions: TOptionList);
begin
  inherited Create(stUse);
  FVariable := AVariable;
//  FExpr := AExpr;
  FOptionList := AOptions;
end;

function TUse.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser){ and

  if not (Expr.ResultType in [rtString, rtObject]) then
  begin
    DoTypeCheckError('Only Variables allowed!', Parser);
    result := false;
  end;  }
end;

{ TCustomCrudCommand }

function TCustomCrudCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  result := TStatementOptionsMap.Create;
end;

function TCustomCrudCommand.GetOptionList: TOptionList;
begin
  result := FOptionList;
end;

constructor TCustomCrudCommand.Create(AOptionList: TOptionList;
  AStatementType: TASTStatementType; ASubCommand: TCrudCommand);
begin
  inherited Create(AStatementType);
  FOptionList := AOptionList;
  FSubCommand := ASubCommand;
end;

function TCustomCrudCommand.HasOption(const Ident: UTF8String; out
  AOption: TOption): boolean;
begin
  result := OptionList.HasOption(Ident, AOption);
end;

function TCustomCrudCommand.HasOption(const Ident: UTF8String): boolean;
begin
  result := OptionList.HasOption(Ident);
end;


{ TSetCommand }

function TSetCommand.GetRequireOpenProject: Boolean;
begin
  result := false;
end;

constructor TSetCommand.Create(AOptionExpr, AAssignmentExpr: TExpr);
begin
  inherited Create(stSet);
  FOptionExpr := AOptionExpr;
  FAssignmentExpr := AAssignmentExpr;
end;

function TSetCommand.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if Assigned(OptionExpr) then
  begin
    Result := Result and OptionExpr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData));
//    Result := Result and (OptionExpr.ResultType = rtString);

    if Result and
       (OptionExpr.ResultType <> rtString)
    then
      begin
        DoTypeCheckError('Set Option Name must be a string!', Parser);
        Result := false;
      end;
  end;

  if Assigned(AssignmentExpr) then
    Result := Result and AssignmentExpr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData));
end;

{ TCustomNewTyped }

constructor TCustomNewTyped.Create(ANewType: TEpiFieldType;
  const AVariable: TCustomVariable; AOptionList: TOptionList;
  ASubCommand: TCrudCommand);
begin
  inherited Create(AVariable, AOptionList, ASubCommand);
  FNewType := ANewType;
end;

{ TAssertCommand }

function TAssertCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  Result.Insert('fail', [rtUndefined]);
  Result.Insert('halt', [rtUndefined]);
  Result.Insert('q',    [rtUndefined]);
end;

function TAssertCommand.GetRequireOpenProject: Boolean;
begin
  result := false;
end;

constructor TAssertCommand.Create(AStatement: TCustomStatement;
  AOptionList: TOptionList);
begin
  inherited Create(AOptionList, stAssert);
  FStatement := AStatement;
end;

function TAssertCommand.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  Expr: TExpr;
begin
  Result := inherited TypeCheck(Parser);

  if (result) and
     (FStatement.InheritsFrom(TExpr)) and
     (not (TExpr(FStatement).ResultType = rtBoolean))
  then
    begin
      DoTypeCheckError(
        rsExpressionReturnType1,
        [ASTResultTypeString[rtBoolean]],
        Parser
      );
      Result := false;
    end;
end;

{ TParamDeclerationTypeList }

function TParamDeclerationTypeList.GetCount: Integer;
begin
  result := FParamPairList.Count;
end;

function TParamDeclerationTypeList.GetItem(Ident: UTF8String): TParamPair;
var
  Pair: TParamPair;
begin
  for Pair in Self do
    if (UTF8CompareText(Pair.Ident, Ident)) = 0 then
      Exit(Pair);

  Result := nil;
end;

function TParamDeclerationTypeList.GetItems(Index: Integer): TParamPair;
begin
  result := FParamPairList.Items[Index];
end;

constructor TParamDeclerationTypeList.Create;
begin
  FParamPairList := TParamPairList.Create;
end;

procedure TParamDeclerationTypeList.Add(ParamPair: TParamPair);
begin
  FParamPairList.Add(ParamPair);
end;

function TParamDeclerationTypeList.GetEnumerator: TParamDeclerationTypeListEnumerator;
begin
  result := TParamDeclerationTypeListEnumerator.Create(self);
end;


{ TParamDeclerationTypeListEnumerator }

procedure TParamDeclerationTypeListEnumerator.RaiseCountError;
begin
  raise Exception.CreateFmt(
      'Enumeration error!' + LineEnding +
      'Expected %d items in list, but MoveNext found %d!' + LineEnding +
      'Deleting/Inserting items during iteration is NOT supported!',
      [FListCount, FParamDeclerationTypeList.Count]
    );
end;

procedure TParamDeclerationTypeListEnumerator.CheckListCount;
begin
  if (FListCount <> FParamDeclerationTypeList.Count) then
    RaiseCountError;
end;

function TParamDeclerationTypeListEnumerator.GetCurrent: TParamPair;
begin
  result := FParamDeclerationTypeList[FCurrentIndex];
end;

constructor TParamDeclerationTypeListEnumerator.Create(
  ParamDeclerationTypeList: TParamDeclerationTypeList);
begin
  FParamDeclerationTypeList := ParamDeclerationTypeList;
  FCurrentIndex := -1;
  FListCount := FParamDeclerationTypeList.Count;
end;

function TParamDeclerationTypeListEnumerator.MoveNext: Boolean;
begin
  CheckListCount;
  Inc(FCurrentIndex);
  Result := (FCurrentIndex < FListCount);
end;

{ TFunctionListEnumerator }

procedure TFunctionListEnumerator.RaiseCountError;
begin
  raise Exception.CreateFmt(
      'Enumeration error!' + LineEnding +
      'Expected %d items in list, but MoveNext found %d!' + LineEnding +
      'Deleting/Inserting items during iteration is NOT supported!',
      [FListCount, FFunctionList.Count]
    );
end;

procedure TFunctionListEnumerator.CheckListCount;
begin
  if (FListCount <> FFunctionList.Count) then
    RaiseCountError;
end;

function TFunctionListEnumerator.GetCurrent: TFunctionDefinition;
begin
  Result := FFunctionList[FCurrentIndex];
end;

constructor TFunctionListEnumerator.Create(FunctionList: TFunctionList);
begin
  FFunctionList := FunctionList;
  FCurrentIndex := -1;
  FListCount := FFunctionList.Count;
end;

function TFunctionListEnumerator.MoveNext: Boolean;
begin
  CheckListCount;
  Inc(FCurrentIndex);
  Result := (FCurrentIndex < FFunctionList.Count);
end;

{ TFunctionList }

function TFunctionList.GetCount: Integer;
begin
  result := FFunctionList.Count;
end;

function TFunctionList.GetItems(Index: Integer): TFunctionDefinition;
begin
  result := FFunctionList.Items[Index];
end;

constructor TFunctionList.Create;
begin
  FFunctionList := TFunctionDefinitionList.Create;
end;

destructor TFunctionList.Destroy;
begin
  FFunctionList.Clear;
  inherited Destroy;
end;

procedure TFunctionList.Add(AFunction: TFunctionDefinition);
begin
  FFunctionList.Add(AFunction);
end;

function TFunctionList.GetEnumerator: TFunctionListEnumerator;
begin
  result := TFunctionListEnumerator.Create(Self);
end;

{ TFunctionDefinition }

function TFunctionDefinition.GetRequireOpenProject: Boolean;
begin
  Result := false;
end;

constructor TFunctionDefinition.Create(const Ident: UTF8String;
  ParameterTypeList: TParamDeclerationTypeList; ReturnType: TASTResultType;
  Statements: TStatementList; ReturnStatements: TReturnStatementList);
var
  Item: TReturn;
begin
  inherited Create(stFunctionDefinition);
  FIdent             := Ident;
  FParameterTypeList := ParameterTypeList;
  FReturnType        := ReturnType;
  FStatements        := Statements;
  FReturnStatements  := ReturnStatements;

  for Item in FReturnStatements do
    Item.FParentFunction := Self;
end;

constructor TParamPair.Create(AIdent: UTF8String; AParamType: TASTResultType);
begin
  FIdent := AIdent;
  FParamType := AParamType;
end;

{ TReturn }

constructor TReturn.Create(AReturnExpression: TExpr);
begin
  inherited Create(stReturn);
  FReturnExpression := AReturnExpression;
end;

function TReturn.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);
end;

{ TEvalExpression }

function TEvalExpression.GetRequireOpenProject: Boolean;
begin
  result := false;
end;

constructor TEvalExpression.Create(AExpr: TExpr);
begin
  inherited Create(stEval);
  FExpr := AExpr;
end;

function TEvalExpression.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser) and
            Expr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData));
end;

{ TIndexVariable }

function TIndexVariable.GetParamCount: Integer;
begin
  Result := FExprList.Count;
end;

function TIndexVariable.GetExpr(const Index: Integer): TExpr;
begin
  result := FExprList[Index];
end;

constructor TIndexVariable.Create(AVariable: TCustomVariable;
  AExecutor: IEpiScriptExecutor; AExprList: TParamList);
begin
  inherited Create(''{AVariable.Ident}, AExecutor);
  FVariable := AVariable;
  FVarType  := vtIndexed;
  FExprList := AExprList;
end;

destructor TIndexVariable.Destroy;
begin
  inherited Destroy;
end;

function TIndexVariable.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
var
  i: Integer;
  E: TExpr;
  EV: TCustomExecutorVariable;
  TaF: TTypesAndFlagsRec;
begin
  TaF := options_hashmap.TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData, [evfInternal, evfAsObject]);

  if (evfExternal in TypesAndFlags.Flags) then
    Include(TaF.Flags, evfExternal);

  result := FVariable.TypeCheck(TypeChecker, TaF);

  // Since we initially set an empty name - we must update it now
  if (Result) then
    FIdent := FVariable.Ident;

  Result := Result and
            inherited TypeCheck(TypeChecker, TypesAndFlags);

  if (not Result) or
     (evfExternal in TypesAndFlags.Flags)
  then
    Exit;

  EV := TypeChecker.GetExecVariable(Ident);
  case EV.VarType of
    evtDataset,
    evtValuelabel,
    evtGlobal,
    evtResultConst:
      begin
        Result := false;
        DoTypeCheckError('Identifier "' + Ident + '" does not accept an index', TypeChecker);
        Exit;
      end;

    evtField,
    evtGlobalVector,
    evtResultVector:
      begin
        Result := (evfAsValue in TypesAndFlags.Flags);

        if (not Result) then
        begin
          DoTypeCheckError('Identifier "' + Ident + '" does not accept an index', TypeChecker);
          Exit;
        end;

        if (ParamCount > 1) then
        begin
          Result := false;
          DoTypeCheckError('Identifier "' + Ident + '" only accepts one index', TypeChecker);
          Exit;
        end;
      end;

    evtResultMatrix:
      begin
        Result := (evfAsValue in TypesAndFlags.Flags);

        if (not Result) then
        begin
          DoTypeCheckError('Identifier "' + Ident + '" does not accept an index', TypeChecker);
          Exit;
        end;

        if (ParamCount > 2) then
        begin
          Result := false;
          DoTypeCheckError('Identifier "' + Ident + '" only accepts one index', TypeChecker);
          Exit;
        end;
      end;

  else
    begin
      DoTypeCheckError('TVariable.TypeCheck: Unknown ExecVariableType!', TypeChecker);
      Exit;
    end;
  end;

  for i := 0 to ParamCount - 1 do
    begin
      E := FExprList[i];

      Result := E.TypeCheck(TypeChecker, options_hashmap.TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData)) and result;

      if Result and
         (not (E.ResultType = rtInteger))
      then
        begin
          DoTypeCheckError(
            rsExpressionReturnType1,
            [ASTResultTypeString[rtInteger]],
            TypeChecker
          );
          Result := false;
        end;
    end;
end;

function TIndexVariable.DoEvaluate: boolean;
var
  i: Integer;
begin
  Result := true;

  for i := 0 to ParamCount - 1 do
    Result := Result and Expr[i].DoEvaluate;

  Result := Result and (inherited DoEvaluate);
end;

{ TCustomOptionsCommand }

function TCustomOptionsCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  result := TStatementOptionsMap.Create;
end;

function TCustomOptionsCommand.GetOptionList: TOptionList;
begin
  result := FOptions;
end;

constructor TCustomOptionsCommand.Create(AOptionList: TOptionList;
  const ACommand: UTF8String);
begin
  inherited Create(ACommand);

  FOptions := AOptionList;
end;

constructor TCustomOptionsCommand.Create(AOptionList: TOptionList;
  ST: TASTStatementType);
begin
  inherited Create(ST);

  FOptions := AOptionList;
end;

function TCustomOptionsCommand.HasOption(const Ident: UTF8String; out
  AOption: TOption): boolean;
begin
  result :=
    Assigned(Options) and
    Options.HasOption(Ident, AOption);
end;

function TCustomOptionsCommand.HasOption(const Ident: UTF8String): boolean;
begin
  result :=
    Assigned(Options) and
    Options.HasOption(Ident);
end;

{ TCustomVariableCommand }

function TCustomVariableCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  // TODO : make a list of common commands that all support v,l,vl,lv
  if StatementType in [stMeans, stFreq, stBrowse] then
    begin
      AddValueLabelOptions(Result);
      AddVariableLabelOptions(Result);
    end;
end;

function TCustomVariableCommand.GetVariableList: TVariableList;
begin
  result := FVariableList;
end;

function TCustomVariableCommand.GetAcceptedVariableCount: TBoundArray;
begin
  SetLength(Result, 1);
  Result[0] := 0;
end;

function TCustomVariableCommand.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  Result.ResultTypes           := AllResultDataTypes;
  Result.ExecutorVariableTypes := [evtField];
  Result.Flags                 := [evfInternal, evfAsObject];
end;

constructor TCustomVariableCommand.Create(AVariableList: TVariableList;
  AOptionList: TOptionList; ST: TASTStatementType);
begin
  inherited Create(AOptionList, ST);

  FVariableList := AVariableList;
end;

class function TCustomVariableCommand.CreateCustomVariableCommand(
  AVariableList: TVariableList; AOptionList: TOptionList; ST: TASTStatementType
  ): TCustomVariableCommand;

  procedure DoError();
  begin
    raise Exception.CreateFmt(
      'CreateCustomVariableCommand: statement not found "%s"',
      [GetEnumName(TypeInfo(TASTStatementType), Integer(ST))]);
  end;

begin
  case ST of
    stMeans:     Result := TMeansCommand.Create(AVariableList, AOptionList);
    stBrowse:    Result := TBrowseCommand.Create(AVariableList, AOptionList);
    stFreq:      Result := TFreqCommand.Create(AVariableList, AOptionList);
    stSort:      Result := TSortCommand.Create(AVariableList, AOptionList);
    stAppend:    Result := TAppendCommand.Create(AVariableList, AOptionList);
    stMerge:     Result := TMergeCommand.Create(AVariableList, AOptionList);
    stReorder:   Result := TReorderCommand.Create(AVariableList, AOptionList);
    stAggregate: Result := TAggregateCommand.Create(AVariableList, AOptionList);
    // Ugly hack, but requested by JL Oct. 2019
    stTables:    if AVariableList.Count = 1 then
                   Result := TFreqCommand.Create(AVariableList, AOptionList)
                 else
                   Result := TTablesCommand.Create(AVariableList, AOptionList);
    stCTable:    Result := TCTableCommand.Create(AVariableList, AOptionList);
    stDescribe:  Result := TDescribeCommand.Create(AVariablelist, AOptionList);
  else
    DoError();
  end;
end;

{ TCustomEmptyCommand }

function TCustomEmptyCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;
end;

function TCustomEmptyCommand.GetRequireOpenProject: Boolean;
begin
  Result := inherited GetRequireOpenProject;

  if (StatementType in [stReset, stCls, stClh, stClose, stQuit, stVersion]) then
    result := false;
end;

class function TCustomEmptyCommand.CreateCustomEmptyCommand(
  AOptions: TOptionList; ST: TASTStatementType): TCustomEmptyCommand;
var
  C: TCustomEmptyCommandClass;
begin
  case ST of
    stClh,
    stCls,
    stCount,
    stClose,
    stQuit,
    stReset,
    stVersion:
      C := TCustomEmptyCommand;
  end;

  result := C.Create(AOptions, ST);
end;

destructor TCustomEmptyCommand.Destroy;
begin
  inherited Destroy;
end;

{ TCustomStatement }

constructor TCustomStatement.Create(StatementType: TASTStatementType);
begin
  FStatementType := StatementType;
end;

function TCustomStatement.DoOptionsCheck(OptionsChecker: IOptionCheck;
  TypeChecker: IEpiTypeChecker): Boolean;
var
  i: Integer;
  Opt: TOption;
  AcceptedOptions: TStatementOptionsMap;
  ProvidedOptions, NewOptionList: TOptionList;
  Prts: TASTResultTypes;
  EV: TCustomExecutorVariable;
  OptParser: TOptionListParser;
  OptStack: TObjectStack;
begin
  Result := false;

  AcceptedOptions := OptionsChecker.GetAcceptedOptions;
  ProvidedOptions := OptionsChecker.GetOptionList;

  // If no ProvidedOptions provided by the statement, then no need to do any further checking!
  if (not Assigned(ProvidedOptions)) or
     (ProvidedOptions.Count = 0)
  then
    begin
      Result := true;
      Exit;
    end;

  try
    // If the statement does not accept ProvidedOptions at all, then fail and do no further checking
    if (AcceptedOptions.Size = 0) and
       (ProvidedOptions.Count > 0)
    then
      begin
        DoTypeCheckError('Options not allowed!', TypeChecker);
        Exit;
      end;

    // Time to expand the ProvidedOptions in case variables with ProvidedOptions have been used.
    // - first put all options into a stack and use that as a platform to do the checking
    OptStack := TObjectStack.Create;
    for i := ProvidedOptions.Count - 1 downto 0 do
      begin
        OptStack.Push(ProvidedOptions.Options[i]);
        ProvidedOptions.Remove(TOption(OptStack.Peek));
      end;


    // Now items are in reverse order - hence the top item is also the first item in the
    // original options list.
    while OptStack.AtLeast(1) do
    begin
      Opt := TOption(OptStack.Pop);

      // If this is a legit option name, then do nothing but check that it cannot be
      // and indexed variable..
      if AcceptedOptions.HasOption(Opt.Variable.Ident) then
        begin
          if (Opt.Variable is TIndexVariable) then
            begin
              DoTypeCheckError('Option "%s" cannot have an index!', [Opt.Variable.Ident], TypeChecker);
              Exit;
            end;

          ProvidedOptions.Add(Opt);
          Continue;
        end;

      // Run the typecheck here because a TIndexVariable may not have activated
      TypeChecker.SetTypeCheckErrorOutput(false);
      Opt.Variable.TypeCheck(TypeChecker, TypesAndFlags([rtString], ExecutorVariableTypesData, [evfInternal, evfAsValue]));
      TypeChecker.SetTypeCheckErrorOutput(true);

      EV  := TypeChecker.GetExecVariable(Opt.Variable.Ident);
      if (not Assigned(EV)) then
        begin
          DoTypeCheckError('Unknown option/variable: ' + Opt.Ident, TypeChecker);
          Exit;
        end;

      if (not Opt.Variable.TypeCheck(TypeChecker, TypesAndFlags([rtString], ExecutorVariableTypesData, [evfInternal, evfAsValue]))) then
        Exit;

      OptParser := TOptionListParser.Create(TypeChecker);
      if (not OptParser.ParseText(Opt.Variable.AsString, NewOptionList)) then
        begin
          DoTypeCheckError('Cannot parse "%s" as options!', [Opt.Variable.AsString], TypeChecker);
          Exit;
        end;

      // Empty the newly parsed list and add it to the stack so it also can be checked
      for i := NewOptionList.Count - 1 downto 0 do
        begin
          OptStack.Push(NewOptionList.Options[i]);
          NewOptionList.Remove(TOption(OptStack.Peek));
        end;
      NewOptionList.Free;
    end;

    // Do a typechecking for the options
    for i := 0 to ProvidedOptions.Count - 1 do
      begin
        Opt := ProvidedOptions.Options[i];

        // This should never fail as all options are checked for existance in the above, even referred ones
        if not (AcceptedOptions.HasOption(Opt.Ident))
        then
          begin
            DoTypeCheckError('Unknown option: ' + Opt.Ident, TypeChecker);
            Exit;
          end;

        Prts := AcceptedOptions.GetResultTypes(Opt.Ident);

        if (not Assigned(Opt.Expr)) and
           (not (rtUndefined in Prts))
        then
          begin
            DoTypeCheckError('Option "' + Opt.Ident + '" most have a value!', TypeChecker);
            Exit;
          end;

        if (Assigned(Opt.Expr)) then
        begin
          if (Prts = [rtUndefined]) then
            begin
              DoTypeCheckError('Option "' + Opt.Ident + '" does not accept an expression', TypeChecker);
              Exit;
            end;

          if (not Opt.Expr.TypeCheck(TypeChecker, AcceptedOptions.GetTypesAndFlags(Opt.Ident)^)) then
            Exit;


          if (not (Opt.Expr is TCustomVariable)) and
             (not (Opt.Expr.ResultType in Prts))
          then
            begin
              DoTypeCheckError('Option "' + Opt.Ident + '" is not the right type!', TypeChecker);
              result := false;
              Exit;
            end;
        end;
      end;
  finally
    AcceptedOptions.Free;
  end;

  Result := true;
end;

function TCustomStatement.DoVarialeCheck(VariableChecker: IVariableCheck;
  TypeChecker: IEpiTypeChecker): Boolean;
var
  Vars: TVariableList;
  AcceptCounts: TBoundArray;
  ActualCount, i: Integer;
  S: UTF8String;
  V: TCustomVariable;
  ReturnTypes: TASTResultTypes;
  RType: TASTResultType;
  TmpRes: Boolean;
  TypeAndFlags: TTypesAndFlagsRec;
begin
  Result := false;

  Vars := VariableChecker.GetVariableList;

  if (not Vars.ExpandVariableList(VariableChecker)) then
    Exit;

  if (not Vars.TypeCheck(VariableChecker, TypeChecker)) then
    Exit;

  AcceptCounts := VariableChecker.GetAcceptedVariableCount;
  ActualCount  := Vars.Count;

  S := '';
  TmpRes := false;
  for i := Low(AcceptCounts) to High(AcceptCounts) do
    begin
      // 0 = infinite number of variables are allowed.
      if AcceptCounts[i] = 0 then
        begin
          TmpRes := true;
          break;
        end;

      // Now check for "at least" variable numbers
      if (AcceptCounts[i] < 0) and
         (ActualCount >= -(AcceptCounts[i]))
      then
        begin
          TmpRes := true;
          break;
        end;

      // Otherwise the numbers have to be an exact match.
      if AcceptCounts[i] = ActualCount then
        begin
          TmpRes := true;
          break;
        end;

      if AcceptCounts[i] < 0 then
        S := S + IntToStr(-AcceptCounts[i]) + ' or more,'
      else
        S := S + IntToStr(AcceptCounts[i]) + ',';
    end;

  if (Length(AcceptCounts) = 0) then
    if (ActualCount = 0) then
      begin
        TmpRes := true
      end
  else
    S := '0,';

  Delete(S, Length(S), 1);

  if (not TmpRes) then
    begin
      TypeChecker.TypeCheckError(
        'Incorrect number of variables given. Accepts ' + S + ' but was given ' + IntToStr(ActualCount),
        0, 0, 0
      );
      Exit;
    end;

  for i := 0 to Vars.Count - 1 do
    begin
      V := Vars[i];
      TypeAndFlags := VariableChecker.GetAcceptedVariableTypesAndFlags(i);

    {  if (not V.TypeCheck(TypeChecker, TypeAndFlags)) then
        Exit;   }

      if (TypeAndFlags.ExecutorVariableTypes * ExecutorVariableTypesData = []) then Continue;
      if (not Assigned(TypeChecker.GetExecVariable(V.Ident))) and
         (evfExternal in TypeAndFlags.Flags)
      then
        Continue;

      ReturnTypes := TypeAndFlags.ResultTypes;

      if not (V.ResultType in ReturnTypes) then
        begin
          for RType in ReturnTypes do
            begin
              if RType = rtAny then
                S += 'missing,'
              else
                S += ASTResultTypeString[RType] + ',';
            end;
          Delete(S, Length(S), 1);

          TypeChecker.TypeCheckError(
            V.Ident + ' is of type: ' + ASTResultTypeString[V.ResultType] + LineEnding +
            'But command expected: ' + S,
            0,0,0
          );

          Exit;
        end;
    end;

  Result := true;
end;

function TCustomStatement.GetRequireOpenProject: Boolean;
begin
  result := true;
end;

function TCustomStatement.GetExecFlags: TCustomStatementExecutionFlags;
begin
  result := [];
end;

function TCustomStatement.GetExecFlagsErrorMsg(
  Flag: TCustomStatementExecutionFlag): UTF8String;
begin
  result := '';
end;

function TCustomStatement.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  IOC: IOptionCheck;
  IVC: IVariableCheck;
begin
  Result := inherited TypeCheck(Parser);

  if Supports(Self, IOptionCheck, IOC) then
    result := result and DoOptionsCheck(IOC, Parser);

  if Supports(Self, IVariableCheck, IVC) then
    result := result and DoVarialeCheck(IVC, Parser);
end;

{ TCustomStringCommand }

function TCustomStringCommand.GetRequireOpenProject: Boolean;
begin
  Result := inherited GetRequireOpenProject;

  if StatementType in [stRead, stClose, stRuntest, stCD, stLS, stTerm, stRun] then
    result := false;
end;

function TCustomStringCommand.GetAcceptedOptions: TStatementOptionsMap;
begin
  Result := inherited GetAcceptedOptions;

  case FStatementType of
    stRuntest:
      begin
        result.Insert('halt',   [rtUndefined]);
      end;
  end;
end;

constructor TCustomStringCommand.Create(const AStringExpr: TExpr;
  AOptions: TOptionList; ACommand: UTF8String);
begin
  inherited Create(AOptions, ACommand);
  FStringExpr := AStringExpr;
end;

function TCustomStringCommand.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);
end;

destructor TCustomStringCommand.Destroy;
begin
  inherited Destroy;
end;

{ TOptionList }

function TOptionList.GetCount: Integer;
begin
  result := FOptList.Count;
end;

function TOptionList.GetOption(const Ident: UTF8String): TOption;
var
  Opt: TOption;
begin
  for Opt in FOptList do
    if UTF8LowerString(Opt.Ident) = UTF8LowerString(Ident) then
      Exit(Opt);

  result := nil;
end;

function TOptionList.GetOptions(const Index: Integer): TOption;
begin
  result := FOptList.Items[Index];
end;

function CompareKeys(const Key1, Key2: UTF8String): Integer;
begin
  result := UTF8CompareText(Key1, Key2);
end;

constructor TOptionList.Create;
begin
  inherited Create;
  FOptList := TOptList.Create;
{  FOptList.Duplicates := dupAccept;
  FOptList.Sorted := true;
  FOptList.OnKeyCompare := @CompareKeys;}
end;

destructor TOptionList.Destroy;
begin
  FOptList.Free;
  inherited Destroy;
end;

procedure TOptionList.Add(Option: TOption);
begin
  FOptList.Add(Option);
end;

procedure TOptionList.Remove(Option: TOption);
begin
  FOptList.Remove(Option);
end;

procedure TOptionList.EvaluateOptions;
var
  Opt: TOption;
begin
  for Opt in Self do
    if (Assigned(Opt.Expr)) then
      Opt.Expr.Evaluate;
end;

function TOptionList.HasOption(const Ident: UTF8String; out AOption: TOption
  ): boolean;
var
  Idx: Integer;
begin
  AOption := Option[Ident];
  Result := Assigned(AOption);
end;

function TOptionList.HasOption(const Ident: UTF8String): boolean;
var
  Dummy: TOption;
begin
  result := HasOption(Ident, Dummy);
end;

function TOptionList.GetEnumerator: TOptionListEnumerator;
begin
  result := TOptionListEnumerator.Create(Self);
end;

{ TOption }

function TOption.GetIdent: UTF8String;
begin
//  result := FVariable.Ident;
  result := UTF8LowerString(FVariable.Ident);
end;

constructor TOption.Create(AVariable: TCustomVariable; AExpr: TExpr);
begin
  inherited Create;

  FVariable := AVariable;
  FExpr     := AExpr;

  ObserveObject(Expr);
end;

destructor TOption.Destroy;
begin
  inherited Destroy;
end;

function TOption.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if Assigned(Expr) then
    result := Expr.TypeCheck(Parser) and
              result;
end;

{ TFor }

function TFor.GetRequireOpenProject: Boolean;
begin
  Result := false;
end;

procedure TFor.DoObservedChange(Sender: TObject);
begin
  if Sender = FStartExpr then
    FStartExpr := nil;

  if Sender = FVariable then
    FVariable := nil;

  if Sender = FEndExpr then
    FEndExpr := nil;
end;

function TFor.GetVariableList: TVariableList;
begin
  result := TVariableList.Create;
  result.Add(FVariable);
end;

function TFor.GetAcceptedVariableCount: TBoundArray;
begin
  SetLength(Result, 1);
  result[0] := 1;
end;

function TFor.GetAcceptedVariableTypesAndFlags(Index: Integer
  ): TTypesAndFlagsRec;
begin
  result.ExecutorVariableTypes := [evtGlobal];

  if (ForType = ftArray) then
    result.ResultTypes := AllResultDataTypes
  else
    result.ResultTypes := [rtInteger];

  result.Flags := [evfInternal, evfAsValue];
end;

constructor TFor.Create(const Variable: TCustomVariable; const StartExpr,
  EndExpr: TExpr; ADirection: UTF8String; Statement: TCustomStatement);
begin
  inherited Create(stFor);

  FForType := ftRange;
  FVariable := Variable;
  FStartExpr := StartExpr;
  FEndExpr := EndExpr;

  ObserveObject(Variable);
  ObserveObject(StartExpr);
  ObserveObject(EndExpr);

  case UTF8LowerCase(ADirection) of
    'to':     FDirection := fdTo;
    'downto': FDirection := fdDownTo;
  end;

  FStatement := Statement;
end;

constructor TFor.Create(const AVariable: TCustomVariable;
  const AArrayVal: TArray; AStatement: TCustomStatement);
begin
  inherited Create(stFor);

  FForType := ftArray;
  FVariable := AVariable;
  FArrayVal := AArrayVal;
  FStatement := AStatement;
end;

destructor TFor.Destroy;
begin
  FVariable.Free;
  FStartExpr.Free;
  FEndExpr.Free;
  FStatement.Free;

  inherited Destroy;
end;

function TFor.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  VFlags, EFlags: TTypesAndFlagsRec;
begin
  Result := inherited TypeCheck(Parser) {and
            Statement.TypeCheck(Parser)};

  if (not result) then
    Exit;

  // Types and flags are for any variables that may be part of the
  // expressions... be it Start, End or the Array
  EFlags := TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesAll);

  if (ForType = ftRange) then
    result := StartExpr.TypeCheck(Parser, EFlags) and
              EndExpr.TypeCheck(Parser, EFlags)
  else
    result := ArrayVal.TypeCheck(Parser, EFlags);


  if (not Result) then
    Exit;

  case ForType of
    ftRange:
      begin
        if not (FStartExpr.ResultType = rtInteger)
        then
          begin
            DoTypeCheckError(
              rsExpressionReturnType1,
              [ASTResultTypeString[rtInteger]],
              Parser
            );
            Result := false;
          end;

        if not (FEndExpr.ResultType = rtInteger)
        then
          begin
            DoTypeCheckError(
              rsExpressionReturnType1,
              [ASTResultTypeString[rtInteger]],
              Parser
            );
            Result := false;
          end;
      end;

    ftArray:
      begin
        if not (FVariable.ResultType = FArrayVal.ResultSubType) then
        begin
          DoTypeCheckError(
            'Array value(s) and variable datatype do not match!' + LineEnding +
              'Array datatype: %s' + LineEnding +
              'Variable datatype: %s',
            [ASTResultTypeString[FArrayVal.ResultSubType],
             ASTResultTypeString[FVariable.ResultType]],
            Parser
          );
          Result := false;
        end;
      end;
  end;
end;

{ TSelect }

procedure TSelect.DoObservedChange(Sender: TObject);
begin
  if Sender = FExpr then
    FExpr := nil;
end;

constructor TSelect.Create(const Expr: TExpr; const Statement: TCustomStatement
  );
begin
  inherited Create(stSelect);
  FExpr := Expr;
  ObserveObject(FExpr);
  FStatement := Statement;
end;

destructor TSelect.Destroy;
begin
  if Assigned(FExpr) then
    FExpr.Destroy;

  if Assigned(FStatement) then
    FStatement.Free;

  inherited Destroy;
end;

function TSelect.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  Result := inherited TypeCheck(Parser) and
            Expr.TypeCheck(Parser, TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData, [evfInternal, evfAsObject, evfAsValue]));

  if result and
     (not (Expr.ResultType = rtBoolean))
  then
    DoTypeCheckError(
      rsExpressionReturnType1,
      [ASTResultTypeString[rtBoolean]],
      Parser
    );

  Result := Result and Statement.TypeCheck(Parser);
end;

{ TParamList }

function TParamList.GetParam(const Index: Integer): TExpr;
begin
  result := FExprList.Items[Index];
end;

function TParamList.GetCount: Integer;
begin
  result := FExprList.Count;
end;

procedure TParamList.DoObservedChange(Sender: TObject);
begin
//  FExprList.Remove(Sender);
end;

constructor TParamList.Create;
begin
  FExprList := TExprList.Create;
end;

destructor TParamList.Destroy;
var
  Item: TExpr;
begin
  for Item in FExprList do
    Item.Free;

  FExprList.Free;
  inherited Destroy;
end;

function TParamList.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  E: TExpr;
begin
  Result := inherited TypeCheck(Parser);

  for E in FExprList do
    result := result and E.TypeCheck(Parser);
end;

procedure TParamList.Add(Param: TExpr);
begin
  FExprList.Add(Param);
end;

{ TMissingLiteral }

function TMissingLiteral.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;
  FEvalValue.Missing := true;
end;

constructor TMissingLiteral.Create;
begin
  inherited Create(otMissingLiteral, nil, nil);
end;

function TMissingLiteral.ResultType: TASTResultType;
begin
  Result := rtAny;
end;

{ TRecNumberLiteral }

constructor TRecNumberLiteral.Create(Executor: IEpiScriptExecutor);
begin
  inherited Create(otRecNumberLiteral, nil, nil);
  FExecutor := Executor;
end;

function TRecNumberLiteral.ResultType: TASTResultType;
begin
  Result := rtInteger;
end;

function TRecNumberLiteral.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;
  FEvalValue.IntVal := FExecutor.GetCurrentRecordNo + 1;
end;

{ TAbstractSyntaxTreeBase }

procedure TAbstractSyntaxTreeBase.DoTypeCheckError(const Msg: String;
  Parser: IEpiTypeChecker);
begin
  Parser.TypeCheckError(
    Msg,
    LineNo,
    ColNo,
    ByteNo
  );
end;

procedure TAbstractSyntaxTreeBase.DoTypeCheckError(const Msg: String;
  const Args: array of const; Parser: IEpiTypeChecker);
begin
  DoTypeCheckError(
    Format(Msg, Args),
    Parser
  );
end;

procedure TAbstractSyntaxTreeBase.ObserveObject(O: TObject);
var
  Obs: IFPObserved;
begin
  if not Assigned(O) then exit;

  if O.GetInterface(SGUIDObserved, Obs) then
  begin
    Obs.FPOAttachObserver(Self);

    if not Assigned(FObservedList) then
      FObservedList := TFPList.Create;

    FObservedList.Add(O);
  end;
end;

procedure TAbstractSyntaxTreeBase.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
var
  Obs: IFPObserved;
begin
  DoObservedChange(ASender);
  if Asender.GetInterface(SGUIDObserved, Obs) then
    Obs.FPODetachObserver(Self);
  FObservedList.Remove(ASender);
end;

destructor TAbstractSyntaxTreeBase.Destroy;
var
  Obs: IFPObserved;
  i: Integer;
begin
  FLine := '';

  if Assigned(FObservedList) then
  begin
    for i := 0 to FObservedList.Count - 1 do
      if TObject(FObservedList[i]).GetInterface(SGUIDObserved, Obs) then
        Obs.FPODetachObserver(Self);
    FreeAndNil(FObservedList);
  end;

  inherited Destroy;
end;

function TAbstractSyntaxTreeBase.TypeCheck(Parser: IEpiTypeChecker): boolean;
begin
  result := true;
end;

procedure TAbstractSyntaxTreeBase.AssignToken(T: TToken);
begin
  FLineNo := T.LineNum;
  FColNo  := T.CaretNum;
  FByteNo := T.BytePos;
end;

procedure TAbstractSyntaxTreeBase.AddCustomData(const Key: UTF8String;
  Data: TObject);
begin
  if not Assigned(FCustomData) then
    FCustomData := TFPObjectHashTable.Create(false);

  try
    FCustomData.Items[Key] := Data;
  except
    raise Exception.Create('TAbstractSyntaxTreeBase: Duplicate CustomData - key=' + Key);
  end;
end;

function TAbstractSyntaxTreeBase.FindCustomData(const Key: UTF8String): TObject;
begin
  result := nil;
  if Assigned(FCustomData) then
    result := FCustomData.Items[Key];
end;

function TAbstractSyntaxTreeBase.RemoveCustomData(const Key: UTF8String
  ): TObject;
begin
  Result := FindCustomData(Key);
  if not Assigned(Result) then exit;

  FCustomData.Delete(Key);

  if FCustomData.Count = 0 then
    FreeAndNil(FCustomData);
end;

{ TVariable }

function TVariable.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
var
  EV: TCustomExecutorVariable;
begin
  Result := inherited TypeCheck(TypeChecker, TypesAndFlags);

  if (not Result) then Exit;

  EV := TypeChecker.GetExecVariable(Ident);

  if (not Assigned(EV)) and
     (evfExternal in TypesAndFlags.Flags)
  then
    Exit;

  case EV.VarType of
    evtGlobal,
    evtResultConst:
        // A global or result can be accepted as either Value or Object.
        Result := true;

    evtDataset,
    evtValuelabel,
    evtGlobalVector,
    evtField,
    evtResultVector,
    evtResultMatrix:
      begin
        // Since the AST found this to be a non-indexed variable, it may only
        // be accepted as an object!
        Result := (evfAsObject in TypesAndFlags.Flags);

        if (not Result) then
 { TODO -oJamie : Why not just recast this as having the default index? }
        begin
          DoTypeCheckError('Identifier "' + Ident + '" must have an index', TypeChecker);
          Exit;
        end;

      end
  else
    begin
      DoTypeCheckError('TVariable.TypeCheck: Unknown ExecVariableType!', TypeChecker);
      Exit;
    end;
  end;
end;

{ TRelationExpr }

function TRelationalExpr.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
const
  RelationalOperationCheck: array[TASTResultType, TASTResultType] of Boolean =
           //    rtAny,     rtBoolean, rtInteger, rtDate, rtFloat, rtTime, rtString, rtArray, rtObject, rtUndefined
               (
 {rtAny}        ( true,     true,      true,      false,  true,    true,   true,     true,     true,     false),
 {rtBoolean}    ( true,     true,      false,     false,  false,   false,  false,    false,    false,    false),
 {rtInteger}    ( true,     false,     true,      true,   true,    true,   false,    false,    false,    false),
 {rtDate}       ( true,     false,     true,      true,   true,    true,   false,    false,    false,    false),
 {rtFloat}      ( true,     false,     true,      true,   true,    true,   false,    false,    false,    false),
 {rtTime}       ( true,     false,     true,      true,   true,    true,   false,    false,    false,    false),
 {rtString}     ( true,     false,     false,     false,  false,   false,  true,     false,    false,    false),
 {rtArray}      ( true,     false,     false,     false,  false,   false,  false,    true ,    false,    false),
 {rtObject}     ( true,     false,     false,     false,  false,   false,  false,    false,    true ,    false),
 {rtUndefined}  ( false,    false,     false,     false,  false,   false,  false,    false,    false,    false)
               );
begin
  result := inherited TypeCheck(TypeChecker, TypesAndFlags);

  if result then
  begin
    Result := RelationalOperationCheck[FL.ResultType, FR.ResultType];

    if not result then
    begin
      DoTypeCheckError(
        'Left and Right cannot be compared. Incompatible types.',
        TypeChecker
      );
    end;
  end;
end;

function TRelationalExpr.ResultType: TASTResultType;
begin
  Result := rtBoolean;
end;

function TRelationalExpr.DoEvaluate: boolean;
var
  CType: TASTResultType;
  Res: PtrInt;
begin
  Result := inherited DoEvaluate;

  if (not Result) then
    Exit;

  FEvalValue.Missing := false;

  if Left.IsMissing or Right.IsMissing then
  begin
    case Operation of
      otEQ:  FEvalValue.BoolVal := not (Left.IsMissing xor Right.IsMissing);
      otNEQ: FEvalValue.BoolVal := Left.IsMissing xor Right.IsMissing;
      otLT:  FEvalValue.BoolVal := false;
      otLTE: FEvalValue.BoolVal := false;
      otGT:  FEvalValue.BoolVal := false;
      otGTE: FEvalValue.BoolVal := false;
    end;

    Exit;
  end;

  CType := CommonType(Left, Right);
  case CType of
    rtBoolean:
      case Operation of
        otEQ:  FEvalValue.BoolVal := Left.AsBoolean =  Right.AsBoolean;
        otNEQ: FEvalValue.BoolVal := Left.AsBoolean <> Right.AsBoolean;
        otLT:  FEvalValue.BoolVal := Left.AsBoolean <  Right.AsBoolean;
        otLTE: FEvalValue.BoolVal := Left.AsBoolean <= Right.AsBoolean;
        otGT:  FEvalValue.BoolVal := Left.AsBoolean >  Right.AsBoolean;
        otGTE: FEvalValue.BoolVal := Left.AsBoolean >= Right.AsBoolean;
      end;

    rtDate:
      case Operation of
        otEQ:  FEvalValue.BoolVal := Left.AsDate =  Right.AsDate;
        otNEQ: FEvalValue.BoolVal := Left.AsDate <> Right.AsDate;
        otLT:  FEvalValue.BoolVal := Left.AsDate <  Right.AsDate;
        otLTE: FEvalValue.BoolVal := Left.AsDate <= Right.AsDate;
        otGT:  FEvalValue.BoolVal := Left.AsDate >  Right.AsDate;
        otGTE: FEvalValue.BoolVal := Left.AsDate >= Right.AsDate;
      end;

    rtInteger:
      case Operation of
        otEQ:  FEvalValue.BoolVal := Left.AsInteger =  Right.AsInteger;
        otNEQ: FEvalValue.BoolVal := Left.AsInteger <> Right.AsInteger;
        otLT:  FEvalValue.BoolVal := Left.AsInteger <  Right.AsInteger;
        otLTE: FEvalValue.BoolVal := Left.AsInteger <= Right.AsInteger;
        otGT:  FEvalValue.BoolVal := Left.AsInteger >  Right.AsInteger;
        otGTE: FEvalValue.BoolVal := Left.AsInteger >= Right.AsInteger;
      end;

    rtTime:
      case Operation of
        otEQ:  FEvalValue.BoolVal := Left.AsTime =  Right.AsTime;
        otNEQ: FEvalValue.BoolVal := Left.AsTime <> Right.AsTime;
        otLT:  FEvalValue.BoolVal := Left.AsTime <  Right.AsTime;
        otLTE: FEvalValue.BoolVal := Left.AsTime <= Right.AsTime;
        otGT:  FEvalValue.BoolVal := Left.AsTime >  Right.AsTime;
        otGTE: FEvalValue.BoolVal := Left.AsTime >= Right.AsTime;
      end;


    rtFloat:
      case Operation of
        otEQ:  FEvalValue.BoolVal :=                                    SameValue(Left.AsFloat, Right.AsFloat, 0.0);
        otNEQ: FEvalValue.BoolVal :=                                not SameValue(Left.AsFloat, Right.AsFloat, 0.0);
        otLT:  FEvalValue.BoolVal :=  Left.AsFloat < Right.AsFloat;
        otLTE: FEvalValue.BoolVal := (Left.AsFloat < Right.AsFloat) or (SameValue(Left.AsFloat, Right.AsFloat, 0.0));
        otGT:  FEvalValue.BoolVal :=  Left.AsFloat > Right.AsFloat;
        otGTE: FEvalValue.BoolVal := (Left.AsFloat > Right.AsFloat) or (SameValue(Left.AsFloat, Right.AsFloat, 0.0));
      end;

    rtString:
      begin
        Res := UTF8CompareStr(Left.AsString, Right.AsString);
        case Operation of
          otEQ:  FEvalValue.BoolVal := Res = 0;
          otNEQ: FEvalValue.BoolVal := Res <> 0;
          otLT:  FEvalValue.BoolVal := Res < 0;
          otLTE: FEvalValue.BoolVal := Res <= 0;
          otGT:  FEvalValue.BoolVal := Res > 0;
          otGTE: FEvalValue.BoolVal := Res >= 0;
        end;
      end;
  end;
end;
{
function TRelationalExpr.AsBoolean: Boolean;
var
  CType: TASTResultType;
  Res: PtrInt;
  BoolResult: Boolean;
begin
  Result := inherited AsBoolean;

  if Left.IsMissing or Right.IsMissing then
  begin
    case Operation of
      otEQ:  Result := not (Left.IsMissing xor Right.IsMissing);
      otNEQ: Result := Left.IsMissing xor Right.IsMissing;
      otLT:  Result := false;
      otLTE: Result := false;
      otGT:  Result := false;
      otGTE: Result := false;
    end;

    Exit;
  end;

  CType := CommonType(Left, Right);
  case CType of
    rtBoolean:
      case Operation of
        otEQ:  Result := Left.AsBoolean =  Right.AsBoolean;
        otNEQ: Result := Left.AsBoolean <> Right.AsBoolean;
        otLT:  Result := Left.AsBoolean <  Right.AsBoolean;
        otLTE: Result := Left.AsBoolean <= Right.AsBoolean;
        otGT:  Result := Left.AsBoolean >  Right.AsBoolean;
        otGTE: Result := Left.AsBoolean >= Right.AsBoolean;
      end;

    rtDate:
      case Operation of
        otEQ:  Result := Left.AsDate =  Right.AsDate;
        otNEQ: Result := Left.AsDate <> Right.AsDate;
        otLT:  Result := Left.AsDate <  Right.AsDate;
        otLTE: Result := Left.AsDate <= Right.AsDate;
        otGT:  Result := Left.AsDate >  Right.AsDate;
        otGTE: Result := Left.AsDate >= Right.AsDate;
      end;

    rtInteger:
      case Operation of
        otEQ:  Result := Left.AsInteger =  Right.AsInteger;
        otNEQ: Result := Left.AsInteger <> Right.AsInteger;
        otLT:  Result := Left.AsInteger <  Right.AsInteger;
        otLTE: Result := Left.AsInteger <= Right.AsInteger;
        otGT:  Result := Left.AsInteger >  Right.AsInteger;
        otGTE: Result := Left.AsInteger >= Right.AsInteger;
      end;

    rtTime:
      case Operation of
        otEQ:  Result := Left.AsTime =  Right.AsTime;
        otNEQ: Result := Left.AsTime <> Right.AsTime;
        otLT:  Result := Left.AsTime <  Right.AsTime;
        otLTE: Result := Left.AsTime <= Right.AsTime;
        otGT:  Result := Left.AsTime >  Right.AsTime;
        otGTE: Result := Left.AsTime >= Right.AsTime;
      end;


    rtFloat:
      case Operation of
        otEQ:  Result :=                                    SameValue(Left.AsFloat, Right.AsFloat, 0.0);
        otNEQ: Result :=                                not SameValue(Left.AsFloat, Right.AsFloat, 0.0);
        otLT:  Result :=  Left.AsFloat < Right.AsFloat;
        otLTE: Result := (Left.AsFloat < Right.AsFloat) or (SameValue(Left.AsFloat, Right.AsFloat, 0.0));
        otGT:  Result :=  Left.AsFloat > Right.AsFloat;
        otGTE: Result := (Left.AsFloat > Right.AsFloat) or (SameValue(Left.AsFloat, Right.AsFloat, 0.0));
      end;

    rtString:
      begin
        Res := UTF8CompareStr(Left.AsString, Right.AsString);
        case Operation of
          otEQ:  Result := Res = 0;
          otNEQ: Result := Res <> 0;
          otLT:  Result := Res < 0;
          otLTE: Result := Res <= 0;
          otGT:  Result := Res > 0;
          otGTE: Result := Res >= 0;
        end;
      end;

{    rtObject:
    case Operation of
      otEQ: ;
      otNEQ: ;
      otLT: ;
      otLTE: ;
      otGT: ;
      otGTE: ;
    end;}

//    rtUndefined: ;
  end;
end;

function TRelationalExpr.AsInteger: ASTInteger;
begin
  Result := ASTInteger(AsBoolean);
end;

function TRelationalExpr.AsFloat: ASTFloat;
begin
  Result := AsInteger;
end;

function TRelationalExpr.AsDate: EpiDate;
begin
  Result := AsInteger;
end;

function TRelationalExpr.AsTime: EpiDateTime;
begin
  Result := AsFloat;
end;

function TRelationalExpr.AsString: EpiString;
begin
  inherited AsString;
  if IsMissing then
    Result := TEpiStringField.DefaultMissing
  else
    Result := BoolToStr(AsBoolean, true);
end;                   }

{ TBinaryExpr }

function TBinaryExpr.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
var
  Lr: TASTResultType;
  Rr: TASTResultType;
  BinaryTypesAndFlags: TTypesAndFlagsRec;
begin
  BinaryTypesAndFlags := options_hashmap.TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData);
  Result := inherited TypeCheck(TypeChecker, BinaryTypesAndFlags);

  if result then
  begin
    Lr := FL.ResultType;
    Rr := FR.ResultType;

    case FOp of
      otAnd,
      otOr:
        begin
          result := (Lr in [rtBoolean, rtInteger]) and
                    (Rr in [rtBoolean, rtInteger]);

          if not (Lr in [rtBoolean, rtInteger]) then
            DoTypeCheckError(
              'Left ' + rsExpressionReturnType2,
              [ASTResultTypeString[rtBoolean],
               ASTResultTypeString[rtInteger]],
              TypeChecker
            );

          if not (Rr in [rtBoolean, rtInteger]) then
            DoTypeCheckError(
              'Right ' + rsExpressionReturnType2,
              [ASTResultTypeString[rtBoolean],
               ASTResultTypeString[rtInteger]],
              TypeChecker
            );
        end;

      otMod,
      otDiv:
        begin
          result := (Lr = rtInteger) and
                    (Rr = rtInteger);

          if not (Lr = rtInteger) then
            DoTypeCheckError(
              'Left ' + rsExpressionReturnType1,
              [ASTResultTypeString[rtInteger]],
              TypeChecker
            );
          if not (Rr = rtInteger) then
            DoTypeCheckError(
              'Right ' + rsExpressionReturnType1,
              [ASTResultTypeString[rtInteger]],
              TypeChecker
            );
        end;

      otMult,
      otDivide:
        begin
          result := (Lr in [rtInteger, rtFloat]) and
                    (Rr in [rtInteger, rtFloat]);

          if not (Lr in [rtInteger, rtFloat]) then
            DoTypeCheckError(
              'Left ' + rsExpressionReturnType2,
              [ASTResultTypeString[rtInteger], ASTResultTypeString[rtFloat]],
              TypeChecker
            );
          if not (Rr in [rtInteger, rtFloat]) then
            DoTypeCheckError(
              'Right ' + rsExpressionReturnType2,
              [ASTResultTypeString[rtInteger], ASTResultTypeString[rtFloat]],
              TypeChecker
            );
        end;

      otMinus:
        begin
          result := (Lr in [rtInteger, rtDate, rtFloat, rtTime]) and
                    (Rr in [rtInteger, rtDate, rtFloat, rtTime]);

          if not (Lr in [rtInteger, rtDate, rtFloat, rtTime]) then
            DoTypeCheckError(
              'Left ' + rsExpressionReturnType4,
              [ASTResultTypeString[rtInteger], ASTResultTypeString[rtDate],
               ASTResultTypeString[rtFloat],   ASTResultTypeString[rtTime]],
              TypeChecker
            );
          if not (Rr in [rtInteger, rtDate, rtFloat, rtTime]) then
            DoTypeCheckError(
              'Right ' + rsExpressionReturnType4,
              [ASTResultTypeString[rtInteger], ASTResultTypeString[rtDate],
               ASTResultTypeString[rtFloat],   ASTResultTypeString[rtTime]],
              TypeChecker
            );
        end;

      otPlus:
          begin
            result := (Lr in [rtInteger, rtDate, rtFloat, rtTime, rtString]) and
                      (Rr in [rtInteger, rtDate, rtFloat, rtTime, rtString]);

            if not (Lr in [rtInteger, rtDate, rtFloat, rtTime, rtString]) then
              DoTypeCheckError(
                'Left ' + rsExpressionReturnType5,
                [ASTResultTypeString[rtInteger], ASTResultTypeString[rtDate],
                 ASTResultTypeString[rtFloat],   ASTResultTypeString[rtTime],
                 ASTResultTypeString[rtString]],
                TypeChecker
              );

            if not (Rr in [rtInteger, rtDate, rtFloat, rtTime, rtString]) then
              DoTypeCheckError(
                'Right ' + rsExpressionReturnType5,
                [ASTResultTypeString[rtInteger], ASTResultTypeString[rtDate],
                 ASTResultTypeString[rtFloat],   ASTResultTypeString[rtTime],
                 ASTResultTypeString[rtString]],
                TypeChecker
              );
          end;
  {    otXor,
      otShl,
      otShr:         }
    end;
  end;
end;

function TBinaryExpr.ResultType: TASTResultType;
begin
  case FOp of
    otAnd,
    otXor,
    otOr:
      Result := CommonType(FL, FR);
    otShl,
    otShr,
    otMod,
    otDiv:
      Result := rtInteger;
    otMult,
    otPlus:
      result := CommonType(FL, FR);
    otMinus:
      if (FL.ResultType = rtDate) and (FR.ResultType = rtDate) then
        result := rtInteger
      else
        result := CommonType(FL, FR);
    otExponential,
    otDivide:
      result := rtFloat;
  end;
end;

function TBinaryExpr.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;

  if (Left.IsMissing or Right.IsMissing) then
    Exit;

  FEvalValue.Missing := false;

  case ResultType of
    rtBoolean:
      case Operation of
        otXor: FEvalValue.BoolVal := Left.AsBoolean xor Right.AsBoolean;
        otOr:  FEvalValue.BoolVal := Left.AsBoolean or  Right.AsBoolean;
        otAnd: FEvalValue.BoolVal := Left.AsBoolean and Right.AsBoolean;
      end;

    rtInteger:
      case Operation of
        otXor:         FEvalValue.IntVal := Left.AsInteger xor Right.AsInteger;
        otOr:          FEvalValue.IntVal := Left.AsInteger or  Right.AsInteger;
        otAnd:         FEvalValue.IntVal := Left.AsInteger and Right.AsInteger;
        otPlus:        FEvalValue.IntVal := Left.AsInteger +   Right.AsInteger;
        otMinus:       FEvalValue.IntVal := Left.AsInteger -   Right.AsInteger;
        otMult:        FEvalValue.IntVal := Left.AsInteger *   Right.AsInteger;
        otDiv:         FEvalValue.IntVal := Left.AsInteger div Right.AsInteger;
        otMod:         FEvalValue.IntVal := Left.AsInteger mod Right.AsInteger;
        otExponential: FEvalValue.IntVal := Left.AsInteger **  Right.AsInteger;
        otShl:         FEvalValue.IntVal := Left.AsInteger shl Right.AsInteger;
        otShr:         FEvalValue.IntVal := Left.AsInteger shr Right.AsInteger;
      end;

    rtDate:
      case Operation of
        otPlus:        FEvalValue.DateVal := Left.AsDate + Right.AsDate;
        otMinus:       FEvalValue.DateVal := Left.AsDate - Right.AsDate;
      end;

    rtFloat:
      case Operation of
        otPlus:        FEvalValue.FloatVal := Left.AsFloat + Right.AsFloat;
        otMinus:       FEvalValue.FloatVal := Left.AsFloat - Right.AsFloat;
        otMult:        FEvalValue.FloatVal := Left.AsFloat * Right.AsFloat;
        otDivide:      FEvalValue.FloatVal := Left.AsFloat / Right.AsFloat;
        otExponential: FEvalValue.FloatVal := Left.AsFloat ** Right.AsFloat;
      end;

    rtTime:
      case Operation of
        otPlus:        FEvalValue.TimeVal := Left.AsTime + Right.AsTime;
        otMinus:       FEvalValue.TimeVal := Left.AsTime - Right.AsTime;
      end;

    rtString:
      case Operation of
        otPlus:        FEvalValue.StringVal := Left.AsString + Right.AsString;
      end;
  end;

end;

{function TBinaryExpr.AsBoolean: Boolean;
begin
  Result := inherited AsBoolean;

  if IsMissing then
    Exit;

  case Operation of
    otXor: result := Left.AsBoolean xor Right.AsBoolean;
    otOr:  result := Left.AsBoolean or  Right.AsBoolean;
    otAnd: result := Left.AsBoolean and Right.AsBoolean;
  end;
end;

function TBinaryExpr.AsInteger: ASTInteger;
var
  Tmp: ASTInteger;
begin
  Result := inherited AsInteger;

  if IsMissing then
    Exit;

  case ResultType of
    rtBoolean:
      Result := ASTInteger(AsBoolean);

    rtDate:
      case Operation of
        otPlus:        result := Left.AsInteger +   Right.AsInteger;
        otMinus:       result := Left.AsInteger -   Right.AsInteger;
      end;

    rtInteger:
      case Operation of
        otXor:         result := Left.AsInteger xor Right.AsInteger;
        otOr:          result := Left.AsInteger or  Right.AsInteger;
        otAnd:         result := Left.AsInteger and Right.AsInteger;
        otPlus:        result := Left.AsInteger +   Right.AsInteger;
        otMinus:       result := Left.AsInteger -   Right.AsInteger;
        otMult:        result := Left.AsInteger *   Right.AsInteger;
        otDiv:         result := Left.AsInteger div Right.AsInteger;
        otMod:         result := Left.AsInteger mod Right.AsInteger;
        otExponential: result := Left.AsInteger **  Right.AsInteger;
        otShl:         result := Left.AsInteger shl Right.AsInteger;
        otShr:         result := Left.AsInteger shr Right.AsInteger;
      end;

    rtFloat,
    rtTime:
      result := trunc(AsFloat);
  end;
end;

function TBinaryExpr.AsFloat: ASTFloat;
begin
  Result := inherited AsFloat;

  if IsMissing then
    Exit;

  case ResultType of
    rtBoolean,
    rtInteger,
    rtDate:
      result := AsInteger;


    rtFloat:
      case Operation of
        otPlus:        result := Left.AsFloat + Right.AsFloat;
        otMinus:       result := Left.AsFloat - Right.AsFloat;
        otMult:        result := Left.AsFloat * Right.AsFloat;
        otDivide:      result := Left.AsFloat / Right.AsFloat;
        otExponential: result := Left.AsFloat ** Right.AsFloat;
      end;

    rtTime:
      case Operation of
        otPlus:        result := Left.AsTime + Right.AsTime;
        otMinus:       result := Left.AsTime - Right.AsTime;
      end;
  end;
end;

function TBinaryExpr.AsString: EpiString;
begin
  Result := inherited AsString;

  if IsMissing and
     (not (ResultType = rtString))
  then
    Exit;

  case ResultType of
    rtBoolean:
      result := BoolToStr(AsBoolean, true);

    rtInteger:
      result := IntToStr(AsInteger);

    rtDate:
      result := DateToStr(AsDate);

    rtFloat:
      result := FloatToStr(AsFloat);

    rtTime:
      result := FormatDateTime('HH:NN:SS', AsTime);

    rtString:
      case Operation of
        otPlus: Result := Left.AsString + Right.AsString;
      end;
  end;
end;

function TBinaryExpr.AsDate: EpiDate;
begin
  Result := inherited AsDate;

  if IsMissing then
    Exit;

  result := AsInteger;
end;

function TBinaryExpr.Astime: EpiTime;
begin
  Result := inherited Astime;

  if IsMissing then
    Exit;

  result := AsFloat;
end;

function TBinaryExpr.IsMissing: Boolean;
begin
  Result := Left.IsMissing or Right.IsMissing;
end;                            }

{ TUnaryExpr }

function TUnaryExpr.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
begin
  Result := inherited TypeCheck(TypeChecker, TypesAndFlags);

  if result then
  case FOp of
    otNot:
      begin
        result := Left.ResultType = rtBoolean;
        if not Result then
          DoTypeCheckError(
            rsExpressionReturnType1,
            [ASTResultTypeString[rtBoolean]],
            TypeChecker
          );
      end;
    otMinus:
      begin
        result := Left.ResultType in [rtFloat, rtInteger];
        if not Result then
          DoTypeCheckError(
            rsExpressionReturnType2,
            [ASTResultTypeString[rtInteger], ASTResultTypeString[rtFloat]],
            TypeChecker
          );
      end;
  end;
end;

function TUnaryExpr.ResultType: TASTResultType;
begin
  Result := Left.ResultType;
end;

function TUnaryExpr.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;

  if (Left.IsMissing) then
    Exit(true);

  FEvalValue.Missing := false;

  case ResultType of
    rtBoolean:
      case Operation of
        otNot: FEvalValue.BoolVal := (not Left.AsBoolean);
      end;

    rtInteger:
      case Operation of
        otMinus: FEvalValue.IntVal := -(Left.AsInteger);
      end;

    rtFloat:
      case Operation of
        otMinus: FEvalValue.FloatVal := -(Left.AsFloat);
      end;
  end;

  Result := true;
end;

{ TFunctionCall }

function TFunctionCall.GetParam(const Index: integer): TExpr;
begin
  result := FParamList.Param[Index];
end;

constructor TFunctionCall.Create(const ParamList: TParamList);
begin
  inherited Create(otFunction, nil, nil);
  FParamList := ParamList;
end;

destructor TFunctionCall.Destroy;
begin
  FParamList.Free;
  inherited Destroy;
end;

function TFunctionCall.ParamCounts: TBoundArray;
begin
  SetLength(Result, 1);
  Result[0] := 0;
end;

function TFunctionCall.ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec;
begin
  result.ExecutorVariableTypes := ExecutorVariableTypesData;
  result.ResultTypes           := [rtUndefined];
  result.Flags                 := [evfInternal, evfAsValue];
end;

class function TFunctionCall.CreateFunction(const FunctionName: string;
  const ParamList: TParamList; Executor: IEpiScriptExecutor): TFunctionCall;
var
  Func: String;
begin
  // This is to catch eg. user functions from dll's or other.
  result := Executor.CreateFunction(FunctionName, ParamList);
  if Assigned(result) then
    begin
      Result.FExecutor := Executor;
      Exit;
    end;

  Func := LowerCase(FunctionName);

  case Func of
    { Math }
    'abs':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncAbs, ParamList);
    'exp':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncExp, ParamList);
    'fraction':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncFraction, ParamList);
    'ln':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncLn, ParamList);
    'log':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncLog, ParamList);
    'round':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncRound, ParamList);
    'sqrt':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncSqrt, ParamList);
    'random':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncRandom, ParamList);
    'samevalue':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncSameValue, ParamList);
    'tan':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncTan, ParamList);
    'arctan':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncArcTan, ParamList);
    'sin':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncSin, ParamList);
    'arcsin':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncArcSin, ParamList);
    'cos':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncCos, ParamList);
    'arccos':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncArcCos, ParamList);
    'lre':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncLre, ParamList);
    'sum':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncSum, ParamList);


    { Date functions }
    'createdate':
      result := TEpiScriptFunction_CreateDate.Create(ParamList);
    'today':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncToday, ParamList);
    'day':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncDay, ParamList);
    'month':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncMonth, ParamList);
    'year':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncYear, ParamList);
    'dayofweek':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncDayOfWeek, ParamList);
    'week':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncWeek, ParamList);

    { Time functions }
    'createtime':
      result := TEpiScriptFunction_CreateTime.Create(ParamList);
    'now':
      result := TEpiScriptFunction_TimeFunctions.Create(otFuncNow, ParamList);
    'hour':
      result := TEpiScriptFunction_TimeFunctions.Create(otFuncHour, ParamList);
    'minute':
      result := TEpiScriptFunction_TimeFunctions.Create(otFuncMinut, ParamList);
    'second':
      result := TEpiScriptFunction_TimeFunctions.Create(otFuncSecond, ParamList);

    { String }
    'substring':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncSubString, ParamList);
    'length':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncLength, ParamList);
    'pos':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncPos, ParamList);
    'trim':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncTrim, ParamList);
    'lower':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncLower, ParamList);
    'upper':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncUpper, ParamList);
    'concat':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncConcat, ParamList);


    { System }
    'exist':
      result := TEpiScriptFunction_SystemFunctions.Create(otFuncIdentExists, ParamList);
    'idtype':
      result := TEpiScriptFunction_SystemFunctions.Create(otFuncIdentType, ParamList);
    'datatype':
      result := TEpiScriptFunction_SystemFunctions.Create(otFuncDataType, ParamList);
    'size':
      result := TEpiScriptFunction_SystemFunctions.Create(otFuncSize, ParamList);
    'iif':
      result := TEpiScriptFunction_SystemFunctions.Create(otFuncIif, ParamList);
    'label':
      result := TEpiScriptFunction_SystemFunctions.Create(otFuncLabel, ParamList);
    'cwd':
      result := TEpiScriptFunction_SystemFunctions.Create(otFuncCwd, ParamList);


    { Casting }
    'boolean':
      result := TTypeCast.Create(otBoolCast,    ParamList);
    'integer':
      result := TTypeCast.Create(otIntegerCast, ParamList);
    'date':
      result := TTypeCast.Create(otDateCast,    ParamList);
    'float':
      result := TTypeCast.Create(otFloatCast,   ParamList);
    'time':
      result := TTypeCast.Create(otTimeCast,    ParamList);
    'string':
      result := TTypeCast.Create(otStringCast,  ParamList);

    { Observations }
    'verified':
      result := TEpiScriptFunction_ObsFunctions.Create(otFuncVerified, ParamList);
    'deleted':
      result := TEpiScriptFunction_ObsFunctions.Create(otFuncDeleted, ParamList);
  else
    result := nil;
  end;

  if (Assigned(Result)) then
    Result.FExecutor := Executor;
end;

function TFunctionCall.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
var
  AType: TASTResultType;
  i: Integer;
  S: String;
  AllowedCounts: TBoundArray;
  CurrentCount: Integer;
  lParam: TExpr;
  TF: TTypesAndFlagsRec;
begin
  Result := inherited TypeCheck(TypeChecker, TypesAndFlags);

//  if Assigned(FParamList) then
//    result := result and FParamList.TypeCheck(TypeChecker);

  if (not Result) then exit;

  AllowedCounts := ParamCounts;

  if Assigned(FParamList) then
    CurrentCount := FParamList.Count
  else
    CurrentCount := 0;

  S := '';
  Result := false;
  for i := Low(AllowedCounts) to High(AllowedCounts) do
  begin
    if (AllowedCounts[i] < 0) and
       (Abs(AllowedCounts[i]) <= CurrentCount)
    then
      begin
        Result := true;
        break;
      end;

    if AllowedCounts[i] = CurrentCount then
    begin
      Result := true;
      break;
    end;
    S := S + IntToStr(AllowedCounts[i]) + ',';
  end;
  Delete(S,Length(S),1);

  if not result then
  begin
    DoTypeCheckError('Incorrect number of parameters given. Accepts: %s Given %d',
      [S, FParamList.Count],
      TypeChecker
    );
    Exit(False);
  end;

  if Assigned(FParamList) then
  begin
    for i := 0 to FParamList.Count -1 do
    begin
      lParam := Param[i];
      TF := ParamAcceptType(i);

      if (not lParam.TypeCheck(TypeChecker, TF)) then
      begin
        Result := false;
        Exit;
      end;

      if not
         (
           ((rtObject in TF.ResultTypes) and (lParam.InheritsFrom(TCustomVariable))) or
           (lParam.ResultType in TF.ResultTypes)
         )
      then
        begin
          S := '';
          for AType in TF.ResultTypes do
          begin
            if AType = rtAny then
              S += 'missing,'
            else
              S += ASTResultTypeString[AType] + ',';
          end;

          Delete(S, Length(S), 1);


          DoTypeCheckError(
            'Parameter no. %d accept types: %s' + LineEnding +
            'But was given: %s',
            [i + 1, S, ASTResultTypeString[Param[i].ResultType]],
            TypeChecker
          );

          Exit(False);
        end;  // if ....
    end;  // for i := 0 ...
  end;  // if assigned( ...
end;

function TFunctionCall.DoEvaluate: boolean;
var
  i: Integer;
begin
  Result := inherited DoEvaluate;

  if (Result) then
    begin
      for i := 0 to FParamList.Count - 1 do
        Param[i].DoEvaluate;
    end;
end;
{
function TFunctionCall.AsInteger: ASTInteger;
begin
  if IsMissing then
    Result := inherited AsInteger
  else
    case ResultType of
      rtBoolean:
        result := ASTInteger(AsBoolean);
    else
      Result := inherited AsInteger;
    end;
end;

function TFunctionCall.AsFloat: ASTFloat;
begin
  if IsMissing then
    Result := inherited AsFloat
  else
    case ResultType of
      rtBoolean,
      rtInteger:
        result := AsInteger;
    else
      Result := inherited AsFloat;
    end;
end;

function TFunctionCall.AsDate: EpiDate;
begin
  if IsMissing then
    Result := inherited AsDate
  else
    case ResultType of
      rtBoolean:
        result := EpiDate(AsBoolean);
    else
      Result := inherited AsDate;
    end;
end;

function TFunctionCall.AsTime: EpiDateTime;
begin
  if IsMissing then
    Result := inherited AsTime
  else
    case ResultType of
      rtBoolean,
      rtInteger:
        result := AsInteger;
    else
      Result := inherited AsTime;
    end;
end;

function TFunctionCall.AsString: EpiString;
begin
  if IsMissing then
    Result := inherited AsString
  else
  case ResultType of
    rtBoolean:
      result := BoolToStr(AsBoolean, True);
    rtInteger:
      result := IntToStr(AsInteger);
    rtFloat:
      result := FloatToStr(AsFloat);
    rtTime:
      result := TimeToStr(AsTime);
    rtDate:
      result := FormatDateTime('YYYY-MM-DD', AsDate);
  else
    Result := inherited AsString;
  end;
end;     }

{ TCustomNew }

function TCustomNew.GetRequireOpenProject: Boolean;
begin
  case SubCommand of
    ccData,
    ccVariable,
    ccDataset,
    ccValuelabel,
    ccResult:
      result := true;

    ccProject,
    ccGlobal:
      result := false;
  else
    result := true;
  end;
end;

constructor TCustomNew.Create(AOptionList: TOptionList;
  ASubCommand: TCrudCommand);
begin
  inherited Create(AOptionList, stNew, ASubCommand);
end;

{ TAssignment }

procedure TAssignment.DoObservedChange(Sender: TObject);
begin
  if Sender = FExpr then
    FExpr := nil;
  if Sender = FVAriable then
    FVAriable := nil;
end;

function TAssignment.GetRequireOpenProject: Boolean;
begin
  Result := false;
end;

constructor TAssignment.Create(const Variable: TCustomVariable; const Expr: TExpr);
begin
  inherited Create(stAssignment);
  FExpr := Expr;
  FVAriable := Variable;
  ObserveObject(FExpr);
  ObserveObject(FVAriable);
end;

destructor TAssignment.Destroy;
begin
  FExpr.Free;
  FVAriable.Free;
  inherited Destroy;
end;

function TAssignment.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  VarT: TASTResultType;
  ExpT: TASTResultType;
  EFlags: TTypesAndFlagsRec;
  EV: TCustomExecutorVariable;
begin
  // Since assignment vary greatly, we need to make diffent check for combination.
  // eg. evtField may be AsValue and AsObject.
  // but evtGlobalVector can only be AsValue. Otherwise assignment fails.
  Parser.SetTypeCheckErrorOutput(false);

  // Check field
  EFlags := TypesAndFlags(AllResultDataTypes, [evtField], [evfInternal, evfAsValue, evfAsObject]);
  Result := FVariable.TypeCheck(Parser, EFlags);

  // Check globals
  EFlags := TypesAndFlags(AllResultDataTypes, [evtGlobal, evtGlobalVector], [evfInternal, evfAsValue]);
  Result := result or FVariable.TypeCheck(Parser, EFlags);

  Parser.SetTypeCheckErrorOutput(true);

  EV := Parser.GetExecVariable(FVAriable.Ident);

  // Just make sure we output the error.
  if (not Result) then
    begin
      if (not Assigned(EV) ) then
        begin
          FVAriable.TypeCheck(Parser, EFlags);
          Exit;
        end;

      if (EV.VarType = evtField) then
        begin
          EFlags := TypesAndFlags(AllResultDataTypes, [evtField], [evfInternal, evfAsValue, evfAsObject]);
          Result := FVariable.TypeCheck(Parser, EFlags);
          Exit;
        end
      else
        begin
          EFlags := TypesAndFlags(AllResultDataTypes, [evtGlobal, evtGlobalVector], [evfInternal, evfAsValue]);
          Result := FVariable.TypeCheck(Parser, EFlags);
          Exit;
        end;
    end;


  EFlags := TypesAndFlags(AllResultDataTypes, ExecutorVariableTypesData);
  if (EV.VarType = evtField) then
    Include(EFlags.Flags, evfAsObject);

  if (not FExpr.TypeCheck(Parser, EFlags)) then
    begin
      result := false;
      Exit;
    end;

  if Result then
    begin
      VarT := FVAriable.ResultType;
      ExpT := FExpr.ResultType;
      Result := (Ord(VarT) >= Ord(ExpT)) or
                ((VarT in [rtInteger, rtDate]) and (ExpT in [rtInteger, rtDate])) or
                ((VarT in [rtFloat,   rtTime]) and (ExpT in [rtFloat,   rtTime]));

      if not result then
        DoTypeCheckError(
          'Incompatible types: ' + LineEnding +
          'Variable ' + FVAriable.FIdent + ' expect result to be of type ' +  ASTResultTypeString[VarT] + LineEnding +
          'but expression is of type ' + ASTResultTypeString[ExpT],
          Parser
        );
    end;
end;

{ TIfThen }

procedure TIfThen.DoObservedChange(Sender: TObject);
begin
  if Sender = FExpr then
    FExpr := nil;
end;

function TIfThen.GetRequireOpenProject: Boolean;
begin
  Result := false;
end;

constructor TIfThen.Create(const Expr: TExpr;
  const ThenStatement: TCustomStatement; const ElseStatement: TCustomStatement);
begin
  inherited Create(stIfThen);
  FExpr := Expr;
  ObserveObject(FExpr);
  FThenStatement := ThenStatement;
  FElseStatement := ElseStatement;
end;

destructor TIfThen.Destroy;
begin
  FExpr.Free;
  FThenStatement.Free;
  FElseStatement.Free;
  inherited Destroy;
end;

function TIfThen.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  EFlags: TTypesAndFlagsRec;
begin
  EFlags := TypesAndFlags(
              AllResultDataTypes,
              ExecutorVariableTypesData,
              [evfAsValue, evfInternal]
            );

  Result := inherited TypeCheck(Parser) and
            (Expr.TypeCheck(Parser, EFlags));

  if result and
     (not (Expr.ResultType = rtBoolean))
  then
    DoTypeCheckError(
      rsExpressionReturnType1,
      [ASTResultTypeString[rtBoolean]],
      Parser
    );

  // Typechecking is done during execution in DoStatement(...)

{  if Result and Assigned(ThenStatement) then
    result := ThenStatement.TypeCheck(Parser);

  if Result and Assigned(ElseStatement) then
    result := ElseStatement.TypeCheck(Parser);}
end;

{ TLiteral }

function TLiteral.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;
  FEvalValue.Missing := false;
end;

{ TBooleanLiteral }

function TBooleanLiteral.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;
  FEvalValue.BoolVal := FOriginalVal;
end;

constructor TBooleanLiteral.Create(const Value: Boolean);
begin
  inherited Create(otBoolLiteral, nil, nil);
  FOriginalVal := Value;
{  FEvalValue.IntVal    := ASTInteger(Value);
  FEvalValue.FloatVal  := ASTFloat(FEvalValue.IntVal);
  FEvalValue.DateVal   := EpiDate(Value);
  FEvalValue.TimeVal   := EpiTime(FEvalValue.IntVal);
  FEvalValue.StringVal := BoolToStr(Value, True);      }
end;

function TBooleanLiteral.ResultType: TASTResultType;
begin
  Result := rtBoolean;
end;

{ TIntegerLiteral }

function TIntegerLiteral.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;
  FEvalValue.IntVal := FOriginalVal;
end;

constructor TIntegerLiteral.Create(const Value: ASTInteger);
begin
  inherited Create(otIntegerLiteral, nil, nil);
//  FEvalValue.BoolVal   := Boolean(Value);
  FOriginalVal := Value;
{  FEvalValue.FloatVal  := ASTFloat(Value);
  FEvalValue.DateVal   := EpiDate(Value);
  FEvalValue.TimeVal   := EpiTime(Value);
  FEvalValue.StringVal := IntToStr(Value);      }
end;

function TIntegerLiteral.ResultType: TASTResultType;
begin
  Result := rtInteger;
end;

{ TFloatLiteral }

function TFloatLiteral.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;
  FEvalValue.FloatVal := FOriginalVal;
end;

constructor TFloatLiteral.Create(const Value: ASTFloat);
begin
  inherited Create(otFloatLiteral, nil, nil);
  FOriginalVal := Value;
{  FEvalValue.BoolVal   := Boolean(Trunc(Value));
  FEvalValue.IntVal    := Trunc(Value);
  FEvalValue.DateVal   := FEvalValue.IntVal;
  FEvalValue.TimeVal   := EpiTime(Value);
  FEvalValue.StringVal := FloatToStr(Value);      }
end;

function TFloatLiteral.ResultType: TASTResultType;
begin
  Result := rtFloat;
end;
{
function TFloatLiteral.AsBoolean: Boolean;
begin
  Result := Boolean(AsInteger);
end;

function TFloatLiteral.AsInteger: ASTInteger;
begin
  Result := Trunc(AsFloat)
end;

function TFloatLiteral.AsFloat: ASTFloat;
begin
  Result := FValue;
end;

function TFloatLiteral.AsDate: EpiDate;
begin
  Result := AsInteger;
end;

function TFloatLiteral.AsTime: EpiDateTime;
begin
  Result := AsFloat;
end;

function TFloatLiteral.AsString: EpiString;
begin
  Result := FloatToStr(AsFloat);
end;
 }
{ TStringLiteral }

function TStringLiteral.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;
  FEvalValue.StringVal := FOriginalVal;
end;

constructor TStringLiteral.Create(const Value: EpiString);
var
  Val: Boolean;
  Dummy: string;
begin
  inherited Create(otStringLiteral, nil, nil);
  FOriginalVal := Value;

{  if not (TryStrToBool(Value, FEvalValue.BoolVal)) then
    FEvalValue.BoolVal := false;

  if not (TryStrToInt64(Value, FEvalValue.IntVal)) then
    FEvalValue.IntVal := TEpiIntField.DefaultMissing;

  if not TryStrToFloat(Value, FEvalValue.FloatVal) then
    FEvalValue.FloatVal := TEpiFloatField.DefaultMissing;

  if not EpiStrToDateGuess(Value, FEvalValue.DateVal, Dummy) then
    FEvalValue.DateVal := TEpiDateField.DefaultMissing;

  if not EpiStrToTimeGues(Value, FEvalValue.TimeVal, Dummy) then
    FEvalValue.TimeVal := TEpiDateTimeField.DefaultMissing;   }
end;

function TStringLiteral.ResultType: TASTResultType;
begin
  Result := rtString;
end;

{ TTypeCast }

function TTypeCast.DoEvaluate: Boolean;
begin
  Result := inherited DoEvaluate;

  if (not Result) then
    exit;

  case Param[0].ResultType of
    rtBoolean:
      begin
        FEvalValue.BoolVal   := Param[0].AsBoolean;

        FEvalValue.IntVal    := Integer(FEvalValue.BoolVal);
        FEvalValue.FloatVal  := FEvalValue.IntVal;
        FEvalValue.DateVal   := FEvalValue.IntVal;
        FEvalValue.TimeVal   := FEvalValue.IntVal;
        FEvalValue.StringVal := BoolToStr(FEvalValue.BoolVal, 'true', 'false');
      end;

    rtInteger:
      begin
        FEvalValue.IntVal    := Param[0].AsInteger;

        FEvalValue.BoolVal   := Boolean(FEvalValue.IntVal);
        FEvalValue.DateVal   := FEvalValue.IntVal;
        FEvalValue.FloatVal  := FEvalValue.IntVal;
        FEvalValue.TimeVal   := FEvalValue.IntVal;
        FEvalValue.StringVal := IntToStr(FEvalValue.IntVal);
      end;

    rtDate:
      begin
        FEvalValue.DateVal   := Param[0].AsDate;

        FEvalValue.BoolVal   := Boolean(FEvalValue.DateVal);
        FEvalValue.IntVal    := FEvalValue.DateVal;
        FEvalValue.FloatVal  := FEvalValue.DateVal;
        FEvalValue.TimeVal   := FEvalValue.DateVal;
        FEvalValue.StringVal := DateToStr(FEvalValue.DateVal);
      end;

    rtFloat:
      begin
        FEvalValue.FloatVal  := Param[0].AsFloat;

        FEvalValue.BoolVal   := Not SameValue(FEvalValue.FloatVal, Extended(0));
        FEvalValue.IntVal    := Trunc(FEvalValue.FloatVal);
        FEvalValue.DateVal   := FEvalValue.IntVal;
        FEvalValue.TimeVal   := FEvalValue.FloatVal;
        FEvalValue.StringVal := FloatToStr(FEvalValue.FloatVal);
      end;

    rtTime:
      begin
        FEvalValue.TimeVal := Param[0].AsTime;

        FEvalValue.BoolVal   := SameValue(FEvalValue.TimeVal, EpiDateTime(0));
        FEvalValue.IntVal    := Trunc(FEvalValue.TimeVal);
        FEvalValue.DateVal   := FEvalValue.IntVal;
        FEvalValue.FloatVal  := FEvalValue.TimeVal;
        FEvalValue.StringVal := TimeToStr(FEvalValue.TimeVal);
      end;

    rtString:
      begin
        FEvalValue.StringVal := Param[0].AsString;

        FEvalValue.BoolVal   := (UTF8LowerString(FEvalValue.StringVal) = 'true');
        FEvalValue.IntVal    := StrToInt64Def(FEvalValue.StringVal, TEpiIntField.DefaultMissing);
        FEvalValue.DateVal   := StrToIntDef(FEvalValue.StringVal, TEpiDateField.DefaultMissing);
        FEvalValue.FloatVal  := StrToFloatDef(FEvalValue.StringVal, TEpiFloatField.DefaultMissing);
        FEvalValue.TimeVal   := StrToTimeDef(FEvalValue.StringVal, TEpiDateTimeField.DefaultMissing);
      end;
  end;
end;

procedure TTypeCast.DoTypeCastError(const Msg: UTF8String);
begin
  raise Exception.Create(Msg);
end;

function TTypeCast.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  Result[0] := 1;
end;

function TTypeCast.ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec;
begin
  result := inherited ParamAcceptType(ParamNo);
  Result.ResultTypes := [rtAny, rtFloat, rtInteger, rtTime, rtDate, rtString, rtBoolean];
end;

function TTypeCast.ResultType: TASTResultType;
begin
  Case FOp of
    otBoolCast:    result := rtBoolean;
    otIntegerCast: result := rtInteger;
    otDateCast:    result := rtDate;
    otFloatCast:   result := rtFloat;
    otTimeCast:    result := rtTime;
    otStringCast:  result := rtString;
  end;
end;

constructor TTypeCast.Create(const AOperation: TParserOperationType;
  const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

{
function TTypeCast.AsBoolean: Boolean;
begin
  result := inherited AsBoolean;

  if IsMissing then
    exit;

  Case FOp of
    otBoolCast:    result := FBoolVal;
    otIntegerCast: result := Boolean(FIntVal);
    otDateCast:    result := Boolean(FDateVal);
    otFloatCast:   result := not SameValue(FFloatVal, 0);
    otTimeCast:    result := not SameValue(FTimeVal, 0);
    otStringCast:  result := UTF8LowerString(FStringVal) = 'true';
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsBoolean: Type casting error!');
  end;
end;

function TTypeCast.AsInteger: ASTInteger;
begin
  Result := inherited AsInteger;

  if IsMissing then Exit;

  Case FOp of
    otBoolCast:    result := Integer(FBoolVal);
    otIntegerCast: result := FIntVal;
    otDateCast:    result := Integer(FDateVal);
    otFloatCast:   result := Trunc(FFloatVal);
    otTimeCast:    result := Trunc(FTimeVal);
    otStringCast:  result := StrToInt(FStringVal);
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsInteger: Type casting error!');
  end;

{  Case FOp of
    otBoolCast:    result := Integer(AsBoolean);
    otIntegerCast: result := Param[0].AsInteger;
    otDateCast:    result := Param[0].AsDate;
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsInteger: Type casting error!');
  end;  }
end;

function TTypeCast.AsFloat: ASTFloat;
begin
  result := inherited AsFloat;

  if IsMissing then
    exit;

  Case FOp of
    otBoolCast:    if FBoolVal then
                     result := 1
                   else
                     result := 0;
    otIntegerCast: result := FIntVal;
    otDateCast:    result := FDateVal;
    otFloatCast:   result := FFloatVal;
    otTimeCast:    result := FTimeVal;
    otStringCast:  result := StrToFloat(FStringVal);
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsFloat: Type casting error!');
  end;

{  Case FOp of
    otBoolCast,
    otIntegerCast: result := AsInteger;
    otDateCast:    result := AsDate;
    otTimeCast:    result := Param[0].AsTime;
    otFloatCast:   result := Param[0].AsFloat;
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsFloat: Type casting error!');
  end;   }
end;

function TTypeCast.AsDate: EpiDate;
begin
  result := inherited AsDate;

  if IsMissing then
    exit;

  Case FOp of
    otBoolCast:    result := EpiDate(FBoolVal);
    otIntegerCast: result := EpiDate(FIntVal);
    otDateCast:    result := FDateVal;
    otFloatCast:   result := Trunc(FFloatVal);
    otTimeCast:    result := Trunc(FTimeVal);
    otStringCast:  result := Trunc(StrToDate(FStringVal));
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsDate: Type casting error!');
  end;

  {
  Case FOp of
    otBoolCast:    result := EpiDate(AsBoolean);
    otIntegerCast: result := Param[0].AsInteger;
    otDateCast:    result := Param[0].AsDate;
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsDate: Type casting error!');
  end;     }
end;

function TTypeCast.AsTime: EpiDateTime;
begin
  result := inherited AsDate;

  if IsMissing then
    exit;

  Case FOp of
    otBoolCast:    if FBoolVal then
                     result := 1
                   else
                     result := 0;
    otIntegerCast: result := FIntVal;
    otDateCast:    result := FDateVal;
    otFloatCast:   result := FFloatVal;
    otTimeCast:    result := FTimeVal;
    otStringCast:  result := StrToTime(FStringVal);
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsTime: Type casting error!');
  end;

  {Case FOp of
    otBoolCast,
    otIntegerCast: result := AsInteger;
    otDateCast:    result := AsDate;
    otTimeCast:    result := Param[0].AsTime;
    otFloatCast:   result := Param[0].AsFloat;
  else
    // Should get caught in TypeChecking.
    DoTypeCastError('TTypeCast.AsTime: Type casting error!');
  end;    }
end;


function TTypeCast.AsString: EpiString;
begin
  result := inherited AsString;

  if IsMissing then
    exit;

  Case FOp of
    otBoolCast:    result := BoolToStr(FBoolVal, 'true', 'false');
    otIntegerCast: result := IntToStr(FIntVal);
    otDateCast:    result := DateToStr(FDateVal);
    otFloatCast:   result := FloatToStr(FFloatVal);
    otTimeCast:    result := TimeToStr(FTimeVal);
    otStringCast:  result := FStringVal;
  end;
end;    }

function TTypeCast.IsMissing: Boolean;
begin
  Result := Param[0].IsMissing;

  if (not Result) then
    begin
      case FOp of
        otStringCast:
          result := TEpiStringField.CheckMissing(FEvalValue.StringVal);

        otIntegerCast:
          result := TEpiIntField.CheckMissing(FEvalValue.IntVal);

        otFloatCast:
          result := TEpiFloatField.CheckMissing(FEvalValue.FloatVal);

        otBoolCast:
          result := false; // Boolean does not have a missing state.

        otDateCast:
          result := TEpiDateField.CheckMissing(FEvalValue.DateVal);

        otTimeCast:
          result := TEpiDateTimeField.CheckMissing(FEvalValue.TimeVal);
      end;
    end;
end;

{ TUserFunction }

procedure TUserFunction.ExecuteStatements;
begin
  FExecutor.ExecStatement(FFunctionDefinition);
end;

function TUserFunction.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  Result[0] := FFunctionDefinition.ParameterTypeList.Count;
end;

function TUserFunction.ParamAcceptType(ParamNo: Integer): TTypesAndFlagsRec;
begin
  Result := inherited ParamAcceptType(ParamNo);
  Result.ResultTypes := [FFunctionDefinition.ParameterTypeList[ParamNo].ParamType];
end;

constructor TUserFunction.Create(FunctionDefinition: TFunctionDefinition;
  const ParamList: TParamList);
begin
  inherited Create(ParamList);

  FFunctionDefinition := FunctionDefinition;
end;

function TUserFunction.DoEvaluate: boolean;
var
  Expr: TExpr;
begin
  Result := inherited DoEvaluate;

  ExecuteStatements;

  if (Assigned(FFunctionDefinition.ActualdReturnStatement)) then
    begin
      Expr := FFunctionDefinition.ActualdReturnStatement.ReturnExpression;

      FEvalValue.BoolVal   := Expr.AsBoolean;
      FEvalValue.IntVal    := Expr.AsInteger;
      FEvalValue.DateVal   := Expr.AsDate;
      FEvalValue.FloatVal  := Expr.AsFloat;
      FEvalValue.TimeVal   := Expr.AsTime;
      FEvalValue.StringVal := Expr.AsString;
    end;
end;

function TUserFunction.IsMissing: Boolean;
begin
  if (Assigned(FFunctionDefinition.ActualdReturnStatement)) then
    Result := FFunctionDefinition.ActualdReturnStatement.ReturnExpression.IsMissing;
end;

function TUserFunction.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
begin
  Result := inherited TypeCheck(TypeChecker, TypesAndFlags);
end;

function TUserFunction.ResultType: TASTResultType;
begin
  Result := FFunctionDefinition.ReturnType;
end;

{ TExpr }

function TExpr.GetAsString: EpiString;
begin
  case ResultType of
    rtBoolean: result := BoolToStr(FEvalValue.BoolVal, True);
    rtInteger: result := IntToStr(FEvalValue.IntVal);
    rtDate:    result := DateToStr(FEvalValue.DateVal);
    rtFloat:   result := FloatToStr(FEvalValue.FloatVal);
    rtTime:    result := TimeToStr(FEvalValue.TimeVal);
    rtString:  result := FEvalValue.StringVal;
  end;
end;

function TExpr.GetAsTime: EpiDateTime;
var
  Msg: string;
begin
  case ResultType of
    rtBoolean: result := Integer(FEvalValue.BoolVal);
    rtInteger: result := FEvalValue.IntVal;
    rtDate:    result := FEvalValue.DateVal;
    rtFloat:   result := FEvalValue.FloatVal;
    rtTime:    result := FEvalValue.TimeVal;
    rtString:  if (not EpiStrToTimeGues(FEvalValue.StringVal, Result, Msg)) then
                  Result := TEpiDateTimeField.DefaultMissing;
  end;
end;

procedure TExpr.ResetEvalValue;
begin
//  FEvalValue := Default(TExprValue);
  FEvalValue.BoolVal   := false;
  FEvalValue.DateVal   := TEpiDateField.DefaultMissing;
  FEvalValue.FloatVal  := TEpiFloatField.DefaultMissing;
  FEvalValue.IntVal    := TEpiIntField.DefaultMissing;
  FEvalValue.StringVal := TEpiStringField.DefaultMissing;
  FEvalValue.TimeVal   := TEpiDateTimeField.DefaultMissing;
  FEvalValue.Missing   := true;
end;

function TExpr.GetAsBoolean: Boolean;
begin
  case ResultType of
    rtBoolean: result := FEvalValue.BoolVal;
    rtInteger: result := Boolean(FEvalValue.IntVal);
    rtDate:    result := Boolean(FEvalValue.DateVal);
    rtFloat:   result := FEvalValue.FloatVal > 0;
    rtTime:    result := FEvalValue.TimeVal > 0;
    rtString:  result := StrToBoolDef(FEvalValue.StringVal, False);
  end;
end;

function TExpr.GetAsDate: EpiDate;
var
  Msg: string;
begin
  case ResultType of
    rtBoolean: result := EpiDate(FEvalValue.BoolVal);
    rtInteger: result := FEvalValue.IntVal;
    rtDate:    result := FEvalValue.DateVal;
    rtFloat:   result := Trunc(FEvalValue.FloatVal);
    rtTime:    result := Trunc(FEvalValue.TimeVal);
    rtString:  if (not EpiStrToDateGuess(FEvalValue.StringVal, Result, Msg)) then
                  Result := TEpiDateField.DefaultMissing;
  end;
end;

function TExpr.GetAsFloat: ASTFloat;
begin
  case ResultType of
    rtBoolean: result := Integer(FEvalValue.BoolVal);
    rtInteger: result := FEvalValue.IntVal;
    rtDate:    result := FEvalValue.DateVal;
    rtFloat:   result := FEvalValue.FloatVal;
    rtTime:    result := FEvalValue.TimeVal;
    rtString:  result := StrToFloatDef(FEvalValue.StringVal, TEpiFloatField.DefaultMissing);
  end;
end;

function TExpr.GetAsInteger: ASTInteger;
begin
  case ResultType of
    rtBoolean: result := Integer(FEvalValue.BoolVal);
    rtInteger: result := FEvalValue.IntVal;
    rtDate:    result := FEvalValue.DateVal;
    rtFloat:   result := Trunc(FEvalValue.FloatVal);
    rtTime:    result := Trunc(FEvalValue.TimeVal);
    rtString:  result := StrToInt64Def(FEvalValue.StringVal, TEpiIntField.DefaultMissing);
  end;
end;

function TExpr.CommonType(const A, B: TExpr): TASTResultType;
begin
  result := TASTResultType(Math.Max(Ord(A.ResultType), Ord(B.ResultType)));
end;

function TExpr.DoEvaluate: boolean;
begin
  Result := true;

  ResetEvalValue;

  if (Assigned(Left)) then
    Result := Result and Left.DoEvaluate;

  if (Assigned(Right)) then
    Result := Result and Right.DoEvaluate;
end;

procedure TExpr.DoObservedChange(Sender: TObject);
begin
  if Sender = FL then
    FL := nil;
  if Sender = FR then
    FR := nil;
end;

procedure TExpr.RuntimeError(EClass: TExceptionClass; const Msg: string);
begin
  Raise EClass.Create(Msg);
end;

constructor TExpr.Create(const Op: TParserOperationType; const L, R: TExpr);
begin
  inherited Create(stNone);
  FOp := Op;
  FL := L;
  FR := R;
  ObserveObject(L);
  ObserveObject(R);
  ResetEvalValue;
end;

destructor TExpr.Destroy;
begin
  FL.Free;
  FR.Free;
  inherited Destroy;
end;

function TExpr.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  TypesAndFlags: TTypesAndFlagsRec;
begin
  TypesAndFlags.ExecutorVariableTypes := ExecutorVariableTypesAll;
  TypesAndFlags.ResultTypes           := AllResultDataTypes;
  TypesAndFlags.Flags                 := [evfInternal, evfAsValue];
  result := TypeCheck(Parser, TypesAndFlags);
end;

function TExpr.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
begin
  result := inherited TypeCheck(TypeChecker);

  if Assigned(FL) then
    result := FL.TypeCheck(TypeChecker, TypesAndFlags);

  if result and Assigned(FR) then
    result := FR.TypeCheck(TypeChecker, TypesAndFlags);
end;

function TExpr.ResultType: TASTResultType;
begin
  // Default result type is rtUndefined
  result := rtUndefined;
end;

function TExpr.Evaluate: TExpr;
begin
  if (FEvaluated) then
    Raise Exception.Create('Double Evaluation!');

  DoEvaluate;

  FEvaluated := true;
  result := self;
end;

procedure TExpr.ResetEvaluation;
begin
  FEvaluated := false;
end;

{function TExpr.AsBoolean: Boolean;
begin
  ASTCurrentExecutionObject := self;
  result := false; //TEpiBoolField.DefaultMissing;
end;

function TExpr.AsInteger: ASTInteger;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiIntField.DefaultMissing;
end;

function TExpr.AsFloat: ASTFloat;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiFloatField.DefaultMissing;
end;

function TExpr.AsDate: EpiDate;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiDateField.DefaultMissing;
end;

function TExpr.AsTime: EpiDateTime;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiDateTimeField.DefaultMissing;
end;

function TExpr.AsString: EpiString;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiStringField.DefaultMissing;
end;
 }
function TExpr.AsIdent: UTF8String;
begin
//  result := '';
  result := AsString;
end;

function TExpr.IsMissing: Boolean;
begin
  result := FEvalValue.Missing;
end;

function TExpr.IsUserMissing: Boolean;
begin
  result := false;
end;

{ TCustomVariable }

function TCustomVariable.GetIdent: UTF8String;
begin
  result := FIdent;
end;

class function TCustomVariable.FieldTypeToParserType(FieldType: TEpiFieldType
  ): TASTResultType;
begin
  result := FieldTypeToASTTypeTable[FieldType];
end;

constructor TCustomVariable.Create(const AIdent: UTF8String;
  AExecutor: IEpiScriptExecutor);
begin
  inherited Create(otVariable, nil, nil);
  FIdent := AIdent;
  FExecutor := AExecutor;
  FVarType := vtVariable;
end;

destructor TCustomVariable.Destroy;
begin
  FIdent := '';
  FPONotifyObservers(Self, ooFree, nil);
  If Assigned(FObservers) then
    FreeAndNil(FObservers);
  inherited Destroy;
end;

function TCustomVariable.ResultType: TASTResultType;
var
  ExecVarType: TExecutorVariableType;
begin
  ExecVarType := Executor.GetVariableExecType(Ident);

  case ExecVarType of
    evtGlobal,
    evtGlobalVector,
    evtField,
    evtResultConst,
    evtResultVector,
    evtResultMatrix,
    evtFunctionParam:
      Result := FieldTypeToParserType(Executor.GetVariableType(Self));

    evtDataset:
      result := rtObject;

    evtValuelabel:
      result := FieldTypeToParserType(
                  TExecutorValuelabelsetVariable(Executor.GetExecVariable(AsIdent)).Valuelabelset.LabelType
                );
  end;
end;

function TCustomVariable.TypeCheck(TypeChecker: IEpiTypeChecker;
  TypesAndFlags: TTypesAndFlagsRec): boolean;
var
  EV: TCustomExecutorVariable;
  S: String;
  E: TExecutorVariableType;
begin
  Result := inherited TypeCheck(TypeChecker, TypesAndFlags);

  if (not Result) then Exit;

  EV := TypeChecker.GetExecVariable(Ident);

  if (not Assigned(EV)) and
     (evfExternal in TypesAndFlags.Flags)
  then
    Exit;

  Result := False;

  if (not Assigned(EV)) then
    begin
      DoTypeCheckError('Identifier "' + Ident + '" not found!', TypeChecker);
      Exit;
    end;

  if (not (EV.VarType in TypesAndFlags.ExecutorVariableTypes)) then
    begin
      S := '';
      for E in TypesAndFlags.ExecutorVariableTypes do
        S := S + ExecutorVariableTypeString[E] + ', ';
      Delete(S, Length(S)-1, 2);

      DoTypeCheckError('Identifier "' + Ident + '" is not the correct category!' + LineEnding +
                         'Expected: ' + S + LineEnding +
                         'Got: ' + ExecutorVariableTypeString[EV.VarType],
                       TypeChecker);
      Exit;
    end;
  Result := true;
end;

function TCustomVariable.AsIdent: UTF8String;
begin
  Result := Ident;
end;

function TCustomVariable.DoEvaluate: boolean;
begin
  Result := inherited DoEvaluate;

  if (not Result) then
    Exit;

  Executor.GetVariableValues(Self, @FEvalValue);
end;

{function TCustomVariable.AsBoolean: Boolean;
begin
  Result := Executor.GetVariableValueBool(Self);
end;

function TCustomVariable.AsInteger: ASTInteger;
begin
  Result := Executor.GetVariableValueInt(Self);
end;

function TCustomVariable.AsFloat: ASTFloat;
begin
  Result := Executor.GetVariableValueFloat(Self);
end;

function TCustomVariable.AsDate: EpiDate;
begin
  Result := Executor.GetVariableValueDate(Self);
end;

function TCustomVariable.AsTime: EpiDateTime;
begin
  Result := Executor.GetVariableValueTime(Self);
end;

function TCustomVariable.AsString: EpiString;
begin
  Result := Executor.GetVariableValueString(Self);
end;}

function TCustomVariable.IsMissing: Boolean;
begin
  result := FExecutor.GetVariableValueMissing(Self);
end;

function TCustomVariable.IsUserMissing: Boolean;
begin
  Result := FExecutor.GetVariableValueUserMissing(Self);
end;

procedure TCustomVariable.FPOAttachObserver(AObserver: TObject);
begin
  if not Assigned(FObservers) then
    FObservers := TFPList.Create;

  FObservers.Add(AObserver);
end;

procedure TCustomVariable.FPODetachObserver(AObserver: TObject);
begin
  if not Assigned(FObservers) then exit;

  FObservers.Remove(AObserver);
end;

procedure TCustomVariable.FPONotifyObservers(ASender: TObject;
  AOperation: TFPObservedOperation; Data: Pointer);
Var
  O : TObject;
  I : Integer;
  Obs : IFPObserver;
begin
  If Assigned(FObservers) then
    For I := FObservers.Count - 1 downto 0 do
      begin
        O := TObject(FObservers[i]);
        If O.GetInterface(SGUIDObserver, Obs) then
          Obs.FPOObservedChanged(Self, AOperation, Data);
      end;
end;

{ TVariableList }

function TVariableList.GetCount: Integer;
begin
  result := FVarList.Count;
end;

function TVariableList.GetVariable(const Index: Integer): TCustomVariable;
begin
  result := FVarList[Index];
end;

constructor TVariableList.Create;
begin
  inherited Create;
  FVarList := TVarList.Create;
  FExpanded := false;
end;

destructor TVariableList.Destroy;
begin
  FVarList.Free;
  inherited Destroy;
end;

procedure TVariableList.Add(Variable: TCustomVariable);
begin
  FVarList.Add(Variable);
end;

procedure TVariableList.Delete(Index: Integer);
begin
  FVarList.Delete(Index);
end;

function TVariableList.TypeCheck(VariableChecker: IVariableCheck;
  TypeChecker: IEpiTypeChecker): boolean;
var
  V: TCustomVariable;
  i: Integer;
begin
  Result := true;

  i := 0;
  for V in FVarList do
    result := result and V.TypeCheck(TypeChecker, VariableChecker.GetAcceptedVariableTypesAndFlags(i));
end;

function TVariableList.GetIdentsAsList: TStrings;
var
  i: Integer;
begin
  Result := TStringListUTF8.Create;

  for i := 0 to Count - 1 do
    result.Add(Variable[i].Ident);
end;

function TVariableList.GetEnumerator: TVariableListEnumerator;
begin
  result := TVariableListEnumerator.Create(Self);
end;

function TVariableList.ExpandVariableList(VariableChecker: IVariableCheck
  ): boolean;
var
  V: TCustomVariable;
  VR: TVariableRange;
  Exec: IEpiScriptExecutor;
  i: Integer;
  VL: TVariableList;
  TmpVarList: TVarList;
begin
  result := true;
  if FExpanded then exit;

  TmpVarList := TVarList.Create;

  for i := 0 to FVarList.Count - 1 do
    begin
      V := FVarList[i];
      Exec := V.Executor;

      // Call typecheck here, such than any nested variables containing indexes (like @{$dataset[...]}
      // get to proper label the ident!
      Exec.SetTypeCheckErrorOutput(false);
      V.TypeCheck(Exec, TypesAndFlags());
      Exec.SetTypeCheckErrorOutput(true);

      if (UTF8Pos('?', V.Ident) > 0) or
         (UTF8Pos('*', V.Ident) > 0) or
         (V is TVariableRange)
      then
        begin
          Result := Exec.ExpandVariableList(V, VariableChecker, TmpVarList.Count, VL);

          if (not result) then
            Exit;

          for V in VL do
            TmpVarList.Add(V);

          VL.Free;
        end
      else
        begin
          TmpVarList.Add(V);
          Continue;
        end;
    end;

  FVarList.Free;
  FVarList := TmpVarList;

  FExpanded := true;
end;


{ TCustomCommand }

constructor TCustomCommand.Create(const ACommand: UTF8String);
var
  S: String;
  ST: TASTStatementType;
begin
  FCommand := ACommand;
  ST := CommandNameToStatementType(ACommand);

  Create(ST);
end;

constructor TCustomCommand.Create(ST: TASTStatementType);
begin
  inherited Create(ST);
end;

class function TCustomCommand.CommandNameToStatementType(
  const ACommand: UTF8String): TASTStatementType;
var
  S: String;

  procedure DoError();
  begin
    Raise Exception.Create('Commandname not found: ' + ACommand);
  end;

begin
  S := UTF8LowerString(UTF8Copy(ACommand, 1, 3));

  case S of
    '!':   Result := stTerm;
    'agg': Result := stAggregate;
    'app': Result := stAppend;
    'ass': Result := stAssert;
    'bro': Result := stBrowse;
    'cd':  Result := stCD;
    'clh': Result := stClh;
    'clo': Result := stClose;
    'cls': Result := stCls;
    'cou': Result := stCount;
    'dir': Result := stLS;
    'era': Result := stErase;
    'fre': Result := stFreq;
    'ls':  Result := stLS;
    'mea': Result := stMeans;
    'mer': Result := stMerge;
    'qui': Result := stQuit;
    'rea': Result := stRead;
    'rec': Result := stRecode;
    'reo': Result := stReorder;
    'res': Result := stReset;
    'run':
      if UTF8LowerString(ACommand) = 'runtest' then
        Result := stRuntest
      else
        Result := stRun;
    'sav': Result := stSave;
    'set': Result := stSet;
    'sor': Result := stSort;
    'tab': Result := stTables;
    'cta': Result := stCTable;
    'des': Result := stDescribe;
    'use': Result := stUse;
    'ver': Result := stVersion;
  else
    DoError();
  end;
end;

{ TStatementList }

function TStatementList.GetStatements(Index: Integer): TCustomStatement;
begin
  result := FStatements.Items[Index];
end;

function TStatementList.GetRequireOpenProject: Boolean;
begin
  result := false;
end;

function TStatementList.GetCount: Integer;
begin
  result := FStatements.Count;
end;

constructor TStatementList.Create;
begin
  inherited Create(stStatementList);
  FStatements := TStatements.Create;
end;

destructor TStatementList.Destroy;
begin
  // TODO: Destroy items
  FStatements.Free;
  inherited Destroy;
end;

procedure TStatementList.AddStatement(AStatement: TCustomStatement);
begin
  FStatements.Add(AStatement);
end;

function TStatementList.TypeCheck(Parser: IEpiTypeChecker): boolean;
var
  St: TCustomStatement;
begin
  Result := inherited TypeCheck(Parser);

//  for St in FStatements do
//    St.TypeCheck(Parser);
end;

end.
