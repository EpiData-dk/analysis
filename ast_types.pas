unit ast_types;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  epidatafiles, epidatafilestypes;

type
//  IdString = String[64];

  TASTResultType = (
    rtAny,          // Special result type that is always compatible/convertable with/to other result types (except rtUndefined). (Eg. missing literal)
    rtBoolean,
    rtInteger,
    rtDate,
    rtFloat,
    rtTime,
    rtString,
    rtArray,
    rtObject,
    rtUndefined     // Default result type which is ALWAYS incompatible with all other types. Classes should always override the default value.
  );
  TASTResultTypes = set of TASTResultType;

const

  ASTResultTypeString: array[TASTResultType] of string = (
    'any',
    'boolean',
    'integer',
    'date',
    'float',
    'time',
    'string',
    'array',
    'identifier',
    'undefined'
  );

  AllResultTypes = [rtAny..rtUndefined];
  AllResultDataTypes = AllResultTypes - [rtAny, rtObject, rtUndefined, rtArray];

  FieldTypeToASTTypeTable: array[TEpiFieldType] of TASTResultType =
  (
//  ftBoolean,
    rtBoolean,

//  ftInteger, ftAutoInc, ftFloat,
    rtInteger, rtInteger, rtFloat,

//  ftDMYDate, ftMDYDate, ftYMDDate,
    rtDate,    rtDate,    rtDate,

//  ftDMYAuto, ftMDYAuto, ftYMDAuto,
    rtDate,    rtDate,    rtDate,

//  ftTime,    ftTimeAuto,
    rtTime,    rtTime,

//  ftString, ftUpperString, ftMemo
    rtString, rtString,      rtString
  );

type
  TASTStatementType = (
  // None + atypical statements
    stNone,
    stStatementList,
    stNew,
    stFor,
    stFunctionDefinition,
    stReturn,
    stAssignment,
    stIfThen,
    stSelect,
    stEval,
    stAssert,
    stSet,

  // Variable Commands
    stTables,
    stCTable,
    stBrowse,
    stList,
    stEdit,
    stDrop,
    stKeep,
    stMeans,
    stAggregate,
    stUse,
    stFreq,
    stDescribe,
    stSort,
    stAppend,
    stMerge,
    stCheck,
    stReport,
    stReorder,
    stRecode,

  // String Commands
    stRead,
    stRun,
    stRuntest,
    stSave,
    stCD,
    stLS,
    stTerm,
    stErase,


  // Empty Commands
    stReset,
    stCls,
    stClh,
    stClose,
    stCount,
    stQuit,
    stVersion
  );

  TParserOperationType = (
    // Literals
    otTrue,
    otFalse,
    otMissingLiteral,
    otBoolLiteral,
    otIntegerLiteral,
    otFloatLiteral,
    otStringLiteral,
    otRecNumberLiteral,
    otIdentifier,

    // Binary ops
    otAnd,
    otOr,
    otMod,
    otDiv,
    otMult,
    otPlus,
    otMinus,
    otDivide,
    otExponential,
    otShl,
    otShr,
    otXor,

    // Unary (excluding minus which is defined above)
    otNot,

    // Relational
    otEQ,
    otNEQ,
    otLT,
    otLTE,
    otGT,
    otGTE,

    // Built in statements
    otIf,
    otThen,
    otElse,

    // typecast
    otStringCast,
    otIntegerCast,
    otFloatCast,
    otBoolCast,
    otDateCast,
    otTimeCast,

    // Symbols
    otOpenParan,
    otCloseParan,
    otOpenBracket,
    otCloseBracket,
    otSemicolon,
    otComma,
    otPeriod,
    otAssign,

    // Term
    otVariable,

    // Function
    otFunction,

    // -date functions
    otFuncToday,          // Current date
    otFuncDay,            // Day part of a date
    otFuncMonth,          // Month part of a date
    otFuncYear,           // Year part of a date
    otFuncDayOfWeek,      // Day of the week, result is a number between 1-7
    otFuncWeek,           // Week number

    // -time functions
    otFuncNow,            // Current time
    otFuncHour,           // Hour part of a time
    otFuncMinut,          // Minut part of a time
    otFuncSecond,         // Seconds part of a time

    // -string functions
    otFuncSubString,      // Takes substring out of string
    otFuncLength,         // Length in representable chars
    otFuncPos,            // Returns position of character in string
    otFuncTrim,           // Trims string for whitespace
    otFuncLower,          // lowercases a string
    otFuncUpper,          // UPPERCASES a string
    otFuncConcat,         // Concatenation function with check for missing

    // -math functions
    otFuncAbs,            // Abs(x)
    otFuncExp,            // Exp(x), e^x...
    otFuncFraction,       // Fraction(x), fractional part of a float
    otFuncLn,             // Ln(x), Natural logarithm
    otFuncLog,            // Log(x), 10-based logarithm
    otFuncRound,          // Round(x,x), round float to number of digits
    otFuncSqrt,           // Srqt(x), Square root of X.
    otFuncRandom,         // Random(x), Generata random integer number between 0 - X.
    otFuncSameValue,      // SameValue(x, y[, z]), compares x vs. y if they are the same (given z as epsilon for precision)
    otFuncLRE,            // LogRelativeError - compares two values, gives best digit precission.
    otFuncSum,            // Sums up parameters, ignoring missing

    // -- trigonometry
    otFuncTan,            // Tan(x)
    otFuncArcTan,         // ArcTan(x)
    otFuncSin,            // Sin(x)
    otFuncArcSin,         // ArcSin(x)
    otFuncCos,            // Cos(x)
    otFuncArcCos,         // ArcCos(x)

    // -system functions
    otFuncIdentExists,    // returns true/false based on the identifier exists
    otFuncIdentType,      // returns type of identifier
    otFuncDataType,       // returns data type of an identifier
    otFuncSize,           // returns the size of an identifier
    otFuncIif,            // terniary operator iif(<expr>, <true-val>, <false-val>)
    otFuncLabel,          // extracts the label of an indentifier
    otFuncCwd,            // returns current working dir as string

    // -observation functions
    otFuncDeleted,        // true/false if an observation is marked for deletion
    otFuncVerified        // true/false if an observation is marked as verified
  );

  TExecutorGetRecordIndex = function(Sender: TObject): integer;
  TExecutorError = procedure(const Msg: string; const LineNo,
    ColNo: integer; const TextFound: string) of object;
  TExecutorSetFieldValue = procedure(Const Sender: TObject;
    Const F: TEpiField; Const Value: Variant) of object;
  TExecutorGetFieldValue = function(Const Sender: TObject;
    Const F: TEpiField): Variant of object;

resourcestring
  rsExpressionReturnType1 = 'Expression return type must be %s';
  rsExpressionReturnType2 = 'Expression return type must be %s or %s';
  rsExpressionReturnType3 = 'Expression return type must be %s, %s or %s';
  rsExpressionReturnType4 = 'Expression return type must be %s, %s, %s or %s';
  rsExpressionReturnType5 = 'Expression return type must be %s, %s, %s, %s or %s';

  rsFunctionAcceptType1 = '"%s" accepts %s expressions. Given expression result is: %s';
  rsFunctionAcceptType2 = '"%s" accepts %s or %s expressions. Given expression result is: %s';


function EpiFieldTypeFromString(Const S: UTF8String; out Ft: TEpiFieldType): boolean;

implementation

uses
  LazUTF8;

function EpiFieldTypeFromString(const S: UTF8String; out Ft: TEpiFieldType
  ): boolean;
begin
  result := true;

  case UTF8LowerString(S) of
    'b', 'bool', 'boolean':
      ft := ftBoolean;

    's', 'str', 'string':
      ft := ftString;

    'i', 'int', 'integer':
      ft := ftInteger;

    't', 'time':
      ft := ftTime;

    'f', 'float':
      ft := ftFloat;

    'd', 'date':
      ft := ftDMYDate;
  else
    result := false;
  end;
end;

end.

