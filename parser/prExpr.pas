unit prExpr;
interface
{
Copyright:   1997-1999 Production Robots Engineering Ltd, all rights reserved.
Version:     1.04 3/7/99
Modified     by Salah Mahmud     2002
Modified     by Michael Bruus, Jens Lauritsen 2005
}

uses
  TypInfo, Math,  Classes,UEpidataTypes, SysUtils,SMUtils,AAPasTok,AAChrStm,UExpToken,Ustatfunctions,UVectorOp
,ansDatatypes;

resourcestring
 SMExpected='"%s" Expected but "%s" found';
 SMUnExpectedEOF='Unexpected end of file';
 SMUnExpectedEOL='Unexpected end of line';

const
 MissingString='.';

type

  TExpression =class(TInterfacedObject, IValue)
  private
    fTag: integer;
  protected
    function TestParameters: Boolean; virtual;
  public
    function GetObject: TObject; virtual;
    function TypeInfo: PTypeInfo; virtual;
    function AsString: String; virtual;
    function AsFloat: extended; virtual;
    function AsInteger: Integer; virtual;
    function AsBoolean: Boolean; virtual;
    function AsObject: TObject; virtual;
    function AsIValue: IValue; virtual;
    function IsMissing2: Boolean; virtual;
    function IsMissingValue: Boolean; virtual; abstract;
    function ExprType: TExprType; virtual; abstract;
    function TypeName: String; virtual;
    function GetVarName :string;virtual;
    Procedure SetVarName(const aName:string);virtual;
    function GetValue: Variant; virtual;
    Procedure SetValue(const value:Variant); virtual;
    constructor Create;
    destructor Destroy; override;
    function CanReadAs(aType: TExprType): Boolean;
    Procedure SetTag(value:integer);virtual;
    function GetTag: integer;virtual;
  end;

  TStringLiteral =class(TExpression)
  private
    FAsString: String;
  protected
  public
    function IsMissing2: boolean; override;
    function AsString: String; override;
    function AsBoolean: boolean; override;
    function ExprType: TExprType; override;
    constructor Create( aAsString: String);
  end;

  TFloatLiteral =  class(TExpression)
  private
    FAsFloat: extended;
  protected
  public
    function AsFloat: extended; override;
    function ExprType: TExprType; override;
    constructor Create( aAsFloat: extended);
  end;

  TIntegerLiteral =  class(TExpression)
  private
    FAsInteger: Integer;
  protected
  public
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function ExprType: TExprType; override;
    constructor Create( aAsInteger: Integer);
  end;

  TEnumeratedLiteral =  class(TIntegerLiteral)
  private
    Rtti: Pointer;
  protected
  public
    function TypeInfo: PTypeInfo; override;
    constructor Create(aRtti: Pointer; aAsInteger: Integer);
    constructor StrCreate(aRtti: Pointer; const aVal: String);
  end;

  TBooleanLiteral =  class(TExpression)
  private
    FAsBoolean: Boolean;
  protected
  public
    function AsBoolean: Boolean; override;
    function ExprType: TExprType; override;
    function AsString: String;override;
    constructor Create( aAsBoolean: Boolean);
  end;

{#TODO1 change date to support Epidate}
  TDateLiteral = class(TExpression)
  private
    fAsDate: EpiInt;
  public
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsString: String; override;
    function ExprType: TExprType; override;
    constructor Create(pAsDate: EpiInt);
  end;

 TMissingLiteral = Class(TExpression)
  public
    function AsFloat: extended;  override;
    function AsInteger: Integer;  override;
    function AsString:  String;  override;
    function AsBoolean: Boolean;  override;
    function ExprType: TExprType;  override;
    constructor Create;
  end;


  TObjectRef =  class(TExpression)
  private
    FObject: TObject;
  protected
  public
    function TypeInfo: PTypeInfo; override;
    function AsObject: TObject; override;
    function ExprType: TExprType; override;
    constructor Create( aObject: TObject);
  end;
(*
  TVarRef = Class(TExpression)
  public
    function AsFloat: extended;  override;
    function AsInteger: Integer;  override;
    function AsString:  String;  override;
    function AsBoolean: Boolean;  override;
    function ExprType: TExprType;  override;
    constructor Create;
  end;
*)

  TParameterList =  class(TList)
  private
    function GetAsObject(i: Integer): TObject;
    function GetAsString(i: Integer): String;
    function GetAsFloat(i: Integer): extended;
    function GetAsInteger(i: Integer): Integer;
    function GetAsBoolean(i: Integer): Boolean;
    function GetExprType(i: Integer): TExprType;
    function GetParam(i: Integer): IValue;
  public
    function AddExpression( e: IValue): Integer;
    destructor Destroy; override;
    property Param[i: Integer]: IValue read GetParam;
    property ExprType[i: Integer]: TExprType read GetExprType;
    property AsObject[i: Integer]: TObject read GetAsObject;
    property AsString[i: Integer]: String read GetAsString;
    property AsFloat[i: Integer]: extended read GetAsFloat;
    property AsInteger[i: Integer]: Integer read GetAsInteger;
    property AsBoolean[i: Integer]: Boolean read GetAsBoolean;
  end;

  TFunction =  class(TExpression)
  private
    function GetParam(n: Integer): IValue;
  protected
    FParameterList: TParameterList;
  public
    constructor Create( aParameterList: TParameterList);
    destructor Destroy; override;
    function ParameterCount: Integer;
  {$IFDEF HANDLEMISSING}
    function MissingParam:boolean;
  {$ENDIF }
   property Param[n: Integer]: IValue read GetParam;
  end;

  EExpression = class(Exception);
  TIdentifierFunction = function( const Identifier: String;
                                  ParameterList: TParameterList): IValue of Object;

  TExprFunc = function( Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction): IValue of Object;

  TSMParser=class
  private
    fTokenizer: TSMBaseTokenizer;
    fIdentifierFunction  :TIdentifierFunction;
    FIgnoreError: boolean;
    function GetLookAheadToken: TSMToken;
    function GetOnError: TOnParseError;
    procedure SetOnError(const Value: TOnParseError);
    function GetPrevToken: TSMToken;
    function GetCurrentToken: TSMToken;
    procedure SetCurrentToken(const Value: TSMToken);
    procedure SetIgnoreError(const Value: boolean);
protected
    constructor Create(IDF:TIdentifierFunction);
    function Chain(Tokenizer: TSMBaseTokenizer; IDF: TIdentifierFunction;
      NextFunc: TExprFunc; Ops: TSMTokenTypeSet): IValue;
    function CharacterString(aToken: TSMToken): IValue;
    function Expression(Tokenizer: TSMBaseTokenizer;IDF: TIdentifierFunction): IValue;
    function Factor(Tokenizer: TSMBaseTokenizer;IDF: TIdentifierFunction): IValue;
    function GetOperator(Tokenizer: TSMBaseTokenizer; var Operator: TSMTokenType): Boolean;
    function HandleIdentifiers(Tokenizer: TSMBaseTokenizer;IDF: TIdentifierFunction): IValue;
    function IsOperator(operator: TSMTokenType): boolean;
    function ObjectProperty(Tokenizer: TSMBaseTokenizer;     IDF: TIdentifierFunction): IValue;
    function Simple(Tokenizer: TSMBaseTokenizer;   IDF: TIdentifierFunction): IValue;
    function SimpleFactor(Tokenizer: TSMBaseTokenizer; IDF: TIdentifierFunction): IValue;
    function StandardFunctions(Ident: TSMToken; PL: TParameterList): IValue;
    function Term(Tokenizer: TSMBaseTokenizer;  IDF: TIdentifierFunction): IValue;
    function UnsignedNumber(aToken: TSMToken): IValue;
    procedure AssignTokenizer(pTokenizer: TSMBaseTokenizer);
    procedure TokenErrHandler(const msg:string;Token:TSMToken;var Handled:boolean);//Tokenizer error handler
  public
    constructor CreateFileParser(const fileName: string;IDF: TIdentifierFunction);
    constructor CreateStringParser(const parseString:string;IDF:TIdentifierFunction);
    constructor CreateStreamParser(aStream : TStream;IDF:TIdentifierFunction);
    constructor CreateToken(pTokenizer: TSMBaseTokenizer;IDF: TIdentifierFunction);
    destructor Destroy;override;
    function Accept(tokens: TSMTokenTypeSet; const ErrorMsg:string=''): TSMToken;
    function ReadToEOL: string;
    function  ReadToChar(const tok: char=#32):String;
    class procedure Error(Token:TSMToken;const ErrorMsg: string='Unknown Error');virtual;
    procedure ErrorFmt(Token:TSMTOken;const ErrorMsg: string;Args:array of const);
    function GenExpression(const parseString:string;IDF:TIdentifierFunction=nil):IValue;
    class function GenBooleanExpression(const parseString:string;IDF:TIdentifierFunction=nil):IValue;
    class function Genxpression(const parseString: string;IDF: TIdentifierFunction): IValue;
    function GetRestOfLine(jumpEOL:boolean=true): string;
    function NextToken: TSMToken;
    property PrevToken : TSMToken read GetPrevToken;// write fPrevToken;
    property LookAheadToken  : TSMToken read GetLookAheadToken;
    property CurrentToken : TSMToken read GetCurrentToken write SetCurrentToken;
    property Tokenizer           :TSMBaseTokenizer read fTokenizer;
    property IdentifierFunction   :TIdentifierFunction read fIdentifierFunction;
    property OnError :TOnParseError read GetOnError write SetOnError;
    property IgnoreError:boolean read FIgnoreError write SetIgnoreError;
 end;

  function CreateExpression( const S: String;
                IdentifierFunction: TIdentifierFunction): IValue;

  function CheckEnumeratedVal(Rtti: Pointer; const aVal: String): IValue;


var
  InstanceCount: Integer = 0;
{
function SetGlobalParserError(pOnError: TOnParseError):boolean;
function SetGlobalParserError(pOnError: TOnParseError):boolean;
}

implementation

uses uDateUtils, Uformats, cStrings, UVectorVar, Udos;


var
 parser:TSMParser;
 fOnError: TOnParseError;

procedure Error(const ErrorMsg:string='Unknown Error');
var
 handled :boolean;
 Token : TSMToken;
begin
 handled := false;
 if assigned(fOnError) then
     fOnError(ErrorMsg,Token,handled);
 if not handled then
   raise EExpression.Create(format('%s' + #13#10+ 'At line %d',[ErrorMsg,Token.line]));
end;



function TokenToExprType(aToken:TSMTokenType):TExprType;
begin
 case aToken of
// opObjectCast:result:= ttObject;
 opStringCast:result:= ttString;
 opFloatCast:result:= ttFloat;
 opIntegerCast:result:=  ttInteger;
 opEnumeratedCast:result:=ttEnumerated;
 opBooleanCast:result:= ttBoolean;
 end;
end;

type

  TUnaryOp =  class(TExpression)
  private
    Operand: IValue;
    OperandType: TExprType;
    Operator: TSMTokenType;
  protected
  public
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function ExprType: TExprType; override;
    constructor Create( aOperator: TSMTokenType; aOperand: IValue);
  end;

  TBinaryOp =  class(TExpression)
  private
    Operand1, Operand2: IValue;
    Operator: TSMTokenType;
    OperandType: TExprType;
  protected
  public
    function AsString: String; override;
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function ExprType: TExprType; override;
    constructor Create( aOperator: TSMTokenType; aOperand1, aOperand2: IValue);
  end;

  TRelationalOp =  class(TExpression)
  private
    Operand1, Operand2: IValue;
    OperandType: TExprType;
    Operator: TSMTokenType;
  protected
  public
    function AsString: String; override;
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function ExprType: TExprType; override;
    constructor Create( aOperator: TSMTokenType; aOperand1, aOperand2: IValue);
  end;

  TObjectProperty =  class(TExpression)
  private
    Obj: TObject;
    PropInfo: PPropInfo;
    PropType: TExprType;
  protected
  public
    function TypeInfo: PTypeInfo; override;
    function AsObject: TObject; override;
    function AsString: String; override;
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function ExprType: TExprType; override;
    constructor Create(aObj: IValue; const PropName: String);
  end;


const
  MaxStringLength = 1204*64; {why?}

  NExprType: array[TExprType] of String =
      ('Object', 'String', 'Float', 'Integer', 'Enumerated', 'Boolean','Missing','Date');

  UnaryOperators = [opNot];
  ExpOperator = [opExp];
  MultiplyingOperators = [opMult, opDivide, opDiv, opMod, opAnd, opShl, opShr];
  AddingOperators = [opPlus, opMinus, opOr, opXor];
  RelationalOperators = [opEq  , opNEq, opLT, opGT, opLTE, opGTE];

  NBoolean: array[Boolean] of String[5] = ('FALSE', 'TRUE');



function ResultType( Operator: TSMTokenType; OperandType: TExprType): TExprType;
  procedure NotAppropriate;
  begin
    Result:= ttString;
    Error(format( 'Operator %s is incompatible with %s',[parser.tokenizer.GetTokenName(Operator),NExprType[OperandType]]))
  end;
begin
  case OperandType of
    ttMissing:
    case Operator of
      opEq, opNEq: Result:= ttBoolean;
//      opMult,opDivide,opPlus,opMinus: Result:=ttFloat;
    else
      NotAppropriate;
    end;
    ttString:
    case Operator of
      opPlus: Result:= ttString;
      opEq..opGTE: Result:= ttBoolean;
    else
      NotAppropriate;
    end;
    ttFloat:
    case Operator of
      opExp, opMult, opDivide, opPlus, opMinus: Result:= ttFloat;
      opDiv: Result := ttInteger;
      opEq..opGTE: Result:= ttBoolean;
    else
      NotAppropriate;
    end;
    ttInteger:
    case Operator of
      opNot, opMult, opDiv, opMod, opAnd, opShl, opShr, opPlus, opMinus,
      opOr, opXor: Result:= ttInteger;
      opExp, opDivide: Result:= ttFloat;
      opEq..opGTE: Result:= ttBoolean;
    else
      NotAppropriate;
    end;
    ttDate:
    case Operator of
      opPlus, opMinus: Result:= ttInteger;
      opEq..opGTE: Result:= ttBoolean;
    else
      NotAppropriate;
    end;
    ttBoolean:
    case Operator of
      opNot, opAnd, opOr, opXor, opEq, opNEq: Result:= ttBoolean;
    else
      NotAppropriate;
    end;
    ttObject:
    case Operator of
      opEq, opNEq: Result:= ttBoolean;
    else
      NotAppropriate;
    end;
  else
    NotAppropriate
  end
end;

function IncompatibleTypes(T1, T2: TExprType): TExprType;
{result is not defined... Do this to avoid warning}
begin
  Error(format( 'Type %s is incompatible with type %s',
                                  [NExprType[T1], NExprType[T2]]))
end;

function CommonType( Op1Type, Op2Type: TExprType;Operator: TSMTokenType): TExprType;
begin
  if (Op1Type = ttObject) or (Op2Type = ttObject) then
  begin
    if Op1Type <> Op2Type then
      Result:= IncompatibleTypes(Op1Type, Op2Type)
    else
      Result:= ttObject
  end else
  if (Op1Type = ttDate) or (Op2Type = ttDate) then
  begin
    if (Op1Type = Op2Type) then
    begin
       if operator =opminus then
          Result:= ttInteger
       else if operator in [opEq..opGTE] then
            result:=ttdate
       else
         Error(format( 'Operator %s is incompatible with dates',[parser.tokenizer.GetTokenName(Operator)]))
    end
    else if ( Op1Type = ttInteger) or (Op2Type = ttInteger) then
    begin
       if operator in [opminus,opPlus] then
          Result:= ttDate
       else if operator in [opEq..opGTE] then
          Result:= IncompatibleTypes(Op1Type, Op2Type)
       else
         Error(format( 'Operator %s is incompatible with dates and integer',[parser.tokenizer.GetTokenName(Operator)]))
    end
    else if ( Op1Type = ttMissing) or (Op2Type = ttMissing) then
    begin
        if operator in [opEQ..opNEq] then
          Result := ttDate
        else
          Error(format( 'Operator %s is incompatible with dates and missing',[parser.tokenizer.GetTokenName(Operator)]))
    end
    else
      Result:= IncompatibleTypes(Op1Type, Op2Type)
  end else
  begin
    if Op1Type < Op2Type then
      Result:= Op1Type else
      Result:= Op2Type
  end;
end;

{$IFDEF HANDLEMISSING}
function IsMissing(Operand: IValue):boolean;
begin
{  if VarIsEmpty(operand.Value) then
    result:= true
  else}
  case Operand.ExprType of
    ttFloat : result:=Operand.AsFloat =NA_FLOAT;
    ttInteger,ttDate : result:=Operand.AsInteger =NA_INT
  else
     result:=false;
  end;
end;
{$ENDIF}

procedure InternalError( Code: Integer);
begin
  Error(format( 'Internal parser error. Code %d', [Code]))
end;

constructor TExpression.Create;
begin
  inherited Create;
  Inc(InstanceCount)
end;

destructor TExpression.Destroy;
begin
  Dec(InstanceCount);
  inherited Destroy
end;


function TExpression.GetValue: Variant;
begin
   result:=AsString;
end;

Procedure TExpression.SetValue(const value:Variant);
begin

end;

function TExpression.AsString: String;
{too scary to deal with Enumerated types here?}
begin
  case ExprType of
    ttObject: Result:= AsObject.ClassName;
    ttFloat:
    begin
{$IFDEF HANDLEMISSING}
   if AsFloat =NA_Float then
   begin
    result:=NA_STR;
    exit;
   end;
{$ENDIF }
    Result:= FloatToStr(AsFloat);
    end;
    ttInteger:
    begin
{$IFDEF HANDLEMISSING}
   if AsInteger =NA_INT then
   begin
    result:=NA_STR;
    exit;
   end;
{$ENDIF }
    Result:= IntToStr(AsInteger);
    end;
    ttEnumerated: Result:= GetEnumName(TypeInfo, AsInteger);
    ttBoolean: Result:= NBoolean[AsBoolean];
    ttMissing: Result:=MissingString;
    ttDate    :
    begin
{$IFDEF HANDLEMISSING}
     if AsInteger =NA_INT then
     begin
      result:=NA_STR;
      exit;
     end;
{$ENDIF }
      result:=EpiDateTostr(AsInteger,dfDMY);
   end
  else
//    Error(format('Cannot read %s as String',[NExprType[ExprType]]));
      result:=NA_STR;
  end
end;

function TExpression.AsFloat: extended;
begin
  case ExprType of
    ttInteger, ttEnumerated, ttBoolean:
    begin
      {$IFDEF HANDLEMISSING}
      if AsInteger =NA_INT then
      begin
        result:=NA_FLOAT;
        exit;
      end;
      {$ENDIF }
      Result:= AsInteger;
    end
  else
    result:=NA_FLOAT;
//    Error(format('Cannot read %s as Float',[NExprType[ExprType]]));
  end
end;

function TExpression.AsInteger: Integer;
begin
  case ExprType of
    ttBoolean: Result:= Integer(AsBoolean);
  else
//    Error(format('Cannot read %s as Integer',[NExprType[ExprType]]));
    result:=NA_INT;
  end;
end;

function TExpression.AsBoolean: Boolean;
begin
  Error(format('Cannot read %s as boolean',[NExprType[ExprType]]))
end;

function TExpression.AsObject: TObject;
begin
  Error(format('Cannot read %s as object', [NExprType[ExprType]]))
end;

function TExpression.TestParameters: Boolean;
begin
  Result:= true
end;

function TExpression.CanReadAs(aType: TExprType): Boolean;
var
  et: TExprType;
begin
  et:= ExprType;
  if (et = ttObject) or
     (aType = ttObject) then
    Result:= aType = et
  else
    Result:= aType <= et
end;

function TExpression.TypeName: String;
begin
  Result:= TypeInfo^.Name
end;

function TExpression.GetVarName :string;
begin
  Result:=''
end;

Procedure TExpression.SetVarName(const aName:string);
begin
end;

function TExpression.AsIValue: IValue;
begin
end;

function TExpression.IsMissing2: Boolean;
begin
  result := false;
end;

function TExpression.GetTag: integer;
begin
 result:=fTag;
end;

procedure TExpression.SetTag(value: integer);
begin
  fTag:=value;
end;


function TStringLiteral.AsString: String;
begin
  if FAsString = ' ' then
    Result := FAsString
  else
    Result:= TrimRight(FAsString);
end;

// TODO : Easy fix for reading "asboolean". Remove when boolean handling fixed!!!
function TStringLiteral.AsBoolean: boolean;
begin
  Result := AnsiUppercase(FAsString) = 'Y';
end;

function TStringLiteral.IsMissing2: boolean;
begin
  Result := FAsString = NA_STR;
end;

function TStringLiteral.ExprType: TExprType;
begin
  Result:= ttString
end;

constructor TStringLiteral.Create( aAsString: String);
begin
  inherited Create;
  FAsString:= aAsString
end;


function TFloatLiteral.AsFloat: extended;
begin
  Result:= FAsFloat
end;

function TFloatLiteral.ExprType: TExprType;
begin
  Result:= ttFloat
end;

constructor TFloatLiteral.Create( aAsFloat: extended);
begin
  inherited Create;
  FAsFloat:= aAsFloat
end;

function TIntegerLiteral.AsInteger: Integer;
begin
  Result:= FAsInteger
end;

// TODO : Easy fix for reading "asboolean". Remove when boolean handling fixed!!!
function TIntegerLiteral.AsBoolean: Boolean;
begin
  Result:= FAsInteger = 1
end;

function TIntegerLiteral.ExprType: TExprType;
begin
  Result:= ttInteger
end;

constructor TIntegerLiteral.Create( aAsInteger: Integer);
begin
  inherited Create;
  FAsInteger:= aAsInteger
end;

function TBooleanLiteral.AsBoolean: Boolean;
begin
  Result:= FAsBoolean
end;

function TBooleanLiteral.ExprType: TExprType;
begin
  Result:= ttBoolean
end;

function TBooleanLiteral.AsString: String;
Begin
  If FAsBoolean THEN Result:='Y' ELSE Result:='N';
END;

constructor TBooleanLiteral.Create( aAsBoolean: Boolean);
begin
  inherited Create;
  FAsBoolean:= aAsBoolean
end;



function TDateLiteral.AsFloat: extended;
BEGIN
  Result:= fAsDate;
END;

function TDateLiteral.AsInteger: Integer;
BEGIN
  Result:=fAsDate;
END;

function TDateLiteral.AsString: String;
BEGIN
  Result:=EpiDateToStr(fAsDate,DFDMY);
END;

Function TDateLiteral.ExprType: TExprType;
BEGIN
  Result:=ttDate;
END;

constructor TDateLiteral.Create(pAsDate: EpiInt);
BEGIN
  inherited Create;
  fAsDate:=pAsDate;
END;




function TMissingLiteral.AsFloat: extended;
begin
  result:=NA_FLOAT;
end;

function TMissingLiteral.AsInteger: Integer;
begin
  result:=NA_INT;
end;

function TMissingLiteral.AsString: String;
begin
  result:=NA_STR;
end;

// TODO : Easy fix for reading "asboolean"!!!
function TMissingLiteral.AsBoolean: Boolean;
begin
  result:=false;
end;

function TMissingLiteral.ExprType: TExprType;
begin
  result:=ttMissing;
end;

Constructor TMissingLiteral.Create;
begin
  inherited Create;
//  IF MissingAction=maRejectMissing THEN ResultEqualsMissing:=True;
end;

(*
function TVarREf.AsFloat: extended;
begin
  result:=NA_FLOAT;
end;

function TVarREf.AsInteger: Integer;
begin
  result:=NA_INT;
end;

function TVarREf.AsString: String;
begin
  result:='';
end;

function TVarREf.AsBoolean: Boolean;
begin
  result:=false;
end;

function TVarREf.ExprType: TExprType;
begin
  result:=ttMissing;
end;

Constructor TVarREf.Create;
begin
  inherited Create;
end;
*)

constructor TEnumeratedLiteral.Create(aRtti: Pointer; aAsInteger: Integer);
begin
  inherited Create(aAsInteger);
  Rtti:= aRtti
end;

constructor TEnumeratedLiteral.StrCreate(aRtti: Pointer; const aVal: String);
var
  i: Integer;
begin
  i:= GetEnumValue(PTypeInfo(aRtti), aVal);
  if i = -1 then
    Error(format('%s is not a valid value for %s',[aVal, PTypeInfo(aRtti)^.Name]));
  Create(aRtti, i)
end;


function CheckEnumeratedVal(Rtti: Pointer; const aVal: String): IValue;
begin
  try
    Result:= TEnumeratedLiteral.StrCreate(Rtti, aVal)
  except
    on EExpression do
      Result:= nil
  end
end;


function TObjectRef.AsObject: TObject;
begin
  Result:= FObject
end;

function TObjectRef.ExprType: TExprType;
begin
  Result:= ttObject
end;

constructor TObjectRef.Create( aObject: TObject);
begin
  inherited Create;
  FObject:= aObject
end;

function TUnaryOp.AsFloat: extended;
begin
{$IFDEF HANDLEMISSING}
  if IsMissing(Operand) then
  begin
    result:=NA_FLOAT;
    exit;
  end;
{$ENDIF }
  case Operator of
    opMinus: Result:= -Operand.AsFloat;
    opPlus: Result:= Operand.AsFloat;
  else
    Result:= inherited AsFloat;
  end
end;

function TUnaryOp.AsInteger: Integer;
begin
{$IFDEF HANDLEMISSING}
  if IsMissing(Operand) then
  begin
    result:=NA_INT;
    exit;
  end;
{$ENDIF }
  Result:= 0;
  case Operator of
    opMinus: Result:= -Operand.AsInteger;
    opPlus: Result:= Operand.AsInteger;
    opNot:
    case OperandType of
      ttInteger: Result:= not Operand.AsInteger;
      ttBoolean: Result:= Integer(AsBoolean);
    else
      InternalError(6);
    end;
  else
    Result:= inherited AsInteger;
  end
end;

function TUnaryOp.AsBoolean: Boolean;
begin
 case Operator of
    opNot: Result:= not(Operand.AsBoolean)
  else
    Result:= inherited AsBoolean;
  end
end;

function TUnaryOp.ExprType: TExprType;
begin
  Result:= ResultType(Operator, OperandType)
end;

constructor TUnaryOp.Create( aOperator: TSMTokenType; aOperand: IValue);
begin
  inherited Create;
  Operand:= aOperand;
  Operator:= aOperator;
  OperandType:= Operand.ExprType;
  if not (Operator in [opNot, opPlus, opMinus]) then
    Error(format( 'Operator %s is not simple unary operator',[parser.tokenizer.GetTokenName(aOperator)]))
end;

function TBinaryOp.AsString: String;
var
  t1, t2: string;
begin
  Result:= '';
  case ExprType of
    ttString:
      case Operator of
        opPlus: begin
                  if Operand1.IsMissing2 then t1 := '' else t1 := Operand1.AsString;
                  if Operand2.IsMissing2 then t2 := '' else t2 := Operand2.AsString;
                  Result:= t1 + t2;
                end
      else
        InternalError(10);
      end;
    ttFloat:
  {$IFDEF HANDLEMISSING}
    if IsMissing(self) then
      result:=NA_STR
    else
  {$ENDIF }
      Result:= FloatToStr(AsFloat);
    ttInteger:
  {$IFDEF HANDLEMISSING}
    if IsMissing(self) then
      result:=NA_STR
    else
  {$ENDIF }
      Result:= IntToStr(AsInteger);
    ttDate:
  {$IFDEF HANDLEMISSING}
   if IsMissing(self) then
      result:=NA_STR
    else
  {$ENDIF }
      Result:= EpiDateToStr(AsInteger, dfDMY);
    ttBoolean:
      Result:= NBoolean[AsBoolean];
  end
end;

function TBinaryOp.AsFloat: extended;
begin
  Result:= NA_FLOAT;
  {$IFDEF HANDLEMISSING}
   if IsMissing(Operand1) or  IsMissing(Operand2) then exit;
  {$ENDIF }
  case ExprType of
    ttFloat:
    begin
      case Operator of
        opExp:
          if (Operand1.AsFloat=0) and (Operand2.AsFloat<0) then
              Result:=NA_FLOAT
          else
              Result:= Power(Operand1.AsFloat,Operand2.AsFloat);
        opPlus: Result:= Operand1.AsFloat + Operand2.AsFloat;
        opMinus: Result:= Operand1.AsFloat - Operand2.AsFloat;
        opMult: Result:= Operand1.AsFloat * Operand2.AsFloat;
        opDivide: Result:= Operand1.AsFloat / Operand2.AsFloat;
      else
        InternalError(11);
      end;
    end;
    ttInteger:
        Result:= AsInteger;
    ttBoolean:
       Result:= Integer(AsBoolean);
  end
end;


function TBinaryOp.AsInteger: Integer;
begin
  Result:= NA_INT;
  {$IFDEF HANDLEMISSING}
    if IsMissing(Operand1) or  IsMissing(Operand2) then exit;
 {$ENDIF}
  case ExprType of
    ttInteger:
    case Operator of
      opPlus: Result:= Operand1.AsInteger + Operand2.AsInteger;
      opMinus: Result:= Operand1.AsInteger - Operand2.AsInteger;
      opMult: Result:= Operand1.AsInteger * Operand2.AsInteger;
      opDiv: Result:= Operand1.AsInteger div Operand2.AsInteger;
      opMod: Result:= Operand1.AsInteger mod Operand2.AsInteger;
      opShl: Result:= Operand1.AsInteger shl Operand2.AsInteger;
      opShr: Result:= Operand1.AsInteger shr Operand2.AsInteger;
      opAnd: Result:= Operand1.AsInteger and Operand2.AsInteger;
      opOr: Result:= Operand1.AsInteger or Operand2.AsInteger;
      opXor: Result:= Operand1.AsInteger xor Operand2.AsInteger
      else InternalError(12);
    end;
    ttdate:
    begin
    case Operator of
      opPlus: Result:= Operand1.AsInteger + Operand2.AsInteger;
      opMinus: Result:= Operand1.AsInteger - Operand2.AsInteger
      else InternalError(12);
    end;
    end;
    ttBoolean:
    begin
      Result:= Integer(AsBoolean);
    end
    else
      InternalError(12);
    end;
end;

function TBinaryOp.AsBoolean: Boolean;
begin
  Result:= false;
  case Operator of
    opAnd: Result:= Operand1.AsBoolean and Operand2.AsBoolean;
    opOr: Result:= Operand1.AsBoolean or Operand2.AsBoolean;
    opXor: Result:= Operand1.AsBoolean xor Operand2.AsBoolean;
  else
    InternalError(13);
  end
end;

function TBinaryOp.ExprType: TExprType;
begin
if OperandType=ttdate then
  result:=ttdate
else
  Result:= ResultType(Operator, OperandType)
end;

constructor TBinaryOp.Create( aOperator: TSMTokenType; aOperand1, aOperand2: IValue);
begin
  inherited Create;
  {what if type changes? Operands might be IF expressions!}
  Operator:= aOperator;
  Operand1:= aOperand1;
  Operand2:= aOperand2;
  OperandType:= CommonType(Operand1.ExprType, Operand2.ExprType,aOperator);
  if not (Operator in [opExp, opMult..opXor]) then
    Error(format( 'Operator %s is not simple binary operator',[parser.tokenizer.GetTokenName(aOperator)]))
end;

function TRelationalOp.AsString: String;
begin
  Result:= NBoolean[AsBoolean]
end;

function TRelationalOp.AsFloat: extended;
begin
  Result:= Integer(AsBoolean)
end;

function TRelationalOp.AsInteger: Integer;
begin
  Result:= Integer(AsBoolean)
end;

function TRelationalOp.AsBoolean: Boolean;
begin
  Result:= false;
  case OperandType of
{    ttMissing:
    Case Operator of
      opEq: Result:= Operand1.AsString = Operand2.AsString;
      opNEq: Result:=Operand1.AsString<>Operand2.AsString;
    else
      raise EExpression.CreateFmt('cannot apply %s to missing operands',
             [TokensList[Operator]]);
    end;}
    ttBoolean:
    case Operator of
      opEq: result:= operand1.AsString = operand2.AsString;
      opNEq: Result:= Operand1.AsString <> Operand2.AsString; // was Operand1.AsBoolean <> Operand2.AsBoolean;
    else
      Error(format('Operator %s does not apply to boolean operands',[parser.tokenizer.GetTokenName(Operator)]));
    end;

    ttInteger, ttdate:                    // added requirement may 2005 both operands must be non-missing
    case Operator of
      opLT: Result:= (Operand1.AsInteger < Operand2.AsInteger) and (Operand2.AsInteger <> NA_INT) and (Operand1.AsInteger <> NA_INT);
      opLTE: Result:= (Operand1.AsInteger <= Operand2.AsInteger) and (Operand2.AsInteger <> NA_INT) and (Operand1.AsInteger <> NA_INT);
      opGT: Result:= (Operand1.AsInteger > Operand2.AsInteger) and (Operand2.AsInteger <> NA_INT)  and (Operand1.AsInteger <> NA_INT);
      opGTE: Result:= (Operand1.AsInteger >= Operand2.AsInteger) and (Operand2.AsInteger <> NA_INT) and (Operand1.AsInteger <> NA_INT);
      opEq: Result:= (Operand1.AsInteger = Operand2.AsInteger) ;
      opNEq: Result:= Operand1.AsInteger <> Operand2.AsInteger;
    end;

    ttFloat:
    case Operator of                     // added requirement may 2005 both operands must be non-missing
      opLT: Result:= (Operand1.AsFloat < Operand2.AsFloat) and (Operand1.AsFloat <> NA_FLOAT) and (Operand2.AsFloat <> NA_FLOAT);
      opLTE: Result:= (Operand1.AsFloat <= Operand2.AsFloat) and (Operand1.AsFloat <> NA_FLOAT) and (Operand2.AsFloat <> NA_FLOAT);
      opGT: Result:= (Operand1.AsFloat > Operand2.AsFloat) and (Operand1.AsFloat <> NA_FLOAT) and (Operand2.AsFloat <> NA_FLOAT);
      opGTE: Result:= (Operand1.AsFloat >= Operand2.AsFloat) and (Operand1.AsFloat <> NA_FLOAT) and (Operand2.AsFloat <> NA_FLOAT) ;
      opEq: Result:= (Operand1.AsFloat = Operand2.AsFloat) ;
      opNEq: Result:= Operand1.AsFloat <> Operand2.AsFloat;
    end;

    ttString:
    case Operator of
      opLT: Result:= Operand1.AsString < Operand2.AsString;
      opLTE: Result:= Operand1.AsString <= Operand2.AsString;
      opGT: Result:= Operand1.AsString > Operand2.AsString;
      opGTE: Result:= Operand1.AsString >= Operand2.AsString;
      opEq: Result:= Operand1.AsString = Operand2.AsString;
      opNEq: Result:= Operand1.AsString <> Operand2.AsString;
    else
      Error(format('Operator %s does not apply to string comparisons',[parser.tokenizer.GetTokenName(Operator)]));
    end;
  end
end;

function TRelationalOp.ExprType: TExprType;
begin
  Result:= ttBoolean
end;

constructor TRelationalOp.Create( aOperator: TSMTokenType; aOperand1, aOperand2: IValue);
begin
  inherited Create;
  Operator:= aOperator;
  Operand1:= aOperand1;
  Operand2:= aOperand2;
  OperandType:= CommonType(Operand1.ExprType, Operand2.ExprType,aOperator);
  if not (Operator in RelationalOperators) then
    Error(format( 'Operator %s is not relational operator',[parser.tokenizer.GetTokenName(aOperator)]))
end;

function TObjectProperty.AsObject: TObject;
begin
  if PropType = ttObject then
    Result:= TObject(GetOrdProp(Obj, PropInfo))
  else
    Result:= inherited AsObject
end;

function TObjectProperty.AsString: String;
begin
  case PropType of
    ttString: Result:= GetStrProp(Obj, PropInfo);
    ttEnumerated: Result:= GetEnumName(PropInfo.PropType^, AsInteger);
  else
    Result:= inherited AsString
  end
end;

function TObjectProperty.AsFloat: extended;
begin
  if PropType = ttFloat then
    Result:= GetFloatProp(Obj, PropInfo)
  else
    Result:= inherited AsFloat
end;

function TObjectProperty.AsInteger: Integer;
begin
  case PropType of
    ttInteger, ttEnumerated:
      Result:= GetOrdProp(Obj, PropInfo)
  else
    Result:= inherited AsInteger;
  end
end;

function TObjectProperty.AsBoolean: Boolean;
begin
  if PropType = ttBoolean then
    Result:= LongBool(GetOrdProp(Obj, PropInfo))
  else
    Result:= inherited AsBoolean
end;

function TObjectProperty.ExprType: TExprType;
begin
  Result:= PropType
end;

constructor TObjectProperty.Create(aObj: IValue; const PropName: String);
begin
  inherited Create;
  Obj:= aObj.AsObject;
  PropInfo:= GetPropInfo(PTypeInfo(Obj.ClassInfo), PropName);
  if not Assigned(PropInfo) then
    Error(format('%s is not published property of %s',
                   [PropName, aObj.AsObject.ClassName]));
  case PropInfo.PropType^^.Kind of
    tkClass: PropType:= ttObject;
    tkEnumeration:
    if PropInfo.PropType^^.Name = 'Boolean' then {special case}
      PropType:= ttBoolean
    else
      PropType:= ttEnumerated; {not boolean}
    tkInteger, tkChar: PropType:= ttInteger;
    tkFloat: PropType:= ttFloat;
    tkString, tkLString, tkWString: PropType:= ttString;
  else
    Error(format('Property %s unsupported type', [PropName]));
  end
end;

function TParameterList.GetAsObject(i: Integer): TObject;
begin
  Result:= Param[i].AsObject
end;


function TParameterList.GetAsString(i: Integer): String;
begin
  Result:= Param[i].AsString
end;

function TParameterList.GetAsFloat(i: Integer): extended;
begin
  Result:= Param[i].AsFloat
end;

function TParameterList.GetAsInteger(i: Integer): Integer;
begin
  Result:= Param[i].AsInteger
end;

function TParameterList.GetAsBoolean(i: Integer): Boolean;
begin
  Result:= Param[i].AsBoolean
end;

function TParameterList.GetExprType(i: Integer): TExprType;
begin
  Result:= Param[i].ExprType
end;

function TParameterList.GetParam(i: Integer): IValue;
begin
  Result:= IValue(Items[i])
end;

function TParameterList.AddExpression( e: IValue): Integer;
begin
  assert(e<>nil, 'Nil expression inside addexpression of TparameterList');
  Result:= Add(Pointer(e));
  e._AddRef
end;

destructor TParameterList.Destroy;
var
  i: Integer;
begin
  for i:= 0 to (Count - 1) do
    IValue(Items[i])._Release;
  inherited Destroy
end;

function TFunction.GetParam(n: Integer): IValue;
begin
  Result:= FParameterList.Param[n]
end;

function TFunction.ParameterCount: Integer;
begin
  if Assigned(FParameterList) then
    ParameterCount:= FParameterList.Count
  else
    ParameterCount:= 0
end;

constructor TFunction.Create( aParameterList: TParameterList);
begin
  inherited Create;
  FParameterList:= aParameterList
end;

destructor TFunction.Destroy;
begin
  FParameterList.Free;
  inherited Destroy
end;

{$IFDEF HANDLEMISSING}
function TFunction.MissingParam: boolean;
var
 i, co : integer;
begin
  co :=ParameterCount;
  result:= true;
  for i := 0 to co-1 do
    if IsMissing(Param[i]) then
      exit;
  result:=false;
end;
{$ENDIF }

type
  TTypeCast = class(TFunction)
  private
    OperandType,
    Operator: TExprType;
  protected
    function TestParameters: Boolean; override;
  public
    function AsObject: TObject; override;
    function AsString: String; override;
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function ExprType: TExprType; override;
    constructor Create( aParameterList: TParameterList;
                        aOperator: TExprType);
  end;


  TMathExpression =  class(TFunction)
  private
    Operator: TSMTokenType;
  protected
    function TestParameters: Boolean; override;
  public
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean : boolean;override;
    function ExprType: TExprType; override;
    constructor Create( aParameterList: TParameterList;
                        aOperator: TSMTokenType);
  end;


  TStringExpression =  class(TFunction)
  private
    Operator: TSMTokenType;
  protected
    function TestParameters: Boolean; override;
  public
    function IsMissing2: boolean; override;
    function AsString: String; override;
    function AsInteger: Integer; override;
    function ExprType: TExprType; override;
    constructor Create( aParameterList: TParameterList;
                        aOperator: TSMTokenType);
  end;

  TConditional =  class(TFunction)
  private
    CommonType: TExprType;
    function Rex: IValue;
  protected
    function TestParameters: Boolean; override;
  public
    function AsString: String; override;
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function ExprType: TExprType; override;
    constructor Create( aParameterList: TParameterList);
  end;


function TStringExpression.AsString: String;
begin
{$IFDEF HANDLEMISSING}
   if MissingParam then
   begin
      result:=NA_STR;
      exit;
   end;
  {$ENDIF }
  case Operator of
    opUpper: Result:= AnsiUpperCase(Param[0].AsString);
    opLower: Result:= AnsiLowerCase(Param[0].AsString);
    opCopy,opSubstr:
    begin
      if ParameterCount=2 then
         Result:=  Copy(Param[0].AsString, Param[1].AsInteger, MAXINT)
      else
         Result:=  Copy(Param[0].AsString, Param[1].AsInteger, Param[2].AsInteger)
    end;
    opTrim: Result:= Sysutils.trim(Param[0].AsString);
    opSysDate: result:= EpiDateToStr(EpiToday,dfDMY);
    opSysTime: result:=TimeToStr(Time);
  else
    Result:= inherited AsString;
  end
end;

function TStringExpression.IsMissing2: boolean;
begin
  case Operator of
    opUpper, opLower, opCopy, opSubstr, opTrim:
      result := Param[0].IsMissing2;
  else
    result := false;
  end;
end;

function TStringExpression.AsInteger: Integer;
begin
{$IFDEF HANDLEMISSING}
  if MissingParam then
  begin
      result:=NA_INT;
      exit;
  end;
{$ENDIF }
  case Operator of
    opPos: Result:= Pos(Param[1].AsString, Param[0].AsString);
    opLength: if Param[0].AsString = NA_STR then result := 0
              else Result:= Length(Param[0].AsString);
    opFindFile: if FileExists(Param[0].AsString) then result:= 1
                else  result := 0;
  else
    Result:= inherited AsInteger
  end
end;

function TStringExpression.ExprType: TExprType;
begin
  case Operator of
    opUpper, opLower, opCopy,opSubstr,optrim,opSysDate,opSysTime: Result:= ttString;
  else
    Result:= ttInteger;
  end
end;

function TStringExpression.TestParameters: Boolean;
begin
  case Operator of
    opSysDate,opSysTime:
       result:=(ParameterCount = 0);
    opUpper, opLower, opLength,optrim, opFindFile:
      Result:= (ParameterCount = 1) and
               Param[0].CanReadAs(ttString);
    opCopy,opSubstr:
      Result:= ((ParameterCount = 3) and
            Param[0].CanReadAs(ttString) and
            (Param[1].ExprType = ttInteger) and
            (Param[2].ExprType = ttInteger)) OR
            ((ParameterCount = 2) and
            Param[0].CanReadAs(ttString) and
            (Param[1].ExprType = ttInteger));
    opPos:
      Result:= (ParameterCount = 2) and
            Param[0].CanReadAs(ttString) and
            Param[1].CanReadAs(ttString);
  else
    Result:= false;
  end;
end;

constructor TStringExpression.Create( aParameterList: TParameterList;
                                      aOperator: TSMTokenType);
begin
  inherited Create(aParameterList);
  Operator:= aOperator
end;

function TMathExpression.AsFloat: extended;
begin
{$IFDEF HANDLEMISSING}
  if MissingParam then
  begin
      result:=NA_FLOAT;
      exit;
  end;
  {$ENDIF }
  try
    case Operator of
      opAbs: Result:= Abs(Param[0].AsFloat);
      opArcTan: Result:= ArcTan(Param[0].AsFloat);
      opCos: Result:= Cos(Param[0].AsFloat);
      opExpf: Result:= Exp(Param[0].AsFloat);
      opFrac: Result:= Frac(Param[0].AsFloat);
      opInt: Result:= Int(Param[0].AsFloat);
      opLn: Result:= Ln(Param[0].AsFloat);
      opPi: Result:= Pi;
      opSin: Result:= Sin(Param[0].AsFloat);
      opSqr: Result:= Sqr(Param[0].AsFloat);
      opSqrt:
        if Param[0].AsFloat < 0 then
          Result := NA_FLOAT
        else
          Result := Sqrt(Param[0].AsFloat);
      opLog : Result:=Epilog10(Param[0].AsFloat);
      opRND :Result:= EpiRnd(Param[0].AsInteger);
      opRANG: Result:= EpiRanG(Param[0].AsFloat,Param[1].AsFloat);
      opPower: Result:= Power(Param[0].AsFloat,Param[1].AsFloat);
      opLRE :  Result:=EpiLRE(Param[0].AsFloat, Param[1].AsFloat);
      {  //todo: add confidence interval for proportion as function   oppropci :
         if ParameterCount= 2 then
             Result:=EpiProportionCI(Param[0].AsFloat,  Param[1].AsFloat)
  //}

      opSum: result:=EpiSum(param[0].AsObject);
  //    opRsum: result:=EpiRSum();
  //    ,opMean,opCentile,opSD, opVar
    else
      Result:= inherited AsFloat;
    end
  except
      result:=NA_FLOAT;
  //  Error('Invalid argument/mathematical expression');
  end;
end;

function TMathExpression.AsInteger: Integer;
var s:string;
begin
{$IFDEF HANDLEMISSING}
  if MissingParam and (Operator <> opMV) then
  begin
      result:=NA_INT;
      exit;
  end;
  {$ENDIF }
try
  case Operator of
    opTrunc: Result:= Trunc(Param[0].AsFloat);
    opRound: Result:= Round(Param[0].AsFloat);
    opAbs: Result:= Abs(Param[0].AsInteger);
    opRAN :Result:= EpiRAN(Param[0].AsInteger);
    opDay : result:=DatePart(Param[0].AsInteger,1);
    opMonth : result:=DatePart(Param[0].AsInteger,2);
    opYear  :  result:=DatePart(Param[0].AsInteger,3);
    opWeekNum: result:=EpiWeekNum(Param[0].AsInteger);
    opDayWeek  : result:=EpiDayOfWeekMon(Param[0].AsInteger);
    opDate :
    begin
       if ParameterCount= 1 then
           result:=EpiStrToDatefmt(Param[0].AsString,'')
      else if ParameterCount= 2 then
          result:=EpiStrToDatefmt(Param[0].AsString,Param[1].AsString)
    end;
    opDMY  : result:=EpiYMDToDate(-1,Param[2].AsInteger,Param[1].AsInteger,Param[0].AsInteger);
    opToday : result:=EpiToday;
    opMv:
    begin
      result := 0;
      if Param[0].IsMissingValue then result := 2;
      if Param[0].IsMissing2 then result := 1;
    end;
  else
    Result:= inherited AsInteger;
  end
except
   result:=NA_INT;
//  Error('Invalid argument/mathematical expression');
end;
end;

function TMathExpression.AsBoolean: boolean;
begin
Result:=false;
case Operator of
   opSameNumber:
   begin
     if ParameterCount=2 then
        Result:=SameValue(Param[0].AsFloat,Param[1].AsFloat,0)
     else
        Result:=SameValue(Param[0].AsFloat,Param[1].AsFloat,Param[2].AsFloat);
     // add precison to testresults
     epilre(Param[0].AsFloat,Param[1].AsFloat,1);
   end;
end;//case
end;


function TMathExpression.TestParameters: Boolean;
var TempError: TOnParseError;
begin
  Result:= True;
  case Operator of
    opToday:
    begin
       Result:= (ParameterCount = 0)
    end;
    opSum,opMean,opCentile,opSD, opVariance:
     begin
      Result:= (ParameterCount = 1);
    end;
    opTrunc, opRound, opArcTan, opCos, opExp, opFrac, opInt,
    opLn, opSin, opSqr, opSqrt, opAbs,opLog:
    begin
      Result:= (ParameterCount = 1) and
           Param[0].CanReadAs(ttFloat);
    end;
    opRAN,opRND:
    begin
      Result:= (ParameterCount = 1) and
           Param[0].CanReadAs(ttFloat);
    end;
    opDate:
    begin
      Result:= (ParameterCount in [1,2]) and
           Param[0].CanReadAs(ttString);
      if ParameterCount=2 then
         result:=result and Param[1].CanReadAs(ttString);
    end;
    opDay,opMonth,opYear,OpWeekNum,opDayWeek:
    begin
      Result:= (ParameterCount = 1) and
           Param[0].CanReadAs(ttDate);
    end;
    opSpan:
    begin
      Result:= (ParameterCount in [2,3]) and
           Param[0].CanReadAs(ttDate) and
           Param[1].CanReadAs(ttDate);
      if ParameterCount=3 then
         result:=result and Param[2].CanReadAs(ttString);
    end;
    opSameNumber:
    begin
      Result:= (ParameterCount in [2,3]) and
           Param[0].CanReadAs(ttFloat) and
           Param[1].CanReadAs(ttFloat);
      if ParameterCount=3 then
         result:=result and Param[2].CanReadAs(ttFloat);
    end;
    opPower, opRanG:
    begin
      Result:= (ParameterCount = 2) and
           Param[0].CanReadAs(ttFloat) and
           Param[1].CanReadAs(ttFloat);
    end;
    opDMY:
    begin
      Result:= (ParameterCount = 3) and
           Param[0].CanReadAs(ttFloat) and
           Param[1].CanReadAs(ttFloat)and
           Param[2].CanReadAs(ttFloat);
    end;
    oplre:
    begin
      Result:= (ParameterCount = 2) and
           Param[0].CanReadAs(ttFloat) and
           Param[1].CanReadAs(ttFloat);
    end;
    opMV:
    begin
      TempError := fOnError;
      fOnError := nil;
      try
        Result:= (ParameterCount = 1) and
                 (Param[0].GetObject is TVectorVar);
      except
        fOnError := TempError;
        Error('Expression MV only supports vectors');
      end;
      fOnError := TempError;
      Result:= Result and Param[0].CanReadAs(ttFloat);
    end;
  end
end;

function TMathExpression.ExprType: TExprType;
begin
  case Operator of
    opTrunc, opRound,opRAN,opDay,opMonth,opYear,OpWeekNum,opDayWeek,opSpan,opMv: Result:= ttInteger;
    opDate, opDMY,opToday: Result:= ttDate;
    opSameNumber: Result:=ttBoolean;
  else
    Result:= ttFloat;
  end
end;

constructor TMathExpression.Create( aParameterList: TParameterList;
                                    aOperator: TSMTokenType);
begin
  inherited Create(aParameterList);
  Operator:= aOperator
end;

function TTypeCast.AsObject: TObject;
begin
  if Operator = ttObject then
    Result:= Param[0].AsObject
  else
    Result:= inherited AsObject {almost certainly bomb}
end;

function TTypeCast.AsString: String;
begin
  if Operator = ttString then
  begin
   {$IFDEF HANDLEMISSING}
    if MissingParam then
    begin
        result:=NA_STR;
        exit;
    end;
    {$ENDIF }
  Result:= Trim(Param[0].AsString);
  end else
  begin
    Result:= Trim(inherited AsString)
  end
end;

function TTypeCast.AsFloat: extended;
var
  Code: Integer;
  s: String;
begin
  {$IFDEF HANDLEMISSING}
    result:=NA_FLOAT;
  {$ENDIF }
  if Operator = ttFloat then
  begin
    case OperandType of
      ttString:
      begin
         s:=Sysutils.trim(Param[0].AsString);
        {$IFDEF HANDLEMISSING}
         if s='' then
          begin
             result:=NA_FLOAT;
             exit;
          end;
         {$ENDIF }
         Val(s, Result, Code);
         if Code <> 0 then exit;
//           Error(format('Cannot convert %s to float', [s]))
      end;
    else
    begin
    {$IFDEF HANDLEMISSING}
      if MissingParam then
      begin
          result:=NA_FLOAT;
          exit;
      end;
      {$ENDIF }
      Result:= Param[0].AsFloat
    end
    end
  end else
  begin
    Result:= inherited AsFloat
  end
end;

function TTypeCast.AsInteger: Integer;
var
  Code: Integer;
  s: String;
begin
{$IFDEF HANDLEMISSING}
     result:=NA_INT;
 {$ENDIF }
  if Operator = ttInteger then
  begin
    case OperandType of
      ttString:
      begin
         s:=Sysutils.trim(Param[0].AsString);
        {$IFDEF HANDLEMISSING}
         if s='' then
          begin
             result:=NA_INT;
             exit;
          end;
         {$ENDIF }
         Val(s, Result, Code);
         if Code <> 0 then exit;
//           Error(format('Cannot convert %s to integer', [s]))
      end;
      ttFloat:
      begin
        {$IFDEF HANDLEMISSING}
        if MissingParam then
        begin
            result:=NA_INT;
            exit;
        end;
        {$ENDIF }
        Result:= Trunc(Param[0].AsFloat);
      end
    else
      Result:= Param[0].AsInteger
    end
  end else
  begin
    Result:= inherited AsInteger
  end
end;

function TTypeCast.AsBoolean: Boolean;
var
  s: String;
const
  Eps30 = 1e-30;
begin
  if Operator = ttBoolean then
  begin
    case OperandType of
      ttString:
      begin
         s:= AnsiUppercase(Param[0].AsString);
         if s =  NBoolean[false] then
           Result:= False
         else
         if s = NBoolean[true] then
           Result:= True
         else
//           Result:=0;
           Error(format('Cannot convert %s to Boolean', [s]))
      end;
      ttFloat:
        Result:= Abs(Param[0].AsFloat) > Eps30;
      ttInteger:
        Result:= Param[0].AsInteger <> 0
    else
      Result:= Param[0].AsBoolean;
    end
  end else
  begin
    Result:= inherited AsBoolean
  end
end;

function TTypeCast.ExprType: TExprType;
begin
  Result:= Operator
end;

constructor TTypeCast.Create( aParameterList: TParameterList;
                              aOperator: TExprType);
begin
  if aOperator = ttEnumerated then
    error('Cannot cast to enumerated');
  if aParameterList.Count = 1 then
    OperandType:= aParameterList.Param[0].ExprType
  else
    error('Invalid parameters to typecast');
  {allow futile cast Object(ObjVar) }
  if (aOperator = ttObject) and
     (OperandType <> ttObject) then
    IncompatibleTypes(aOperator, OperandType);

  {objects may be cast to string or object only
   casting to string helplessly returns class name}
  if (OperandType = ttObject) and
     not ((aOperator = ttObject) or
          (aOperator = ttString)) then
      IncompatibleTypes(aOperator, OperandType);
  inherited Create(aParameterList);
  Operator:= aOperator
end;

function TTypeCast.TestParameters: Boolean;
begin
  Result:= ParameterCount = 1
end;

function TConditional.Rex: IValue;
begin
  if Param[0].AsBoolean then
    Result:= Param[1] else
    Result:= Param[2]
end;

constructor TConditional.Create( aParameterList: TParameterList);
begin
  inherited Create(aParameterList);
  CommonType:= Param[1].ExprType
end;

function TConditional.TestParameters: Boolean;
begin
  if not (ParameterCount = 3) then
    error('IF must have 3 parameters');
  if not (Param[0].ExprType = ttBoolean) then
    error('First parameter to If must be Boolean');
  if not (Param[1].ExprType = Param[2].ExprType) then
    error('IF options must be the same type');
  Result:= true
end;

function TConditional.AsString: String;
begin
  Result:= Rex.AsString
end;

function TConditional.AsFloat: extended;
begin
  Result:= Rex.AsFloat
end;

function TConditional.AsInteger: Integer;
begin
  Result:= Rex.AsInteger
end;

function TConditional.AsBoolean: Boolean;
begin
  Result:= Rex.AsBoolean
end;

function TConditional.ExprType: TExprType;
begin
  Result:= CommonType
end;


function TExpression.GetObject: TObject;
begin
  result := self;
end;

function TExpression.TypeInfo: PTypeInfo;
begin
  case ExprType of
    ttString: Result:= System.TypeInfo(String);
    ttFloat: Result:= System.TypeInfo(extended);
    ttInteger: Result:= System.TypeInfo(Integer);
    ttBoolean: Result:= System.TypeInfo(Boolean);
  else
    Error(format('Cannot provide TypeInfo for %s', [ClassName]))
  end
end;

function TEnumeratedLiteral.TypeInfo: PTypeInfo;
begin
  Result:= Rtti
end;

function TObjectRef.TypeInfo: PTypeInfo;
begin
  if Assigned(FObject) then
    Result:= FObject.ClassInfo
  else
    Result:= TObject.ClassInfo
end;

//function FoldConstant( Value: IValue): IValue;
  {replace complex constant expression with literal, hence reducing
   evaluation time. This function does not release Value - caller
   should do that if it is appropriate. This is usually, but not
   necessarily always, the case.}
//
function FoldConstant( Value: IValue): IValue;
begin
  if Assigned(Value) then
  case Value.ExprType of
    ttObject: Result:= TObjectRef.Create(Value.AsObject);
    ttString: Result:= TStringLiteral.Create(Value.AsString);
    ttFloat: Result:= TFloatLiteral.Create(Value.AsFloat);
    ttInteger: Result:= TIntegerLiteral.Create(Value.AsInteger);
    ttEnumerated: Result:= TEnumeratedLiteral.Create(Value.TypeInfo, Value.AsInteger);
    ttBoolean: Result:= TBooleanLiteral.Create(Value.AsBoolean);
  else
    Result:= nil
  end
end;

function TObjectProperty.TypeInfo: PTypeInfo;
begin
  Result:= PropInfo.PropType^
end;



const
   ParamDelimiter:TSMTokenType=opComma ;
//   DecSeparator = '.';

{note: These two cannot be the same}
(*
  Whitespace = [#$1..#$20];
  SignChars = ['+', '-'];
  RelationalChars = ['<', '>', '='];
  OpChars = SignChars + ['^', '/', '*'] + RelationalChars;

  OpenSub = '(';
  CloseSub = ')';
  SQuote = '''';
  {mst}
  SHex = '$';
  HexDigs = Digits+['a'..'f','A'..'F'];
  {mst}
*)

function TSMParser.IsOperator(operator:TSMTokenType):boolean;
begin
 result:= Tokenizer.Isoperator(operator);//operator in SMOperators;
end;

procedure TSMParser.ErrorFmt(Token:TSMTOken;const ErrorMsg:string;Args:array of const);
var
  s : string;
begin
  s :=format(ErrorMsg,Args) ;
  Error(Token,s);
end;


class procedure TSMParser.Error(Token:TSMToken;const ErrorMsg:string='Unknown Error');
var
 handled :boolean;
begin
 handled := false;
 if assigned(fOnError) then
     fOnError(format('%s' ,[ErrorMsg]),Token,handled);
 if not handled then
   raise EExpression.Create(format('%s' + #13#10+ 'at line %d',[ErrorMsg,Token.line]));
end;

function TSMParser.GetOperator( Tokenizer:TSMBaseTokenizer; var Operator: TSMTokenType): Boolean;
begin
 operator:=opInvalidToken;
 Result:=Tokenizer.CurrentToken.TokenGroup=opOperator;
 if result then
   operator := Tokenizer.CurrentToken.TokenType;
end;

function TSMParser.Chain(Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction;
                   NextFunc: TExprFunc; Ops: TSMTokenTypeSet): IValue;
var
  NextOpr: TSMTokenType;
  StopF: Boolean;
begin
  StopF:= false;
  Result:= NextFunc(Tokenizer, IDF);
  try
    repeat
      if not Tokenizer.EndOfExp and GetOperator(Tokenizer, NextOpr) and (NextOpr in Ops) then
      begin
        if NextOpr in RelationalOperators then
          Result:= TRelationalOp.Create(NextOpr, Result, NextFunc(Tokenizer, IDF))
        else
        if NextOpr in [opAnd,opOr] then
          Result:= TBinaryOp.Create(NextOpr, Result, Expression(Tokenizer, IDF))
        else
          Result:= TBinaryOp.Create(NextOpr, Result, NextFunc(Tokenizer, IDF));
      end else
      begin
        StopF:= true
      end
    until StopF
  except
    Result:= nil;
    raise
  end
end;


function TSMParser.SimpleFactor(Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction): IValue;
var
  Identifier: String;
  aToken :TSMToken;
begin
  Result:= nil;
   try
    aToken:=Tokenizer.NextToken;
    case aToken.TokenType of
    opCloseParen:exit;
    opPlus: Result:= TUnaryOp.Create(opPlus, SimpleFactor(Tokenizer, IDF));
    opMinus: Result:= TUnaryOp.Create(opMinus, SimpleFactor(Tokenizer, IDF));
    opNot: Result:= TUnaryOp.Create(opNot, SimpleFactor(Tokenizer, IDF));
    opPeriod: Result:=TMissingLiteral.Create;
    opString: Result:= CharacterString(aToken);
    opHexNumber,opNumber:Result:= UnsignedNumber(aToken);
    opOpenParen:
     begin
        Result:= Expression(Tokenizer, IDF);
        if Result = nil then
          error(aToken,'invalid sub-expression');
        if Tokenizer.CurrentToken.TokenType <> opCloseParen then
          error(aToken,' ) expected')
      end;
      
    opIdentifier:
        result:=HandleIdentifiers(Tokenizer,IDF);
    else
    if Tokenizer.EndOfExp then
    begin
      if not IgnoreError then
          error(aToken,'Unexpected end of Expression')
    end
    else
        error(aToken,'Syntax error') {leak here ?}
    end
  except
    Result:= nil;
    raise
  end
end;  {Simplefactor}



function TSMParser.ObjectProperty( Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction): IValue;
var
  PropName: String;
  aToken :TSMToken;
begin
  Result:= SimpleFactor(Tokenizer, IDF);
  aToken:=Tokenizer.NextToken;
  while (result<> nil) and(Result.ExprType = ttObject) and (aToken.TokenType =opPeriod) do
  begin
    aToken:=Tokenizer.NextToken;
    if aToken.TokenType=opIdentifier then
    begin
      Result:= TObjectProperty.Create(Result, aToken.Token);
      aToken:=Tokenizer.NextToken;
    end
    else
    begin
      ErrorFmt(aToken,'Invalid property of object %s', [Result.AsObject.ClassName])
    end
  end;
end;

function TSMParser.Factor( Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction): IValue;
begin
  Result:= Chain(Tokenizer, IDF, ObjectProperty, [opExp])
end;


function TSMParser.Term(Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction): IValue;
begin
  Result:= Chain(Tokenizer, IDF, Factor, [opMult, opDivide, opDiv, opMod, opShl, opShr])
end;

function TSMParser.Simple( Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction): IValue;
begin
  Result:= Chain(Tokenizer, IDF, Term, [opPlus, opMinus, opXor])
end;

function TSMParser.Expression( Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction): IValue;
begin
  Result:= Chain(Tokenizer, IDF, Simple, RelationalOperators+ [opAnd, opOr])
end;



function TSMParser.StandardFunctions (Ident: TSMToken; PL: TParameterList): IValue;
begin
  if (Ident.TokenGroup = opKeyword) then
  case Ident.TokenSubType  of
  opIIF:  Result:= TConditional.Create(PL);
  opStringCast.. opBooleanCast: Result:= TTypeCast.Create(PL,TokenToExprType(Ident.TokenSubType));
  opTrunc.. opPower: Result:= TMathExpression.Create(PL,Ident.TokenSubType );
  opUpper..opFindFile: Result:= TStringExpression.Create(PL,Ident.TokenSubType);
//To be done change to Void expression
//  opLog:Result:= TStringExpression.Create(PL,Ident.TokenSubType );
  else
     Result:= nil
  end;
end;

function TSMParser.HandleIdentifiers(Tokenizer:TSMBaseTokenizer; IDF: TIdentifierFunction):IValue;
var
   IdenToken,aToken:TSMToken;
   PList : TParameterList;
   MoreParameters:boolean;
   BracketParameters, Fail:boolean;
   TempError: TOnParseError;
   exp :IValue;
begin
     Result:=nil;
     IdenToken := Tokenizer.CurrentToken;
//here all subtype =opidentifier
     case IdenToken.TokenSubType of
      opTrue:  Result:= TBooleanLiteral.Create(true);
      opFalse:  Result:= TBooleanLiteral.Create(false);
     else
     begin
       PList:= nil;
       try
         aToken:=Tokenizer.LookAheadToken;
          try
           MoreParameters:= aToken.TokenType=opOpenParen;
           BracketParameters:= aToken.TokenType=opOpenBracket;
           if MoreParameters then
           begin
              PList:= TParameterList.Create;
              aToken:=Tokenizer.NextToken;
              while MoreParameters do
              begin
                exp:=Expression(Tokenizer, IDF);
                if exp<> nil then
                   PList.AddExpression(exp);
                aToken:=Tokenizer.CurrentToken;
                MoreParameters:=(aToken.TokenType = ParamDelimiter);
              end;//while
              MoreParameters := true;
              if (exp<>nil) and (aToken.TokenType<>opCloseParen) then
                error(Atoken,'Incorrectly formed or empty parameters')
            end;
            if BracketParameters then
            begin
              PList:= TParameterList.Create;
              aToken:=Tokenizer.NextToken;
              exp:=Expression(Tokenizer, IDF);
              if (exp = nil) or (not exp.CanReadAs(ttInteger)) then
                error(Atoken, 'Empty parameter or not an integer expression');
              PList.AddExpression(exp);
              aToken:=Tokenizer.CurrentToken;
              if (aToken.TokenType<>opCloseBracket) then
                error(Atoken,'Incorrectly formed parameters')
            end;
            Result:= StandardFunctions(IdenToken, PList);
            if (Result = nil) and Assigned(IDF) then
              Result:= IDF(IdenToken.Token, PList);
            if Result = nil then
              error(IdenToken, format('Unknown Identifier %s', [IdenToken.Token]))
            else
            if not Result.TestParameters then
               error(IdenToken, format('Invalid parameters to %s', [IdenToken.Token]));
            try
              fail := false;
              TempError := fOnError;
              fOnError := nil;
              Fail := (not (Result.GetObject is TVectorVar) and BracketParameters)
                       or ((Result.GetObject is TVectorVar) and MoreParameters);
            except
              // Do nothing, just catch the exception!
            end;
            fOnError := TempError;
            if Fail then
              error(IdenToken, 'Vectors must be indexed with brackets "[]", expression/functions must use parantheses "()"');
       except
          raise;
       end;
       finally
            if Result = nil then
              PList.Free
        end;//try finally end
       end;//else case
     end//case
end;//begin


function TSMParser.UnsignedNumber(aToken:TSMToken): IValue;
const
  Digits = ['0'..'9'];
type
  TNScan = (nsMantissa, nsDPFound, nsExpFound, nsFound);
var
  S: String;
  State: TNScan;
  Int: Boolean;
  SaveSep: Char;
  p :pchar;
procedure Bomb;
begin
   error(aToken,'Not number or Bad numeric format')
end;

begin
  S:='';
  if (Length(aToken.Token) > 28) then
      Bomb;
  p:=pchar(aToken.Token);
  Int:= false;
  State:= nsMantissa;
  repeat
    if P^ in Digits then
    begin
      S:= S + P^;
      inc(P)
    end else
    if P^ = EpiDecSeparator then
    begin
      if State = nsMantissa then
      begin
        S:= S + P^;
        inc(P);
        State:= nsDPFound
      end else
      begin
        Bomb
      end;
    end else
    if (P^ = 'e') or (P^ = 'E') then
    begin
      if (State = nsMantissa) or
         (State = nsDPFound) then
      begin
        S:= S + 'E';
        inc(P);
        if P^ = '-' then
        begin
          S:= S + P^;
          inc(P)
        end;
        State:= nsExpFound;
        if not (P^ in Digits) then
          Bomb
      end else
      begin
        Bomb
      end
    end else
    begin
      Int:= (State = nsMantissa);
      State:= nsFound
    end;
  until State = nsFound;
  if Int then
  begin
  try
    Result:= TIntegerLiteral.Create(StrToInt(S))
  except
    ErrorFmt(aToken,'Integer %s is invalid or exceeds maximum integer size',[S]);
  end;
  end else
  begin
      SaveSep:=SysUtils.DecimalSeparator;   //MIB 17mar04
      SysUtils.DecimalSeparator:=EpiDecSeparator;  //MIB 17mar04
      try    //MIB 17mar04
        Result:= TFloatLiteral.Create(StrToFloat(S));
      finally             //MIB 17mar04
        SysUtils.DecimalSeparator:=SaveSep;   //MIB 17mar04
      end;    //MIB 17mar04
  end
end;

{#Todo3  check validity of passing empty string ""}
function TSMParser.CharacterString(aToken:TSMToken): IValue;
var
  SR: String;
  n  :integer;
  p : pchar;
  dt  : Epidate;
begin
  P:=pchar(aToken.Token);
  SR:=StrUnquote(p);
//  n:=;
  if Length(SR) > MaxStringLength then
      ErrorFmt(aToken,'Too long string %s',[SR]);
  Result:= TStringLiteral.Create(SR)
end;


{ TSMParser }

constructor TSMParser.Create(IDF: TIdentifierFunction);
begin
   inherited Create;
   fIdentifierFunction:=IDF;
end;

constructor TSMParser.CreateToken(pTokenizer: TSMBaseTokenizer;IDF: TIdentifierFunction);
begin
   AssignTokenizer(pTokenizer);
   Create(IDF);
end;

constructor TSMParser.CreateStreamParser(aStream: TStream;
  IDF: TIdentifierFunction);
begin
   AssignTokenizer(TSMBaseTokenizer.createStream(aStream));
   Create(IDF);
end;

constructor TSMParser.CreateStringParser(const parseString: string;
  IDF: TIdentifierFunction);
begin
   AssignTokenizer(TSMBaseTokenizer.createString(parseString));
   Create(IDF);
end;

constructor TSMParser.CreateFileParser(const fileName: string;
  IDF: TIdentifierFunction);
begin
   AssignTokenizer(TSMBaseTokenizer.createFile(FileName));
   Create(IDF);
end;

destructor TSMParser.Destroy;
begin
  fTokenizer.free;
end;


function TSMParser.GenExpression(const parseString: string;IDF:TIdentifierFunction=nil): IValue;
begin
  Result:=nil;
  if fTokenizer<> nil then
    fTokenizer.free;
  fTokenizer :=TSMExpTokenizer.createString(parseString);
  if not assigned(IDF) then
     IDF:= fIdentifierFunction;
  Result:= Expression(fTokenizer,IDF);
end;


function TSMParser.Accept(tokens:TSMTokenTypeSet;const ErrorMsg:string='' ):TSMToken;
var
  s ,s1: string;
  i : TSMTokenType;
begin
  result:=NextToken;
  if (result.TokenType =opEndofFile) and not(opEndofFile in tokens) then
    Error(result,SMUnExpectedEOF);
  if (result.TokenType =opEndofLine) and not(opEndofLine in tokens) then
    Error(result,SMUnExpectedEOL);
  s:='';
  if not (result.TokenType in tokens) then
  begin
    for i:=low(TSMTokenType) to high(TSMTokenType) do
       if i in Tokens then
       begin
           s1:=Tokenizer.TokenName[i];
           if s1<>'' then s := s1 +',' + s;
       end;
    if s='' then Error(Result);
    setlength(s,length(s)-1);
    errorfmt(result,SMExpected,[s, Tokenizer.TokenName[result.TokenType]]);
  end;
end;


function TSMParser.NextToken :TSMToken;
begin
// PrevToken:=Token;
 Tokenizer.NextToken;
 while CurrentToken.TokenType = opComment do
 begin
    Tokenizer.NextToken;
    Tokenizer.NextToken;
 end;
 result:=CurrentToken;
end;


function TSMParser.ReadToChar(const tok: char=#32):String;
begin
  result:='';
  if lookaheadToken.TokenType =opString then
  begin
     result:=Nexttoken.token;
     exit;
  end;
  result:=lookaheadToken.token;
  result:=result+Tokenizer.ReadToChar(tok).token;
//  NextToken; //read till you get tok
end;

{function TSMParser.ReadToToken1(const tok: string):string;
begin
  result:='';
  if lookaheadToken.TokenType =opString then
     result:=Nexttoken.token
  else
  begin
    result:=lookaheadToken.token;
    result:=result+Tokenizer.ReadToEol.token; //read till you get EOL
  end;
end;
}
function TSMParser.ReadToEOL:string;
begin
  result:=lookaheadToken.Token;
  if lookaheadToken.TokenType =opString then
     result:='"'+result+'"';
  // Why a space between results??? Old style commented out Torsten - 16. nov. 2006
//  result:=result+' '+ Tokenizer.ReadToEol.token;
  result:=result+Tokenizer.ReadToEol.token;
//  Token
end;

function TSMParser.GetRestOfLine(jumpEOL:boolean):string;
var
 p : integer;
 tok : string;
begin
  Result:=Tokenizer.GetRestOfLine;
  Tok:=CurrentToken.token;
  if jumpEOL then  Tokenizer.ReadToEol;
  if result='' then exit;
  p:=Ansipos(AnsiUppercase(Tok), AnsiUppercase(Result));
  if p=0 then exit;
  Result:=copy(Result,p+length(Tok)+1,length(Result));
end;



function CreateExpression( const S: String;IdentifierFunction: TIdentifierFunction): IValue;
begin
   if not assigned(parser) then
      parser := TSMParser.CreateStringParser(s,IdentifierFunction{,nil});
   Result:= parser.GenExpression(s, IdentifierFunction);
end;


procedure TSMParser.AssignTokenizer(pTokenizer: TSMBaseTokenizer);
begin
    fTokenizer :=pTokenizer;
    fTokenizer.OnError :=self.TokenErrHandler;
end;

procedure TSMParser.TokenErrHandler(const msg: string; Token: TSMToken;
  var Handled: boolean);
begin
  handled :=true;
  Error(token,msg);
end;

function TSMParser.GetLookAheadToken: TSMToken;
begin
  result:= Tokenizer.LookAheadToken;
end;

function TSMParser.GetPrevToken: TSMToken;
begin
  result:= Tokenizer.PrevToken;
end;

function TSMParser.GetCurrentToken: TSMToken;
begin
  result:= Tokenizer.CurrentToken;
end;

procedure TSMParser.SetCurrentToken(const Value: TSMToken);
begin
  Tokenizer.CurrentToken:=Value;
end;


class function TSMParser.GenBooleanExpression(const parseString: string;
  IDF: TIdentifierFunction): IValue;
var
 Token: TSMToken;
begin
 Token.Token := parseString;
 try
   Result:=CreateExpression(ParseString,IDF);
 except
   on E : Exception do
       error(parser.Currenttoken,E.message);
 end;
  if result.CanReadAs(ttBoolean) then exit;
    error(parser.Currenttoken,'Not a boolean expression');
end;

class function TSMParser.Genxpression(const parseString: string;
  IDF: TIdentifierFunction): IValue;
var
 Token: TSMToken;
begin
 Token.Token := parseString;
 try
   Result:=CreateExpression(ParseString,IDF);
 except
   on E : Exception do
       error(parser.Currenttoken,E.message);
 end;
end;


function TSMParser.GetOnError: TOnParseError;
begin
  result:=fOnError;
end;

procedure TSMParser.SetOnError(const Value: TOnParseError);
begin
  fOnError:=value;
end;


procedure TSMParser.SetIgnoreError(const Value: boolean);
begin
  FIgnoreError := Value;
end;


initialization

parser :=nil;

finalization
 if assigned(parser) then
   parser.free;

end.









