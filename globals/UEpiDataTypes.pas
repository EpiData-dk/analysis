unit UEpidataTypes;

interface

uses TypInfo, ansDatatypes, UCmdTypes;

const

  EpiDecSeparator='.';
  EpiDateSeparator='/';

  EpiRecNormal = $00000000;
  EpiRecDeleted  = $00000001;
  EpiRecVerified    = $00000002;

  EpiRecSelected = $00000000;
  EpiRecDeSelected = $00000001;

// EpiRecTempDeSelected  = $00000008;   -- not used

{
faDirectory = $00000010;
faArchive   = $00000020;
faAnyFile   = $0000003F;
}

                  // ROW         // COLUMN
SmallPercentileMatrix: Array[1..19] of Array[1..11] of Integer =
//   N =      P1, P2.5, P5, P10, P25, P50, P75, P90, P95, P97.5, P99
     {1}    ((1,  1,    1,  1,   1,   1,   1,   1,   1,   1,     1),
     {2}     (1,  1,    1,  1,   1,   1,   2,   2,   2,   2,     2),
     {3}     (1,  1,    1,  1,   1,   2,   3,   3,   3,   3,     3),
     {4}     (1,  1,    1,  1,   1,   2,   3,   4,   4,   4,     4),
     {5}     (1,  1,    1,  1,   1,   1,   3,   4,   5,   5,     5),   // OBS: P50
     {6}     (1,  1,    1,  1,   1,   3,   5,   6,   6,   6,     6),
     {7}     (1,  1,    1,  1,   2,   4,   6,   7,   7,   7,     7),
     {8}     (1,  1,    1,  1,   2,   4,   6,   8,   8,   8,     8),
     {9}     (1,  1,    1,  1,   2,   5,   7,   9,   9,   9,     9),
    {10}     (1,  1,    1,  1,   2,   5,   8,   9,  10,  10,    10),
    {11}     (1,  1,    1,  1,   3,   6,   9,  10,  11,  11,    11),
    {12}     (1,  1,    1,  1,   3,   6,   9,  11,  12,  12,    12),
    {13}     (1,  1,    1,  1,   3,   7,  10,  12,  13,  13,    13),
    {14}     (1,  1,    1,  1,   3,   7,  11,  13,  14,  14,    14),
    {15}     (1,  1,    1,  1,   4,   8,  12,  14,  15,  15,    15),
    {16}     (1,  1,    1,  1,   4,   8,  12,  15,  16,  16,    16),
    {17}     (1,  1,    1,  1,   5,   9,  13,  16,  17,  17,    17),
    {18}     (1,  1,    1,  1,   4,   9,  14,  17,  18,  18,    18),   // OBS: P25
    {19}     (1,  1,    1,  2,   5,  10,  15,  18,  19,  19,    19));




  NumChars:        Set of CHAR=['0'..'9'];
  AlfaNumChars:    Set of CHAR=['0'..'9','A'..'Z','a'..'z'];
  AlfaChars:       Set of CHAR=['A'..'Z','a'..'z'];
  IdentChars:      Set of CHAR=['0'..'9','A'..'Z','a'..'z','_'];
  IntegerChars:    Set of CHAR=['0'..'9','-','+'];
  FloatChars:      Set of CHAR=['0'..'9', '.', ',', '-', '+'];
  DateChars:       Set of CHAR=['0'..'9','/'];
  BooleanYesChars: Set of CHAR=['y','Y','T','t','1'];
  BooleanNoChars:  Set of CHAR=['n','N','F','f','0'];
  BooleanChars :   Set of CHAR=['y','Y','T','t','1','n','N','F','f','0'];

  EpiMaxFldNameLength=10;
  EpiMaxIntegerLength=18;
  EpiMaxFloatLength=18;
  EpiMaxStringLength=80;

type

  TEntryType   = (eftNumeric, eftAlpha,    eftDate, eftUppercase, eftCheckBox,  eftYesNo,
                  eftRealNum, eftPhoneNum, eftTime, eftLocalNum,  eftTodayType, eftEuroDate,
                  eftIDNum,   eftRes4,     eftRes5, eftQuestion,  eftEuroToday, eftSoundex,
                  eftCrypt,   eftYMDDate,  eftYMDToday);

  TMissingAction=(maIgnoreMissing,maRejectMissing);

  TOnShowOutput = procedure(const output: string) of object;
  TOnOpenFile = procedure(const aFileName:string; CreateNew, ReadOnly: boolean) of object;

  TExprType = (ttObject, ttString, ttFloat, ttInteger, ttEnumerated, ttBoolean, ttMissing, ttDate);

  IValue = interface(IUnknown)
    function GetObject: TObject;
    function TestParameters: Boolean;   {after parsing may either return false or raise EExpression. This call is used internally}
    function CanReadAs(aType: TExprType): Boolean;
    function TypeInfo: PTypeInfo;
    function AsString: String;
    function AsFloat: extended;
    function AsInteger: Integer;
    function AsBoolean: Boolean;
    function AsObject: TObject;
    function AsIValue: IValue;
    function ExprType: TExprType;
    function TypeName: String;
    function IsMissing2: Boolean;
    function IsMissingValue: Boolean;
    function GetVarName :string;
    Procedure SetVarName(const aName:string);
    function GetValue: Variant;
    Procedure SetTag(value:integer);
    function GetTag: integer;
    Procedure SetValue(const value:Variant);
    property VarName :string read GetVarName write SetVarName;
    property Value :Variant read GetValue write setValue;
    property Tag :integer read GetTag write setTag;
  end;

function EpiFieldTypeToParserDataType(ftype: integer): TExprType;
function EpiParserDataTypeToFieldType(ftype: TExprType):integer ;
function EntryTypeToFieldType(ET: TEntryType): word;
function EntryTypeToFelttyper(et: TEntryType): TFelttyper;
function GetEpiInfoTypeName(ET: TEntryType):string;
function GetFieldTypeName(typ: word):string;
function EpiDataFieldType2VectorType(ft: TFelttyper):Word;
function VectorType2EpiDataFieldType(vt:word):TFelttyper;

function IsValidEpiIdent(const Ident: string): Boolean;

implementation

function SMGetEnumName(aRtti: pointer; aVal: integer):string;
begin
  Result:= GetEnumName(PTypeInfo(aRtti), aVal);
end;


function EpiFieldTypeToParserDataType(ftype: integer): TExprType;
begin
//TExprType = (ttObject, ttString, ttFloat, ttInteger, ttEnumerated, ttBoolean,ttMissing,ttDate);
  case ftype of
     EpiTyUnknown,EpiTyUnSupported:result:=ttMissing;
     EpiTyBoolean  :result:= ttBoolean;
     EpiTyInteger  :result:= ttInteger;
     EpiTyFloat    :result:= ttFloat;
     EpiTyDate     :result:= ttDate;
     EpiTyUppercase,
     EpiTyString   :result:= ttString;
 end;
end;


function EntryTypeToFelttyper(et: TEntryType): TFelttyper;
begin
  case et of
    eftNumeric:   result:=ftInteger;
    eftAlpha:     result:=ftAlfa;
    eftDate:      result:=ftDate;
    eftUppercase: result:=ftUpperAlfa;
    eftCheckBox:  result:=ftCheckBox;
    eftYesNo:     result:=ftBoolean;
    eftRealNum:   result:=ftFloat;
    eftTodayType: result:=ftToday;
    eftEuroDate:  result:=ftEuroDate;
    eftIDNum:     result:=ftIDNUM;
    eftQuestion:  result:=ftQuestion;
    eftEuroToday: result:=ftEuroToday;
    eftSoundex:   result:=ftSoundex;
    eftCrypt:     result:=ftCrypt;
    eftYMDDate:   result:=ftYMDDate;
    eftYMDToday:  result:=ftYMDToday;
  end;
end;



function EpiParserDataTypeToFieldType(ftype: TExprType):integer ;
begin
  case ftype of
     ttMissing  : result := EpiTyUnknown;
     ttBoolean  : result := EpiTyBoolean;
     ttInteger  : result := EpiTyInteger;
     ttFloat    : result := EpiTyFloat;
     ttDate     : result := EpiTyDate;
     ttString   : result := EpiTyString;
 end;
end;



function EntryTypeToFieldType(ET: TEntryType): word;
begin
case ET of
  eftAlpha,eftSoundex,
  eftPhoneNum,eftLocalNum,
  eftCrypt,eftTime                            : result := EpiTyString;
  eftUppercase                                : result := EpiTyUppercase;
  eftNumeric,eftIDNum                         : result := EpiTyInteger;
  eftDate,eftTodayType,eftEuroDate,
  eftEuroToday,eftYMDDate,eftYMDToday         : result := EpiTyDate;
  eftCheckBox, eftYesNo                       : result := EpiTyBoolean;
  eftRealNum                                  : result := EpiTyFloat;
  eftRes4, eftRes5                            : result:=  EpiTyUnSupported;
  eftQuestion                                 : result:=  EpiTyIgnore
  else result:= EpiTyUnknown;
end;
end;

function EpiDataFieldType2VectorType(ft: TFelttyper):Word;
begin
  case ft of
    ftAlfa,ftSoundex,ftPhoneNum,ftLocalNum,
    ftCrypt:                                  result:=EpiTyString;
    ftUpperAlfa:                              result:=EpiTyUppercase;
    ftInteger,ftIDNUM:                        result:=EpiTyInteger;
    ftDate,ftToday,ftEuroDate,
    ftEuroToday,ftYMDDate,ftYMDToday:         result:=EpiTyDate;
    ftBoolean:                                result:=EpiTyBoolean;
    ftFloat:                                  result:=EpiTyFloat;
    ftRes4,ftRes5:                            result:=EpiTyUnSupported;
    ftQuestion:                               result:=EpiTyIgnore;
  else
                                              result:=EpiTyUnknown;
  end;
end;

function VectorType2EpiDataFieldType(vt:word):TFelttyper;
begin
  case vt of
    EpiTyString: result:=ftAlfa;
    EpiTyUppercase: result:=ftUpperAlfa;
    EpiTyInteger:   result:=ftInteger;
    EpiTyDate:      result:=ftEuroDate;
    EpiTyBoolean:   result:=ftBoolean;
    EpiTyFloat:     result:=ftFloat;
    EpiTyUnSupported: result:=ftRes4;
    EpiTyIgnore:    result:=ftQuestion;
  end;
end;


function GetEpiInfoTypeName(ET: TEntryType):string;
begin
  Result:=SMGetEnumName(TypeInfo(TEntryType),ord(ET));
end;

function GetFieldTypeName(typ: word):string;
begin
  Result:=EpiTypeNameArray[typ];
end;


function IsValidEpiIdent(const Ident: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or (Length(Ident) > EpiMaxFldNameLength) or not (Ident[1] in AlfaChars) then Exit;
  for I := 2 to Length(Ident) do if not (Ident[I] in IdentChars) then Exit;
  Result := True;
end;

end.
