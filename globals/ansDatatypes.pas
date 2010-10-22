unit ansDatatypes;

interface
 uses sysutils, windows;

resourcestring
 EpiUNDEFINED        = '??????';
 MHWORD       = 'Mantel-Haenszel';
 CRUDEWORD    = 'Crude OR';
 PVWORD       = 'P value';
 EXACT     = 'Fisher exact';
 ORWord    = 'Odds ratio';


 EpiDuplicateName = 'An object named %s already exists';
 EpiVectorNameMissing = 'Field name missing';
 EpiVectorNotFound = 'Field ''%s'' not found';

 EpiDataFilter='Datafiles|*.rec;*.dbf;*.csv;*.txt;*.epx;*.epz|DBF|*.dbf|EpiData & Epi6 Data Files|*.rec|EpiData Project File XML|*.epx;*.epz|All|*.*';
 EpiProgramFilter='Program Files (*.pgm)|*.pgm|Text Files (*.txt)|*.txt |HTML Files (*.htm,*.html)|*.htm;*.html';
 EpiLogFileFilter='HTML Files (*.htm,*.html)|*.htm;*.html|Text Files (*.txt)|*.txt|Log files (*.log)|*.log';

Const
// two constants required for the xmatrix declaration below
// duplicated from RegComp.inc    - but should be deleted from regcomp.inc
  MAXVARIABLES      =     20;
  MAXOBSERVATIONS   =   5000;
  MAXDEFINEDMISSINGVALUES = 2;


type
 EEpiFile=class(Exception);
 EPIAnaException=Exception;

 EpiFloatStorage=extended;   //rettet MIB 14mar04
 EpiFloat=extended;
 EpiInt=integer;
 EpiUnInt=cardinal;
 EpiVariant=variant;
 EpiBool=boolean;
 EpiString=string;
 EpiByte =byte;
// EpiDate=Tdatetime; now defined in udateutils
 EpiDate = integer;

 TTestRecord = record
   FileName: string;
   Planned:  integer;
   Asserts:  integer;
   Errors:   integer;
   DigitPrecision: EpiFloat;
   Dif: EpiFloat;
   Comment:  string;
 end;

 TMissingValues = array[0..MAXDEFINEDMISSINGVALUES] of string;

 TTestRecordArray = array of TTestRecord;


 TEpiOutputType=(EpiOTHTML, EpiOTText);

 TEpiOption=class
 private
    fValue: string;
    fname: string;
    foType: word;
 public
  constructor Create(const pname,pvalue:string; ptype:word);
  property name :string read fname write fname;
  property oType: word read foType write foType;
  property Value:string read fValue write fValue;
 end;



 TAnaReCodeData=class
  private
    fvhigh: variant;
    fvcode: variant;
    fvlow: variant;
    fisElse: boolean;
    fOperation: string;
 public
  constructor Create(vlow, vhigh, vcode:variant;pOperation:string;isElse:boolean=false);
  property vlow :variant read fvlow write fvlow;
  property vhigh :variant read fvhigh write fvhigh;
  property vcode :variant read fvcode write fvcode;
  property isElse:boolean read fisElse write fisElse;
  property Operation:string read fOperation write fOperation;
 end;


 TAnaVariableDescriptor=class
  private
    fDecimals: integer;
    fDataType: integer;
    flength: integer;
    fformat: string;
    fName: string;
    fDefaultValue: variant;
    fVarScope: word;
 public
   constructor Create(const pName:string;pDataType,pLength,pDecimals:integer; pformat:string='');
   constructor CreateCum(const pName:string;pDataType,pLength,pDecimals:integer; pformat:string='');
   constructor CreateGlobal(const pName:string;pDataType,pLength,pDecimals:integer; pformat:string='');
   constructor CreateSystem(const pName:string;pDataType,pLength,pDecimals:integer; pformat:string='');
   constructor CreateLocal(const pName:string;pDataType,pLength,pDecimals:integer; pformat:string='');
   constructor CreateResult(const pName: string; pDataType, pLength, pDecimals: integer; pformat: string='');
   property Name : string read fName write fname;
   property Length  : integer read flength write flength;
   property Decimals : integer read fDecimals write fDecimals;
   property Format : string read fformat write fformat;
   property DataType : integer read fDataType write fDataType;
   property DefaultValue :variant read fDefaultValue write fDefaultValue;
   property VarScope: word read fVarScope write fVarScope;
 end;

const
//data types
 EpiTyUnknown=0;
 EpiTyBoolean=1;
 EpiTyInteger=2;
 EpiTyFloat=3;
 EpiTyDate=4;
 EpiTyString=5;
 EpiTyUppercase=6;
 EpiTyByte=7;
 EpiTyIgnore=98;
 EpiTyUnSupported=99;

 EpiVarStandard=0;
 EpiVarCum=1;
 EpiVarlocal=2;
 EpiVarGlobal=3;
 EpiVarPerm=4;
 EpiVarResult=5;
 EpiVarSystem=6;

 EpiTypeNameArray:array[EpiTyUnknown..EpiTyByte] of string =
   ('Unknown','Boolean','Integer','Float','Date','String', 'Uppercase', 'String');

 EpiTypeShortNameArray:array[EpiTyUnknown..EpiTyByte] of string =
   ('U','B','I','F','D','S','U','S');

 // TODO: Use max_extended as NA_FLOAT;
 NA_FLOAT   : EpiFloatStorage = 1E+38;
 EpiMissingChar='.';
 NA_STR = '.';
 NA_INT = 2147483647;
 NA_DATE = NA_INT;
 NA_BYTE = 255;
 NA_BOOL = NA_BYTE;


const
  DlgResCancel=0;
  DlgResReset=1;
  DlgResPaste=2;
  DlgResRun=3;



type
//  TExprType = (ttObject, ttString, ttFloat, ttInteger, ttEnumerated, ttBoolean);
 TEpiAdvOptions = set of (GrpSpcOpt, GrpStdOpt);

 TEpiDatatypes = set of byte;
 // (EpiTyBoolean, EpiTyInteger, EpiTyFloat, EpiTyDate, EpiTyString, EpiTyUppercase, EpiTyByte);

 TEpiFileName =string;

// EpiByteArrayS=array[0..0] of byte;
// EpiByteArraySP=^EpiByteArrayS;

// EpiCharArrayS=array[0..0] of char;
// EpiCharArraySP=^EpiCharArrayS;


 EpiByteArray=array of EpiByte;
 EpiIntArray=array of EpiInt;
 EpiFloatArray=array of EpiFloat;
 EpiStringArray=array of EpiString;
 EpiPCharArray=array of Pchar;

 TSortOptionsType=(SOCaseSensitive,SODescending);
 TSortOptions=set of TSortOptionsType;

type
 TVectorSortCompare = function (Item1, Item2: EpiUnInt): Integer of object;
 TVectorSortCompareFunc = function (Item1, Item2: EpiUnInt): Integer;

 TEpiDescriptives=record
    sum,SumSq,SumSS,mean,variance  : Epifloat;
    avgdev,stddev,stdvar,skew,curt : EpiFloat;
    max,min,med,p5,p10,p25,p75,p90,cfiL,cfiH,p95    : EpiFloat;
    n,nmn                          : EpiUnInt;
    Level                          : variant;
 end;

 TEpiDescriptivesArray= array of TEpiDescriptives;

 TEpiFreqDescriptive=record
    n, nmn              :EpiUnInt;
    Level: array[0..100] of char;
{    Level2             :string;
    Level3             :string;
    Level4             :string;
    Level5             :string;}
 end;

 TEpiAnovaRecord=record
  SSB,MSB,SSW,MSW,SST,MST,dfw,dfb,dft,f,prob: Epifloat
 end;

 TEpiFreqDescriptiveArray= array of TEpiFreqDescriptive;

//XVector        = array [1..MAXVARIABLES+1] of extended;
XVector        = array of extended;
//1 based matrix
XMatrix = array [1..MAXVARIABLES+1,1..MAXVARIABLES+1] of extended;

// A TMultStatRec used in MutlipleRegression function
TMultStatRec = record
    N,                  // Number of Obs.
    k        : integer; // Number of variables
    R,                  // Multiple Correlation
    R2,                 // Sqr(R)
    FStat,              // F-statistic
    RMSE,               // Root of MSE
    MSE,                // Mean Sqr for Error
    MSR,                // Mean Sqr for Regression
    SSE,                // Sum Sqrs of Error
    SSR,                // Sum Sqrs of Regression
    SST,                // Sum Sqrs Total
    S2,                 // Overall variance
    S       : extended; // Overall S..Sqrt(S2)
    Beta    : XVector;  // Vector for Beta params
    SeBeta  : XVector;  // Vector for StdErr of Beta's
    TValue  : XVector;  // Vector for TValues for Beta's
    DFModel : integer;  // Deg. Freedom for Model
    DFError : integer;  // Deg. Freedom for Error
    DFTotal : integer;  // Deg. Freedom Total
end;
procedure CheckfileError(ErrorNo:integer);

procedure EpiError(const msg:string;ErrorID:integer);
function StringToEpiType(const s: string):integer;
function ShortStringToEpiType(const s: string):integer;

function FormatOR(const s,fmt: string; val: Epifloat):string;

function GetFalseString: char;
function GetTrueString: char;
procedure SetFalseString(const Value: char);
procedure SetTrueString(const Value: char);

var
   BoolStr:array[boolean] of char;

implementation

uses UstringConst;

function GetFalseString: char;
begin
  Result:=BoolStr[false]
end;

function GetTrueString: char;
begin
  Result:=BoolStr[true]
end;

procedure SetFalseString(const Value: char);
begin
  BoolStr[false]:=value;
end;

procedure SetTrueString(const Value: char);
begin
   BoolStr[True]:=value;
end;



procedure EpiFileError(const Message: string);
begin
  raise EEpiFile.create(Message);
end;

procedure EpiFileErrorFmt(const Message: string; const Args: array of const);
begin
  raise EEpiFile.create(format(Message,Args));
end;


procedure CheckfileError(ErrorNo:integer);
var
 msg :string;
begin
  case Errorno of
   0: exit;
   -2: ;
   -13:EpiFileError('File has no fields')
   else
   begin
      Msg := SysErrorMessage(GetLastError);
      raise EEpiFile.create(Msg);
   end;
  end;
end;

function FormatOR(const s,fmt: string; val: Epifloat):string;
begin
  if (val <= -9999) then
      result :=format('%s %s',[s,Epiundefined])
  else
     result :=format('%s %s',[s,format(fmt,[val])])
end;

function ShortStringToEpiType(const s: string):integer;
var
 i, co: integer;
begin
  Result:=EpiTyUnknown;
  co := length(EpiTypeShortNameArray);
  // Start from 1, since index 0 = Unknowm type (using 'U' as short identifier -
  // just like uppercase).
  for i:= 1 to co-1 do
     if (AnsiUpperCase(s)= AnsiUpperCase(EpiTypeShortNameArray[i])) then
     begin
        result:=i;
        exit;
     end;
end;

function StringToEpiType(const s: string):integer;
var
 i, co: integer;
begin
  Result:=EpiTyUnknown;
  co := length(EpiTypeNameArray);
  for i:= 0 to co-1 do
     if (AnsiUpperCase(s)= AnsiUpperCase(EpiTypeNameArray[i])) then
     begin
        result:=i;
        exit;
     end;
end;

{ TAnaVariableDescriptor }

constructor TAnaVariableDescriptor.Create(const pName: string; pDataType,
  pLength, pDecimals:integer; pformat: string);
begin
  fName:=pName;
  fDataType:= pDataType;
  fLength :=pLength;
  fDecimals:=pDecimals;
  fFormat:=pformat;
end;




procedure EpiError(const msg:string;ErrorID:integer);
begin
  raise EPIAnaException.Create(msg);
end;



constructor TAnaVariableDescriptor.CreateCum(const pName: string;
  pDataType, pLength, pDecimals: integer; pformat: string);
begin
 Create(pName,pDataType, pLength, pDecimals,pformat);
 fVarScope:= EpiVarCum;
end;

constructor TAnaVariableDescriptor.CreateGlobal(const pName: string;
  pDataType, pLength, pDecimals: integer; pformat: string);
begin
 Create(pName,pDataType, pLength, pDecimals,pformat);
 fVarScope:= EpiVarGlobal;
end;

constructor TAnaVariableDescriptor.CreateSystem(const pName: string;
  pDataType, pLength, pDecimals: integer; pformat: string);
begin
 Create(pName,pDataType, pLength, pDecimals,pformat);
 fVarScope:= EpiVarSystem;
end;

constructor TAnaVariableDescriptor.CreateLocal(const pName: string;
  pDataType, pLength, pDecimals: integer; pformat: string);
begin
 Create(pName,pDataType, pLength, pDecimals,pformat);
 fVarScope:= EpiVarLocal;
end;

constructor TAnaVariableDescriptor.CreateResult(const pName: string;
  pDataType, pLength, pDecimals: integer; pformat: string);
begin
 Create(pName,pDataType, pLength, pDecimals,pformat);
 fVarScope:= EpiVarResult;
end;


{ TAnaReCodeData }

constructor TAnaReCodeData.Create(vlow, vhigh, vcode: variant;pOperation:string;isElse:boolean=false);
begin
   fvlow    :=  vlow;
   fvhigh   :=  vhigh;
   fvcode   :=  vcode;
   fisElse  := isElse;
   fOperation:=pOperation;
end;

{ TEpiOption }

constructor TEpiOption.Create(const pname, pvalue: string; ptype: word);
begin
 fname:=pName;
 fValue:=pValue;
 fOtype:=ptype;
end;

end.
