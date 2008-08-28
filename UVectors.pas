unit UVectors;
{defines IEpiVector and IEpiData interfaces}

//{$DEFINE recstatus}

interface
  uses sysutils, windows,classes,db,Uframes,ansDataTypes,Uepifile,UEpiDataTypes,SMutils,
   uDateutils,SGrep, uformats, UCheckProps, UOutPut;

const
  DefaultGrowthFactor:integer=100;

Type

  TEpiVector = class;

  TEpiRelatedVectors=record
    aV,bV : TEpiVector;
    flag  : integer;
  end;
  TEpiRelatedVectorsArray = array of TEpiRelatedVectors;


  TEpiDataFrame = class;
  TEpiVectors = class;

  TEpiVector = class
  private
    fName :String;
    fOrgLength: EpiInt;
    fCapacity : EpiUnint;
    fIsSorted : epibool;
    fVectors: TEpiVectors;
    fFldDataType : Word;
    fFldDataSize : Word;
    fFldDataDecimals : Word;
    fFldDataFormat :string;
//    fInternal: boolean;
    fVariableLabel: string;
    fFormat :string;
    fReadOnly: Boolean;
    fRepeated: Boolean;
    fMustEnter: Boolean;
    fCheckProperties: TVectorChkProp;
    function GetIsSorted: epibool;
    function GetFieldDataType: EpiUnInt;
    procedure SetFieldDataType(const Value: EpiUnInt);
    function GetFieldDataDecimals: EpiUnInt;
    function GetFieldDataSize: EpiUnInt;
    procedure SetFieldDataDecimals(const Value: EpiUnInt);
    procedure SetFieldDataFormat(const Value: Epistring);virtual;
    procedure SetFieldDataSize(const Value: EpiUnInt);
    function GetCapacity: EpiUnInt;
    procedure SetCapacity(const Value: EpiUnInt);
    function  GetFormat: Epistring;
    procedure SetFormat(const Value: Epistring);
    function GetAsByte(const index: Integer): EpiByte;Virtual;
    procedure SetAsByte(const index: Integer; const Value: EpiByte);Virtual;
    function GetMemoryUsed: Integer;virtual;abstract;
    procedure InternalClone(dest: TEpiVector; NoData:boolean=false; SameDataframe: boolean = true);
  protected
    function GetMissingCount: EpiUnInt;virtual;abstract;
    function GetName: string;virtual;
    procedure SetName(const Value: string);virtual;
    function GetFieldDataFormat: Epistring;virtual;
    function GetVectorLength: EpiUnInt;virtual;abstract;
    procedure SetVectorLength(const Value: EpiUnInt);virtual;abstract;
    function GetDataType: EpiUnInt;virtual;
    function GetDataSize: EpiUnInt;virtual;
    function GetIsMissing(const index: Integer): EpiBool;virtual;abstract;
    procedure SetIsMissing(const index: Integer; const Value: EpiBool);virtual;abstract;
    function GetIsMissingValue(const index: Integer): EpiBool; virtual; abstract;
    function GetAsBoolean(const index: Integer): EpiBool;virtual; abstract;
    function GetAsDate(const index: Integer): EpiDate;virtual; abstract;
    function GetAsFloat(const index: Integer): EpiFloat;virtual; abstract;
    function GetAsInteger(const index: Integer): EpiInt;virtual; abstract;
    function GetAsString(const index: Integer): EpiString;virtual; abstract;
    function GetValue(const index: Integer): Epivariant;virtual;
    procedure SetAsBoolean(const index: Integer; const Value: EpiBool);virtual; abstract;
    procedure SetAsDate(const index: Integer; const Value: EpiDate);virtual; abstract;
    procedure SetAsFloat(const index: Integer; const Value: EpiFloat);virtual; abstract;
    procedure SetAsInteger(const index: Integer; const Value: EpiInt);virtual; abstract;
    procedure SetAsString(const index: Integer; const Value: EpiString);virtual; abstract;
    procedure SetValue(const index: Integer; const Value: Epivariant);virtual;
    procedure initialize;virtual; abstract;
    procedure Finalize;virtual; abstract;
    procedure SetAllMissing;virtual; abstract;
    procedure Reset;virtual;
    procedure InternalSort(iLo, iHi: Integer;SortOptions:TSortOptions);virtual; abstract;
  public
    constructor Create(const VName:string; Size:EpiInt=-1; DfChkProp: TdfChkProp = nil);
//    constructor Create(Collection: TEpiCollection;VecLen:EpiUnInt); overload;
    destructor Destroy; override;
    function BinarySearch(aItem: pointer; var aInx: integer;Plength:integer): boolean;virtual;
    function Sort(Sortoptions:TSortOptions=[]):boolean;
    procedure Exchange(i,j:integer);virtual;abstract;
    function compare(i,j:integer):integer;virtual;abstract;
    function CompareItem(idx:integer;item:pointer):integer;virtual;
    function Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false):TEPiVector; virtual; abstract;
    function GetValueLabel(const value:String; Options: TObject = nil):string;
    procedure Assign(source:TEPiVector;NoData:boolean=false);
    procedure AssignData(source:TEPiVector);virtual;abstract;
    function HasMissingValuesDefined(): boolean;
    function HasValueLabels(): boolean;
    function GetVariableLabel(Options: TObject = nil): string;
    property Value[const index: Integer]: Epivariant read GetValue write SetValue;
    property IsMissing[const index: Integer]: EpiBool read GetIsMissing write SetIsMissing;
    property IsMissingValue[const index: Integer]: EpiBool read GetIsMissingValue;
    property AsBoolean[const index: Integer]: EpiBool read GetAsBoolean write SetAsBoolean;
    property AsInteger[const index: Integer]: EpiInt read GetAsInteger write SetAsInteger; default;
    property AsFloat[const index: Integer]: EpiFloat read GetAsFloat write SetAsFloat;
    property AsDate[const index: Integer]: EpiDate read GetAsDate write SetAsDate;
    property AsString[const index: Integer]: EpiString read GetAsString write SetAsString;
    property AsByte[const index: Integer]: EpiByte read GetAsByte write SetAsByte;
    property DataType: EpiUnInt read GetDataType;
    property DataSize: EpiUnInt read GetDataSize;
    property FieldDataType: EpiUnInt read GetFieldDataType Write SetFieldDataType;
    property FieldDataSize: EpiUnInt read GetFieldDataSize Write SetFieldDataSize;
    property FieldDataDecimals: EpiUnInt read GetFieldDataDecimals Write SetFieldDataDecimals;
    property FieldDataFormat: Epistring read GetFieldDataFormat Write SetFieldDataFormat;
    property Name : string read GetName write SetName;
    property Length  : EpiUnInt read GetVectorLength write SetVectorLength;
    property Capacity  : EpiUnInt read GetCapacity write SetCapacity;
    property FieldFormat: Epistring read GetFormat Write SetFormat;
    property IsSorted : epibool read GetIsSorted;
{#Todo2: Optimize count of missing values}
    property MissingCount : EpiUnInt read GetMissingCount;
    property Vectors: TEpiVectors read fVectors;
    property VariableLabel : string {read GetVariableLabel} write fVariableLabel;
//To support data editing of REC files, default to false
    property  MustEnter  : Boolean read fMustEnter write fMustEnter;
    property  Repeated   : Boolean read fRepeated write fRepeated;
    property  ReadOnly   : Boolean read fReadOnly write fReadOnly;
    property MemoryUsed : Integer read GetMemoryUsed;
    property CheckProperties: TVectorChkProp read fCheckProperties write fCheckProperties;
  end;

  TEpiFloatVector = class(TEpiVector)
  private
    Fdata : EpiFloatArray;
    procedure checkDataIndex(index:integer);
  protected
    function GetMemoryUsed: Integer;override;
    function GetFieldDataFormat: Epistring;override;
    function GetMissingCount: EpiUnInt;override;
    function CompareItem(idx: integer; item: pointer): integer;override;
    procedure InternalSort(iLo, iHi: Integer; SortOptions: TSortOptions);override;
    function GetVectorLength: EpiUnInt;override;
    procedure SetVectorLength(const Value: EpiUnInt);override;
    function GetIsMissing(const index: Integer): EpiBool;override;
    procedure SetIsMissing(const index: Integer; const Value: EpiBool);override;
    function GetIsMissingValue(const index: integer): EpiBool; override;
    function GetAsBoolean(const index: Integer): EpiBool;override;
    function GetAsDate(const index: Integer): EpiDate;override;
    function GetAsFloat(const index: Integer): EpiFloat;override;
    function GetAsInteger(const index: Integer): EpiInt;override;
    function GetAsString(const index: Integer): EpiString;override;
    function GetDataType: EpiUnInt;override;
    function GetValue(const index: Integer): Epivariant;override;
    procedure SetAsBoolean(const index: Integer; const Value: EpiBool);override;
    procedure SetAsDate(const index: Integer; const Value: EpiDate);override;
    procedure SetAsFloat(const index: Integer; const Value: EpiFloat);override;
    procedure SetAsInteger(const index: Integer; const Value: EpiInt);override;
    procedure SetAsString(const index: Integer; const Value: EpiString);override;
    procedure SetValue(const index: Integer; const Value: Epivariant);override;
    procedure initialize;override;
    procedure Finalize;override;
    procedure Reset;override;
    procedure SetAllMissing;override;
  public
    procedure Exchange(i, j: integer);override;
    function compare(i,j:integer):integer; override;
    function Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false):TEPiVector; override;
    procedure AssignData(source:TEPiVector); override;
end;


  TEpiIntVector = class(TEpiVector)
  private
    Fdata : EpiIntArray;
    procedure checkDataIndex(index:integer);
  protected
    function GetMemoryUsed: Integer;override;
    function GetMissingCount: EpiUnInt;override;
    procedure InternalSort(iLo, iHi: Integer; SortOptions: TSortOptions);override;
    function GetVectorLength: EpiUnInt;override;
    procedure SetVectorLength(const Value: EpiUnInt);override;
    function GetIsMissing(const index: Integer): EpiBool;override;
    procedure SetIsMissing(const index: Integer; const Value: EpiBool);override;
    function GetIsMissingValue(const index: integer): EpiBool; override;
    function GetAsBoolean(const index: Integer): EpiBool;override;
    function GetAsDate(const index: Integer): EpiDate;override;
    function GetAsFloat(const index: Integer): EpiFloat;override;
    function GetAsInteger(const index: Integer): EpiInt;override;
    function GetAsString(const index: Integer): EpiString;override;
    function GetDataType: EpiUnInt;override;
    function GetValue(const index: Integer): Epivariant;override;
    procedure SetAsBoolean(const index: Integer; const Value: EpiBool);override;
    procedure SetAsDate(const index: Integer; const Value: EpiDate);override;
    procedure SetAsFloat(const index: Integer; const Value: EpiFloat);override;
    procedure SetAsInteger(const index: Integer; const Value: EpiInt);override;
    procedure SetAsString(const index: Integer; const Value: EpiString);override;
    procedure SetValue(const index: Integer; const Value: Epivariant);override;
    procedure initialize;override;
    procedure Finalize;override;
    procedure Reset;override;
    procedure SetAllMissing; override;
  public
    procedure Exchange(i, j: integer);override;
    function compare(i,j:integer):integer; override;
    function Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false):TEPiVector; override;
    procedure AssignData(source:TEPiVector); override;
end;


  TEpiStringVector = class(TEpiVector)
  private
    FData: EpiStringArray;
    FUpperCase: boolean;
    fDataSize : integer;
    procedure checkDataIndex(index:integer);
  protected
    function GetMemoryUsed: Integer;override;
    function GetMissingCount: EpiUnInt;override;
    procedure InternalSort(iLo, iHi: Integer; SortOptions: TSortOptions);override;
    function GetVectorLength: EpiUnInt;override;
    procedure SetVectorLength(const Value: EpiUnInt);override;
    function GetIsMissing(const index: Integer): EpiBool;override;
    procedure SetIsMissing(const index: Integer; const Value: EpiBool);override;
    function GetIsMissingValue(const index: integer): EpiBool; override;
    function GetAsBoolean(const index: Integer): EpiBool;override;
    function GetAsDate(const index: Integer): EpiDate;override;
    function GetAsFloat(const index: Integer): EpiFloat;override;
    function GetAsInteger(const index: Integer): EpiInt;override;
    function GetAsString(const index: Integer): EpiString;override;
    function GetDataType: EpiUnInt;override;
    function GetDataSize: EpiUnInt;override;
    function GetValue(const index: Integer): Epivariant;override;
    procedure SetAsBoolean(const index: Integer; const Value: EpiBool);override;
    procedure SetAsDate(const index: Integer; const Value: EpiDate);override;
    procedure SetAsFloat(const index: Integer; const Value: EpiFloat);override;
    procedure SetAsInteger(const index: Integer; const Value: EpiInt);override;
    procedure SetAsString(const index: Integer; const Value: EpiString);override;
    procedure SetValue(const index: Integer; const Value: Epivariant);override;
    procedure initialize;override;
    procedure Finalize;override;
    procedure Reset;override;
    procedure SetAllMissing; override;
  public
    constructor Create(const VName:string; VLength:EpiInt; pDataSize:EpiInt; DfChkProp: TDfChkProp = nil);
    procedure Exchange(i, j: integer);override;
    function CompareItem(idx: integer; item: pointer): integer;override;
    function compare(i,j:integer):integer; override;
    procedure AssignData(source: TEPiVector);override;
    function Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;override;
    property Data: EpiStringArray read FData;
    Property Upper: Boolean read FUppercase; //Solving save uppercase field
end;


TEpiDateVector = class(TEpiVector)
  private
    Fdata : EpiIntArray;
    procedure checkDataIndex(index:integer);
  protected
    function GetMemoryUsed: Integer;override;
    function GetMissingCount: EpiUnInt;override;
    procedure InternalSort(iLo, iHi: Integer; SortOptions: TSortOptions);override;
    function GetVectorLength: EpiUnInt;override;
    procedure SetVectorLength(const Value: EpiUnInt);override;
    function GetIsMissing(const index: Integer): EpiBool;override;
    procedure SetIsMissing(const index: Integer; const Value: EpiBool);override;
    function GetIsMissingValue(const index: integer): EpiBool; override;
    function GetAsBoolean(const index: Integer): EpiBool;override;
    function GetAsDate(const index: Integer): EpiDate;override;
    function GetAsFloat(const index: Integer): EpiFloat;override;
    function GetAsInteger(const index: Integer): EpiInt;override;
    function GetAsString(const index: Integer): EpiString;override;
    function GetDataType: EpiUnInt;override;
    function GetValue(const index: Integer): Epivariant;override;
    procedure SetAsBoolean(const index: Integer; const Value: EpiBool);override;
    procedure SetAsDate(const index: Integer; const Value: EpiDate);override;
    procedure SetAsFloat(const index: Integer; const Value: EpiFloat);override;
    procedure SetAsInteger(const index: Integer; const Value: EpiInt);override;
    procedure SetAsString(const index: Integer; const Value: EpiString);override;
    procedure SetValue(const index: Integer; const Value: Epivariant);override;
    procedure initialize;override;
    procedure Finalize;override;
    procedure Reset;override;
    procedure SetAllMissing; override;
  public
    procedure Exchange(i, j: integer);override;
    function compare(i,j:integer):integer; override;
    procedure AssignData(source: TEPiVector); override;
    function Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector; override;
end;

  TEpiByteVector = class(TEpiVector)
  private
    FData: array of Byte;
    procedure checkDataIndex(index:integer);
  protected
    function GetMemoryUsed: Integer;override;
    function GetMissingCount: EpiUnInt;override;
    procedure InternalSort(iLo, iHi: Integer; SortOptions: TSortOptions);override;
    function GetVectorLength: EpiUnInt;override;
    procedure SetVectorLength(const Value: EpiUnInt);override;
    function GetIsMissing(const index: Integer): EpiBool;override;
    procedure SetIsMissing(const index: Integer; const Value: EpiBool);override;
    function GetIsMissingValue(const index: integer): EpiBool; override;
    function GetAsByte(const index: Integer): EpiByte;override;
    function GetAsBoolean(const index: Integer): EpiBool;override;
    function GetAsDate(const index: Integer): EpiDate;override;
    function GetAsFloat(const index: Integer): EpiFloat;override;
    function GetAsInteger(const index: Integer): EpiInt;override;
    function GetAsString(const index: Integer): EpiString;override;
    function GetDataType: EpiUnInt;override;
    function GetDataSize: EpiUnInt;override;
    function GetValue(const index: Integer): Epivariant;override;
    procedure SetAsByte(const index: Integer; const Value: EpiByte);override;
    procedure SetAsBoolean(const index: Integer; const Value: EpiBool);override;
    procedure SetAsDate(const index: Integer; const Value: EpiDate);override;
    procedure SetAsFloat(const index: Integer; const Value: EpiFloat);override;
    procedure SetAsInteger(const index: Integer; const Value: EpiInt);override;
    procedure SetAsString(const index: Integer; const Value: EpiString);override;
    procedure SetValue(const index: Integer; const Value: Epivariant);override;
    procedure initialize;override;
    procedure Finalize;override;
    procedure Reset;override;
    procedure SetAllMissing; override;
  public
    constructor Create(const VName:string; VLength:EpiInt; DfChkProp: TDfChkProp = nil);
    procedure Exchange(i, j: integer);override;
    function CompareItem(idx: integer; item: pointer): integer;override;
    function compare(i,j:integer):integer; override;
    procedure AssignData(source: TEPiVector);override;
    function Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;override;
end;       


  TEpiVectors = class(TObject)
  private
    FList: TList;
    FDataFrame: TEpiDataFrame;
    FOnChange: TNotifyEvent;
    fOwned: boolean;
    procedure AddNew(Vector: TEpiVector);
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
//    FValidVectorKinds: TVectorKinds;
  protected
    procedure Changed;
//    procedure CheckVectorKind(VectorKind: TVectorKind; Vector: TEpiVector);
    function GetCount: Integer;
    function GetVector(Index: Integer): TEpiVector;
    procedure SetVectorIndex(Vector: TEpiVector; Value: Integer);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
//    property ValidVectorKinds: TVectorKinds read FValidVectorKinds write FValidVectorKinds;
  public
    constructor Create(ADataFrame: TEpiDataFrame;pOwned:boolean=true);
    destructor Destroy; override;
    procedure Add(Vector: TEpiVector); overload;
    procedure Add(Vector: TEpiVector; Idx: integer); overload;
    procedure CheckVectorName(const VectorName: string);
    procedure CheckVectorNames(const VectorNames: string);
    procedure Clear;
    function GetVectorIndex(const VectorName: string): Integer;
    function FindVector(const VectorName: string): TEpiVector;
    function VectorByName(const VectorName: string): TEpiVector;
//    function VectorByNumber(VectorNo: Integer): TEpiVector;
    procedure GetVectorNames(List: TStrings);
    function IndexOf(Vector: TEpiVector): Integer;
    procedure Remove(Vector: TEpiVector);
    property Count: Integer read GetCount;
    property DataFrame: TEpiDataFrame read FDataFrame;
    property Vectors[Index: Integer]: TEpiVector read GetVector; default;
    property Owned : boolean read fOwned write fOwned;
    property Capacity :integer read GetCapacity write SetCapacity;
  end;


{new Byte vector}

  TEpiBoolVector = class(TEpiVector)
//  TEpiByteVector = class(TEpiVector)
  private
    FData:   array of byte;
    procedure checkDataIndex(index:integer);
  protected
    function GetMemoryUsed: Integer;override;
    function GetMissingCount: EpiUnInt;override;
    procedure InternalSort(iLo, iHi: Integer; SortOptions: TSortOptions);override;
    function GetVectorLength: EpiUnInt;override;
    procedure SetVectorLength(const Value: EpiUnInt);override;
    function GetIsMissing(const index: Integer): EpiBool;override;
    procedure SetIsMissing(const index: Integer; const Value: EpiBool);override;
    function GetIsMissingValue(const index: integer): EpiBool; override;
    function GetAsByte(const index: Integer): EpiByte;override;
    function GetAsBoolean(const index: Integer): EpiBool;override;
    function GetAsDate(const index: Integer): EpiDate;override;
    function GetAsFloat(const index: Integer): EpiFloat;override;
    function GetAsInteger(const index: Integer): EpiInt;override;
    function GetAsString(const index: Integer): EpiString;override;
    function GetDataType: EpiUnInt;override;
    function GetDataSize: EpiUnInt;override;
    function GetValue(const index: Integer): Epivariant;override;
    procedure SetAsByte(const index: Integer; const Value: EpiByte);override;
    procedure SetAsBoolean(const index: Integer; const Value: EpiBool);override;
    procedure SetAsDate(const index: Integer; const Value: EpiDate);override;
    procedure SetAsFloat(const index: Integer; const Value: EpiFloat);override;
    procedure SetAsInteger(const index: Integer; const Value: EpiInt);override;
    procedure SetAsString(const index: Integer; const Value: EpiString);override;
    procedure SetValue(const index: Integer; const Value: Epivariant);override;
    procedure initialize;override;
    procedure Finalize;override;
    procedure Reset;override;
    procedure SetAllMissing; override;
  public
    constructor Create(const VName:string; VLength:EpiInt; DfChkProp: TDfChkProp = nil);
    procedure Exchange(i, j: integer);override;
    function CompareItem(idx: integer; item: pointer): integer;override;
    function compare(i,j:integer):integer; override;
    procedure AssignData(source: TEPiVector);override;
    function Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;override;
end;


TEpiDataFrame=class(TPersistent)
private
    FVectors :TEpiVectors;
    FOpenMode: Epiint;
    fFileName: TEpiFileName;
    fRowNoVector : TEpiVector;
    fSelectVector :TEpiVector;
    fRecStatusVector: TEpiVector;
    fBackupSelector : TEpiVector;
    fRowNo: integer;
    fCapacity: integer;
    fModified: boolean;
    fRowCount: integer;
    fSupportsLabels: boolean;
    fDataLabel: string;
    fRecordSize: integer;
    fCheckProperties: TdfChkProp;
    FErrorcount: integer;
    Sortlist:Tlist;
    function GetFileName: TEpiFileName;
    function GetOpenMode: Epiint;
    procedure SetFileName(const Value: TEpiFileName);
    procedure SetOpenMode(const Value: Epiint);
    function GetVectorByName(VName: string): TEpiVector;
    function GetRowCount: integer;
    function GetVectorCount: integer;
    function InternalCompareRecords(Record1,Record2:integer): Integer;
    procedure Exchange(I, J: Integer);
    procedure InternalSort(L, R: Integer);
    function GetSupportsLabels: boolean;
    function GetSelectorBacked: boolean;
    procedure SetRowCount(const Value: integer);
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
    function GetDeleted(Index: integer): boolean;
    procedure SetDeleted(Index: integer; const Value: boolean);
    function GetVerified(Index: integer): boolean;
    procedure SetVerified(Index: integer; const Value: boolean);
    function GetSelected(Index: integer): boolean;
    procedure SetSelected(Index: integer; const Value: boolean);
    function GetRecordNo(Index: integer): EpiInt;
    procedure SetErrorcount(const Value: integer);
protected
   procedure InitializeInternalVectors(co:integer);
   procedure DeinitializeInternalVectors();
   constructor InternalCreate;
public
   constructor Create(EpiDataSetClass:TEpiDataSetClass;const filename: TEpiFileName;Mode:Epiint; const pw:string='');
   constructor CreateTemp(rows:integer);
   constructor CreateFromDataSet(aDataSet: TEpiDataSet);
   Destructor Destroy; override;
   function ToStattable(varnames: TStrings): TStatTable;
   procedure SendToOutput(Varnames: TStrings);
   function PrepareDataframe(var CloneVectors: TStrings; MissingVectors: TStrings; Datatypes: TEpiDatatypes = []): TepiDataframe;
   function ExpandDataframe(GroupVarnames: TStrings; aBegin: variant; aEnd: variant): TEpiDataframe;
   function ConvertVector(Name: string; DataType: word; Const txt: string): boolean;
   function DropVectors(varnames: Tstrings): boolean;
   function FormatVectors(varnames: Tstrings; const fmt: String): boolean;
   function LoadFromDataSet(ADataset:TEpiDataSet):Epiint;
   function AppendNewRows(af: TEpiDataFrame; VAR ExcludedFields,LackingFields,InCompatibleFields:string):boolean;
   function SaveToDataSet(ADataset:TEpiDataSet):Epiint;
   function CreateNewDataSet(EpiDataSetClass: TEpiDataSetClass;const fn:TEpifilename;Varnames: TStrings; const pw:string=''): TEpiDataSet;
   function CopyStruFrom(pDataFrame:TEpiDataFrame; var  RelatedVectors : TEpiRelatedVectorsArray; relatename: string):boolean;
   function NewVector(pVarDesc: TAnaVariableDescriptor): TEpiVector;
   function UpdateVector(V:TEpiVector;Exp:IValue):Epiint;
   function UpdateVectorRow(V: TEpiVector; Exp: IValue;aRow: integer): Epiint;
   function ApplySelection(Exp:IValue;opposite:boolean):Epiint;
   function ApplySelectionOld(Exp: IValue): Epiint;
   function FindVector(const VectorName: string): TEpiVector;
   function SelectedRowCount: integer;
   function GetVectorListByName(varnames: Tstrings):TEpiVectors;
   function GetVectorNames(Varnames:TStrings; Datatypes: TEpiDataTypes = []): TStrings;
   function Sort(VectList:TEpiVectors):boolean;overload;
   function Sort(const varname: string): boolean; overload;
   function RebuildRecnumber():boolean;
   function Clone(varnames: Tstrings):TEpiDataFrame; deprecated;
   function BackupSelector:boolean;
   function RestoreSelector:boolean;
   function GetValueBlockPointer(const labname:string): TLabelValueList;
//   function RowSelectToRowNoselect(aRowNo: integer): integer;
   function RowNoselectToRowSelect(aRowNo: integer): integer;
   property FileName :TEpiFileName read GetFileName write SetFileName;
   property OpenMode : Epiint read GetOpenMode write SetOpenMode;
   property Vectors :TEpiVectors read FVectors;
   property VectorByName[VName:string]:TEpiVector read GetVectorByName;
   property RowCount :integer read GetRowCount write SetRowCount;
   property Capacity :integer read GetCapacity write SetCapacity;
   property VectorCount :integer read GetVectorCount;
   property RowNo : integer read fRowNo write fRowNo;
   property Modified: boolean read fModified write fModified;
   property SupportsLabels:boolean read GetSupportsLabels write fSupportsLabels;
   property RecordSize : integer read fRecordSize;
   property SelectorBacked: boolean  read GetSelectorBacked;
   property DataLabel  : string read fDataLabel write fDataLabel;
   property Deleted[Index:integer] : boolean read GetDeleted write SetDeleted;
   property Verified[Index:integer] : boolean read GetVerified write SetVerified;
   property Selected[Index:integer] : boolean read GetSelected write SetSelected;
   property RecordNo[Index:integer] : EpiInt read GetRecordNo;
   property Errorcount: integer read FErrorcount write SetErrorcount;
   property CheckProperties:TdfChkProp read fCheckProperties write fCheckProperties;
end;



implementation
uses Ucmdprocessor, Math, StrUtils, UVariables, UDebug, UAggregate, Ucommands, Forms, EpiDataFile,CheckObjUnit, UCmdTypes;

const
  UnitName = 'UVectors';

function R2(const AValue : extended ; const ADigit : TRoundToRange) :extended ;
var X : extended ; i : integer ;
begin
  X := 1.0 ;
  for i := 1 to Abs(ADigit) do X := X * 10 ;
  if ADigit<0
    then Result := Round(AValue * X) / X
    else Result := Round(AValue / X) * X ;
end {R2} ;

procedure error(const msg:string);
begin
   raise Exception.create(msg);
end;

procedure ErrorFmt(const Message: string; const Args: array of const);
begin
  error(Format(Message, Args));
end;

constructor TEpiDataFrame.InternalCreate;
begin
  inherited create;
  Sortlist:=Tlist.create;
  fCheckProperties:=TdfChkProp.Create;
  fBackupSelector:=nil;
  fFilename:='';
  fOpenMode := -1;
  fVectors :=TEpiVectors.Create(self);
  fModified := false;
end;

constructor TEpiDataFrame.Create(EpiDataSetClass: TEpiDataSetClass;const filename: TEpiFileName;Mode:Epiint; const pw:string='');
var
   FDataSet :TEpiDataSet;
begin
  FDataSet := nil;
  InternalCreate;
  fFilename:=filename;
  fOpenMode := Mode;
  try
    try
      EpiDataSetClass.SetErrorText('');
      FDataSet := EpiDataSetClass.Create(fFileName,fOpenMode,fCheckProperties,pw);
      DataLabel := FDataSet.FileLabel;
      Errorcount := LoadFromDataSet(FDataSet);
    except
        if EpiDataSetClass.GetErrorText<>'' THEN
        begin
          dm.Error(EpiDataSetClass.GetErrorText, [], -1, 0);
          EpiDataSetClass.SetErrorText('');
        end;
      raise;
    end;
    if (filename = 'from clipboard') then
      fModified := true; 
  finally
    If Assigned(FDataSet) then FreeAndNil(FDataSet);// FDataSet.free;
  end;
end;


constructor TEpiDataFrame.CreateFromDataSet(aDataSet: TEpiDataSet);
begin
  InternalCreate;
  fFilename:=aDataSet.filename;
  fOpenMode := 0;
  try
    DataLabel:= aDataSet.FileLabel;
    Errorcount:=LoadFromDataSet(aDataSet);
  except
    raise
  end;
end;


constructor TEpiDataFrame.CreateTemp(rows:integer);
begin
  InternalCreate;
  InitializeInternalVectors(rows);
  fRowCount:=rows;
  frowNo :=1;
end;



destructor TEpiDataFrame.Destroy;
begin
  DeinitializeInternalVectors();
  FreeAndNil(fVectors);
  FreeAndNil(sortlist);
  FreeAndNil(fCheckProperties);
  inherited;
end;

function TEpiDataFrame.GetFileName: TEpiFileName;
begin
  result:=fFilename;
end;

function TEpiDataFrame.GetOpenMode: Epiint;
begin
  result:=fOpenMode;
end;

function TEpiDataFrame.GetRowCount: integer;
begin
  result := fRowCount;
end;

function TEpiDataFrame.SelectedRowCount: integer;
var
 i, co : integer;
begin
  result:=0;
  if fSelectVector = nil then
    error('No selection vector');
  for i:= 1 to RowCount do
    if Selected[i] then inc(result);
end;

function TEpiDataFrame.GetVectorByName(VName: string): TEpiVector;
begin
  Result := Vectors.VectorByName(VName);
end;

function TEpiDataFrame.GetVectorCount: integer;
begin
  Result := Vectors.Count;
end;

procedure TEpiDataFrame.InitializeInternalVectors(co:integer);
var
  i, j, n   : integer;
begin
  fRowNoVector:= TEpiIntVector.Create('__n_',co, fCheckProperties);
  fRowNoVector.FieldDataType:=EpiTyInteger;
  fSelectVector:= TEpiIntVector.Create('__s_',co, fCheckProperties);
  fSelectVector.FieldDataType:=EpiTyInteger;
  fRecStatusVector:=TEpiIntVector.Create('__r_',co, fCheckProperties);
  fRecStatusVector.FieldDataType:=EpiTyInteger;
  for i:= 1 to co do
  begin
     fRowNoVector.AsInteger[i] := i;
     fSelectVector.AsInteger[i] := EpiRecSelected;
     fRecStatusVector.AsInteger[i] := EpiRecNormal;
  end;
end;

procedure TEpiDataFrame.DeinitializeInternalVectors();
begin
  FreeAndNil(fRownoVector);
  FreeAndNil(fSelectVector);
  FreeAndNil(fRecStatusVector);
end;

function TEpiDataFrame.backupSelector: boolean;
begin
  fBackupSelector := fSelectVector.Clone(self);
end;

function TEpiDataFrame.RestoreSelector: boolean;
begin
  if assigned(fBackupSelector) then
  begin
      fSelectVector.AssignData(fBackupSelector);
     FreeAndNil(fBackupSelector);
  end;
end;

function TEpiDataFrame.GetSelectorBacked: boolean;
begin
  Result := Assigned(fBackupSelector);
end;


function TEpiDataFrame.CreateNewDataSet(EpiDataSetClass: TEpiDataSetClass;const fn:TEpifilename;Varnames: TStrings; const pw:string=''): TEpiDataSet;
var
  i, co, j, n, t, q   : integer;
  //List       : TEpiFieldList;
  //fld        : TEpiField;
  v          :TEpiVector;
  opt        :TEpioption;
  s : string;
  epdFile    : TEpiDataFile;
  aField     : TeField;
  chkWriter  : TCheckWriter;
  aLabelRec,prevLabelRec,firstLabel  : PLabelRec;
  val :TLabelValueList;
begin
   Result:=nil;
   epdFile:=NIL;
try
   if varnames<>nil then
      TStringList(Varnames).sorted :=true;

   epdFile:=TEpiDataFile.Create;
   fCheckProperties.CopyToEpiFile(epdFile);
   co := VectorCount;
   for i:= 0 to co-1 do
   begin
       v := Vectors[i];
       //if v.internal then continue;
       if (varnames<>nil) and (Varnames.IndexOf(v.Name)<0) then continue;
       aField:=TeField.Create;
       aField.FieldName:=V.Name;
       if v.CheckProperties.OrigFelttype=ftRes4
       then aField.Felttype:=VectorType2EpiDataFieldType(v.FieldDataType)
       else aField.Felttype:=v.CheckProperties.OrigFelttype;
       if v.fFldDataType = EpiTyUppercase then
            aField.Felttype := ftUpperAlfa;

       aField.FLength:=v.FieldDataSize;
       aField.FNumDecimals:=v.FieldDataDecimals;
       // here the file should  be written either as first word or epi6 style:
       // this will write only label
       // Do not use "GetVariableLabel" it's a formatted edition of the fVariableLabel.
       //if dm.GetOptionValue('SAVEDATA FIRSTWORD', opt) AND (opt.value = 'ON') then
         //Firstword field naming - but do not dublicate first word:
       //  begin
       //    if v.fVariableLabel = v.fName
       //       then aField.FQuestion:=v.fName
       //       else aField.FQuestion:=v.fName + ' ' + v.fVariableLabel;
       //  end
       //else
       //  // MIB: Insert epi6 field naming style here
       //  dm.Error('v2.0: Only first word file type can be saved<br>Set savedata firstword=on');  //todo MIB: change to appropriate form
       if CheckProperties.EpiInfoFieldNaming
       then aField.FQuestion:=v.fVariableLabel
       else
         begin
           if AnsiLowerCase(copy(trim(v.fVariableLabel),1,length(v.fName)))=AnsiLowerCase(v.fName) then aField.FQuestion:=v.fVariableLabel
           else aField.FQuestion:=v.fName + ' ' + v.fVariableLabel;
         end;
       v.CheckProperties.CopyToField(aField);
       q:=epdFile.ValueLabels.IndexOf(aField.FValueLabel);
       if q>-1 then aField.FCommentLegalRec:=PLabelRec(epdFile.ValueLabels.Objects[q]);
       epdFile.AddField(aField);

       // dm.info(v.name + format('%d',[v.fielddatatype]));
   end;
      //Saves rec-file and checkfile with header but no data:
      Result:=EpiDataSetClass.GetNewDataSet(fn,epdFile,fCheckProperties,pw);
finally
  epdFile.Free;

  //List.free;
  //chkWriter.Free;
end;
end;

function TEpiDataFrame.AppendNewRows(af: TEpiDataFrame; VAR ExcludedFields,LackingFields, InCompatibleFields:string):Boolean;
var
  n,j,co,new_co: integer;
  CurField,CurRow,OrigRowCount,AppendNoVectors: integer;
  ok:Boolean;
  appVector,origVector: TEpiVector;

  function isCompatible(source,destination:TEpiVector):boolean;
  begin
    result:=false;
    if (source.DataType=EpiTyInteger) and (destination.DataType=EpiTyFloat) THEN result:=true;
    if source.DataType=destination.DataType THEN result:=true;
    if (destination.DataType=EpiTyString) or
       (destination.DataType=EpiTyUppercase) THEN result:=true;
    if (source.DataType in [EpiTyUnknown, EpiTyUnSupported]) THEN result:=false;
    if (source.FieldDataSize>destination.FieldDataSize) then result:=false;
  end;

begin
  //Tag alle felter i appendframe og test
  //  - findes mindst et af felterne i appendframe i dataframe
  //  - er de, der findes assign-compatible (type og length)
  //  - afbryd når der er fundet eet compatibelt felt
  //Hvis fundet mindst eet compatibelt felt så
  // - tilføj nødvendige antal records
  //Iterer over alle felter i append frame
  // - hvis feltet i appendframe findes i dataframe, så
  //   - overfør data for alle rækker

  //Test if at least one field in source is compatible with destination
  CurField:=0;
  ok:=False;
  ExcludedFields:='';
  LackingFields:='';
  InCompatibleFields:='';
  REPEAT
    origVector:=FindVector(af.Vectors.Vectors[CurField].fName);
    IF origVector<>nil then ok:=isCompatible(af.Vectors.Vectors[CurField],origVector);
    INC(CurField);
  UNTIL (CurField=af.VectorCount) or (ok);
  if (not ok) THEN error('No compatible fields found.');

  //Fill out LackingFields, ie fields in original dataframe not found in appendframe
  FOR curField:=0 to Vectors.Count-1 DO
    BEGIN
      origVector:=Vectors.Vectors[curfield];
      //if (not origVector.Internal) then
        begin
          appVector:=af.FindVector(origVector.fName);
          IF appVector=NIL then
            begin
              IF LackingFields='' THEN Lackingfields:=origVector.fName
              ELSE LackingFields:=LackingFields+', '+origVector.fName;
            end;
        end;
    END;

  //Add the necessary new rows
  co:=fRowCount;
  OrigRowCount:=co;
  //initialize internal vectors in new rows
  vectors.Capacity:=vectors.Capacity+af.RowCount;
  for n:=0 to fVectors.Count-1 DO
    BEGIN
      //if (not fVectors.Vectors[n].Internal) then
        begin
          for j:=co+1 to co+af.RowCount do
            fVectors.Vectors[n].IsMissing[j]:=True;
        end;
    END;
  for n:=co+1 to co+af.RowCount do
    begin
      fRowNoVector.AsInteger[n] := n;
      fRecStatusVector.AsInteger[n] := af.fRecStatusVector.AsInteger[n-co];
      Selected[n] := true;
    end;
  fRowCount:=co+af.RowCount;

  //Iterate fields in source file and transfer data to destination file
  FOR curfield:=0 to af.Vectors.Count-1 DO
    BEGIN
     // if (not af.Vectors.Vectors[curfield].Internal) then
        begin
          appVector:=af.Vectors.Vectors[curfield];
          OrigVector:=FindVector(appVector.fName);
          if OrigVector<>NIL then
            begin
              //a vector with same name as source vector is found
              if isCompatible(appVector,OrigVector) then
                begin
                  //vectors are compatible - transfer data
                  for currow:=1 to af.RowCount do
                    begin
                      case appVector.DataType of
                        EpiTyBoolean:   OrigVector.AsBoolean[OrigRowCount+currow]:=appVector.AsBoolean[currow];
                        EpiTyInteger:   OrigVector.AsInteger[OrigRowCount+currow]:=appVector.AsInteger[currow];
                        EpiTyFloat:     OrigVector.AsFloat[OrigRowCount+currow]:=appVector.AsFloat[currow];
                        EpiTyDate:      OrigVector.AsDate[OrigRowCount+currow]:=appVector.AsDate[currow];
                        EpiTyUppercase,
                        EpiTyString:    OrigVector.AsString[OrigRowCount+currow]:=appVector.AsString[currow];
                        EpiTyByte:      OrigVector.AsByte[OrigRowCount+currow]:=appVector.AsByte[currow];
                      end;  //case
                    end;  //for
                end  //if
              else
                begin
                  //incompatible field found
                  IF InCompatibleFields='' THEN InCompatibleFields:=appVector.fName
                  ELSE InCompatibleFields:=InCompatibleFields+', '+appVector.fName;
                end
            end  //if
          else
            begin
              //field in appendframe not found in original file
              IF ExcludedFields='' THEN ExcludedFields:=appVector.fName
              ELSE ExcludedFields:=ExcludedFields+', '+appVector.fName;
            end;
        end;  //if
    END;  //for
  fModified := true;
end;

function TEpiDataFrame.LoadFromDataSet(ADataset:TEpiDataSet): Epiint;
var
  i, co, obs, j, n, lcapacity, vallabno  : integer;
  rowcount: integer;
  Anint      : integer;
  AFloat     :EpiFloat;
  BoolVal    : word;
  Blank, valid      : boolean;
  List : TList;
  s          : string;
  sv         :TEpiStringVector;
  v          :TEpiVector;
  dt1        : Epidate;
  lErrCount, delcount,dateerror, finalobs  : integer;
  c          : char;
  strbuf:    array[0..100] of char;
  opt:       TEpiOption;
  loopcounter: integer;
begin
   assert(AdataSet<> nil);
   List :=nil;
try
  Result:=0;
  list:=TList.Create;
  co := Adataset.FieldCount;
  n := Adataset.RecordCount;
  lcapacity:=(n div DefaultGrowthFactor)* DefaultGrowthFactor + DefaultGrowthFactor;
  if n=0 then n:=-1;
  for i:= 0 to co-1 do
    begin
      case Adataset.fields[i].Felttype of
        ftBoolean:
          begin
            v:= TEpiBoolVector.Create(ADataSet.Fields[i].Fieldname,lcapacity, fCheckProperties);
          end;
        ftInteger,ftIDNUM:
          begin
            v:= TEpiIntVector.Create(ADataSet.Fields[i].Fieldname,lcapacity, fCheckProperties);
          end;
        ftAlfa,ftUpperAlfa,ftCrypt,ftSoundex:
          begin
            v:= TEpiStringVector.Create(ADataSet.Fields[i].Fieldname,lcapacity,ADataSet.Fields[i].FLength, fCheckProperties);
            if Adataset.fields[i].Felttype = ftUpperAlfa then
              TEpiStringVector(v).fUppercase := true;
          end;
        ftFloat:
          begin
            v:= TEpiFloatVector.Create(ADataSet.Fields[i].Fieldname,lcapacity, fCheckProperties)
          end;
        ftDate,ftToday,ftEuroDate,ftEuroToday,ftYMDDate,ftYMDToday:
          begin
            v:= TEpiDateVector.Create(ADataSet.Fields[i].Fieldname,lcapacity, fCheckProperties)
          end;
        ftQuestion,ftRes4:
          begin
            continue
          end
      else
        Error('Unsupported field type');
      end;//case
      //if  (Adataset.fields[i].datatype=EpiTyString) and (ADataSet.Fields[i].FieldLen= 1) then Adataset.fields[i].datatype:= EpiTyByte;
      v.FieldDataType:=     EpiDataFieldType2VectorType(Adataset.fields[i].Felttype);
      v.FieldDataSize:=     Adataset.fields[i].FLength;
      v.FieldDataDecimals:= Adataset.fields[i].FNumDecimals;
//      v.ValueLabelName :=   Adataset.fields[i].FValueLabel;
      v.VariableLabel :=    Adataset.fields[i].FVariableLabel;
      v.FieldDataFormat :=  Adataset.fields[i].FFieldFormat;
      if (Adataset.Fields[i].Felttype in [ftToday,ftEuroToday,ftYMDToday]) then v.ReadOnly:=true else v.ReadOnly:=false;
      v.Repeated :=         Adataset.fields[i].FRepeat;
      v.MustEnter  :=       Adataset.fields[i].FMustEnter;
      //v.FieldUserData:=     Adataset.fields[i].FieldUserData;
      if (not assigned(v.CheckProperties)) then v.CheckProperties:=TVectorChkProp.Create;
      v.CheckProperties.CopyFromField(Adataset.Fields[i]);
      // Valuelabels should be handles during CopyFromField and CopyToField.
      if (ADataset.Fields[i].FValueLabel <> '') then
      begin
        vallabno := fCheckProperties.ValueLabels.IndexOf(ADataset.Fields[i].FValueLabel);
        if vallabno>-1 then v.fCheckProperties.ValueLabelSet :=
          TLabelValueList(fCheckProperties.ValueLabels.Objects[vallabno]);
      end;
      vectors.Add(v);
      List.Add(ADataSet.Fields[i]);
    end;// for i
  loopcounter:=0;
  co := List.count;
  ADataSet.first;
  ADataSet.Next;
  j:=1;
  dateerror := 0;
  Rowcount:=1;
  SetLength(s,81);
  InitializeInternalVectors(lcapacity);
  obs:=0;
  delcount:=0;
  if n< 1 then
    begin
      for i:= 0 to co-1 do
        Vectors[i].SetAllMissing;
    end
  else
    for j:= 1 to n do
      begin
        if ADataSet.Deleted then inc(delcount);
        If (dm.GetOptionValue('READ DELETED', Opt) and (ansiuppercase(Opt.Value) = 'OFF') AND ADataSet.Deleted) then
          begin
            ADataSet.Next;
            continue;
          end;
        inc(obs);

        s:= '';
        lErrCount:=0;

        for i:= 0 to co-1 do   // go over all fields in this record
          begin
            case TeField(List.items[i]).Felttype of
              ftInteger,ftIDNUM:
                begin
                  valid:=ADataSet.GetFieldData(j,List[i],@Anint,Blank);
                  if Blank then Vectors[i].asinteger[obs]:=NA_INT
                  else Vectors[i].asinteger[obs]:=Anint;
                end;
              ftFloat:
                begin
                  valid:=ADataSet.GetFieldData(j,List[i],@AFloat,Blank);
                  if Blank then Vectors[i].asfloat[obs]:=NA_FLOAT
                  else Vectors[i].asfloat[obs]:=AFloat;
                end;
              ftBoolean:
                begin
                  valid:=ADataSet.GetFieldData(j,List[i],@BoolVal,Blank);
                  //if Blank then Vectors[i].isMissing[j]:=true  {code changed 31/5-2005, Michael Bruus}
                  if Blank then Vectors[i].AsByte[obs]:=NA_BOOL
                  else Vectors[i].asboolean[obs]:=BoolVal=1;
                end;
              {EpiTyByte:
                begin
                  valid:=ADataSet.GetFieldData(obs,List[i],@c,Blank);
                  if Blank then Vectors[i].isMissing[j]:=true
                  else Vectors[i].AsString[j]:=c;
                end;}
              ftAlfa,ftUpperAlfa,ftSoundex,ftCrypt:
                begin
                  strbuf:='12345678912345678912345678912345678912345678912345678';
                  valid := ADataSet.GetFieldData(j,List[i],@strbuf,Blank);
                  s:=strbuf;
                  if (blank) then Vectors[i].isMissing[obs] := true
                  else Vectors[i].AsString[obs] := s;
                end;
              ftDate,ftToday,ftEuroDate,ftEuroToday,ftYMDDate,ftYMDToday:
                begin
                  // ugly bypass when record is marked for deletion (JL March 2007)
                  if ADataSet.Deleted
                    then valid:=ADataSet.GetFieldData(-1,List[i],@dt1,Blank)
                    else valid:=ADataSet.GetFieldData(j,List[i],@dt1,Blank);
                  if not valid then inc(dateerror);
                  if (Blank) or (not valid) then Vectors[i].asDate[obs]:=NA_DATE
                  else Vectors[i].asDate[obs]:=dt1;
                end;
            end;  //case

            if not valid then
              begin
                if (not ( ( adataset.deleted ) and (dm.GetOptionValue('READ DELETED', Opt) and (ansiuppercase(Opt.Value) = 'OFF')) ) )then
                  begin
                    //dm.info('Error-> record: ' + inttostr(j) + ' ' + vectors[i].Name  +  ' (' + s + ')= .');
                    inc(lErrCount);
                  end;
              end;

            if ((loopcounter mod 10) = 0) then
              begin
                if dm.CheckforBreak then
                  begin
                    aDataset.error('Cancelled by user.');
                    raise exception.create('cancelled by user.');
                    exit;
                  end;
              end
            else INC(loopcounter);
          end;  //for i - next field

        fRowNoVector.AsInteger[obs]:= Rowcount;
        if lErrCount>0 then inc(Result);
        fRecStatusVector.AsInteger[obs] := EpiRecNormal;
        if ADataSet.Verified then Verified[obs] := true;
        Deleted[obs]:= ADataSet.Deleted;
        inc(rowcount);
        ADataSet.Next;
      end;  //for j   -- get next observation.
    frowNo :=1;
    frowcount:=Adataset.RecordCount;
    if (delcount > 0) then
      begin
        if (dm.GetOptionValue('READ DELETED', Opt)) and (ansiuppercase(Opt.Value) = 'ON') then
          begin
            dm.info('Included: %d records marked for deletion',[delcount], 209001);
          end
        Else
          begin
            //vectors.Capacity:=j-delcount;
            frowcount:=ADataset.RecordCount-delcount;
            dm.info('Excluded: %d records marked for deletion',[delcount],209002);
          end;
      end;
finally
  List.free;
end;  //try..finally
end;  //LoadFromdataSet


function TEpiDataFrame.SaveToDataSet(ADataset: TEpiDataSet): Epiint;
var
  listpointer,i, co, j, n,ErrorCode   : integer;
  Anint      : integer;
  AFloat     :EpiFloat;
  BoolVal    : integer;
  Blank      : boolean;
  List       : TList;
  s          : string;
  sv         :TEpiStringVector;
  v          :TEpiVector;
  dt1        : Epidate;
  c          : array[0..1] of char ;
  aField: TeField;
begin
   assert(AdataSet<>nil);
   List :=nil;
   c[1]:=#0;
   Result:=0;
   try
     TRY
       List:=TList.Create;
       co:=0;
       for n:=0 to ADataSet.FieldCount-1 do
         begin
           if (ADataSet.Fields[n].Felttype<>ftQuestion)  then
             begin
               list.Add(pointer(ADataSet.Fields[n]));
               inc(co);
             end;
         end;
       n:= RowCount;
       co:=vectorcount;
       ADataSet.first;
       for j:= 1 to n do
         begin
           listpointer:=0;
           if not Selected[j] then continue;
           ADataSet.Append;
           for i:= 0 to co-1 do
             begin
               //if Vectors[i].Internal then continue;
               aField:=TeField(list.Items[listpointer]);
               case aField.Felttype of
                 ftInteger,ftIDNUM:
                   begin
                     Anint:=Vectors[i].asinteger[j];
                     if Anint=NA_INT then ADataSet.SetFieldData(aField,nil)
                     else ADataSet.SetFieldData(aField,@Anint);
                   end;
                 ftFloat:
                   begin
                     AFloat:=Vectors[i].asfloat[j];
                     if AFloat=NA_FLOAT then ADataSet.SetFieldData(aField,nil)
                     else ADataSet.SetFieldData(aField,@AFloat);
                   end;
                 ftBoolean:
                   begin
                     //BoolVal:= Word(Vectors[i+1].asboolean[j]);
                     AFloat:=Vectors[i].AsFloat[j];
                     if AFloat=NA_FLOAT then BoolVal:=NA_BYTE
                     else if AFloat=1 then BoolVal:=1
                     else if AFloat=0 then BoolVal:=0;
                     if BoolVal=NA_BYTE then ADataSet.SetFieldData(aField,nil)
                     else ADataSet.SetFieldData(aField,@BoolVal);
                   end;
                 {EpiTyByte:
                   begin
                     if Vectors[i+1].IsMissing[j] then ADataSet.SetFieldData(List[i],nil)
                     else ADataSet.SetFieldData(List[i],pchar(Vectors[i+1].asstring[j]));
                   end;}
                 ftAlfa,ftUpperAlfa,ftCrypt,ftSoundex:
                   begin
                     s:=Vectors[i].AsString[j];
                     if Vectors[i].IsMissing[j] then s:='';
                     ADataSet.SetFieldData(AField,pchar(s));
                     //sv:= TEpiStringVector(Vectors[i+1]);
                     //ADataSet.SetFieldData(aField,pchar(sv.FData[j-1]));  //  sv.FArray + ((j-1)* sv.DataSize));

                   end;
                 ftDate,ftToday,ftEuroDate,ftEuroToday,ftYMDDate,ftYMDToday:
                   begin
                     dt1:= Vectors[i].asDate[j];
                     if dt1=NA_DATE then ADataSet.SetFieldData(aField,nil)
                     else ADataSet.SetFieldData(aField,@dt1);
                   end;
               end;  //case
               inc(listpointer);
             end;//for i
           //ADataSet.Deleted:=Deleted[j];
           //ADataSet.Verified:=Verified[j];
           if assigned(fRecStatusVector) then
             begin  // TODO -OMichael : Omskriv til IKKE at bruge EpiRecDeleted og EpiRecVerified, men .Deleted[i] og .Verified[i]
               ADataSet.Deleted :=((fRecStatusVector.AsInteger[j] AND EpiRecDeleted)=EpiRecDeleted);
               ADataSet.Verified :=((fRecStatusVector.AsInteger[j] AND EpiRecVerified)=EpiRecVerified);
             end
           else
             begin
               ADataSet.Deleted:=false;
               ADataSet.Verified:=false;
             end;

           ADataSet.Post;
         end;// for j
     except
       on E: EInOutError do
         begin
           Result:=-1;
           CheckfileError(ErrorCode);
         end;
     end;  //try..except
  finally
    List.free;
  end;
end;

{
procedure TEpiDataFrame.CopyLabels(ADataset:TEpiDataSet);
var
 i,co : integer;
 val :TLabelValueList;
begin
 fSupportsLabels:=Adataset.SupportsLabels;
 if not fSupportsLabels then exit;
 co := Adataset.GetLabelsCount;
 for i:= 0 to co-1 do
 begin
    val := Adataset.GetValueLabel(i);
    LabelBlocks.AddObject(val.LabelName ,val);
 end;
end;
}
{
procedure TEpiDataFrame.LinkLabelBlocks;
var
 i,co : integer;
 val :TLabelValueList;
begin
 if not SupportsLabels then exit;
 co := VectorCount;
 for i:= 0 to co-1 do
 begin
    vectors[i].CheckProperties.ValueLabelSet := GetValueBlockPointer(vectors[i].ValueLabelName);
 end;
end;     }


function TEpiDataFrame.GetValueBlockPointer(const labname: string): TLabelValueList;
var
 idx : integer;
begin
 result:=nil;
 if not fCheckProperties.ValueLabels.Find(labname, idx) then exit;
 result:=TLabelValueList(fCheckProperties.ValueLabels.objects[idx]);
end;

function TEpiDataFrame.GetSupportsLabels: boolean;
begin
  Result := true;//fSupportsLabels and (LabelBlocks<> nil);
end;


procedure TEpiDataFrame.SetRowCount(const Value: integer);
begin
  if value = fRowCount then exit;
  fRowCount := Value;
end;

function TEpiDataFrame.GetCapacity: integer;
begin
  Result := Vectors.capacity
end;

procedure TEpiDataFrame.SetCapacity(const Value: integer);
var
 i,co : integer;
begin
  if (Capacity=Value) then exit;
{  if Value < Rowcount then
       error('Can''t reduce capacity below count');}
   vectors.Capacity := Value;
   RowCount := Value;
end;


function TEpiDataFrame.GetDeleted(Index: integer): boolean;
begin
  Result:= fRecStatusVector[Index] and EpiRecDeleted = EpiRecDeleted
end;

procedure TEpiDataFrame.SetDeleted(Index: integer; const Value: boolean);
begin
  if value then
    fRecStatusVector[Index]:= fRecStatusVector[Index] or EpiRecDeleted
  else
    fRecStatusVector[Index]:= fRecStatusVector[Index] and (not EpiRecDeleted);
end;

function TEpiDataFrame.GetVerified(Index: integer): boolean;
begin
  Result := fRecStatusVector[Index] and EpiRecVerified = EpiRecVerified
end;

procedure TEpiDataFrame.SetVerified(Index: integer; const Value: boolean);
begin
  if value then
    fRecStatusVector[Index]:= fRecStatusVector[Index] or EpiRecVerified
  else
    fRecStatusVector[Index]:= fRecStatusVector[Index] and (not EpiRecVerified);
end;

function TEpiDataFrame.GetSelected(Index: integer): boolean;
begin
  Result := fSelectVector[Index] = EpiRecSelected;
end;

procedure TEpiDataFrame.SetSelected(Index: integer; const Value: boolean);
begin
  if value then
    fSelectVector[Index] := EpiRecSelected
  else
    fSelectVector[Index] := EpiRecDeSelected;
end;

function TEpiDataFrame.GetRecordNo(index: integer): EpiInt;
begin
  result := fRowNoVector[index];
end;


procedure TEpiDataFrame.SetErrorcount(const Value: integer);
begin
  FErrorcount := Value;
end;

{
function TEpiDataFrame.RowSelectToRowNoselect(aRowNo: integer): integer;
begin
  //
end;
}

function TEpiDataFrame.RowNoSelectToRowSelect(aRowNo: integer): integer;
var
  i,k : integer;
  vec: TEpiVector;
begin
  k := 0;
  // We might be dealing with a temporary IF, so we need to apply this to the
  // original selectvector if it exist!
  if Assigned(fBackupSelector) then
    vec := fBackupSelector
  else
    vec := fSelectVector;
  for i := 1 to RowCount do
  begin
    if vec.AsInteger[i] = EpiRecDeselected then continue;
    inc(k);
    result := i;
    if k = aRowNo then exit;
  end;
end;

procedure TEpiDataFrame.SetFileName(const Value: TEpiFileName);
begin
  fFileName := Value;
end;

procedure TEpiDataFrame.SetOpenMode(const Value: Epiint);
begin

end;

constructor TEpiVectors.Create(ADataFrame: TEpiDataFrame;pOwned:boolean=true);
begin
  FList := TList.Create;
  FDataFrame := ADataFrame;
  fOwned :=pOwned;
end;

destructor TEpiVectors.Destroy;
begin
  if FList <> nil then Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TEpiVectors.Changed;
begin
//  if (FDataSet <> nil) and not (csDestroying in FDataSet.ComponentState) then
//    FDataSet.DataEvent(deVectorListChange, 0);
//  if Assigned(OnChange) then OnChange(Self);
end;

{procedure TEpiVectors.CheckVectorKind(VectorKind: TVectorKind; Vector: TEpiVector);
begin
  if not (VectorKind in ValidVectorKinds) then
    DatabaseError(SInvalidVectorKind, Vector);
end;}

procedure TEpiVectors.Add(Vector: TEpiVector);
begin
  FList.Add(Vector);
  if Owned then
     Vector.fVectors := Self;
  Changed;
end;

procedure TEpiVectors.Add(Vector: TEpiVector; Idx: integer);
begin
  FList.Insert(Idx, Vector);
  if Owned then
     Vector.fVectors := Self;
  Changed;
end;

procedure TEpiVectors.AddNew(Vector: TEpiVector);
var
  i : integer;
begin
  flist.Add(Vector);
  Vector.fVectors := Self;
  Changed;
end;


procedure TEpiVectors.Remove(Vector: TEpiVector);
begin
  FList.Remove(Vector);
  if Owned then
    Vector.fVectors := nil;
  Changed;
end;

procedure TEpiVectors.Clear;
var
  F: TEpiVector;
begin
  while FList.Count > 0 do
  begin
    if owned then
    begin
      F := FList.Last;
      FreeAndNil(F);
    end;
    FList.Delete(FList.Count-1);
  end;
  Changed;
end;

function TEpiVectors.GetVector(Index: Integer): TEpiVector;
begin
    Result := FList[Index];
end;

function TEpiVectors.GetCount: Integer;
begin
    Result := FList.Count;
end;

function TEpiVectors.IndexOf(Vector: TEpiVector): Integer;
begin
  Result := FList.IndexOf(Vector);
end;

procedure TEpiVectors.CheckVectorName(const VectorName: string);
begin
  if VectorName = '' then Error(EpiVectorNameMissing);
  if FindVector(VectorName) <> nil then
    Errorfmt(EpiDuplicateName,[VectorName]);
end;

procedure TEpiVectors.CheckVectorNames(const VectorNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
{  while Pos <= Length(VectorNames) do                      
    VectorByName(ExtractVectorName(VectorNames, Pos));}
end;

procedure TEpiVectors.GetVectorNames(List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to FList.Count - 1 do
      List.Add(TEpiVector(FList.Items[I]).Name)
  finally
    List.EndUpdate;
  end;
end;

function TEpiVectors.FindVector(const VectorName: string): TEpiVector;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Result := FList.Items[I];
    if AnsiCompareText(Result.Name, VectorName) = 0 then Exit;
  end;
  Result := nil;
end;

function TEpiVectors.VectorByName(const VectorName: string): TEpiVector;
begin
  Result := FindVector(VectorName);
  if Result = nil then ErrorFmt(EpiVectorNotFound, [VectorName]);
end;

{
function TEpiVectors.VectorByNumber(VectorNo: Integer): TEpiVector;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Result := FList.Items[I];
//    if Result.VectorNo = VectorNo then Exit;
  end;
  Result := nil;
end;
}

function TEpiVectors.GetVectorIndex(const VectorName: string): Integer;
begin
  for Result := 0 to FList.Count - 1 do
  begin
    if AnsiCompareText(TEpiVector(FList.Items[Result]).Name, VectorName) = 0 then Exit;
  end;
  Result := -1;
end;


procedure TEpiVectors.SetVectorIndex(Vector: TEpiVector; Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := FList.IndexOf(Vector);
  if CurIndex >= 0 then
  begin
    Count := FList.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> CurIndex then
    begin
      FList.Delete(CurIndex);
      FList.Insert(Value, Vector);
//      Vector.PropertyChanged(True);
      Changed;
    end;
  end;
end;


function TEpiVectors.GetCapacity: integer;
begin
  Result:=0;
  if Count< 1 then exit;
  result:= Vectors[0].Length;
end;

procedure TEpiVectors.SetCapacity(const Value: integer);
var
 i,co : integer;
begin
  if (Capacity=Value) then exit;
  co := Count;
  for i:= 0 to co-1 do
    vectors[i].Capacity := Value;
  DataFrame.fRowNoVector.Capacity := Value;
  DataFrame.fSelectVector.Capacity := Value;
  DataFrame.fRecStatusVector.Capacity := Value;
end;


function TEpiDataFrame.UpdateVectorRow(V: TEpiVector; Exp: IValue;aRow:integer): Epiint;
var
 oldrow,co : integer;
begin
 oldrow :=frowno;
 frowno :=aRow;
try
try
 case v.FieldDataType of
 EpiTyBoolean:
  begin
    if not Exp.CanReadAs(ttBoolean) then
       error('Data type mismatch');
     v.Asboolean[aRow] :=Exp.AsBoolean;
  end;
  EpiTyInteger:
  begin
    if not Exp.CanReadAs(ttInteger) then
       error('Data type mismatch');
      v.AsInteger[aRow] :=Exp.AsInteger;
  end;
  EpiTyDate:
  begin
    if not Exp.CanReadAs(ttInteger) then
       error('Data type mismatch');
       v.AsDate[aRow] :=Exp.AsInteger;
  end;
  EpiTyString,EpiTyByte,EpiTyUppercase:
  begin
    if not Exp.CanReadAs(ttString) then
       error('Data type mismatch');
       v.AsString[aRow] :=Exp.AsString;
  end;
  EpiTyFloat:
  begin
    if not Exp.CanReadAs(ttFloat) then
       error('Data type mismatch');
       v.AsFloat[aRow] :=Exp.AsFloat;
  end;
 end;//case
 fModified := true;
except
  raise;
end;
finally
 frowno:=oldrow;
end;
end;


function TEpiDataFrame.UpdateVector(V: TEpiVector; Exp: IValue): Epiint;
var
 i,co : integer;
begin
 co := RowCount;
 case v.FieldDataType of
 EpiTyBoolean:
  begin
    if not Exp.CanReadAs(ttBoolean) then
       error('Data type mismatch');
//    co := v.Length;
    for i:= 1 to co do
    begin
      if not Selected[i] then continue;
       frowno :=i;
       v.Asboolean[i] :=Exp.AsBoolean;
    end;
  end;
  EpiTyInteger:
  begin
    if not Exp.CanReadAs(ttInteger) then
       error('Data type mismatch');
//    co := v.Length;
    for i:= 1 to co do
    begin
      if not Selected[i]  then continue;
      frowno :=i;
      v.AsInteger[i] :=Exp.AsInteger;
    end;
  end;
  EpiTyDate:
  begin
    if not Exp.CanReadAs(ttInteger) then
       error('Data type mismatch');
//    co := v.Length;
    for i:= 1 to co do
    begin
      if not Selected[i]  then continue;
       frowno :=i;
       v.AsDate[i] :=Exp.AsInteger;
    end;
  end;
  EpiTyString,EpiTyByte,EpiTyUppercase:
  begin
    if not Exp.CanReadAs(ttString) then
       error('Data type mismatch');
//    co := v.Length;
    for i:= 1 to co do
    begin
      if not Selected[i]  then continue;
       frowno :=i;
       v.AsString[i] := Exp.AsString;
    end;
  end;
  EpiTyFloat:
  begin
    if not Exp.CanReadAs(ttFloat) then
       error('Data type mismatch');
//    co := v.Length;
    for i:= 1 to co do
    begin
      if not Selected[i]  then continue;
       frowno :=i;
       if Exp.AsFloat = NA_INT then
         v.AsFloat[i] := NA_FLOAT
       else
         v.AsFloat[i] :=Exp.AsFloat;
    end;
  end;
 end;//case
 fModified := true;
end;

function TEpiDataFrame.ApplySelection(Exp: IValue; opposite: boolean): EpiInt;
var
  i,co : integer;
begin
  co := RowCount;
  result := 0;
  if Exp= nil then
  begin
    for i:= 1 to co do
      Selected[i] := True
  end
  else if opposite then
  begin
    for i:= 1 to co do
    begin
      frowno :=i;
      Selected[i] := Selected[i] and (Exp.AsBoolean);
//      if not Exp.AsBoolean then
//        fSelectVector.AsInteger[i] := EpiRecDeSelected //set selection bit if true
    end;
  end else begin
    for i:= 1 to co do
    begin
      frowno :=i;
      Selected[i] := Selected[i] and (not Exp.AsBoolean);
//      if Exp.AsBoolean then
//        fSelectVector.AsInteger[i] := EpiRecDeSelected //set selection bit if true
    end;
  end;
  fModified := true;
end;


function TEpiDataFrame.ApplySelectionOld(Exp: IValue): Epiint;
var
 i,co : integer;
begin
  co := RowCount;
  if Exp= nil then
    for i:= 1 to co do
       fSelectVector.AsInteger[i] := EpiRecSelected
  else
    for i:= 1 to co do
    begin
       frowno :=i;
       if fSelectVector.AsInteger[i] = EpiRecSelected then
          fSelectVector.AsBoolean[i] := Exp.AsBoolean;
    end;
  fModified := true;
end;


function TEpiDataFrame.FindVector(const VectorName: string): TEpiVector;
begin
  Result:= Vectors.FindVector(Vectorname);
end;


function TEpiDataFrame.NewVector(pVarDesc: TAnaVariableDescriptor): TEpiVector;
var
  v          :TEpiVector;
  i          : integer;
begin
  result := nil;
  if findVector(pVarDesc.Name) <> nil then
    error('Variable already exist');
  case pVarDesc.datatype of
  EpiTyBoolean:
  begin
    v:= TEpiBoolVector.Create(pVarDesc.Name ,RowCount, fCheckProperties);
  end;
  EpiTyByte:
  begin
    v:= TEpiByteVector.Create(pVarDesc.Name ,RowCount, fCheckProperties);
  end;
  EpiTyInteger:
  begin
    v:= TEpiIntVector.Create(pVarDesc.Name ,RowCount, fCheckProperties);
  end;
  EpiTyString, EpiTyUppercase:
  begin
    v:= TEpiStringVector.Create(pVarDesc.Name ,RowCount,
      pVarDesc.Length, fCheckProperties);
    if pVarDesc.datatype = EpiTyUppercase then
      TEpiStringVector(v).FUpperCase := true;
  end;
  EpiTyFloat:
  begin
   v:= TEpiFloatVector.Create(pVarDesc.Name ,RowCount, fCheckProperties)
  end;
  EpiTyDate:
  begin
   v:= TEpiDateVector.Create(pVarDesc.Name ,RowCount, fCheckProperties)
  end
  else
    error('Unknown data type');
  end;//case
  v.FieldDataType:= pVarDesc.datatype;
  v.FieldDataSize:= pVarDesc.Length;
  v.FieldDataDecimals:= pVarDesc.Decimals;
  {#TODO1 change to file format-independent data format}
  v.FieldDataFormat:= pVarDesc.Format;
  //  v.fFieldUserData := TEpiInfoCustomFieldUserData.Create;
  v.SetAllMissing;
  vectors.AddNew(v);
  result := v;
  fModified := true;
end;

function TEpiDataFrame.GetVectorNames(Varnames: TStrings; Datatypes: TEpiDatatypes = []): TStrings;
var
  i, j, vbidx, veidx: integer;
  varname, vb,ve :string;

  procedure AddUnique(const S: string; List: TStrings);
  begin
    if List.IndexOf(S) > 0 then exit;
    List.Add(S); 
  end;

begin
  result := TStringList.Create();
  if not Assigned(VarNames) then
    Vectors.GetVectorNames(Result)
  else begin
    Result.BeginUpdate;
    for i:=0 to varnames.Count-1 do
    begin
      varname := trim(Varnames[i]);
      if varname='*' then begin
        for j := 0 to Vectors.Count -1 do
          AddUnique(Vectors[j].Name, Result);
        Result.EndUpdate;
        break;
      end;
      if (pos('*',varname) > 0) or (pos('?',varname) > 0) or (pos('[',varname) > 0)then
      begin
        for j:=0 to Vectors.Count-1 do
          if MatchString(varname, Vectors[j].name, 0).BPos > -1 then
            AddUnique(Vectors[j].Name, Result);
        continue;
      end;
      j := pos('-',varname);
      if j > 0 then
      begin
          vb := trim(copy(varname, 1, j-1));
          vbidx := Vectors.GetVectorIndex(vb);
          if vbidx = -1 then
             ErrorFmt(EpiVectorNotFound, [vb]);
          ve := trim(copy(varname, j+1, length(varname)));
          veidx := Vectors.GetVectorIndex(ve);
          if veidx = -1 then
             ErrorFmt(EpiVectorNotFound, [ve]);
          for j := vbidx to veidx do
             AddUnique(Vectors[j].Name, Result);
          continue;
      end;
      if (VectorByName[varname] <> nil) then
        AddUnique(varname, Result);
    end;
    Result.EndUpdate;
  end;
  if Datatypes = [] then exit;
  for i := result.Count-1 downto 0 do
    if not (Vectors.VectorByName(result[i]).DataType in Datatypes) then
      result.Delete(i);
end;

function TEpiDataFrame.GetVectorListByName(varnames: Tstrings): TEpiVectors;
var
  vectornames: TStrings;
  i: integer;
begin
  result:= TEpiVectors.create(self, false);
  vectornames := GetVectorNames(Varnames);
  for i := 0 to vectornames.Count-1 do
    result.Add(Vectors.VectorByName(vectornames[i]));
  FreeAndNil(Vectornames);
end;


function TEpiDataFrame.InternalCompareRecords(Record1,Record2:integer): Integer;
var
  fld:TEpiVector;
  i:integer;
begin
  Result:=0;
  // Always put deselected records last when sorting...
  if (not Selected[Record1]) and (Selected[Record2]) then result := 1;
  if (not Selected[Record2]) and (Selected[Record1]) then result := -1;
  if result = 0 then
    for i:=0 to Sortlist.Count-1 do
    begin
      result:= TEpiVector(Sortlist[i]).compare(Record1,Record2);
      if Result<>0 then break;
    end;
//     if (Descending in FSortOptions) then Result:=-Result;
end;

procedure TEpiDataFrame.InternalSort(L, R: Integer);
var
   I, J, P: Integer;
begin
  I:=L;
  J:=R;
  P:=(L + R) shr 1;
  repeat
    while InternalCompareRecords(I,P) < 0 do Inc(I);
    while InternalCompareRecords(J,P) > 0 do Dec(J);
    if I <= J then
    begin
      Exchange(J,I);
      if p=i then p:=j
      else if p=j then p:=i;
      Inc(I);
      Dec(J);
    end;
  until I>J;
  if L<J then InternalSort(L,J);
  if I < R then InternalSort(I, R);
end;

procedure TEpiDataFrame.Exchange(I,J: Integer);
var
 m,co : integer;
 v    : TEpiVector;
begin
  co := VectorCount;
  for m:= 0 to co-1 do
  begin
    v := Vectors[m];
    v.exchange(I,J);
  end;
  fSelectVector.Exchange(I,J);
  fRowNoVector.Exchange(I,J);
  fRecStatusVector.Exchange(I,J);
end;

function TEpiDataFrame.Sort(VectList: TEpiVectors): boolean;
var
 i, co : integer;
begin
  Sortlist.clear;
  for i:= 0 to VectList.count-1 do
     Sortlist.add(VectList[i]);
  InternalSort(1,RowCount);
  RebuildRecnumber();
  fModified := true;
end;

function TEpiDataFrame.Sort(const varname:string): boolean;
var
  VectList: TEpiVectors;
  Varnames :TStringList;
 i : integer;
begin
  Vectlist:=nil;
  varnames:=nil;
  try
    try
       Varnames :=TStringList.Create;
       Varnames.CommaText:=varname;
       VectList:=GetVectorListByName(Varnames);
       Sortlist.clear;
       for i:= 0 to VectList.count-1 do
          Sortlist.add(VectList[i]);
       InternalSort(1,RowCount);
       RebuildRecnumber();
       fModified := true;
    except
      raise;
    end;
  finally
    Varnames.free;
    VectList.free;
  end;
end;

function TEpiDataFrame.RebuildRecnumber():boolean;
var
  i, k: integer;
  v: TEpiVector;
begin
  k := 1;
  for i:=1 to RowCount do
  begin
    if fSelectVector.asInteger[i] = EpiRecDeselected then
      fRowNoVector.asInteger[i] := NA_INT
    else
    begin
      fRowNoVector.asInteger[i] := k;
      inc(k);
    end;
  end;
end;


function TEpiDataFrame.Clone(varnames: Tstrings): TEpiDataFrame; deprecated;
var
 i, co : integer;
 v : TEpiVector;
begin
  Result := PrepareDataframe(varnames, nil);
  {
  Result:= TEpiDataFrame.CreateTemp(fRowCount);
  fCheckProperties.Clone(Result.fCheckProperties);
  co := varnames.count;
  for i:= 0 to co-1 do
  begin
     v := Vectors.FindVector(varnames[i]);
     Result.Vectors.Add(v.Clone(Result.fCheckProperties));
  end;
  frowNo :=1;
  Result.fRowNo := fRowno;
  Result.fRowCount := fRowCount;    }
end;

function TEpiDataFrame.DropVectors(varnames: Tstrings): boolean;
var
 i, co : integer;
 v : TEpiVector;
begin
  Result:=False;
  co :=varnames.count;
  for i:= 0 to co-1 do
  begin
     v := Vectors.FindVector(varnames[i]);
     if v <> nil then
     begin
       Vectors.Remove(v);
       FreeAndNil(v);
     end;
  end;
  fModified := true;
  Result:=true;
end;


(*************************************************
 * Generates a TStatTable table with the content *
 * of the dataframe base on which varnames has   *
 * been chosen.                                  *
 * Varnames can be nil - then the full dataframe *
 * is returned.                                  *
 *************************************************)
function TEpiDataframe.ToStattable(Varnames: TStrings): TStatTable;
var
  VectorList: TEpiVectors;
  i, j: integer;
begin
  VectorList := GetVectorListByName(Varnames);
  result := dm.CodeMaker.Output.NewTable(VectorList.Count, RowCount+1);

  for i := 0 to VectorList.Count-1 do
  begin
    result.Cell[i+1,1] := Vectorlist[i].Name;
    for j := 1 to RowCount do
    begin
      result.Cell[i+1, j+1] := VectorList[i].AsString[j];
    end;
  end;
  FreeAndNil(VectorList);
end;

(*****************************************
 * Send the dataframe to output using an *
 * TStatTable (regular formatted table). *
 * This procedure is meant for internal  *
 * testing purpose only.                 *
 * Perhaps as a possible replacement for *
 * the "list" command later.             *
 *****************************************)
procedure TEpiDataFrame.SendToOutput(Varnames: TStrings);
begin
  dm.CodeMaker.OutputTable(ToStattable(Varnames), '');
  dm.Sendoutput();
end;

function TEpiDataFrame.FormatVectors(varnames: Tstrings;const fmt:String): boolean;
var
 i, co : integer;
 v : TEpiVector;
begin
  Result:=False;
  co :=varnames.count;
  for i:= 0 to co-1 do
  begin
    v := Vectors.FindVector(varnames[i]);
    if v<> nil then v.FieldFormat:=fmt;
  end;
  fModified := true;
  Result:=true;
end;

(***********************************************************************
 * PrepareDataframe:                                                   *
 *    Makes a copy of the original dataframe, given certain criterias: *
 *    - CloneVectors: A list with names of vectors to be cloned        *
 *    - MissingVectors: Check this list of vectors for missing values  *
 *         (either noted as "." or self defined) and do NOT copy this  *
 *         record if missing is true;                                  *
 *    The copy will contain all internal vectors "__n_", "__s_" and    *
 *         "__r_"                                                      *
 *    Preserves any sort order present.                                *
 ***********************************************************************)
function TEpiDataFrame.prepareDataframe(var CloneVectors: TStrings; MissingVectors: TStrings; Datatypes: TEpiDatatypes = []): TepiDataframe;
var
  rc, vc, src, i,j, k  : integer;
  TempVector, OrgVector, OldSelectVector: TEpiVector;
  s: string;

begin
  TempVector := nil;
  Result := nil;
  OldSelectVector := nil;
  try
    try
      CloneVectors := self.GetVectorNames(CloneVectors, Datatypes);

      vc := VectorCount;
      rc := RowCount;

      OldSelectVector := fSelectVector.Clone(self, false);

      if Assigned(MissingVectors) then
      begin
        for j:=0 to (MissingVectors.Count-1) do
        begin
          TempVector := Vectors.VectorByName(MissingVectors[j]);
          for i := 1 to rc do
          begin
            if not selected[i] then
              continue
            else
              if TempVector.IsMissing[i] or TempVector.IsMissingValue[i]
                then selected[i] := false;
          end;
        end;
      end;

      src := SelectedRowCount;
      result := TEpiDataFrame.CreateTemp(src);
      fCheckProperties.Clone(result.CheckProperties);

      // Copying filename MUST be done here - otherwise cloning vectors will
      // break labelvalues.
      Result.FileName := self.FileName;

      k:=1;
      for j:= 0 to (CloneVectors.Count-1) do
      begin
        k := 1;
        OrgVector := Vectors.VectorByName(CloneVectors[j]);
        TempVector := OrgVector.Clone(result, true);
        for i := 1 to rc do
        begin
          if selected[i] then
          begin
            // this is an ugly hack, but the .Value[] function is not implemented well
            // in most of the vector types... :(  (Torsten Christiansen 2004-06-29)
            case OrgVector.DataType of
              EpiTyInteger: TempVector.AsInteger[k] := OrgVector.AsInteger[i];
              EpiTyFloat:   TempVector.AsFloat[k] := OrgVector.AsFloat[i];
              EpiTyDate:    TempVector.AsDate[k] := OrgVector.AsDate[i];
              EpiTyString, EpiTyUppercase,
              EpiTyByte:    TempVector.AsString[k] := OrgVector.AsString[i];
              EpiTyBoolean: TempVector.AsByte[k] := OrgVector.AsByte[i];
            end; //case
            inc(k);
          end; //if
        end; // for i
        TempVector.Length := k-1;
        Result.Vectors.Add(TempVector);
      end;  // for j

      Result.RowNo := 1;
      Result.RowCount := k-1;
      k:=1;
      for i:=1 to rc do
      begin
        if selected[i] then
          begin
            result.fRecStatusVector.AsInteger[k] := fRecStatusVector.AsInteger[i];
            inc(k);
          end;  //if selected
      end;  //for i
      Result.RebuildRecnumber();
      Result.DataLabel := self.DataLabel;
      fSelectVector.AssignData(OldSelectVector);
    except
      if Assigned(result) then FreeAndNil(Result);
      raise;
    end;
  finally
    if Assigned(OldSelectVector) then FreeAndNil(OldSelectVector);
  end;
end;

function TEpiDataframe.ExpandDataframe(GroupVarnames: TStrings; aBegin: variant; aEnd: variant): TEpiDataframe;
var
  i, j, k, d, diff, adddiff, rc: integer;
  maxval: variant;
  minvalues, maxvalues: array of variant;
  vec, orgvec: TEpiVector;
  df, adddf: TEpiDataframe;
  dummy: string;
  List: TStrings;

  function LevelChanged(level: integer): boolean;
  begin
    result := true;
    if level >= GroupVarnames.Count then exit;
    if k = 1 then
      result := (AddDF.VectorByName[GroupVarnames[level]].AsInteger[k] -
                df.VectorByName[GroupVarnames[level]].AsInteger[i]) < 0
    else
      result := AddDF.VectorByName[GroupVarnames[level]].compare(k, k-1) < 0
  end;

  function NextLevelInt(index: integer): EpiInt;
  begin
    if LevelChanged(index+1) then
      if index = 0 then
        if k = 1 then
          if i = 0 then
            result := aBegin + 1
          else //i = 0
            result := orgvec.AsInteger[i] + 1
        else
          result := vec.AsInteger[k-1] + 1
      else begin
        result := ((orgvec.AsInteger[i] - minvalues[j-1] + k) mod
                   (maxvalues[j-1] - minvalues[j-1] + 1)) +
                  (minvalues[j-1]);
      end
    else
      if k = 1 then
        result := orgvec.AsInteger[i]
      else
        result := vec.AsInteger[k-1];
  end;

  function NextLevelDate(index: integer): EpiDate;
  begin
    if LevelChanged(index+1) then
      if k = 1 then
        if i = 0 then
          result := aBegin + 1
        else //i = 0
          result := orgvec.AsDate[i] + 1
      else // k = 1
        result := vec.AsDate[k-1] + 1
    else // levelchanged
      if k = 1 then
        result := orgvec.AsDate[i]
      else
        result := vec.AsDate[k-1];
  end;

  function NextLevelFloat(index: integer): EpiFloat;
  begin
    if LevelChanged(index+1) then
      if k = 1 then
        if i = 0 then
          result := aBegin + Power(10, -d)
        else //i = 0
          result := orgvec.AsFloat[i] + Power(10, -d)
      else
        result := vec.AsFloat[k-1] + Power(10, -d)
    else
      if k = 1 then
        result := orgvec.AsFloat[i]
      else
        result := vec.AsFloat[k-1];
  end;

const
  procname = 'ExpandDataframe';
  procversion = '1.0.0.1';         // 1: round for decimals >  .1
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  result := nil;
  // Various errorchecks
  if GroupVarnames = nil then exit;
  if GroupVarnames.Count = 0 then exit;
  if not (VectorByName[GroupVarnames[0]].DataType in [EpiTyDate, EpiTyInteger, EpiTyFloat]) then
    raise Exception.Create('Error expanding dataframe: First groupname must be of type Date, Integer or Float.');

  // TODO : Use a copy of the dataframe instead of SELF.
  //  List := nil; //TStringList.Create();
  df := self; //.PrepareDataframe(List, nil);

  // Find the maximum defined values of the valuelabels.
  // Here we assume that labelvalue are in sorted order!
  setlength(MaxValues, GroupVarnames.Count-1);
  setlength(MinValues, GroupVarnames.Count-1);
  for i := 1 to GroupVarnames.Count-1 do
  begin
    vec := df.VectorByName[GroupVarnames[i]];
    if vec.HasValueLabels() then
    begin
      minvalues[i-1] := StrToInt(pchar(vec.CheckProperties.ValueLabelSet[0]));
      maxvalues[i-1] := StrToInt(pchar(vec.CheckProperties.ValueLabelSet[vec.CheckProperties.ValueLabelSet.Count-1]));
    end
    else
      raise Exception.Create('Error expanding dataframe: ' + vec.fName + ' must have value label');
  end;

  df.Sort(GroupVarnames.CommaText);

  i := 0;
  while i <= df.RowCount do
  begin
    // Find difference between two consecutive rows in the dataframe.
    // In the following it is assumed that if vec contains missing data, then
    //   it is sorted last in the vector.
    vec := df.VectorByName[GroupVarnames[0]];
    if i=0 then
      case vec.DataType of
        EpiTyInteger,
        EpiTyDate:  Diff := vec.AsInteger[i+1] - integer(aBegin);
        EpiTyFloat: begin
                      d := vec.FieldDataDecimals;
                      if d > 1 then d := 1;  // TODO : check if 2 decimals as TC did in first v.
                      Diff := Round((R2(vec.AsFloat[i+1], -d) -
                                     R2(EpiFloat(aBegin), -d))*
                                     IntPower(10,d));
                    end;
      end
    else if (i=df.RowCount) or (vec.IsMissing[i+1]) then
      case vec.DataType of
        EpiTyInteger,
        EpiTyDate:  Diff := integer(aEnd) - vec.AsInteger[i];
        EpiTyFloat: begin
                      d := vec.FieldDataDecimals;
                      if d > 1 then d := 1;
                      Diff := Round((R2(EpiFloat(aEnd), -d) -
                                     R2(vec.AsFloat[i], -d))*
                                     IntPower(10,d));
                    end;
      end
    else //Default case!
      case vec.DataType of
        EpiTyInteger,
        EpiTyDate:  Diff := vec.AsInteger[i+1] - vec.AsInteger[i];
        EpiTyFloat: begin
                      d := vec.FieldDataDecimals;
                      if d > 1 then d := 1;
                      Diff := Round((R2(vec.AsFloat[i+1], -d) -
                                     R2(vec.AsFloat[i], -d))*
                                     IntPower(10,d));
                    end;
      end;

    for j := 1 to GroupVarnames.Count - 1 do
      diff := diff * (maxvalues[j-1] - minvalues[j-1] + 1);
    for j := 1 to GroupVarnames.Count-1 do
    begin
      vec := df.VectorByName[GroupVarnames[j]];
      adddiff := vec.compare(i+1,i);
      for k := j+1 to GroupVarnames.Count - 1 do
        adddiff := adddiff * (maxvalues[k-1] - minvalues[k-1] + 1);
      diff := diff+adddiff;
    end;


    if (i<df.rowcount) and vec.IsMissing[i+1] then
      i := df.RowCount;
    if Diff > 100 then
      dm.Info('Warning: Large gaps. To cancel process press ESC or click stop button.', [], 209003);
    if Diff > 1 then
    begin
      // Create the DF to be inserted.
      AddDF := TEpiDataFrame.CreateTemp(Diff -1);
      fCheckProperties.Clone(AddDF.CheckProperties);
      for j := 0 to df.VectorCount-1 do
      begin
        //if (df.Vectors[j].internal) then continue;
        orgvec := df.VectorByName[df.Vectors[j].Name];
        vec := orgvec.Clone(AddDF, true);
        vec.Length := Diff -1;
        AddDF.Vectors.Add(vec);
      end;

      // Insert the data missing for grouping variables.
      for k := 1 to diff -1 do
      begin
        if (k mod 100) = 0 then application.ProcessMessages;
        if dm.Cancelled then dm.Error('Cancelled', [], 109002);

        for j := GroupVarnames.Count-1 downto 0 do
        begin
          orgvec := df.VectorByName[GroupVarnames[j]];
          vec := AddDF.VectorByName[GroupVarnames[j]];

          case orgvec.DataType of
            EpiTyInteger: vec.AsInteger[k] := NextLevelInt(j);
            EpiTyDate:    vec.AsDate[k] := NextLevelDate(j); //orgvec.AsInteger[i] + k;
            EpiTyFloat:   vec.AsFloat[k] := NextLevelFloat(j);// orgvec.AsFloat[i] + Power(10, -d) * k;
          end;
        end;
      end;
      // Insert data for non-grouping variables vectors.
      for j := 0 to df.VectorCount-1 do
      begin
        if //(df.Vectors[j].internal) or
           (GroupVarnames.IndexOf(df.Vectors[j].Name) <> -1) then continue;
        orgvec := df.VectorByName[df.Vectors[j].Name];
        vec := AddDF.VectorByName[df.Vectors[j].Name];
        for k := 1 to diff -1 do
        begin
          vec.IsMissing[k] := true;
          if (k mod 100) = 0 then application.ProcessMessages;
          if dm.Cancelled then dm.Error('Cancelled', [], 109002);
        end;
      end;
      // TODO : Switch to own append procedure!
      df.AppendNewRows(AddDF, dummy, dummy, dummy);
      df.Sort(GroupVarnames.CommaText);
      inc(i, diff-1)
    end;
    inc(i);
  end;
  result := df;
  ODebug.DecIndent;
end;

function TEpiDataframe.ConvertVector(Name: string; DataType: word; const txt : string): boolean;
var
  oldv, newv: TEpiVector;
  i: integer;
  slist: TStringList;
  s : String;
begin
  oldv := Vectors.VectorByName(name);
  if oldv.DataType = datatype then exit;
  case datatype of
    EpiTyBoolean: newv := TEpiBoolVector.Create(Name ,RowCount, fCheckProperties);
    EpiTyByte:    newv := TEpiByteVector.Create(Name ,RowCount, fCheckProperties);
    EpiTyInteger: newv := TEpiIntVector.Create(Name ,RowCount, fCheckProperties);
    EpiTyUppercase,
    EpiTyString:  begin
                    newv := TEpiStringVector.Create(Name ,RowCount, oldv.DataSize, fCheckProperties);
                    if datatype = EpiTyUppercase then
                      TEpiStringVector(newv).FUpperCase := true;
                  end;
    EpiTyFloat:   newv := TEpiFloatVector.Create(Name ,RowCount, fCheckProperties);
    EpiTyDate:    newv := TEpiDateVector.Create(Name ,RowCount, fCheckProperties);
    else
      error('Unknown data type');
  end;//case
  case datatype of
    EpiTyBoolean: s := 'Boolean';
    EpiTyByte:    s := 'Byte';
    EpiTyInteger: s := 'Integer';
    EpiTyString:  s := 'String';
    EpiTyUppercase: s := 'Uppercase';
    EpiTyFloat:   s := 'Float';
    EpiTyDate:    s := 'Date';
    else
      s := ('Unknown data type');
  end;//case
  dm.info(Name + txt + s, [], 0);
  for i := 1 to RowCount do
  begin
    case datatype of
      EpiTyInteger: newv.AsInteger[i] := oldv.AsInteger[i];
      EpiTyFloat:   newv.AsFloat[i]   := oldv.AsFloat[i];
      EpiTyDate:    newv.AsDate[i]    := oldv.AsDate[i];
      EpiTyUppercase,
      EpiTyString:  newv.AsString[i]  := oldv.AsString[i];
      EpiTyByte:    newv.AsByte[i]    := oldv.AsByte[i];
      EpiTyBoolean: newv.AsBoolean[i] := oldv.AsBoolean[i];
    end;
  end;
  newv.FieldDataType      := datatype;
  newv.FieldDataSize      := oldv.Length;
  newv.FieldDataDecimals  := oldv.fFldDataDecimals;
  case datatype of
      EpiTyInteger: newv.FieldDataFormat := '%8d';
      EpiTyFloat:   newv.FieldDataFormat := '%11.6f';
      EpiTyDate:    newv.FieldDataFormat := '%DMY';
      EpiTyUppercase, EpiTyString, EpiTyByte,
      EpiTyBoolean: newv.FieldDataFormat := '';
  end;
  newv.fVariableLabel     := oldv.fVariableLabel;
  oldv.fCheckProperties.Clone(newv.fCheckProperties);
  newv.fCheckProperties.OrigFelttype := VectorType2EpiDataFieldType(oldv.DataType);

  i := Vectors.IndexOf(oldv);
  slist := TStringList.Create();
  slist.Add(name);
  DropVectors(slist);
  Vectors.Add(newv, i);
  FreeAndNil(slist);
  fModified := true;
  result := true;
end;

function TEpiDataFrame.CopyStruFrom(pDataFrame: TEpiDataFrame;var  RelatedVectors : TEpiRelatedVectorsArray; relatename: string): boolean;
var
 i,co,j,jj: integer;
 v,y : TEpiVector;
 vname :string;
begin
 result:=false;
 co := pDataFrame.VectorCount;
 setLength(RelatedVectors,co);
 j:=0;
try
 for i:= 0 to co-1 do
 begin
    v := pDataFrame.Vectors[i];
    //if v.Internal then continue;
//do we have a vector with the this name?
    vname := v.name;
    if trim(AnsiUppercase(vname)) = trim(AnsiUppercase(relatename)) then continue;
    jj:=1;
    while findVector(vname)<> nil do
    begin
       vname:=format('%s%d',[vname,jj]);
       if not IsValidEpiident(vname) then
       begin
         vname:=copy(vname,3,length(vname));
         jj:=0;
       end;
       inc(jj);
    end;
    y := v.clone(self, true);
    y.Name :=vname;
    y.Length:=capacity;
    self.Vectors.AddNew(y);
//master field now
    RelatedVectors[j].aV:=y;
//related field
    RelatedVectors[j].bV:=v;
    inc(j);
 end;
 setLength(RelatedVectors,j);
except
  result:=false;
  RelatedVectors :=nil;
end;
end;


procedure TEpiVector.Assign(source: TEPiVector;NoData:boolean=false);
begin
  FieldDataType := source.FieldDataType;
  FieldDataSize := source.FieldDataSize;
  FieldDataDecimals := source.FieldDataDecimals;
  if not nodata then AssignData(source);
end;

function TEpiVector.HasMissingValuesDefined(): Boolean;
var
  i: integer;
begin
  result:=false;
  if (not assigned(fCheckProperties)) then exit;
  try
    for i:=0 to MAXDEFINEDMISSINGVALUES do
      if fCheckProperties.MissingValues[i] <> '' then result := true;
  except
    exit;
  end
end;

function TEpiVector.HasValueLabels(): boolean;
begin
  result := Assigned(fCheckProperties.ValueLabelSet);
end;

constructor TEpiVector.Create(const VName:string; Size:EpiInt = -1; DfChkProp: TdfChkProp = nil);
begin
  inherited Create;
  fName :=Vname;
  fOrgLength:=Size;
  fCheckProperties := TVectorChkProp.Create;
  fCheckProperties.DfChkProp := DfChkProp;
  Initialize;
  SetAllMissing;
end;

destructor TEpiVector.Destroy;
begin
  finalize;
  fCheckProperties.Free;
  inherited;
end;


function TEpiVector.GetCapacity: EpiUnInt;
begin
  Result:=length;
end;

function TEpiVector.GetDataSize: EpiUnInt;
begin
 result:=0;
end;

function TEpiVector.GetDataType: EpiUnInt;
begin

end;


function TEpiVector.GetFieldDataDecimals: EpiUnInt;
begin
   Result:=fFldDataDecimals;
end;

function TEpiVector.GetFieldDataFormat: Epistring;
begin
    Result:=fFldDataFormat;
end;

function TEpiVector.GetFieldDataSize: EpiUnInt;
begin
   Result:=fFldDataSize;
end;

function TEpiVector.GetFieldDataType: EpiUnInt;
begin
 Result:=fFldDataType;
end;

function TEpiVector.GetIsSorted: epibool;
begin
  Result:=fIsSorted;
end;


function TEpiVector.GetName: string;
begin
  result:=fname;
end;


function TEpiVector.GetValue(const index: Integer): Epivariant;
begin

end;

procedure TEpiVector.Reset;
var
  i: integer;
begin
 dm.sysinfo('epivector reset');
  fIsSorted:=false;
end;

procedure TEpiVector.SetCapacity(const Value: EpiUnInt);
begin
  if value = Capacity then exit;
  Length := Value;
end;

procedure TEpiVector.SetFieldDataDecimals(const Value: EpiUnInt);
begin
  fFldDataDecimals:=value;
end;

procedure TEpiVector.SetFieldDataFormat(const Value: Epistring);
begin
   fFldDataFormat:=value;
   fFormat       :=fFldDataFormat;
end;

procedure TEpiVector.SetFieldDataSize(const Value: EpiUnInt);
begin
   fFldDataSize:=value;
end;

function TEpiVector.GetVariableLabel(Options: TObject = nil): string;
var
  opt: TEpiOption;
  v, n,
  varlabel, varname: string;
begin
  if Options <> nil then
  begin
    if TVarList(Options).VarByName['VN'] <> nil then n := 'VN';
    if TVarList(Options).VarByName['VNL'] <> nil then n := 'VNL';
  end;
  varname := trim(fName);
  varlabel := trim(fVariableLabel);
  result := varname;
  if n = 'VN' then
    result := varname
  else if n = 'VNL' then
    result := varname + ' ' + varlabel
  else
    if varlabel <> '' then result := varlabel;
end;

procedure TEpiVector.SetFieldDataType(const Value: EpiUnInt);
begin
 fFldDataType:=value;
end;

procedure TEpiVector.SetName(const Value: string);
begin
 Fname:=Value;
end;


procedure TEpiVector.SetValue(const index: Integer; const Value: Epivariant);
begin

end;

function TEpiVector.Sort(Sortoptions: TSortOptions): boolean;
begin
   InternalSort(0,pred(length),Sortoptions);
   fIsSorted:=true;
end;


function TEpiVector.BinarySearch(aItem    : pointer; var aInx : integer; Plength:integer) : boolean;
var
  L, R, M : integer;
  CompareResult : integer;
begin
  L := 1;
  R := pLength;
  while L <= R do begin
    M := (L + R) div 2;
    CompareResult := CompareItem(M, aItem);
    if (CompareResult < 0) then
      L := succ(M)
    else if (CompareResult > 0) then
      R := pred(M)
    else begin
      aInx := M;
      Result := true;
      Exit;
    end;
  end;
  aInx := L;
  Result := false;
end;

function TEpiVector.CompareItem(idx: integer; item: pointer): integer;
begin
  result:=0;
  if AsInteger[idx]> integer(item^) then result:= 1
  else if AsInteger[idx]< integer(item^) then result:= -1;
end;

procedure TEpiVector.InternalClone(dest: TEpiVector; NoData:boolean=false; SameDataframe: boolean = true);
begin
  dest.FieldFormat := FieldFormat;
  dest.fCapacity := fCapacity;
  dest.fIsSorted := fIsSorted;
  dest.fFldDataType := fFldDataType;
  dest.fFldDataSize := fFldDataSize;
  dest.fFldDataDecimals := fFldDataDecimals;
  dest.fFldDataFormat :=fFldDataFormat;
  dest.fVariableLabel := fVariableLabel;
  dest.fFormat :=fFormat;
  dest.fReadOnly := fReadOnly;
  dest.fRepeated := fRepeated;
  dest.fMustEnter := fMustEnter;
  fCheckProperties.Clone(dest.fCheckProperties, SameDataframe);
  dest.Assign(self, nodata);
end;

function TEpiVector.GetValueLabel(const value: String; Options: TObject = nil): string;
var
  Opt : TEpioption;
  v, vlabel: string;
begin
  Result:=value;
  if not HasValueLabels() then exit;
  vlabel := fCheckProperties.ValueLabelSet.Labels[value];
  //if (dm.GetOptionValue('SHOW VAR VALUE', Opt)) then v := AnsiUpperCase(opt.Value);
  if options <> nil then
  begin
    if TVarList(Options).VarbyName['VL'] <> nil then v := 'VL';
    if TVarList(Options).VarbyName['V'] <> nil then v := 'V';
  end;
  result := vlabel;
  if v = 'V' then result := value;
  if v = 'VL' then result := Value + ' ' + vlabel;
end;

function TEpiVector.GetFormat: Epistring;
begin
  Result:=fFormat;
end;

procedure TEpiVector.SetFormat(const Value: Epistring);
begin
 fFormat:=value;
end;

function TEpiVector.GetAsByte(const index: Integer): EpiByte;
begin

end;

procedure TEpiVector.SetAsByte(const index: Integer; const Value: EpiByte);
begin

end;


{ TEpiIntVector }


procedure TEpiIntVector.Finalize;
begin
  Reset;
end;

function TEpiIntVector.GetAsBoolean(const index: Integer): EpiBool;
begin
   result:=  AsInteger[index] > 0
end;

function TEpiIntVector.GetAsDate(const index: Integer): EpiDate;
begin

end;

function TEpiIntVector.GetAsFloat(const index: Integer): EpiFloat;
begin
  result :=  AsInteger[index];
  if result = NA_INT then
    result := NA_FLOAT;
end;

function TEpiIntVector.GetAsInteger(const index: Integer): EpiInt;
begin
{$IFDEF CHECKVRANGE}
   checkDataIndex(index);
{$ENDIF};
  Result:=Fdata[index-1];
end;

function TEpiIntVector.GetAsString(const index: Integer): EpiString;
begin
if IsMissing[Index] then
   result:= EpiMissingChar
else
   result:=EpiFormat(Fdata[index-1],Fieldformat);
end;

function TEpiIntVector.GetDataType: EpiUnInt;
begin
 result:=EpiTyInteger;
end;

function TEpiIntVector.GetVectorLength: EpiUnInt;
begin
  result:=System.length(Fdata);
end;

function TEpiIntVector.GetValue(const index: Integer): Epivariant;
begin
   result:=AsInteger[Index];
end;

procedure TEpiIntVector.initialize;
begin
  if fOrgLength= -1 then
     system.setlength(Fdata, 1000)
  else
     system.setlength(Fdata, fOrgLength);
  fCheckProperties.OrigFelttype := ftInteger;
end;

procedure TEpiIntVector.Reset;
begin
  FData := nil;
end;

procedure TEpiIntVector.SetAsBoolean(const index: Integer;
  const Value: EpiBool);
begin
    AsInteger[index] := integer(Value);
end;

procedure TEpiIntVector.SetAsDate(const index: Integer; const Value: EpiDate);
begin
end;

procedure TEpiIntVector.SetAsFloat(const index: Integer; const Value: EpiFloat);
  function DoRound(const X: Extended): Int64;
  begin
    Result := 0;
    if X >= 0 then
      Result := trunc(X + 0.5)
    else
      Result := trunc(X - 0.5);
  end;
begin
  If Value = NA_FLOAT then
    AsInteger[index] := NA_INT
  else
    AsInteger[index] := DoRound(Value);
end;

procedure TEpiIntVector.SetAsInteger(const index: Integer; const Value: EpiInt);
begin
{$IFDEF CHECKVRANGE}
   checkDataIndex(index);
{$ENDIF};
 Fdata[index-1]:=Value;
end;

procedure TEpiIntVector.SetAsString(const index: Integer;
  const Value: EpiString);
begin
  AsInteger[index]:=StrToIntDef(Value,NA_INT);
end;

procedure TEpiIntVector.SetVectorLength(const Value: EpiUnInt);
begin
  Setlength(Fdata, Value);
end;

procedure TEpiIntVector.SetValue(const index: Integer;
  const Value: Epivariant);
begin

end;

procedure TEpiIntVector.checkDataIndex(index: integer);
begin
   if Fdata=nil then error('Invalid operation, data storage has not been initialized'); //***
   if (index<1) or (index > length) then
       error(format('Invalid index %d, valid values for variable %s range from 1 to %d',
       [index,name,length]));
end;

function TEpiIntVector.GetIsMissing(const index: Integer): EpiBool;
begin
  result := asInteger[index] = NA_INT;
end;

procedure TEpiIntVector.SetIsMissing(const index: Integer;
  const Value: EpiBool);
begin
  asInteger[index] := NA_INT;
end;


function TEpiIntVector.GetIsMissingValue(const index: integer): EpiBool;
var
  n,t:integer;
begin
  result:=false;
  if (not assigned(fCheckProperties)) then exit;
  if isMissing[index] then exit;
  result := False;
  try
    for n:=0 to MAXDEFINEDMISSINGVALUES do
    begin
      if fCheckProperties.MissingValues[n] = '' then exit;
      t := StrToInt(fCheckProperties.MissingValues[n]);
      if t = asInteger[index] then
      begin
        Result := True;
        exit;
      end;
   end;  //for
 except
   exit;
 end; // try
end;


procedure TEpiIntVector.InternalSort(iLo, iHi: Integer;SortOptions:TSortOptions);
var
  Lo, Hi, Mid, T: Integer;
  desc: boolean;
begin
  desc:= SODescending in SortOptions;
  Lo := iLo;
  Hi := iHi;
  Mid := FData[(Lo + Hi) div 2];
  repeat
     if desc then
     begin
          while FData[Lo] > Mid do Inc(Lo);
          while FData[Hi] < Mid do Dec(Hi);
     end
     else
      begin
          while FData[Lo] < Mid do Inc(Lo);
          while FData[Hi] > Mid do Dec(Hi);
     end;
     if Lo <= Hi then
     begin
        T := FData[Lo];
        FData[Lo] := FData[Hi];
        FData[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then InternalSort(iLo, Hi,SortOptions);
    if Lo < iHi then InternalSort(Lo, iHi,SortOptions);
end;


function TEpiIntVector.GetMissingCount: EpiUnInt;
var
 i,co : integer;
begin
  Result:=0;
  co :=Length;
  for i := 0 to co-1 do
    if Fdata[i] = NA_INT then inc(Result);
end;

procedure TEpiIntVector.SetAllMissing;
var
 i,co : integer;
begin
  co :=Length;
  for i := 0 to co-1 do
    Fdata[i] := NA_INT;
end;

procedure TEpiIntVector.Exchange(i,j:integer);
var
 t : integer;
begin
  T:=Fdata[I-1];
  Fdata[I-1]:=Fdata[J-1];
  Fdata[J-1]:=T;
end;
function TEpiIntVector.compare(i, j: integer): integer;
begin
  result:= FData[i-1]- Fdata[J-1];
end;

procedure TEpiIntVector.AssignData(source: TEPiVector);
var
 i,co : integer;
begin
  co := Length;
  for i := 1 to co do
    AsInteger[i] := Source.AsInteger[i];
//    fdata[i] := TEpiIntVector(Source).fdata[i];
end;

function TEpiIntVector.Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;
begin
  Result:= TEpiIntVector.Create(Name,Length, DestDataframe.fCheckProperties);
  if Assigned(Vectors) then
    InternalClone(result, nodata, DestDataframe.fFileName = Vectors.DataFrame.fFileName)
  else
    InternalClone(result, nodata);
end;

function TEpiIntVector.GetMemoryUsed: Integer;
begin
    Result := sizeof(Fdata);
end;

{ TEpiStringVector  }


procedure TEpiStringVector.Finalize;
begin
  Reset;
end;

function TEpiStringVector.GetAsBoolean(const index: Integer): EpiBool;
begin
  // TODO : Define how a String is converted to a bool!
end;

function TEpiStringVector.GetAsDate(const index: Integer): EpiDate;
begin
  // TODO : Define how a string is converted to a date!
end;


function TEpiStringVector.GetAsFloat(const index: Integer): EpiFloat;
begin
    result:=StrToFloatDef(asString[Index],NA_FLOAT);
end;

function TEpiStringVector.GetAsInteger(const index: Integer): EpiInt;
begin
  result:=strtointdef(asString[Index],NA_INT);
end;

function TEpiStringVector.GetAsString(const index: Integer): EpiString;
var
 i,blank, alen: integer;
begin
{$IFDEF CHECKVRANGE}
   checkDataIndex(index);
{$ENDIF};
  Result := FData[index-1];
  if fUpperCase then
    result := AnsiUppercase(result);
end;

function TEpiStringVector.GetVectorLength: EpiUnInt;
begin
  result:=fOrglength;
end;

function TEpiStringVector.GetValue(const index: Integer): Epivariant;
begin
  Result:=AsString[index];
end;

procedure TEpiStringVector.initialize;
var
 i, co : integer;
begin
  if fOrgLength= -1 then fOrgLength:=1000;
  SetLength(FData, fOrgLength);
  fCheckProperties.OrigFelttype:=ftAlfa;
end;

procedure TEpiStringVector.Reset;
var
 i, co : integer;
begin
  FData := Nil; // malfunction: FreeAndNil(FData); //  := nil;
end;

procedure TEpiStringVector.SetAsBoolean(const index: Integer;
  const Value: EpiBool);
begin
  // TODO : Define how to convert Bool to String;
end;

procedure TEpiStringVector.SetAsDate(const index: Integer; const Value: EpiDate);
begin
  // TODO : Define how to convert Date to String;
end;

procedure TEpiStringVector.SetAsFloat(const index: Integer; const Value: EpiFloat);
begin
  // TODO : Define how to convert Float to String;
end;

procedure TEpiStringVector.SetAsInteger(const index: Integer; const Value: EpiInt);
begin
  // TODO : Define how to convert Integer to String;
end;

procedure TEpiStringVector.SetAsString(const index: Integer; const Value: EpiString);
var
 i,aLen : integer;
begin
{$IFDEF CHECKVRANGE}
  checkDataIndex(index);
{$ENDIF};
  if fUppercase then
    FData[index-1] := LeftStr(Sysutils.AnsiUpperCase(Value), fDataSize)
  else
    FData[index-1] := LeftStr(Value, fDataSize)
end;

procedure TEpiStringVector.SetVectorLength(const Value: EpiUnInt);
begin
 if fOrgLength=Value then exit;
 fOrgLength:=Value;
 Initialize;
end;

procedure TEpiStringVector.SetValue(const index: Integer;
  const Value: Epivariant);
begin

end;

procedure TEpiStringVector.checkDataIndex(index: integer);
begin
  if FData = nil then error('Invalid operation, data storage has not been initialized'); //***
  if (index<1) or (index > length) then
    error(format('Invalid index %d, valid values for variable %s range from 1 to %d',
                 [index,name,length])); //***
end;

function TEpiStringVector.GetIsMissing(const index: Integer): EpiBool;
begin
  result:=Trim(asString[index])=NA_STR;
end;

procedure TEpiStringVector.SetIsMissing(const index: Integer;
  const Value: EpiBool);
begin
   asString[index]:=NA_STR;
end;

function TEpiStringVector.GetIsMissingValue(const index: integer): EpiBool;
var
  n:Integer;
begin
  result:=false;
  if (not assigned(fCheckProperties)) then exit;
  if isMissing[index] then exit;
  for n:=0 to MAXDEFINEDMISSINGVALUES do
    if trim(fCheckProperties.MissingValues[n]) = asString[index] then result:=true;
end;


procedure TEpiStringVector.InternalSort(iLo, iHi: Integer;SortOptions:TSortOptions);
var
  Lo, Hi, Mid, T: Integer;
  desc: boolean;
begin
{  desc:= SODescending in SortOptions;
  Lo := iLo;
  Hi := iHi;
  Mid := FData[(Lo + Hi) div 2];
  repeat
     if desc then
     begin
          while FData[Lo] > Mid do Inc(Lo);
          while FData[Hi] < Mid do Dec(Hi);
     end
     else
      begin
          while FData[Lo] < Mid do Inc(Lo);
          while FData[Hi] > Mid do Dec(Hi);
     end;
     if Lo <= Hi then
     begin
        T := FData[Lo];
        FData[Lo] := FData[Hi];
        FData[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then InternalSort(iLo, Hi,SortOptions);
    if Lo < iHi then InternalSort(Lo, iHi,SortOptions);}
end;

function TEpiStringVector.GetDataType: EpiUnInt;
begin
  if FUpperCase then
    result := EpiTyUppercase
  else
    result := EpiTyString;
end;

function TEpiStringVector.GetMissingCount: EpiUnInt;
var
 i,co : integer;
begin
 Result:=0;
 co :=Length;
 for i := 0 to co-1 do
   if isMissing[i] then inc(Result);
//    if asString[i]=NA_STR then inc(Result);
end;

function TEpiStringVector.GetDataSize: EpiUnInt;
begin
  result:=fDataSize;
end;

constructor TEpiStringVector.Create(const VName:string; VLength:EpiInt; pDataSize:EpiInt; DfChkProp: TDfChkProp = nil);
begin
  fDataSize := pDataSize;
  fUpperCase := false;
  inherited Create(VName,VLength, DfChkProp);
end;

procedure TEpiStringVector.SetAllMissing;
var
 i,co : integer;
begin
 co :=Length;
 for i :=1  to co do
   AsString[i]:=NA_STR;
end;

procedure TEpiStringVector.Exchange(i,j:integer);
var
 t : string;
begin
   t := FData[i-1];
   FData[i-1] := FData[j-1];
   FData[j-1] := t;
end;


function TEpiStringVector.compare(i, j: integer): integer;
begin
  // Inserted by Torsten Christiansen 20. sept. 2006 - since sorting strings places '.' before anything else.
  if self.IsMissing[i] and self.IsMissing[j] then
    result := 0
  else if Self.IsMissing[i] then
    result := 1
  else if self.IsMissing[j] then
    result := -1
  else
    Result := AnsiCompareStr(AsString[i], AsString[j]);
end;

function TEpiStringVector.CompareItem(idx: integer; item: pointer): integer;
begin
  result:= AnsiCompareStr(AsString[idx],pchar(item));
end;


procedure TEpiStringVector.AssignData(source: TEPiVector);
var
 i,co : integer;
begin
 co :=(Length);
 for i := 1 to co do
    AsString[i]:= Source.AsString[i];
end;

function TEpiStringVector.Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;
begin
  Result:= TEpiStringVector.Create(Name,Length,Datasize, DestDataframe.fCheckProperties);
  if Assigned(Vectors) then
    InternalClone(result, nodata, DestDataframe.fFileName = Vectors.DataFrame.fFileName)
  else
    InternalClone(result, nodata);
end;

function TEpiStringVector.GetMemoryUsed: Integer;
begin
    Result := Length*DataSize;
end;

{ TEpiFloatVector }

procedure TEpiFloatVector.checkDataIndex(index: integer);
begin
   if Fdata=nil then error('Invalid operation, data storage has not been initialized'); //***
   if (index<1) or (index > length) then
       error(format('Invalid index %d, valid values for variable %s range from 1 to %d',
       [index,name,length])); //***
end;

procedure TEpiFloatVector.Finalize;
begin
  Reset;
end;

function TEpiFloatVector.GetAsBoolean(const index: Integer): EpiBool;
begin

end;


function TEpiFloatVector.GetAsDate(const index: Integer): EpiDate;
begin

end;

function TEpiFloatVector.GetAsFloat(const index: Integer): EpiFloat;
begin
{$IFDEF CHECKVRANGE}
  checkDataIndex(index);
{$ENDIF};
  if Fdata[index-1] = NA_Float then
    Result := FData[index-1]
  else
    result:=R2(FData[index-1],-fFldDataDecimals);
end;

function TEpiFloatVector.GetAsInteger(const index: Integer): EpiInt;
begin
  if Fdata[index-1] = NA_Float then
    Result := NA_Int
  else
  result:=trunc(asFloat[index]);
end;

function TEpiFloatVector.GetAsString(const index: Integer): EpiString;
begin
if IsMissing[Index] then
   result:= EpiMissingChar
else
   result:=EpiFormat(asFloat[Index],FieldFormat)
//   format(FieldDataFormat,[asFloat[Index]]);
end;

function TEpiFloatVector.GetDataType: EpiUnInt;
begin
    result:=EpiTyFloat;
end;

function TEpiFloatVector.GetIsMissing(const index: Integer): EpiBool;
begin
  result:=asFloat[index]=NA_Float;
end;

function TEpiFloatVector.GetIsMissingValue(const index: integer): EpiBool;
var
  tmp,mval: EpiFloat;
  n: Integer;
begin
  result:=false;
  if isMissing[index] then exit;
  if (not assigned(fCheckProperties)) then exit;
  tmp := asFloat[index];
  try
    for n := 0 to MAXDEFINEDMISSINGVALUES do
    begin
      if fCheckProperties.MissingValues[n] = '' then exit;
      mval := StrToFloat(fCheckProperties.MissingValues[n]);
      if mval < 0 then mval := -(R2(-(mval), -fFldDataDecimals))
      else mval := R2(mval, -fFldDataDecimals);
      if tmp = mval then
      begin
        Result := True;
        exit;
      end;
    end;  //for
  except
   exit;
  end; // try
end;


function TEpiFloatVector.GetMissingCount: EpiUnInt;
begin

end;

function TEpiFloatVector.GetValue(const index: Integer): Epivariant;
begin
  Result:=AsFloat[index];
end;

function TEpiFloatVector.GetVectorLength: EpiUnInt;
begin
   result:=System.length(Fdata);
end;

procedure TEpiFloatVector.initialize;
begin
  if fOrgLength= -1 then fOrgLength:=1000;
  system.setlength(Fdata,fOrgLength);
  fCheckProperties.OrigFelttype:=ftFloat;
  fFldDataDecimals := 6;
  fFldDataSize := 12;
end;

procedure TEpiFloatVector.InternalSort(iLo, iHi: Integer; SortOptions: TSortOptions);
var
  Lo, Hi: Integer;
  Mid,T : EpiFloat;
  desc: boolean;
begin
  desc:= SODescending in SortOptions;
  Lo := iLo;
  Hi := iHi;
  Mid := FData[(Lo + Hi) div 2];
  repeat
     if desc then
     begin
          while FData[Lo] > Mid do Inc(Lo);
          while FData[Hi] < Mid do Dec(Hi);
     end
     else
      begin
          while FData[Lo] < Mid do Inc(Lo);
          while FData[Hi] > Mid do Dec(Hi);
     end;
     if Lo <= Hi then
     begin
        T := FData[Lo];
        FData[Lo] := FData[Hi];
        FData[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then InternalSort(iLo, Hi,SortOptions);
    if Lo < iHi then InternalSort(Lo, iHi,SortOptions);
end;

procedure TEpiFloatVector.Reset;
begin
  Fdata:=nil;
end;

procedure TEpiFloatVector.SetAllMissing;
var
 i,co : integer;
begin
 co :=Length;
 for i := 0 to co-1 do
    Fdata[i]:=NA_FLOAT;
end;

procedure TEpiFloatVector.SetAsBoolean(const index: Integer;
  const Value: EpiBool);
begin

end;

procedure TEpiFloatVector.SetAsDate(const index: Integer;
  const Value: EpiDate);
begin

end;

procedure TEpiFloatVector.SetAsFloat(const index: Integer;
  const Value: EpiFloat);
begin
{$IFDEF CHECKVRANGE}
  CheckDataIndex(index);
{$ENDIF};
  Fdata[index-1] := value;
end;

procedure TEpiFloatVector.SetAsInteger(const index: Integer;
  const Value: EpiInt);
begin
  AsFloat[index] := Value;
end;

procedure TEpiFloatVector.SetAsString(const index: Integer;
  const Value: EpiString);
begin
  AsFloat[index] := StrToFloatDef(Value, NA_FLOAT);
end;

procedure TEpiFloatVector.SetIsMissing(const index: Integer;
  const Value: EpiBool);
begin
  asFloat[index] := NA_FLOAT;
end;

procedure TEpiFloatVector.SetValue(const index: Integer;
  const Value: Epivariant);
begin

end;


function TEpiFloatVector.CompareItem(idx: integer; item: pointer): integer;
begin
  result:=0;
  if AsFloat[idx] > EpiFloat(item^) then result:= 1
  else if AsFloat[idx]< EpiFloat(item^) then result:= -1;
end;

procedure TEpiFloatVector.SetVectorLength(const Value: EpiUnInt);
begin
  Setlength(Fdata, Value);
end;

procedure TEpiFloatVector.Exchange(i,j:integer);
var
 t : EpiFloat;
begin
{    T:=AsFloat[I];
    AsFloat[I]:=AsFloat[J];
    AsFloat[J]:=T;}
    T:=Fdata[I-1];
    Fdata[I-1]:=Fdata[J-1];
    Fdata[J-1]:=T;
end;


function TEpiFloatVector.compare(i, j: integer): integer;
begin
  result:=0;
  if FData[i-1]> Fdata[J-1] then result:= 1
  else if FData[i-1] < Fdata[J-1] then result:= -1;
end;

function TEpiFloatVector.Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;
begin
  Result:= TEpiFloatVector.Create(Name,Length, DestDataframe.fCheckProperties);
  if Assigned(Vectors) then
    InternalClone(result, nodata, DestDataframe.fFileName = Vectors.DataFrame.fFileName)
  else
    InternalClone(result, nodata);
end;

procedure TEpiFloatVector.AssignData(source: TEPiVector);
var
 i,co : integer;
begin
//  move(TEpiFloatVector(source).fdata[0],fdata[0],length*sizeof(EpiFloatStorage));
 co :=Length;
 for i := 0 to co-1 do
    fdata[i]:= TEpiFloatVector(source).fdata[i]
end;

function TEpiFloatVector.GetFieldDataFormat: Epistring;
begin
  result:=inherited GetFieldDataFormat;
  if result='' then result:='%8.2f';
end;

function TEpiFloatVector.GetMemoryUsed: Integer;
begin
    Result:=sizeof(Fdata);
end;

{ TEpiDateVector }


procedure TEpiDateVector.Finalize;
begin
  Reset;
end;

function TEpiDateVector.GetAsBoolean(const index: Integer): EpiBool;
begin

end;

function TEpiDateVector.GetAsDate(const index: Integer): EpiDate;
begin
{$IFDEF CHECKVRANGE}
   checkDataIndex(index);
{$ENDIF};
 Result:=Fdata[index-1];
end;

function TEpiDateVector.GetAsFloat(const index: Integer): EpiFloat;
begin
  result := GetAsInteger(index);
end;

function TEpiDateVector.GetAsInteger(const index: Integer): EpiInt;
begin
{$IFDEF CHECKVRANGE}
   checkDataIndex(index);
{$ENDIF};
 Result:=Fdata[index-1];
end;

function TEpiDateVector.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result:= EpiMissingChar
  else
    result:=EpiFormat(Fdata[index-1],FieldFormat);
  result := result;
end;

function TEpiDateVector.GetDataType: EpiUnInt;
begin
 result:=EpiTyDate;
end;

function TEpiDateVector.GetVectorLength: EpiUnInt;
begin
  result:=System.length(Fdata);
end;

function TEpiDateVector.GetValue(const index: Integer): Epivariant;
begin
  if IsMissing[index] then
    result := AsDate[Index]
  else
    result:=EpiDateToTDateTime(AsDate[Index]);
end;

procedure TEpiDateVector.initialize;
begin
  if fOrgLength= -1 then
     system.setlength(Fdata,1000)
  else
     system.setlength(Fdata,fOrgLength);
  fCheckProperties.OrigFelttype:=ftEuroDate;
end;

procedure TEpiDateVector.Reset;
begin
  Fdata:=nil;
end;

procedure TEpiDateVector.SetAsBoolean(const index: Integer;
  const Value: EpiBool);
begin
end;

procedure TEpiDateVector.SetAsDate(const index: Integer; const Value: EpiDate);
begin
{$IFDEF CHECKVRANGE}
  checkDataIndex(index);
{$ENDIF};
  FData[index-1] := Value;
end;

procedure TEpiDateVector.SetAsFloat(const index: Integer; const Value: EpiFloat);
begin
  AsInteger[index] := Round(Value);
end;

procedure TEpiDateVector.SetAsInteger(const index: Integer; const Value: EpiInt);
begin
 if EpiIsValidDate(value) then
   AsDate[Index] := Value
 else
   AsDate[Index] := NA_DATE
end;

procedure TEpiDateVector.SetAsString(const index: Integer;const Value: EpiString);
var
 d : EPidate;
 s: string;
begin

//  EpiStrToDate(Value,d,dfYMD);
{  if fFormat[1] = '%' then
    s := RightStr(fFormat, system.length(fFormat)-1)
  else}
    s := fFormat;
  AsDate[index]:= EpiStrToDatefmt(Value, s); //d;
end;

procedure TEpiDateVector.SetVectorLength(const Value: EpiUnInt);
begin
  Setlength(Fdata, Value);
end;

procedure TEpiDateVector.SetValue(const index: Integer;
  const Value: Epivariant);
begin

end;

procedure TEpiDateVector.checkDataIndex(index: integer);
begin
   if Fdata=nil then error('Invalid operation, data storage has not been initialized'); //***
   if (index<1) or (index > length) then
       error(format('Invalid index %d, valid values for variable %s range from 1 to %d',
       [index,name,length])); //***
end;

function TEpiDateVector.GetIsMissing(const index: Integer): EpiBool;
begin
  result:=asInteger[index]=NA_DATE;
end;

procedure TEpiDateVector.SetIsMissing(const index: Integer;
  const Value: EpiBool);
begin
 asInteger[index]:=NA_DATE;
end;

function TEpiDateVector.GetIsMissingValue(const index: integer): EpiBool;
begin
  result:=false;
end;

procedure TEpiDateVector.InternalSort(iLo, iHi: Integer;SortOptions:TSortOptions);
var
  Lo, Hi, Mid, T: Integer;
  desc: boolean;
begin
  desc:= SODescending in SortOptions;
  Lo := iLo;
  Hi := iHi;
  Mid := FData[(Lo + Hi) div 2];
  repeat
     if desc then
     begin
          while FData[Lo] > Mid do Inc(Lo);
          while FData[Hi] < Mid do Dec(Hi);
     end
     else
      begin
          while FData[Lo] < Mid do Inc(Lo);
          while FData[Hi] > Mid do Dec(Hi);
     end;
     if Lo <= Hi then
     begin
        T := FData[Lo];
        FData[Lo] := FData[Hi];
        FData[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then InternalSort(iLo, Hi,SortOptions);
    if Lo < iHi then InternalSort(Lo, iHi,SortOptions);
end;


function TEpiDateVector.GetMissingCount: EpiUnInt;
var
 i,co : integer;
begin
 Result:=0;
 co :=Length;
 for i := 0 to co-1 do
    if Fdata[i]=NA_DATE then inc(Result);
end;



procedure TEpiDateVector.SetAllMissing;
var
 i,co : integer;
begin
 co :=Length;
 for i := 0 to co-1 do
    Fdata[i]:=NA_DATE;
end;

procedure TEpiDateVector.Exchange(i, j: integer);
var
 t : integer;
begin
{    T:=AsInteger[I];
    AsInteger[I]:=AsInteger[J];
    AsInteger[J]:=T;}
    T:=Fdata[I-1];
    Fdata[I-1]:=Fdata[J-1];
    Fdata[J-1]:=T;
end;

function TEpiDateVector.compare(i, j: integer): integer;
begin
    result:= FData[i-1]- Fdata[J-1];
end;

procedure TEpiDateVector.AssignData(source: TEPiVector);
var
 i,co : integer;
begin
 co :=Length;
 for i := 0 to co-1 do
    fdata[i]:= TEpiDateVector(source).fdata[i]
end;

function TEpiDateVector.Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;
begin
  Result:= TEpiDateVector.Create(Name,Length,DestDataframe.fCheckProperties);
  if Assigned(Vectors) then
    InternalClone(result, nodata, DestDataframe.fFileName = Vectors.DataFrame.fFileName)
  else
    InternalClone(result, nodata);
end;

function TEpiDateVector.GetMemoryUsed: Integer;
begin
  Result:=sizeof(FData);
end;

{ TEpiBoolVector }

procedure TEpiBoolVector.initialize;
var
 i, co : integer;
begin
  if fOrgLength= -1 then fOrgLength := 1000;
  SetLength(FData, fOrgLength);
  fCheckProperties.OrigFelttype := ftBoolean;
end;

procedure TEpiBoolVector.Reset;
var
 i, co : integer;
begin
  FData := nil;
end;


procedure TEpiBoolVector.Finalize;
begin
  Reset;
end;

function TEpiBoolVector.GetAsBoolean(const index: Integer): EpiBool;
begin
  Result:= ((FData[index-1]<>0) and (FData[index-1]<>NA_BOOL));
end;

function TEpiBoolVector.GetAsDate(const index: Integer): EpiDate;
begin
    error('invalid data conversion');
end;

function TEpiBoolVector.GetAsByte(const index: Integer): EpiByte;
begin
   Result := FData[index-1];
end;

function TEpiBoolVector.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[index] then
    result := NA_FLOAT
  else
    result := AsByte[index];
end;

function TEpiBoolVector.GetAsInteger(const index: Integer): EpiInt;
begin
  if IsMissing[index] then
    result := NA_INT
  else
    result := AsByte[index];
end;

function TEpiBoolVector.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[index] then
    result := NA_STR
  else
    result := BoolStr[Asboolean[index]];
end;


function TEpiBoolVector.GetVectorLength: EpiUnInt;
begin
  result := System.Length(FData);
end;

function TEpiBoolVector.GetValue(const index: Integer): EpiVariant;
begin
  Result := AsByte[index];
end;

procedure TEpiBoolVector.SetAsBoolean(const index: Integer;
  const Value: EpiBool);
begin
   FData[index-1] := Word(value);
end;

procedure TEpiBoolVector.SetAsDate(const index: Integer; const Value: EpiDate);
begin
    error('invalid data conversion');
end;

procedure TEpiBoolVector.SetAsFloat(const index: Integer; const Value: EpiFloat);
begin
   error('invalid data conversion');
end;

procedure TEpiBoolVector.SetAsInteger(const index: Integer; const Value: EpiInt);
begin
 if Value = NA_int then
   AsByte[index] := NA_BOOL
 else
   Asboolean[index] := boolean(Value);
end;

procedure TEpiBoolVector.SetAsString(const index: Integer; const Value: EpiString);
var
 i,aLen : integer;
begin
 if Value=NA_STR then
    FData[index-1] := NA_BOOL
 else
    FData[index-1] := byte(Value[1] = GetTrueString);
end;

procedure TEpiBoolVector.SetAsByte(const index: Integer; const Value: EpiByte);
begin
    FData[index-1]:=Value
end;

procedure TEpiBoolVector.SetVectorLength(const Value: EpiUnInt);
begin
  if fOrgLength=Value then exit;
  SetLength(FData, Value);
end;

procedure TEpiBoolVector.SetValue(const index: Integer;
  const Value: Epivariant);
begin

end;

procedure TEpiBoolVector.checkDataIndex(index: integer);
begin
   if System.Length(FData) = 0 then error('Invalid operation, data storage has not been initialized'); //***
   if (index<1) or (index > length) then
       error(format('Invalid index %d, valid values for variable %s range from 1 to %d',
       [index,name,length])); //***
end;

function TEpiBoolVector.GetIsMissing(const index: Integer): EpiBool;
begin
  result := FData[index-1] = NA_BOOL;
end;

procedure TEpiBoolVector.SetIsMissing(const index: Integer;
  const Value: EpiBool);
begin
   FData[index-1] := NA_BOOL;
end;

function TEpiBoolVector.GetIsMissingValue(const index: integer): EpiBool;
var
  n, t:Integer;
begin
  result:=false;
  if isMissing[index] then exit;
  if (not assigned(fCheckProperties)) then exit;
  for n:=0 to MAXDEFINEDMISSINGVALUES do
  try
    if fCheckProperties.MissingValues[n] = '' then exit;
    t := StrToInt(fCheckProperties.MissingValues[n]);
    if t = asInteger[index] then result:=true;
  except
    result:= False;
  end;  //for
end;


procedure TEpiBoolVector.InternalSort(iLo, iHi: Integer;SortOptions:TSortOptions);
var
  Lo, Hi, Mid, T: Integer;
  desc: boolean;
begin
end;

function TEpiBoolVector.GetDataType: EpiUnInt;
begin
  result := EpiTyBoolean;
end;

function TEpiBoolVector.GetMissingCount: EpiUnInt;
var
 i,co : integer;
begin
 Result:=0;
 co := Length;
 for i := 1 to co do
    if IsMissing[i] then inc(Result);
end;

function TEpiBoolVector.GetDataSize: EpiUnInt;
begin
  result := 1;
end;

constructor TEpiBoolVector.Create(const VName:string; VLength:EpiInt; DfChkProp: TDfChkProp = nil);
begin
  inherited Create(VName,VLength, DfChkProp);
end;

procedure TEpiBoolVector.SetAllMissing;
var
 i,co : integer;
begin
 co :=Length;
 for i :=1  to co do
   IsMissing[i] := true;
end;


procedure TEpiBoolVector.Exchange(i,j:integer);
var
  t : Epibyte;
begin
  T := FData[I-1];
  FData[I-1] := FData[J-1];
  FData[J-1] := T;
end;


function TEpiBoolVector.compare(i, j: integer): integer;
begin
    result:= FData[i-1] - FData[J-1];
end;

function TEpiBoolVector.CompareItem(idx: integer; item: pointer): integer;
begin
end;


procedure TEpiBoolVector.AssignData(source: TEPiVector);
var
 i,co : integer;
begin
  co :=Length;
  for i := 0 to co-1 do
   FData[i]:= TEpiBoolVector(source).FData[i]
end;


function TEpiBoolVector.Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;
begin
  Result:= TEpiBoolVector.Create(Name, Length, DestDataframe.fCheckProperties);
  if Assigned(Vectors) then
    InternalClone(result, nodata, DestDataframe.fFileName = Vectors.DataFrame.fFileName)
  else
    InternalClone(result, nodata);
end;


function TEpiBoolVector.GetMemoryUsed: Integer;
begin
  Result := Length * Datasize;
end;

{ TEpiByteVector }

procedure TEpiByteVector.initialize;
var
 i, co : integer;
begin
  if fOrgLength= -1 then fOrgLength:=1000;
  SetLength(FData, fOrgLength);
  fCheckProperties.OrigFelttype:=ftInteger;
end;

procedure TEpiByteVector.Reset;
begin
  FData := nil;
end;


procedure TEpiByteVector.Finalize;
begin
  dm.sysinfo('epibytevector reset');
  Reset;
end;

function TEpiByteVector.GetAsBoolean(const index: Integer): EpiBool;
begin
  Result := char(FData[index-1]) = GetTrueString;
end;

function TEpiByteVector.GetAsDate(const index: Integer): EpiDate;
begin
    error('invalid data conversion');
end;

function TEpiByteVector.GetAsByte(const index: Integer): EpiByte;
begin
  Result := FData[index-1];
{  if IsMissing[index] then
     result := NA_BYTE
  else
     Result:=AsInteger[index];}
end;

function TEpiByteVector.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[index] then
    result := NA_Float
  else
    result := StrToFloat(AsString[index]);
end;

function TEpiByteVector.GetAsInteger(const index: Integer): EpiInt;
begin
  if IsMissing[index] then
    result := NA_INT
  else
    result := Strtoint(AsString[index]);
end;

function TEpiByteVector.GetAsString(const index: Integer): EpiString;
begin
  Result := pchar(FData[index-1]);
end;


function TEpiByteVector.GetVectorLength: EpiUnInt;
begin
  result := System.Length(FData);
end;

function TEpiByteVector.GetValue(const index: Integer): Epivariant;
begin
  Result := AsByte[index];
end;


procedure TEpiByteVector.SetAsBoolean(const index: Integer;
  const Value: EpiBool);
begin
   FData[index-1] := byte(BoolStr[value]);
end;

procedure TEpiByteVector.SetAsDate(const index: Integer; const Value: EpiDate);
begin
    error('invalid data conversion');
end;

procedure TEpiByteVector.SetAsFloat(const index: Integer; const Value: EpiFloat);
begin
   error('invalid data conversion');
end;

procedure TEpiByteVector.SetAsInteger(const index: Integer; const Value: EpiInt);
begin
  if Value = NA_INT then
    IsMissing[index] := true
  else
    AsByte[index] := Value;
end;

procedure TEpiByteVector.SetAsString(const index: Integer; const Value: EpiString);
begin
  if Value = NA_STR then
    IsMissing[index] := true
  else
    AsByte[index] := byte(Value[1]);
end;

procedure TEpiByteVector.SetAsByte(const index: Integer; const Value: EpiByte);
begin
  FData[index-1] := Value
end;


procedure TEpiByteVector.SetVectorLength(const Value: EpiUnInt);
begin
  if fOrgLength=Value then exit;
  fOrgLength:=Value;
  SetLength(FData, Value);
end;

procedure TEpiByteVector.SetValue(const index: Integer;
  const Value: Epivariant);
begin

end;

procedure TEpiByteVector.checkDataIndex(index: integer);
begin
   if System.Length(FData) = 0 then error('Invalid operation, data storage has not been initialized'); //***
   if (index<1) or (index > length) then
       error(format('Invalid index %d, valid values for variable %s range from 1 to %d',
       [index,name,length])); //***
end;

function TEpiByteVector.GetIsMissing(const index: Integer): EpiBool;
begin
  result := FData[index-1] = NA_BYTE;
end;

procedure TEpiByteVector.SetIsMissing(const index: Integer;
  const Value: EpiBool);
begin
   FData[index] := NA_BYTE;
end;


function TEpiByteVector.GetIsMissingValue(const index: integer): EpiBool;
var
  n,t:integer;
begin
  result:=false;
  if (not assigned(fCheckProperties)) then exit;
  if isMissing[index] then exit;
  for n:=0 to MAXDEFINEDMISSINGVALUES do
  try
    if fCheckProperties.MissingValues[n] = '' then exit;
    t := StrToInt(fCheckProperties.MissingValues[n]);
    if t = asInteger[index] then result:=true;
  except
    result := False;
  end;
end;

procedure TEpiByteVector.InternalSort(iLo, iHi: Integer;SortOptions:TSortOptions);
var
  Lo, Hi, Mid, T: Integer;
  desc: boolean;
begin
{  desc:= SODescending in SortOptions;
  Lo := iLo;
  Hi := iHi;
  Mid := FData[(Lo + Hi) div 2];
  repeat
     if desc then
     begin
          while FData[Lo] > Mid do Inc(Lo);
          while FData[Hi] < Mid do Dec(Hi);
     end
     else
      begin
          while FData[Lo] < Mid do Inc(Lo);
          while FData[Hi] > Mid do Dec(Hi);
     end;
     if Lo <= Hi then
     begin
        T := FData[Lo];
        FData[Lo] := FData[Hi];
        FData[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then InternalSort(iLo, Hi,SortOptions);
    if Lo < iHi then InternalSort(Lo, iHi,SortOptions);}
end;

function TEpiByteVector.GetDataType: EpiUnInt;
begin
  result := EpiTyByte;
end;

function TEpiByteVector.GetMissingCount: EpiUnInt;
var
 i,co : integer;
begin
  Result:=0;
  co := Length;
  for i := 1 to co do
    if IsMissing[i] then inc(Result);
end;

function TEpiByteVector.GetDataSize: EpiUnInt;
begin
  result := 1;
end;

constructor TEpiByteVector.Create(const VName:string; VLength:EpiInt; DfChkProp: TDfChkProp = nil);
begin
  inherited Create(VName,VLength, DfChkProp);
end;

procedure TEpiByteVector.SetAllMissing;
var
 i,co : integer;
begin
 co :=Length;
 for i :=1  to co do
   IsMissing[i] := true;
end;


procedure TEpiByteVector.Exchange(i,j:integer);
var
  t : EpiByte;
begin
  T := FData[I-1];
  FData[I-1] := FData[J-1];
  FData[J-1] := T;
end;


function TEpiByteVector.compare(i, j: integer): integer;
begin
    result:= ord(FData[i-1])- ord(FData[J-1]);
end;

function TEpiByteVector.CompareItem(idx: integer; item: pointer): integer;
begin
end;


procedure TEpiByteVector.AssignData(source: TEPiVector);
var
 i,co : integer;
begin
  co :=Length;
  for i := 0 to co-1 do
    FData[i]:= TEpiByteVector(source).FData[i]
end;


function TEpiByteVector.Clone(DestDataframe: TEpiDataframe = nil; NoData:boolean=false): TEPiVector;
begin
  Result:= TEpiByteVector.Create(Name,Length, DestDataframe.fCheckProperties);
  if Assigned(Vectors) then
    InternalClone(result, nodata, DestDataframe.fFileName = Vectors.DataFrame.fFileName)
  else
    InternalClone(result, nodata);
end;


function TEpiByteVector.GetMemoryUsed: Integer;
begin
  Result := Length * Datasize;
end;


end.


