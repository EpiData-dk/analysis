unit result_variables;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes, epidatafiles, fgl, epidatafilerelations,
  epivaluelabels, epicustombase;

type

  TExecutorVariableType = (
    evtGlobal,         // A user created global variable       (single value)
    evtGlobalVector,
    evtField,          // A user created / loaded variable     (vector)
    evtDataset,        // A user created / loaded variable     (no value)
    evtValuelabel,     // A user created / loaded variable     (no value)
    evtResultConst,    // An internally created result variable (single value)
    evtResultVector,   // An internally created result variable (vector)
    evtResultMatrix,   // An internally created result variable (matrix)
    evtFunctionParam   // Parameters for a user created function
  );
  TExecutorVariableTypes = set of TExecutorVariableType;



  TExecutorVariableFlag = (
    evfInternal,     // Marks that the variable may already be IN loaded memory and may be checked by the executor
    evfExternal,     // Marks that the variable is to be loaded in during a statement/command and should be checked
                     // by the statement itself.
    evfAsObject,     // Marks that the variable is being read as object
    evfAsValue       // Marks that the variable is being read as value
  );
  TExecutorVariableFlags = set of TExecutorVariableFlag;

const
  ExecutorVariableTypesAll     = [evtGlobal..evtResultMatrix];
  ExecutorVariableTypesSingle  = [evtGlobal, evtDataset, evtResultConst];
  ExecutorVariableTypesVectors = [evtGlobalVector, evtResultVector, evtField, evtValuelabel];
  ExecutorVariableTypesMatrix  = [evtResultMatrix];
  ExecutorVariableTypesData    = ExecutorVariableTypesAll - [evtDataset, evtValuelabel];

  ExecutorVariableTypeString: array[TExecutorVariableType] of UTF8String = (
    'global',
    'global vector',
    'variable',
    'dataset',
    'valuelabel',
    'result const',
    'result vector',
    'result matrix',
    'function parameter'
  );

type

  { TCustomExecutorVariable }

  TCustomExecutorVariable = class
  private
    FIdent: UTF8String;
  protected
    function GetVarType: TExecutorVariableType; virtual; abstract;
  public
    constructor Create(Const AIdent: UTF8String); virtual;
    property VarType: TExecutorVariableType read GetVarType;
    property Ident: UTF8String read FIdent;
  end;

  { TExecutorDatasetVariable }

  TExecutorDatasetVariable = class(TCustomExecutorVariable)
  private
    FRelation: TEpiMasterRelation;
    function GetDataFile: TEpiDataFile;
  protected
    function GetVarType: TExecutorVariableType; override;
  public
    constructor Create(Const AIdent: UTF8String; ARelation: TEpiMasterRelation);
    property DataFile: TEpiDataFile read GetDataFile;
    property Relation: TEpiMasterRelation read FRelation;
  end;

  { TExecutorValuelabelsetVariable }

  TExecutorValuelabelsetVariable = class(TCustomExecutorVariable)
  private
    FValuelabelset: TEpiValueLabelSet;
  protected
    function GetVarType: TExecutorVariableType; override;
  public
    constructor Create(Const AIdent: UTF8String; AValuelabelset: TEpiValueLabelSet);
    property Valuelabelset: TEpiValueLabelSet read FValuelabelset;
  end;

  { TExecParamVar }

  TExecParamVar = class(TCustomExecutorVariable)
  protected
    function GetAsBoolean: Boolean; virtual;
    function GetAsDate: EpiDate; virtual;
    function GetAsFloat: EpiFloat; virtual;
    function GetAsInteger: EpiInteger; virtual;
    function GetAsMissing: boolean; virtual;
    function GetAsString: EpiString; virtual;
    function GetAsTime: EpiTime; virtual;
    procedure SetAsBoolean(AValue: Boolean); virtual;
    procedure SetAsDate(AValue: EpiDate); virtual;
    procedure SetAsFloat(AValue: EpiFloat); virtual;
    procedure SetAsInteger(AValue: EpiInteger); virtual;
    procedure SetAsMissing(AValue: boolean); virtual;
    procedure SetAsString(AValue: EpiString); virtual;
    procedure SetAsTime(AValue: EpiTime); virtual;
  public
    constructor Create(const AIdent: UTF8String);
    property AsBoolean: Boolean    read GetAsBoolean write SetAsBoolean;
    property AsInteger: EpiInteger read GetAsInteger write SetAsInteger;
    property AsFloat:   EpiFloat   read GetAsFloat   write SetAsFloat;
    property AsString:  EpiString  read GetAsString  write SetAsString;
    property AsDate:    EpiDate    read GetAsDate    write SetAsDate;
    property AsTime:    EpiTime    read GetAsTime    write SetAsTime;
    property IsMissing: boolean    read GetAsMissing write SetAsMissing;
  end;

  { TCustomExecutorDataVariable }

  TCustomExecutorDataVariable = class(TCustomExecutorVariable)
  private
  protected
    function GetAsBooleanV(const Index: Integer): Boolean; virtual;
    function GetAsIntegerV(const Index: Integer): EpiInteger; virtual;
    function GetAsFloatV(const   Index: Integer): EpiFloat; virtual;
    function GetAsStringV(const  Index: Integer): EpiString; virtual;
    function GetAsDateV(const  Index: Integer): EpiDate; virtual;
    function GetAsTimeV(const  Index: Integer): EpiTime; virtual;
    procedure SetAsBooleanV(const Index: Integer; AValue: Boolean); virtual;
    procedure SetAsIntegerV(const Index: Integer; AValue: EpiInteger); virtual;
    procedure SetAsFloatV(const   Index: Integer; AValue: EpiFloat); virtual;
    procedure SetAsStringV(const  Index: Integer; AValue: EpiString); virtual;
    procedure SetAsDateV(const  Index: Integer; AValue: EpiDate); virtual;
    procedure SetAsTimeV(const  Index: Integer; AValue: EpiTime); virtual;
    function  GetAsMissingV(const Index: Integer): boolean; virtual;
    procedure SetAsMissingV(const Index: Integer; AValue: boolean); virtual;
  public
    property AsBooleanVector[Const Index: Integer]: Boolean    read GetAsBooleanV write SetAsBooleanV;
    property AsIntegerVector[Const Index: Integer]: EpiInteger read GetAsIntegerV write SetAsIntegerV;
    property AsFloatVector[Const   Index: Integer]: EpiFloat   read GetAsFloatV   write SetAsFloatV;
    property AsStringVector[Const  Index: Integer]: EpiString  read GetAsStringV  write SetAsStringV;
    property AsDateVector[Const    Index: Integer]: EpiDate    read GetAsDateV    write SetAsDateV;
    property AsTimeVector[Const    Index: Integer]: EpiTime    read GetAsTimeV    write SetAsTimeV;
    property IsMissing[Const       Index: Integer]: boolean    read GetAsMissingV write SetAsMissingV;
  protected
    function GetAsBooleanM(const Col, Row: Integer): Boolean; virtual;
    function GetAsIntegerM(const Col, Row: Integer): EpiInteger; virtual;
    function GetAsFloatM(const   Col, Row: Integer): EpiFloat; virtual;
    function GetAsStringM(const  Col, Row: Integer): EpiString; virtual;
    function GetAsDateM(const    Col, Row: Integer): EpiDate; virtual;
    function GetAsTimeM(const    Col, Row: Integer): EpiTime; virtual;
    procedure SetAsBooleanM(const Col, Row: Integer; AValue: Boolean); virtual;
    procedure SetAsIntegerM(const Col, Row: Integer; AValue: EpiInteger); virtual;
    procedure SetAsFloatM(const   Col, Row: Integer; AValue: EpiFloat); virtual;
    procedure SetAsStringM(const  Col, Row: Integer; AValue: EpiString); virtual;
    procedure SetAsDateM(const    Col, Row: Integer; AValue: EpiDate); virtual;
    procedure SetAsTimeM(const    Col, Row: Integer; AValue: EpiTime); virtual;
  public
    property AsBooleanMatrix[Const Col, Row: Integer]: Boolean    read GetAsBooleanM write SetAsBooleanM;
    property AsIntegerMatrix[Const Col, Row: Integer]: EpiInteger read GetAsIntegerM write SetAsIntegerM;
    property AsFloatMatrix[Const   Col, Row: Integer]: EpiFloat   read GetAsFloatM   write SetAsFloatM;
    property AsStringMatrix[Const  Col, Row: Integer]: EpiString  read GetAsStringM  write SetAsStringM;
    property AsDateMatrix[Const    Col, Row: Integer]: EpiDate    read GetAsDateM    write SetAsDateM;
    property AsTimeMatrix[Const    Col, Row: Integer]: EpiTime    read GetAsTimeM    write SetAsTimeM;
  private
    FDataType: TEpiFieldType;
  protected
    function GetAcceptedIndices: Integer; virtual;
  public
    constructor Create(Const AIdent: UTF8String; ADataType: TEpiFieldType); virtual;
    property DataType: TEpiFieldType read FDataType;
    property AcceptedIndices: Integer read GetAcceptedIndices;
  end;

  { TExecVarGlobal }

  TExecVarGlobal = class(TCustomExecutorDataVariable)
  private
    FField: TEpiField;
  protected
    function GetAsBooleanV(const Index: Integer): Boolean; override;
    function GetAsIntegerV(const Index: Integer): EpiInteger; override;
    function GetAsFloatV(const   Index: Integer): EpiFloat; override;
    function GetAsStringV(const  Index: Integer): EpiString; override;
    function GetAsDateV(const Index: Integer): EpiDate; override;
    function GetAsTimeV(const Index: Integer): EpiTime; override;
    procedure SetAsBooleanV(const Index: Integer; AValue: Boolean); override;
    procedure SetAsIntegerV(const Index: Integer; AValue: EpiInteger); override;
    procedure SetAsFloatV(const   Index: Integer; AValue: EpiFloat); override;
    procedure SetAsStringV(const  Index: Integer; AValue: EpiString); override;
    procedure SetAsDateV(const Index: Integer; AValue: EpiDate); override;
    procedure SetAsTimeV(const Index: Integer; AValue: EpiTime); override;
    function  GetAsMissingV(const Index: Integer): boolean; override;
    procedure SetAsMissingV(const Index: Integer; AValue: boolean); override;
    function  GetVarType: TExecutorVariableType; override;
  public
    constructor Create(Const AIdent: UTF8String; ADataType: TEpiFieldType); override;
  end;

  { TExecVarGlobalVector }

  TExecVarGlobalVector = class(TCustomExecutorDataVariable)
  private
    FField: TEpiField;
    function GetLength: Integer;
  protected
    function GetAsBooleanV(const Index: Integer): Boolean; override;
    function GetAsIntegerV(const Index: Integer): EpiInteger; override;
    function GetAsFloatV(const   Index: Integer): EpiFloat; override;
    function GetAsStringV(const  Index: Integer): EpiString; override;
    function GetAsDateV(const Index: Integer): EpiDate; override;
    function GetAsTimeV(const Index: Integer): EpiTime; override;
    procedure SetAsBooleanV(const Index: Integer; AValue: Boolean); override;
    procedure SetAsIntegerV(const Index: Integer; AValue: EpiInteger); override;
    procedure SetAsFloatV(const   Index: Integer; AValue: EpiFloat); override;
    procedure SetAsStringV(const  Index: Integer; AValue: EpiString); override;
    procedure SetAsDateV(const Index: Integer; AValue: EpiDate); override;
    procedure SetAsTimeV(const Index: Integer; AValue: EpiTime); override;
    function  GetAsMissingV(const Index: Integer): boolean; override;
    procedure SetAsMissingV(const Index: Integer; AValue: boolean); override;
    function  GetVarType: TExecutorVariableType; override;
  public
    constructor Create(Const AIdent: UTF8String; ADataType: TEpiFieldType; ALength: Integer);
    property Length: Integer read GetLength;
  end;

  { TExecVarResultConst }

  TExecVarResultConst = class(TExecVarGlobal)
  protected
    function GetVarType: TExecutorVariableType; override;
  end;

  { TExecVarSystem }
  TExecVarSystem = class(TExecVarResultConst);

  { TExecVarSystemDateTime }

  TExecVarSystemDateTime = class(TExecVarSystem)
  protected
    function GetAsBooleanV(const Index: Integer): Boolean; override;
    function GetAsIntegerV(const Index: Integer): EpiInteger; override;
    function GetAsFloatV(const   Index: Integer): EpiFloat; override;
    function GetAsStringV(const  Index: Integer): EpiString; override;
  public
    constructor Create;
  end;

  { TExecVarVector }

  TExecVarVector = class(TCustomExecutorDataVariable)
  private
    function GetLength: Integer;
  protected
    FField: TEpiField;
    function GetAsBooleanV(const Index: Integer): Boolean; override;
    function GetAsIntegerV(const Index: Integer): EpiInteger; override;
    function GetAsFloatV(const Index: Integer): EpiFloat; override;
    function GetAsStringV(const Index: Integer): EpiString; override;
    function GetAsDateV(const Index: Integer): EpiDate; override;
    function GetAsTimeV(const Index: Integer): EpiTime; override;
    procedure SetAsBooleanV(const Index: Integer; AValue: Boolean); override;
    procedure SetAsIntegerV(const Index: Integer; AValue: EpiInteger); override;
    procedure SetAsFloatV(const Index: Integer; AValue: EpiFloat); override;
    procedure SetAsStringV(const Index: Integer; AValue: EpiString); override;
    procedure SetAsDateV(const Index: Integer; AValue: EpiDate); override;
    procedure SetAsTimeV(const Index: Integer; AValue: EpiTime); override;
    function  GetAsMissingV(const Index: Integer): boolean; override;
    procedure SetAsMissingV(const Index: Integer; AValue: boolean); override;
    function GetAcceptedIndices: Integer; override;
    function GetVarType: TExecutorVariableType; override;
  public
    constructor Create(Const AIdent: UTF8String; ADataType: TEpiFieldType; ALength: Integer);
    destructor Destroy; override;
    property Length: Integer read GetLength;
  end;

  { TExecVarField }

  TExecVarField = class(TExecVarVector)
  protected
    function GetVarType: TExecutorVariableType; override;
  public
    constructor Create(AField: TEpiField);
    destructor Destroy; override;
    property Field: TEpiField read FField;
  end;

  { TExecVarMatrix }

  TExecVarMatrix = class(TCustomExecutorDataVariable)
  private
    FFields: TEpiFields;
    function GetCols: Integer;
    function GetRows: Integer;
  protected
    function GetAsBooleanM(const Col, Row: Integer): Boolean; override;
    function GetAsIntegerM(const Col, Row: Integer): EpiInteger; override;
    function GetAsFloatM(const   Col, Row: Integer): EpiFloat; override;
    function GetAsStringM(const  Col, Row: Integer): EpiString; override;
    function GetAsDateM(const Col, Row: Integer): EpiDate; override;
    function GetAsTimeM(const Col, Row: Integer): EpiTime; override;
    procedure SetAsBooleanM(const Col, Row: Integer; AValue: Boolean); override;
    procedure SetAsIntegerM(const Col, Row: Integer; AValue: EpiInteger); override;
    procedure SetAsFloatM(const   Col, Row: Integer; AValue: EpiFloat); override;
    procedure SetAsStringM(const  Col, Row: Integer; AValue: EpiString); override;
    procedure SetAsDateM(const Col, Row: Integer; AValue: EpiDate); override;
    procedure SetAsTimeM(const Col, Row: Integer; AValue: EpiTime); override;
    function GetAcceptedIndices: Integer; override;
    function GetVarType: TExecutorVariableType; override;
  public
    constructor Create(Const AIdent: UTF8String; ADataType: TEpiFieldType; ACols, ARows: Integer);
    property Cols: Integer read GetCols;
    property Rows: Integer read GetRows;
  end;

  TExecutorDataVariables = specialize TFPGMap<UTF8String, TCustomExecutorDataVariable>;
  TExecutorDatasetVariables = specialize TFPGMap<UTF8String, TExecutorDatasetVariable>;
  TExecutorValuelabelsets = specialize TFPGMap<UTF8String, TExecutorValuelabelsetVariable>;

function ExecutorDataVariablesCompare(const Key1, Key2: UTF8String): Integer;

implementation

uses
  LazFileUtils, LazUTF8;

function ExecutorDataVariablesCompare(const Key1, Key2: UTF8String): Integer;
begin
  result := UTF8CompareText(Key1, Key2);
end;

{ TExecVarGlobalVector }

function TExecVarGlobalVector.GetLength: Integer;
begin
  result := FField.Size;
end;

function TExecVarGlobalVector.GetAsBooleanV(const Index: Integer): Boolean;
begin
  Result := Boolean(FField.AsBoolean[Index]);
end;

function TExecVarGlobalVector.GetAsIntegerV(const Index: Integer): EpiInteger;
begin
  Result := FField.AsInteger[Index];
end;

function TExecVarGlobalVector.GetAsFloatV(const Index: Integer): EpiFloat;
begin
  Result := FField.AsFloat[Index];
end;

function TExecVarGlobalVector.GetAsStringV(const Index: Integer): EpiString;
begin
  Result := FField.AsString[Index];
end;

function TExecVarGlobalVector.GetAsDateV(const Index: Integer): EpiDate;
begin
  Result := FField.AsDate[Index];
end;

function TExecVarGlobalVector.GetAsTimeV(const Index: Integer): EpiTime;
begin
  Result := FField.AsTime[Index];
end;

procedure TExecVarGlobalVector.SetAsBooleanV(const Index: Integer;
  AValue: Boolean);
begin
  FField.AsBoolean[Index] := Byte(AValue);
end;

procedure TExecVarGlobalVector.SetAsIntegerV(const Index: Integer;
  AValue: EpiInteger);
begin
  FField.AsInteger[Index] := AValue;
end;

procedure TExecVarGlobalVector.SetAsFloatV(const Index: Integer;
  AValue: EpiFloat);
begin
  FField.AsFloat[Index] := AValue;
end;

procedure TExecVarGlobalVector.SetAsStringV(const Index: Integer;
  AValue: EpiString);
begin
  FField.AsString[Index] := AValue;
end;

procedure TExecVarGlobalVector.SetAsDateV(const Index: Integer; AValue: EpiDate
  );
begin
  FField.AsDate[Index] := AValue;
end;

procedure TExecVarGlobalVector.SetAsTimeV(const Index: Integer; AValue: EpiTime
  );
begin
  FField.AsTime[Index] := AValue;
end;

function TExecVarGlobalVector.GetAsMissingV(const Index: Integer): boolean;
begin
  Result := FField.IsMissing[Index];
end;

procedure TExecVarGlobalVector.SetAsMissingV(const Index: Integer;
  AValue: boolean);
begin
  FField.IsMissing[Index] := AValue;
end;

function TExecVarGlobalVector.GetVarType: TExecutorVariableType;
begin
  result := evtGlobalVector;
end;

constructor TExecVarGlobalVector.Create(const AIdent: UTF8String;
  ADataType: TEpiFieldType; ALength: Integer);
begin
  inherited Create(AIdent, ADataType);
  FField := TEpiField.CreateField(nil, ADataType);
  FField.Size := ALength;
end;

{ TExecVarSystemDateTime }

function TExecVarSystemDateTime.GetAsBooleanV(const Index: Integer): Boolean;
begin
  Result := false;
end;

function TExecVarSystemDateTime.GetAsIntegerV(const Index: Integer): EpiInteger;
begin
  Result := trunc(GetAsFloatV(Index));
end;

function TExecVarSystemDateTime.GetAsFloatV(const Index: Integer): EpiFloat;
begin
  Result := Now;
end;

function TExecVarSystemDateTime.GetAsStringV(const Index: Integer): EpiString;
begin
  Result := DateTimeToStr(GetAsFloatV(Index));
end;

constructor TExecVarSystemDateTime.Create;
begin
  inherited Create('$SYSTEMDATETIME', ftString);
end;

{ TExecutorValuelabelsetVariable }

function TExecutorValuelabelsetVariable.GetVarType: TExecutorVariableType;
begin
  result := evtValuelabel;
end;

constructor TExecutorValuelabelsetVariable.Create(const AIdent: UTF8String;
  AValuelabelset: TEpiValueLabelSet);
begin
  inherited Create(AIdent);
  FValuelabelset := AValuelabelset;
end;

{ TExecParamVar }

function TExecParamVar.GetAsBoolean: Boolean;
begin

end;

function TExecParamVar.GetAsDate: EpiDate;
begin

end;

function TExecParamVar.GetAsFloat: EpiFloat;
begin

end;

function TExecParamVar.GetAsInteger: EpiInteger;
begin

end;

function TExecParamVar.GetAsMissing: boolean;
begin

end;

function TExecParamVar.GetAsString: EpiString;
begin

end;

function TExecParamVar.GetAsTime: EpiTime;
begin

end;

procedure TExecParamVar.SetAsBoolean(AValue: Boolean);
begin

end;

procedure TExecParamVar.SetAsDate(AValue: EpiDate);
begin

end;

procedure TExecParamVar.SetAsFloat(AValue: EpiFloat);
begin

end;

procedure TExecParamVar.SetAsInteger(AValue: EpiInteger);
begin

end;

procedure TExecParamVar.SetAsMissing(AValue: boolean);
begin

end;

procedure TExecParamVar.SetAsString(AValue: EpiString);
begin

end;

procedure TExecParamVar.SetAsTime(AValue: EpiTime);
begin

end;

constructor TExecParamVar.Create(const AIdent: UTF8String);
begin

end;

{ TExecVarResultConst }

function TExecVarResultConst.GetVarType: TExecutorVariableType;
begin
  Result := evtResultConst;
end;

{ TCustomExecutorVariable }

constructor TCustomExecutorVariable.Create(const AIdent: UTF8String);
begin
  FIdent := AIdent;
end;

{ TExecutorDatasetVariable }

function TExecutorDatasetVariable.GetDataFile: TEpiDataFile;
begin
  result := nil;

  if Assigned(Relation) then
    result := Relation.Datafile;
end;

function TExecutorDatasetVariable.GetVarType: TExecutorVariableType;
begin
  result := evtDataset;
end;

constructor TExecutorDatasetVariable.Create(const AIdent: UTF8String;
  ARelation: TEpiMasterRelation);
begin
  inherited Create(AIdent);
  FRelation := ARelation;
end;

{ TExecVarField }

function TExecVarField.GetVarType: TExecutorVariableType;
begin
  Result := evtField;
end;

constructor TExecVarField.Create(AField: TEpiField);
begin
  inherited Create(AField.Name, AField.FieldType, 0);
  FField.Free;
  FField := AField;
end;

destructor TExecVarField.Destroy;
begin
  FField := nil;  // prevents the inherited from calling .Free on a "live" Field.
  inherited Destroy;
end;

{ TCustomExecutorDataVariable }

function TCustomExecutorDataVariable.GetAsBooleanM(const Col, Row: Integer
  ): Boolean;
begin

end;

function TCustomExecutorDataVariable.GetAsDateV(const  Index: Integer): EpiDate;
begin

end;

function TCustomExecutorDataVariable.GetAsTimeV(const  Index: Integer): EpiTime;
begin

end;

procedure TCustomExecutorDataVariable.SetAsDateV(const  Index: Integer;
  AValue: EpiDate);
begin

end;

procedure TCustomExecutorDataVariable.SetAsTimeV(const  Index: Integer;
  AValue: EpiTime);
begin

end;

function TCustomExecutorDataVariable.GetAsDateM(const    Col, Row: Integer
  ): EpiDate;
begin

end;

function TCustomExecutorDataVariable.GetAsTimeM(const    Col, Row: Integer
  ): EpiTime;
begin

end;

procedure TCustomExecutorDataVariable.SetAsDateM(const    Col, Row: Integer;
  AValue: EpiDate);
begin

end;

procedure TCustomExecutorDataVariable.SetAsTimeM(const    Col, Row: Integer;
  AValue: EpiTime);
begin

end;

function TCustomExecutorDataVariable.GetAsMissingV(const       Index: Integer
  ): boolean;
begin
  result := false;
end;

procedure TCustomExecutorDataVariable.SetAsMissingV(const       Index: Integer;
  AValue: boolean);
begin

end;

function TCustomExecutorDataVariable.GetAsBooleanV(const Index: Integer
  ): Boolean;
begin

end;

function TCustomExecutorDataVariable.GetAsFloatM(const   Col, Row: Integer
  ): EpiFloat;
begin

end;

function TCustomExecutorDataVariable.GetAsFloatV(const   Index: Integer): EpiFloat;
begin

end;

function TCustomExecutorDataVariable.GetAsIntegerM(const Col, Row: Integer
  ): EpiInteger;
begin

end;

function TCustomExecutorDataVariable.GetAsIntegerV(const Index: Integer
  ): EpiInteger;
begin

end;

function TCustomExecutorDataVariable.GetAsStringM(const  Col, Row: Integer
  ): EpiString;
begin

end;

function TCustomExecutorDataVariable.GetAsStringV(const  Index: Integer): EpiString;
begin

end;

procedure TCustomExecutorDataVariable.SetAsBooleanM(const Col, Row: Integer;
  AValue: Boolean);
begin

end;

procedure TCustomExecutorDataVariable.SetAsBooleanV(const Index: Integer;
  AValue: Boolean);
begin

end;

procedure TCustomExecutorDataVariable.SetAsFloatM(const   Col, Row: Integer;
  AValue: EpiFloat);
begin

end;

procedure TCustomExecutorDataVariable.SetAsFloatV(const   Index: Integer;
  AValue: EpiFloat);
begin

end;

procedure TCustomExecutorDataVariable.SetAsIntegerM(const Col, Row: Integer;
  AValue: EpiInteger);
begin

end;

procedure TCustomExecutorDataVariable.SetAsIntegerV(const Index: Integer;
  AValue: EpiInteger);
begin

end;

procedure TCustomExecutorDataVariable.SetAsStringM(const  Col, Row: Integer;
  AValue: EpiString);
begin

end;

procedure TCustomExecutorDataVariable.SetAsStringV(const  Index: Integer;
  AValue: EpiString);
begin

end;

function TCustomExecutorDataVariable.GetAcceptedIndices: Integer;
begin
  result := 0;
end;

constructor TCustomExecutorDataVariable.Create(const AIdent: UTF8String;
  ADataType: TEpiFieldType);
begin
  inherited Create(AIdent);
  FDataType := ADataType;
end;

{ TExecVarGlobal }

function TExecVarGlobal.GetAsBooleanV(const Index: Integer): Boolean;
begin
  result := Boolean(FField.AsBoolean[0]);
end;

function TExecVarGlobal.GetAsFloatV(const Index: Integer): EpiFloat;
begin
  result := FField.AsFloat[0];
end;

function TExecVarGlobal.GetAsIntegerV(const Index: Integer): EpiInteger;
begin
  result := FField.AsInteger[0];
end;

function TExecVarGlobal.GetAsStringV(const Index: Integer): EpiString;
begin
  result := FField.AsString[0];
end;

function TExecVarGlobal.GetAsDateV(const Index: Integer): EpiDate;
begin
  result := FField.AsDate[0];
end;

function TExecVarGlobal.GetAsTimeV(const Index: Integer): EpiTime;
begin
  Result := FField.AsTime[0];
end;

procedure TExecVarGlobal.SetAsBooleanV(const Index: Integer; AValue: Boolean);
begin
  FField.AsBoolean[0] := EpiBool(AValue);
end;

procedure TExecVarGlobal.SetAsFloatV(const Index: Integer; AValue: EpiFloat);
begin
  FField.AsFloat[0] := AValue;
end;

procedure TExecVarGlobal.SetAsIntegerV(const Index: Integer; AValue: EpiInteger);
begin
  FField.AsInteger[0] := AValue;
end;

procedure TExecVarGlobal.SetAsStringV(const Index: Integer; AValue: EpiString);
begin
  FField.AsString[0] := AValue;
end;

procedure TExecVarGlobal.SetAsDateV(const Index: Integer; AValue: EpiDate);
begin
  FField.AsDate[0] := AValue;
end;

procedure TExecVarGlobal.SetAsTimeV(const Index: Integer; AValue: EpiTime);
begin
  FField.AsTime[0] := AValue;
end;

function TExecVarGlobal.GetAsMissingV(const Index: Integer): boolean;
begin
  Result := FField.IsMissing[0];
end;

procedure TExecVarGlobal.SetAsMissingV(const Index: Integer; AValue: boolean);
begin
  FField.IsMissing[0] := AValue;
end;

function TExecVarGlobal.GetVarType: TExecutorVariableType;
begin
  result := evtGlobal;
end;

constructor TExecVarGlobal.Create(const AIdent: UTF8String;
  ADataType: TEpiFieldType);
begin
  inherited Create(AIdent, ADataType);

  FField := TEpiField.CreateField(nil, DataType);
  FField.Size := 1;
end;

{ TExecVarVector }

function TExecVarVector.GetLength: Integer;
begin
  result := FField.Size;
end;

function TExecVarVector.GetAsBooleanV(const Index: Integer): Boolean;
begin
  result := Boolean(FField.AsBoolean[Index]);
end;

function TExecVarVector.GetAsFloatV(const Index: Integer): EpiFloat;
begin
  result := FField.AsFloat[Index];
end;

function TExecVarVector.GetAsIntegerV(const Index: Integer): EpiInteger;
begin
  result := FField.AsInteger[Index];
end;

function TExecVarVector.GetAsStringV(const Index: Integer): EpiString;
begin
  result := FField.AsString[Index];
end;

function TExecVarVector.GetAsDateV(const Index: Integer): EpiDate;
begin
  result := FField.AsDate[Index];
end;

function TExecVarVector.GetAsTimeV(const Index: Integer): EpiTime;
begin
  result := FField.AsDateTime[Index];
end;

procedure TExecVarVector.SetAsBooleanV(const Index: Integer; AValue: Boolean);
begin
  FField.AsBoolean[Index] := EpiBool(AValue);
end;

procedure TExecVarVector.SetAsFloatV(const Index: Integer; AValue: EpiFloat);
begin
  FField.AsFloat[Index] := AValue;
end;

procedure TExecVarVector.SetAsIntegerV(const Index: Integer; AValue: EpiInteger);
begin
  FField.AsInteger[Index] := AValue;
end;

procedure TExecVarVector.SetAsStringV(const Index: Integer; AValue: EpiString);
begin
  FField.AsString[Index] := AValue;
end;

procedure TExecVarVector.SetAsDateV(const Index: Integer; AValue: EpiDate);
begin
  FField.AsDate[Index] := AValue;
end;

procedure TExecVarVector.SetAsTimeV(const Index: Integer; AValue: EpiTime);
begin
  FField.AsTime[Index] := AValue;
end;

function TExecVarVector.GetAsMissingV(const Index: Integer): boolean;
begin
  Result := FField.IsMissing[Index];
end;

procedure TExecVarVector.SetAsMissingV(const Index: Integer; AValue: boolean);
begin
  FField.IsMissing[Index] := AValue;
end;

function TExecVarVector.GetAcceptedIndices: Integer;
begin
  Result := 1;
end;

function TExecVarVector.GetVarType: TExecutorVariableType;
begin
  result := evtResultVector;
end;

constructor TExecVarVector.Create(const AIdent: UTF8String;
  ADataType: TEpiFieldType; ALength: Integer);
begin
  inherited Create(AIdent, ADataType);

  FField := TEpiField.CreateField(nil, DataType);
  FField.Size := ALength;
end;

destructor TExecVarVector.Destroy;
begin
  FField.Free;
  inherited Destroy;
end;

{ TExecVarMatrix }

function TExecVarMatrix.GetCols: Integer;
begin
  result := FFields.Count;
end;

function TExecVarMatrix.GetRows: Integer;
begin
  result := 0;
  if FFields.Count > 0 then
    Result := FFields[0].Size;
end;

function TExecVarMatrix.GetAsBooleanM(const Col, Row: Integer): Boolean;
begin
  result := Boolean(FFields[Col].AsBoolean[Row]);
end;

function TExecVarMatrix.GetAsFloatM(const   Col, Row: Integer): EpiFloat;
begin
  result := FFields[Col].AsFloat[Row];
end;

function TExecVarMatrix.GetAsIntegerM(const Col, Row: Integer): EpiInteger;
begin
  result := FFields[Col].AsInteger[Row];
end;

function TExecVarMatrix.GetAsStringM(const  Col, Row: Integer): EpiString;
begin
  result := FFields[Col].AsString[Row];
end;

function TExecVarMatrix.GetAsDateM(const Col, Row: Integer): EpiDate;
begin
  Result := FFields[Col].AsDate[Row];
end;

function TExecVarMatrix.GetAsTimeM(const Col, Row: Integer): EpiTime;
begin
  Result := FFields[Col].AsDateTime[Row];
end;

procedure TExecVarMatrix.SetAsBooleanM(const Col, Row: Integer; AValue: Boolean
  );
begin
  FFields[Col].AsBoolean[Row] := EpiBool(AValue);
end;

procedure TExecVarMatrix.SetAsFloatM(const   Col, Row: Integer; AValue: EpiFloat);
begin
  FFields[Col].AsFloat[Row] := AValue;
end;

procedure TExecVarMatrix.SetAsIntegerM(const Col, Row: Integer; AValue: EpiInteger
  );
begin
  FFields[Col].AsInteger[Row] := AValue;
end;

procedure TExecVarMatrix.SetAsStringM(const  Col, Row: Integer; AValue: EpiString
  );
begin
  FFields[Col].AsString[Row] := AValue;
end;

procedure TExecVarMatrix.SetAsDateM(const Col, Row: Integer; AValue: EpiDate);
begin
  FFields[Col].AsDate[Row] := AValue;
end;

procedure TExecVarMatrix.SetAsTimeM(const Col, Row: Integer; AValue: EpiTime);
begin
  FFields[Col].AsDateTime[Row] := AValue;
end;

function TExecVarMatrix.GetAcceptedIndices: Integer;
begin
  result := 2;
end;

function TExecVarMatrix.GetVarType: TExecutorVariableType;
begin
  result := evtResultMatrix;
end;

constructor TExecVarMatrix.Create(const AIdent: UTF8String;
  ADataType: TEpiFieldType; ACols, ARows: Integer);
var
  i: Integer;
begin
  inherited Create(AIdent, ADataType);

  FFields := TEpiFields.Create(nil);
  for i := 0 to ACols -1 do
    FFields.NewField(DataType).Size := ARows;
end;
end.

