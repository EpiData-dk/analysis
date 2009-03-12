unit UVariables;

interface

uses sysutils,classes,prExpr,UEpidataTypes,SMutils,variants, ansDatatypes;

Type

TVar= class (TFunction)
private
   procedure ClearParamList();
protected
    FExprType: TExprType;
    FVarname: string;
    fVarValue : Variant;
    function TestParameters: Boolean; override;
public
    Constructor Create( const pName : string; pValue : Variant; pExpTyp : TExprType = ttstring);overload;
    Constructor Create( const pName : string; pValue : TObject);overload;
    Constructor Create( aParameterList: TParameterList);overload;
    function  clone:TVar;
    function GetObject: TObject; override;
    function IsMissing2: boolean; override;
    function GetVarName :string;override;
    Procedure SetVarName(const aName:string); override;
    Procedure SetParameterList(aParameterList: TParameterList);
    function GetValue: Variant; override;
    Procedure SetValue(const value:Variant); override;
    function AsString: String; override;
    function AsFloat: EpiFloat; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function AsObject: TObject; override;
    function AsIValue: IValue; override;
    function ExprType: TExprType; override;
    function AcceptParameters: boolean;
end;

TVarList =  class
protected
  List : TList;
  function GetVarByName(const VarName: string): IValue;
  function GetVarExists(const VarName: string): Boolean;
  function GetCloneVarByName(const VarName: string): IValue;
  function GetCount: integer;
  function GetItem(Index: integer): IValue;
  procedure SetItem(Index: integer; const Value: IValue);
public
  constructor Create;
  Destructor Destroy; override;
  procedure Clear;
  function GetIndexByName(const VarName: string): integer;
  function AddVar( aVar: IValue; const Multiple: boolean = false): Integer;overload;
  function AddVar( const pName : string; pValue:Variant): Integer;overload;
  function RemoveVar( aVar: IValue): Integer;
  function LoadFromString(const source:String):integer;
  function SaveToString:String;
  procedure GetVarNames(AList: TStrings);
  Property Items[Index:integer]:IValue read GetItem write SetItem;default;
  property Count: integer read GetCount;
  property VarbyName[Const VarName:string]: IValue read GetVarByName;
  property VarExists[Const VarName:string]: Boolean read GetVarExists;
  property CloneByName[Const VarName:string]: IValue read GetCloneVarByName;
end;


implementation

uses
  UDateUtils;

{ TVarList }

function TVarList.RemoveVar(aVar: IValue): Integer;
begin
   if aVar.VarName <> '' then
   begin
     Result:= GetIndexByName(aVar.VarName);
     if Result <>-1 then
     begin
        List.delete(Result);
     end;
   end;
end;


function TVarList.AddVar(aVar: IValue; const Multiple: boolean = false): Integer;
var
  lvar :IValue;
begin
   if aVar.VarName <> '' then
   begin
     Result:= GetIndexByName(aVar.VarName);
     //if the varible is already defined overwrite it
     //not if multiple is true (added Torsten Christiansen: 12-aug-2004)
     if (Result <>-1) and not Multiple then
     begin
        lvar:= items[Result];
        lvar._Release;
        items[Result]:=aVar;
        exit;
     end;
   end;
   Result:=list.Add(pointer(aVar));
   aVar._AddRef;
   if aVar.VarName ='' then
      aVar.VarName :='%'+inttostr(list.count-1);
end;


procedure TVarList.Clear;
var
 i :integer;
begin
 for i:= 0 to (Count - 1) do
    IValue(Items[i])._Release;
 list.clear;
end;

constructor TVarList.Create;
begin
    inherited Create;
    List := TList.create;
end;

destructor TVarList.Destroy;
begin
  clear;
  list.free;
  inherited;
end;

function TVarList.GetCount: integer;
begin
 if assigned(list) then
    result:=list.count
 else
   result:=0;
end;

function TVarList.GetIndexByName(const VarName: string): integer;
var
 co : integer;
begin
  co := count;
  for Result := 0 to Co - 1 do
    if AnsiCompareText(IValue(Items[Result]).VarName, VarName) = 0 then Exit;
  Result := -1;
end;

procedure TVarList.SetItem(Index: integer; const Value: IValue);
begin
    List[Index]:=pointer(Value);
    Value._AddRef;
end;

function TVarList.GetItem(Index: integer): IValue;
begin
     result:= IValue(List[Index]);
end;

function TVarList.GetVarByName(const VarName: string): IValue;
var
 idx : integer;
begin
  result:=nil;
  idx := GetIndexByName(VarName);
  if idx > -1 then
    result:=Items[idx];
end;

function TVarList.GetVarExists(const VarName: string): Boolean;
begin
  result := Assigned(VarbyName[VarName]);
end;

function TVarList.GetCloneVarByName(const VarName: string): IValue;
var
  temp: IValue;
begin
  result := nil;
  temp := GetVarByName(VarName);
  if Assigned(Temp) then
  begin
   Result := TVar.Create(Temp.VarName, Temp.Value, Temp.ExprType);
   Result.Tag := temp.Tag;
  end;
end;

function TVarList.LoadFromString(const source: String): integer;
var
 list : TStringList;
 i: integer;
begin
try
    list := TStringList.create;
    list.CommaText:=stringreplace(trim(source),' ',',', [rfReplaceAll]);
    result :=list.count;
    for i:= 0 to result-1 do
    begin
       if list[i]<>'' then
         addVar('',list[i]);
    end;
finally
   list.free;
end;
end;

function TVarList.AddVar(const pName : string; pValue:Variant): Integer;
begin
  addvar(TVar.Create(pName ,pValue));
end;

function TVarList.SaveToString: String;
var
 i : integer;
begin
 result:='';
 for i:= 0 to count-1 do
   result:=result+' '+ items[i].Value;
end;

procedure TVarList.GetVarNames(AList: TStrings);
var
  i: integer;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    for i := 0 to List.Count - 1 do
      AList.Add(IValue(List.Items[I]).VarName);
  finally
    AList.EndUpdate;
  end;
end;

{ TVar }

constructor TVar.Create(const pName: string;pValue:Variant;pExpTyp:TExprType=ttstring);
begin
  inherited create(nil);
  FVarname := pName;
  FExprType:=pExpTyp;
{  if pValue=Unassigned then
  begin
    case FExprType of
    ttFloat : fVarValue := NA_FLOAT;
    ttInteger,ttDate : fVarValue :=NA_INT;
    ttstring         : fVarValue:=''
    end
  end
  else}
  if VarIsEmpty(pvalue) then
     fVarValue:=NULL
  else
     fVarValue:=pValue;
end;

function TVar.GetValue: Variant;
begin
  if AcceptParameters then
  begin
    if Assigned(FParameterList) then
      result := fVarValue[Param[0].AsInteger, Param[1].AsInteger]
    else
      result := fVarValue;
  end else
    result:=fVarValue;
end;

function TVar.GetVarName: string;
begin
 Result:= AnsiUpperCase(FVarname);
end;

procedure TVar.SetValue(const value: Variant);
begin
  fVarValue:=value;
end;

procedure TVar.SetVarName(const aName: string);
begin
  FVarname:=aName;
end;

procedure TVar.ClearParamList();
begin
//  if Assigned(FParameterList) then
//    FreeAndNil(FParameterList);
end;

procedure TVar.SetParameterList(aParameterList: TParameterList);
begin
  Self.FParameterList := aParameterList;
end;

function TVar.AsBoolean: Boolean;
begin
  if AcceptParameters then
  begin
    if Assigned(FParameterList) then
      result := fVarValue[Param[0].AsInteger, Param[1].AsInteger]
    else
      result := fVarValue;
  end else
    result:=fVarValue;
  ClearParamList();
end;

function TVar.AsFloat: EpiFloat;
var
  saveSep:Char;
begin
  SaveSep:=SysUtils.DecimalSeparator;
  SysUtils.DecimalSeparator:=EpiDecSeparator;
  try
    if AcceptParameters then
    begin
      if Assigned(FParameterList) then
        result := fVarValue[Param[0].AsInteger, Param[1].AsInteger]
      else
        result := strToFloat(fVarValue);
    end else
      result := strToFloat(fVarValue);
  finally
    SysUtils.DecimalSeparator:=SaveSep;
    ClearParamList();
  end;
end;

function TVar.AsInteger: Integer;
begin
  case FExprType of
    ttInteger:
      begin
      if (AcceptParameters) and Assigned(FParameterList) then
        result := fVarValue[Param[0].AsInteger, Param[1].AsInteger]
      else
        result := fVarValue;
      end;
    ttDate: result := EpiStrToDatefmt(fVarValue, '');
  else
    result:=fVarValue;
  end;
  ClearParamList();
end;

function TVar.AsObject: TObject;
begin
  Result:=nil;
  if (ExprType = ttObject) then
     Result:=TObject(AsInteger);
end;

function TVar.GetObject: TObject;
begin
  result := self;
end;

function TVar.IsMissing2: boolean;
begin
  if (ExprType = ttString) then
    result := (fVarValue = NA_STR)
  else
    result := false;
end;

function TVar.AsString: String;
begin
  if (ExprType = ttString) and not (AcceptParameters) then
    if fVarValue = ' ' then
      result := fVarValue
    else
      result:= trimright(fVarValue)
  else if AcceptParameters then
  begin
    if Assigned(FParameterList) then
    begin
      if (Param[0].AsInteger < VarArrayLowBound(fVarValue, 1)) or
         (Param[0].AsInteger > VarArrayHighBound(fVarValue, 1)) then
         raise Exception.Create('Index out of bounds');
      if (Param[1].AsInteger < VarArrayLowBound(fVarValue, 2)) or
         (Param[1].AsInteger > VarArrayHighBound(fVarValue, 2)) then
         raise Exception.Create('Index out of bounds');
      result := fVarValue[Param[0].AsInteger, Param[1].AsInteger]
    end else
      // Dirty hack for result variables of tables.
      result := 'Table is not printable'
  end else
    Result:= fVarValue;
  ClearParamList();
end;


function TVar.ExprType: TExprType;
begin
  Result:= FExprType
end;

function TVar.AcceptParameters: Boolean;
begin
  result :=  VarType(fVarValue) and varArray = varArray;
end;


function TVar.AsIValue: IValue;
begin
  if TVarData(fVarValue).VType = varUnknown then
     result:=IValue(TVarData(fVarValue).VUnknown);
end;

constructor TVar.Create( aParameterList: TParameterList);
begin
  inherited Create(aParameterList);
end;
{
constructor TVar.Create(const pName: string; pValue: IValue);
begin

end;
}
constructor TVar.Create(const pName: string; pValue: TObject);
begin
    Create(pName,Integer(pvalue),ttObject);
end;

function TVar.clone:TVar;
begin
  Result := Create(FVarname,fVarValue,FExprType);
//  Result.FParameterList := Self.FParameterList;
end;

function TVar.TestParameters: boolean;
begin
  result := false;
  if VarType(fVarValue) and varArray = varArray then
  begin
    if Assigned(FParameterList) then
      if not (FParameterList.Count = 2) then
        exit;
    result := true;
  end else
    result := inherited TestParameters;
end;

end.
