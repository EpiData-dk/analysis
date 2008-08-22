unit UVectorVar;

interface

uses Sysutils,UVectors,ansDataTypes,UEpidataTypes,UVariables,prExpr;


const
 dfRecordCount=1;
 dfRecDeleted=2;
 dfRecVerified=3;
 dfRecNumber=4;

Type
{# TODO2 : create CustomVar contained by Varlist and parent of DataVar and vectorVar}

TVectorVar= class(TVar)
protected
    fV : TEpiVector;
    DataFrame:TEpiDataFrame;
    IndexExp : IValue;
public
    Constructor Create(V : TEpiVector;aParameterList: TParameterList);
    function GetValue: Variant; override;
    Procedure SetValue(const value:Variant); override;
    function GetObject: TObject; override;
    function AsString: String; override;
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function AsObject: TObject; override;
    function GetVarName :string; override;
    Procedure SetVarName(const aName:string); override;
    function IsMissing2: Boolean; override;
    function IsMissingValue: Boolean; override;
    property  Vector: TEpiVector read fV;
end;

TFrameVar= class(TVar)
protected
    fDataFrame:TEpiDataFrame;
    fFuncId : integer;
public
    Constructor Create(DataFrame:TEpiDataFrame;const Identifier: string);
    function GetValue: Variant; override;
    Procedure SetValue(const value:Variant); override;
    function AsString: String; override;
    function AsFloat: extended; override;
    function AsInteger: Integer; override;
    function AsBoolean: Boolean; override;
    function GetVarName :string;override;
    Procedure SetVarName(const aName:string); override;
end;


implementation

{ TVectorVar }

function TVectorVar.AsBoolean: Boolean;
var
 idx: integer;
begin
 if IndexExp=nil then
   result:=Fv.AsBoolean[DataFrame.RowNo]
 else
 begin
    idx:= IndexExp.AsInteger;
    Result:= false;
    if (idx<1) or (idx> DataFrame.RowCount) then exit;
    idx := Dataframe.RowNoselectToRowSelect(idx);
    result:=Fv.AsBoolean[idx]
 end;
end;



function TVectorVar.AsFloat: extended;
var
 idx: integer;
begin
 if IndexExp=nil then
  result:=Fv.AsFloat[DataFrame.RowNo]
 else
 begin
    idx:= IndexExp.AsInteger;
    Result:= NA_FLOAT;
    if (idx<1) or (idx> DataFrame.RowCount) then exit;
    idx := Dataframe.RowNoselectToRowSelect(idx);
    result:=Fv.AsFloat[idx];
 end;
end;

function TVectorVar.AsInteger: Integer;
var
 idx: integer;
begin
 if IndexExp=nil then
  result:=Fv.AsInteger[DataFrame.RowNo]
 else
 begin
    idx:= IndexExp.AsInteger;
     Result:= NA_INT;
    if (idx<1) or (idx> DataFrame.RowCount) then exit;
    idx := Dataframe.RowNoselectToRowSelect(idx);
    result:=Fv.AsInteger[idx];
 end;
end;

function TVectorVar.AsObject: TObject;
begin
  
end;

function TVectorVar.GetObject: TObject;
begin
  result := self;
end;

function TVectorVar.AsString: String;
var
 idx: integer;
begin
  if IndexExp=nil then
    if Fv.AsString[DataFrame.RowNo] = ' ' then
      result := Fv.AsString[DataFrame.RowNo]
{    else if Fv.IsMissing[DataFrame.RowNo] then
      result := ''
}    else
      result:= TrimRight(Fv.AsString[DataFrame.RowNo])
  else
  begin
    idx:= IndexExp.AsInteger;
    Result:= NA_STR;
    if (idx<1) or (idx> DataFrame.RowCount) then exit;
    idx := Dataframe.RowNoselectToRowSelect(idx);
    if Fv.AsString[idx] = ' ' then
      Result := Fv.AsString[idx]
{    else if Fv.IsMissing[idx] then
     result := ''
}    else
      result := TrimRight(Fv.AsString[idx]);
 end;
end;



constructor TVectorVar.Create(V: TEpiVector;aParameterList: TParameterList);
begin
 inherited Create(aParameterList);
 if ParameterCount>0 then
 begin
     if Param[0]<> nil then
       IndexExp :=Param[0];
 end;
 fv := V;
 DataFrame := Fv.vectors.DataFrame;
 if dataframe =nil then
    raise exception.create('Nil dataframe in vector var '+ v.name);
 case Fv.FieldDataType  of
    EpiTyString,EpiTyByte,
    EpiTyUppercase : FExprType:= ttString;
    EpiTyInteger   : FExprType:= ttInteger;
    EpiTyBoolean   : FExprType:= ttBoolean;
    EpiTyFloat     : FExprType:= ttFloat;
    EpiTyDate      : FExprType:= ttDate;
  else
    FExprType:= ttObject  {should maybe just raise an exception?}
  end;
end;

function TVectorVar.GetValue: Variant;
var
 idx: integer;
begin
 if IndexExp=nil then
   result:=Fv.Value[DataFrame.RowNo]
 else
 begin
    idx := IndexExp.AsInteger;
    idx := Dataframe.RowNoselectToRowSelect(idx);
    result:=Fv.Value[idx]
 end;
end;

function TVectorVar.GetVarName: string;
begin
  result:= FV.Name;
end;

procedure TVectorVar.SetValue(const value: Variant);
begin
   Fv.Value[DataFrame.RowNo]:=value;
end;

function TVectorVar.IsMissing2: boolean;
var
  idx: integer;
begin
 if IndexExp=nil then
   result:=Fv.IsMissing[DataFrame.RowNo]
 else
 begin
    idx := IndexExp.AsInteger;
    idx := Dataframe.RowNoselectToRowSelect(idx);
    result:=Fv.IsMissing[idx]
 end;
end;

function TVectorVar.IsMissingValue: boolean;
var
  idx: integer;
begin
 if IndexExp=nil then
   result:=Fv.IsMissingValue[DataFrame.RowNo]
 else
 begin
    idx := IndexExp.AsInteger;
    idx := Dataframe.RowNoselectToRowSelect(idx);
    result:=Fv.IsMissingValue[idx]
 end;
end;

procedure TVectorVar.SetVarName(const aName: string);
begin
//do nothing
end;

{ TFrameVar }

function TFrameVar.AsBoolean: Boolean;
begin
  result := false;
  if fDataFrame = nil then exit;
  case fFuncid of
    dfRecDeleted: result := fDataFrame.Deleted[fDataFrame.RowNo];
    dfRecVerified: result := fDataFrame.Verified[fDataFrame.RowNo];
  end;
end;

function TFrameVar.AsFloat: extended;
begin
  result:=asInteger;
end;

function TFrameVar.AsInteger: Integer;
begin
  result := NA_INT;
  if fDataFrame = nil then exit;
  case fFuncid of
    dfRecordCount: result := fDataFrame.SelectedRowCount;
    dfRecNumber: result := fDataframe.RecordNo[fDataFrame.RowNo];
    dfRecDeleted,dfRecVerified: if AsBoolean then result := 1 else result := 0;
  end;
end;

function TFrameVar.AsString: String;
begin
  if FExprType=ttBoolean then result:=BoolStr[Asboolean]
  else result:=inttostr(asinteger);
end;

constructor TFrameVar.Create(DataFrame: TEpiDataFrame;const Identifier: string);
var
  ident: string;
begin
  inherited Create(nil);
  ident := AnsiUppercase(Identifier);
  fDataFrame:=DataFrame;
  if ident = 'RECORDCOUNT'      then fFuncid:=dfRecordCount
  else if ident = 'RECNUMBER'   then fFuncid:=dfRecNumber
  else if ident = 'RECDELETED'  then fFuncid:=dfRecDeleted
  else if ident = 'RECVERIFIED' then fFuncid:=dfRecVerified
  else
    EpiError('Unknown identifier '+  Identifier, 0);
  if fdataframe =nil then
    raise exception.create('Nil dataframe in dataframe var ');

  if (ident='RECDELETED') or (ident='RECVERIFIED') then
    FExprType:=ttBoolean
  else
    FExprType:=ttInteger;
end;

function TFrameVar.GetValue: Variant;
begin
  result:=asinteger;
end;

function TFrameVar.GetVarName: string;
begin
 case fFuncid of
  dfRecordCount: result:='RECORDCOUNT';
  dfRecDeleted: result:='RECDELETED';
  dfRecVerified: result:='RECVERIFIED';
  dfRecNumber:result:='RECNUMBER';
end;
end;

procedure TFrameVar.SetValue(const value: Variant);
begin
   EpiError('Invalid procedure',0);
end;

procedure TFrameVar.SetVarName(const aName: string);
begin
   EpiError('Invalid procedure',0);
end;

end.
