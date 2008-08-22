unit UFrameDS;

interface

uses Classes,SysUtils,Windows,DB,BaseDataset,UVectors, ansDatatypes;

Type

TSMFrameDataSet=class(TGXBaseDataset)
  private
    FCurRec: Integer;
    FReadOnly: Boolean;
    fDataFrame: TEpiDataFrame;
    procedure SetReadOnly(Value: Boolean);
    procedure SetDataFrame(const Value: TEpiDataFrame);
  protected {Simplified Dataset methods}
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoDeleteRecord; override;
    procedure DoCreateFieldDefs; override;
    function GetFieldValue(Field: TField): Variant; override;
    procedure SetFieldValue(Field: TField; Value: Variant); override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    procedure SetBlobField(Field: TField; Stream: TStream); override;
    procedure DoFirst; override;
    procedure DoLast; override;
    function Navigate(GetMode: TGetMode): TGetResult; override;
    //Record ID functions
    function AllocateRecordID: Pointer; override;
    procedure DisposeRecordID(Value: Pointer); override;
    procedure GotoRecordID(Value: Pointer); override;
    //Bookmark functions
    function GetBookMarkSize: Integer; override;
    procedure AllocateBookMark(RecordID: Pointer; Bookmark: Pointer); override;
    procedure DoGotoBookmark(Bookmark: Pointer); override;
    //Others
    procedure DoBeforeGetFieldValue; override;
    procedure DoAfterGetFieldValue; override;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); override;
    procedure DoAfterSetFieldValue(Inserting: Boolean); override;
  protected {Overriden datatset methods}
    function GetCanModify: Boolean; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
  published
    property DataFrame : TEpiDataFrame read fDataFrame write SetDataFrame;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
End;

implementation

{ TSMFrameDataSet }

function VectorTypeToDelphiFieldType(vtype:word):TFieldType;
begin
result:= ftUnknown;
case vtype of
  EpiTyBoolean : result:=ftBoolean;
  EpiTyInteger : result:=ftInteger;
  EpiTyFloat   : result:=ftFloat;
  EpiTyDate    : result:=ftDatetime;
  EpiTyString  : result:=ftString;
end;
end;

procedure TSMFrameDataSet.DoClose;
begin
 DataFrame :=nil;
end;

procedure TSMFrameDataSet.DoCreateFieldDefs;
var
  i, co , vsize: integer;
  v     : TEpiVector;
  vtype : TfieldType;
begin
  if DataFrame=nil then
       DatabaseError('Dataframe is not assigned');
  co := DataFrame.VectorCount;
  for i:= 0 to co -1 do
  begin
     v := DataFrame.Vectors[i];
     vtype := VectorTypeToDelphiFieldType(v.DataType);
     vsize:=0;
     if vtype =ftString then vsize := DataFrame.Vectors[i].DataSize;
     FieldDefs.Add(v.Name,vtype,vsize,False);
  end;
end;

procedure TSMFrameDataSet.DoDeleteRecord;
begin

end;

function TSMFrameDataSet.DoOpen: Boolean;
begin
  FCurRec:=-1;
  Result:=(DataFrame<>nil);
end;

procedure TSMFrameDataSet.DoAfterGetFieldValue;
begin
end;

procedure TSMFrameDataSet.DoBeforeGetFieldValue;
begin
//  FMailItem:=FItems.Item(FCurRec+1) as MailItem;
end;

function TSMFrameDataSet.GetFieldValue(Field: TField): Variant;
var
  i : variant;
begin
  if FCurRec<0 then exit;
  result:= dataframe.Vectors[Field.FieldNo-1].Value[FCurRec+1];
//  result:=i;
end;

procedure TSMFrameDataSet.DoBeforeSetFieldValue(Inserting: Boolean);
begin
{  if Inserting then FMailItem:=FItems.Add(olMailItem) as MailItem
  else FMailItem:=FItems.Item(FCurRec+1) as MailItem;}
end;

procedure TSMFrameDataSet.DoAfterSetFieldValue(Inserting: Boolean);
//var Index: Integer;
begin
{  FMailItem.Save;
  if Inserting then
    Begin
    FMailItem:=FMailItem.Move(FFolder) as MailItem;
    Index:=EntryIDToIndex(FMailItem.EntryID);
    if Index>=1 then FCurRec:=Index-1;
    End;
  FMailItem:=nil;}
end;

procedure TSMFrameDataSet.SetFieldValue(Field: TField; Value: Variant);
begin
  if FCurRec<0 then exit;
//  result:= dataframe.Vectors[Field.FieldNo].Value[FCurRec+1];
end;

procedure TSMFrameDataSet.GetBlobField(Field: TField; Stream: TStream);
begin
end;

procedure TSMFrameDataSet.SetBlobField(Field: TField; Stream: TStream);
begin
end;


function TSMFrameDataSet.GetRecordCount: Longint;
Begin
  Result:=dataframe.RowCount;
End;

procedure TSMFrameDataSet.DoFirst;
Begin
  FCurRec:=-1;
End;

procedure TSMFrameDataSet.DoLast;
Begin
  FCurRec:=RecordCount;
End;

procedure TSMFrameDataSet.SetRecNo(Value: Integer);
begin
  if (Value>0) and (Value<RecordCount) then
  begin
    FCurRec:=Value-1;
    Resync([]);
  end;
end;

function TSMFrameDataSet.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FCurRec=-1) and (RecordCount>0) then
    Result := 1 else
    Result := FCurRec + 1;
end;

function TSMFrameDataSet.Navigate(GetMode: TGetMode): TGetResult;
begin
  if RecordCount<1 then
    Result := grEOF
  else
    begin
    Result:=grOK;
    case GetMode of
      gmNext:
        Begin
        if FCurRec>=RecordCount-1 then Result:=grEOF
        else Inc(FCurRec);
        End;
      gmPrior:
        Begin
        if FCurRec<=0 then
          Begin
          Result:=grBOF;
          FCurRec:=-1;
          End
        else Dec(FCurRec);
        End;
      gmCurrent:
        if (FCurRec < 0) or (FCurRec >= RecordCount) then
          Result := grError;
    End;
    End;
end;

function TSMFrameDataSet.AllocateRecordID: Pointer;
begin
  Result:=Pointer(FCurRec);
end;

procedure TSMFrameDataSet.DisposeRecordID(Value: Pointer);
begin
  //Do nothing, no need to dispose since pointer is just an integer
end;

procedure TSMFrameDataSet.GotoRecordID(Value: Pointer);
begin
  FCurRec:=Integer(Value);
end;

procedure TSMFrameDataSet.SetReadOnly(Value: Boolean);
begin
  if Value<>FReadOnly then
    Begin
    if Active then DatabaseError('Cannot change readonly property when dataset is active');
    FReadOnly:=Value;
    End;
end;

function TSMFrameDataSet.GetCanModify: Boolean;
begin
  Result:=not FReadOnly;
end;

procedure TSMFrameDataSet.AllocateBookMark(RecordID: Pointer; Bookmark: Pointer);
begin
  PInteger(Bookmark)^:=Integer(RecordID);
end;

function TSMFrameDataSet.GetBookMarkSize: Integer;
begin
  Result:=sizeof(Integer);
end;

procedure TSMFrameDataSet.DoGotoBookmark(Bookmark: Pointer);
begin
  GotoRecordID(Pointer(PInteger(Bookmark)^));
end;

procedure TSMFrameDataSet.SetDataFrame(const Value: TEpiDataFrame);
begin
  fDataFrame := Value;
end;

end.
