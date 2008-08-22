unit BaseDataset;

interface

uses Classes,SysUtils,Windows,Forms,DB;

Type
PRecordInfo = ^TRecordInfo;
TRecordInfo=record
  RecordID: Pointer;
  Bookmark: Pointer;
  BookMarkFlag: TBookmarkFlag;
End;

TGXBaseDataset=class(TDataset)
  private
    FisOpen: Boolean;
    FStartCalculated: Integer;
    FBufferMap: TStringList;
    procedure FillBufferMap;
  protected {My simplified methods to override}
    function DoOpen: Boolean; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoDeleteRecord; virtual;
    procedure DoCreateFieldDefs; virtual; abstract;
    function GetFieldValue(Field: TField): Variant; virtual; abstract;
    procedure SetFieldValue(Field: TField; Value: Variant); virtual; abstract;
    procedure GetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    procedure SetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    //Called before and after getting a set of field values
    procedure DoBeforeGetFieldValue; virtual;
    procedure DoAfterGetFieldValue; virtual;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); virtual;
    procedure DoAfterSetFieldValue(Inserting: Boolean); virtual;
    //Handle buffer ID
    function AllocateRecordID: Pointer; virtual; abstract;
    procedure DisposeRecordID(Value: Pointer); virtual; abstract;
    procedure GotoRecordID(Value: Pointer); virtual; abstract;
    //BookMark functions
    function GetBookMarkSize: Integer; virtual;
    procedure DoGotoBookmark(Bookmark: Pointer); virtual; abstract;
    procedure AllocateBookMark(RecordID: Pointer; Bookmark: Pointer); virtual; abstract;
    //Navigation methods
    procedure DoFirst; virtual; abstract;
    procedure DoLast; virtual; abstract;
    function Navigate(GetMode: TGetMode): TGetResult; virtual; abstract;
    //Internal isOpen property
    property isOpen: Boolean read FisOpen;
  protected {TGXBaseDataset Internal functions that can be overriden if needed}
    procedure AllocateBLOBPointers(Buffer: PChar); virtual;
    procedure FreeBlobPointers(Buffer: PChar); virtual;
    procedure FreeRecordPointers(Buffer: PChar); virtual;
    function GetDataSize: Integer; virtual;
    function GetFieldOffset(Field: TField): Integer; virtual;
    procedure BufferToRecord(Buffer: PChar); virtual;
    procedure RecordToBuffer(Buffer: PChar); virtual;
  protected
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalInsert; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalEdit; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    function IsCursorOpen: Boolean; override;
    function GetCanModify: Boolean; override;
    procedure ClearCalcFields(Buffer: PChar); override;
    function GetActiveRecordBuffer: PChar; virtual;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
End;

TGXBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TGXBaseDataSet;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FOpened: Boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
end;

implementation

{ TGXBaseDataset }
constructor TGXBaseDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBufferMap:=TStringList.Create;
end;

destructor TGXBaseDataset.Destroy;
begin
  if Active then Close;
  FBufferMap.Free;
  inherited Destroy;
end;

procedure TGXBaseDataset.FillBufferMap;
var Index: Integer;
Begin
  FBufferMap.Clear;
  for Index:=0 to FieldCount-1 do
    FBufferMap.Add(Fields[Index].FieldName);
End;

procedure TGXBaseDataset.InternalOpen;
begin
  if DoOpen then
    Begin
    BookmarkSize:=GetBookMarkSize;  //Bookmarks not supported
    InternalInitFieldDefs;
    if DefaultFields then CreateFields;
    BindFields(True);
    FisOpen:=True;
    FillBufferMap;
    End;
end;

function TGXBaseDataset.AllocRecordBuffer: PChar;
begin
  GetMem(Result,GetRecordSize);
  FillChar(Result^,GetRecordSize,0);
  AllocateBlobPointers(Result);
end;

procedure TGXBaseDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeRecordPointers(Buffer);
  FreeMem(Buffer,GetRecordSize);
end;

procedure TGXBaseDataset.FreeRecordPointers(Buffer: PChar);
Begin
  FreeBlobPointers(Buffer);
  DisposeRecordID(PRecordInfo(Buffer+GetDataSize).RecordID);
  if PRecordInfo(Buffer+GetDataSize)^.BookMark<>nil then
    Begin
    FreeMem(PRecordInfo(Buffer+GetDataSize)^.BookMark);
    PRecordInfo(Buffer+GetDataSize)^.BookMark:=nil;
    End;
End;

procedure TGXBaseDataset.AllocateBLOBPointers(Buffer: PChar);
var Index: Integer;
Offset: Integer;
Stream: TMemoryStream;
Begin
  for Index:=0 to FieldCount-1 do
    if Fields[Index].DataType in [ftMemo,ftGraphic] then
      Begin
      Offset:=GetFieldOffset(Fields[Index]);
      Stream:=TMemoryStream.Create;
      Move(Pointer(Stream),(Buffer+Offset)^,sizeof(Pointer));
      End;
End;

procedure TGXBaseDataset.FreeBlobPointers(Buffer: PChar);
var Index: Integer;
Offset: Integer;
FreeObject: TObject;
Begin
  for Index:=0 to FieldCount-1 do
    if Fields[Index].DataType in [ftMemo,ftGraphic] then
      Begin
      Offset:=GetFieldOffset(Fields[Index]);
      Move((Buffer+Offset)^,Pointer(FreeObject),sizeof(Pointer));
      if FreeObject<>nil then FreeObject.Free;
      FreeObject:=nil;
      Move(Pointer(FreeObject),(Buffer+Offset)^,sizeof(Pointer));
      End;
End;

procedure TGXBaseDataset.InternalInitFieldDefs;
begin
  DoCreateFieldDefs;
end;

procedure TGXBaseDataset.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FStartCalculated],CalcFieldsSize,0);
end;

function TGXBaseDataset.GetActiveRecordBuffer: PChar;
begin
  Case State of
    dsBrowse: if isEmpty then Result:=nil else Result:=ActiveBuffer;
    dsCalcFields: Result:=CalcBuffer;
    dsFilter: Result:=nil;
    dsEdit,dsInsert: Result:=ActiveBuffer;
    else Result:=nil;
  End;
end;

function TGXBaseDataset.GetCanModify: Boolean;
begin
  Result:=False;
end;

function TGXBaseDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result:=Navigate(GetMode);
  if (Result=grOk) then
    Begin
    RecordToBuffer(Buffer);
    ClearCalcFields(Buffer);
    GetCalcFields(Buffer);
    End
  else if (Result=grError) and DoCheck then DatabaseError('No Records');
end;

function TGXBaseDataset.GetRecordSize: Word;
begin
  Result:=GetDataSize+sizeof(TRecordInfo)+CalcFieldsSize;
  FStartCalculated:=GetDataSize+sizeof(TRecordInfo);
end;

function TGXBaseDataset.GetDataSize: Integer;
var Index: Integer;
Begin
  Result:=0;
  for Index:=0 to FieldCount-1 do
    case Fields[Index].DataType of
      ftString: Result:=Result+Fields[Index].Size+1; //Leave space for terminating null
      ftInteger,ftSmallInt,ftDate,ftTime: Result:=Result+sizeof(Integer);
      ftFloat,ftCurrency,ftBCD,ftDateTime: Result:=Result+sizeof(Double);
      ftBoolean: Result:=Result+sizeof(WordBool);
      ftMemo,ftGraphic: Result:=Result+sizeof(Pointer);
    End;
End;

procedure TGXBaseDataset.InternalClose;
begin
  BindFields(False);
  if DefaultFields then DestroyFields;
  DoClose;
  FisOpen:=False;
end;

procedure TGXBaseDataset.InternalDelete;
begin
  DoDeleteRecord;
end;

procedure TGXBaseDataset.InternalEdit;
begin
  if GetActiveRecordBuffer<>nil then
    InternalSetToRecord(GetActiveRecordBuffer);
end;

procedure TGXBaseDataset.InternalFirst;
begin
  DoFirst;
end;

procedure TGXBaseDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

{This is called by the TDataset to initialize an already existing buffer.
We cannot just fill the buffer with 0s since that would overwrite our BLOB pointers.
Therefore we free the blob pointers first, then fill the buffer with zeros, then
reallocate the blob pointers}
procedure TGXBaseDataset.InternalInitRecord(Buffer: PChar);
begin
  FreeRecordPointers(Buffer);
  FillChar(Buffer^,GetRecordSize,0);
  AllocateBlobPointers(Buffer);
end;

procedure TGXBaseDataset.InternalInsert;
begin

end;

procedure TGXBaseDataset.InternalLast;
begin
  DoLast;
end;

procedure TGXBaseDataset.InternalPost;
begin
  if FisOpen then
    Begin
    DoBeforeSetFieldValue(State=dsInsert);
    BufferToRecord(GetActiveRecordBuffer);
    DoAfterSetFieldValue(State=dsInsert);
    End;
end;

procedure TGXBaseDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then InternalLast;
  DoBeforeSetFieldValue(True);
  BufferToRecord(Buffer);
  DoAfterSetFieldValue(True);
end;

procedure TGXBaseDataset.InternalSetToRecord(Buffer: PChar);
begin
  GotoRecordID(PRecordInfo(Buffer+GetDataSize).RecordID);
end;

function TGXBaseDataset.IsCursorOpen: Boolean;
begin
  Result:=FisOpen;
end;

function TGXBaseDataset.GetFieldOffset(Field: TField): Integer;
var Index,FPos: Integer;
Begin
  Result:=0;
  FPos:=FBufferMap.Indexof(Field.FieldName);
  for Index:=0 to FPos-1 do
    Begin
    Case FieldbyName(FBufferMap[Index]).DataType of
      ftString: inc(Result,FieldbyName(FBufferMap[Index]).Size+1);
      ftInteger,ftSmallInt,ftDate,ftTime: inc(Result,sizeof(Integer));
      ftDateTime,ftFloat,ftBCD,ftCurrency: inc(Result,sizeof(Double));
      ftBoolean: inc(Result,sizeof(WordBool));
      ftGraphic,ftMemo: inc(Result,sizeof(Pointer));
    End;
    End;
End;

procedure TGXBaseDataset.BufferToRecord(Buffer: PChar);
var TempStr: String;
TempInt: Integer;
TempDouble: Double;
TempBool: WordBool;
Offset: Integer;
Index: Integer;
Stream: TStream;
begin
  for Index:=0 to FieldCount-1 do
    Begin
    Offset:=GetFieldOffset(Fields[Index]);
    Case Fields[Index].DataType of
      ftString:
        Begin
        TempStr:=PChar(Buffer+Offset);
        SetFieldValue(Fields[Index],TempStr);
        End;
      ftInteger,ftSmallInt,ftDate,ftTime:
        Begin
        Move((Buffer+Offset)^,TempInt,sizeof(Integer));
        SetFieldValue(Fields[Index],TempInt);
        End;
      ftFloat,ftBCD,ftCurrency,ftDateTime:
        Begin
        Move((Buffer+Offset)^,TempDouble,sizeof(Double));
        SetFieldValue(Fields[Index],TempDouble);
        End;
      ftBoolean:
        Begin
        Move((Buffer+Offset)^,TempBool,sizeof(WordBool));
        SetFieldValue(Fields[Index],TempBool);
        End;
      ftGraphic,ftMemo:
        Begin
        Move((Buffer+Offset)^,Pointer(Stream),sizeof(Pointer));
        Stream.Position:=0;
        SetBlobField(Fields[Index],Stream);
        End;
      End;
    End;
end;

procedure TGXBaseDataset.RecordToBuffer(Buffer: PChar);
var Value: Variant;
TempStr: String;
TempInt: Integer;
TempDouble: Double;
TempBool: WordBool;
Offset: Integer;
Index: Integer;
Stream: TStream;
begin
  with PRecordInfo(Buffer+GetDataSize)^ do
    Begin
    BookmarkFlag:=bfCurrent;
    RecordID:=AllocateRecordID;
    if GetBookMarkSize>0 then
      Begin
      if BookMark=nil then
        GetMem(BookMark,GetBookMarkSize);
      AllocateBookMark(RecordID,BookMark);
      End
    else BookMark:=nil;
    End;
  DoBeforeGetFieldValue;
  for Index:=0 to FieldCount-1 do
    Begin
    if not (Fields[Index].DataType in [ftMemo,ftGraphic]) then
      Value:=GetFieldValue(Fields[Index]);
    Offset:=GetFieldOffset(Fields[Index]);
    Case Fields[Index].DataType of
      ftString:
        Begin
        TempStr:=Value;
{        if length(TempStr)>Fields[Index].Size then
          System.Delete(TempStr,Fields[Index].Size,length(TempStr)-Fields[Index].Size);}
        StrLCopy(PChar(Buffer+Offset),PChar(TempStr),{length(TempStr)} Fields[Index].Size);
        End;
      ftInteger,ftSmallInt,ftDate,ftTime:
        Begin
        TempInt:=Value;
        Move(TempInt,(Buffer+Offset)^,sizeof(TempInt));
        End;
      ftFloat,ftBCD,ftCurrency,ftDateTime:
        Begin
        TempDouble:=Value;
        Move(TempDouble,(Buffer+Offset)^,sizeof(TempDouble));
        End;
      ftBoolean:
        Begin
        TempBool:=Value;
        Move(TempBool,(Buffer+Offset)^,sizeof(TempBool));
        End;
      ftMemo,ftGraphic:
        Begin
        Move((Buffer+Offset)^,Pointer(Stream),sizeof(Pointer));
        Stream.Size:=0; Stream.Position:=0;
        GetBlobField(Fields[Index],Stream);
        End;
    End;
    End;
  DoAfterGetFieldValue;
end;

procedure TGXBaseDataset.DoDeleteRecord;
begin
  //Nothing in base class
end;

function TGXBaseDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var RecBuffer: PChar;
Offset: Integer;
TempDouble: Double;
Data: TDateTimeRec;
TimeStamp: TTimeStamp;
TempBool: WordBool;
begin
  Result:=false;
  if not FisOpen then exit;
  RecBuffer:=GetActiveRecordBuffer;
  if RecBuffer=nil then exit;
  if Buffer=nil then
    Begin
    //Dataset checks if field is null by passing a nil buffer
    //Tell it is not null by passing back a result of True
    Result:=True;
    exit;
    End;
  if (Field.FieldKind=fkCalculated) or (Field.FieldKind=fkLookup) then
    Begin
    inc(RecBuffer,FStartCalculated+Field.Offset);
    if (RecBuffer[0]=#0) or (Buffer=nil) then exit
    else CopyMemory(Buffer,@RecBuffer[1],Field.DataSize);
    End
  else
    Begin
    Offset:=GetFieldOffset(Field);
    Case Field.DataType of
      ftInteger,ftTime,ftDate:  Move((RecBuffer+Offset)^,Integer(Buffer^),sizeof(Integer));
      ftBoolean:
        Begin
        Move((RecBuffer+Offset)^,TempBool,sizeof(WordBool));
        Move(TempBool,WordBool(Buffer^),sizeof(WordBool));
        End;
      ftString: StrLCopy(Buffer, PChar(RecBuffer+Offset), StrLen(PChar(RecBuffer+Offset)));
      ftCurrency,ftFloat: Move((RecBuffer+Offset)^,Double(Buffer^),sizeof(Double));
      ftDateTime:
        Begin
        Move((RecBuffer+Offset)^,TempDouble,sizeof(Double));
        TimeStamp:=DateTimeToTimeStamp(TempDouble);
        Data.DateTime:=TimeStampToMSecs(TimeStamp);
        Move(Data,Buffer^,sizeof(TDateTimeRec));
        End;
    End;
    End;
  Result:=True;
end;

procedure TGXBaseDataset.SetFieldData(Field: TField; Buffer: Pointer);
var Offset: Integer;
RecBuffer: Pchar;
TempDouble: Double;
Data: TDateTimeRec;
TimeStamp: TTimeStamp;
TempBool: WordBool;
Begin
  if not Active then exit;
  RecBuffer:=GetActiveRecordBuffer;
  if RecBuffer=nil then exit;
  if Buffer=nil then exit;
  if (Field.FieldKind=fkCalculated) or (Field.FieldKind=fkLookup) then
    Begin
    Inc(RecBuffer,FStartCalculated+Field.Offset);
    Boolean(RecBuffer[0]):=(Buffer<>nil);
    if Boolean(RecBuffer[0]) then
      CopyMemory(@RecBuffer[1],Buffer,Field.DataSize);
    End
  else
    Begin
    Offset:=GetFieldOffset(Field);
    Case Field.DataType of
      ftInteger,ftDate,ftTime: Move(Integer(Buffer^),(RecBuffer+Offset)^,sizeof(Integer));
      ftBoolean:
        Begin
        Move(WordBool(Buffer^),TempBool,sizeof(WordBool));
        Move(TempBool,(RecBuffer+Offset)^,sizeof(WordBool));
        end;
      ftString: StrLCopy(PChar(RecBuffer+Offset), Buffer, StrLen(PChar(Buffer)));
      ftDateTime:
        Begin
        Data:=TDateTimeRec(Buffer^);
        TimeStamp:=MSecsToTimeStamp(Data.DateTime);
        TempDouble:=TimeStampToDateTime(TimeStamp);
        Move(TempDouble,(RecBuffer+Offset)^,sizeof(TempDouble));
        End;
      ftFloat,ftCurrency: Move(Double(Buffer^),(RecBuffer+Offset)^,sizeof(Double));
    End;
    End;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
End;

function TGXBaseDataset.GetBookMarkSize: Integer;
begin
  Result:=0;
end;

procedure TGXBaseDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if BookMarkSize>0 then
    AllocateBookMark(PRecordInfo(Buffer+GetDataSize).RecordID,Data);
end;

function TGXBaseDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result:=PRecordInfo(Buffer+GetDataSize).BookMarkFlag;
end;

procedure TGXBaseDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if PRecordInfo(Buffer+GetDataSize)^.BookMark=nil then
    GetMem(PRecordInfo(Buffer+GetDataSize)^.BookMark,GetBookMarkSize);
  Move(PRecordInfo(Buffer+GetDataSize).BookMark^,Data,GetBookMarkSize);
end;

procedure TGXBaseDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer+GetDataSize).BookMarkFlag:=Value;
end;

procedure TGXBaseDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
  DoGotoBookMark(BookMark);
end;

function TGXBaseDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result:=TGXBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TGXBaseDataset.DoAfterGetFieldValue;
begin

end;

procedure TGXBaseDataset.DoBeforeGetFieldValue;
begin

end;

procedure TGXBaseDataset.DoAfterSetFieldValue(Inserting: Boolean);
begin

end;

procedure TGXBaseDataset.DoBeforeSetFieldValue(Inserting: Boolean);
begin

end;

//************************** TOBlobStream ***************************************
constructor TGXBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
Begin
  inherited Create;
  FField:=Field;
  FMode:=Mode;
  FDataSet:=FField.DataSet as TGXBaseDataset;
  if Mode<>bmWrite then LoadBlobData;
End;

destructor TGXBlobStream.Destroy;
Begin
  if FModified then SaveBlobData;
  inherited Destroy;
End;

function TGXBlobStream.Read(var Buffer; Count: Longint): Longint;
Begin
  Result:=inherited Read(Buffer,Count);
  FOpened:=True;
End;

function TGXBlobStream.Write(const Buffer; Count: Longint): Longint;
Begin
  Result:=inherited Write(Buffer,Count);
  FModified:=True;
End;

procedure TGXBlobStream.LoadBlobData;
var Stream: TMemoryStream;
Offset: Integer;
RecBuffer: PChar;
Begin
  Self.Size:=0;
  RecBuffer:=FDataset.GetActiveRecordBuffer;
  if RecBuffer<>nil then
    Begin
    Offset:=FDataset.GetFieldOffset(FField);
    Move((RecBuffer+Offset)^,Pointer(Stream),sizeof(Pointer));
    Self.CopyFrom(Stream,0);
    End;
  Position:=0;
End;

procedure TGXBlobStream.SaveBlobData;
var Stream: TMemoryStream;
Offset: Integer;
RecBuffer: Pchar;
Begin
  RecBuffer:=FDataset.GetActiveRecordBuffer;
  if RecBuffer<>nil then
    Begin
    Offset:=FDataset.GetFieldOffset(FField);
    Move((RecBuffer+Offset)^,Pointer(Stream),sizeof(Pointer));
    Stream.Size:=0;
    Stream.CopyFrom(Self,0);
    Stream.Position:=0;
    End;
  FModified:=False;
End;

end.
