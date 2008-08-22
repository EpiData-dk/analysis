{*********************************************************}
{* AARecFil                                              *}
{* Copyright (c) Julian M Bucknall 2001                  *}
{* All rights reserved.                                  *}
{*********************************************************}
{* Algorithms Alfresco: Record stream/file classes       *}
{*********************************************************}

{Note: this unit is released as freeware. In other words, you are free
       to use this unit in your own applications, however I retain all
       copyright to the code. JMB}

{modified by SM}

unit AARecFil;

interface

uses
  SysUtils, Classes, smUtils, BufferedStreams;

Type
  TStMemSize = Integer;

  TBufferedStream = class(TStream)
    private
      FBufCount: TStMemSize;   {count of valid bytes in buffer}
      FBuffer  : PAnsiChar;    {buffer into underlying stream}
      FBufOfs  : longint;      {offset of buffer in underlying stream}
      FBufPos  : TStMemSize;   {current position in buffer}
      FBufSize : TStMemSize;   {size of buffer}
      FDirty   : boolean;      {has data in buffer been changed?}
      FSize    : longint;      {size of underlying stream}
      FStream  : TStream;      {underlying stream}
    protected
      procedure bsSetStream(aValue : TStream);
      procedure bsInitForNewStream; virtual;
      function bsReadChar(var aCh : AnsiChar) : boolean;
      procedure bsReadFromStream;
      procedure bsWriteToStream;
    public
      constructor Create(aStream : TStream);
      constructor CreateEmpty;
      destructor Destroy; override;

      function Read(var Buffer; Count : longint) : longint; override;
      function Seek(Offset : longint; Origin : word) : longint; override;
      function Write(const Buffer; Count : longint) : longint; override;
      property FastSize : longint read FSize;
      property Stream : TStream read FStream write bsSetStream;
   end;


type
  TaaRecordStream = class
  private
    Fsequential: boolean;
    procedure Setsequential(const Value: boolean);
    protected
      fBufStream : TBufferedFileStream;
      FFileStrm  :TFileStream;
      FCreateNew: boolean;
      FStream   : TStream;
      FRecLen   : integer;
      RTempRec : PChar;
      fHeaderLen: integer;
      procedure rsCreateNew; virtual;
      procedure rsOpenExisting; virtual;

      function rsCalcRecordOffset(aRecNum : integer) : longint;

      procedure rsReadStream(var aRec; aRecLen : integer);
      procedure rsSeekStream(aOffset : longint);
      procedure rsWriteStream(const aRec; aRecLen : integer);
      function GetHeaderLen: integer;virtual; abstract;
      function ReadHeader: boolean;virtual;
      function WriteHeader: boolean;virtual;
      function FreeHeaderMem: boolean; virtual;
      function GetRecordCount: integer;virtual; abstract;
//      function GetRecordLen: integer; virtual; abstract;
      procedure SetRecordCount(const Value: integer);virtual;abstract;
    public
      constructor Create(aStream    : TStream; aCreateNew : boolean; aRecLen : integer);
      constructor CreateFile(const aFileName  : string; aCreateNew : boolean; aRecLen : integer);
      destructor Destroy; override;

      function Add(const aRec) : integer; virtual;
      procedure Delete(aRecNum : integer); virtual;
      procedure Open;
      function Read(aRecNum : integer; var aRec) : boolean; virtual;
      procedure UpdateHeader;virtual;
      procedure Write(aRecNum : integer; var aRec); virtual;
      property RecordLength : integer read FRecLen;
      property RecordCount : integer read GetRecordCount write SetRecordCount;
      property HeaderLen : integer read fHeaderLen;
      property sequential: boolean read Fsequential write Setsequential;
  end;

type
  TaaRecordStreamCache = class(TaaRecordStream)
    private
      FCache : TList;
      FLRU   : integer;
    protected
      procedure rscDestroyCache;
      procedure rscCreateCache(aCacheSize : integer);
      function rscGetNextLRU : integer;
    public
      constructor Create(aStream    : TStream;
                         aCreateNew : boolean;
                         aRecLen    : integer;
                         aCacheSize : integer);
      constructor CreateFile(const aFileName  : string;
                                   aCreateNew : boolean;
                                   aRecLen    : integer;
                                   aCacheSize : integer);
      destructor Destroy; override;

      function Add(const aRec) : integer; override;
      procedure Delete(aRecNum : integer); override;
      function Read(aRecNum : integer; var aRec) : boolean; override;
      procedure Write(aRecNum : integer; var aRec); override;
  end;

implementation



{====================================================================}
{$IFDEF VER80}
procedure Assert(aCondition : boolean; const aMsg : string);
begin
  if not aCondition then
    raise Exception.Create(aMsg);
end;
{$ENDIF}
{====================================================================}


{====================================================================}
constructor TaaRecordStream.Create(aStream    : TStream;
                                   aCreateNew : boolean;
                                   aRecLen    : integer);
begin
  {create the ancestor}
  inherited Create;
  {save the parameters}
  FStream := aStream;
  FRecLen := aRecLen;
  FCreateNew := aCreateNew;
  GetMem(RTempRec, RecordLength);
end;
{--------}
constructor TaaRecordStream.CreateFile(const aFileName  : string;
                                             aCreateNew : boolean;
                                             aRecLen    : integer);
begin
  {if needed, create the file}
  if aCreateNew then
  begin
    FFileStrm := TFileStream.Create(aFileName, fmCreate);
    FFileStrm.size:=0;
    FFileStrm.Free;
    FFileStrm:= nil;
  end;
{#TODO1 report more specific error messages}
  {create the file stream}
//try
//  FFileStrm:= TFileStream.Create(aFileName, fmOpenReadWrite + fmShareDenyWrite);
  fBufStream := TBufferedFileStream.CreateBuffered(aFileName,fmOpenReadWrite + fmShareDenyWrite,(1024*128*2)); ;
  //  FFileStrm:=TBufferedStream.create(fs);
//except
//  on E: EStreamError do showmsg(E.Message);
//end;
  {call the other constructor to finish off}
  Create(fBufStream, aCreateNew, aRecLen);
end;
{--------}
destructor TaaRecordStream.Destroy;
begin
//  if (Fs <> nil) then
//    Fs.Free;
  {if we created a file stream, close it}
  if fBufStream<> nil then
     fBufStream.free;
{  if (FFileStrm <> nil) then
     FFileStrm.Free;}
  {free the header record}
  FreeHeaderMem;
  FreeMem(RTempRec, RecordLength);
  {finally destroy the ancestor}
  inherited Destroy;
end;
{--------}

function TaaRecordStream.FreeHeaderMem:boolean;
begin
(*  if (FHeader <> nil) then
    FreeMem(FHeader, HeaderLen);*)
end;

function TaaRecordStream.ReadHeader:boolean;
begin
end;

procedure TaaRecordStream.rsOpenExisting;
begin
  ReadHeader;
//  FRecLen :=GetRecordLen;
  fHeaderLen:= GetHeaderLen;
  if sequential then
  begin
     rsSeekStream( fHeaderLen);
  end;
end;

function TaaRecordStream.Add(const aRec) : integer;
var
  TempRec : PChar;
  Offset  : longint;
begin
  {if the deleted record chain is empty, we'll be adding the record
   to the end of the stream; calculate its offset}
//  if (FHeader^.hr1stDelRec = cEndOfDeletedChain) then begin
    Result :=RecordCount;
    RecordCount:=result+1;

    Offset := rsCalcRecordOffset(Result);
//  end

  {otherwise, use the first deleted record, update the header record's
   deleted record chain to start at the next deleted record}
(*  else begin
    Result := FHeader^.hr1stDelRec;
    Offset := rsCalcRecordOffset(Result);
    rsSeekStream(Offset);
    rsReadStream(FHeader^.hr1stDelRec, sizeof(longint));
  end;
*)
  {seek to the record offset and write the new record}
  rsSeekStream(Offset);
//  GetMem(TempRec, RecordLength {+ SizeOfDelFlag});
  try
//    PLongint(TempRec)^ := cActiveRecord;
//    Move(aRec, TempRec[0{SizeOfDelFlag}], RecordLength);
    rsWriteStream(aRec, RecordLength );
  finally
//    FreeMem(TempRec, RecordLength {+ SizeOfDelFlag});
  end;

  {we have one more active record}
//  inc(FHeader^.hrCount);
  UpdateHeader;
end;
{--------}

procedure TaaRecordStream.Delete(aRecNum : integer);
var
  DelLink : longint;
  Offset  : longint;
begin
 {calculate the record offset}
  Offset := rsCalcRecordOffset(aRecNum);

  {check to see that the record is not already deleted}
  rsSeekStream(Offset);

  //do specific delete action
(*  rsReadStream(DelLink, SizeOfDelFlag);
  if (DelLink <> cActiveRecord) then
    raise Exception.Create('TaaRecordStream.Delete: record already deleted');

  {write the first deleted record number to the first 4 bytes of the
   record we're deleting}
  rsSeekStream(Offset);
  rsWriteStream(FHeader^.hr1stDelRec, SizeOfDelFlag);

  {update the deleted record chain to start at the record we're
   deleting}
  FHeader^.hr1stDelRec := aRecNum;
 *)

  {we have one less record}
//  dec(FHeader^.hrCount);

  {now update the header record}
  UpdateHeader;
end;
{--------}
procedure TaaRecordStream.Open;
begin
  {if we should create a new record stream, do so}
  if FCreateNew then begin
    Assert(RecordLength > 0,
           'the record length must be greater than zero');
    rsCreateNew;
  end
  {otherwise open the existing record stream}
  else
    rsOpenExisting;
end;
{--------}
function TaaRecordStream.Read(aRecNum : integer; var aRec) : boolean;
//  ActLen  : integer;
var
  Offset  : longint;
begin
  {calculate the record offset}
//  Offset := rsCalcRecordOffset(aRecNum);
  if (aRecNum < 0) or (aRecNum > RecordCount) then
    raise Exception.Create('record number is out of range');
  {second, calculate the offset}
if Not sequential then
begin
  Offset := HeaderLen +(aRecNum * (RecordLength));
  {get a temporary buffer in order to read the entire record}
    {seek and read the entire record}
 //  rsSeekStream(Offset);
  if Offset <> FStream.Seek(Offset, soFromBeginning) then
    raise Exception.Create('could not seek to required offset');
end;
//    rsReadStream(RTempRec^, RecordLength);
//   rsReadStream(aRec, RecordLength);
  if RecordLength <> FStream.Read(aRec, RecordLength) then
    raise Exception.Create('could not read required record');
   Result := true;
//    Move(RTempRec[0{SizeOfDelFlag}], aRec, RecordLength);
end;
{--------}
procedure TaaRecordStream.UpdateHeader;
begin

end;

procedure TaaRecordStream.Write(aRecNum : integer; var aRec);
var
  DelLink : longint;
  Offset  : longint;
begin
  {calculate the record offset}
  Offset := rsCalcRecordOffset(aRecNum);

  {check to see that the record is not deleted}
  rsSeekStream(Offset);
{  rsReadStream(DelLink, sizeof(DelLink));
  if (DelLink <> cActiveRecord) then
    raise Exception.Create('TaaRecordStream.Write: record is deleted and cannot be updated');
}
  {update the record}
  rsWriteStream(aRec, RecordLength);
end;

function TaaRecordStream.rsCalcRecordOffset(aRecNum : integer) : longint;
begin
  {first verify that the record number is in range}
  //allow aRecNum=RecordCount for appending
  if (aRecNum < 0) or (aRecNum > RecordCount) then
    raise Exception.Create('record number is out of range');
  {second, calculate the offset}
  Result := HeaderLen +(aRecNum * (RecordLength));
end;
{--------}

function TaaRecordStream.WriteHeader;
begin
end;


procedure TaaRecordStream.rsCreateNew;
begin
  WriteHeader;
  fHeaderLen:= GetHeaderLen;
//  FRecLen:= GetRecordLen;
  {now update the header record}
//  UpdateHeader;
  RecordCount:=0;
end;
{--------}


{--------}
procedure TaaRecordStream.rsReadStream(var aRec; aRecLen : integer);
var
  BytesRead : longint;
begin
  BytesRead := FStream.Read(aRec, aRecLen);
  if (BytesRead <> aRecLen) then
    raise Exception.Create('could not read required record');
end;
{--------}
procedure TaaRecordStream.rsSeekStream(aOffset : longint);
var
  Offset : longint;
begin
  Offset := FStream.Seek(aOffset, soFromBeginning);
  if (Offset <> aOffset) then
    raise Exception.Create('could not seek to required offset');
end;
{--------}

procedure TaaRecordStream.rsWriteStream(const aRec; aRecLen : integer);
var
  BytesWritten : longint;
begin
  BytesWritten := FStream.Write(aRec, aRecLen);
  if (BytesWritten <> aRecLen) then
    raise EInOutError.Create('could not write required record');
end;
{--------}


{====================================================================}

{====================================================================}
type
  TCacheFlag = (        {possible cache item attributes..}
    cfCacheRecUnused,   {..item is unused}
    cfCacheRecClean,    {..record read, but unchanged}
    cfCacheRecDirty,    {..record should be written}
    cfCacheRecDelete);  {..record should be deleted}
  PCacheItem = ^TCacheItem;
  TCacheItem = packed record
    ciRecNum : integer;
    ciRecord : PChar;
    ciLRU    : integer;
    ciFlag   : TCacheFlag;
  end;
{--------}
const
  cActiveRecord = -2;
  cUnusedRecord = -1;


constructor TaaRecordStreamCache.Create(aStream    : TStream;
                                        aCreateNew : boolean;
                                        aRecLen    : integer;
                                        aCacheSize : integer);
begin
  {create the ancestor}
  inherited Create(aStream, aCreateNew, aRecLen);

  {create the record cache}
  rscCreateCache(aCacheSize);
end;
{--------}
constructor TaaRecordStreamCache.CreateFile(const aFileName  : string;
                                                  aCreateNew : boolean;
                                                  aRecLen    : integer;
                                                  aCacheSize : integer);
begin
  {create the ancestor}
  inherited CreateFile(aFileName, aCreateNew, aRecLen);

  {create the record cache}
  rscCreateCache(aCacheSize);
end;
{--------}
destructor TaaRecordStreamCache.Destroy;
begin
  {if the cache exists, flush any remaining pages to the stream}
  if (FCache <> nil) then
    rscDestroyCache;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
function TaaRecordStreamCache.Add(const aRec) : integer;
var
  i : integer;
  Item       : PCacheItem;
  MinLRU     : integer;
  MinLRUItem : integer;
begin
  {we'll be looking for a deleted item so that we can reuse it, and if
   there are no deleted records, we'll reuse the least recently used
   item instead; initialize for the loop}
  MinLRU := MaxLongInt;
  MinLRUItem := 0;
  {find a deleted record in the cache}
  for i := 0 to pred(FCache.Count) do
  begin
    {get the item}
    Item := PCacheItem(FCache.List^[i]);
    {if the item is deleted...}
(*    if (Item^.ciFlag = cfCacheRecDelete) then
    begin
      {copy the record over}
      Move(aRec, Item^.ciRecord^, RecordLength);
      {mark the item}
      Item^.ciFlag := cfCacheRecDirty;
      Item^.ciLRU := rscGetNextLRU;
      {return the reused record number}
      Result := Item^.ciRecNum;
      {we're done}
      Exit;
    end;*)
     {check for the least recently used item}
    if (Item^.ciLRU < MinLRU) then begin
      MinLRU := Item^.ciLRU;
      MinLRUItem := i;
    end;
  end;
  {if we reach this point, there were no deleted items in the cache;
   however, during the search we made a note of the least recently
   used item so we'll reuse it}
  {get the least recently item}
  Item := PCacheItem(FCache.List^[MinLRUItem]);
  {if it indicates that the record must be written, do so}
  if (Item^.ciFlag = cfCacheRecDirty) then
    inherited Write(Item^.ciRecNum, Item^.ciRecord^);
  {add the new record to the stream}
  Result := inherited Add(aRec);
  {set the cache item to our record}
  Item^.ciRecNum := Result;
  Move(aRec, Item^.ciRecord^, RecordLength);
  Item^.ciFlag := cfCacheRecClean;
  Item^.ciLRU := rscGetNextLRU;
end;
{--------}
procedure TaaRecordStreamCache.Delete(aRecNum : integer);
var
  i : integer;
  Item : PCacheItem;
begin
  {find the record number in the cache}
  for i := 0 to pred(FCache.Count) do
  begin
    {get the item}
    Item := PCacheItem(FCache.List^[i]);
    {if the item is the record we want...}
    if (Item^.ciRecNum = aRecNum) then
    begin
      {validation check}
      Assert((Item^.ciFlag <> cfCacheRecUnused) and
             (Item^.ciFlag <> cfCacheRecDelete),
             'found item with invalid flag in cache');
      {mark the item as to be deleted}
      Item^.ciFlag := cfCacheRecDelete;
      {we're done}
      Exit;
    end;
  end;
  {if we reach this point, the item is not present in the cache;
   so instead we'll delete it directly from the stream}
  inherited Delete(aRecNum);
end;
{--------}
function TaaRecordStreamCache.Read(aRecNum : integer; var aRec) : boolean;
var
  i : integer;
  Item       : PCacheItem;
  MinLRU     : integer;
  MinLRUItem : integer;
begin
  {we're going to search for the least recently used item in the cache
   just in case; initialize the variables for that}
  MinLRU := MaxLongInt;
  MinLRUItem := 0;
  {find the record number in the cache}
  for i := 0 to pred(FCache.Count) do
  begin
    {get the item}
    Item := PCacheItem(FCache.List^[i]);
    {if the item is the record we want...}
    if (Item^.ciRecNum = aRecNum) then
    begin
      {validation check}
      Assert(Item^.ciFlag <> cfCacheRecUnused,
             'found item with invalid flag in cache');
      {return whether the record is active or not}
      Result := (Item^.ciFlag <> cfCacheRecDelete);
      {get the record and update the cache, if required}
      if Result then begin
        Move(Item^.ciRecord^, aRec, RecordLength);
        Item^.ciLRU := rscGetNextLRU;
      end;
      {we're done}
      Exit;
    end;
    {check for the least recently used item}
    if (Item^.ciLRU < MinLRU) then begin
      MinLRU := Item^.ciLRU;
      MinLRUItem := i;
    end;
  end;
  {if we reach this point, the item is not present in the cache;
   however, during the search we made a note of the least recently
   used item so we'll reuse it}
  {get the least recently item}
  Item := PCacheItem(FCache.List^[MinLRUItem]);
  {if it indicates that the record must be written or deleted, do so}
  if (Item^.ciFlag = cfCacheRecDirty) then
    inherited Write(Item^.ciRecNum, Item^.ciRecord^)
  {if the item indicates that the record needs to be deleted, do it}
  else if (Item^.ciFlag = cfCacheRecDelete) then
    inherited Delete(Item^.ciRecNum);
  {read the record from the stream}
  Result := inherited Read(aRecNum, aRec);
  {if the record was read (ie, wasn't deleted) set the cache item to
   our record}
  if Result then begin
    Item^.ciRecNum := aRecNum;
    Move(aRec, Item^.ciRecord^, RecordLength);
    Item^.ciFlag := cfCacheRecClean;
    Item^.ciLRU := rscGetNextLRU;
  end
  {otherwise, set the cache item to unused}
  else begin
    Item^.ciRecNum := cUnusedRecord;
    Item^.ciFlag := cfCacheRecUnused;
    Item^.ciLRU := 0;
  end;
end;
{--------}
procedure TaaRecordStreamCache.rscCreateCache(aCacheSize : integer);
var
  i : integer;
  Item : PCacheItem;
begin
  {the cache is a list of simple records, each record having a record
   number, a pointer to the record, and a flag detailing information
   about the record (unused, clean, dirty, to be deleted)}
  Assert(aCacheSize > 0,
         'TaaRecordStreamCache.rscCreateCache: the cache size should be greater than zero');
  {create the cache}
  FCache := TList.Create;
  {fill the cache with unused items}
  for i := 0 to pred(aCacheSize) do begin
    New(Item);
    Item^.ciRecNum := cUnusedRecord;
    GetMem(Item^.ciRecord, RecordLength);
    Item^.ciFlag := cfCacheRecUnused;
    Item^.ciLRU := 0;
    FCache.Add(Item);
  end;
end;
{--------}
procedure TaaRecordStreamCache.rscDestroyCache;
var
  i : integer;
  Item : PCacheItem;
begin
  {for each item in the cache...}
  for i := 0 to pred(FCache.Count) do begin
    {get the item}
    Item := PCacheItem(FCache.List^[i]);
    {if the item is dirty, write the data to the stream}
    if (Item^.ciFlag = cfCacheRecDirty) then
      inherited Write(Item^.ciRecNum, Item^.ciRecord^)
    {if the item indicates that the record needs to be deleted, do it}
    else if (Item^.ciFlag = cfCacheRecDelete) then
      inherited Delete(Item^.ciRecNum);
    {free the item}
    FreeMem(Item^.ciRecord, RecordLength);
    Dispose(Item);
  end;
  {now all the data has been flushed and all cache items deallocated,
   free the cache}
  FCache.Free;
end;
{--------}
function TaaRecordStreamCache.rscGetNextLRU : integer;
var
  i : integer;
  Item : PCacheItem;
begin
  {if the maximum LRU value has been reached, reset all LRU values in
   the cache and then set the overall LRU to zero}
  if (FLRU = MaxLongInt) then begin
    for i := 0 to pred(FCache.Count) do begin
      Item := PCacheItem(FCache.List^[i]);
      Item^.ciLRU := 0;
    end;
    FLRU := 0;
  end;
  {return the next LRU}
  inc(FLRU);
  Result := FLRU;
end;
{--------}
procedure TaaRecordStreamCache.Write(aRecNum : integer; var aRec);
var
  i : integer;
  Item       : PCacheItem;
  MinLRU     : integer;
  MinLRUItem : integer;
begin
  {we're going to search for the least recently used item in the cache
   just in case; initialize the variables for that}
  MinLRU := MaxLongInt;
  MinLRUItem := 0;
  {find the record number in the cache}
  for i := 0 to pred(FCache.Count) do
  begin
    {get the item}
    Item := PCacheItem(FCache.List^[i]);
    {if the item is the record we want...}
    if (Item^.ciRecNum = aRecNum) then
    begin
      {validation check}
      Assert((Item^.ciFlag <> cfCacheRecUnused) and
             (Item^.ciFlag <> cfCacheRecDelete),
             'TaaRecordStreamCache.Write: found item with invalid flag in cache');
      {copy the record over}
      Move(aRec, Item^.ciRecord^, RecordLength);
      {mark the item}
      Item^.ciFlag := cfCacheRecDirty;
      Item^.ciLRU := rscGetNextLRU;
      {we're done}
      Exit;
    end;
    {check for the least recently used item}
    if (Item^.ciLRU < MinLRU) then begin
      MinLRU := Item^.ciLRU;
      MinLRUItem := i;
    end;
  end;

  {if we reach this point, the item is not present in the cache;
   however, during the search we made a note of the least recently
   used item so we'll reuse it}

  {get the least recently item}
  Item := PCacheItem(FCache.List^[MinLRUItem]);

  {if it indicates that the record must be written or deleted, do so}
  if (Item^.ciFlag = cfCacheRecDirty) then
    inherited Write(Item^.ciRecNum, Item^.ciRecord^)
  {if the item indicates that the record needs to be deleted, do it}
  else if (Item^.ciFlag = cfCacheRecDelete) then
    inherited Delete(Item^.ciRecNum);
  {set the item to our record}
  Item^.ciRecNum := aRecNum;
  Move(aRec, Item^.ciRecord^, RecordLength);
  Item^.ciFlag := cfCacheRecDirty;
  Item^.ciLRU := rscGetNextLRU;
end;
{====================================================================}

{-----------------------------------------------------------------------------}
{                          TBufferedStream                                  }
{-----------------------------------------------------------------------------}

constructor TBufferedStream.Create(aStream : TStream);
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := 1024*128;
  GetMem(FBuffer, FBufSize);

  {save the stream}
  if (aStream = nil) then
   raise Exception.Create('Nil stream'); ;
  FStream := aStream;

  bsInitForNewStream;
end;

{-----------------------------------------------------------------------------}

constructor TBufferedStream.CreateEmpty;
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := 4096;
  GetMem(FBuffer, FBufSize);

  bsInitForNewStream
end;

{-----------------------------------------------------------------------------}

destructor TBufferedStream.Destroy;
begin
  if (FBuffer <> nil) then begin
    if FDirty and (FStream <> nil) then
      bsWriteToStream;
    FreeMem(FBuffer, FBufSize);
  end;

  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure TBufferedStream.bsInitForNewStream;
begin
  if (FStream <> nil) then
    FSize := FStream.Size
  else
    FSize := 0;
  FBufCount := 0;
  FBufOfs := 0;
  FBufPos := 0;
  FDirty := false;
end;

{-----------------------------------------------------------------------------}

function TBufferedStream.bsReadChar(var aCh : AnsiChar) : boolean;
begin
  {is there anything to read?}
  if (FSize = (FBufOfs + FBufPos)) then begin
    Result := false;
    Exit;
  end;
  {if we get here, we'll definitely read a character}
  Result := true;
  {make sure that the buffer has some data in it}
  if (FBufCount = 0) then
    bsReadFromStream
  else if (FBufPos = FBufCount) then begin
    if FDirty then
      bsWriteToStream;
    FBufPos := 0;
    inc(FBufOfs, FBufSize);
    bsReadFromStream;
  end;
  {get the next character}
  aCh := AnsiChar(FBuffer[FBufPos]);
  inc(FBufPos);
end;

{-----------------------------------------------------------------------------}

procedure TBufferedStream.bsReadFromStream;
var
  NewPos : longint;
begin
  {assumptions: FBufOfs is where to read the buffer
                FBufSize is the number of bytes to read
                FBufCount will be the number of bytes read}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs) then
       raise Exception.Create('No Seek For Read');
  FBufCount := FStream.Read(FBuffer^, FBufSize);
end;

{-----------------------------------------------------------------------------}

procedure TBufferedStream.bsSetStream(aValue : TStream);
begin
  if (aValue <> FStream) then begin
    {if the buffer is dirty, flush it to the current stream}
    if FDirty and (FStream <> nil) then
      bsWriteToStream;
    {remember the stream and initialize all fields}
    FStream := aValue;
    bsInitForNewStream;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TBufferedStream.bsWriteToStream;
var
  NewPos       : longint;
  BytesWritten : longint;
begin
  {assumptions: FDirty is true
                FBufOfs is where to write the buffer
                FBufCount is the number of bytes to write
                FDirty will be set false afterwards}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs) then
       raise Exception.Create('No Seek For Write');
  BytesWritten := FStream.Write(FBuffer^, FBufCount);
  if (BytesWritten <> FBufCount) then
       raise Exception.Create('Cannot Write');
  FDirty := false;
end;

{-----------------------------------------------------------------------------}

function TBufferedStream.Read(var Buffer; Count : longint) : longint;
var
  BytesToGo   : longint;
  BytesToRead : longint;
//  BufAsBytes  : TByteArray absolute Buffer;                          {!!.02}
//  DestPos     : longint;                                             {!!.02}
  BufAsBytes  : PChar;                                                 {!!.02}
begin
  BufAsBytes := @Buffer;                                               {!!.02}

  if (FStream = nil) then
      raise Exception.Create('stscNilStream');
  {calculate the number of bytes we could read if possible}
  BytesToGo := Min(Count, FSize - (FBufOfs + FBufPos));
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to read some data after all?}
  if (BytesToGo > 0) then begin
    {make sure that the buffer has some data in it}
    if (FBufCount = 0) then
      bsReadFromStream;
    {read as much as we can from the current buffer}
    BytesToRead := Min(BytesToGo, FBufCount - FBufPos);
    {transfer that number of bytes}
//    Move(FBuffer[FBufPos], BufAsBytes[0], BytesToRead);              {!!.02}
    Move(FBuffer[FBufPos], BufAsBytes^, BytesToRead);                  {!!.02}
    {update our counters}
    inc(FBufPos, BytesToRead);
    dec(BytesToGo, BytesToRead);
    {if we have more bytes to read then we've reached the end of the
     buffer and so we need to read another, and another, etc}
//    DestPos := 0;                                                    {!!.02}
    while BytesToGo > 0 do begin
      {if the current buffer is dirty, write it out}
      if FDirty then
        bsWriteToStream;
      {position and read the next buffer}
      FBufPos := 0;
      inc(FBufOfs, FBufSize);
      bsReadFromStream;
      {calculate the new destination position, and the number of bytes
       to read from this buffer}
//      inc(DestPos, BytesToRead);                                     {!!.02}
      Inc(BufAsBytes, BytesToRead);                                    {!!.02}
      BytesToRead := Min(BytesToGo, FBufCount - FBufPos);
      {transfer that number of bytes}
//      Move(FBuffer[FBufPos], BufAsBytes[DestPos], BytesToRead);      {!!.02}
      Move(FBuffer[FBufPos], BufAsBytes^, BytesToRead);                {!!.02}

      {update our counters}
      inc(FBufPos, BytesToRead);
      dec(BytesToGo, BytesToRead);
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TBufferedStream.Seek(Offset : longint; Origin : word) : longint;
var
  NewPos : longint;
  NewOfs : longint;
begin
  if (FStream = nil) then
      raise Exception.Create('stscNilStream');
  {optimization: to help code that just wants the current stream
   position (ie, reading the Position property), check for this as a
   special case}
  if (Offset = 0) and (Origin = soFromCurrent) then begin
    Result := FBufOfs + FBufPos;
    Exit;
  end;
  {calculate the desired position}
  case Origin of
    soFromBeginning : NewPos := Offset;
    soFromCurrent   : NewPos := (FBufOfs + FBufPos) + Offset;
    soFromEnd       : NewPos := FSize + Offset;
  else
          raise Exception.Create('bad origin');
    NewPos := 0; {to fool the compiler's warning--we never get here}
  end;
  {force the new position to be valid}
  if (NewPos < 0) then
    NewPos := 0
  else if (NewPos > FSize) then
    NewPos := FSize;
  {calculate the offset for the buffer}
  NewOfs := (NewPos div FBufSize) * FBufSize;
  {if the offset differs, we have to move the buffer window}
  if (NewOfs <> FBufOfs) then begin
    {check to see whether we have to write the current buffer to the
     original stream first}
    if FDirty then
      bsWriteToStream;
    {mark the buffer as empty}
    FBufOfs := NewOfs;
    FBufCount := 0;
  end;
  {set the position within the buffer}
  FBufPos := NewPos - FBufOfs;
  Result := NewPos;
end;

{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}

function TBufferedStream.Write(const Buffer; Count : longint) : longint;
var
  BytesToGo   : longint;
  BytesToWrite: longint;
//  BufAsBytes  : TByteArray absolute Buffer;                          {!!.02}
//  DestPos     : longint;                                             {!!.02}
  BufAsBytes  : PChar;                                                 {!!.02}
begin
  BufAsBytes := @Buffer;                                               {!!.02}

  if (FStream = nil) then
          raise Exception.Create('stscNilStream');
  {calculate the number of bytes we should be able to write}
  BytesToGo := Count;
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to write some data?}
  if (BytesToGo > 0) then begin
    {try and make sure that the buffer has some data in it}
    if (FBufCount = 0) then
      bsReadFromStream;
    {write as much as we can to the current buffer}
    BytesToWrite := Min(BytesToGo, FBufSize - FBufPos);
    {transfer that number of bytes}
//    Move(BufAsBytes[0], FBuffer[FBufPos], BytesToWrite);             {!!.02}
    Move(BufAsBytes^, FBuffer[FBufPos], BytesToWrite);                 {!!.02}
    FDirty := true;
    {update our counters}
    inc(FBufPos, BytesToWrite);
    if (FBufCount < FBufPos) then begin
      FBufCount := FBufPos;
      FSize := FBufOfs + FBufPos;
    end;
    dec(BytesToGo, BytesToWrite);
    {if we have more bytes to write then we've reached the end of the
     buffer and so we need to write another, and another, etc}
//    DestPos := 0;                                                    {!!.02}
    while BytesToGo > 0 do begin
      {as the current buffer is dirty, write it out}
      bsWriteToStream;
      {position and read the next buffer, if required}
      FBufPos := 0;
      inc(FBufOfs, FBufSize);
      if (FBufOfs < FSize) then
        bsReadFromStream
      else
        FBufCount := 0;
      {calculate the new destination position, and the number of bytes
       to write to this buffer}
//      inc(DestPos, BytesToWrite);                                    {!!.02}
      Inc(BufAsBytes, BytesToWrite);                                   {!!.02}
      BytesToWrite := Min(BytesToGo, FBufSize - FBufPos);
      {transfer that number of bytes}
//      Move(BufAsBytes[DestPos], FBuffer[0], BytesToWrite);           {!!.02}
      Move(BufAsBytes^, FBuffer[0], BytesToWrite);                     {!!.02}
      FDirty := true;
      {update our counters}
      inc(FBufPos, BytesToWrite);
      if (FBufCount < FBufPos) then begin
        FBufCount := FBufPos;
        FSize := FBufOfs + FBufPos;
      end;
      dec(BytesToGo, BytesToWrite);
    end;
  end;
end;


procedure TaaRecordStream.Setsequential(const Value: boolean);
begin
  Fsequential := Value;
end;

end.
