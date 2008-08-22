unit AAChrStm;

interface

uses
  SysUtils,Classes;

type
  TaaInCharStream = class(TStream)
    private
      FBufEnd : integer;
      FBuffer : PByteArray;
      FBufPos : integer;
      FStream : TStream;
      FPutBackBuf : array [0..1] of char;
      FPutBackInx : integer;
      function GetAbsCharPosition: integer;
    protected
      procedure icsGetBuffer;
      function InternalGetChar: char;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;
      function Read(var Buffer; Count : longint) : longint; override;
      function Write(const Buffer; Count : longint) : longint; override;
      function Seek(Offset : longint; Origin : word) : longint; override;
      function GetChar : char;
      procedure PutBackChar(aCh : char);
      property AbsCharPosition:integer read  GetAbsCharPosition;
  end;

  TaaEndOfLine = (eolCRLF, eolLF);

  TaaOutCharStream = class(TStream)
    private
      FBuffer : PByteArray;
      FBufPos : integer;
      FEOL    : TaaEndOfLine;
      FStream : TStream;
    protected
      procedure ocsFlush;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;
      function Read(var Buffer; Count : longint) : longint; override;
      function Write(const Buffer; Count : longint) : longint; override;
      function Seek(Offset : longint; Origin : word) : longint; override;
      procedure PutChar(aCh : char);
      property EndOfLine : TaaEndOfLine read FEOL write FEOL;
  end;

implementation

const
  BufSize = 8192;
  CR      = #13;
  LF      = #10;

{===TaaInCharStream==================================================}
constructor TaaInCharStream.Create(aStream : TStream);
begin
  {create the ancestor}
  inherited Create;
  {save the stream}
  FStream := aStream;
  {create the buffer}
  GetMem(FBuffer, BufSize);
//  fLineNumber:=1;
 {FBufPos := 0;}
 {FBufEnd := 0;}
end;
{--------}
destructor TaaInCharStream.Destroy;
begin
  if (FBuffer <> nil) then
    FreeMem(FBuffer, BufSize);
end;

function TaaInCharStream.GetChar : char;
var
 ch :char;
begin
   result:=InternalGetChar;
{   case result of
   #0:exit;
   CR:
   begin
    ch:=InternalGetChar;
    if ch =LF then
       inc(fLineNumber)
    else
       PutBackChar(ch);
   end;
  end;//case}
end;

{--------}
function TaaInCharStream.InternalGetChar : char;
begin
//  repeat
    {use putback chars if available}
    if (FPutBackInx <> 0) then begin
      dec(FPutBackInx);
      Result := FPutBackBuf[FPutBackInx];
    end
    {otherwise use the buffer}
    else begin
      {make sure the buffer has data}
      if (FBufPos = FBufEnd) then
        icsGetBuffer;
      {if there is no more data, return #0 to signal end of stream}
      if (FBufEnd = 0) then
        Result := #0
      {otherwise return the current character}
      else begin
        Result := char(FBuffer^[FBufPos]);
        Assert(Result <> #0,
               'TaaInCharStream.GetChar: input stream is not text, read null');
        inc(FBufPos);
      end;
   end;
//  until (Result <> CR);
end;
{--------}
procedure TaaInCharStream.icsGetBuffer;
begin
  FBufPos := 0;
  FBufEnd := FStream.Read(FBuffer^, BufSize);
end;
{--------}
procedure TaaInCharStream.PutBackChar(aCh : char);
begin
  Assert(FPutBackInx < 2,
         'TaaInCharStream.PutBackChar: put back buffer is full');
  FPutBackBuf[FPutBackInx] := aCh;
  inc(FPutBackInx);
end;
{--------}
function TaaInCharStream.Read(var Buffer; Count : longint) : longint;
var
  BytesToRead : longint;
  OutBuf       : PByteArray;
  OutBufPos    : integer;
begin
  {make sure the buffer has data}
  if (FBufPos = FBufEnd) then
    icsGetBuffer;
  {assume we read nothing}
  Result := 0;
  if (FBufEnd = 0) then
    Exit;
  {calculate the number of bytes to copy the first time}
  BytesToRead := FBufEnd - FBufPos;
  if (Count < BytesToRead) then
    BytesToRead := Count;
  {copy the calculated number of bytes}
  Move(FBuffer^[FBufPos], Buffer, BytesToRead);
  inc(FBufPos, BytesToRead);
  dec(Count, BytesToRead);
  inc(Result, BytesToRead);
  {if there are still bytes to copy, do so}
  if (Count <> 0) then begin
    {create indexable pointer to output buffer}
    OutBuf := PByteArray(@Buffer);
    OutBufPos := BytesToRead;
    {while there are bytes to copy...}
    while (Count <> 0) do begin
      {read from the underlying stream}
      icsGetBuffer;
      if (FBufEnd = 0) then
        Exit;
      {calculate the number of bytes to copy this time}
      BytesToRead := FBufEnd;
      if (Count < BytesToRead) then
        BytesToRead := Count;
      {copy the calculated number of bytes}
      Move(FBuffer^[FBufPos], OutBuf^[OutBufPos], BytesToRead);
      inc(FBufPos, BytesToRead);
      inc(OutBufPos, BytesToRead);
      dec(Count, BytesToRead);
      inc(Result, BytesToRead);
    end;
  end;
end;
{--------}
function TaaInCharStream.Seek(Offset : longint; Origin : word) : longint;
begin
  Assert(false,
         'TaaOutCharStream.Seek: this class is read only, it cannot seek');
  Result := 0; {to satify the compiler}
end;
{--------}
function TaaInCharStream.Write(const Buffer; Count : longint) : longint;
begin
  Assert(false,
         'TaaInCharStream.Write: this class is read only, it cannot write');
  Result := 0; {to satisfy the compiler}
end;
{====================================================================}


{===TaaOutCharStream=================================================}
constructor TaaOutCharStream.Create(aStream : TStream);
begin
  {create the ancestor}
  inherited Create;
  {save the stream}
  FStream := aStream;
  {create the buffer}
  GetMem(FBuffer, BufSize);
 {FBufPos := 0;}
end;
{--------}
destructor TaaOutCharStream.Destroy;
begin
  {if there is a buffer and there is some data, flush it,
   then free the buffer}
  if (FBuffer <> nil) then begin
    ocsFlush;
    FreeMem(FBuffer, BufSize);
  end;
  {free the ancestor}
  inherited Destroy;
end;
{--------}
procedure TaaOutCharStream.ocsFlush;
begin
  {if there's data in the buffer, write it to the underlying stream}
  if (FBufPos <> 0) then begin
    FStream.WriteBuffer(FBuffer^, FBufPos);
    FBufPos := 0;
  end;
end;
{--------}
procedure TaaOutCharStream.PutChar(aCh : char);
begin
  if (FEOL = eolCRLF) and (aCh = LF) then begin
    {add a CR to the buffer}
    FBuffer^[FBufPos] := byte(CR);
    inc(FBufPos);
    {if the buffer is full, flush it to the underlying stream}
    if (FBufPos = BufSize) then
      ocsFlush;
  end;
  {add the character to the buffer}
  FBuffer^[FBufPos] := byte(aCh);
  inc(FBufPos);
  {if the buffer is full, flush it to the underlying stream}
  if (FBufPos = BufSize) then
    ocsFlush;
end;
{--------}
function TaaOutCharStream.Read(var Buffer; Count : longint) : longint;
begin
  Assert(false,
         'TaaOutCharStream.Read: this class is write only, it cannot read');
  Result := 0; {to satisfy the compiler}
end;
{--------}
function TaaOutCharStream.Seek(Offset : longint; Origin : word) : longint;
begin
  Assert(false,
         'TaaOutCharStream.Seek: this class is write only, it cannot seek');
  Result := 0; {to satisfy the compiler}
end;
{--------}
function TaaOutCharStream.Write(const Buffer; Count : longint) : longint;
var
  BytesToWrite : longint;
  InBuf        : PByteArray;
  InBufPos     : integer;
begin
  {assume we write the entire buffer}
  Result := Count;
  {calculate the number of bytes to copy the first time}
  BytesToWrite := BufSize - FBufPos;
  if (Count < BytesToWrite) then
    BytesToWrite := Count;
  {copy the calculated number of bytes}
  Move(Buffer, FBuffer^[FBufPos], BytesToWrite);
  inc(FBufPos, BytesToWrite);
  dec(Count, BytesToWrite);
  {if there are still bytes to copy, do so}
  if (Count <> 0) then begin
    {create indexable pointer to input buffer}
    InBuf := PByteArray(@Buffer);
    InBufPos := BytesToWrite;
    {while there are bytes to copy...}
    while (Count <> 0) do begin
      {flush the output buffer}
      ocsFlush;
     {calculate the number of bytes to copy this time}
      BytesToWrite := BufSize;
      if (Count < BytesToWrite) then
        BytesToWrite := Count;
      {copy the calculated number of bytes}
      Move(InBuf^[InBufPos], FBuffer^[FBufPos], BytesToWrite);
      inc(FBufPos, BytesToWrite);
      inc(InBufPos, BytesToWrite);
      dec(Count, BytesToWrite);
    end;
  end;
  {if the buffer is full, flush it to the underlying stream}
  if (FBufPos = BufSize) then
    ocsFlush;
end;
{====================================================================}



function TaaInCharStream.GetAbsCharPosition: integer;
begin

end;

end.
