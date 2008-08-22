{

  Buffered TFileStream and TStringStream "decorator" classes.

  Danny Heijl, Dec 2001 (danny.heijl@pandora.be)

  The implementation is based on widely spread ideas and techniques,
  so consider this public domain to be used as you see fit.

  Using the buffered string stream in multithreaded apps is especially
  useful as it suppresses all those calls to the memory allocator you
  would get when repeatedly using TStringStream.Write or string concatenation
  to the same string, avoiding a big performance bottleneck.
  WebBroker server applications building their Response content on a
  multiprocessor machine are obvious candidates.

}

unit BufferedStreams;

{$ALIGN ON}
{$OPTIMIZATION ON}


interface

uses SysUtils, Classes, windows,AnsDataTypes;

{ the default Buffer size used when you don't specify one in the constructor }
const
  IOBufSize = $F000;

{
  BufferedStringStream "filter" class,
  follows the "decorator" pattern for TStringStreams.
  Implements buffered writing to the (owned) TStringStream.
  Reading and seeking is pass-through.

  Expect big performance gains when tacking vary many very small
  pieces on to the end of a string with TBufferedStringStream.Write
  or WriteString when compared to repeatedly writing s := s + ...  or
  TStringStream.Write.
  At least 2 to thousands of times faster (using the standard Delphi memory
  allocator), 2 being the optimal case where Delphi can expand the string
  "in place" without calling ReallocMem, but this only happens in the
  rare case where no other memory allocations are taking place while you
  are expanding the string or stringstream. If any memory allocations
  prevent "in place" string expansion, the repeated ReallocMem calls will
  kill your performance, while TBufferedStringStream will only call ReallocMem
  when the "write buffer" if full.
  The performance of TBufferedStringStream is predictable, while repeated string
  or stringstream expansion is not, as it depends heavily on in place
  reallocation being possible, and this is seldom the case.
  In one test (tacking on 10000 small strings to a string),
  TbufferedStringStream remained constant at 13 to 15 msec, while TStringStream
  randomly took between 35 and 3500 msec, obviously depending on patterns
  in memory (re)allocations.

  One Constructor takes an existing TStringStream as argument.
  A second constructor takes a string and creates a TStringStream
  internally (it "owns" this Stream).
  Both constructors take a buffer size as an optional second argument.

  Behaves as a "normal" string stream, but all Write and
  WriteString calls are buffered internally. Reading, Seeking etc...
  is entirely "Pass-Through" as a string stream is in memory anyway.

  Flushing to the "owned" string stream is automatic unless you decide
  to access the owned stream directly before you have destroyed the filter.
  In this case call TBufferedStringStream.Flush before you do this.

  Take care to always destroy the BufferedStringStream before you destroy
  a TStringStream that you passed as the stream argument in the
  first form of the constructor.

}


type
  EBufferedStringStream = class(Exception);

type
  TBufferedStringStream = class(TStringStream)
  private
    FStream: TStringStream;
    FBuffer: PChar;
    FBufp: PChar;
    FOwnStream: boolean;
    FBufSize: integer;
    function AllocBuffer(BufSize: integer): PChar;
    function GetPosition: longint;
    procedure SetPosition(const Value: longint);
    function GetDataString: string;
    function GetSize: longint; 
  protected
    procedure SetSize(NewSize: longint); override;
    procedure FlushBuffer; virtual;
  public
    constructor Create(const Astring: string); reintroduce; overload;
    constructor Create(const AString: string; BufSize: integer); overload;
    constructor Create(AStream: TStringStream; BufSize: integer = IOBufSize); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    function ReadString(Count: longint): string;
    procedure WriteString(const AString: string);
    procedure PutChar(Ch: char);
    procedure Flush;
    property Size: longint read GetSize write SetSize;
    property Position: longint read GetPosition write SetPosition;
    property DataString: string read GetDataString;
  end;

{
  BufferedFileStream "filter" class,
  follows the "decorator" pattern for TFileStreams.
  Implements buffered writing to the (owned) TFileStream.
  Reading is entirely "Pass-through" for now.

  TBufferedFileStream.Write is about 5 times faster when writing very many
  very small strings to a file when compared to TFileStream.Write.
  The performance gain is caused by reducing the number of system calls,
  so this could be especially significant for server apps.

  One Constructor takes an existing FileStream as argument.
  A second constructor takes a filename and filemode as arguments and
  creates a TFileStream internally (it "owns" this stream).
  Both constructors take a buffer size as an optional second argument.
  Behaves as a "normal" File stream, but all Write and
  WriteString calls are buffered internally.
  Reading, Seeking etc... is entirely "Pass-Through".

  Flushing to the "owned" File Stream is automatic unless you decide
  to access the owned stream directly before you have destroyed the filter.
  In this case call BufferedFileStream.Flush before you do this.

  Take care to always destroy the BufferedFileStream before you destroy
  the owned FileStream if you passed a stream in the constructor.

}

type
  EBufferedFileStream = class(Exception);

type
  TBufferedFileStream = class(TFileStream)
  private
    FStream: TFileStream;
    FBuffer: PChar;
    FBufp: PChar;
    FOwnStream: boolean;
    FBufSize: integer;
    function AllocBuffer(BufSize: integer): PChar;
    function GetPosition: longint;
    procedure SetPosition(const Value: longint);
    function GetSize: longint;
  protected
    procedure SetSize(NewSize: longint); override;
    procedure FlushBuffer; virtual;
  public
    constructor Create(const FileName: string; Mode: word); reintroduce; overload;
    constructor CreateBuffered(const FileName: string; Mode: word; BufSize: integer);
    constructor Create(AStream: TFileStream; BufSize: integer); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: word): longint; override;
    procedure WriteString(const AString: string);
    procedure PutChar(Ch: char);
    procedure Flush;
    property Size: longint read GetSize write SetSize;
    property Position: longint read GetPosition write SetPosition;
  end;

{
  Optimized byte move for moving (small) non-aligned strings by Peter Morris
  see http://www.borland.com/delphi/news/delphi_developer/optimizing.html
  Don't use it for overlapping moves !
}
procedure ByteMove(const Source; var Dest; Count: integer);

implementation

procedure ByteMove(const Source; var Dest; Count: integer);
asm
                    // ECX = Count
                    // EAX = Const Source
                    // EDX = Var Dest
                    // If there are no bytes to copy, just quit
                    // altogether; there's no point pushing registers.
  Cmp   ECX,0
  Je    @JustQuit
                    // Preserve the critical Delphi registers.
  Push  ESI
  Push  EDI
                    // Move Source into ESI (SOURCE register).
                    // Move Dest into EDI (DEST register).
                    // This might not actually be necessary, as I'm not using MOVsb etc.
                    // I might be able to just use EAX and EDX;
                    // there could be a penalty for not using ESI, EDI, but I doubt it.
                    // This is another thing worth trying!
  Mov   ESI, EAX
  Mov   EDI, EDX
                    // The following loop is the same as repNZ MovSB, but oddly quicker!
@Loop:
  Mov   AL, [ESI]   // get a source byte
  Inc   ESI         // bump source address
  Mov   [EDI], AL   // Put it into the destination
  Inc   EDI         // bump destination address
  Dec   ECX         // Dec ECX to note how many we have left to copy
  Jnz   @Loop       // If ECX <> 0, then loop.
                    // Pop the critical Delphi registers that we've altered.
  Pop   EDI
  Pop   ESI
@JustQuit:
end;


{ TBufferedStringStream }

{ allocate "slack" space at the end of the stream  so that }
{ we can tack on strings without reallocating                }

function TBufferedStringStream.AllocBuffer(BufSize: integer): PChar;
var
  iEOF: longint;
begin
  FBufSize := BufSize;
  FStream.Seek(0, soFromEnd);
  iEOF := FStream.Position;
  FStream.Size := FStream.Size + FBufSize;
  FBuffer := PChar(FStream.DataString) + iEOF;
  Result := FBuffer;
end;

{ "normal" constructor }
constructor TBufferedStringStream.Create(const AString: string);
begin
  Create(Astring, IOBufSize);
end;

{ Constructor 1: "decorate" an existing TStringStream }
constructor TBufferedStringStream.Create(AStream: TStringStream; BufSize: integer);
begin
  if (AStream = nil) or (not (AStream is TStringStream)) then
    raise EBufferedStringStream.Create('TBufferedStringStream.Create: invalid StringStream');
  FStream := AStream;
  FBufp := AllocBuffer(BufSize);
end;

{ Constructor 2: create an internal TStringStream to "decorate" }
constructor TBufferedStringStream.Create(const AString: string; BufSize: integer);
begin
  FStream := TSTringStream.Create(AString);
  FOwnStream := True;
  FBufp := AllocBuffer(BufSize);
end;

destructor TBufferedStringStream.Destroy;
begin
  FlushBuffer;
  if FOwnStream then FStream.Free;
  inherited;
end;

procedure TBufferedStringStream.Flush;
begin
  FlushBuffer;
end;

{ FlusfBuffer is really a truncating operation, as there is no real "buffer".  }
{ It simply gets rid of any unused "slack" space reserved previously by        }
{ AllocBuffer and positions at the end of the string.                          }

procedure TBufferedStringStream.FlushBuffer;
var
  BytesInBuf: longint;
begin
  BytesInBuf := FBufp - FBuffer;
  if BytesInBuf > 0 then 
  begin
    FStream.Size := FStream.Size - (FBufSize - BytesInBuf);
    FStream.Seek(0, soFromEnd);
    FBufp := nil;
    FBuffer := nil;
  end;
end;

function TBufferedStringStream.GetDataString: string;
begin
  FlushBuffer;
  Result := FStream.DataString;
end;

function TBufferedStringStream.GetPosition: longint;
begin
  FlushBuffer;
  Result := FStream.Position;
end;

function TBufferedStringStream.GetSize: longint;
begin
  FlushBuffer;
  Result := FStream.Size;
end;

procedure TBufferedStringStream.PutChar(Ch: char);
var
  BytesLeft: integer;
begin
  if FBufp = nil then                 { no buffer yet, get one }
    FBufp := AllocBuffer(FBufSize);
  BytesLeft := FBufSize - (FBufp - FBuffer);
  if BytesLeft < 1 then 
  begin
    FlushBuffer;                         { flush the buffer now that it is full }
    FBufp := AllocBuffer(FBufSize);      { get a new write buffer               }
  end;
  FBufp^ := Ch;
  Inc(FBufp);
end;

function TBufferedStringStream.Read(var Buffer; Count: integer): longint;
begin
  FlushBuffer;
  Result := FStream.Read(Buffer, Count);
end;

function TBufferedStringStream.ReadString(Count: integer): string;
begin
  FlushBuffer;
  Result := FStream.ReadString(Count);
end;

function TBufferedStringStream.Seek(Offset: integer;
  Origin: word): longint;
begin
  FlushBuffer;
  Result := FStream.Seek(Offset, Origin);
end;


procedure TBufferedStringStream.SetPosition(const Value: longint);
begin
  FlushBuffer;
  FStream.Position := Value;
end;

procedure TBufferedStringStream.SetSize(NewSize: longint);
begin
  FlushBuffer;
  FStream.Size := NewSize;
  FBufp := nil;
  FBuffer := nil;
end;

function TBufferedStringStream.Write(const Buffer; Count: integer): longint;
var
  BytesLeft: integer;
  pResidue: PChar;
  lResidue: integer;
begin
  Result := Count;
  if FBufp = nil then                 { no buffer yet, get one }
    FBufp := AllocBuffer(FBufSize);
  BytesLeft := FBufSize - (FBufp - FBuffer);
  if Count > 0 then 
  begin
    if Count < BytesLeft then 
    begin   { just a small write, it goes into the buffer }
      ByteMove(Buffer, FBufp^, Count);
      Inc(FBufp, Count);
      exit;
    end;
    if Count >= FBufSize then 
    begin    { no point in buffering writes larger than FBufSize  }
      FlushBuffer;                             { get rid of any slack          }
      Result := FStream.Write(Buffer, Count);  { do a "pass-through" write     }
      FBufp := AllocBuffer(FBufSize);          { and get a new write buffer    }
      exit;
    end;
    if Count >= Bytesleft then 
    begin       { always try to write full buffers     }
      ByteMove(Buffer, FBufp^, Bytesleft); { in an attempt to keep memory aligned }
      Inc(FBufp, BytesLeft);
      FlushBuffer;                         { flush the buffer now that it is full }
      FBufp := AllocBuffer(FBufSize);      { get a new write buffer               }
      lResidue := Count - Bytesleft;       { and dump what's left in the buffer   }
      if lResidue > 0 then 
      begin
        pResidue := @Buffer;
        Inc(pResidue, BytesLeft);
        ByteMove(pResidue^, FBufp^, lResidue);
        Inc(FBufp, lResidue);
      end;
    end;
  end;
end;

procedure TBufferedStringStream.WriteString(const AString: string);
begin
  Write(Pointer(Astring)^, Length(AString));
end;


{ TBufferedFileStream }


function TBufferedFileStream.AllocBuffer(BufSize: integer): PChar;
begin
  FBufSize := BufSize;
  try
    GetMem(FBuffer, FBufsize);
    Result := FBuffer;
  except
    raise EBufferedFileStream.Create('TBufferedFileStream.Create: AllocBuffer out of memory');
  end;
end;

constructor TBufferedFileStream.Create(const FileName: string; Mode: word);
begin
  CreateBuffered(FileName, Mode, integer(IOBufSize));
end;

constructor TBufferedFileStream.Create(AStream: TFileStream;
  BufSize: integer);
begin
  if (AStream = nil) or (not (AStream is TFileStream)) then
    raise EBufferedFileStream.Create('TBufferedFileStream.Create: need a valid TFileStream');
  FStream := AStream;
  FStream.Seek(0, soFromEnd);
  FBufp := AllocBuffer(BufSize);
end;


constructor TBufferedFileStream.CreateBuffered(const FileName: string; Mode: word;
  BufSize: integer);
begin
  FStream := TFileStream.Create(FileName, Mode);
  FStream.Seek(0, soFromEnd);
  FOwnStream := True;
  FBufp := AllocBuffer(BufSize);
end;

destructor TBufferedFileStream.Destroy;
begin
  FlushBuffer;
  if FOwnStream then FStream.Free;
  Freemem(FBuffer);
  inherited;
end;

procedure TBufferedFileStream.Flush;
begin
  FlushBuffer;
end;

procedure TBufferedFileStream.FlushBuffer;
var
  BytesInBuf: longint;
  E : EInOutError;
begin
  BytesInBuf := FBufp - FBuffer;
  if BytesInBuf > 0 then
  begin
   if FStream.Write(FBuffer^, BytesInBuf) = 0 then
   begin
     E := EInOutError.Create('');
     E.ErrorCode:=GetLastError;
     raise E;
   end;
    FBufp := FBuffer;
  end;
end;


function TBufferedFileStream.GetPosition: longint;
begin
  FlushBuffer;
  Result := FStream.Position;
end;

function TBufferedFileStream.GetSize: longint;
begin
  FlushBuffer;
  Result := FStream.Size;
end;

procedure TBufferedFileStream.PutChar(Ch: char);
var
  BytesLeft: integer;
begin
  BytesLeft := FBufSize - (FBufp - FBuffer);
  if Bytesleft < 1 then
    FlushBuffer;
  FBufp^ := Ch;
  Inc(FBufp);
end;

function TBufferedFileStream.Read(var Buffer; Count: integer): longint;
begin
  FlushBuffer;
  Result := FStream.Read(Buffer, Count);
end;


function TBufferedFileStream.Seek(Offset: integer;
  Origin: word): longint;
begin
  FlushBuffer;
  Result := FStream.Seek(Offset, Origin);
end;


procedure TBufferedFileStream.SetPosition(const Value: longint);
begin
  FlushBuffer;
  FStream.Position := Value;
end;

procedure TBufferedFileStream.SetSize(NewSize: longint);
begin
  FlushBuffer;
  FStream.Size := NewSize;
end;

function TBufferedFileStream.Write(const Buffer; Count: integer): longint;
var
  p: PChar;
  BytesLeft: integer;
begin
  Result := Count;
  p := @Buffer;
  while Count > 0 do 
  begin
    BytesLeft := FBufSize - (FBufp - FBuffer);
    if Count >= Bytesleft then 
    begin
      ByteMove(p^, FBufp^, Bytesleft);    { fill the buffer }
      Inc(FBufp, BytesLeft);
      Inc(p, BytesLeft);
      Dec(Count, Bytesleft);
      FlushBuffer;                         { flush the buffer now that it is full }
    end 
    else 
    begin                         { just a small write, it goes into the buffer }
      ByteMove(p^, FBufp^, Count);
      Inc(FBufp, Count);
      break;
    end;
  end;
end;

procedure TBufferedFileStream.WriteString(const AString: string);
begin
  Write(Pointer(Astring)^, Length(AString));
end;

end.
