unit Ustack;

interface
uses sysutils;

type
  PsllNode = ^TsllNode;
  TsllNode = packed record
    sllnNext : PsllNode;
    sllnData : pointer;
  end;

TaaStack = class
private
  FCount : integer;
  FHead  : PsllNode;
protected
public
  constructor Create;
  destructor Destroy; override;

  procedure Clear;
  function Examine : pointer;
  function IsEmpty : boolean;
  function Pop : pointer;
  procedure Push(aItem : pointer);

  property Count : integer read FCount;
end;

implementation



function snmAllocNode : PsllNode;
begin
  New(Result);
  Result^.sllnNext := nil;
  Result^.sllnData := nil;
end;



{===TaaStack=========================================================}
constructor TaaStack.Create;
begin
  inherited Create;
  {allocate a head node}
  FHead := snmAllocNode;
  FHead^.sllnNext := nil;
  FHead^.sllnData := nil;
end;

{--------}
destructor TaaStack.Destroy;
begin
  Clear;
  Dispose(FHead);
  inherited Destroy;
end;
{--------}
procedure TaaStack.Clear;
var
  Temp : PsllNode;
begin
  Temp := FHead^.sllnNext;
  while (Temp <> nil) do begin
    FHead^.sllnNext := Temp^.sllnNext;
    dispose(Temp);
    Temp := FHead^.sllnNext;
  end;
  FCount := 0;
end;
{--------}
function TaaStack.Examine : pointer;
begin
  if (Count = 0) then
    raise Exception.Create('TaaStack.Examine: stack is empty');
  Result := FHead^.sllnNext^.sllnData;
end;
{--------}
function TaaStack.IsEmpty : boolean;
begin
  Result := Count = 0;
end;
{--------}
function TaaStack.Pop : pointer;
var
  Temp : PsllNode;
begin
  if (Count = 0) then
    raise Exception.Create('TaaStack.Pop: stack is empty');
  Temp := FHead^.sllnNext;
  Result := Temp^.sllnData;
  FHead^.sllnNext := Temp^.sllnNext;
  dispose(Temp);
  dec(FCount);
end;
{--------}
procedure TaaStack.Push(aItem : pointer);
var
  Temp : PsllNode;
begin
  Temp := snmAllocNode;
  Temp^.sllnData := aItem;
  Temp^.sllnNext := FHead^.sllnNext;
  FHead^.sllnNext := Temp;
  inc(FCount);
end;
{====================================================================}


end.
 
