{*********************************************************}
{* HashLinP                                              *}
{* Copyright (c) Julian M Bucknall 1997                  *}
{* All rights reserved.                                  *}
{*********************************************************}
{* Dynamic Hash Table using Linear Probing               *}
{*********************************************************}

{Note: this unit is released as freeware. In other words, you are free
       to use this unit in your own applications, however I retain all
       copyright to the code. JMB}

unit AAHshLnP;

interface

{$IFOPT D+}
{$DEFINE DebugMode}
{$ENDIF}

type
  TaaHashFunction = function (const S : string) : longint;
    {-Function type for a hash function}
  TaaDeleteString = procedure (const S : string; aObject : pointer);
    {-Procedural type for a routine to free an associated object when
      a hash element (ie, string) is deleted from the table}

type
  TaaHashTableLinear = class
    {-a hash table that uses linear probing to resolve collisions}
    private
      htlArray     : pointer;
      htlCount     : integer;
      htlDeleteStr : TaaDeleteString;
      htlHashFunc  : TaaHashFunction;
      htlTableSize : integer;
      {$IFDEF DebugMode}
      htlDebugSeeks: integer;
      {$ENDIF}
    function htlGetString(aIndex: integer): string;

    protected
      procedure htlAlterTableSize(aNewTableSize : integer);
      procedure htlDoDeleteString(const aKey : string; aObject : pointer);
      function htlFindPrim(const aKey : string; var aIndex : integer) : boolean;
      function htlGetItem(aIndex : integer) : pointer;
      procedure htlGrowTable;
      function htlHash(const aKey : string) : integer;
      procedure htlShrinkTable;

    public
      constructor Create(aTableSize : integer;
                         aHashFunc  : TaaHashFunction);
        {-constructor to create a hash table that can hold aTableSize
          elements and that uses aHashFunc to hash strings}
      destructor Destroy; override;
        {-destructor to destroy the hash table}

      procedure Delete(const aKey : string);
        {-delete the element defined by aKey; an exception is raised
          if the string is not found}
      procedure Empty;
        {-delete all elements in the hash table and reset it to empty}
      function Find(const aKey : string; var aObject : pointer) : boolean;
        {-find the element defined by aKey; return true and the
          associated object if the string is found, otherwise false}
      procedure Insert(const aKey : string; aObject : pointer);
        {-insert a new element defined by aKey with its associated
          object aObject; an exception is raised if the string is
          already present}

      property Count : integer read htlCount;
        {-current number of elements in the hash table}
      property Items[aIndex : integer] : pointer   read htlGetItem; default;
      property Strings[aIndex : integer] : string read htlGetString;

        {-the items in the hash table}
      property TableSize : integer read htlTableSize;
        {-maximum number of elements in the hash table}
      property OnDeleteString : TaaDeleteString
         read htlDeleteStr write htlDeleteStr;
        {-routine to delete an associated object when the string is
          deleted}

      {$IFDEF DebugMode}
      procedure debugPrint(aFileName : string; aDetailed : boolean);
      {$ENDIF}
  end;

function AAELFHash(const S : string) : longint;

implementation

uses
  SysUtils;

{===Hash function====================================================}
function AAELFHash(const S : string) : longint;
var
  G : longint;
  i : integer;
begin
  Result := 0;
  for i := 1 to length(S) do begin
    Result := (Result shl 4) + ord(S[i]);
    G := Result and $F0000000;
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;
{====================================================================}

type
  THashElementState = (hesEmpty, hesDeleted, hesInUse);

  THashElement = packed record
    {$IFDEF Windows}
    heString : PString;
    {$ELSE}
    heString : string;
    {$ENDIF}
    heObject : pointer;
    heState  : THashElementState;
    heFiller : array [0..2] of byte;
  end;

  PHashElementArray = ^THashElementArray;
  THashElementArray =
     array [0..pred(MaxInt div sizeof(THashElement))] of THashElement;


{===Helper routines==================================================}
procedure RaiseException(const S : string);
begin
  raise Exception.Create(S);
end;
{--------}
function GetClosestPrime(N : integer) : integer;
{$I AAPrimes.inc}
const
  Forever = true;
var
  L, R, M : integer;
  RootN   : integer;
  IsPrime : boolean;
  DivisorIndex : integer;
begin
  {treat 2 as a special case}
  if (N = 2) then begin
    Result := N;
    Exit;
  end;
  {make the result equal to N, and if it's even, the next odd number}
  if Odd(N) then
    Result := N
  else
    Result := succ(N);
  {if the result is within our prime number table, use binary search
   to find the equal or next highest prime number}
  if (Result <= MaxPrime) then begin
    L := 0;
    R := pred(PrimeCount);
    while (L <= R) do begin
      M := (L + R) div 2;
      if (Result = Primes[M]) then
        Exit
      else if (Result < Primes[M]) then
        R := pred(M)
      else
        L := succ(M);
    end;
    Result := Primes[L];
    Exit;
  end;
  {the result is outside our prime number table range, use the
   standard method for testing primality (do any of the primes up to
   the root of the number divide it exactly?) and continue
   incrementing the result by 2 until it is prime}
  if (Result <= (MaxPrime * MaxPrime)) then begin
    while Forever do begin
      RootN := round(Sqrt(Result));
      DivisorIndex := 1; {ignore the prime number 2}
      IsPrime := true;
      while (DivisorIndex < PrimeCount) and (RootN > Primes[DivisorIndex]) do begin
        if ((Result div Primes[DivisorIndex]) * Primes[DivisorIndex] = Result) then begin
          IsPrime := false;
          Break;
        end;
        inc(DivisorIndex);
      end;
      if IsPrime then
        Exit;
      inc(Result, 2);
    end;
  end;
end;
{====================================================================}


{===TaaHashTableLinear===============================================}
constructor TaaHashTableLinear.Create(aTableSize : integer;
                                      aHashFunc  : TaaHashFunction);
begin
  inherited Create;
  aTableSize := GetClosestPrime(aTableSize);
  GetMem(htlArray, aTableSize * sizeof(THashElement));
  FillChar(htlArray^, aTableSize * sizeof(THashElement), 0);
  htlTableSize := aTableSize;
  htlHashFunc := aHashFunc;
end;
{--------}
destructor TaaHashTableLinear.Destroy;
begin
  if (htlArray <> nil) then begin
    Empty;
    FreeMem(htlArray, htlTableSize * sizeof(THashElement));
  end;
  inherited Destroy;
end;
{--------}
{$IFDEF DebugMode}
procedure TaaHashTableLinear.debugPrint(aFileName : string; aDetailed : boolean);
const
  StateStrs : array [THashElementState] of string[9] =
              ('<empty>  ', '<deleted>', '<in use> ');
var
  Inx        : integer;
  discardInx : integer;
  TotSeeks   : integer;
  F          : System.Text;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);
  try
    writeln(F, 'Hash Table (Linear Probe) Debug Print [', aFileName, ']');
    writeln(F, '-------------------------------------');
    if aDetailed then
      writeln(F);
    TotSeeks := 0;
    for Inx := 0 to pred(htlTableSize) do begin
      with PHashElementArray(htlArray)^[Inx] do begin
        if aDetailed then
          write(F, Inx:4, ': ', StateStrs[heState]);
        if (heState = hesInUse) then begin
          {$IFDEF Windows}
          htlFindPrim(heString^, discardInx);
          {$ELSE}
          htlFindPrim(heString, discardInx);
          {$ENDIF}
          inc(TotSeeks, htlDebugSeeks);
          if aDetailed then
            {$IFDEF Windows}
            writeln(F, '  ', heString^, '  (seekcount: ', htlDebugSeeks, ')')
            {$ELSE}
            writeln(F, '  ', heString, '  (seekcount: ', htlDebugSeeks, ')')
            {$ENDIF}
        end
        else
          if aDetailed then
            writeln(F);
      end;
    end;
    writeln(F);
    writeln(F, 'The table has ', htlCount,
               ' element(s) (of ', htlTableSize,
               ') and is ', (100.0 * htlCount / htlTableSize):0:2,
               '% full');
    if (htlCount > 0) then
      writeln(F, 'The average path length is ', (TotSeeks / htlCount):0:2, ' seeks');
  finally
    System.Close(F);
  end;
end;
{$ENDIF}
{--------}
procedure TaaHashTableLinear.Delete(const aKey : string);
var
  Inx : integer;
begin
  if not htlFindPrim(aKey, Inx) then
    RaiseException('TaaHashTableLinear.Delete: key not found');
  with PHashElementArray(htlArray)^[Inx] do begin
    {$IFDEF Windows}
    htlDoDeleteString(heString^, heObject);
    DisposeStr(heString);
    {$ELSE}
    htlDoDeleteString(heString, heObject);
    heString := '';
    {$ENDIF}
    heState := hesDeleted;
  end;
  dec(htlCount);
  {shrink the table if we're under 1/6 full}
  if ((htlCount * 6) < htlTableSize) then
    htlShrinkTable;
end;
{--------}
procedure TaaHashTableLinear.Empty;
var
  Inx : integer;
begin
  for Inx := 0 to pred(htlTableSize) do begin
    with PHashElementArray(htlArray)^[Inx] do begin
      if (heState = hesInUse) then begin
        {$IFDEF Windows}
        htlDoDeleteString(heString^, heObject);
        DisposeStr(heString);
        {$ELSE}
        htlDoDeleteString(heString, heObject);
        heString := '';
        {$ENDIF}
      end;
      heState := hesEmpty;
    end;
  end;
  htlCount := 0;
end;
{--------}
function TaaHashTableLinear.Find(const aKey : string; var aObject : pointer) : boolean;
var
  Inx : integer;
begin
  if htlFindPrim(aKey, Inx) then begin
    Result := true;
    aObject := PHashElementArray(htlArray)^[Inx].heObject;
  end
  else begin
    Result := false;
    aObject := nil;
  end;
end;
{--------}
procedure TaaHashTableLinear.htlAlterTableSize(aNewTableSize : integer);
var
  Inx          : integer;
  OldTableSize : integer;
  NewArray     : PHashElementArray;
  OldArray     : PHashElementArray;
begin
  {allocate a new array}
  GetMem(NewArray, aNewTableSize * sizeof(THashElement));
  FillChar(NewArray^, aNewTableSize * sizeof(THashElement), 0);
  {save the old array and element count and then set the object
   fields to the new values}
  OldArray := PHashElementArray(htlArray);
  OldTableSize := htlTableSize;
  htlArray := NewArray;
  htlTableSize := aNewTableSize;
  htlCount := 0;
  {read through the old array and transfer over the strings/objects}
  for Inx := 0 to pred(OldTableSize) do begin
    with OldArray^[Inx] do begin
      if (heState = hesInUse) then begin
        {$IFDEF Windows}
        Insert(heString^, heObject);
        DisposeStr(heString);
        {$ELSE}
        Insert(heString, heObject);
        heString := '';
        {$ENDIF}
      end;
    end;
  end;
  {finally free the old array}
  FreeMem(OldArray, OldTableSize * sizeof(THashElement));
end;
{--------}
procedure TaaHashTableLinear.htlDoDeleteString(const aKey : string; aObject : pointer);
begin
  if Assigned(htlDeleteStr) then
    htlDeleteStr(aKey, aObject);
end;
{--------}
function TaaHashTableLinear.htlFindPrim(const aKey : string; var aIndex : integer) : boolean;
var
  FirstDeleted : integer;
  KeyHash      : integer;
  FirstKeyHash : integer;
begin
  {assume we'll fail}
  Result := false;
  {we may need to make note of the first deleted element we find, so
   set the variable to some impossible value so that we know whether
   we found one yet}
  FirstDeleted := -1;
  {calculate the hash for the string, make a note of it so we can find
   out when (if) we wrap around the table completely}
  KeyHash := htlHash(aKey);
  FirstKeyHash := KeyHash;
  {$IFDEF DebugMode}
  htlDebugSeeks := 1;
  {$ENDIF}
  {do forever - we'll be exiting out of the loop when needed}
  while true do begin
    {with the current element...}
    with PHashElementArray(htlArray)^[KeyHash] do
      case heState of
        hesEmpty   : begin
                       {the state is 'empty', we must stop the linear
                        probe and return either this index or the
                        first deleted one we encountered}
                       if (FirstDeleted <> -1) then
                         aIndex := FirstDeleted
                       else
                         aIndex := KeyHash;
                       Exit;
                     end;
        hesDeleted : begin
                       {the state is 'deleted', we must make a note of
                        this index if it's the first one we found and
                        continue the linear probe}
                       if (FirstDeleted = -1) then
                         FirstDeleted := KeyHash;
                     end;
        hesInUse   : begin
                       {the state is 'in use', we check to see if it's
                        our string, if it is, exit returning true and
                        the index}
                       {$IFDEF Windows}
                       if (heString^ = aKey) then begin
                       {$ELSE}
                       if (heString = aKey) then begin
                       {$ENDIF}
                         aIndex := KeyHash;
                         Result := true;
                         Exit;
                       end;
                     end;
      else
        {bad news}
        RaiseException('TaaHashTableLinear.htlFindPrim: invalid element state')
      end;{case}
    {we didn't find the key or an empty slot this time around, so
     increment the index (taking care of the wraparound) and exit if
     we've got back to the start again}
    inc(KeyHash);
    if (KeyHash = htlTableSize) then
      KeyHash := 0;
    if (KeyHash = FirstKeyHash) then begin
      if (FirstDeleted <> -1) then
        aIndex := FirstDeleted
      else
        aIndex := -1; {this value means that the table is full}
      Exit;
    end;
    {$IFDEF DebugMode}
    inc(htlDebugSeeks);
    {$ENDIF}
  end;{forever loop}
end;
{--------}
function TaaHashTableLinear.htlGetItem(aIndex : integer) : pointer;
begin
  if (aIndex < 0) or (aIndex >= htlTableSize) then
    RaiseException('TaaHashTableLinear.htlGetItem: index out of bounds');
  with PHashElementArray(htlArray)^[aIndex] do
    if (heState = hesInUse) then
      Result := heObject
    else
      Result := nil;
end;

function TaaHashTableLinear.htlGetString(aIndex: integer): string;
begin
  if (aIndex < 0) or (aIndex >= htlTableSize) then exit;
//    RaiseException('TaaHashTableLinear.htlGetItem: index out of bounds');
  with PHashElementArray(htlArray)^[aIndex] do
    if (heState = hesInUse) then
      Result := heString
    else
      Result :='';
end;

{--------}
procedure TaaHashTableLinear.htlGrowTable;
begin
  {make the table roughly twice as large as before}
  htlAlterTableSize(GetClosestPrime(succ(htlTableSize * 2)));
end;
{--------}
function TaaHashTableLinear.htlHash(const aKey : string) : integer;
begin
  if Assigned(htlHashFunc) then
    Result := htlHashFunc(aKey) mod htlTableSize
  else
    Result := 0;
end;
{--------}
procedure TaaHashTableLinear.htlShrinkTable;
begin
  {make the table roughly half as large as before}
  htlAlterTableSize(GetClosestPrime(pred(htlTableSize) div 2));
end;
{--------}
procedure TaaHashTableLinear.Insert(const aKey : string; aObject : pointer);
var
  Inx : integer;
begin
  if htlFindPrim(aKey, Inx) then
    RaiseException('TaaHashTableLinear.Insert: duplicate key '+aKey);
  if (Inx = -1) then
    RaiseException('TaaHashTableLinear.Insert: table is full');
  with PHashElementArray(htlArray)^[Inx] do begin
    {$IFDEF Windows}
    heString := NewStr(aKey);
    {$ELSE}
    heString := aKey;
    {$ENDIF}
    heObject := aObject;
    heState := hesInUse;
  end;
  inc(htlCount);
  {grow the table if we're over 2/3 full}
  if ((htlCount * 3) > (htlTableSize * 2)) then
    htlGrowTable;
end;
{====================================================================}


end.
