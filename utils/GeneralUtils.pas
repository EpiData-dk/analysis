unit GeneralUtils;

interface

type
  /// <summary>A PriorityQueue maintains a partial ordering of its elements such that the
  /// least element can always be found in constant time.  Put()'s and pop()'s
  /// require log(size) time.
  /// </summary>
  TBasePriorityQueue = class(TObject)
  {
   Copyright 2005 Soluciones Vulcano
   Read License.txt for open source licensing info.
   Visit http://mutis.sourceforge.net/ for get more info.
  }
  private
    FHeap: array of TObject;
    FSize: Integer;
    FMaxSize: Integer;

    procedure UpHeap;
    procedure DownHeap;
  protected
      /// <summary>Subclass constructors must call this. </summary>
    procedure Initialize(maxSize: Integer);
  public
    /// <summary>Determines the ordering of objects in this priority queue.  Subclasses
    /// must define this one method.
    /// </summary>
    function LessThan(a: TObject; b: TObject): Boolean; virtual; abstract;
    /// <summary> Adds an Object to a PriorityQueue in log(size) time.
    /// If one tries to add more objects than maxSize from initialize
    /// a RuntimeException (ArrayIndexOutOfBound) is thrown.
    /// </summary>
    procedure Put(element: TObject);
    /// <summary> Adds element to the PriorityQueue in log(size) time if either
    /// the PriorityQueue is not full, or not lessThan(element, top()).
    /// </summary>
    /// <param name="">element
    /// </param>
    /// <returns> true if element is added, false otherwise.
    /// </returns>
    function Insert(element: TObject): Boolean; virtual;
    /// <summary>Returns the least element of the PriorityQueue in constant time. </summary>
    function Top: TObject;
    /// <summary>Removes and returns the least element of the PriorityQueue in log(size)
    /// time.
    /// </summary>
    function Pop: TObject;
    /// <summary>Should be called when the Object at top changes values.  Still log(n)
    /// worst case, but it's at least twice as fast to <pre>
    /// { pq.top().change(); pq.adjustTop(); }
    /// </pre> instead of <pre>
    /// { o = pq.pop(); o.change(); pq.push(o); }
    /// </pre>
    /// </summary>
    procedure AdjustTop;
    /// <summary>Returns the number of elements currently stored in the PriorityQueue. </summary>
    function Size: Integer;
    /// <summary>Removes all entries from the PriorityQueue. </summary>
    procedure Clear;
  end;

   TString = class(TObject)
   private
     fStr: String;
   public
     constructor Create(const AStr: String) ;
     property Str: String read FStr write FStr;
   end;

implementation

procedure TBasePriorityQueue.Initialize(maxSize: Integer);
var
  heapSize: Integer;
begin
  FSize := 0;

  heapSize := maxSize + 1;

  SetLength(FHeap, heapSize);

  FMaxSize := maxSize;
end;

function TBasePriorityQueue.Insert(element: TObject): Boolean;
begin
  if FSize < FMaxSize then
  begin
    Put(element);

    Result := True;
  end
  else if (Size > 0) and not (LessThan(element, Top)) then
  begin
    FHeap[1] := element;
    AdjustTop();

    Result := True;
  end
  else
  begin
    Result := False;
  end; //if
end;

function TBasePriorityQueue.Top: TObject;
begin
  if FSize > 0 then
  begin
    Result := FHeap[1];
  end
  else
  begin
    Result :=  nil;
  end;//if
end;

function TBasePriorityQueue.Pop: TObject;
var
  Value: TObject;
begin
  if Size > 0 then
  begin
    Value := FHeap[1];// save first value

    FHeap[1] := FHeap[Size];// move last to first
    FHeap[Size] := nil;   // permit GC of objects

    Dec(FSize);   // adjust heap

    DownHeap;

    Result := Value;
  end
  else
  begin
    Result := nil;
  end; //if
end;

procedure TBasePriorityQueue.AdjustTop;
begin
  DownHeap;
end;

procedure TBasePriorityQueue.Put(element: TObject);
begin
  Inc(FSize);

  FHeap[FSize] := element;

  UpHeap;
end;

function TBasePriorityQueue.Size: Integer;
begin
  Result := FSize;
end;

procedure TBasePriorityQueue.Clear;
var
  i: Integer;
begin
  for i := 0 to FSize - 1 do
  begin
    FHeap[i] := nil;
  end; //for

  FSize := 0;
end;

procedure TBasePriorityQueue.UpHeap;
var
  j: Cardinal;
  node: TObject;
  i: Cardinal;
begin
  i := FSize;
  node := FHeap[i]; // save bottom node

  j := Integer(Cardinal(i) shr 1);

  while (j > 0) and LessThan(node, FHeap[j]) do
  begin
    FHeap[i] := FHeap[j];  // shift parents down

    i := j;
    j := Integer(Cardinal(j) shr 1);
  end; //while

  FHeap[i] := node;  // install saved node
end;

procedure TBasePriorityQueue.DownHeap;
var
  k: Integer;
  j: Integer;
  node: TObject;
  i: Integer;
begin
  i := 1;
  node := FHeap[i]; // save top node
  j := i shl 1;  // find smaller child
  k := j + 1;

  if (k <= Size) and LessThan(FHeap[k], FHeap[j]) then
  begin
    j := k;
  end; //if

  while (j <= Size) and LessThan(FHeap[j], node) do
  begin
    FHeap[i] := FHeap[j]; // shift up child

    i := j;
    j := i shl 1;
    k := j + 1;

    if (k <= Size) and LessThan(FHeap[k], FHeap[j]) then
    begin
      j := k;
    end; //if
  end; //while

  FHeap[i] := node;  // install saved node
end;

constructor TString.Create(const AStr: String) ;
begin
   inherited Create;
   FStr := AStr;
end;

end.
