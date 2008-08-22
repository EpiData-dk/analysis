{+--------------------------------------------------------------------------+
 | Unit:        mwSupportProcs
 | Author:      Michael Hieke
 | Last change: 1999-11-14
 | Description: Supporting procedures for mwCustomEdit.
 | Version:     0.90
 | Thanks to:   HANAI Tohru
 +--------------------------------------------------------------------------+}

unit mwSupportProcs;

{$I MWEDIT.INC}

interface

uses Windows, Classes; 

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxListSize - 1] of integer;

// NOTE: this is redefinition of mwHighlighter.TIdentChars.  Should maybe go
//       into a common unit for types.  Has to be resolved.
  TIdentChars = set of char;

function Max(x, y: integer): integer;
function Min(x, y: integer): integer;
function MinMax(x, mi, ma: integer): integer;
procedure SwapInt(var l, r: integer);

function GetIntArray(Count: Cardinal; InitialValue: integer): PIntArray;

procedure InternalFillRect(dc: HDC; const rcPaint: TRect);

// Converting tabs to spaces: To use the function several times it's better
// to use a function pointer that is set to the fastest conversion function.
type
  TConvertTabsProc = function(const Line: AnsiString;
                              TabWidth: integer): AnsiString;

function GetBestConvertTabsProc(TabWidth: integer): TConvertTabsProc;
// This is the slowest conversion function which can handle TabWidth <> 2^n.
function ConvertTabs(const Line: AnsiString; TabWidth: integer): AnsiString;

function CharIndex2CaretPos(Index, TabWidth: integer;
                            const Line: string): integer;
function CaretPos2CharIndex(Position, TabWidth: integer; const Line: string;
                            var InsideTabChar: boolean): integer;

// search for the first char of set AChars in Line, starting at index Start
function StrScanForCharInSet(const Line: string; Start: integer;
                             AChars: TIdentChars): integer;
// the same, but searching backwards
function StrRScanForCharInSet(const Line: string; Start: integer;
                              AChars: TIdentChars): integer;

implementation

uses SysUtils;

{***}

function Max(x, y: integer): integer;
begin
  if x > y then Result := x else Result := y;
end;

function Min(x, y: integer): integer;
begin
  if x < y then Result := x else Result := y;
end;

function MinMax(x, mi, ma: integer): integer;
begin
  if (x < mi) then Result := mi
    else if (x > ma) then Result := ma else Result := x;
end;

procedure SwapInt(var l, r: integer);
var tmp: integer;
begin
  tmp := r;
  r := l;
  l := tmp;
end;

{***}

function GetIntArray(Count: Cardinal; InitialValue: integer): PIntArray;
var p: PInteger;
begin
  Result := AllocMem(Count * SizeOf(integer));
  if Assigned(Result) and (InitialValue <> 0) then begin
    p := PInteger(Result);
    while (Count > 0) do begin
      p^ := InitialValue;
      Inc(p);
      Dec(Count);
    end;
  end;
end;

procedure InternalFillRect(dc: HDC; const rcPaint: TRect);
begin
  ExtTextOut(dc, 0, 0, ETO_OPAQUE, @rcPaint, nil, 0, nil);
end;

{***}

// mh: Please don't change; no stack frame and efficient register use.
function HasTabs(pLine: PChar; var CharsBefore: integer): boolean;
begin
  CharsBefore := 0;
  if Assigned(pLine) then begin
    while (pLine^ <> #0) do begin
      if (pLine^ = #9) then break;
      Inc(CharsBefore);
      Inc(pLine);
    end;
    Result := (pLine^ = #9);
  end else
    Result := FALSE;
end;

function ConvertTabs1(const Line: AnsiString; TabWidth: integer): AnsiString;
var pDest: PChar;
    nBeforeTab: integer;
begin
  Result := Line;  // increment reference count only
  if HasTabs(pointer(Line), nBeforeTab) then begin
    pDest := @Result[nBeforeTab + 1]; // this will make a copy of Line
    // We have at least one tab in the string, and the tab width is 1.
    // pDest points to the first tab char. We overwrite all tabs with spaces.
    repeat
      if (pDest^ = #9) then pDest^ := ' ';
      Inc(pDest);
    until (pDest^ = #0);
  end;
end;

function ConvertTabs2n(const Line: AnsiString; TabWidth: integer): AnsiString;
var i, DestLen, TabCount, TabMask: integer;
    pSrc, pDest: PChar;
begin
  Result := Line;  // increment reference count only
  if HasTabs(pointer(Line), DestLen) then begin
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width equals 2^n.
    // pSrc points to the first tab char in Line. We get the number of tabs
    // and the length of the expanded string now.
    TabCount := 0;
    TabMask := (TabWidth - 1) xor $7FFFFFFF;
    repeat
      if (pSrc^ = #9) then begin
        DestLen := (DestLen + TabWidth) and TabMask;
        Inc(TabCount);
      end else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    // We use another TabMask here to get the difference to 2^n.
    TabMask := TabWidth - 1;
    repeat
      if (pSrc^ = #9) then begin
        i := TabWidth - (DestLen and TabMask);
        Inc(DestLen, i);
        repeat
          pDest^ := ' ';
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if (TabCount = 0) then begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end else begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end;
end;

function ConvertTabs(const Line: AnsiString; TabWidth: integer): AnsiString;
var i, DestLen, TabCount: integer;
    pSrc, pDest: PChar;
begin
  Result := Line;  // increment reference count only
  if HasTabs(pointer(Line), DestLen) then begin
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width is greater
    // than 1. pSrc points to the first tab char in Line. We get the number
    // of tabs and the length of the expanded string now.
    TabCount := 0;
    repeat
      if (pSrc^ = #9) then begin
        DestLen := DestLen + TabWidth - DestLen mod TabWidth;
        Inc(TabCount);
      end else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    repeat
      if (pSrc^ = #9) then begin
        i := TabWidth - (DestLen mod TabWidth);
        Inc(DestLen, i);
        repeat
          pDest^ := ' ';
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if (TabCount = 0) then begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end else begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end;
end;

function IsPowerOfTwo(TabWidth: integer): boolean;
var nW: integer;
begin
  nW := 2;
  repeat
    if (nW >= TabWidth) then break;
    Inc(nW, nW);
  until (nW >= $10000);  // we don't want 64 kByte spaces...
  Result := (nW = TabWidth);
end;

function GetBestConvertTabsProc(TabWidth: integer): TConvertTabsProc;
begin
  if (TabWidth < 2) then Result := TConvertTabsProc(@ConvertTabs1)        
    else if IsPowerOfTwo(TabWidth) then
      Result := TConvertTabsProc(@ConvertTabs2n)
    else
      Result := TConvertTabsProc(@ConvertTabs);
end;

{***}

function CharIndex2CaretPos(Index, TabWidth: integer;
                            const Line: string): integer;
var iChar: integer;
    pNext: PChar;
begin
// possible sanity check here: Index := Max(Index, Length(Line));
  if Index > 1 then begin
    if (TabWidth <= 1) or not HasTabs(pointer(Line), iChar) then
      Result := Index
    else begin
      if iChar + 1 >= Index then
        Result := Index
      else begin
        // iChar is number of chars before first #9
        Result := iChar;
        // Index is *not* zero-based
        Inc(iChar);
        Dec(Index, iChar);
        pNext := @Line[iChar];
        while Index > 0 do begin
          case pNext^ of
            #0: break;
            #9: begin
                  // Result is still zero-based
                  Inc(Result, TabWidth);
                  Dec(Result, Result mod TabWidth);
                end;
            else Inc(Result);
          end;
          Dec(Index);
          Inc(pNext);
        end;
        // done with zero-based computation
        Inc(Result);
      end;
    end;
  end else
    Result := 1;
end;

function CaretPos2CharIndex(Position, TabWidth: integer; const Line: string;
                            var InsideTabChar: boolean): integer;
var iPos: integer;
    pNext: PChar;
begin
  InsideTabChar := FALSE;
  if Position > 1 then begin
    if (TabWidth <= 1) or not HasTabs(pointer(Line), iPos) then
      Result := Position
    else begin
      if iPos + 1 >= Position then
        Result := Position
      else begin
        // iPos is number of chars before first #9
        Result := iPos + 1;
        pNext := @Line[Result];
        // for easier computation go zero-based (mod-operation)
        Dec(Position);
        while iPos < Position do begin
          case pNext^ of
            #0: break;
            #9: begin
                  Inc(iPos, TabWidth);
                  Dec(iPos, iPos mod TabWidth);
                  if iPos > Position then begin
                    InsideTabChar := TRUE;
                    break;
                  end;
                end;
            else Inc(iPos);
          end;
          Inc(Result);
          Inc(pNext);
        end;
      end;
    end;
  end else
    Result := Position;
end;

function StrScanForCharInSet(const Line: string; Start: integer;
                             AChars: TIdentChars): integer;
var p: PChar;
begin
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    p := PChar(@Line[Start]);
    repeat
      if p^ in AChars then
      begin
        Result := Start;
        exit;
      end;
      Inc(p);
      Inc(Start);
    until p^ = #0;
  end;
  Result := 0;
end;

function StrRScanForCharInSet(const Line: string; Start: integer;
                              AChars: TIdentChars): integer;
var p: PChar;
begin
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    p := PChar(@Line[Start]);
    repeat
      if p^ in AChars then
      begin
        Result := Start;
        exit;
      end;
      Dec(p);
      Dec(Start);
    until Start < 1;
  end;
  Result := 0;
end;

end.
