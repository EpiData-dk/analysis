{ ===
TSGrep.pas    copyright (C) 2000   R. Collins
rcoll@rtd.com

This software is provided "as-is".  This software comes without warranty 
or guarantee, explicit or implied.  Use this software at your own risk.  
The author will not be liable for any damage to equipment, data, or information
that may result while using this software.

By using this software, you agree to the conditions stated above.

This software may be used freely, provided the author's name and copyright
statement remain a part of the source code.
=== }


unit SGrep;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;



{ ===========================================

PATTERN MATCHING OPERATORS:

?           match one of anything
*           match any number of anything
"xxx"       match literal string inside quotes
\000        literal char code; if starts with 0 then code in octal
\999        literal char code; if starts with 1-9 then code in decimal
\xHH        literal char code; "x" followed by 2 hex digits
\n          line feed
\r          carraige return
\t          tab
\f          form feed
[xxxx]      any one of a set of chars; inside, use "A-Z" to span chars
[-xxxx]     any one not in set of chars
%x          special char set; any char of the set
%-x         special char set; any char not in set
            x == W  any number of white space
                 w  any single white space
                 N  any number of "0123456789"
                 n  any single char "0123456789"
                 A  any number "a-z" or "A-Z"
                 a  any single char of "a-z" or "A-Z"
                 X  any number of "0-9" or "a-z" or "A-Z"
                 x  any single char of "0-9" or "a-z" or "A-Z"
@nnn        current match bead position
@-nnn       current match bead position, from end of string
(xxx)       (parentheses) define sub-expression or hierarchy
(min:max)   (curly braces) repeat count for next operation
&           logical AND of 2 expressions; normally not needed
|           logical OR of 2 expressions
!x          write value to OutStrings list
            x == A  write anchor value
                 B  write bead value
                 S  write matching substring (index anchor to bead)
                 T  write total substring (index 1 to bead)

============================================ }



// when searching for one of many ...

type
    TSGrepCharset   = set of char;

// an individual pattern token

type
    PSGrepOpcode    = ^TSGrepOpcode;
    TSGrepOpcode    = record
        rOpcode:      char;             // operation to perform
        rNegate:      boolean;          // negative operation?
        rValid:       boolean;          // has this opcode been eliminated?
        rTouched:     boolean;          // check for faulty tree
        rSvalue:      string;           // data needed by this opcode
        rIvalue1:     integer;          // data needed by this opcode
        rIvalue2:     integer;          // data needed by this opcode
        rTag:         integer;          // ID for this record
        rNext:        integer;          // next sequential opcode
        rPrev:        integer;          // previous sequential opcode
        rLeft:        integer;          // next opcode on a decision branch
        rRight:       integer;          // next opcode on a decision branch
        end;

// operators

const
    kSGrepHead         = 'H';              // head of opcode list
    kSGrepTail         = 'T';              // end of the opcode list
    kSGrepRight        = 'r';              // right side of decision tree
    kSGrepLeft         = 'l';              // left side of decision tree
    kSGrepSubTail      = 't';              // end of a sub-expression list
    kSGrepIgnore       = ' ';              // no operation
    kSGrepAnyChar      = '?';              // match any single char
    kSGrepLiteral      = 'L';              // match exactly
    kSGrepSpan         = 'S';              // match any of a set
    kSGrepPosition     = '@';              // position in match string
    kSGrepSubBeg       = '(';              // begin sub-expression
    kSGrepSubEnd       = ')';              // end sub-expression
    kSGrepRepeat       = 'R';              // repeat factor
    kSGrepOr           = '|';              // logical OR of LEFT and RIGHT
    kSGrepAnd          = '&';              // logical AND of LEFT and RIGHT
    kSGrepOutput       = '!';              // write something


// simple grep events

type
    TSGrepEvent     = procedure(Sender: TObject; Str: string) of object;

// the grep pattern class

type
    TSGrep = class(TObject)
    private                                             { Private declarations }
        fTag:           integer;                    // individual ID
        fPReady:        boolean;                    // pattern parsed yet?
        fPSuccess:      boolean;                    // parse OK?
        fPBead:         integer;                    // current parse position
        fPLength:       integer;                    // length of PSource
        fPSource:       string;                     // user-defined pattern source
        fOpList:        TList;                      // master list of parsed ops
        fESource:       string;                     // user-defined source string
        fEAnchor:       integer;                    // beginning of pattern match
        fEBead:         integer;                    // next char to match
        fELength:       integer;                    // length of ESource
        fESuccess:      boolean;                    // match OK?
        fESpanset:      TSGrepCharset;              // set of chars to span
        fESpanIndex:    integer;                    // which op?
        fESubString:    string;                     // matching substring
        fOnWrite:       TSGrepEvent;                // when writing
        fOnError:       TSGrepEvent;                // parse errors


        procedure Set_Tag(N: integer);
        function  Get_Tag: integer;
        procedure Set_Pattern(Pattern: string);
        function  Get_Pattern: string;
        procedure Set_Source(Source: string);
        function  Get_Source: string;
        function  Get_Success: boolean;
        procedure Set_Anchor(ank: integer);
        function  Get_Anchor: integer;
        procedure Set_Bead(beed: integer);
        function  Get_Bead: integer;
        function  Get_SubString: string;
        procedure Set_OnWrite(proc: TSGrepEvent);
        function  Get_OnWrite: TSGrepEvent;
        procedure Set_OnError(proc: TSGrepEvent);
        function  Get_OnError: TSGrepEvent;

        procedure FNote(msg: string);


    protected                                           { Protected declarations }
        function  Alloc_Opcode: PSGrepOpcode;

        procedure Parse_Begin;
        function  Parse_CheckChar(clist: TSGrepCharset): boolean;
        procedure Parse_Number(Base: integer; var Neg: boolean; var Num: integer);
        procedure Parse_Opcode;
        procedure Parse_Quote(qopen: char; qclose: char; bneg: boolean; bspan: boolean; var str: string; var neg: boolean);
        procedure Parse_Repeat(var rmin: integer; var rmax: integer);
        procedure Parse_Slash(var c: char);
        procedure Parse_SpecialSpan(var str: string; var neg: boolean; var ser: boolean);
        procedure Parse_Whitespace;

        procedure Sort_Begin;
        procedure Sort_Branches;
        function  Sort_FindTail(cur: integer): integer;
        procedure Sort_Flat;
        function  Sort_GetNext(cur: integer): integer;
        function  Sort_GetPrev(cur: integer): integer;
        procedure Sort_InValid(idx: integer);
        procedure Sort_Link(prev, next: integer);
        procedure Sort_LinkLeft(opd: integer; opf: integer; opl: integer);
        procedure Sort_LinkRight(opd: integer; opf: integer; opl: integer);
        procedure Sort_Repeat;
        procedure Sort_SubExpr;
        procedure Sort_UnTouched;

        procedure Eval_Begin;
        procedure Eval_List(Top: integer);
        procedure Eval_AnyChar(Index: integer);
        procedure Eval_Literal(Index: integer);
        procedure Eval_Span(Index: integer);
        procedure Eval_Position(Index: integer);
        procedure Eval_Repeat(Index: integer);
        procedure Eval_And(Index: integer);
        procedure Eval_Or(Index: integer);
        procedure Eval_Output(Index: integer);


    public                                              { Public declarations }

    published                                           { Published declarations }
        constructor Create;
        constructor CreateFromPattern(const Pattern: string);
        procedure Clear;
        procedure Free;

        function  MatchFirst(S: string): integer;
        function  MatchNext: integer;
        function  Execute(P,S: string): boolean;

        procedure Dump_OpList(Strings: TStrings);

        property Tag:           integer     read Get_Tag            write Set_Tag;
        property Pattern:       string      read Get_Pattern        write Set_Pattern;
        property Source:        string      read Get_Source         write Set_Source;
        property Success:       boolean     read Get_Success;
        property Anchor:        integer     read Get_Anchor         write Set_Anchor;
        property Bead:          integer     read Get_Bead           write Set_Bead;
        property SubString:     string      read Get_SubString;
        property OnWrite:       TSGrepEvent read Get_OnWrite        write Set_OnWrite;
        property OnError:       TSGrepEvent read Get_OnError        write Set_OnError;

    end;


{ ---------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }

Type
TMatchResult=record
   Matched :string;
   BPos,EPos : integer;
   Remained :string;
end;

function MatchString(const Mask,source:string;StartPos:integer):TMatchResult;

implementation


function MatchString(const Mask,source:string;StartPos:integer):TMatchResult;
var
 sg:     TSGrep;
 n : integer;
 s : string;
begin
  result.BPos:=-1;
try
  sg := TSGrep.CreateFromPattern(Mask);
  sg.Anchor:= StartPos;
  result.BPos :=sg.MatchFirst(source);
  result.remained:=source;
  if result.BPos> -1 then
  begin
     result.matched :=sg.SubString;
     result.EPos :=sg.Bead;
     delete(result.remained, result.BPos+1,result.EPos-result.BPos);
  end;
finally
  sg.free;
end;
end;



{ ---------------------------------------------------------------------------- }

constructor TSGrep.Create;
begin
inherited Create;

fTag         := 0;
fPReady      := false;
fPSuccess    := false;
fPBead       := 0;
fPLength     := 0;
fPSource     := '';
fOpList      := TList.Create;
fESource     := '';
fEAnchor     := 0;
fEBead       := 0;
fELength     := 0;
fESuccess    := false;
fESpanSet    := [];
fESpanIndex  := -1;
fESubString  := '';
fOnWrite     := nil;
fOnError     := nil;
end;

{ ---------------------------------------------------------------------------- }

constructor TSGrep.CreateFromPattern(const Pattern: string);
begin
inherited Create;

// initialize object

fTag         := 0;
fPReady      := false;
fPSuccess    := false;
fPBead       := 0;
fPLength     := 0;
fPSource     := '';
fOpList      := TList.Create;
fESource     := '';
fEAnchor     := 0;
fEBead       := 0;
fELength     := 0;
fESuccess    := false;
fESpanSet    := [];
fESpanIndex  := -1;
fESubString  := '';
fOnWrite     := nil;
fOnError     := nil;

// parse the pattern to search for

Set_Pattern(Pattern);           // init the pattern pieces
Parse_Begin;                    // parse pattern into tokens
Sort_Begin;                     // sort tokens into tree
fPReady := true;                // OK, it's ready
end;

{ ---------------------------------------------------------------------------- }
{ release memory used by the object }

procedure TSGrep.Clear;
var
    i,n:    integer;
    p:      PSGrepOpcode;
begin

// free saved opcode records

for i := 0 to (fOpList.Count - 1) do
    begin
    p := fOpList.Items[i];
    if (p <> nil) then dispose(p);
    end;

fOpList.Clear;

// and reset everything else

fPReady      := false;
fPSuccess    := false;
fPBead       := 0;
fPLength     := 0;
fPSource     := '';
fESource     := '';
fEAnchor     := 0;
fEBead       := 0;
fELength     := 0;
fESuccess    := false;
fESpanSet    := [];
fESpanIndex  := -1;
fESubString  := '';
end;

{ ---------------------------------------------------------------------------- }

procedure TSGrep.Free;
begin
Clear;
fOplist.Free;

inherited Free;
end;



{ ---------------------------------------------------------------------------- }
{ property TAG }

procedure TSGrep.Set_Tag(N: integer);
begin
fTag := N;
end;

function TSGrep.Get_Tag: integer;
begin
Get_Tag := fTag;
end;

{ ---------------------------------------------------------------------------- }
{ property PATTERN }

procedure TSGrep.Set_Pattern(Pattern: string);
begin
Clear;

fPSource  := Pattern;
fPReady   := false;
fPSuccess := true;
fPBead    := 0;
fPLength  := length(fPSource);
end;

function TSGrep.Get_Pattern: string;
begin
Get_Pattern := fPSource;
end;


{ ---------------------------------------------------------------------------- }
{ property SOURCE }

procedure TSGrep.Set_Source(Source: string);
begin
fESource  := Source;
fELength  := length(fESource);
fEAnchor  := 0;
fEBead    := 0;
fESuccess := true;
fESUbString := '';
end;

function TSGrep.Get_Source: string;
begin
Get_Source := fESource;
end;


{ ---------------------------------------------------------------------------- }
{ property SUCCESS }

function TSGrep.Get_Success: boolean;
begin
Get_Success := fESuccess;
end;

{ ---------------------------------------------------------------------------- }
{ property ANCHOR }

procedure TSGrep.Set_Anchor(ank: integer);
begin
fEAnchor := ank;
end;

function TSGrep.Get_Anchor: integer;
begin
Get_Anchor := fEAnchor;
end;

{ ---------------------------------------------------------------------------- }
{ property BEAD }

procedure TSGrep.Set_Bead(beed: integer);
begin
fEBead := beed;
end;

function TSGrep.Get_Bead: integer;
begin
Get_Bead := fEBead;
end;

{ ---------------------------------------------------------------------------- }
{ property SUBSTRING }

function TSGrep.Get_SubString: string;
begin
Get_SubString := fESubString;
end;

{ ---------------------------------------------------------------------------- }
{ property OnWrite }

procedure TSGrep.Set_OnWrite(proc: TSGrepEvent);
begin
fOnWrite := proc;
end;

function TSGrep.Get_OnWrite: TSGrepEvent;
begin
Get_OnWrite := fOnWrite;
end;

{ ---------------------------------------------------------------------------- }
{ property OnError }

procedure TSGrep.Set_OnError(proc: TSGrepEvent);
begin
fOnError := proc;
end;

function TSGrep.Get_OnError: TSGrepEvent;
begin
Get_OnError := fOnError;
end;



{ ---------------------------------------------------------------------------- }
{ display a note and set fail flag }

procedure TSGrep.FNote(msg: string);
begin
if (assigned(fOnError)) then fOnError(Self, '(error) ' + msg);
fPSuccess := false;
fESuccess := false;
end;


{ ---------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }
{ allocate memory for a new opcode record and save on master list }

function TSGrep.Alloc_Opcode: PSGrepOpcode;
var
    i:      integer;
    p:      PSGrepOpcode;
begin
new(p);

p^.rOpcode   := kSGrepIgnore;
p^.rSvalue   := '';
p^.rIvalue1  := 0;
p^.rIvalue2  := 0;
p^.rNegate   := false;
p^.rTag      := 0;
p^.rNext     := -1;
p^.rPrev     := -1;
p^.rLeft     := -1;
p^.rRight    := -1;
p^.rValid    := true;

i := fOpList.Add(p);
p^.rTag := i;

Alloc_Opcode := p;
end;



{ ---------------------------------------------------------------------------- }
{ format and display an entire opcode list }

procedure TSGrep.Dump_OpList(Strings: TStrings);
var
    i,n:    integer;
    v,g:    char;
    s:      string;
    p:      PSGrepOpcode;
begin
for i := 0 to (fOpList.Count - 1) do
    begin
    p := fOpList.Items[i];
    v := ' ';               // valid opcode
    g := ' ';               // negated operation
    if (p^.rValid)  then v := 'V';
    if (p^.rNegate) then g := '-';

    with p^ do
        begin
        FmtStr(s, '%1s %3d)%1s%1s [%3d,%3d,%3d,%3d] I1=%4d I2=%4d S="%s"',
        [v, rTag, rOpcode, g, rPrev, rNext, rLeft, rRight, rIvalue1, rIvalue2, rSvalue]);
        end;

    Strings.Add(s);
    end;
end;

{ ---------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }
{ parse a text string into a list of specific op-codes }


procedure TSGrep.Parse_Begin;
var
    p:      PSGrepOpcode;
begin

// insert head of list
// this also guarantees a list of at least 1 item

p := Alloc_Opcode;
p^.rOpcode := kSGrepHead;

// head must be element 0

if (p^.rTag <> 0) then FNote('Parse_Begin: list head is not tag 0');

// start at beginning of source string

fPBead := 1;

// and get everything until done

while ((fPBead <= fPLength) and (fPSuccess)) do Parse_Opcode;

// put in last opcode

p := Alloc_OpCode;
p^.rOpcode := kSGrepTail;
end;


{ ---------------------------------------------------------------------------- }
{ any of the set of chars next in parse string? }

function TSGrep.Parse_CheckChar(clist: TSGrepCharset): boolean;
var
    b:  boolean;
begin
b := ((fPBead <= fPLength) and (fPSource[fPBead] in clist));
Parse_CheckChar := b;
end;


{ ---------------------------------------------------------------------------- }

procedure TSGrep.Parse_Number(Base: integer; var Neg: boolean; var Num: integer);
var
    i,n:    integer;
    l:      integer;
begin
Neg := false;
Num := 0;
l := 0;
if (Base = 8) then
    begin
    while (Parse_CheckChar(['0'..'7'])) do
        begin
        i := ord(fPSource[fPBead]) - ord('0');
        Num := (Num * 8) + i;
        fPBead := fPBead + 1;
        l := l + 1;
        end;
    end
else if (Base = 10) then        // may be negative
    begin
    if (Parse_CheckChar(['-'])) then
        begin
        Neg := true;
        fPBead := fPBead + 1;
        end;
    while (Parse_CheckChar(['0'..'9'])) do
        begin
        i := ord(fPSource[fPBead]) - ord('0');
        Num := (Num * 10) + i;
        fPBead := fPBead + 1;
        l := l + 1;
        end;
    end
else if (Base = 16) then
    begin
    while (Parse_CheckChar(['0'..'7', 'a'..'f',  'A'..'F'])) do
        begin
        if (fPSource[fPBead] in ['a'..'f']) then
            i := (ord(fPSource[fPBead]) - ord('a')) - 10
        else if (fPSource[fPBead] in ['A'..'F']) then
            i := (ord(fPSource[fPBead]) - ord('A')) - 10
        else
            i := ord(fPSource[fPBead]) - ord('0');
        Num := (Num shl 4) or i;
        fPBead := fPBead + 1;
        l := l + 1;
        end;
    end;

if (l = 0) then
    begin
    FNote('Parse_Number: number not found');
    end;
end;


{ ---------------------------------------------------------------------------- }
{ parse next operator & data from source pattern string }

procedure TSGrep.Parse_Opcode;
var
    i,n:        integer;
    c:          char;
    prpt:       boolean;                            // repeat count?
    ptid:       char;                               // pattern ID
    pstr:       string;                             // pattern string
    pmin:       integer;
    pmax:       integer;
    pneg:       boolean;
    pint:       integer;
    nstr:       string;
    series:     boolean;
    p:          PSGrepOpcode;

begin

// trim white spaces

Parse_Whitespace;

// init local op vars

prpt := false;
ptid := kSGrepIgnore;
pstr := '';
pint := 0;
pmin := 1;
pmax := 1;
pneg := false;

// check if at end of source

if (fPBead > fPLength) then exit;

// parse a single operator

case fPSource[fPBead] of
    ' ':    begin                       // ignore spaces unless quoted
            ptid := kSGrepIgnore;
            fPBead := fPBead + 1;
            end;
    '?':    begin                       // match exactly one
            ptid := kSGrepAnyChar;
            fPBead := fPBead + 1;
            end;
    '*':    begin                       // 0..32767 of anything
            ptid := kSGrepAnyChar;
            prpt := true;
            pmin := 0;
            pmax := 32767;
            fPBead := fPBead + 1;
            end;
    '"':    begin                       // quoted string
            ptid := kSGrepLiteral;
            Parse_Quote('"', '"', false, false, pstr, pneg);
            end;
    '\':    begin                       // single literal char
            ptid := kSGrepLiteral;
            Parse_Slash(c);
            pstr := c;
            end;
    '[':    begin                       // span of chars
            ptid := kSGrepSpan;
            Parse_Quote('[', ']', true, true, pstr, pneg);
            end;
    '%':    begin                       // special span of chars
            ptid := kSGrepSpan;
            Parse_SpecialSpan(pstr, pneg, prpt);
            pmin := 0;                  // only used if PRPT is true
            pmax := 32767;
            end;
    '@':    begin                       // positiion
            ptid := kSGrepPosition;
            fPBead := fPBead + 1;
            Parse_Number(10, pneg, pint);
            end;
    '(':    begin                       // begin sub-expression
            ptid := kSGrepSubBeg;
            fPBead := fPBead + 1;
            end;
    ')':    begin                       // end sub-expression
            ptid := kSGrepSubEnd;
            fPBead := fPBead + 1;
            end;
    '{':    begin                       // begine repeat count
            ptid := kSGrepRepeat;
            Parse_Repeat(pmin, pmax);
            prpt := true;
            end;
    '&':    begin                       // logical AND
            ptid := kSGrepAnd;
            fPBead := fPBead + 1;
            end;
    '|':    begin                       // logical OR
            ptid := kSGrepOr;
            fPBead := fPBead + 1;
            end;
    '!':    begin                       // write something out
            ptid := kSGrepOutput;
            fPBead := fPBead + 1;
            if (Parse_Checkchar(['A','B','S','T'])) then pstr := fPSource[fPBead]
            else  FNote('Parse_Opcode: invalid output directive');
            fPBead := fPBead + 1;
            end;

    else    begin                       // misc literals
            ptid := kSGrepLiteral;
            pstr := fPSource[fPBead];
            fPBead := fPBead + 1;
            end;
    end;                                // case


// automatic max repeat?

if (prpt) then
    begin
    p := Alloc_Opcode;
    p^.rOpcode := kSGrepIgnore;

    p := Alloc_Opcode;
    p^.rOpcode  := kSGrepRepeat;
    p^.rIvalue1 := pmin;
    p^.rIvalue2 := pmax;
    end;

// add item to master opcode list and
// make note of what we found

if ((ptid <> kSGrepIgnore) and (ptid <> kSGrepRepeat)) then
    begin
    p := Alloc_Opcode;
    p^.rOpcode  := ptid;
    p^.rSvalue  := pstr;
    p^.rIvalue1 := pint;
    p^.rIvalue2 := 0;
    p^.rNegate  := pneg;
    end;
end;


{ ---------------------------------------------------------------------------- }
{ generic quoting utility }
{ qopen, qclose         open and close quote chars }
{ bneg                  can string be negated? }
{ bspan                 can chars be spanned; e.g. a-z  or 0-9 ? }
{ str                   output string }
{ neg                   output negation }

procedure TSGrep.Parse_Quote(qopen: char; qclose: char; bneg: boolean; bspan: boolean; var str: string; var neg: boolean);
var
    i:      integer;
    ch:     char;
    n1,n2:  integer;
    rc:     boolean;
begin

// nothing yet

str := '';
neg := false;
rc := false;

// first char must be opening quote

if (not Parse_CheckChar([qopen])) then FNote('Parse_Quote: no open quote (' + qopen + ')');

// skip opening quote

fPBead := fPBead + 1;

// check for negation operator -- MUST be next char in the source

if ((bneg) and (Parse_CheckChar(['-']))) then
    begin
    neg := true;
    fPBead := fPBead + 1;
    end;

// get chars one at a time

while ((fPBead <= fPLength) and (fPSource[fPBead] <> qclose)) do
    begin
    if (fPSource[fPBead] = '\') then        // get next char
        begin
        Parse_Slash(ch);
        end
    else
        begin
        ch := fPSource[fPBead];
        fPBead := fPBead + 1;
        end;

    if (not bspan) then                     // spanning not allowed
        begin
        str := str + ch;
        end
    else if (rc) then                       // second of a range?
        begin
        n2 := ord(ch);
        for i := n1 to n2 do str := str + chr(i);
        rc := false;
        end
    else if ((fPBead <= fPLength) and       // first of a range?
            (fPSource[fPBead] = '-')) then
        begin
        n1 := ord(ch);
        rc := true;
        fPBead := fPBead + 1;
        end
    else                                    // just a normal char
        begin
        str := str + ch;
        rc := false;
        end;
    end;

// end of quoted string

if (Parse_CheckChar([qclose])) then fPBead := fPBead + 1
else                                FNote('Parse_Quote: no close quote (' + qclose + ') "' + str + '"');
end;


{ ---------------------------------------------------------------------------- }
{ parse out a repeat construction }

procedure TSGrep.Parse_Repeat(var rmin: integer; var rmax: integer);
var
    fmin:   boolean;
    fmax:   boolean;
    fsep:   boolean;
    s:      boolean;
begin

// nothing yet

rmin := 0;
rmax := 0;
fmin := false;
fmax := false;
fsep := false;

// first char

if (Parse_CheckChar(['{'])) then
    begin
    fPBead := fPBead + 1;
    Parse_Whitespace;
    end
else
    begin
    FNote('Parse_Repeat: open bracket not found');
    end;

// the first number

if (Parse_CheckChar(['-','0'..'9'])) then
    begin
    Parse_Number(10, s, rmin);
    Parse_Whitespace;
    if (s) then rmin := -(rmin);
    fmin := true;
    end;

// colon separator

if (Parse_CheckChar([':'])) then
    begin
    fsep := true;
    fPBead := fPBead + 1;
    Parse_Whitespace;
    end;

// second number

if (Parse_CheckChar(['-','0'..'9'])) then
    begin
    Parse_Number(10, s, rmax);
    Parse_Whitespace;
    if (s) then rmax := -(rmax);
    fmax := true;
    end;

// and terminator

if (Parse_CheckChar(['}'])) then
    begin
    fPBead := fPBead + 1;
    end
else
    begin
    FNote('Parse_Repeat: close bracket not found');
    end;

// and apply defaults where applicable

if      (not fsep) then rmax := rmin
else if (not fmin) then rmin := 0
else if (not fmax) then rmax := 32767;

// valid numbers?

if ((rmin < 0) or (rmax < 0)) then FNote('Parse_Repeat: invalid repeat factor: ' + IntToStr(rmin) + ' ' + IntToStr(rmax));
end;


{ ---------------------------------------------------------------------------- }

procedure TSGrep.Parse_Slash(var c: char);
var
    i,n:    integer;
    s:      boolean;
begin
if (Parse_CheckChar(['\'])) then
    begin
    fPBead := fPBead + 1;
    if (fPBead > fPLength) then
        begin
        FNote('Parse_Slash: no character after slash');
        end
    else if (fPSource[fPBead] = '0') then          // octal number
        begin
        Parse_Number(8, s, n);
        c := chr(n);
        end
    else if (Parse_CheckChar(['0'..'9'])) then  // decimal number
        begin
        Parse_Number(10, s, n);
        c := chr(n);
        end
    else if (fPSource[fPBead] = 'x') then       // hex number
        begin
        fPBead := fPBead + 1;
        Parse_Number(16, s, n);
        c := chr(n);
        end
    else if (fPSource[fPBead] = 'n') then       // line feed
        begin
        c := chr(10);
        fPBead := fPBead + 1;
        end
    else if (fPSource[fPBead] = 'r') then       // carraige return
        begin
        c := chr(13);
        fPBead := fPBead + 1;
        end
    else if (fPSource[fPBead] = 't') then       // tab
        begin
        c := chr(9);
        fPBead := fPBead + 1;
        end
    else if (fPSource[fPBead] = 'f') then       // form feed
        begin
        c := chr(12);
        fPBead := fPBead + 1;
        end
    else                                        // single character
        begin
        c := fPSource[fPBead];
        fPBead := fPBead + 1;
        end;
    end
else
    begin
    FNote('Parse_Slash: leading "\" not found');
    end;
end;


{ ---------------------------------------------------------------------------- }
{ parse special spanning characters }

procedure TSGrep.Parse_SpecialSpan(var str: string; var neg: boolean; var ser: boolean);
var
    c:      char;
begin

// nothing yet

str := '';
neg := false;
ser := false;

// must start with special character

if (Parse_CheckChar(['%'])) then fPBead := fPBead + 1
else                             FNote('Parse_SpecialSpan: no open "%" found');

// may be negated

if (Parse_CheckChar(['-'])) then
    begin
    neg := true;
    fPBead := fPBead + 1;
    end;

// and check what we are spanning

if (fPBead > fPLength) then
    begin
    FNote('Parse_SpecialSpan: no spanning definition character after "%"');
    end
else if (Parse_CheckChar(['n','N'])) then
    begin
    if (fPSource[fPBead] = 'N') then ser := true;
    fPBead := fPBead + 1;
    str := '0123456789';
    end
else if (Parse_CheckChar(['a','A'])) then
    begin
    if (fPSource[fPBead] = 'A') then ser := true;
    fPBead := fPBead + 1;
    str := '';
    for c := 'a' to 'z' do str := str + c;
    for c := 'A' to 'Z' do str := str + c;
    end
else if (Parse_CheckChar(['x','X'])) then
    begin
    if (fPSource[fPBead] = 'X') then ser := true;
    fPBead := fPBead + 1;
    str := '0123456789';
    for c := 'a' to 'z' do str := str + c;
    for c := 'A' to 'Z' do str := str + c;
    end
else if (Parse_CheckChar(['w','W'])) then
    begin
    if (fPSource[fPBead] = 'W') then ser := true;
    fPBead := fPBead + 1;
    str := '';
    for c := chr(1) to chr(32) do str := str + c;
    end
else
    begin
    c := fPSource[fPBead];
    FNote('Parse_SpecialSpan: unknown spanning definition character "' + c + '"');
    end;
end;


{ ---------------------------------------------------------------------------- }
{ skip leading white space }

procedure TSGrep.Parse_Whitespace;
begin
while ((fPBead <= fPLength) and (fPSource[fPBead] in [#1..#32])) do fPBead := fPBead + 1;
end;


{ ---------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }
{ OPLIST now has all parsed opcodes in left -> right order }
{ make execution stack out of them }

procedure TSGrep.Sort_Begin;
begin

// first, make a flat list where each opcode points before and after itself

Sort_Flat;

// separate sub-expressions into single tree branches

Sort_SubExpr;

// take care of user-define AND and OR

Sort_Branches;

// repeat codes effect only the next immediate op

Sort_Repeat;
end;


{ ---------------------------------------------------------------------------- }
{ define the branches for AND and OR }
{ note: an AND put in by the user is essentially a no-op; take it out }
{ an OR applies only to the token immediately before and after it }

procedure TSGrep.Sort_Branches;
var
    i,n:        integer;
    nn:         integer;
    nl,nr:      integer;
    nh,nt:      integer;
    pn:         PSGrepOpcode;
    pl,pr:      PSGrepOpcode;
    ph,pt:      PSGrepOpcode;
begin

// nothing touched yet

Sort_UnTouched;

// combine consecutive literals

nn := 0;
while ((nn >= 0) and (nn < fOpList.Count) and (fPSuccess)) do
    begin

// get record pointer

    pn := fOpList.Items[nn];

// already been here?

    if (pn^.rTouched) then FNote('Sort_Branches: circular reference at ' + IntToStr(nn));

// is this a branch not yet taken?

    if ((pn^.rValid)
    and ((pn^.rOpcode = kSGrepAnd) or (pn^.rOpcode = kSGrepOr))
    and ((pn^.rLeft < 0) and (pn^.rRight < 0))) then
        begin
        nl := pn^.rPrev;
        nr := pn^.rNext;

// valid refs?

        if ((nl < 0) or (nr < 0)) then
            begin
            FNote('Sort_Branches: invalid branch at '+IntToStr(nn));
            nl := nn;
            pl := pn;
            nr := nn;
            pr := pn;
            end
        else
            begin
            pl := fOpList.Items[nl];
            pr := fOpList.Items[nr];
            end;

// branch around left and right terms

        Sort_Link(pl^.rPrev, nn);
        Sort_Link(nn, pr^.rNext);

// link in the left and right terms

        Sort_LinkLeft(nn, nl, nl);
        Sort_LinkRight(nn, nr, nr);

// ok, start over

        Sort_UnTouched;
        nn := 0;
        end

// get next record

    else
        begin
        pn^.rTouched := true;
        nn := nn + 1;
        end; // if
    end; // while
end;

{ ---------------------------------------------------------------------------- }
{ find the tail of a tree headed by CUR }

function TSGrep.Sort_FindTail(cur: integer): integer;
var
    t:      boolean;
    i,n:    integer;
    p:      PSGrepOpcode;
begin
n := cur;
if (n >= 0) then
    begin
    repeat
        p := fOpList.Items[n];
        t := ((p^.rOpCode = kSGrepTail) or (p^.rOpCode = kSGrepSubTail));
        n := p^.rNext;
    until ((t) or (n < 0));

    if (t) then n := p^.rTag;
    end;

Sort_FindTail := n;
end;

{ ---------------------------------------------------------------------------- }
{ make a simple prev <-> next list }

procedure TSGrep.Sort_Flat;
var
    i,n:        integer;
    n1:         integer;
    p1:         PSGrepOpcode;
begin

// first, make a flat list where each opcode points before and after itself

for i := 0 to (fOpList.Count - 1) do
    begin
    p1 := fOpList.Items[i];
    p1^.rNext  := i + 1;
    p1^.rPrev  := i - 1;
    end;

i := 0;
p1 := fOpList.Items[i];
p1^.rPrev := -1;

i := fOpList.Count - 1;
p1 := fOpList.Items[i];
p1^.rNext := -1;
end;


{ ---------------------------------------------------------------------------- }
{ reterun NEXT pointer; do some error checking }

function TSGrep.Sort_GetNext(cur: integer): integer;
var
    i,n:    integer;
    p:      PSGrepOpcode;
begin
n := -1;
if (cur >= 0) then
    begin
    p := fOpList.Items[cur];
    n := p^.rNext;
    end;

Sort_GetNext := n;
end;

{ ---------------------------------------------------------------------------- }
{ reterun PREV pointer; do some error checking }

function TSGrep.Sort_GetPrev(cur: integer): integer;
var
    i,n:    integer;
    p:      PSGrepOpcode;
begin
n := -1;
if (cur >= 0) then
    begin
    p := fOpList.Items[cur];
    n := p^.rPrev;
    end;

Sort_GetPrev := n;
end;

{ ---------------------------------------------------------------------------- }
{ mark the opcode as no longer needed }

procedure TSGrep.Sort_InValid(idx: integer);
var
    p:      PSGrepOpcode;
begin
p := fOpList.Items[idx];
p^.rValid := false;
p^.rPrev  := -2;
p^.rNext  := -2;
p^.rLeft  := -2;
p^.rRight := -2;
end;


{ ---------------------------------------------------------------------------- }
{ link two opcodes together PREV <-> NEXT }

procedure TSGrep.Sort_Link(prev, next: integer);
var
    p:      PSGrepOpcode;
begin
if (prev >= 0) then
    begin
    p := fOpList.Items[prev];
    p^.rNext := next;
    end;

if (next >= 0) then
    begin
    p := fOpList.Items[next];
    p^.rPrev := prev;
    end;
end;

{ ---------------------------------------------------------------------------- }
{ link a branch to the LEFT side of a decision }
{ opd = decision opcode }
{ opf = first of list of opcodes }
{ opl = last of list of opcodes }

procedure TSGrep.Sort_LinkLeft(opd: integer; opf: integer; opl: integer);
var
    nh,nt:  integer;
    ph,pt:  PSGrepOpcode;
    pd:     PSGrepOpcode;
begin

// make a head-of-branch

ph := Alloc_Opcode;
nh := ph^.rTag;
ph^.rOpcode := kSGrepLeft;

Sort_Link(nh, opf);

// make a tail-of-branch

pt := Alloc_Opcode;
nt := pt^.rTag;
pt^.rOpcode := kSGrepSubTail;
pt^.rIvalue1 := opd;

Sort_Link(opl, nt);

// link to left decision

pd := fOpList.Items[opd];
pd^.rLeft := nh;
ph^.rPrev := opd;
end;


{ ---------------------------------------------------------------------------- }
{ link a branch to the RIGHT side of a decision }
{ opd = decision opcode }
{ opf = first of list of opcodes }
{ opl = last of list of opcodes }

procedure TSGrep.Sort_LinkRight(opd: integer; opf: integer; opl: integer);
var
    nh,nt:  integer;
    ph,pt:  PSGrepOpcode;
    pd:     PSGrepOpcode;
begin

// make a head-of-branch

ph := Alloc_Opcode;
nh := ph^.rTag;
ph^.rOpcode := kSGrepRight;

Sort_Link(nh, opf);

// make a tail-of-branch

pt := Alloc_Opcode;
nt := pt^.rTag;
pt^.rOpcode := kSGrepSubTail;
pt^.rIvalue1 := opd;

Sort_Link(opl, nt);

// link to left decision

pd := fOpList.Items[opd];
pd^.rRight := nh;
ph^.rPrev  := opd;
end;


{ ---------------------------------------------------------------------------- }
{ the repeat factor is a special type of branch }
{ the REPEAT operator applies only to the next opcode }
{ the next opcode is stored in the RIGHT branch, }
{ the rest of the current tree is stored in the LEFT branch }
{ the terminating TAIL or SUBTAIL is the NEXT opcode }


procedure TSGrep.Sort_Repeat;
var
    i,n:        integer;
    nn:         integer;
    nl,nr:      integer;
    ne,nt:      integer;
    n1,n2:      integer;
    pn:         PSGrepOpcode;
    pl,pr:      PSGrepOpcode;
    p1,p2:      PSGrepOpcode;
begin

// nothing touched yet

Sort_UnTouched;

// check each record in list

nn := 0;
while ((nn >= 0) and (nn < fOpList.Count) and (fPSuccess)) do
    begin

// get record pointer

    pn := fOpList.Items[nn];

// already been here?

    if (pn^.rTouched) then FNote('Sort_Repeat: circular reference at ' + IntToStr(nn));

// is this a branch not yet taken?

    if ((pn^.rValid)
    and (pn^.rOpcode = kSGrepRepeat)
    and (pn^.rLeft < 0) and (pn^.rRight < 0)) then
        begin

// get the next opcode; this becomes the right branch
// there must be at least 2 ops following a REPEAT:
// first op: the operator that will be repeated
// second op: the rest of the tree OR a tail marker
// (since it is not legal to REPEAT the tail marker)

        nr := Sort_GetNext(nn);         // right branch op
        nl := Sort_GetNext(nr);         // left branch tree head
        nt := Sort_FindTail(nl);        // next tail marker
        ne := Sort_GetPrev(nt);         // end of left branch (not a tail)

// simple error checking

        if ((nr < 0) or (nl < 0) or (nt < 0) or (ne < 0)) then
            begin
            FNote('Sort_Repeat: invalid repeat code at ' + IntToStr(nn));
            nr := nn;
            nl := nn;
            nt := nn;
            ne := nn;
            end;

// save next op as right branch

        Sort_LinkRight(nn, nr, nr);

// if the left branch consists only of a tail, don't bother with a branch

        pl := fOpList.Items[nl];
        if ((pl^.rOpCode = kSGrepTail) or (pl^.rOpCode = kSGrepSubTail)) then
            begin
            Sort_Link(nn, nl);
            end

// otherwise, save the tail as NEXT and the tree NL -> NE as left branch

        else
            begin
            Sort_LinkLeft(nn, nl, ne);
            Sort_Link(nn, nt);
            end;

// ok, start over

        Sort_UnTouched;
        nn := 0;
        end

// get next record

    else
        begin
        pn^.rTouched := true;
        nn := nn + 1;
        end; // if
    end; // while
end;


{ ---------------------------------------------------------------------------- }
{ parse sub-expressions (parentheses) into a single branch }

procedure TSGrep.Sort_SubExpr;
var
    i,n:        integer;
    nn,nr:      integer;
    pn,pr:      PSGrepOpcode;
    n1,n2,nx:   integer;
    p1,p2,px:   PSGrepOpcode;
begin

// nothing touched yet

Sort_UnTouched;

// things surrounded by parenthese are a separate branch

n1 := -1;
n2 := -1;
nn := 0;
while ((nn >= 0) and (fPSuccess)) do
    begin

// get next opcode

    pn := fOpList.Items[nn];

// already been here?

    if (pn^.rTouched) then FNote('Sort_SubExpr: circular reference at ' + IntToStr(nn));

// if open or close parens

    if      (pn^.rOpcode = kSGrepSubBeg) then n1 := nn
    else if (pn^.rOpCode = kSGrepSubEnd) then n2 := nn;

// a matched set of parens?

    if ((n1 >= 0) and (n2 >= 0) and (n2 > n1)) then
        begin
        p1 := fOpList.Items[n1];
        p2 := fOpList.Items[n2];

// make a new decision op

        px := Alloc_Opcode;
        nx := px^.rTag;
        px^.rOpcode := kSGrepAnd;

// link it in place of the sub-expression

        Sort_Link(p1^.rPrev, nx);
        Sort_Link(nx, p2^.rNext);

// trim leading "(" and trailing ")"

        nn := n1;
        n1 := p1^.rNext;
        p1 := fOpList.Items[n1];
        Sort_InValid(nn);

        nn := n2;
        n2 := p2^.rPrev;
        p2 := fOpList.Items[n2];
        Sort_InValid(nn);

// link this branch to right-hand decision

        Sort_LinkRight(nx, n1, n2);

// start again from top of list

        Sort_UnTouched;
        n1 := -1;
        n2 := -1;
        nn := 0;
        end

// or just a generic op

    else
        begin
        pn^.rTouched := true;
        nn := pn^.rNext;
        end;

// valid pointer?

    if (nn >= fOpList.Count) then FNote('Sort_SubExpr: invalid op index '+IntToStr(nn));
    end;    // while

if ((n1 >= 0) or (n2 >= 0)) then FNote('Sort_SubExpr: unmatched parentheses');
end;


{ ---------------------------------------------------------------------------- }
{ mark all ops as un-touched yet }

procedure TSGrep.Sort_UnTouched;
var
    i:      integer;
    p:      PSGrepOpcode;
begin
// nothing touched yet

for i := 0 to (fOpList.Count - 1) do
    begin
    p := fOpList.Items[i];
    p^.rTouched := false;
    end;
end;



{ ---------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }
{ execute the entire pattern tree }

procedure TSGrep.Eval_Begin;
var
    i,n:    integer;
    head:   integer;
    p:      PSGrepOpcode;
begin

// if could not parse correctly, then cannot eval correctly

if (fPSuccess) then
    begin

// find beginning of pattern tree

    head := -1;
    for i := 0 to (fOpList.Count - 1) do
        begin
        p := fOpList.Items[i];
        if (p^.rOpCode = kSGrepHead) then head := i;
        end;

// eval pattern starting at each point in source string until a match

//    EAnchor := 0;
    repeat
        fESuccess   := true;
        fESpanset   := [];
        fESpanIndex := -1;
        fEBead      := fEAnchor;

        Eval_List(head);

        if (not fESuccess) then fEAnchor := fEAnchor + 1;
    until ((fESuccess) or (fEAnchor > fELength));

// find a match?
// remember, anchor and bead are the space immediately before a character

    if (fESuccess) then
        begin
        i := fEAnchor + 1;
        n := fEBead - fEAnchor;
        fESubString := copy(fESource, i, n);
        end;
    end

// did not parse?

else
    begin
    fESuccess := false;
    fESpanset := [];
    fESpanIndex := -1;
    fEAnchor := -1;
    fEBead := 0;
    end;
end;


{ ---------------------------------------------------------------------------- }
{ execute a linear list of opcodes }

procedure TSGrep.Eval_List(Top: integer);
var
    i,n:    integer;
    cur:    integer;                // current op tag or index
    next:   integer;                // next op to eval
    noop:   integer;                // signals a no-op
    pp:     PSGrepOpcode;               // working record
begin
cur := Top;
while ((cur >= 0) and (fESuccess)) do
    begin
    pp := fOpList.Items[cur];
    next := pp^.rNext;
    noop := 1;

    if (not pp^.rValid) then
        begin
        fESuccess := false;
        end;

    case pp^.rOpCode of
        kSGrepHead:        noop :=  0;
        kSGrepTail:        next := -1;
        kSGrepRight:       noop :=  0;
        kSGrepLeft:        noop :=  0;
        kSGrepSubTail:     next := -1;
        kSGrepIgnore:      noop :=  0;

        kSGrepAnyChar:     Eval_AnyChar(cur);
        kSGrepLiteral:     Eval_Literal(cur);
        kSGrepSpan:        Eval_Span(cur);
        kSGrepPosition:    Eval_Position(cur);
        kSGrepRepeat:      Eval_Repeat(cur);
        kSGrepAnd:         Eval_And(cur);
        kSGrepOr:          Eval_Or(cur);

        kSGrepOutput:      Eval_Output(cur);

        else            FNote('Eval_List: unknown opcode: ' + pp^.rOpCode);
        end;

    cur := next;
    end;
end;

{ ---------------------------------------------------------------------------- }
{ evaluate AnyChar - advance beady by 1 character }
{ ESuccess is assumed to be TRUE }

procedure TSGrep.Eval_AnyChar(Index: integer);
begin
if (fEBead < fELength) then fEBead := fEBead + 1
else                        fESuccess := false;
end;


{ ---------------------------------------------------------------------------- }
{ evaluate Literal character match }
{ ESuccess is assumed to be TRUE }

procedure TSGrep.Eval_Literal(Index: integer);
var
    i,n:    integer;
    p:      PSGrepOpcode;
begin
p := fOpList.Items[Index];

for i := 1 to length(p^.rSvalue) do
    begin
    if      (fEBead >= fELength)                  then fESuccess := false
    else if (fESource[fEBead+1] <> p^.rSValue[i]) then fESuccess := false
    else                                               fEBead := fEBead + 1;
    end;
end;



{ ---------------------------------------------------------------------------- }
{ evaluate a span of characters }
{ ESuccess is assumed to be TRUE }

procedure TSGrep.Eval_Span(Index: integer);
var
    i,n:    integer;
    s:      string;
    p:      PSGrepOpcode;
begin
p := fOpList.Items[Index];

if (Index <> fESpanIndex) then
    begin
    fESpanset := [];
    fESpanIndex := Index;
    s := p^.rSValue;

    if (p^.rNegate) then
        begin
        for i := 1 to 255 do fESpanset := fESpanset + [chr(i)];
        for i := 1 to length(s) do fESpanset := fESPanset - [s[i]];
        end
    else
        begin
        for i := 1 to length(s) do fESpanset := fESPanset + [s[i]];
        end;
    end;

// check if next char is in the span set

if      (fEBead >= fELength)                    then fESuccess := false
else if (not (fESource[fEBead+1] in fESpanset)) then fESuccess := false
else                                                 fEBead := fEBead + 1;
end;


{ ---------------------------------------------------------------------------- }
{ check the position of the bead }
{ ESuccess is assumed to be TRUE }

procedure TSGrep.Eval_Position(Index: integer);
var
    i,n:    integer;
    p:      PSGrepOpcode;
begin
p := fOpList.Items[Index];

if (p^.rNegate) then n := (fELength + 1) - p^.rIvalue1
else                 n := p^.rIValue1;

if (fEBead <> n) then fESuccess := false;
end;

{ ---------------------------------------------------------------------------- }
{ repeat an op a number of times }
{ repeat right-hand branch min to max times until both right and left }
{ branches succeeed, or until we run out of repeat options }
{ ESuccess is assumed to be TRUE }

procedure TSGrep.Eval_Repeat(Index: integer);
var
    ok:     boolean;
    a,b:    integer;
    i,n:    integer;
    p:      PSGrepOpcode;
begin
p := fOpList.Items[Index];

// do the minimum required

n := 0;
while ((n < p^.rIvalue1) and (fESuccess)) do
    begin
    n := n + 1;
    Eval_List(p^.rRight);
    end;

// repeat up to the maximum until left side succeeds also

repeat

// save locators in case we need righ branch again

    a := fEAnchor;
    b := fEBead;

// eval left branch if right branch succeeded

    if (fESuccess) then Eval_List(p^.rLeft);

// our progress

    if (fESuccess) then
        begin
        ok := true;
        end
    else if (n >= p^.rIValue2) then
        begin
        ok := true;
        fESuccess := false;
        end
    else
        begin
        n := n + 1;
        ok := false;
        fESuccess := true;
        fEAnchor := a;
        fEBead := b;
        Eval_List(p^.rRight);
        if (not fESuccess) then ok := true;
        end;
until (ok);
end;

{ ---------------------------------------------------------------------------- }
{ evaluate left and right sides of a branch }
{ ESuccess is assumed to be TRUE }

procedure TSGrep.Eval_And(Index: integer);
var
    i,n:    integer;
    p:      PSGrepOpcode;
begin
p := fOpList.Items[Index];

// always do the left branch; do the right branch only if the left succeeds

Eval_List(p^.rLeft);
if (fESuccess) then Eval_List(p^.rRight);
end;

{ ---------------------------------------------------------------------------- }
{ evaluate either left or right sides of a branch }
{ ESuccess is assumed to be TRUE }

procedure TSGrep.Eval_Or(Index: integer);
var
    i,n:    integer;
    a,b:    integer;
    p:      PSGrepOpcode;
begin
p := fOpList.Items[Index];

// always do the left branch; do the right branch only if the left fails

a := fEAnchor;
b := fEBead;
Eval_List(p^.rLeft);

if (not fESuccess) then
    begin
    fEAnchor := a;
    fEBead := b;
    fESuccess := true;
    Eval_List(p^.rRight);
    end;
end;


{ ---------------------------------------------------------------------------- }
{ write requested output to the OnWrite event }

procedure TSGrep.Eval_Output(Index: integer);
var
    d:      char;
    i,n:    integer;
    p:      PSGrepOpcode;
    s:      string;
begin
p := fOpList.Items[Index];

i := fEAnchor + 1;
n := fEBead - fEAnchor;
s := '';

if (length(p^.rSValue) = 0) then d := ' '
else                             d := p^.rSValue[1];

case d of
    'A':    s := IntToStr(fEAnchor);
    'B':    s := IntToStr(fEBead);
    'S':    s := copy(fESource, i, n);
    'T':    s := copy(fESource, 1, fEBead);
    else    FNote('Eval_Output: invalid output directive "' + d + '"');
    end;

if (assigned(fOnWrite)) then fOnWrite(Self, s);
end;

{ ---------------------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }


function TSGrep.MatchFirst(S: string): integer;
var
    i,n:    integer;
begin
Set_Source(S);                  // define the source to search
if (not fPReady) then           // pattern parsed yet?
    begin
    Parse_Begin;                // parse pattern into tokens
    Sort_Begin;                 // sort tokens into tree
    fPReady := true;            // OK, it's ready
    end;

Eval_Begin;                     // find first occurance

if (fESuccess) then n := fEAnchor
else                n := -1;

// done

MatchFirst := n;
end;

{ ---------------------------------------------------------------------------- }

function TSGrep.MatchNext: integer;
var
    i,n:    integer;
begin
fEanchor := fEBead + 1;
Eval_Begin;
if (fESuccess) then n := fEAnchor
else                n := -1;

MatchNext := n;
end;

{ ---------------------------------------------------------------------------- }

function TSGrep.Execute(P,S: string): boolean;
begin
Set_Pattern(P);
Set_Source(S);
Parse_Begin;
Sort_Begin;
Eval_Begin;

Execute := fESuccess;
end;

end.
