
unit SMUtils;

interface
uses SysUtils,Windows,Dialogs, forms,messages, {grids,} graphics,
 classes,stdctrls,extctrls,controls,ShellAPI, TypInfo;

const
 NullDate :double =-999999999;
 nothing: string='nothing';

 sDBF_DEC_SEP= '.';

type
  TNCRStringArray = array of string;
  TNCRIntegerArray= array of integer;
  TNCRVariantArray = array of variant;

  charset=set of char;

function IsAlphaNumeric(Ch : Char) : Boolean;
function IsNumericTest (Ch : Char; TestCh : Char) : Boolean;
function IsNumeric (Ch : Char) : Boolean;
Function GetVarName(const line:String;LinePos:integer) : String;

function GetLastErrorMsg:string;

// if condition then return exp1 else return exp2
function smif(condition:boolean; exp1, exp2:string):string;overload
function smif(condition:boolean; exp1, exp2:longint):longint;overload
function smif(condition:boolean; exp1, exp2:double):double;overload
function smif(condition:boolean; exp1, exp2:pointer):pointer;overload
function smif(condition:boolean; exp1, exp2:TObject):TObject;overload


function min(x,y:integer):integer;
function max(x,y:integer):integer;

function showmsg(msg:String):boolean;

Function Dup (const S : String; const Count : Integer) : String; overload                    // Pretty fast
Function Dup (const Ch : Char; const Count : Integer) : String; overload;                    // Blazing

Function PadRight (const S : String; const PadCh : Char; const Length : Integer;
         const Cut : Boolean = False) : String;


procedure SplitFirst(s:string; operators:charset;
var s1,s2:string; var operator:char);

//File routines
function ExtractFileNameNoExt(const FileName: string): string;
function ExtractFileNameNoPath(const FileName: string):string;

function CanReadWriteFile(const FileName:String):integer;

function DirExists(Name: string): Boolean;
function AdjustDirName(const fn:string):string;


function  SMGetTempfileName(const aExt: String;TempPath:string=''): String;

function AdjustFileExt(const fn:string;const ext:string):String;

function GetSaveFileName(var pfilename:string; const pFilter,pExt:string):boolean;
function GetOpenFileName(var pfilename: string; const pFilter:string):boolean;


function ValidFileName(const FileName: string): Boolean;

implementation

uses UCmdProcessor, AnsDatatypes;

type
  TTextLayout = (tlTop, tlCenter, tlBottom);
const
  BoolChars: array[Boolean] of Char = ('F', 'T');


function smif(condition:boolean; exp1, exp2:string):string;
begin
if condition then
   result:=exp1
else
   result:=exp2;
end;

function smif(condition:boolean; exp1, exp2:longint):longint;
begin
if condition then
   result:=exp1
else
   result:=exp2;
end;

function smif(condition:boolean; exp1, exp2:double):double;
begin
if condition then
   result:=exp1
else
   result:=exp2;
end;

function smif(condition:boolean; exp1, exp2:pointer):pointer;
begin
if condition then
   result:=exp1
else
   result:=exp2;
end;

function smif(condition:boolean; exp1, exp2:TObject):TObject;
begin
if condition then
   result:=exp1
else
   result:=exp2;
end;


function min(x,y:integer):integer;
begin
    if x < y then
       result:=x
    else
       result:=y;
end;

function max(x,y:integer):integer;
begin
    if x > y then
       result:=x
    else
       result:=y;
end;

function showmsg(msg:String):boolean;
begin
application.messagebox(pchar(msg),pchar(application.Title),
        MB_OK+MB_ICONINFORMATION)
end;

{                                                                              }
{ Dup                                                                          }
{                                                                              }
Function Dup (const S : String; const Count : Integer) : String;
var I, L : Integer;
  Begin
    L := Length (S);
    SetLength (Result, Count * L);
    For I := 0 to Count - 1 do
      Move (S [1], Result [I * L + 1], L);
  End;

Function Dup (const Ch : Char; const Count : Integer) : String;
  Begin
    if count<1 then exit;
    SetLength (Result, Count);
    FillChar (Result [1], Count, Ord (Ch));
  End;

Function PadRight (const S : String; const PadCh : Char; const Length : Integer;
         const Cut : Boolean = False) : String;
  Begin
    Result := S + Dup (PadCh, Length - System.Length (S));
    if Cut then
      SetLength (Result, Length);
  End;


  
function AdjustDirName(const fn:string):string;
begin
  result:=fn;
  if length(result)=0 then exit;
  if result[length(result)]<>'\' then
     result:=result+'\';
end;


procedure SplitFirst(s:string; operators:charset;
var s1,s2:string; var operator:char);
    {
    Splits s into s1 (before first operator), operator (the operator)
    and s2 (after operator). Operators within inner parentheses are skipped.
    charset operators is the set of operators allowed to split.
    If no splitting operator is found, s1 becomes s, s2 becomes empty and
    operator becomes #0
    }
var p,l:word;
begin
l:=length(s);
p:=0;
if l>0 then
    repeat
    inc(p);
    until (p>l) or (s[p] in operators);
if (p<=l) and (l>0) then operator:=s[p] else operator:=#0;
    if p>0 then s1:=copy(s,1,p-1) else s1:='';
    s2:=copy(s,p+1,l);
end;


function GetLastErrorMsg:string;
begin
 Result:=SysErrorMessage(GetLastError)
end;

function GetStringPrecChar(const str: string; ch : char): string;
var
  I: Integer;
begin
  I := AnsiPos(ch, str);
  if I = 0 then
    Result := str else
    Result := Copy(str, 1, I - 1);
end;

function GetSaveFileName(var pfilename: string; const pFilter,pExt:string):boolean;
var
sd : TSaveDialog;
begin
result:=false;
sd := TSaveDialog.Create(application) ;
try
   if pExt='' then
      SD.DefaultExt:='txt'
   else
      SD.DefaultExt:=pExt;
   sd.FileName:=pFilename;
   sd.Filter:=pFilter;
   if sd.Filter='' then SD.Filter:='Text files (*.txt)|*.TXT';
//   InitialDir:=
   SD.Options:=[{ofOverwritePrompt,}ofNoChangeDir	, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
   SD.Title:='Save as...';
   result:= sd.execute;
   if result then pfilename:=sd.Filename;
finally
 sd.free;
end;

end;

function GetOpenFileName(var pfilename: string; const pFilter:string):boolean;
var
  sd : TOpenDialog;
  opt: TEpiOption;
begin
  result:=false;
  sd := TOpenDialog.Create(application) ;
  try
    sd.FileName:=pFilename;
    sd.Filter:=pFilter;
    if sd.Filter='' then
      SD.Filter:='Text files (*.txt)|*.TXT|All files|*.*'
    else
      SD.Filter:=SD.Filter+'|All files|*.*';
    SD.Options:=[ofHideReadOnly,ofNoChangeDir, ofPathMustExist, ofFileMustExist,ofEnableSizing];
    SD.Title:='Open...';
    if Dm.GetOptionValue('DEFAULT CURRENT DIR', opt) and (AnsiUpperCase(opt.Value) = 'ON') then
      sd.InitialDir := GetCurrentDir
    else
      sd.InitialDir := '';
    result:= sd.execute;
    if result then pfilename:=sd.Filename;
  finally
   sd.free;
  end;
end;


function ExtractFileNameNoExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
  I := LastDelimiter('.', Result);
  if I > 0 then
    result:=copy(result,1,I-1);
end;

function ExtractFileNameNoPath(const FileName: string):string;
var
  I: Integer;
begin
  I := LastDelimiter('\', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function SMGetTempfileName(const aExt: String;TempPath:string=''): String;
var
  Buffer: array[0..1023] of Char;
  aFile : String;
begin
  if TempPath<>'' then
    strCopy(buffer,pchar(tempPath))
  else
    GetTempPath(Sizeof(Buffer)-1,Buffer);
  GetTempFileName(Buffer,'TMP',0,Buffer);
  SetString(aFile, Buffer, StrLen(Buffer));
  Result:=ChangeFileExt(aFile,aExt);
  RenameFile(aFile,Result);
end;


function ValidFileName(const FileName: string): Boolean;
  function HasAny(const Str, Substr: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(Substr) do begin
      if Pos(Substr[I], Str) > 0 then begin
        Result := True;
        Break;
      end;
    end;
  end;
begin
  Result := (FileName <> '') and (not HasAny(FileName, '<>"[]|'));
  if Result then Result := Pos('\', ExtractFileName(FileName)) = 0;
end;



function DirExists(Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;


function CanReadWriteFile(const FileName:String):Integer;
var
    f        : TextFile;
begin
result:=0;
try
try
     AssignFile( f, filename );
//     FileMode := 2; //Read-Write
     append( f );
except
   on E: EinOutError do result:=E.ErrorCode;
end;
finally
try
    closefile(f);
except
end;
end;
end;


function StrToFloatDef(const S: string;def:Extended): Extended;
begin
  if  not TextToFloat(PChar(S), Result, fvExtended) then
    result:=def;
end;



function IsAlphaNumeric(Ch : Char) : Boolean;
begin
 result:=ch in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;

function IsNumeric (Ch : Char) : Boolean;
begin
   Result := (Ch in ['0'..'9'])
 end ;

function IsNumericTest (Ch : Char; TestCh : Char) : Boolean;
begin
   Result := (Ch = TestCh) or (Ch in ['0'..'9'])
 end ;


Function GetVarName(const line:String;LinePos:integer) : String;
var
   alen :integer;
Begin
  result := '';
  alen :=length(line);
  if alen=0 then exit;
  While (Line[LinePos] = ' ') And (LinePos <= Length (Line)) Do
          Inc (LinePos);
  If Line[LinePos] in ['A'..'Z', 'a'..'z', '_'] then
    Repeat
      result := result + Line[LinePos];
      Inc (LinePos);
    Until (Not (Line[LinePos] in ['A'..'Z', 'a'..'z', '0'..'9', '_'])) or
            (LinePos > aLen);
End;

function AdjustFileExt(const fn:string;const ext:string):String;
begin
  Result:=trim(fn);
  if Result='' then exit;
  if extractfileext(Result)='' then
      Result:=fn+ext;
end;


end.














