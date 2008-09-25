unit AAPasTok;

interface

uses
  SysUtils, classes,AAChrStm, AAHshLnP;

const
DefaultStringDelim ='"';   //#39;

//token identifiers
opInvalidToken=0;
opExp =1;
opMult=2;
opDivide=3;
opPlus=4;
opMinus=5;
opEq=6;
opNEq=7;
opLT=8;
opGT=9;
opLTE=10;
opGTE=11;
opIdentifier=12;    {..identifier}
opString=13;        {..string or character constant}
opHexNumber=14;     {..number in hex, starts with $}
opNumber=15;        {..sequence of digits, maybe with radix point}
opComment=16;        {..comment, any type}
opComma=17;          {..comma: ,}
opSemicolon=18;      {..semicolon: ;}
opColon=19;         {..colon: :}
opPeriod=20;        {..period: .}
opRange=21;         {..range: ..}
opAssign=22;        {..assignment: :=}
opOpenParen=23;     {..open parenthesis: (}
opCloseParen=24;    {..close parenthesis: )}
opOpenBracket=25;   {..open bracket: [}
opCloseBracket=26;  {..close bracket: ]}
opCaret=27;         {..caret: ^}
opHash=28;          {..hash: #}
opAt=29;            {..ampersand: @}
opQuery=30;         { ? question mark}
opDoDos=31;         {..exclamation: ! (for doing Dos-commands without typing DOS first)}
opNot=32;
opDiv=33;
opMod=34;
opAnd=35;
opShl=36;
opShr=37;
opOr=38;
opXor=39;
opParserdefined=40;
opLineRead=96;
opOperator=97;
opKeyword=98;
opEndOfline=99;
opEndOfFile=100;

opUserDefined=101;

Type

 TSMTokenType = 0..255; //255 keywords should be enough even for Arabic

 TSMTokenTypeSet = set of TSMTokenType;


const
  SMOperators:TSMTokenTypeSet=[opNot,opExp,opMult, opDivide, opDiv, opMod, opAnd, opShl, opShr,
                opPlus, opMinus, opOr, opXor,opEq, opNEq, opLT, opGT, opLTE, opGTE];

  TokensList: array[opNot..opXor] of String =
              ( 'NOT','DIV', 'MOD', 'AND', 'SHL', 'SHR','OR', 'XOR');


type

  TSMToken=record
   TokenSubType :TSMTokenType;
   TokenType : TSMTokenType;
   TokenGroup : TSMTokenType;
   Token     : string;
   line,col,AbsPos  : word;
  end;

  TParserException = class(exception);
  TOnParseError = procedure(const msg:string;Token:TSMToken;var Handled:boolean) of object;

type
  TSMBaseTokenizer = class
    private
    FInStrm   : TaaInCharStream;
    fStream   : TStream;
    FKeywords : TaaHashTableLinear;
    fCurrentToken,fPrevToken,fLookAheadToken :TSMToken;
    fStringDelim: char;
    StreamCreated: boolean;
    fLineNumber: integer;
    FTokenName: string;
    fdequoteStrings: boolean;
    fOnError: TOnParseError;
    lastCh:char;
    FStringToParse: string;
    function GetEndOfExp: boolean;
    function GetExprDelimiters: TSMTokenTypeSet;
    procedure SetExprDelimiters(const Value: TSMTokenTypeSet);
    function GetCurrentToken: TSMToken;
    function GetLookAheadToken: TSMToken;
    function NextValidChar(var aToken: TSMToken): char;
    procedure SetCurrentToken(const Value: TSMToken);
//    procedure SetStringToParse(const Value: string);
   protected
     function GetNextToken: TSMToken;virtual;
     procedure ReadNumber(var aToken: string);
     procedure ReadHexNumber(var aToken: string);
     procedure ReadIdentifier(var aToken: string);
     procedure ReadString(var aToken: string);
     procedure ReadBraceComment(var aToken: string);
     procedure AAGetToken(var aToken: TSMToken);
     procedure ReadParenComment(var aToken: string);
     procedure ReadSlashComment(var aToken: string);
     procedure Error(const msg: string='');virtual;
     procedure ppInitKeywords;virtual;
     procedure GetToken(var aToken: TSMToken);virtual;
    public
      constructor Create(aInStm : TaaInCharStream);
      constructor CreateStream(aStream: TStream);
      constructor CreateFile(FileName :string);
      constructor CreateString(const ParseString: string);
      destructor Destroy; override;
      procedure Initialize; virtual;
      procedure GetTokenClass(var aToken: TSMToken);virtual;
      function IsOperator(aTokenType: TSMTokenType): boolean;virtual;
      function GetTokenName(index:TSMTokenType): string; virtual;
      procedure PutBackLastToken(aToken: TSMToken);
      function ReadToEOL :TSMToken;
      function ReadToChar(pTerminator: char): TSMToken;
      function GetRestOfLine:string;
      property EndOfExp:boolean read GetEndOfExp;
      property ExprDelimiters:TSMTokenTypeSet read GetExprDelimiters write SetExprDelimiters;
      property LookAheadToken:TSMToken read GetLookAheadToken;
      property NextToken:TSMToken read GetNextToken;
      property PrevToken :TSMToken read fPrevToken;
      property CurrentToken:TSMToken read GetCurrentToken write SetCurrentToken;
      property StringDelim :char read fStringDelim write fStringDelim;
      property InStream   : TaaInCharStream  read FInStrm;
      property Keywords : TaaHashTableLinear read FKeywords;
      property LineNumber:integer read fLineNumber;
      property TokenName[index: TSMTokenType]: string read GetTokenName;
      property dequoteStrings: boolean read fdequoteStrings write fdequoteStrings;
      property OnError :TOnParseError read fOnError write fOnError;
      property StringToParse: string read FStringToParse;// write SetStringToParse;
  end;

TSMTokenizerClass=class of TSMBaseTokenizer;

//procedure AAGetToken(aInStm     : TaaInCharStream;var aToken : TSMToken);

implementation



{===TSMBaseTokenizer==================================================}
const

AxTokensList: array[opInvalidToken..opDoDos] of String =
  ('InvalidToken','Exp','Mult','Divide','Plus','Minus','Eq','NEq','LT','GT','LTE',
  'GTE','Identifier','String','HexNumber','Number','Comment','Comma','Semicolon','Colon',
  'Period','Range','Assign','Open Parenthesis','Close parenthesis',  'Open Bracket',
  'CloseBracket','Caret','Hash','At','Query','Dos');

constructor TSMBaseTokenizer.Create(aInStm : TaaInCharStream);
begin
  {create the ancestor}
  inherited Create;
  {save the stream}
  FInStrm := aInstm;
  Initialize;
  {create the keywords list}
  FKeywords := TaaHashTableLinear.Create(199, AAELFHash);
  ppInitKeywords;
  GetNextToken;
end;

constructor TSMBaseTokenizer.CreateStream(aStream:TStream);
begin
  FInStrm :=nil;
try
   FInStrm := TaaInCharStream.Create(aStream);
   Create(FInStrm);
except
  FInStrm.free;
  raise;
end;
end;

constructor TSMBaseTokenizer.CreateFile(FileName: string);
begin
  fstream:=nil;
try
   fStream := TfileStream.create(Filename,fmOpenRead);
   StreamCreated :=true;
   CreateStream(fStream);
except
  fStream.free;
  raise;
end;
end;

constructor TSMBaseTokenizer.CreateString(const ParseString: string);
begin
  fstream:=nil;
try
   fStringToParse:= ParseString;
   fStream := TStringStream.create(ParseString);
   StreamCreated :=true;
   CreateStream(fStream);
except
  fStream.free;
  raise;
end;
end;


destructor TSMBaseTokenizer.Destroy;
begin
  {destroy the keywords list}
  FInStrm.free;
  if StreamCreated then
  begin
     FStream.Free;
  end;
  FKeywords.Free;
  {destroy the ancestor}
  inherited Destroy;
end;

procedure TSMBaseTokenizer.Initialize;
begin
  fStringDelim :=DefaultStringDelim;
  fLineNumber:=1;
  fDequoteStrings:=false;
end;

{--------}
{===Helper routines==================================================}
procedure TSMBaseTokenizer.ReadNumber(var aToken : string);
var
  Ch : char;
  State : (BeforeDecPt, GotDecPt, AfterDecPt, Finished);
begin
  State := BeforeDecPt;
  while (State <> Finished) do begin
    Ch := FInStrm.GetChar;
    if (Ch = #0) then begin
      State := Finished;
      FInStrm.PutBackChar(Ch);
    end
    else begin
      case State of
        BeforeDecPt :
          begin
            if (Ch = '.') then begin
              State := GotDecPt;
            end
            else if (Ch < '0') or (Ch > '9') then begin
              State := Finished;
              FInStrm.PutBackChar(Ch);
            end
            else
              aToken := aToken + Ch;
          end;
        GotDecPt :
          begin
            if (Ch = '.') then begin
              FInStrm.PutBackChar(Ch);
              FInStrm.PutBackChar(Ch);
              State := Finished;
            end
            else begin
              aToken := aToken + '.';
              aToken := aToken + Ch;
              State := AfterDecPt;
            end;
          end;
        AfterDecPt :
          begin
            if (Ch < '0') or (Ch > '9') then begin
              State := Finished;
              FInStrm.PutBackChar(Ch);
            end
            else
              aToken := aToken + Ch;
          end;
      end;
    end;
  end;
end;
{--------}
procedure TSMBaseTokenizer.ReadHexNumber(var aToken : string);
var
  Ch : char;
  State : (NormalScan, Finished);
begin
  State := NormalScan;
  while (State <> Finished) do begin
    Ch := FInStrm.GetChar;
    if (Ch = #0) then begin
      State := Finished;
      FInStrm.PutBackChar(Ch);
    end
    else begin
      case State of
        NormalScan :
          begin
            if not (Ch in ['A'..'F', 'a'..'f', '0'..'9']) then begin
              State := Finished;
              FInStrm.PutBackChar(Ch);
            end
            else
              aToken := aToken + Ch;
          end;
      end;
    end;
  end;
end;
{--------}
procedure TSMBaseTokenizer.ReadIdentifier(var aToken : string);
var
  Ch : char;
begin
  Ch := FInStrm.GetChar;
  while Ch in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do begin
    aToken := aToken + Ch;
    Ch := FInStrm.GetChar;
  end;
  FInStrm.PutBackchar(Ch);
end;
{--------}
procedure TSMBaseTokenizer.ReadString(var aToken : string);
var
  Ch : char;
begin
  Ch := FInStrm.GetChar;
//  aToken:='';
  while (Ch <> StringDelim) and (Ch <> #0) do begin
    aToken := aToken + Ch;
    Ch := FInStrm.GetChar;
  end;
  if (Ch = StringDelim) then
    aToken := aToken + Ch
  else
    error('malformed string');
//  if (Ch <> StringDelim) then
//    FInStrm.PutBackchar(Ch);
end;
{--------}
procedure TSMBaseTokenizer.ReadBraceComment(var aToken : string);
var
  Ch : char;
begin
  Ch := FInStrm.GetChar;
  while (Ch <> '}') and (Ch <> #0) do begin
    aToken := aToken + Ch;
    Ch := FInStrm.GetChar;
  end;
  if (Ch = '}') then
    aToken := aToken + Ch
  else
    FInStrm.PutBackchar(Ch);
end;
{--------}
procedure TSMBaseTokenizer.ReadSlashComment(var aToken : string);
var
  Ch : char;
begin
  Ch := FInStrm.GetChar;
  while (Ch <> #13) and (Ch <> #0) do begin
    aToken := aToken + Ch;
    Ch := FInStrm.GetChar;
  end;
  FInStrm.PutBackchar(Ch);
end;
{--------}
procedure TSMBaseTokenizer.ReadParenComment(var aToken : string);
var
  Ch : char;
  State : (NormalScan, GotStar, Finished);
begin
  State := NormalScan;
  while (State <> Finished) do begin
    Ch := FInStrm.GetChar;
    if (Ch = #0) then begin
      State := Finished;
      FInStrm.PutBackChar(Ch);
    end
    else begin
      aToken := aToken + Ch;
      case State of
        NormalScan :
          if (Ch = '*') then
            State := GotStar;
        GotStar :
          if (Ch = ')') then
            State := Finished
          else
            State := NormalScan;
      end;
    end;
  end;
end;
{====================================================================}

function TSMBaseTokenizer.NextValidChar(var aToken : TSMToken):char;
begin
  {assume we have an invalid token}
  aToken.TokenGroup := opInvalidToken;
  aToken.TokenType := opInvalidToken;
  aToken.TokenSubType:= opInvalidToken;
  aToken.Token  := '';
  {ignore any whitespace prior to the token}
  Result := FInStrm.GetChar;
  while (Result <> #0) and (Result <> #13) and (Result <= ' ') do
    Result := FInStrm.GetChar;
  {if we've reached end-of-file, exit returning that token type}
  if (Result = #0) then begin
    aToken.TokenType := opEndOfFile;
    Exit;
  end;
  {if we've readched end-of-line, exit returning that token type}
  if (Result = #13) then
  begin
    Result := FInStrm.GetChar;
    if Result=#10 then
    begin
      aToken.TokenType := opEndOfLine;
      inc(fLineNumber);
      lastCh:=#13;
      Exit;
    end
    else
      FInStrm.PutBackChar(Result);
  end;
 aToken.Token := Result;
end;

procedure TSMBaseTokenizer.AAGetToken(var aToken : TSMToken);
var
  Ch : char;
begin
 Ch := NextValidChar(aToken);
 if aToken.TokenType in [opEndofLine,opEndofFile] then
 begin
//   inc(fLineNumber);
//   lastCh:=#13;
   exit;
 end;
// if ProcessToken
 {parse the token based on the current character}
 if ch =StringDelim then
 begin
     aToken.TokenType := opString;
     ReadString( aToken.Token);
 end
 else
 case Ch of
    '#' : aToken.TokenType := opHash;
{    '$' : begin
            aToken.TokenType := opNumber;
            ReadHexNumber(aToken.token);
          end;}
    '(' : begin
            Ch := FInStrm.GetChar;
            if (Ch <> '*') then begin
              FInStrm.PutBackChar(Ch);
              aToken.TokenType := opOpenParen;
//              aToken.Token := '(';
            end
            else begin
              aToken.TokenType := opComment;
              aToken.Token := '(*';
              ReadParenComment( aToken.Token);
            end;
          end;
    ')' :begin
     aToken.TokenType := opCloseParen;
//     aToken.Token := ')';
     end;
    '*' :
    begin
     if lastCh=#13 then
     begin
          aToken.TokenType := opComment;
          aToken.Token := '*';
          ReadSlashComment( aToken.Token);
     end
     else
       aToken.TokenType := opMult;
    end;
    '+' : aToken.TokenType := opPlus;
    ',' : aToken.TokenType := opComma;
    '-' : aToken.TokenType := opMinus;
    '.' : begin
            Ch := FInStrm.GetChar;
            if (Ch = '.') then
              aToken.TokenType := opRange
            else begin
              FInStrm.PutBackChar(Ch);
              aToken.TokenType := opPeriod;
            end;
          end;
    '/' : begin
            Ch := FInStrm.GetChar;
            if (Ch <> '/') then begin
              FInStrm.PutBackChar(Ch);
              aToken.TokenType := opDivide;
            end
            else begin
              aToken.TokenType := opComment;
              aToken.Token := '//';
              ReadSlashComment( aToken.Token);
            end;
          end;
    '0'..'9' :
          begin
            aToken.TokenType := opNumber;
//            aToken.Token := Ch;
            ReadNumber( aToken.Token);
          end;
    ':' : begin
            Ch := FInStrm.GetChar;
            if (Ch = '=') then
              aToken.TokenType := opAssign
            else begin
              FInStrm.PutBackChar(Ch);
              aToken.TokenType := opColon;
            end;
          end;
    ';' : aToken.TokenType := opSemicolon;
    '<' : begin
            Ch := FInStrm.GetChar;
            if (Ch = '=') then
              aToken.TokenType := opLTE
            else if (Ch = '>') then
              aToken.TokenType := opNEq
            else begin
              FInStrm.PutBackChar(Ch);
              aToken.TokenType := opLT;
            end;
          end;
    '=' : aToken.TokenType := opEq;
    '>' : begin
            Ch := FInStrm.GetChar;
            if (Ch = '=') then
              aToken.TokenType := opGTE
            else begin
              FInStrm.PutBackChar(Ch);
              aToken.TokenType := opGT;
            end;
          end;
    '@' : aToken.TokenType := opAt;
    'A'..'Z', 'a'..'z', '_','$' :
          begin
            aToken.TokenType := opIdentifier;
            aToken.Token := Ch;
            ReadIdentifier( aToken.Token);
{            Ch := FInStrm.GetChar;
            if (Ch = '(') then
              aToken.TokenType := opRoutine;
            FInStrm.PutBackchar(Ch);}
          end;
    '[' : aToken.TokenType := opOpenBracket;
    ']' : aToken.TokenType := opCloseBracket;
    '^' : aToken.TokenType := opExp;
    '?' : aToken.TokenType := opQuery;
    '!' : aToken.TokenType := opDoDos;
    '{' : begin
            aToken.TokenType := opComment;
            aToken.Token := '{';
            ReadBraceComment(aToken.Token);
          end;
     else
       aToken.Token := Ch;
  end;
  lastCh:=Ch;
//  if aToken.TokenType = opInvalidToken then
end;
{====================================================================}

procedure TSMBaseTokenizer.Error(const msg:string='');
var
  lmsg:string;
  handled: boolean;
begin
  lmsg :=msg;
  if lmsg='' then
     lmsg:=format('Illegal character/word: [%s] in line %d',
        [CurrentToken.Token,lineNumber]);
  handled := false;
  if assigned(fOnError) then
     fOnError(lmsg,CurrentToken,handled);
  if handled then
    raise TParserException.create(lmsg);
end;


function TSMBaseTokenizer.ReadToChar(pTerminator:char):TSMToken;
var
  Ch : char;
begin
  Result.token:='';
  Ch := FInStrm.GetChar;
  while (Ch <> #0) and (Ch <> #13) and (ch<>pTerminator) do
  begin
     result.Token := result.Token+ch;
     ch:=FInStrm.GetChar;
  end;
  Result.TokenType:=opLineRead;
  FInStrm.PutBackChar(ch);
//now the tokenizer is ready to read next char
  GetNextToken;
//  GetNextToken;
//The current token now is invalid so update it
  fCurrentToken :=Result;
end;


function TSMBaseTokenizer.ReadToEOL:TSMToken;
var
  Ch : char;
begin
  Result.token:='';
  Ch := FInStrm.GetChar;
  while (Ch <> #0) and (Ch <> #13)  do
  begin
     result.Token := result.Token+ch;
     ch:=FInStrm.GetChar;
  end;
  Result.TokenType:=opLineRead;
  FInStrm.PutBackChar(ch);
//now the tokenizer is ready to read EOL or EOF
  GetNextToken;
//The current token now is invalid so update it
  fCurrentToken :=Result;
end;



function TSMBaseTokenizer.GetEndOfExp: boolean;
begin
  result :=CurrentToken.TokenType in ExprDelimiters;
end;

function TSMBaseTokenizer.GetExprDelimiters: TSMTokenTypeSet;
begin
 result:=[opEndOfFile{,opCloseParen,opComma}];
end;

function TSMBaseTokenizer.IsOperator(aTokenType: TSMTokenType):boolean;
begin
 result:= (aTokenType in SMOperators) ;
end;


function TSMBaseTokenizer.GetNextToken: TSMToken;
var
  delim : char;
  p : pchar;
begin
 fPrevToken:=fCurrentToken;
 fCurrentToken:= fLookAheadToken;
 GetToken(fLookAheadToken);
 if (fLookAheadToken.TokenType=opstring) and (fLookAheadToken.token<>'')and dequoteStrings then
 begin
    delim := StringDelim;
    p := pchar(fLookAheadToken.Token);
    fLookAheadToken.token:=AnsiExtractQuotedStr(p,delim);
 end;
 result:=fCurrentToken;
 if (result.TokenType <> opString) then
   result.token:=trim(result.token);
end;

procedure TSMBaseTokenizer.PutBackLastToken(aToken: TSMToken);
var
 diff,i :integer;
begin
  diff:=length(aToken.token);
  for i:= 0 to diff-1 do
     FInStrm.PutBackChar(aToken.token[i]);
end;


procedure TSMBaseTokenizer.GetTokenClass(var aToken: TSMToken);
var
  DummyObj : pointer;
begin
   aToken.TokenType:=opInvalidToken;
   aToken.TokenGroup  := opInvalidToken;
//   aToken.Token:=UpperCase(aToken.Token);
   if FKeywords.Find(AnsiUppercase(aToken.Token), DummyObj) then
   begin  //word operators (e.g. and, div, shr etc) will be found here
      if isOperator(TSMTokenType(DummyObj)) then
         aToken.TokenType := TSMTokenType(DummyObj)
      else
      begin
        aToken.TokenGroup  := opKeyword;
        aToken.TokenSubType :=TSMTokenType(DummyObj);
      end;
   end;
//operators (single char or words) will be processed further here
  if isOperator(aToken.TokenType)  then
  begin
     aToken.TokenGroup :=opOperator;
     aToken.TokenSubType :=aToken.TokenType;
  end;
end;

procedure TSMBaseTokenizer.GetToken(var aToken: TSMToken);
var
  DummyObj : pointer;
begin
  AAGetToken(aToken);
  aToken.line :=LineNumber;
//preporcess token to handle certain operators
  aToken.TokenGroup  := aToken.TokenType;
  if (aToken.TokenType in [opIdentifier]) then
  begin
    if FKeywords.Find(AnsiUppercase(aToken.Token), DummyObj) then
    begin  //word operators (e.g. and, div, shr etc) will be found here
      if isOperator(TSMTokenType(DummyObj)) then
         aToken.TokenType := TSMTokenType(DummyObj)
      else
      begin
        aToken.TokenGroup  := opKeyword;
//        aToken.TokenType :=TSMTokenType(DummyObj);
        aToken.TokenSubType :=TSMTokenType(DummyObj);
      end;
    end;
  end;
  if isOperator(aToken.TokenType)  then
  begin
  //operators (single char or words) will be processed further here
     aToken.TokenGroup :=opOperator;
     aToken.TokenSubType :=aToken.TokenType;
  end
  else if (aToken.TokenType in [opQuery, opDoDos]) then
  begin
     aToken.TokenGroup :=opKeyword;
     aToken.TokenSubType :=aToken.TokenType;
     aToken.TokenType := opKeyword;
  end;
end;
{--------}
procedure TSMBaseTokenizer.ppInitKeywords;
var
  i : TSMTokenType;
begin
  Assert(FKeywords <> nil,
        'ppInitKeywords cannot be called with nil hash table');
  for i :=low(TokensList) {opNot}  to high(TokensList) {opXor} do
    FKeywords.Insert(TokensList[i], pointer(i));
end;

function TSMBaseTokenizer.GetTokenName(index:TSMTokenType): string;
begin
  result := '';
//try and get it from the keywords list
//  result:=keywords.Strings[index];
// empty? try and get it directly from the tokenlist
 if result<>'' then exit;
 if (index>=low(TokensList)) and (index<=high(TokensList)) then
     result:= TokensList[index];
 if result<>'' then exit;
 if (index >= low(AxTokensList)) and (index <= high(AxTokensList)) then
    result:= AxTokensList[index];
end;


procedure TSMBaseTokenizer.SetExprDelimiters(const Value: TSMTokenTypeSet);
begin

end;

function TSMBaseTokenizer.GetCurrentToken: TSMToken;
begin
   result:=fCurrentToken;
end;

procedure TSMBaseTokenizer.SetCurrentToken(const Value: TSMToken);
begin
  fCurrentToken:=value;
end;


function TSMBaseTokenizer.GetLookAheadToken: TSMToken;
begin
 result:=fLookAheadToken;
end;


{====================================================================}

{
procedure TSMBaseTokenizer.SetStringToParse(const Value: string);
begin
  FStringToParse := Value;
end;
}
function TSMBaseTokenizer.GetRestOfLine: string;
begin
  Result:= StringToParse;
end;

end.
