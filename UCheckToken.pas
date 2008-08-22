unit UCheckToken;

interface

uses AAPasTok;

const

 opBegin=opUserDefined+1;
 opEnd=opUserDefined+2;
{ opRUN= opUserDefined+3;
 opEXEC=opUserDefined+4;
 opEXECUTE=opUserDefined+5;}

//modify the following code to add more keywords to the glue tokenizer
 {
 opIIF=opUserDefined+3;
 opUpper=opUserDefined+4;
 opLower=opParserdefined+5;
 opCopy=opParserdefined+6;
 opPos=opParserdefined+7;
 opLength=opParserdefined+8;
 opTrunc=opParserdefined+9;
 opRound=opParserdefined+10;
 opAbs=opParserdefined+11;
 opArcTan=opParserdefined+12;
 opCos=opParserdefined+13;
 opExpF=opParserdefined+14;
 opFrac=opParserdefined+15;
 opInt=opParserdefined+16;
 opLn=opParserdefined+17;
 opPi=opParserdefined+18;
 opSin=opParserdefined+19;
 opSqr=opParserdefined+20;
 opSqrt=opParserdefined+21;
 opPower=opParserdefined+22;
 opObjectCast=opParserdefined+23;
 opStringCast=opParserdefined+24;
 opFloatCast=opParserdefined+25;
 OpIntegerCast=opParserdefined+26;
 OpEnumeratedCast=opParserdefined+27;
 opBooleanCast=opParserdefined+28;
 opLog=opParserdefined+29;}



Type

TSMCheckTokenizer=class(TSMBaseTokenizer)
protected
   procedure ppInitKeywords;override;
   function GetTokenName(index:TSMTokenType): string;override;
public
   procedure Initialize; override;
end;

implementation
//modify the following array to add more keyword

const
TokensList: array[opBegin..opEnd] of String =(
                'BEGIN','END'{,'RUN','EXEC', 'EXECUTE'{, 'COPY', 'POS', 'LENGTH',
                'TRUNC', 'ROUND', 'ABS', 'ARCTAN', 'COS', 'EXP', 'FRAC', 'INT',
                'LN', 'PI', 'SIN', 'SQR', 'SQRT', 'POWER',
                'OBJECT', 'STRING', 'FLOAT', 'INTEGER', 'ENUMERATED', 'BOOLEAN',
                'LOG'});


{ TSMExpTokenizer }
{
constructor TSMCheckTokenizer.Create(aInStm: TaaInCharStream);
begin
  inherited Create(aInStm);
  DequoteStrings:=true;
end;
}
function TSMCheckTokenizer.GetTokenName(index: TSMTokenType): string;
begin
  Result:=inherited GetTokenName(index);
  if result<> '' then exit;
  if (index<low(TokensList)) or (index>high(TokensList)) then exit;
    result:= TokensList[index];
end;


procedure TSMCheckTokenizer.Initialize;
begin
inherited;
DequoteStrings:=true;
end;

procedure TSMCheckTokenizer.ppInitKeywords;
var
  i : TSMTokenType;
begin
  inherited;
  for i :=low(TokensList) to high(TokensList) do
    Keywords.Insert(TokensList[i], pointer(i));
end;

end.
