unit UExpToken;

interface

uses AAPasTok;

const

 opTRUE=opParserdefined+1;
 opFALSE=opTRUE+1;
 opIIF=opFALSE+1;
 opUpper=opIIF+1;
 opLower=opUpper+1;
 opCopy=opLower+1;
 opSubstr=opCopy+1;
 opPos=opSubstr+1;
 opTrim=opPos+1 ;
 opSysDate=opTrim+1;
 opSysTime=opSysDate+1;
 opLength=opSysTime+1;
 opFindFile=opLength+1;


//start of math expressions
 opTrunc=opFindFile+1;
 opRound=opTrunc+1;
 opAbs=opRound+1;
 opArcTan=opAbs+1;
 opCos=opArcTan+1;
 opExpF=opCos+1;
 opFrac=opExpF+1;
 opInt=opFrac+1;
 opLn=opInt+1;
 opPi=opLn+1;
 opSin=opPi+1;
 opSqr=opSin+1;
 opSqrt=opSqr+1;
 opLog=opSqrt+1;
 opRND=opLog+1;
 opRAN=opRND+1 ;
 opRanG=opRAN+1;
 opDate=opRanG+1;
 opDMY=opDate+1;
 opToday=opDMY+1;
 opDay=opToday+1;
 opMonth=opDay+1;
 opYear=opMonth+1;
 opWeekNum=opYear+1;
 opDayWeek=opWeekNum+1;
 opSpan=opDayWeek+1;
 opSum=opSpan+1;
 opMean=opSum+1;
 opCentile=opMean+1;
 opSD=opCentile+1;
 opVariance=opSD+1;
 opLRE=opVariance+1;
 opRsum=opLRE+1;
 opSameNumber=opRsum+1;
 opMv=opSameNumber+1;
 opPower=opMv+1;



//end of math expressions
 opStringCast=opPower+1;
 opFloatCast=opStringCast+1;
 OpIntegerCast=opFloatCast+1;
 OpEnumeratedCast=OpIntegerCast+1;
 opBooleanCast=OpEnumeratedCast+1;



Type

TSMExpTokenizer=class(TSMBaseTokenizer)

protected
   procedure ppInitKeywords;override;
   function GetTokenName(index:TSMTokenType): string;override;
end;

implementation

const
    TokensList: array[opTRUE..opBooleanCast] of String =(
                'TRUE','FALSE','IIF',

                'UPPER', 'LOWER', 'COPY', 'SUBSTR','POS',
                'TRIM','SYSTEMDATE','SYSTEMTIME', 'LENGTH','FINDFILE',

                'TRUNC', 'ROUND', 'ABS', 'ARCTAN', 'COS', 'EXP', 'FRAC', 'INT',
                'LN', 'PI', 'SIN', 'SQR', 'SQRT','LOG','RND','RAN','RANG',
                'DATE','DMY','TODAY','DAY','MONTH','YEAR','WEEKNUM','DAYOFWEEK',
                'SPAN','GETSUM','GETMEAN', 'GETCENTILE','GETSD','GETVARIANCE',
                'LRE','RSUM','SAMENUM','MV','POWER',
                'STRING', 'FLOAT', 'INTEGER', 'ENUMERATED', 'BOOLEAN');




//  opTrunc.. opPower: all mathematical functions should be between those;
//  opUpper..opLength,opSubstr: all string functions should be between those;
//  opStringCast.. opBooleanCast : all casting function between those


{ TSMExpTokenizer }

function TSMExpTokenizer.GetTokenName(index: TSMTokenType): string;
begin
  Result:=inherited GetTokenName(index);
  if result='' then
  begin
    if (index<low(TokensList)) or (index>high(TokensList)) then exit;
    result:= TokensList[index];
  end;
end;

procedure TSMExpTokenizer.ppInitKeywords;
var
  i : TSMTokenType;
begin
  inherited;
  for i :=low(TokensList) {opTRUE}  to high(TokensList) {opBooleanCast} do
    Keywords.Insert(TokensList[i], pointer(i));
end;

end.
