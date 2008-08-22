unit EpiDataUtils;

interface

USES
  SysUtils,Rijndael,Base64, UCmdTypes;


CONST
  IntegerChars:    Set of CHAR=['0'..'9','-','+'];
  FloatChars:      Set of CHAR=['0'..'9', '.', ',', '-', '+'];
  FieldNameLen:Byte=10;
  DaysInMonth:     ARRAY[1..12] OF BYTE = (31,29,31,30,31,30,31,31,30,31,30,31);

TYPE
  str10=String[10];

VAR
  Cipher: TDCP_Rijndael;

Function cFill(c:Char; Len:Integer):String;
Function  IsInteger(s:String):Boolean;
Function  IsFloat(s:String):Boolean;
Function  FirstWord(s:String):str10;
Function  mibIsDate(VAR s:String; Style:TFelttyper):Boolean;
Function  mibDateToStr(d:TDateTime; Style:TFelttyper):String;
Function  mibStrToDate(s:String; Style:TFelttyper):TDateTime;
Function  FormatNumberToIndex(s:String):str30;
Function  FormatFloating(s:String;Len:Integer):String;
Function  FormatInt(Num,Len:Integer):String;
Function  FormatStr(s:String;Len:Integer):String;
Function  EncryptString(CONST s,key: String):String;
Function  DecryptString(CONST s,key: String):String;
Procedure Double2SingleQuotes(VAR s:String);
Function  GetColors(s:String; VAR txtcolor,bgcolor, HighLightColor:Byte; VAR IsEpiInfo:Boolean):Boolean;
Function  IsCompliant(s:String; Style:TFelttyper):Boolean;
Function  RemoveQuotes(s:String):String;
Procedure Single2DoubleQuotes(VAR s:String);
Function  FitLength(s:String;L: Integer):String;
Function  GetCommentLegalText(CONST s:String; ComLegRec: PlabelRec):String;
Function  eStrToFloat(s: String):Double;
Function  GetEncodedLength(decodedlength: byte):byte; 
Function  GetDecodedLength(encodedlength: byte):byte;


implementation

Function cFill(c:Char; Len:Integer):String;
VAR
  s:String;
  n:Integer;
BEGIN
  s:='';
  FOR n:=1 TO Len DO s:=s+c;
  Result:=s;
END;   //function Fill


Function IsInteger(s:String):Boolean;
VAR
  t:Integer;
BEGIN
  Result:=True;
  s:=trim(s);
  IF s='' THEN Result:=False;
  IF POS(' ',s)>0 THEN Result:=False;
  IF POS('-',s)>1 THEN Result:=False;
  IF Pos('+',s)>1 THEN Result:=False;
  IF Result THEN
    FOR t:=1 TO Length(s) DO
      IF NOT (s[t] in IntegerChars) THEN Result:=False;
END;   //Function IsInteger

Function IsFloat(s:String):Boolean;
VAR
  t:Integer;
BEGIN
  Result:=True;
  s:=trim(s);
  FOR t:=1 TO Length(s) DO
    IF NOT ( s[t] in FloatChars ) THEN Result:=False;
  IF POS(' ',s)>0 THEN Result:=False;
  IF POS('-',s)>1 THEN Result:=False;
END;  //function IsFloat

Function FirstWord(s:String):str10;
VAR
  n:Integer;
BEGIN
  s:=trim(s);
  WHILE Pos(#9,s)>0 DO s[Pos(#9,s)]:=' ';
  n:=Pos(' ',s);
  IF n=0 THEN n:=Length(s)+1;
  IF n>FieldNameLen THEN n:=FieldNameLen+1;
//  IF n>0 THEN Result:=Copy(s,1,n-1) ELSE Result:='';
  Result:=Copy(s,1,n-1);
END;  //function FirstWord

Function mibIsDate(VAR s:String; Style:TFeltTyper):Boolean;

VAR
  tmpS,eMonthStr,eDayStr,eYearStr:String[10];
  day,month,year,tmpDay:Word;
  d2,m2:Word;
  tmpDate:TDateTime;
  qq:Integer;  //&&
BEGIN
  Result:=True;
  tmpS:=s;
  IF trim(tmpS)='' THEN
    BEGIN
      Result:=False;
      Exit;
    END;
  IF pos('/',tmpS)<>0 THEN
    BEGIN   //first slash is found
      IF (Style=ftYMDDate) OR (Style=ftYMDToday) THEN  //&&
        BEGIN
          qq:=pos('/',tmpS);
          tmpS[qq]:='¤';
          IF pos('/',tmpS)>0 THEN
            BEGIN
              //String has two slashes meaning year is included
              eYearStr:=Copy(tmpS,1,pos('¤',tmpS)-1);
              Delete(tmpS,1,pos('¤',tmpS));    //deletes year and separator
              eMonthStr:=copy(tmpS,1,pos('/',tmpS)-1);
              Delete(tmpS,1,pos('/',tmpS));   //deletes month and second separator
              eDayStr:=tmpS;
            END
          ELSE
            BEGIN
              //String has one slash meaning year is not included
              eYearStr:='';
              eMonthStr:=copy(tmpS,1,pos('¤',tmpS)-1);
              Delete(tmpS,1,pos('¤',tmpS));  //deletes month and separator
              eDayStr:=tmpS;
            END;
        END  //if ftYMDDate
      ELSE
        BEGIN
          eDayStr:=Copy(tmpS,1,pos('/',tmpS)-1);
          Delete(tmpS,1,pos('/',tmpS));
          IF pos('/',tmpS)<>0 THEN
            BEGIN  //second slash is found
              eMonthStr:=Copy(tmpS,1,pos('/',tmpS)-1);
              Delete(tmpS,1,pos('/',tmpS));
              eYearStr:=tmpS;
              IF trim(eYearStr)='' THEN eYearStr:='';
            END
          ELSE
            BEGIN
              eMonthStr:=tmpS;
              IF trim(eDayStr)='' THEN eDayStr:='';
              eYearStr:='';
            END;   //if there is a second slash
        END;  //if not YMDDate
    END   //if there is a first slash
  ELSE
    BEGIN   //the string contains no slash
      IF (Style=ftYMDDate) OR (Style=ftYMDToday) THEN  //&&
        BEGIN
          eMonthStr:='';
          eDayStr:='';
          eYearStr:='';
          CASE Length(tmpS) OF
            1,2: eDayStr:=trim(tmpS);
            4:   BEGIN
                   eMonthStr:=Copy(tmpS,1,2);
                   eDayStr:=Copy(tmpS,3,2);
                 END;
            6:   BEGIN
                   eYearStr:=Copy(tmpS,1,2);
                   eMonthStr:=Copy(tmpS,3,2);
                   eDayStr:=Copy(tmpS,5,2);
                 END;
            8:   BEGIN
                   eYearStr:=Copy(tmpS,1,4);
                   eMonthStr:=Copy(tmpS,5,2);
                   eDayStr:=Copy(tmpS,7,2);
                 END;
          ELSE
            result:=False;
          END;  //case
        END  //if ftYMDDate
      ELSE
        BEGIN
          While Length(tmpS)<8 DO tmpS:=tmpS+' ';
          eDayStr:=Copy(tmpS,1,2);
          eMonthStr:=Copy(tmpS,3,2);
          eYearStr:=Copy(tmpS,5,4);
        END;
    END;  //if string has no slash
  IF (trim(eMonthStr)<>'') AND (isInteger(eMonthStr))
    THEN Month:=StrToInt(trim(eMonthStr)) ELSE Result:=False;
  IF (trim(eDayStr)<>'') AND (IsInteger(eDayStr))
    THEN Day:=StrToInt(trim(eDayStr)) ELSE Result:=False;
  IF (trim(eYearStr)='') THEN
    BEGIN
      DecodeDate(Date,Year,m2,d2);
      eYearStr:=IntToStr(Year);
    END
  ELSE
    IF IsInteger(eYearStr)
      THEN Year:=StrToInt(trim(eYearStr))
    ELSE
      BEGIN
        Result:=False;
        Year:=0;
      END;
  IF (Style=ftDate) or (Style=ftToday) THEN
    BEGIN
      tmpDay:=Day;
      Day:=Month;
      Month:=tmpDay;
    END;
  IF (Year>=0)  AND (Year<50)  THEN Year:=Year+2000;
  IF (Year>=50) AND (Year<100) THEN Year:=Year+1900;
  IF (Month>12) OR  (Month<1)  THEN Result:=False
  ELSE
    BEGIN
      IF (Day<1) OR (Day>DaysInMonth[Month]) THEN Result:=False;
      IF (Result) AND (Day=29) AND (Month=2)
        THEN IF IsLeapYear(Year) THEN Result:=True ELSE Result:=False;
    END;
  {Formatter output}
  IF Result THEN  //legal date entered
    BEGIN
      tmpDate:=EncodeDate(Year,Month,Day);
      s:=mibDateToStr(tmpDate,Style);
    END;
END;



Function mibDateToStr(d:TDateTime; Style:TFeltTyper):String;
BEGIN
  IF (Style=ftEuroDate) or (Style=ftEuroToday) THEN Result:=FormatDateTime('dd"/"mm"/"yyyy',d)
  ELSE IF (Style=ftYMDDate) or (Style=ftYMDtoday) THEN Result:=FormatDateTime('yyyy"/"mm"/"dd',d)
  ELSE Result:=FormatDateTime('mm"/"dd"/"yyyy',d);
END;

Function mibStrToDate(s:String; Style:TFeltTyper):TDateTime;
VAR
  day,month,year,tmpDay:word;
BEGIN
  IF mibIsDate(s,Style) THEN
    BEGIN
      IF (Style=ftYMDDate) OR (Style=ftYMDToday) THEN  //&&
        BEGIN
          year:=StrToInt(Copy(s,1,4));
          month:=StrToInt(Copy(s,6,2));
          day:=StrToInt(Copy(s,9,2));
        END
      ELSE
        BEGIN
          day:=StrToInt(Copy(s,1,2));
          month:=StrToInt(Copy(s,4,2));
          year:=StrToInt(Copy(s,7,4));
          IF (Style=ftDate) or (Style=ftToday) THEN
            BEGIN
              tmpDay:=Day;
              Day:=Month;
              Month:=tmpDay;
            END;
        END;
      Result:=EncodeDate(year,month,day);
    END
  ELSE Result:=0;
END;


Function FormatNumberToIndex(s:String):str30;
VAR
  n:Double;
BEGIN
  IF trim(s)='' THEN Result:=Format('%30s',[s])
  ELSE
    BEGIN
      IF IsInteger(s) THEN
        BEGIN
          n:=StrToFloat(s);
          Result:=Format('%30g',[n]);
        END
      ELSE Result:=Format('%30s',[s]);
    END;
END;

Function FormatInt(Num,Len:Integer):String;
VAR
  TempStr:String;
BEGIN
  TempStr:=IntToStr(Num);
  WHILE Length(TempStr)<Len DO TempStr:=' '+TempStr;
  IF Length(TempStr)>Len THEN TempStr:=COPY(TempStr,1,Len);
  FormatInt:=TempStr;
END;  //function FormatInt



Function FormatFloating(s:String;Len:Integer):String;
BEGIN
  WHILE Length(s)<Len DO s:=' '+s;
  IF Length(s)>Len THEN s:=COPY(s,1,Len);
  Result:=s;
END;

Function FormatStr(s:String;Len:Integer):String;
BEGIN
  WHILE Length(s)<Len DO s:=s+' ';
  IF Length(s)>Len THEN s:=COPY(s,1,Len);
  FormatStr:=s;
END;  //function FormatStr



Procedure InitCryptograph;
BEGIN
  Cipher:=TDCP_Rijndael.create(nil);
END;


Function  EncryptString(CONST s,key: String):String;
VAR
  ss: String;
BEGIN
  IF (NOT Assigned(Cipher)) THEN InitCryptograph;
  Cipher.InitStr(key);    // initialize the cipher with the key
  ss:=s;
  Cipher.EncryptCFB(ss[1],ss[1],Length(ss));  // encrypt all of the strings
  result:=B64Encode(ss);        // Base64 encode the string to ensure all characters are printable
  Cipher.Reset;         // we are using CFB chaining mode so we must reset after each block of encrypted/decrypts
  Cipher.Burn;
END;

Function  DecryptString(CONST s,key: String):String;
VAR
  ss: String;
BEGIN
  IF (NOT Assigned(Cipher)) THEN InitCryptograph;
  Cipher.InitStr(key);    // initialize the cipher with the key
  ss:=s;
  ss:= B64Decode(ss);        // decode the Base64 encoded string
  Cipher.DecryptCFB(ss[1],ss[1],Length(ss));  // decrypt all of the strings
  result:= ss;
  Cipher.Reset;            // we are using CFB chaining mode so we must reset after each block of encrypted/decrypts
  Cipher.Burn;
END;

Procedure Double2SingleQuotes(VAR s:String);
BEGIN
  WHILE pos('"',s)>0 DO
    s[Pos('"',s)]:='''';
END;

Function GetColors(s:String; VAR txtcolor,bgcolor, HighLightColor:Byte; VAR IsEpiInfo:Boolean):Boolean;
VAR
  n: Integer;
  s2,s3: String;
BEGIN
  //input can be either a epi info color code (one number)
  //or 1-2 EpiData color words
  //one word present: s2=word, s='', s3=''
  //two words present: s2=word1, s=word2,  s3=''
  //three words present:  s2=word1,  s=word2,  s3=word3
  txtcolor:=255;
  bgcolor:=255;
  HighLightColor:=255;
  s:=AnsiUpperCase(trim(s));
  n:=pos(' ',s);
  IF n=0 THEN
    BEGIN
      s2:=s;
      s:='';
    END
  ELSE
    BEGIN
      s2:=copy(s,1,n-1);
      s:=copy(s,n+1,length(s));
      //is third word present?
      n:=pos(' ',s);
      IF n=0 THEN s3:=''
      ELSE
        BEGIN
          s3:=trim(copy(s,n+1,length(s)));;
          s:=copy(s,1,n-1);
        END;
    END;
  IF IsInteger(s2) THEN
    BEGIN
      //input is a epi info color number
      IsEpiInfo:=True;
      n:=StrToInt(s2);
      IF n>255 THEN
        BEGIN
          Result:=False;
          Exit;
        END;
      n:=n AND $7F;  //clear first bit which indicates flashing text in epi info
      bgcolor:=(n AND $F0) SHR 4;
      txtcolor:=(n AND $0F);
      Result:=True;
      Exit;
    END
  ELSE
    BEGIN
      //input is one, two or three EpiData color words
      IsEpiInfo:=False;
      FOR n:=0 TO 17 DO
        IF s2=ColorNames[n] THEN txtcolor:=n;
      Result:=False;
      IF txtcolor=255 THEN Exit;
      IF s<>'' THEN
        BEGIN
          //get second word - if present
          FOR n:=0 TO 17 DO
            IF s=ColorNames[n] THEN bgcolor:=n;
          IF bgcolor=255 THEN Exit;
        END;
      IF s3<>'' THEN
        BEGIN
          //get 3rd word - if present
          FOR n:=0 TO 17 DO
            IF s3=ColorNames[n] THEN HighLightColor:=n;
          IF HighLightColor=255 THEN Exit;
        END;
      Result:=True
    END;
END;  //function GetColors

Function  IsCompliant(s:String; Style:TFelttyper):Boolean;
BEGIN
  Result:=True;
  CASE Style OF
    ftInteger,ftIDNUM: IF NOT IsInteger(s) THEN Result:=False;
    ftFloat:   IF NOT IsFloat(s)   THEN Result:=False;
    ftBoolean: IF NOT (s[1] in BooleanChars) THEN Result:=False;
    ftDate,ftEuroDate,ftToday,ftEuroToday,ftYMDDate,ftYMDToday: IF NOT mibIsDate(s,Style) THEN Result:=False;
    ftUpperAlfa: IF s<>AnsiUpperCase(s) THEN Result:=False;
  END;
END;  //function IsComliant

Function RemoveQuotes(s:String):String;
VAR
  n,QStart,EndQ:Integer;
  KeepQ:Boolean;
  tmpS:String;
BEGIN
  IF s='' THEN Exit;
  n:=1;
  QStart:=0;
  EndQ:=0;
  KeepQ:=False;
  tmpS:='';
  REPEAT
    IF (QStart>0) AND ( (s[n]=',') OR (s[n]=' ') ) THEN KeepQ:=True
      ELSE IF (QStart>0) AND (s[n]='"') THEN
        BEGIN
          EndQ:=n;
          IF NOT KeepQ THEN tmpS:=tmpS+Copy(s,QStart+1,EndQ-QStart-1)
          ELSE tmpS:=tmpS+Copy(s,QStart,EndQ-QStart+1);
          QStart:=0;
        END  //if EndQuote found
        ELSE IF (s[n]='"') AND (QStart=0) THEN
          BEGIN
            QStart:=n;
            KeepQ:=False;
          END
          ELSE IF QStart=0 THEN tmpS:=tmpS+s[n];
    INC(n);
  UNTIL n>Length(s);
  Result:=tmpS;
END;  //function RemoveQuotes

Procedure Single2DoubleQuotes(VAR s:String);
BEGIN
  WHILE pos('''',s)>0 DO
    s[Pos('''',s)]:='"';
END;

Function  FitLength(s:String;L: Integer):String;
{Makes sure that a string is exactly L chars in length}
BEGIN
  IF Length(s)>L THEN Result:=Copy(s,1,L)
  ELSE IF Length(s)<L THEN Result:=s+cFill(' ',L-Length(s))
  ELSE Result:=s;
END;

Function GetCommentLegalText(CONST s:String; ComLegRec: PlabelRec):String;
BEGIN
  Result:='';
  IF ComLegRec<>NIL THEN
    BEGIN
      WHILE (ComLegRec<>NIL) AND (Result='') DO
        BEGIN
          IF ComLegRec^.Value[1]<>'*' THEN
            BEGIN
              IF trim(s)=trim(ComLegRec^.Value) THEN Result:=ComLegRec^.Text;
            END;
          ComLegRec:=ComLegRec^.Next;
        END;  //while
    END;  //if
END;  //function GetCommentLegalText

Function  eStrToFloat(s: String):Double;
VAR
  n:Integer;
BEGIN
  FOR n:=1 TO Length(s) DO
    BEGIN
      IF s[n]=',' THEN s[n]:=DecimalSeparator;
      IF s[n]='.' THEN s[n]:=DecimalSeparator;
    END;
  Result:=StrToFloat(s);
END;  //function eStrToFloat

Function  GetEncodedLength(decodedlength: byte):byte;   //&&
BEGIN
  Result:=((decodedlength+2) div 3)*4;
END;

Function  GetDecodedLength(encodedlength: byte):byte;   //&&
BEGIN
  Result:=(encodedlength div 4)*3;
END;

end.
