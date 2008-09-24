{*********************************************************}
{* AADate.PAS                                            *}
{* Copyright (c) Julian M Bucknall 1993 - 1999           *}
{* All rights reserved.                                  *}
{*********************************************************}
{* Date arithmetic routines                              *}
{*********************************************************}

{Note: this unit is released as freeware. In other words, you are free
       to use this unit in your own applications, however I retain all
       copyright to the code. JMB}

{ naming adapted to EpiData Analysis by Salah Mahmud 2002

  further adaptation 2007 JLauritsen
}

unit uDateUtils;

interface

uses
  SysUtils,
  Classes
,ansDatatypes, UEpiDataTypes
;



type

  EEpiDateException= class(Exception);

  pDATERec = ^DATERec;
  DATERec = packed record
  {000} Year     : array[0..3]of Char;
  {004} Month    : array[0..1]of Char;
  {006} Day      : array[0..1]of Char;
  end;


//  EpiDate = longint;
  TEpiDOW = (EpiSunday, EpiMonday, EpiTuesday, EpiWednesday,
            EpiThursday, EpiFriday, EpiSaturday);
  TEpiDateFormat = (  {Date string formats..}
        dfWindows,   {..Windows defined}
        dfLotus,     {..dd-Mmm-yyyy}
        dfLotusDOW,  {..Ddd dd-Mmm-yyyy}
        dfDMY,       {..dd/mm/yyyy}
        dfMDY,       {..mm/dd/yyyy}
        dfYMD);      {..yyyy/mm/dd}

{--basic routines---}
function EpiYMDToDate(Obs,Y, M, D : integer) : EpiDate;
procedure EpiDateToYMD(aDate : EpiDate; var Y, M, D : integer);
function EpiIsLeapYear(Y : integer) : boolean;
function EpiDaysInMonth(Y, M : integer) : integer;
function EpiToday : EpiDate;
function DatePart(aDate: EpiDate;part: word):integer;

{--conversion to/from other formats--}
function EpiDateToTDateTime(aDate : EpiDate) : TDateTime;
function EpiTDateTimeToDate(aDate : TDateTime) : EpiDate;
function EpiDateToStDate(aDate : EpiDate) : longint;
function EpiStDateToDate(aDate : longint) : EpiDate;
function EpiDateToGregDate(aDate : EpiDate) : longint;
function EpiGregDateToDate(aDate : longint) : EpiDate;
procedure EpiDateToISODate(aDate : EpiDate; var Y, W, D : integer);
function EpiISODateToDate(Y, W, D : integer) : EpiDate;

{--month arithmetic--}
function EpiDateAddMonths(aDate : EpiDate; aMonths : integer;
                         aStickyMonthEnds : boolean) : EpiDate;
function EpiDateDiffInMonths(aDate1, aDate2 : EpiDate;
                            aStickyMonthEnds : boolean;
                        var aDays : integer) : integer;


{--day of week arithmetic---}
function EpiDayOfWeek(aDate : EpiDate) : TEpiDOW;
function EpiDayofWeekMon(aDate : EpiDate) : integer;
function EpiIsDayOfWeek(aDate : EpiDate; aDOW : TEpiDOW) : boolean;
function EpiNextDayOfWeek(aDate : EpiDate; aDOW : TEpiDOW) : EpiDate;
function EpiPrevDayOfWeek(aDate : EpiDate; aDOW : TEpiDOW) : EpiDate;


function EpiWeekNum(ADate: EpiDate):Integer;

{---validation---}
function EpiIsValidYMD(Y, M, D : integer) : boolean;
function EpiIsValidDate(aDate : EpiDate) : boolean;

{---string representation---}
Function EpiStrToDatefmt(const s,fmt:String):Epidate;
//Function EpiStrToDate(const s:String;Typ:TEpiDateFormat):Epidate;
Function EpiStrToDate(const s:String;var Value:Epidate;Typ:TEpiDateFormat):boolean;
function EpiDateToStr(aDate : EpiDate; aFormat : TEpiDateFormat;aLen:integer=10) : string;
function EpiShortDayName(aDOW : TEpiDOW) : string;
function EpiLongDayName(aDOW : TEpiDOW) : string;
function DateFmtToEpiDateFmt(DateFmt: string) :TEpiDateFormat;




implementation

{$IFDEF Win32}
uses
  Windows, ucmdprocessor, UCmdTypes, EpiDataUtils;
{$ENDIF}

type
  PFirstJanuarys = ^TFirstJanuarys;
  TFirstJanuarys = array [0..400] of EpiDate;

  PCumulativeDays = ^TCumulativeDays;
  TCumulativeDays = array [boolean, 0..12] of word;

const
  DaysInMonth : array [1..13] of byte =
                (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 0);
  DaysInFeb = 28;
  DaysInLeapFeb = 29;
  MaxDate = 146096; {date values range from 0 to 146096}
  MinYear = 1800;   {year values range from 1800..}
  MaxYear = 2199;   {..to 2199}
  MaxMonth = 4799;  {month values range from 0 to 4799}
  DOW18000101 = EpiWednesday;  {1 Jan 1800 was a Wednesday}
  {$IFDEF Windows}
  MagicTDateTime = -657072;
  {$ELSE}
  MagicTDateTime = 36522;
  {$ENDIF}
  MagicStDate = -73049;

var
  FirstJanuarys : PFirstJanuarys;
  CumulativeDays : PCumulativeDays;
  WindowsDateFormat : TEpiDateFormat;

   fThisYear           : word;

{===Primitives=======================================================}
function IsLeapYearPrim(Y : integer) : boolean;
begin
  {assumes Y is valid}
  Result := ((Y mod 4) = 0) and
            (Y <> 1800) and (Y <> 1900) and (Y <> 2100);
end;
{--------}
function DaysInMonthPrim(Y, M : integer) : integer;
begin
  if (M = 2) and IsLeapYearPrim(Y) then
    Result := DaysInLeapFeb
  else
    Result := DaysInMonth[M];
end;
{====================================================================}


{===Interfaced routines==============================================}
function EpiDateAddMonths(aDate : EpiDate; aMonths : integer;
                         aStickyMonthEnds : boolean) : EpiDate;
var
  Y, M, D : integer;
  DaysInM : integer;
  StickToMonthEnd : boolean;
  s : string;
begin
  s:='';
  EpiDateToYMD(aDate, Y, M, D);
  StickToMonthEnd := aStickyMonthEnds and (D = DaysInMonthPrim(Y, M));
  {calculate the month number from January 1800}
  M := (Y - MinYear) * 12 + pred(M) + aMonths;
  {if its out of range say so}
  if (M < 0) or (M > MaxMonth) then
    raise EEpiDateException.Create('EpiDateAddMonths: calculated date in out of range');
  {calculate the new year and month}
  Y := (M div 12) + MinYear;
  M := succ(M mod 12);
  {check to see that the date is in range for the month}
  DaysInM := DaysInMonthPrim(Y, M);
  if StickToMonthEnd or (D > DaysInM) then
    D := DaysInM;
  Result := EpiYMDToDate(-1,Y, M, D);
end;
{--------}
function EpiDateDiffInMonths(aDate1, aDate2 : EpiDate;
                            aStickyMonthEnds : boolean;
                        var aDays : integer) : integer;
var
  TempDate : EpiDate;
  Y1, M1, D1 : integer;
  Y2, M2, D2 : integer;
  Date1AtME  : boolean;
  s: string;
  Date2AtME  : boolean;
begin
  s:='';
  {make sure that aDate1 is less than aDate2}
  if (aDate1 > aDate2) then begin
    TempDate := aDate1;
    aDate1 := aDate2;
    aDate2 := TempDate;
  end;
  {convert dates to YMD}
  EpiDateToYMD(aDate1, Y1, M1, D1);
  EpiDateToYMD(aDate2, Y2, M2, D2);
  {make first approximation to answer}
  Result := ((Y2 - Y1) * 12) + (M2 - M1);
  {if both day numbers are less then 28, we don't have to worry about
   any month end calculations}
  if (D1 < 28) and (D2 < 28) then begin
    {if the first day is less than or equal to the second, then the
     day count is just the difference}
    if (D1 <= D2) then
      aDays := D2 - D1
    {otherwise, the month count is one too many, then we have to count
     the number of days from Y2/(M2-1)/D1 to Y2/M2/D2; the former date
     being Result whole months from aDate1}
    else begin
      dec(Result);
      dec(M2);
      if (M2 = 0) then begin
        M2 := 12;
        dec(Y2);
      end;
      if (D1 > DaysInMonthPrim(Y2, M2)) then
        D1 := DaysInMonthPrim(Y2, M2);
      aDays := aDate2 - EpiYMDToDate(-1,Y2, M2, D1);
    end;
    Exit;
  end;
  {if we reach this point, one or both of the dates might be at a
   month end, so *beware*}
  Date1AtME := D1 = DaysInMonthPrim(Y1, M1);
  Date2AtME := D2 = DaysInMonthPrim(Y2, M2);
  {the easiest case is both days are at month ends and we want sticky
   month ends: we're done after setting aDays to zero}
  if aStickyMonthEnds and Date1AtME and Date2AtME then begin
    aDays := 0;
    Exit;
  end;
  {the next easiest cases all use sticky month ends}
  if aStickyMonthEnds then begin
    {if the first date is at a month end (the second won't be) then
     the number of months is one too many, and the number of days is
     equal to the second day value}
    if Date1AtME then begin {note: Date2AtME = false}
      dec(Result);
      aDays := D2;
      Exit;
    end;
    {if the second date is at a month end (the first won't be) then
     the number of months is correct, and the number of days is
     equal to the second day value minus the first, or zero if this
     is negative}
    if Date2AtME then begin {note: Date1AtME = false}
      if D2 >= D1 then
        aDays := D2 - D1
      else
        aDays := 0;
      Exit;
    end;
  end;
  {if the second day number is greater or equal to the first, the
   number of days is the difference; the number of months is correct}
  if (D2 >= D1) then begin
    aDays := D2 - D1;
    Exit;
  end;
  {otherwise, the number of months is one too many, and the number of
   days is that from Y2/(M2-1)/D1 to Y2/M2/D2}
  dec(Result);
  dec(M2);
  if (M2 = 0) then begin
    M2 := 12;
    dec(Y2);
  end;
  if (D1 > DaysInMonthPrim(Y2, M2)) then
    D1 := DaysInMonthPrim(Y2, M2);
  aDays := aDate2 - EpiYMDToDate(-1,Y2, M2, D1);
end;
{--------}
function EpiDateToGregDate(aDate : EpiDate) : longint;
var
  Y, M, D : integer;
begin
  EpiDateToYMD(aDate, Y, M, D);
  Result := (((longint(Y) * 100) + M) * 100) + D;
end;
{--------}
procedure EpiDateToISODate(aDate : EpiDate; var Y, W, D : integer);
var
  xY, xM, xD : integer;
  FirstWeek  : EpiDate;
  FirstWeekNext  : EpiDate;
begin
  {Notes: an ISO date is defined by the year, week number and day
          within the week. A week starts on a Monday and this is day 1
          (hence Sunday is day 7). The first week of the year is the
          one that contains the first Thursday of the year.
          Clever Stuff Dept: Week 1 of year Y starts on the first
          Monday after 28 December, (Y-1); of course, *that* is 4 days
          before 1 January, Y}
  EpiDateToYMD(aDate, xY, xM, xD);
  FirstWeek := EpiNextDayOfWeek(FirstJanuarys^[xY-MinYear]-4, EpiMonday);
  if (aDate < FirstWeek) then begin
    dec(xY);
    FirstWeek := EpiNextDayOfWeek(FirstJanuarys^[xY-MinYear]-4, EpiMonday);
  end
  else begin
    FirstWeekNext := EpiNextDayOfWeek(FirstJanuarys^[xY-MinYear+1]-4, EpiMonday);
    if (aDate >= FirstWeekNext) then begin
      inc(xY);
      FirstWeek := FirstWeekNext;
    end;
  end;
  Y := xY;
  W := succ((aDate - FirstWeek) div 7);
  D := succ((aDate - FirstWeek) mod 7);
end;
{--------}
function EpiDateToStr(aDate : EpiDate; aFormat : TEpiDateFormat;aLen:integer=10) : string;
var
  Y, M, D : integer;
  DOW     : TEpiDOW;
begin
  EpiDateToYMD(aDate, Y, M, D);
  if aFormat = dfWindows then
    aFormat := WindowsDateFormat;
  case aFormat of
    dfLotus :
      begin
        Result := Format('%2d-%s-%d',
                         [D, ShortMonthNames[M], Y]);
      end;
    dfLotusDOW :
      begin
        DOW := EpiDayOfWeek(aDate);
        Result := Format('%s %2d-%s-%d',
                         [ShortDayNames[succ(ord(DOW))],
                          D, ShortMonthNames[M], Y]);
      end;
    dfDMY :
      begin
      case aLen of
      10 :  Result := Format('%2d-%2d-%d', [D, M, Y]);
      8:    Result := Format('%2d-%2d-', [D, M]) + copy(inttostr(Y),3,2);
      5:    Result := Format('%2d-%2d', [D, M]);
      end;
       if Result[1] = ' ' then
            Result[1] := '0';
       if Result[4] = ' ' then
            Result[4] := '0';
       Result[3] := EpiDateSeparator;
       Result[6] := EpiDateSeparator;
      end;
    dfMDY :
      begin
      case aLen of
      10 :  Result := Format('%2d-%2d-%d', [M, D, Y]);
      8:    Result := Format('%2d-%2d-', [M,D]) + copy(inttostr(Y),3,2);
      5:    Result := Format('%2d-%2d', [M,D]);
      end;
       if Result[1] = ' ' then
            Result[1] := '0';
       if Result[4] = ' ' then
            Result[4] := '0';
        Result[3] := EpiDateSeparator;
        Result[6] := EpiDateSeparator;
      end;
    dfYMD :
      begin
        Result := Format('%d-%2d-%2d', [Y, M, D]);
        if Result[6] = ' ' then
            Result[6] := '0';
        if Result[9] = ' ' then
            Result[9] := '0';
        Result[5] := EpiDateSeparator;
        Result[8] := EpiDateSeparator;
      end;
  else
    Result := '';
  end;
end;
{--------}
function EpiDateToStDate(aDate : EpiDate) : longint;
begin
  if (aDate < 0) or (aDate > MaxDate) then
    raise EEpiDateException.Create('invalid date');
  Result := aDate - MagicStDate;
end;
{--------}
function EpiDateToTDateTime(aDate : EpiDate) : TDateTime;
begin
  if ((aDate < 0) or (aDate > MaxDate)) {and (TODO : how to handle missing)} then
    raise EEpiDateException.Create('invalid date');
  Result := aDate - MagicTDateTime;
end;
{--------}
procedure EpiDateToYMD(aDate : EpiDate; var Y, M, D : integer);
{.$DEFINE SequentialSearch}
{.$DEFINE BinarySearch}
{$DEFINE InterpolationSearch}
var
  Inx : integer;
  IsLeap : boolean;
  {$IFDEF SequentialSearch}
  FoundIt : boolean;
  {$ENDIF}
  {$IFDEF BinarySearch}
  FoundIt : boolean;
  L, Mid, R : integer;
  {$ENDIF}
begin
  if (aDate < 0) or (aDate > MaxDate) then
    raise EEpiDateException.Create('invalid date');
  {$IFDEF SequentialSearch}
  FoundIt := true;
  for Inx := 0 to 400 do
    if (aDate < FirstJanuarys^[Inx]) then begin
      FoundIt := true;
      Break;
    end;
  if FoundIt then
    dec(Inx)
  else
    Inx := 399;
  {$ENDIF}
  {$IFDEF BinarySearch}
  FoundIt := false;
  L := 0;
  R := 400;
  while (L <= R) do begin
    Mid := (L + R) div 2;
    if (aDate < FirstJanuarys^[Mid]) then
      R := pred(Mid)
    else if (aDate > FirstJanuarys^[Mid]) then
      L := succ(Mid)
    else {equal} begin
      FoundIt := true;
      Break;
    end;
  end;
  if FoundIt then
    Inx := Mid
  else
    Inx := L-1;
  {$ENDIF}
  {$IFDEF InterpolationSearch}
  {use interpolation search to calculate 1 January, & hence the year}
  Inx := aDate div 365;
  if (aDate < FirstJanuarys^[Inx]) then
    dec(Inx);
  {$ENDIF}
  Y := MinYear + Inx;
  IsLeap := ((Inx mod 4) = 0) and
            (Inx <> 0) and (Inx <> 100) and (Inx <> 300);
  {use interpolation search to calculate the month}
  aDate := aDate - FirstJanuarys^[Inx];
  Inx := (aDate div 32) + 1;
  if (aDate < CumulativeDays^[IsLeap, Inx]) then
    dec(Inx);
  M := succ(Inx);
  {calculate the day}
  D := aDate - CumulativeDays^[IsLeap, Inx] + 1;
end;
{--------}
function EpiDayOfWeek(aDate : EpiDate) : TEpiDOW;
begin
  if (aDate < 0) or (aDate > MaxDate) then
    raise EEpiDateException.Create('invalid date');
  Result := TEpiDOW((aDate + ord(DOW18000101)) mod 7);
end;

function EpiDayofWeekMon(aDate : EpiDate) : integer;
var
  d :TEpiDOW;
begin
  d := EpiDayOfWeek(aDate);
  result:=ord(d);
  IF Result=0 THEN Result:=6 ELSE DEC(Result);
  result:=result+1;
end;


{--------}
function EpiDaysInMonth(Y, M : integer) : integer;
begin
  if (Y < MinYear) or (Y > MaxYear) or
     (M < 1) or (M > 12) then
    raise EEpiDateException.Create('invalid year and/or month');
  if (M = 2) and IsLeapYearPrim(Y) then
    Result := DaysInLeapFeb
  else
    Result := DaysInMonth[M];
end;
{--------}
function EpiGregDateToDate(aDate : longint) : EpiDate;
var
  Y, M, D : integer;
  s : string;
  begin
  s := '';
  Y := aDate div 10000;
  M := (aDate mod 10000) div 100;
  D := aDate mod 100;
  Result := EpiYMDToDate(-1,Y, M, D);
end;
{--------}
function EpiIsDayOfWeek(aDate : EpiDate; aDOW : TEpiDOW) : boolean;
begin
  Result := EpiDayOfWeek(aDate) = aDOW;
end;
{--------}
function EpiIsLeapYear(Y : integer) : boolean;
begin
  if (Y < MinYear) or (Y > MaxYear) then
    raise EEpiDateException.Create('invalid year, should be 1800-2199');
  Result := ((Y mod 4) = 0) and
            (Y <> 1800) and (Y <> 1900) and (Y <> 2100);
end;
{--------}
function EpiISODateToDate(Y, W, D : integer) : EpiDate;
var
  FirstWeek : EpiDate;
begin
  {Notes: an ISO date is defined by the year, week number and day
          within the week. A week starts on a Monday and this is day 1
          (hence Sunday is day 7). The first week of the year is the
          one that contains the first Thursday of the year.
          Clever Stuff Dept: Week 1 of year Y starts on the first
          Monday after 28 December, (Y-1); of course, *that* is 4 days
          before 1 January, Y}
  if (Y < MinYear) or (Y > MaxYear) then
    raise EEpiDateException.Create('invalid year, should be 1800-2199');
  if (W < 1) or (W > 53) then
    raise EEpiDateException.Create('invalid week, should be 1-53');
  if (D < 1) or (D > 7) then
    raise EEpiDateException.Create('invalid day, should be 1 (Monday) to 7 (Sunday)');
  FirstWeek := EpiNextDayOfWeek(FirstJanuarys^[Y-MinYear]-4, EpiMonday);
  Result := FirstWeek + ((W - 1) * 7) + (D - 1);
end;
{--------}
function EpiIsValidDate(aDate : EpiDate) : boolean;
begin
  Result := (0 <= aDate) and (aDate <= MaxDate);
end;
{--------}
function EpiIsValidYMD(Y, M, D : integer) : boolean;
begin
  Result := false;
  {easy checks}
  if (Y < MinYear) or (Y > MaxYear) then Exit;
  if (M < 1) or (M > 12) then Exit;
  if (D < 1) then Exit;
  {full check on day}
  if (D > 28) then begin
    {if February..}
    if (M = 2) then begin
      {if leap year..}
      if ((Y mod 4) = 0) and
         (Y <> 1800) and (Y <> 1900) and (Y <> 2100) then begin
        if (D > DaysInLeapFeb) then Exit;
      end
      else
        if (D > DaysInFeb) then Exit;
    end
    else
      if (D > DaysInMonth[M]) then Exit;
  end;
  {otherwise it's OK}
  Result := true;
end;
{--------}
function EpiLongDayName(aDOW : TEpiDOW) : string;
begin
  Result := LongDayNames[succ(ord(aDOW))];
end;
{--------}
function DateFmtToEpiDateFmt(DateFmt: string) :TEpiDateFormat;
begin
  result := dfWindows;
  if DateFmt = '%DMY' then result := dfDMY;
  if DateFmt = '%MDY' then result := dfMDY;
  if DateFmt = '%YMD' then result := dfYMD;
end;
{--------}
function EpiNextDayOfWeek(aDate : EpiDate; aDOW : TEpiDOW) : EpiDate;
var
  ThisDOW : TEpiDOW;
begin
  ThisDOW := EpiDayOfWeek(aDate);
  Result := aDate + (ord(aDOW) - ord(ThisDOW));
  if (ThisDOW >= aDOW) then
    inc(Result, 7);
  if (Result < 0) or (Result > MaxDate) then
    raise EEpiDateException.Create('calculated date out of range');
end;
{--------}
function EpiPrevDayOfWeek(aDate : EpiDate; aDOW : TEpiDOW) : EpiDate;
var
  ThisDOW : TEpiDOW;
begin
  ThisDOW := EpiDayOfWeek(aDate);
  Result := aDate + (ord(aDOW) - ord(ThisDOW));
  if (ThisDOW <= aDOW) then
    dec(Result, 7);
  if (Result < 0) or (Result > MaxDate) then
    raise EEpiDateException.Create('calculated date out of range');
end;
{--------}
function EpiShortDayName(aDOW : TEpiDOW) : string;
begin
  Result := ShortDayNames[succ(ord(aDOW))];
end;
{--------}
function EpiStDateToDate(aDate : longint) : EpiDate;
begin
  Result := aDate + MagicStDate;
  if (Result < 0) or (Result > MaxDate) then
    raise EEpiDateException.Create('invalid date');
end;
{--------}
function EpiTDateTimeToDate(aDate : TDateTime) : EpiDate;
begin
  Result := Trunc(aDate) + MagicTDateTime;
  if (Result < 0) or (Result > MaxDate) then
    raise EEpiDateException.Create('invalid date');
end;
{--------}
function EpiToday : EpiDate;
{$IFDEF Windows}
assembler;
asm
  mov ah, 2Ah       {get date from DOS}
  int 21h
  push cx           {push year}
  xor ax, ax
  mov al, dh
  push ax           {push month}
  mov al, dl
  push ax           {push day}
  call EpiYMDToDate  {convert}
end;
{$ELSE}
var
  SystemTime: TSystemTime;
  s: string;
begin
  GetLocalTime(SystemTime);
  with SystemTime do
    Result := EpiYMDToDate(-1,wYear, wMonth, wDay);
end;
{$ENDIF}
{--------}
function EpiYMDToDate(OBS,Y, M, D : integer) : EpiDate;
var
  IsLeap : boolean;
begin
  if not EpiIsValidYMD(Y, M, D) then
        if obs = -1 then
          dm.Info('Invalid year %d, month %d, day %d', [Y, M, D], 221001)
        else
          dm.Info('Obs. %d Invalid year %d, month %d, day %d', [obs,Y, M, D], 221002);
  //  raise EEpiDateException.Create(Format('invalid year %d, month %d, day %d',[Y, M, D]));

  IsLeap := ((Y mod 4) = 0) and
            (Y <> 1800) and (Y <> 1900) and (Y <> 2100);
  Result := FirstJanuarys^[Y-MinYear] +
            CumulativeDays^[IsLeap, pred(M)] +
            pred(D);
end;


Function EpiStrToDatefmt(const s,fmt:String):Epidate;
var
  sf, ns :string;
  typ :TEpiDateFormat;
//  typ: TFeltTyper;
begin
{  sf :=AnsiUppercase(trim(fmt));
  if sf= '' then typ :=dfdmy
  else if sf='DMY' then typ :=dfdmy
  else if sf='MDY' then typ:=dfmdy
  else if sf='YMD' then typ :=dfymd
  else if sf='V' then typ := dfdmy
  else if sf='MDYV' then typ:=dfmdy
  else if sf='YMDV' then typ :=dfymd
  else raise EEpiDateException.createfmt('Invalid format: %s',[fmt]);
  EpiStrToDate(s,Result,typ);
  if (result = NA_Date) and (pos('V',sf) > 0) then
     result :=EpiStrToDatefmt('01/01/1800','DMY');  }

  sf := AnsiUppercase(trim(fmt));
  ns := s;
  if sf= '' then typ :=dfdmy
  else if sf='%DMY' then typ :=dfdmy
  else if sf='%MDY' then typ:=dfmdy
  else if sf='%YMD' then typ :=dfymd
  else if sf='%V' then typ := dfdmy
  else if sf='%MDYV' then typ:=dfmdy
  else if sf='%YMDV' then typ :=dfymd
  else raise EEpiDateException.createfmt('Invalid format: %s',[fmt]);
  mibIsDate(ns, ftDate);
  EpiStrToDate(ns,Result,typ);
  if (result = NA_Date) and (pos('V',sf) > 0) then
     result :=EpiStrToDatefmt('01/01/1800','DMY');     
end;



Function EpiStrToDate(const s:String;var Value:Epidate;Typ:TEpiDateFormat):boolean;
{#TODO2 optimize string processing}
{#TODO2 Handle errors from strtoint conversion}
var
 eYear,eMonth,eDay,alen :integer;
 ys : string;
 IsLeap : boolean;
BEGIN
  Value:=NA_DATE;
  Result:=false;
  aLen :=length(s);
  Case aLen OF
    5:  eYear := fThisYear;
    8:
    begin
    case typ of
       dfMDY,dfDMY:  ys:= Copy(s,7,2);
       dfYMD:        ys:= Copy(s,1,2);
    end;//case typ
    eyear:= StrToIntdef(ys,-1);
    if eyear= -1 then exit;
    if eyear <50 then
          eYear:=2000+ eyear
      else
          eYear:=1900+ eyear;
    end;//8:
    10:
    begin
    case typ of
        dfMDY,dfDMY: ys:= Copy(s,7,4);
        dfYMD:       ys:= Copy(s,1,4);
    end;//case typ
    eyear:= StrToIntdef(ys,-1);
    if eyear= -1 then exit;
//       raise EEpiDateException.create('Invalid date');
    end;
  END;  //case
  CASE typ OF
    dfMDY: BEGIN
      eMonth:=StrToIntdef(Copy(s,1,2),-1);
      eDay:=StrToIntdef(Copy(s,4,2),-1);
      END;
    dfDMY: BEGIN
      eMonth:=StrToIntdef(Copy(s,4,2),-1);
      eDay:=StrToIntdef(Copy(s,1,2),-1);
      END;
     dfYMD: BEGIN
     case alen of
     8:
     begin
        eMonth:=StrToIntdef(Copy(s,4,2),-1);
        eDay:=StrToIntdef(Copy(s,7,2),-1);
      END;
     10:
     begin
        eMonth:=StrToIntdef(Copy(s,6,2),-1);
        eDay:=StrToIntdef(Copy(s,9,2),-1);
      END;
    end;//case alen
    end;//dfYMD:
  END;  //case
  if not EpiIsValidYMD(eYear, eMonth, eDay) then exit;
  IsLeap := ((eYear mod 4) = 0) and
            (eYear <> 1800) and (eYear <> 1900) and (eYear <> 2100);
  Value := FirstJanuarys^[eYear-MinYear] +
            CumulativeDays^[IsLeap, pred(eMonth)] + pred(eDay);
  Result:=true;
END;

function DatePart(aDate: EpiDate;part: word):integer;
var
  Y, M, D : integer;
begin
  result:=0;
  EpiDateToYMD(aDate, Y, M, D);
  case part of
    1 : result:=D;
    2 : result:=M;
    3 : result:=Y;
  end;
end;

Function dkDayOfWeek(ADate: TDateTime):Integer;
{Makes DayOfWeek where monday=1, sunday=7}
BEGIN
  Result:=DayOfWeek(ADate);
  IF Result=1 THEN Result:=7 ELSE DEC(Result);
END;   //dkDayOfWeek


FUNCTION WeekNum(ADate: TDateTime):Integer;
VAR
  midWeekNum:Integer;
  FirstDayWeekOne:TDateTime;
  CurY,CurM,curD:Word;

BEGIN  //WeekNum
  DecodeDate(ADate,CurY,CurM,CurD);
  FirstDayWeekOne:=EncodeDate(CurY,1,4)-
                   (dkDayOfWeek(EncodeDate(CurY,1,4))-1);
  IF ADate<FirstDayWeekOne THEN
    BEGIN
      MidWeekNum:=52;
      {Prev. year has 53 weeks if 1st January is a Thursday
       or if previous year is a leap year and 1st January is a Wednessday}
      IF (dkDayOfWeek(EncodeDate(CurY-1,1,1))=4)
          OR ( (dkDayOfWeek(EncodeDate(CurY-1,1,1))=3)
               AND (IsLeapYear(CurY-1)) )
          THEN INC(MidWeekNum);
    END
  ELSE
    BEGIN
      MidWeekNum:=Trunc((ADate-FirstDayWeekOne)/7)+1;
      IF MidWeekNum=53 THEN
        BEGIN  //Has current year 53 weeks?
          IF (dkDayOfWeek(EncodeDate(CurY,1,1))=4)
              OR ( (dkDayOfWeek(EncodeDate(CurY,1,1))=3)
                   AND (IsLeapYear(CurY)) )
              THEN MidWeekNum:=53 ELSE MidWeekNum:=1;
        END;  //if MidWeekNum=53
    END;   //if not ADate<FirstDayWeekOne
  WeekNum:=MidWeekNum;
END;   //WeekNum


FUNCTION EpiWeekNum(ADate: EpiDate):Integer;
begin
  result:= WeekNum(EpiDateToTDateTime(Adate));
end;


{====================================================================}
type

  TEpiHolidayList = class
    private
      FList   : TList;
      FWEDays : array [EpiSunday..EpiSaturday] of boolean;
    protected
      function hlGetHolidayCount : integer;
      function hlGetItem(aInx : integer) : EpiDate;
      function hlGetWeekend(aDOW : TEpiDOW) : boolean;
      procedure hlSetWeekend(aDOW : TEpiDOW; aValue : boolean);

      procedure hlStreamRead(aStream : TStream;
                         var aBuffer; aCount : longint);
      procedure hlStreamWrite(aStream : TStream;
                          var aBuffer; aCount : longint);
    public
      constructor Create;
        {-create instance, set weekend to Sat/Sun}
      destructor Destroy; override;
        {-free instance}

      procedure AddHoliday(aDate : EpiDate);
        {-add a new holiday}
      procedure Clear;
        {-clear all holidays, set weekend to Sat/Sun}
      procedure ClearBefore(aDate : EpiDate);
        {-clear all holidays before a certain date, leave weekend}
      procedure DeleteHoliday(aDate : EpiDate);
        {-delete a single holiday (no error if not there)}

      function IsBusinessDay(aDate : EpiDate) : boolean;
        {-return true if the day is not a weekend or holiday}
      function BusinessDaysDiff(aDate1, aDate2 : EpiDate) : integer;
        {-return the number of business days between aDate1 and
          aDate2; aDate1 <= aDate2, otherwise they're swapped over;
          count starts from aDate+1}
      function NextBusinessDay(aDate : EpiDate) : EpiDate;
        {-return the next business day from aDate}
      function PrevBusinessDay(aDate : EpiDate) : EpiDate;
        {-return the previous business day from aDate}
      function NearestBusinessDay(aDate      : EpiDate;
                                  aSameMonth : boolean) : EpiDate;
        {-return the nearest business day to aDate; is aDate is a
          business day then it is returned, otherwise the next
          business day from aDate is returned; if aSameMonth is true,
          the date returned is forced to be in the same month as
          aDate.}

      procedure LoadFromStream(aStream : TStream);
        {-clear object, load data from stream}
      procedure StoreToStream(aStream : TStream);
        {-store objact data to stream}

      property Weekend[aDOW : TEpiDOW] : boolean
         read hlGetWeekend write hlSetWeekend;
        {-for each day: weekend day if true, normal day if false}
      property HolidayCount : integer
         read hlGetHolidayCount;
      property Holidays[aInx : integer] : EpiDate
         read hlGetItem; default;
        {-holiday date list (sorted)}
  end;


{===TEpiHolidayList===================================================}
constructor TEpiHolidayList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FWEDays[EpiSaturday] := true;
  FWEDays[EpiSunday] := true;
end;
{--------}
destructor TEpiHolidayList.Destroy;
begin
  FList.Free;
  inherited Create;
end;
{--------}
procedure TEpiHolidayList.AddHoliday(aDate : EpiDate);
var
  L, R, M : integer;
  MidDate : EpiDate;
begin
  if (FList.Count = 0) then
    FList.Add(pointer(aDate))
  else begin
    {find aDate in the list by binary search, if found, exit, if not
     insert at the correct spot}
    L := 0;
    R := pred(FList.Count);
    while L <= R do begin
      M := (L + R) div 2;
      MidDate := EpiDate(FList[M]);
      if (aDate < MidDate) then
        R := pred(M)
      else if (aDate > MidDate) then
        L := succ(M)
      else {they're equal}
        Exit;
    end;
    FList.Insert(L, pointer(aDate));
  end;
end;
{--------}
function TEpiHolidayList.BusinessDaysDiff(aDate1, aDate2 : EpiDate) : integer;
var
  TempDate : EpiDate;
begin
  {make sure that aDate1 is less than aDate2}
  if (aDate1 > aDate2) then begin
    TempDate := aDate1;
    aDate1 := aDate2;
    aDate2 := TempDate;
  end;
  {count the business days from aDate1 to aDate2 inclusive}
  Result := 0;
  inc(aDate1);
  while (aDate1 <= aDate2) do begin
    if IsBusinessDay(aDate1) then
      inc(Result);
    inc(aDate1);
  end;
end;
{--------}
procedure TEpiHolidayList.Clear;
begin
  FList.Clear;
  FillChar(FWEDays, sizeof(FWEDays), 0);
  FWEDays[EpiSaturday] := true;
  FWEDays[EpiSunday] := true;
end;
{--------}
procedure TEpiHolidayList.ClearBefore(aDate : EpiDate);
var
  L, R, M : integer;
  MidDate : EpiDate;
  PointerList : PPointerList;
begin
  if (FList.Count > 0) then begin
    {find aDate in the list by binary search}
    L := 0;
    R := pred(FList.Count);
    while L <= R do begin
      M := (L + R) div 2;
      MidDate := EpiDate(FList[M]);
      if (aDate < MidDate) then
        R := pred(M)
      else if (aDate > MidDate) then
        L := succ(M)
      else {they're equal} begin
        L := M;
        Break;
      end;
    end;
    {we now have to delete all entries prior to L}
    if (L > 0) then begin
      PointerList := FList.List;
      Move(PointerList^[L],
           PointerList^[0],
           (FList.Count - L) * sizeof(pointer));
      FList.Count := FList.Count - L;
    end;
  end;
end;
{--------}
procedure TEpiHolidayList.DeleteHoliday(aDate : EpiDate);
var
  L, R, M : integer;
  MidDate : EpiDate;
begin
  if (FList.Count > 0) then begin
    {find aDate in the list by binary search and delete it}
    L := 0;
    R := pred(FList.Count);
    while L <= R do begin
      M := (L + R) div 2;
      MidDate := EpiDate(FList[M]);
      if (aDate < MidDate) then
        R := pred(M)
      else if (aDate > MidDate) then
        L := succ(M)
      else {they're equal} begin
        FList.Delete(M);
        Exit;
      end;
    end;
  end;
end;
{--------}
function TEpiHolidayList.hlGetHolidayCount : integer;
begin
  Result := FList.Count;
end;
{--------}
function TEpiHolidayList.hlGetItem(aInx : integer) : EpiDate;
begin
  Result := EpiDate(FList[aInx]);
end;
{--------}
function TEpiHolidayList.hlGetWeekend(aDOW : TEpiDOW) : boolean;
begin
  Result := FWEDays[aDOW];
end;
{--------}
procedure TEpiHolidayList.hlSetWeekend(aDOW : TEpiDOW; aValue : boolean);
begin
  FWEDays[aDOW] := aValue;
end;
{--------}
procedure TEpiHolidayList.hlStreamRead(aStream : TStream;
                                  var aBuffer; aCount : longint);
var
  BytesRead : longint;
begin
  BytesRead := aStream.Read(aBuffer, aCount);
  if (BytesRead <> aCount) then
    raise EEpiDateException.Create('not enough bytes read');
end;
{--------}
procedure TEpiHolidayList.hlStreamWrite(aStream : TStream;
                                   var aBuffer; aCount : longint);
var
  BytesWrit : longint;
begin
  BytesWrit := aStream.Write(aBuffer, aCount);
  if (BytesWrit <> aCount) then
    raise EEpiDateException.Create('not enough bytes written');
end;
{--------}
function TEpiHolidayList.IsBusinessDay(aDate : EpiDate) : boolean;
var
  DOW : TEpiDOW;
  L, R, M : integer;
  MidDate : EpiDate;
begin
  Result := true;
  {first calculate the day of the week and check whether it's a
   weekend day}
  DOW := EpiDayOfWeek(aDate);
  if FWEDays[DOW] then
    Result := false
  {otherwise, try to find the date in the holiday list}
  else if (FList.Count <> 0) then begin
    L := 0;
    R := pred(FList.Count);
    while L <= R do begin
      M := (L + R) div 2;
      MidDate := EpiDate(FList[M]);
      if (aDate < MidDate) then
        R := pred(M)
      else if (aDate > MidDate) then
        L := succ(M)
      else {they're equal} begin
        Result := false;
        Exit;
      end;
    end;
  end;
end;
{--------}
procedure TEpiHolidayList.LoadFromStream(aStream : TStream);
var
  Count : integer;
  PointerList : PPointerList;
begin
  hlStreamRead(aStream, FWEDays, sizeof(FWEDays));
  hlStreamRead(aStream, Count, sizeof(Count));
  FList.Count := Count;
  PointerList := FList.List;
  hlStreamRead(aStream, PointerList^, Count * sizeof(pointer));
end;
{--------}
function TEpiHolidayList.NearestBusinessDay(aDate      : EpiDate;
                                           aSameMonth : boolean) : EpiDate;
var
  Y1, M1, D1 : integer;
  Y2, M2, D2 : integer;
begin
  if IsBusinessDay(aDate) then
    Result := aDate
  else begin
    Result := succ(aDate);
    while not IsBusinessDay(Result) do
      Result := succ(Result);
    if aSameMonth then begin
      EpiDateToYMD(aDate, Y1, M1, D1);
      EpiDateToYMD(Result, Y2, M2, D2);
      if (M1 <> M2) then begin
        Result := pred(Result);
        while not IsBusinessDay(Result) do
          Result := pred(Result);
      end;
    end;
  end;
end;
{--------}
function TEpiHolidayList.NextBusinessDay(aDate : EpiDate) : EpiDate;
begin
  Result := succ(aDate);
  while not IsBusinessDay(Result) do
    Result := succ(Result);
end;
{--------}
function TEpiHolidayList.PrevBusinessDay(aDate : EpiDate) : EpiDate;
begin
  Result := pred(aDate);
  while not IsBusinessDay(Result) do
    Result := pred(Result);
end;
{--------}
procedure TEpiHolidayList.StoreToStream(aStream : TStream);
var
  Count : integer;
  PointerList : PPointerList;
begin
  hlStreamWrite(aStream, FWEDays, sizeof(FWEDays));
  Count := FList.Count;
  hlStreamWrite(aStream, Count, sizeof(Count));
  PointerList := FList.List;
  hlStreamWrite(aStream, PointerList^, Count * sizeof(pointer));
end;
{====================================================================}


{===Initialization and finalization==================================}
procedure InitFirstJans;
var
  NextValue : longint;
  Year      : integer;
begin
  {allocate the memory}
  New(FirstJanuarys);
  {initialize the values}
  NextValue := 0;
  for Year := MinYear to MaxYear do begin
    FirstJanuarys^[Year-MinYear] := NextValue;
    if EpiIsLeapYear(Year) then
      inc(NextValue, 366)
    else
      inc(NextValue, 365)
  end;
  FirstJanuarys^[400] := NextValue;
end;
{--------}
procedure InitCumulativeDays;
var
  NextValue : longint;
  Month     : integer;
begin
  {allocate the memory}
  New(CumulativeDays);
  {initialize the non-leap year values}
  NextValue := 0;
  for Month := 1 to 12 do begin
    CumulativeDays^[false, pred(Month)] := NextValue;
    inc(NextValue, DaysInMonth[Month]);
  end;
  CumulativeDays^[false, 12] := NextValue;
  {initialize the non-leap year values}
  NextValue := 0;
  for Month := 1 to 12 do begin
    CumulativeDays^[true, pred(Month)] := NextValue;
    if (Month = 2) then
      inc(NextValue, DaysInLeapFeb)
    else
      inc(NextValue, DaysInMonth[Month]);
  end;
  CumulativeDays^[true, 12] := NextValue;
end;
{--------}
procedure CalcWindowsDateFormat;
var
  i : integer;
begin
  {simple calculation}
  for i := 1 to length(ShortDateFormat) do
    case ShortDateFormat[i] of
      'd' : begin
              WindowsDateFormat := dfDMY;
              Exit;
            end;
      'm' : begin
              WindowsDateFormat := dfMDY;
              Exit;
            end;
      'y' : begin
              WindowsDateFormat := dfYMD;
              Exit;
            end;
    end;
  WindowsDateFormat := dfDMY;
end;
{--------}
procedure FinalizeUnit; far;
begin
  if (FirstJanuarys <> nil) then
    Dispose(FirstJanuarys);
  if (CumulativeDays <> nil) then
    Dispose(CumulativeDays);
end;
{====================================================================}

procedure GetCurrentYear;
var
  m,d : word;
begin
  decodedate(date, fThisYear,m,d);
end;

initialization
  GetCurrentYear;
  FirstJanuarys := nil;
  CumulativeDays := nil;
  InitFirstJans;
  InitCumulativeDays;
  CalcWindowsDateFormat;
  {$IFDEF Windows}
  AddExitProc(FinalizeUnit);
  {$ENDIF}

{$IFDEF Win32}
finalization
  FinalizeUnit;
{$ENDIF}



end.
