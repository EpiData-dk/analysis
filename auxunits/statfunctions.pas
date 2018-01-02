unit statfunctions;

{$codepage UTF-8}
{$mode delphi}

interface

uses sysutils, Math, epidatafilestypes;

function EpiProportionCI(n,d: EpiInteger; var UL, LL: EpiFloat): string;

// 2x2 estimators - should be coordinated with epiinfostats.
Function CornFieldLimit(A, B, C, D: EpiFloat; WhichLim: integer; var inexact : boolean): EpiFloat;
function EpiPower(base,power : extended ): extended;

FUNCTION betai(a,b,x: EpiFloat): EpiFloat;
FUNCTION gammln(xx: EpiFloat): EpiFloat;
FUNCTION betacf(a,b,x: EpiFloat): EpiFloat;
function tdist(t : EpiFloat; df : EpiFloat) : EpiFloat;
function fdist(f : EpiFloat; d1, d2 : EpiFloat) : EpiFloat;

//function EpiLog10 (const X: Extended): Extended;

//function EpiRND(x: Epiint):EpiFloat;
//function EpiRND(x: EpiInteger):EpiFloat;
//function EpiRAN(x: EpiInteger):EpiInteger;
function EpiRanG(Mean, StdDev: Extended): Extended;

Function ChiPValue (ChiSqr : epifloat; Degrees: Integer) : epifloat;
function chisquaredprob(X : extended; k : integer) : extended;

/// functions and procedures added from CHolst/Tue Tjur spdist.pas
{---------------------------------------------------------------------------}
FUNCTION LNGAMMA(F:LONGINT):EXTENDED;
      { COMPUTES THE LOGARITHM OF THE GAMMA FUNCTION AT F/2. }


FUNCTION PGAMMA(F:LONGINT;Y:EXTENDED):EXTENDED;
      { RETURNS THE RIGHT TAIL PROBABILITY IN THE GAMMA
        DISTRIBUTION WITH LAMBDA = F/2. }


FUNCTION PNORMAL(U:EXTENDED):EXTENDED;
      { RETURNS THE RIGHT TAIL PROBABILITY IN THE NORMAL DISTRIBUTION. }


FUNCTION PCHI2(F:LONGINT;Y:EXTENDED):EXTENDED;
      { RETURNS THE RIGHT TAIL PROBABILITY IN THE CHI SQUARE DISTRIBUTION
        WITH F DEGREES OF FREEDOM. }


FUNCTION PBETA(F1,F2:LONGINT;Y:EXTENDED):EXTENDED;
      { RETURNS THE LEFT TAIL PROBABILITY IN THE BETA DISTRIBUTION
        WITH PARAMTERS LAMBDA1=F1/2 AND LAMBDA2=F2/2. USE ONLY
        F1 AND F2 < 1E6.  }


FUNCTION PFDISTR(F1,F2:LONGINT;Y:EXTENDED):EXTENDED; overload;
      { RETURNS THE RIGHT TAIL PROBABILITY IN THE F DISTRIBUTION
        WITH (F1,F2) DEGREES OF FREEDOM.
        USE ONLY F1 AND F2 < 1E6. }


FUNCTION PTDISTR(F:LONGINT;Y:EXTENDED):EXTENDED; overload;
      { RETURNS THE RIGHT TAIL PROBABILITY IN THE T DISTRIBUTION.
        USE ONLY  F < 1E6. }


FUNCTION PNORMALINV(P:EXTENDED):EXTENDED;
      { INVERSE OF PNORMAL. }


FUNCTION PGAMMAINV(F:LONGINT;P:EXTENDED):EXTENDED;
      { INVERSE OF PGAMMA(F,*). }



FUNCTION PCHI2INV(F:LONGINT;P:EXTENDED):EXTENDED;
      { INVERSE OF PCHI2(F,*). }


FUNCTION PBETAINV(F1,F2:LONGINT;P:EXTENDED):EXTENDED;
      { INVERSE OF PBETA(F1,F2,*) (NOTICE: LEFT TAIL). }


FUNCTION PFDISTRINV(F1,F2:LONGINT;P:EXTENDED):EXTENDED; overload;
      { 1-P PERCENTILE OF F DISTRIBUTION. }

FUNCTION PTDISTRINV(F:LONGINT;P:EXTENDED):EXTENDED; overload;
      { 1-P PERCENTILE OF T DISTRIBUTION. }


FUNCTION PPOISS(LAMBDA:EXTENDED; N:LONGINT): EXTENDED;
      { RETURNS THE RIGHT TAIL PROBABILITY IN THE POISSON DISTRIBUTION. }


FUNCTION POISSCL(N:LONGINT; P:EXTENDED): EXTENDED;
      { LOWER 1-P CONFIDENCE LIMITS FOR LAMBDA IN POISSON DISTRIBUTION
        WHEN N IS OBSERVED. }


FUNCTION PBIN(N,X:LONGINT; P:EXTENDED): EXTENDED;
      { RETURNS THE BINOMIAL RIGHT TAIL PROBABILITY. }


FUNCTION BINCL(N,X:LONGINT; PP:EXTENDED): EXTENDED;
      { RETURNS CONFIDENCE LIMIT FOR BINOMIAL PROBABILITY PARAMETER,
        I.E. INVERSE TO PBIN(N,*). }

{---------------------------------------------------------------------------}

const
// EpiTolerance : Extended = MinExtended;
 //ConfScore=1.960;
 ConfScore=1.645;


implementation

function EpiPower(base,power : extended ): extended;
begin
     if power = 0 then Result := 1.0
     else if power < 0 then Result := 1 / EpiPower(base,-power)
     else Result := exp(power * ln(base))
end;

function betacf(a, b, x: EpiFloat): EpiFloat;
LABEL 1;
CONST
   itmax=100;
   eps=3.0e-7;
VAR
   tem,qap,qam,qab,em,d: real;
   bz,bpp,bp,bm,az,app: real;
   am,aold,ap: real;
   m: integer;
BEGIN
   am := 1.0;
   bm := 1.0;
   az := 1.0;
   qab := a+b;
   qap := a+1.0;
   qam := a-1.0;
   bz := 1.0-qab*x/qap;
   FOR m := 1 TO itmax DO BEGIN
      em := m;
      tem := em+em;
      d := em*(b-m)*x/((qam+tem)*(a+tem));
      ap := az+d*am;
      bp := bz+d*bm;
      d := -(a+em)*(qab+em)*x/((a+tem)*(qap+tem));
      app := ap+d*az;
      bpp := bp+d*bz;
      aold := az;
      am := ap/bpp;
      bm := bp/bpp;
      az := app/bpp;
      bz := 1.0;
      IF ((abs(az-aold)) < (eps*abs(az))) THEN GOTO 1
   END;
   raise exception.create('a or b too big, or itmax too small');
1:   betacf := az
END;


function gammln(xx: EpiFloat): EpiFloat;
CONST
   stp = 2.50662827465;
   half = 0.5;
   one = 1.0;
   fpf = 5.5;
VAR
   x,tmp,ser: EpiFloat;
   j: integer;
   cof: ARRAY [1..6] OF EpiFloat;
BEGIN
   cof[1] := 76.18009173;
   cof[2] := -86.50532033;
   cof[3] := 24.01409822;
   cof[4] := -1.231739516;
   cof[5] := 0.120858003e-2;
   cof[6] := -0.536382e-5;
   x := xx-one;
   tmp := x+fpf;
   tmp := (x+half)*ln(tmp)-tmp;
   ser := one;
   FOR j := 1 TO 6 DO BEGIN
      x := x+one;
      ser := ser+cof[j]/x
   END;
   gammln := tmp+ln(stp*ser)
END;

function betai(a, b, x: EpiFloat): EpiFloat;
VAR
   bt: real;
BEGIN
   IF ((x < 0.0) OR (x > 1.0)) THEN BEGIN
      raise exception.create('Invalid value passed to betai');
   END;
   IF ((x = 0.0) OR (x = 1.0)) THEN bt := 0.0
   ELSE bt := exp(gammln(a+b)-gammln(a)-gammln(b)
           +a*ln(x)+b*ln(1.0-x));
   IF (x < ((a+1.0)/(a+b+2.0))) THEN
      betai := bt*betacf(a,b,x)/a
   ELSE betai := 1.0-bt*betacf(b,a,1.0-x)/b
END;



function tdist(t : EpiFloat; df : EpiFloat) : EpiFloat;
begin
   Result :=betai(0.5*df,0.5,df/(df+sqr(t)));
end;


function fdist(f : EpiFloat; d1, d2 : EpiFloat) : EpiFloat;
begin
     Result := betai(0.5*d2, 0.5*d1, d2/(d2+d1* f));
end;

function ChiPValue(ChiSqr: epifloat; Degrees: Integer): epifloat;
(*Adapted from Poole, Lon, and Mary Borchers. Some Common BASIC Programs.
 Berkeley, CA, Adam Osborne & Assoc., 1977.*)
Var
  I : Integer;
  Firstterm, R, Sum, M, J, P : epifloat;
Begin
  If ChiSqr <= 0.0
  Then
    ChiPValue := 1.0
{dcs 6/28/93 Should be able to calculate a p value for chi square greater than 40}
{dcs 6/23/94 This one can't}
  Else If ChiSqr > 40 Then
    ChiPValue := 0.0
  Else
    Begin
      R:=1E-20;
      I := Degrees;
      Repeat
        R := R*I;
        dec(I, 2);
      Until I < 1;
      FirstTerm := (Exp((1.0*((degrees+1) DIV 2))*
                          Ln(chisqr))*Exp(-chisqr/2)/1E+20)/r;
      If odd(degrees) then
        J := sqrt (2.0/chisqr/3.141592653559)
      else
        J := 1;
      sum := 1;
      m := 1;
      While m > 1E-10 do
        begin
          degrees := degrees +2;
          m := M*chisqr/degrees;
          sum := sum+ m
        end;
      P := 1-(J*firstterm*sum);
      ChiPValue := P;
     End;(*If ChiSqr*)
End (*ChiPValue*);


function chisquaredprob(X : extended; k : integer) : extended;
var
   factor : extended;   // factor which multiplies sum of series
   g      : extended;   // lngamma(k1+1)
   k1     : extended;   // adjusted degrees of freedom
   sum    : extended;   // temporary storage for partial sums
   term   : extended;   // term of series
   x1     : extended;   // adjusted argument of funtion
   chi2prob : extended; // chi-squared probability
begin
     // the distribution function of the chi-squared distribution based on k d.f.
     if (X < 0.01) or (X > 1000.0) then
     begin
          if X < 0.01 then chi2prob := 0.0001
          else chi2prob := 0.999;
     end
     else
     begin
    	x1 := 0.5 * X;
    	k1 := 0.5 * k;
    	g := gammln(k1 + 1);
    	factor := exp(k1 * ln(x1) - g - x1);
    	sum := 0.0;
    	if factor > 0 then
    	begin
        	term := 1.0;
          	sum := 1.0;
          	while ((term / sum) > 0.000001) do
          	begin
                     k1 := k1 + 1;
                     term  := term * (x1 / k1);
                     sum := sum + term;
                end;
        end;
    	chi2prob := sum * factor;
     end; //end if .. else
     Result := chi2prob;
end;

{
function EpiLog10 (const X: Extended): Extended;

  function FLog10 (X: Extended): Extended;
  asm
    fldlg2         // St(0) <- Log10 of 2
    fld      [X]      // St(0) <- X, St(1) <- Log10 of 2
    fyl2x         // St(0) <- log10 (2) * Log2 (X)
    fwait
  end;

  function AltFLog10 (X: Extended): Extended;
  asm
    fldlg2         // St(0) <- Log10 of 2
    fld      [X]      // St(0) <- X, St(1) <- Log10 of 2
    fyl2xp1         // St(0) <- log10 (2) * Log2 (X+1)
    fwait
  end;

begin
  if not (X >= EpiTolerance) then // must be Positive
    raise Exception.Create ('Value must be > 0')
  else if abs (X - 1) < 0.1 then
    Result := AltFLog10 (X - 1)
  else
    Result := FLog10 (X);
End;

function EpiRND(x: EpiInteger): EpiFloat;
var RandSeed: LongInt;
opt : TEpiOption;
begin
//  randomize;
 randseed := 0;
 if (DM.GetOptionValue('RANDOM SEED', opt)) THEN   RandSeed := strtoint(opt.value);
 result:=random;
end;



function EpiRAN(x: EpiInteger): EpiInteger;
var RandSeed: LongInt;
 opt : TEpiOption;
begin
//  randomize;
 randseed := 0;
  if (DM.GetOptionValue('RANDOM SEED', opt)) THEN   RandSeed := strtoint(opt.value);
  result:=random(x);
end;    }

function EpiRanG(Mean, StdDev: Extended): Extended;
begin
  result:=randg(Mean,StdDev);
end;


//formula according to Altman, Machin, Bryant, Gardner p 46, 2nd edition. BMJ Books.
function EpiProportionCI(n, d: EpiInteger; var UL, LL: EpiFloat): string;
var
  a,b,c: EpiFloat;
// opt : TEpiOption;
 begin
 //dm.info(format('n: %d   d: %d',[n,d]));
  result := '.';
  if d = 0 then exit;
  A := 2*n + 1.96*1.96;
  B := 1.96*sqrt(sqr(1.96) + 4*n*(1-(n/d)));
  C := 2*(d+sqr(1.96));
  LL:=(a-b)/c;
  UL:=(a+b)/C;
  result:=format('%3.2f - %3.2f',[LL,UL]);
end;

function CornFieldLimit(A, B, C, D: EpiFloat; WhichLim: integer;
  var inexact: boolean): EpiFloat;
{Returns Cornfield Confidence interval, upper if Whichlim is 1 and
 lower if Whichlim is -1, with confidence based on ConfScore}
{Returns zero if limits cannot be calculated. Inexact is true if KKM test is failed}
 var Approx : EpiFloat;
     M1,M0,N1,N0,Tot: EpiFloat;
     i : integer;
     W,X,Y,ExpA,ExpB,ExpC,ExpD : EpiFloat;
     Invalid : boolean;

Function FirstApprox (WhichLim: Integer) : EpiFloat;
var L,ODR, StdErrL : EpiFloat;

begin
  ODR := ((A+0.5)*(D+0.5))/((B+0.5)*(C+0.5));
  L := Ln(ODR);
  StdErrL := Sqrt((1/(A+0.5))+(1/(B+0.5))+(1/(C+0.5))+(1/(D+0.5)));
  If WhichLim = 1
  Then
    FirstApprox := Exp(L + (ConfScore * StdErrL))
  Else
    If ODR < 1   Then
      FirstApprox := ODR / 1000
    Else
{#TODO1 fix dofisher}
{      If DoFisher  Then
        FirstApprox := 0.0001
      Else}
        FirstApprox := Exp(L - (ConfScore * StdErrL))
end;

Function F (Approx : EpiFloat; WhichLim : Integer) : EpiFloat;
{Computes F for a given upper or lower limit approximation}
{Whichlim is -1 if for lower limit, + 1 if for upper}
begin

 X := (Approx*(N1+M1))+(N0-M1);
 Y := (X*X) - (4*N1*M1*Approx*(Approx-1));
 If Y >= 0 then
 begin
   Y := Sqrt(Y);
   ExpA := (X-Y)/(2*(Approx-1));
   ExpB := N1 - ExpA;
   ExpC := M1-ExpA;
   ExpD := M0 - N1 + ExpA;
   If (ExpA>0.0) and (ExpB>0.0) and (ExpC>0.0) and (ExpD>0.0)
     and (Y >0.0) then
   begin
     W := (1/ExpA)+(1/ExpB)+(1/ExpC)+(1/ExpD);
     F := ((Sqr(A - ExpA + (WhichLim/2))) * W) - 2 * ConfScore  (* Was 3.84 *);
   end
   else
     Invalid := True;
 end
 else
   F := 9999;
end;

Function NewLimit (OldLimit : EpiFloat; WhichLim : Integer) : EpiFloat;
Var T,U,V : EpiFloat;
begin
 T := (1/(2*sqr(OldLimit-1)))*(Y-Tot-(((OldLimit-1)/Y))*((X*(N1+M1))-(2*N1*M1)*((2*OldLimit)-1)));
 U := (1/Sqr(ExpB))+(1/Sqr(ExpC))-(1/Sqr(ExpA))-(1/Sqr(ExpD));
 V := T*(Sqr(A - ExpA + (Whichlim/2))*U - (2*W*(A-ExpA+(Whichlim/2))));
 NewLimit := OldLimit - (F(Approx,Whichlim)/V);
end;

Function CornValid : Boolean;
{Checks to see if the interval ExpA +- ConfScore sqrt W is >= max(0,m1-n0) and
<= min(n1,n1).  See Kleinbaum, Kupper, and Morgenstern, 1982, p. 305-6.}
Var IntL,IntH : EpiFloat;
Begin
   IntL := ExpA - (ConfScore * (1/sqrt(W)));
   IntH := ExpA + (ConfScore * (1/sqrt(W)));
   If (IntL < 0) or (IntL < (N1-M0)) or (IntH > N1) or (IntH > M1) or (Invalid)
   Then
     CornValid := False
   else
     CornValid := True
end;

begin
   Invalid := False;
    N1 := A + B;
    N0 := C + D;
    M1 := A + C;
    M0 := B + D;
    Tot := N1 + N0;
   If (((B*C) > 0) or (Whichlim = -1)) and (((A*D) > 0 ) or (Whichlim = 1)) then
      begin
      Approx := FirstApprox(WhichLim);
      i := 1;
      While (ABS(F(Approx,WhichLim)) > 0.01) and (Approx < 9E36) and
            (Invalid = False) and (i < 200) do
            begin
            Approx := NewLimit (Approx,WhichLim);
            If Approx < -10E25 then
               Invalid := True;
            Inc (i)
            end;
      If (invalid = False) or (i > 199) then
         begin
         Approx := NewLimit(Approx,WhichLim);
         If Approx < 0 then
            Result := 0
         else
             Result := Approx;
         If CornValid then
            inexact := false
         else
            inexact := True;
         end
      else
         Result := 0;
      end
   else
      Result := 0;
end;

//////////////////////////////////////

//
// cleanup from here, partial overlap

//////////////////////////////////////


function LNGAMMA(F: LONGINT): EXTENDED;

VAR SUM,TERM,Y   : EXTENDED;
    K           : LONGINT;

BEGIN   Y:=F/2;
IF F>500 THEN
    BEGIN
    SUM:= LN(2*PI)/2  + (Y-1/2)*LN(Y) -Y + 1/(12*Y);
    SUM:=SUM - 1/360/Y/Y/Y;
    LNGAMMA:=SUM;
    END
ELSE
    BEGIN
    K:=F; SUM:=0;
    WHILE K>2 DO
        BEGIN
        K:=K-2;
        SUM:=SUM+LN(K/2);
        END;
    IF K=1 THEN SUM:=SUM+LN(PI)/2;
    LNGAMMA:=SUM;
    END;
END;

{---------------------------------------------------------------------------}
function PGAMMA(F: LONGINT; Y: EXTENDED): EXTENDED;

VAR  TERM,SUM: EXTENDED;
     K      : LONGINT;

BEGIN IF (Y<=0) THEN PGAMMA:=1 ELSE
IF (Y<F/2) OR (Y<42) THEN
    BEGIN
    TERM:=(F/2)*LN(Y)-Y-LNGAMMA(F+2);
    TERM:=EXP(TERM);
    SUM:=0; K:=0;
    WHILE ((F+K)*TERM>(F+K-2*Y)*1E-20) DO
        BEGIN
        SUM:=SUM+TERM;
        TERM:=2*TERM*Y/(F+K+2);
        K:=K+2;
        END;
    PGAMMA:=ABS(1-SUM);
    END
ELSE
    BEGIN
    TERM:=(F/2-1)*LN(Y)-Y-LNGAMMA(F);
    {CH ADDED TO AVOID OVERFLOW}
    IF TERM<-11356 THEN TERM:=0 ELSE TERM:=EXP(TERM);
    SUM:=0; K:=0;
    WHILE (TERM*Y > (2*Y-F+K)*0.5E-20) AND (F-K>1) DO
        BEGIN
        SUM:=SUM+TERM;
        K:=K+2;
        TERM:=TERM*(F-K)/2/Y;
        END;
    PGAMMA:=ABS(SUM);
    END;
END;

{---------------------------------------------------------------------------}
function PNORMAL(U: EXTENDED): EXTENDED;

VAR P: EXTENDED;

BEGIN
P:= PGAMMA(1,U*U/2)/2;
IF U<0 THEN P:=1-P;
PNORMAL:=P;
END;

{---------------------------------------------------------------------------}
function PCHI2(F: LONGINT; Y: EXTENDED): EXTENDED;

BEGIN
PCHI2:= PGAMMA(F,Y/2);
END;
{---------------------------------------------------------------------------}
FUNCTION PBETA0(F1,F2:LONGINT; Y:EXTENDED): EXTENDED;

      { RETURNS THE LEFT TAIL PROBABILITY OF THE BETA DISTRIBUTION
        WITH PARAMTERS LAMBDA1=F1/2 AND LAMBDA2=F2/2. USE ONLY F1+F2<40.
        ACCURACY AROUND +/- 1E-16 . }

VAR SUM,TERM             : EXTENDED;
    K                   : LONGINT;

BEGIN
SUM:=0; K:=0;
TERM:=LNGAMMA(F1+F2)-LNGAMMA(F2)-LNGAMMA(F1+2)+F1*LN(Y)/2;
TERM:=EXP(TERM);
WHILE (K<F2) OR (ABS(TERM) > 1E-20) DO
    BEGIN
    SUM:=SUM+TERM;
    K:=K+2;
    TERM:=-TERM*Y*(F2-K)*(F1+K-2)/K/(F1+K);
    END;
PBETA0:=SUM;
END;

{---------------------------------------------------------------------------}
function PBETA(F1, F2: LONGINT; Y: EXTENDED): EXTENDED;

VAR SUM,TERM             : EXTENDED;
    K                   : LONGINT;
    INTCH               : BOOLEAN;

BEGIN  IF (F1=F2) AND (Y=0.5) THEN PBETA:=0.5 ELSE
       IF Y<=0 THEN PBETA:=0 ELSE
       IF Y>=1 THEN PBETA:=1 ELSE
    BEGIN
    INTCH:=FALSE;
    IF Y>(1-Y) THEN
        BEGIN INTCH:=TRUE;
        K:=F1; F1:=F2; F2:=K;
        Y:=1-Y;
        END;
    IF F1+F2<41 THEN SUM:=PBETA0(F1,F2,Y) ELSE
        BEGIN
        TERM:= (F2/2-1)*LN(1-Y) + (F1/2)*LN(Y)
            + LNGAMMA(F1+F2) - LNGAMMA(F1+2)
            - LNGAMMA(F2);
        TERM:=EXP(TERM);
        IF (TERM<1E-35) AND (Y<F1/(F1+F2)) THEN SUM:=0
        ELSE IF  (TERM<1E-35) AND (Y>F1/(F1+F2)) THEN SUM:=1
        ELSE
            BEGIN
            K:=0; SUM:=0;
            WHILE (ABS(TERM)>1E-25) OR (Y*(F2-K) > (1-Y)*(F1+K)) DO
                BEGIN SUM:=SUM+TERM;
                K:=K+2;
                TERM:= TERM*Y*(F2-K)/(1-Y)/(F1+K);
                END;
            END;
        END;
    IF INTCH THEN SUM:=1-SUM;
    PBETA:= ABS(SUM);
    END;
END;

{---------------------------------------------------------------------------}
function PFDISTR(F1, F2: LONGINT; Y: EXTENDED): EXTENDED;

BEGIN
PFDISTR:=PBETA(F2,F1,F2/(F1*Y+F2));
END;

{---------------------------------------------------------------------------}
function PTDISTR(F: LONGINT; Y: EXTENDED): EXTENDED;

VAR  P: EXTENDED;

BEGIN
IF Y=0 THEN PTDISTR:=0.5 ELSE
    BEGIN
    P:=F/(Y*Y+F);
    P:=PBETA(F,1,P);  P:=P/2;
    IF Y<0 THEN P:=1-P;
    PTDISTR:=P;
    END;
END;

{---------------------------------------------------------------------------}
function PNORMALINV(P: EXTENDED): EXTENDED;

VAR  PP,Y,A,B,Y0       :EXTENDED;

BEGIN
Y:= 0;  Y0:=1;
PP:=0.5;
WHILE Y0>1E-10 DO
    BEGIN Y0:=Y;
    A:=-LN(2*PI)/2-Y*Y/2;
    B:=Y;
    IF ABS(B)<1E-2 THEN Y:=Y+(PP-P)*EXP(-A)
    ELSE Y:=Y+LN(1+B*(PP-P)*EXP(-A))/B;
    PP:=PNORMAL(Y);  Y0:=ABS(Y-Y0);
    END;
PNORMALINV:=Y;
END;


{---------------------------------------------------------------------------}
function PGAMMAINV(F: LONGINT; P: EXTENDED): EXTENDED;

VAR  PP,Y,Y0,A,B,A0       :EXTENDED;

BEGIN A0:=-LNGAMMA(F);
IF F=1 THEN
    BEGIN
    Y:=PNORMALINV(P/2); Y:=Y*Y/2;
    END
ELSE
    BEGIN IF F>100 THEN
        BEGIN Y:= SQRT(2*F-1)+PNORMALINV(P); Y:=Y*Y/4;
        END
    ELSE Y:=F/2;
    Y0:=1;
    PP:=PGAMMA(F,Y);
    WHILE Y0>1E-7 DO
        BEGIN Y0:=Y;
        A:=A0+(F/2-1)*LN(Y)-Y;
        B:=(F/2-1)/Y-1;
        IF ABS(B*(PP-P)*EXP(-A))<1E-5 THEN Y:=Y+(PP-P)*EXP(-A)
        ELSE Y:=Y+LN(1+B*(PP-P)*EXP(-A))/B;
        PP:=PGAMMA(F,Y);
        Y0:=ABS(Y-Y0);
        END;
    END;
PGAMMAINV:=Y;
END;

{---------------------------------------------------------------------------}
function PCHI2INV(F: LONGINT; P: EXTENDED): EXTENDED;

VAR Y:EXTENDED;

BEGIN
Y:=PGAMMAINV(F,P);
PCHI2INV:=2*Y;
END;
{---------------------------------------------------------------------------}

FUNCTION PBETAINV1(F1,F2:LONGINT;P:EXTENDED):EXTENDED;

VAR  PP,Y,Y0,A,B,A0       :EXTENDED;
BEGIN
IF P<=0 THEN Y:=0
ELSE IF P>=1 THEN Y:=1
ELSE IF (F1=1) AND (F2=1) THEN Y:=SIN(P*PI/2)*SIN(P*PI/2)
ELSE IF (F1=1) AND (F2=2) THEN Y:=P*P
ELSE IF (F1=2) AND (F2=1) THEN Y:=1-(1-P)*(1-P)
ELSE IF (F1=2) AND (F2=2) THEN Y:=P
ELSE
    BEGIN
    A0:=-LNGAMMA(F1)-LNGAMMA(F2)+LNGAMMA(F1+F2);
    Y:=F1/(F1+F2);
    IF F1=1 THEN
        BEGIN
        Y:= PGAMMAINV(1,1-P);
        Y:= 2*Y/(2*Y+F2-1/2);
        END;
    Y0:=1;
    PP:=PBETA(F1,F2,Y);
    WHILE Y0>1E-8 DO
        BEGIN
        A:=A0+(F1/2-1)*LN(Y)+(F2/2-1)*LN(1-Y);
        B:=(F1/2-1)/Y-(F2/2-1)/(1-Y);
        IF ABS(B*(PP-P))*EXP(-A)<1E-5 THEN Y0:=-(PP-P)*EXP(-A)
        ELSE Y0:=LN(1-B*(PP-P)*EXP(-A))/B;
        Y:=Y+Y0;
        PP:=PBETA(F1,F2,Y);
        Y0:=ABS(Y0)/Y/(1-Y);
        END;
    END;
PBETAINV1:=Y;
END;

{---------------------------------------------------------------------------}
function PBETAINV(F1, F2: LONGINT; P: EXTENDED): EXTENDED;

VAR Y: EXTENDED;

BEGIN IF F1<=F2 THEN Y:=PBETAINV1(F1,F2,P)
ELSE Y:= 1-PBETAINV1(F2,F1,1-P);
PBETAINV := Y;
END;

{---------------------------------------------------------------------------}
function PFDISTRINV(F1, F2: LONGINT; P: EXTENDED): EXTENDED;

VAR  Y : EXTENDED;

BEGIN
Y:=PBETAINV(F2,F1,P);
PFDISTRINV:=F2/F1*(1-Y)/Y;
END;

{---------------------------------------------------------------------------}
function PTDISTRINV(F: LONGINT; P: EXTENDED): EXTENDED;

VAR T:EXTENDED;

BEGIN IF P<=0.5 THEN T:=SQRT(PFDISTRINV(1,F,2*P))
ELSE T:=-SQRT(PFDISTRINV(1,F,2*(1-P)));
PTDISTRINV:=T;
END;

{---------------------------------------------------------------------------}
function PPOISS(LAMBDA: EXTENDED; N: LONGINT): EXTENDED;

BEGIN PPOISS:= 1-PGAMMA(2*N,LAMBDA);
END;

{---------------------------------------------------------------------------}
function PBIN(N, X: LONGINT; P: EXTENDED): EXTENDED;

BEGIN PBIN:= PBETA(2*X,2+2*(N-X),P);
END;

{---------------------------------------------------------------------------}
function POISSCL(N: LONGINT; P: EXTENDED): EXTENDED;

BEGIN POISSCL := PGAMMAINV(2*N,1-P);
END;

{---------------------------------------------------------------------------}
function BINCL(N, X: LONGINT; PP: EXTENDED): EXTENDED;

BEGIN BINCL:= PBETAINV(2*X,2+2*(N-X),PP);
END;
{---------------------------------------------------------------------------}

{
           // alternative:
Function PPND16(P:extended):extended;

Function PPND16;
C
C	ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3
C
C	Produces the normal deviate Z corresponding to a given lower
C	tail area of P; Z is accurate to about 1 part in 10**16.
C
C	The hash sums below are the sums of the mantissas of the
C	coefficients.   They are included for use in checking
C	transcription.
C

var
	ZERO, ONE, HALF, SPLIT1, SPLIT2, CONST1 : Extended;

const
 CONST2, A0, A1,	A2, A3, A4, A5, A6, A7, B1, B2, B3,
     *          B4, B5, B6, B7,
     *		C0, C1, C2, C3, C4, C5, C6, C7,	D1, D2, D3, D4, D5,
     *		D6, D7, E0, E1, E2, E3, E4, E5, E6, E7, F1, F2, F3,
     *		F4, F5, F6, F7, P, Q, R;
	PARAMETER (ZERO = 0.D0, ONE = 1.D0, HALF = 0.5D0,
     *		SPLIT1 = 0.425D0, SPLIT2 = 5.D0,
     *		CONST1 = 0.180625D0, CONST2 = 1.6D0)
C
C	Coefficients for P close to 0.5
C
	PARAMETER (A0 = 3.38713 28727 96366 6080D0,
     *		   A1 = 1.33141 66789 17843 7745D+2,
     *		   A2 = 1.97159 09503 06551 4427D+3,
     *		   A3 = 1.37316 93765 50946 1125D+4,
     *		   A4 = 4.59219 53931 54987 1457D+4,
     *		   A5 = 6.72657 70927 00870 0853D+4,
     *		   A6 = 3.34305 75583 58812 8105D+4,
     *		   A7 = 2.50908 09287 30122 6727D+3,
     *		   B1 = 4.23133 30701 60091 1252D+1,
     *		   B2 = 6.87187 00749 20579 0830D+2,
     *		   B3 = 5.39419 60214 24751 1077D+3,
     *		   B4 = 2.12137 94301 58659 5867D+4,
     *		   B5 = 3.93078 95800 09271 0610D+4,
     *		   B6 = 2.87290 85735 72194 2674D+4,
     *		   B7 = 5.22649 52788 52854 5610D+3)
C	HASH SUM AB    55.88319 28806 14901 4439
C
C	Coefficients for P not close to 0, 0.5 or 1.
C
	PARAMETER (C0 = 1.42343 71107 49683 57734D0,
     *		   C1 = 4.63033 78461 56545 29590D0,
     *		   C2 = 5.76949 72214 60691 40550D0,
     *		   C3 = 3.64784 83247 63204 60504D0,
     *		   C4 = 1.27045 82524 52368 38258D0,
     *		   C5 = 2.41780 72517 74506 11770D-1,
     *             C6 = 2.27238 44989 26918 45833D-2,
     *		   C7 = 7.74545 01427 83414 07640D-4,
     *		   D1 = 2.05319 16266 37758 82187D0,
     *		   D2 = 1.67638 48301 83803 84940D0,
     *		   D3 = 6.89767 33498 51000 04550D-1,
     *		   D4 = 1.48103 97642 74800 74590D-1,
     *		   D5 = 1.51986 66563 61645 71966D-2,
     *		   D6 = 5.47593 80849 95344 94600D-4,
     *		   D7 = 1.05075 00716 44416 84324D-9)
C	HASH SUM CD    49.33206 50330 16102 89036
C
C	Coefficients for P near 0 or 1.
C
	PARAMETER (E0 = 6.65790 46435 01103 77720D0,
     *		   E1 = 5.46378 49111 64114 36990D0,
     *		   E2 = 1.78482 65399 17291 33580D0,
     *		   E3 = 2.96560 57182 85048 91230D-1,
     *		   E4 = 2.65321 89526 57612 30930D-2,
     *		   E5 = 1.24266 09473 88078 43860D-3,
     *		   E6 = 2.71155 55687 43487 57815D-5,
     *		   E7 = 2.01033 43992 92288 13265D-7,
     *		   F1 = 5.99832 20655 58879 37690D-1,
     *		   F2 = 1.36929 88092 27358 05310D-1,
     *		   F3 = 1.48753 61290 85061 48525D-2,
     *		   F4 = 7.86869 13114 56132 59100D-4,
     *		   F5 = 1.84631 83175 10054 68180D-5,
     *		   F6 = 1.42151 17583 16445 88870D-7,
     *		   F7 = 2.04426 31033 89939 78564D-15)
C	HASH SUM EF    47.52583 31754 92896 71629
C
	IFAULT = 0
	Q = P - HALF
	IF (ABS(Q) .LE. SPLIT1) THEN
	  R = CONST1 - Q * Q
	  PPND16 = Q * (((((((A7 * R + A6) * R + A5) * R + A4) * R + A3)
     *			* R + A2) * R + A1) * R + A0) /
     *		      (((((((B7 * R + B6) * R + B5) * R + B4) * R + B3)
     *			* R + B2) * R + B1) * R + ONE)
	  RETURN
	ELSE
	  IF (Q .LT. ZERO) THEN
	    R = P
	  ELSE
	    R = ONE - P
	  END IF
	  IF (R .LE. ZERO) THEN
	    IFAULT = 1
	    PPND16 = ZERO
	    RETURN
	  END IF
	  R = SQRT(-LOG(R))
	  IF (R .LE. SPLIT2) THEN
	    R = R - CONST2
	    PPND16 = (((((((C7 * R + C6) * R + C5) * R + C4) * R + C3)
     *			* R + C2) * R + C1) * R + C0) /
     *		     (((((((D7 * R + D6) * R + D5) * R + D4) * R + D3)
     *			* R + D2) * R + D1) * R + ONE)
	  ELSE
	    R = R - SPLIT2
	    PPND16 = (((((((E7 * R + E6) * R + E5) * R + E4) * R + E3)
     *			* R + E2) * R + E1) * R + E0) /
     *		     (((((((F7 * R + F6) * R + F5) * R + F4) * R + F3)
     *			* R + F2) * R + F1) * R + ONE)
	  END IF
	  IF (Q .LT. ZERO) PPND16 = - PPND16
	  RETURN
	END IF
	END
}

end.

