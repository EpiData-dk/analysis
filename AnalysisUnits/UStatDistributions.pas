UNIT Ustatdistributions;

// This unit copied from Claus Holst SP programme. 
// SP version based on similar unit made by Tue Tjur
// copyright : to be added
// compiler directive deleted: $I SPCOMDIR

INTERFACE

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


FUNCTION PFDISTR(F1,F2:LONGINT;Y:EXTENDED):EXTENDED;

      { RETURNS THE RIGHT TAIL PROBABILITY IN THE F DISTRIBUTION
        WITH (F1,F2) DEGREES OF FREEDOM.
        USE ONLY F1 AND F2 < 1E6. }


FUNCTION PTDISTR(F:LONGINT;Y:EXTENDED):EXTENDED;

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


FUNCTION PFDISTRINV(F1,F2:LONGINT;P:EXTENDED):EXTENDED;

      { 1-P PERCENTILE OF F DISTRIBUTION. }

FUNCTION PTDISTRINV(F:LONGINT;P:EXTENDED):EXTENDED;
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
IMPLEMENTATION

FUNCTION LNGAMMA;

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
FUNCTION PGAMMA;

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
FUNCTION PNORMAL;

VAR P: EXTENDED;

BEGIN
P:= PGAMMA(1,U*U/2)/2;
IF U<0 THEN P:=1-P;
PNORMAL:=P;
END;
{---------------------------------------------------------------------------}
FUNCTION PCHI2;

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
FUNCTION PBETA;

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
FUNCTION PFDISTR;

BEGIN
PFDISTR:=PBETA(F2,F1,F2/(F1*Y+F2));
END;
{---------------------------------------------------------------------------}
FUNCTION PTDISTR;

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
FUNCTION PNORMALINV;

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
FUNCTION PGAMMAINV;

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
FUNCTION PCHI2INV;

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
FUNCTION PBETAINV;

VAR Y: EXTENDED;

BEGIN IF F1<=F2 THEN Y:=PBETAINV1(F1,F2,P)
ELSE Y:= 1-PBETAINV1(F2,F1,1-P);
PBETAINV := Y;
END;
{---------------------------------------------------------------------------}
FUNCTION PFDISTRINV;

VAR  Y : EXTENDED;

BEGIN
Y:=PBETAINV(F2,F1,P);
PFDISTRINV:=F2/F1*(1-Y)/Y;
END;
{---------------------------------------------------------------------------}
FUNCTION PTDISTRINV;

VAR T:EXTENDED;

BEGIN IF P<=0.5 THEN T:=SQRT(PFDISTRINV(1,F,2*P))
ELSE T:=-SQRT(PFDISTRINV(1,F,2*(1-P)));
PTDISTRINV:=T;
END;

{---------------------------------------------------------------------------}
FUNCTION PPOISS;

BEGIN PPOISS:= 1-PGAMMA(2*N,LAMBDA);
END;

{---------------------------------------------------------------------------}
FUNCTION PBIN;

BEGIN PBIN:= PBETA(2*X,2+2*(N-X),P);
END;

{---------------------------------------------------------------------------}
FUNCTION POISSCL;

BEGIN POISSCL := PGAMMAINV(2*N,1-P);
END;
{---------------------------------------------------------------------------}
FUNCTION BINCL;

BEGIN BINCL:= PBETAINV(2*X,2+2*(N-X),PP);
END;
{---------------------------------------------------------------------------}

END.
