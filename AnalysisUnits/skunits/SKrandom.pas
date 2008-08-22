{$O+,F+}
UNIT SKrandom;

INTERFACE

Uses SKTYPES,skstat,distr;

//// OBS: There is an error in CHPrandom. Instead the RANDOM procedure of DELPHI is used

(*++++++++++++++++++++++++++++++++++++++++++++++*)
(* This unit contains procedures for generating *)
(* random numbers and tables                    *)
(*++++++++++++++++++++++++++++++++++++++++++++++*)

(* THIS VERSION CONTAINS PROCEDURES FOR SEQUENTIAL EXACT TESTS *)


VAR
    (**-----------------------------------------------***)
    (** declarations for the random number generators ***)
    (**-----------------------------------------------***)

    I                      : LONGINT;
    XD0,XD1,XD2,XD3        : INTEGER;
    MODULO,MODULO2,MODULO3 : EXTENDED;

    (**--------------------------------------------***)
    (** declarations for the log-factorial function **)
    (**--------------------------------------------***)

    LNFAK    : PTR_DOUBLE500;
    LNSQR2PI : real;

    (**--------------------------------------------***)
    (** parameters guiding sequential exact tests  ***)
    (**--------------------------------------------***)

    seq_limit,  // seq_limit is used to stop sequential tests
    SEQ_P0,SEQ_P1,SEQ_ALPHA,SEQ_BETA,SEQ_LNA,SEQ_LNB,SEQ_B,
    SEQ_LNPRATIO,SEQ_LNQRATIO,CHI_LNPRT,GAMMA_LNPRT,
    local_p0,local_alpha,local_b,local_p_risk,local_p_critical    : REAL;

    SEQUENTIAL,Local_sequential : BOOLEAN;

  FUNCTION  CHPRANDOM : REAL;

  PROCEDURE CHPINIT(SEED:INTEGER);

  FUNCTION  LNF(N:LONGINT) : REAL;

  PROCEDURE GENTAB(var outfile : textfile;
                       C,R         : BYTE;
                       TAB         : ETWOWAYTABLEarray;
                       ORDINAL     : BOOLEAN;
                   VAR CHI         : REAL;
                   VAR PPQ,PMQ     : REAL);

(** THE PROCEDURE GENERATES A TWO-DIMENSIONAL TABLE CORRE- **)
(** SPONDING TO THE 'EXPECTED' TABLE, ETAB2, CONTAINING    **)
(** THE SUFFICIENT MARGINALS AND CALCULATES THE CHISQUARE  **)
(** AND GAMMA STATISTICS (ONLY PPQ AND PMQ)                **)
(** THE PROCEDURE UTILIZES THE PROCEDURE, CHPRANDOM        **)

(** BEFORE THE GENTAB PROCEDURE IS CALLED FOR THE FIRST    **)
(** TIME, THE FOLLOWING VARIABLES SHOULD BE INITIALED:     **)
(**           LNFAKT AND ETAB2                             **)
(** CHPINIT SHOULD ALSO HAVE BEEN CALLED BEFORE THE FIRST  **)
(** CALL OF GENTAB                                         **)
(** WE SHOULD ALSO CHECK THAT NO ETAB DOES NOT CONTAIN     **)
(** MORE THAN MAXCASES CASES                               **)

PROCEDURE GENTAB0(var outfile : textfile;
                  C,R         : BYTE;
                  TAB         : ETWOWAYTABLEarray;
              var Newtab      : TwowaytableArray);

// the same procedure as Gentab, except taht the result is a twoway table without test statistics

(****)
procedure GenPermutedGamma(var outfile: textfile;
                           nxperms,nyperms : integer;
                           xperms,yperms   : perm_ptr;
                           c,r             : byte;
                           tab             : Etwowaytablearray;
                           var ppqpmq      : perm_ppqpmq_ptr);
// generate ppq and pmq values and add them to ppqpmq
(***)

   PROCEDURE SEQ_INIT(VAR P0    : REAL;
                          NSIM  : INTEGER;
                      VAR ALPHA : REAL;
                      VAR SEQ_B : REAL;
                      VAR TMPfile : TextFile);

   // TMPfile must be opened for writing before this procedure is called

   FUNCTION SEQ_T(S,N : INTEGER; P0 : REAL): REAL;

   Function Random_norm01: extended;

   Function Random_polynom(nkat: byte; CumP: Evector): Byte;


IMPLEMENTATION


(*-------------------------------------------------------------*)
   FUNCTION CHPRANDOM;

    (*** GENERATES A RANDOM NUMBER BETWEEN 0 AND 1          ***)
    (*** BY THE SAME ALGORITHM AS G05CAF OF THE NAG-LIBRARY ***)

    (* CONST AD3=8; AD2=19930; AD1=13895; AD0=17917;  *)

    VAR DUMMY0,DUMMY1,DUMMY2,DUMMY3             :EXTENDED;
        P00,P01,P10,P02,P11,P20,P03,P12,P21,P30 :EXTENDED;


    BEGIN
        P00:=17917.0*XD0;
        P01:=17917.0*XD1; P10:=13895.0*XD0;
        P02:=17917.0*XD2; P11:=13895.0*XD1; P20:=19930.0*XD0;
        P03:=17917.0*XD3; P12:=13895.0*XD2; P21:=19930.0*XD1; P30:=8.0*XD0;

        DUMMY0:=P00;
        DUMMY1:=P01+P10;
        DUMMY2:=P02+P11+P20;
        DUMMY3:=P03+P12+P21+P30;
        XD1:=TRUNC(DUMMY0/MODULO);
        DUMMY1:=DUMMY1+XD1;
        IF DUMMY1>=MODULO2 THEN
        BEGIN
            P00:=DUMMY1/MODULO2;
            DUMMY3:=DUMMY3+P00;
            DUMMY1:=DUMMY1-TRUNC(P00)*MODULO2;
        END;
        XD0:=ROUND((DUMMY0/MODULO-XD1)*MODULO);
        XD2:=TRUNC(DUMMY1/MODULO);
        DUMMY2:=DUMMY2+XD2;
        IF DUMMY2>=MODULO2 THEN
        BEGIN
            P00:=DUMMY2/MODULO2;
            DUMMY2:=DUMMY2-TRUNC(P00)*MODULO2;
        END;
        XD1:=ROUND((DUMMY1/MODULO-XD2)*MODULO);
        XD3:=TRUNC(DUMMY2/MODULO);
        DUMMY3:=DUMMY3+XD3;
        IF DUMMY3>=MODULO2 THEN DUMMY3:=DUMMY3-TRUNC(DUMMY3/MODULO2)*MODULO2;
        XD2:=ROUND((DUMMY2/MODULO-XD3)*MODULO);
        XD3:=ROUND((DUMMY3/MODULO-TRUNC(DUMMY3/MODULO))*MODULO);
        IF XD3>=16384 THEN XD3:=XD3-16384;
        CHPRANDOM:=2.0*(XD3/MODULO+XD2/MODULO2+XD1/MODULO3);
    END; (** CHPRANDOM***)
(*-----------------------------------------------------------------------*)

    PROCEDURE CHPINIT;


    (* CORRESPONDS TO G05CBF OF NAG *)

    BEGIN
        MODULO:=1.0+MAXINT;
        MODULO2:=MODULO*MODULO;
        MODULO3:=MODULO*MODULO2;
        XD3:=0; XD2:=0; XD1:=0; XD0:=2*SEED+1;

        randseed:=seed;

    END; (** CHPINIT **)

(*----------------------------------------------------------*)



     FUNCTION LNF;

     (* RETURNS THE NATURAL LOGARITHM OF THE FACTORIALS *)

     BEGIN
         IF N<501 THEN LNF:=LNFAK^(.N.) ELSE
         LNF:=LNSQR2PI+(N+0.5)*LN(N)-N+1/(24*N)-2.756; (** WE MUST CHECK **
                                                        ** THIS CONSTANT **)
     END; (**LNF**)

(*-----------------------------------------------------------*)

PROCEDURE GENTAB;

(** THE PROCEDURE GENERATES A TWO-DIMENSIONAL TABLE CORRE- **)
(** SPONDING TO THE 'EXPECTED' TABLE, ETAB2, CONTAINING    **)
(** THE SUFFICIENT MARGINALS AND CALCULATES THE CHISQUARE  **)
(** AND GAMMA STATISTICS (ONLY PPQ AND PMQ)                **)
(** THE PROCEDURE UTILIZES THE PROCEDURE, CHPRANDOM        **)

(** BEFORE THE GENTAB PROCEDURE IS CALLED FOR THE FIRST    **)
(** TIME, THE FOLLOWING VARIABLES SHOULD BE INITIALED:     **)
(**           LNFAKT AND ETAB2                             **)
(** CHPINIT SHOULD ALSO HAVE BEEN CALLED BEFORE THE FIRST  **)
(** CALL OF GENTAB                                         **)
(** WE SHOULD ALSO CHECK THAT NO ETAB DOES NOT CONTAIN     **)
(** MORE THAN MAXCASES CASES                               **)

LABEL 1,2,3;
VAR
   IC,IR,dfdummy                           : INTEGER;
   K,GENC,
   COLUMN1,COLUMN2,ROW1,ROW2,TMAX,TMIN,E,
   STEPNED,STEPOP,STEPMIN,
   GENTOT,NFRIE,T11,T12,T21,T22            : LONGINT;
   X,XXX,P,CUMP                            : EXTENDED;

   GENR                                    : ARRAY(.1..MAXDIM.) OF LONGINT;
   NEWTAB                                  : TWOWAYTABLEarray;
   PVALUES                                 : BOOLEAN;
   GAMMA,PDUMMY,SDUMMY,Z                   : REAL;

PROCEDURE CALCULATEP;
BEGIN
    T12:=COLUMN1-T11;
    T21:=ROW1-T11;
     T22:=NFRIE-ROW1-T12;
    P:=LNF(COLUMN1)+LNF(COLUMN2);
    P:=P+LNF(ROW1)+LNF(ROW2);
    p:=p-LNF(T11)-LNF(T12);
    P:=P-LNF(T21)-LNF(T22)-LNF(NFRIE);
    P:=EXP(P);
    CUMP:=CUMP+P;
END;

BEGIN
    PVALUES:=FALSE;
    dfdummy:=0;
    pdummy:=0;
    sdummy:=0;

    FOR IC:=1 TO C+1 DO NEWTAB(.IC,R+1.):=ROUND(TAB(.IC,R+1.));
    FOR IR:=1 TO R   DO NEWTAB(.C+1,IR.):=ROUND(TAB(.C+1,IR.));
    FOR IR:=1 TO R DO GENR(.IR.):=0;
    GENTOT:=0;

    IF NEWTAB(.C+1,R+1.) = 0 THEN
    BEGIN
        (*** NO CASES IN THIS TABLE ***)
        FOR IC:=1 TO C DO FOR IR:=1 TO R DO NEWTAB(.IC,IR.):=0;
        GOTO 1;
    END;

    FOR IC:=1 TO C-1 DO
    BEGIN
        IF NEWTAB(.IC,R+1.) = 0 THEN
        BEGIN
            (** NO CASES IN THIS COLUMN **)
            P:=1;
            FOR IR:=1 TO R DO NEWTAB(.IC,IR.):=0;
            GOTO 2;
        END;

        GENC:=0;
        NFRIE:=NEWTAB(.C+1,R+1.)-GENTOT;

        FOR IR:=1 TO R-1 DO
        BEGIN
            (** GENERATION OF T11=NEWTAB(.IC,IR.)  **)
            IF ((NEWTAB(.C+1,IR.)-GENR(.IR.)) = 0) or (nfrie=0) THEN
            BEGIN
                (** NO MORE CASES IN THIS ROW **)
                T11:=0; P:=1; CUMP:=1;
                GOTO 3;
            END;

            COLUMN1:=NEWTAB(.IC,R+1.)-GENC;
            ROW1:=NEWTAB(.C+1,IR.)-GENR(.IR.);
            COLUMN2:=NFRIE-COLUMN1;
            ROW2:=NFRIE-ROW1;

            IF COLUMN1 < ROW1 THEN TMAX:=COLUMN1 ELSE TMAX:=ROW1;

            IF (COLUMN1+ROW1-NFRIE) > 0 THEN TMIN:=COLUMN1+ROW1-NFRIE
                                        ELSE TMIN:=0;

            // NFRIE can be equal to 0 here

            Z:=COLUMN1*(ROW1/NFRIE);
            E:=ROUND(Z);
            x:=random; //X:=CHPRANDOM;

            STEPNED:=E-TMIN; STEPOP:=TMAX-E;
            IF STEPNED < STEPOP THEN STEPMIN:=STEPNED ELSE STEPMIN:=STEPOP;
            CUMP:=0; T11:=E;
            CALCULATEP;
            IF CUMP >= X THEN GOTO 3;
            FOR K:= 1 TO STEPMIN DO
            BEGIN
                T11:=E+K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;

                T11:=E-K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;
            END;
            IF STEPNED > STEPOP THEN
            BEGIN
                FOR K:=STEPMIN+1 TO STEPNED DO
                BEGIN
                    T11:=E-K;
                    CALCULATEP;
                    IF CUMP >= X THEN GOTO 3;
                END;
            END
            ELSE
            IF STEPOP > 0 THEN
            FOR K:=(STEPMIN+1) TO STEPOP DO
            BEGIN
                T11:=E+K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;
            END;
3:
            NEWTAB(.IC,IR.):=T11;
            GENTOT:=GENTOT+T11;
            GENC:=GENC+T11;
            NFRIE:=NFRIE-NEWTAB(.C+1,IR.)+GENR(.IR.);
            GENR(.IR.):=GENR(.IR.)+T11;
        END;  (** OF IR **)
        T11:=NEWTAB(.IC,R+1.)-GENC;
        NEWTAB(.IC,R.):=T11;
        GENTOT:=GENTOT+T11;
        GENC:=GENC+T11;
        GENR(.R.):=GENR(.R.)+T11;
        NFRIE:=NFRIE-NEWTAB(.C+1,R.)+GENR(.R.);
2:  END; (** OF IC **)
    FOR IR:=1 TO R DO
    NEWTAB(.C,IR.):=NEWTAB(.C+1,IR.)-GENR(.IR.);
1:
   (*** WE SHOULD AT THIS POINT HAVE A NEW TABLE ***)
   (*** FOR WHICH THE CHI-SQUARE AND GAMMA STA-  ***)
   (*** TISTICS SHOULD BE CALCULATED.            ***)

   RCCHI(C,R,NEWTAB,TAB,CHI,DFDUMMY,PDUMMY,PVALUES);
   RCGAMMA(C,R,NEWTAB,GAMMA,PDUMMY,PPQ,PMQ,SDUMMY,PVALUES);

   (*** test output **
   for ic:=1 to c+1 do
   begin
       for ir:=1 to r+1 do write(outfile,newtab(.ic,ir.):6);writeln(outfile);
   end;
   writeln(outfile,'pmq = ',pmq:7:0,' ppq = ',ppq:7:0);
   ******************)
END;  (** GENTAB **)

(*----------------------------------------------------------*)

PROCEDURE GENTAB0;
// the same as Gentab, except that the output is twowaytabel

LABEL 1,2,3;
VAR
   IC,IR,dfdummy                           : INTEGER;
   K,GENC,
   COLUMN1,COLUMN2,ROW1,ROW2,TMAX,TMIN,E,
   STEPNED,STEPOP,STEPMIN,
   GENTOT,NFRIE,T11,T12,T21,T22            : LONGINT;
   X,XXX,P,CUMP                            : EXTENDED;

   GENR                                    : ARRAY(.1..MAXDIM.) OF LONGINT;
   PVALUES                                 : BOOLEAN;
   GAMMA,PDUMMY,SDUMMY,Z                   : REAL;

PROCEDURE CALCULATEP;
BEGIN
    T12:=COLUMN1-T11;
    T21:=ROW1-T11;
    T22:=NFRIE-ROW1-T12;
    P:=LNF(COLUMN1)+LNF(COLUMN2);
    P:=P+LNF(ROW1)+LNF(ROW2);
    p:=p-LNF(T11)-LNF(T12);
    P:=P-LNF(T21)-LNF(T22)-LNF(NFRIE);
    P:=EXP(P);
    CUMP:=CUMP+P;
END;

BEGIN
    PVALUES:=FALSE;
    dfdummy:=0;
    pdummy:=0;
    sdummy:=0;


    newtab:=EmptyTwowaytable;

    FOR IC:=1 TO C+1 DO NEWTAB(.IC,R+1.):=ROUND(TAB(.IC,R+1.));
    FOR IR:=1 TO R   DO NEWTAB(.C+1,IR.):=ROUND(TAB(.C+1,IR.));
    FOR IR:=1 TO R DO GENR(.IR.):=0;

    GENTOT:=0;

    IF NEWTAB(.C+1,R+1.) = 0 THEN goto 1; // no cases in this table

    FOR IC:=1 TO C-1 DO
    BEGIN
        IF NEWTAB(.IC,R+1.) = 0 THEN
        BEGIN
            (** NO CASES IN THIS COLUMN **)
            P:=1;
            FOR IR:=1 TO R DO NEWTAB(.IC,IR.):=0;
            GOTO 2;
        END;

        GENC:=0;
        NFRIE:=NEWTAB(.C+1,R+1.)-GENTOT;

        FOR IR:=1 TO R-1 DO
        BEGIN
            (** GENERATION OF T11=NEWTAB(.IC,IR.)  **)
            IF ((NEWTAB(.C+1,IR.)-GENR(.IR.)) = 0) or (nfrie=0) THEN
            BEGIN
                (** NO MORE CASES IN THIS ROW **)
                T11:=0; P:=1; CUMP:=1;
                GOTO 3;
            END;

            COLUMN1:=NEWTAB(.IC,R+1.)-GENC;
            ROW1:=NEWTAB(.C+1,IR.)-GENR(.IR.);
            COLUMN2:=NFRIE-COLUMN1;
            ROW2:=NFRIE-ROW1;

            IF COLUMN1 < ROW1 THEN TMAX:=COLUMN1 ELSE TMAX:=ROW1;

            IF (COLUMN1+ROW1-NFRIE) > 0 THEN TMIN:=COLUMN1+ROW1-NFRIE
                                        ELSE TMIN:=0;

            // NFRIE can be equal to 0 here

            Z:=COLUMN1*(ROW1/NFRIE);
            E:=ROUND(Z);
            x:=random; //X:=CHPRANDOM;

            STEPNED:=E-TMIN; STEPOP:=TMAX-E;
            IF STEPNED < STEPOP THEN STEPMIN:=STEPNED ELSE STEPMIN:=STEPOP;
            CUMP:=0; T11:=E;
            CALCULATEP;
            IF CUMP >= X THEN GOTO 3;
            FOR K:= 1 TO STEPMIN DO
            BEGIN
                T11:=E+K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;

                T11:=E-K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;
            END;
            IF STEPNED > STEPOP THEN
            BEGIN
                FOR K:=STEPMIN+1 TO STEPNED DO
                BEGIN
                    T11:=E-K;
                    CALCULATEP;
                    IF CUMP >= X THEN GOTO 3;
                END;
            END
            ELSE
            IF STEPOP > 0 THEN
            FOR K:=(STEPMIN+1) TO STEPOP DO
            BEGIN
                T11:=E+K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;
            END;
3:
            NEWTAB(.IC,IR.):=T11;
            GENTOT:=GENTOT+T11;
            GENC:=GENC+T11;
            NFRIE:=NFRIE-NEWTAB(.C+1,IR.)+GENR(.IR.);
            GENR(.IR.):=GENR(.IR.)+T11;
        END;  (** OF IR **)
        T11:=NEWTAB(.IC,R+1.)-GENC;
        NEWTAB(.IC,R.):=T11;
        GENTOT:=GENTOT+T11;
        GENC:=GENC+T11;
        GENR(.R.):=GENR(.R.)+T11;
        NFRIE:=NFRIE-NEWTAB(.C+1,R.)+GENR(.R.);
2:  END; (** OF IC **)
    FOR IR:=1 TO R DO
    NEWTAB(.C,IR.):=NEWTAB(.C+1,IR.)-GENR(.IR.);
1:
   (*** WE SHOULD AT THIS POINT HAVE A NEW TABLE ***)
END;  (** GENTAB0 **)

//------------------------------------------------------------------------------

procedure GenPermutedGamma;
LABEL 1,2,3;
VAR
   IC,IR,dfdummy                           : INTEGER;
   K,GENC,
   COLUMN1,COLUMN2,ROW1,ROW2,TMAX,TMIN,E,
   STEPNED,STEPOP,STEPMIN,
   GENTOT,NFRIE,T11,T12,T21,T22            : LONGINT;
   X,XXX,P,CUMP                            : EXTENDED;

   GENR                                    : ARRAY(.1..MAXDIM.) OF LONGINT;
   NEWTAB,permtab                          : TWOWAYTABLEarray;
   PVALUES                                 : BOOLEAN;
   GAMMA,PDUMMY,SDUMMY,Z,ppq,pmq           : REAL;

   permnr,xnr,ynr,i,j                      : integer;
   xperm,yperm                             : bvector;

PROCEDURE CALCULATEP;
BEGIN
    T12:=COLUMN1-T11;
    T21:=ROW1-T11;
     T22:=NFRIE-ROW1-T12;
    P:=LNF(COLUMN1)+LNF(COLUMN2);
    P:=P+LNF(ROW1)+LNF(ROW2);
    p:=p-LNF(T11)-LNF(T12);
    P:=P-LNF(T21)-LNF(T22)-LNF(NFRIE);
    P:=EXP(P);
    CUMP:=CUMP+P;
END;

BEGIN
    PVALUES:=FALSE;
    dfdummy:=0;
    pdummy:=0;
    sdummy:=0;

    FOR IC:=1 TO C+1 DO NEWTAB(.IC,R+1.):=ROUND(TAB(.IC,R+1.));
    FOR IR:=1 TO R   DO NEWTAB(.C+1,IR.):=ROUND(TAB(.C+1,IR.));
    FOR IR:=1 TO R DO GENR(.IR.):=0;
    GENTOT:=0;

    IF NEWTAB(.C+1,R+1.) = 0 THEN
    BEGIN
        (*** NO CASES IN THIS TABLE ***)
        FOR IC:=1 TO C DO FOR IR:=1 TO R DO NEWTAB(.IC,IR.):=0;
        GOTO 1;
    END;

    FOR IC:=1 TO C-1 DO
    BEGIN
        IF NEWTAB(.IC,R+1.) = 0 THEN
        BEGIN
            (** NO CASES IN THIS COLUMN **)
            P:=1;
            FOR IR:=1 TO R DO NEWTAB(.IC,IR.):=0;
            GOTO 2;
        END;

        GENC:=0;
        NFRIE:=NEWTAB(.C+1,R+1.)-GENTOT;

        FOR IR:=1 TO R-1 DO
        BEGIN
            (** GENERATION OF T11=NEWTAB(.IC,IR.)  **)
            IF ((NEWTAB(.C+1,IR.)-GENR(.IR.)) = 0) or (nfrie=0) THEN
            BEGIN
                (** NO MORE CASES IN THIS ROW **)
                T11:=0; P:=1; CUMP:=1;
                GOTO 3;
            END;

            COLUMN1:=NEWTAB(.IC,R+1.)-GENC;
            ROW1:=NEWTAB(.C+1,IR.)-GENR(.IR.);
            COLUMN2:=NFRIE-COLUMN1;
            ROW2:=NFRIE-ROW1;

            IF COLUMN1 < ROW1 THEN TMAX:=COLUMN1 ELSE TMAX:=ROW1;

            IF (COLUMN1+ROW1-NFRIE) > 0 THEN TMIN:=COLUMN1+ROW1-NFRIE
                                        ELSE TMIN:=0;

            // NFRIE can be equal to 0 here

            Z:=COLUMN1*(ROW1/NFRIE);
            E:=ROUND(Z);
            x:=random; //X:=CHPRANDOM;

            STEPNED:=E-TMIN; STEPOP:=TMAX-E;
            IF STEPNED < STEPOP THEN STEPMIN:=STEPNED ELSE STEPMIN:=STEPOP;
            CUMP:=0; T11:=E;
            CALCULATEP;
            IF CUMP >= X THEN GOTO 3;
            FOR K:= 1 TO STEPMIN DO
            BEGIN
                T11:=E+K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;

                T11:=E-K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;
            END;
            IF STEPNED > STEPOP THEN
            BEGIN
                FOR K:=STEPMIN+1 TO STEPNED DO
                BEGIN
                    T11:=E-K;
                    CALCULATEP;
                    IF CUMP >= X THEN GOTO 3;
                END;
            END
            ELSE
            IF STEPOP > 0 THEN
            FOR K:=(STEPMIN+1) TO STEPOP DO
            BEGIN
                T11:=E+K;
                CALCULATEP;
                IF CUMP >= X THEN GOTO 3;
            END;
3:
            NEWTAB(.IC,IR.):=T11;
            GENTOT:=GENTOT+T11;
            GENC:=GENC+T11;
            NFRIE:=NFRIE-NEWTAB(.C+1,IR.)+GENR(.IR.);
            GENR(.IR.):=GENR(.IR.)+T11;
        END;  (** OF IR **)
        T11:=NEWTAB(.IC,R+1.)-GENC;
        NEWTAB(.IC,R.):=T11;
        GENTOT:=GENTOT+T11;
        GENC:=GENC+T11;
        GENR(.R.):=GENR(.R.)+T11;
        NFRIE:=NFRIE-NEWTAB(.C+1,R.)+GENR(.R.);
2:  END; (** OF IC **)
    FOR IR:=1 TO R DO
    NEWTAB(.C,IR.):=NEWTAB(.C+1,IR.)-GENR(.IR.);
1:
   (*** WE SHOULD AT THIS POINT HAVE A NEW TABLE ***)
   (*** FOR WHICH THE CHI-SQUARE AND GAMMA STA-  ***)
   (*** TISTICS SHOULD BE CALCULATED.            ***)

   //RCCHI(C,R,NEWTAB,TAB,CHI,DFDUMMY,PDUMMY,PVALUES);

   permnr:=0;
   for xnr:=1 to nxperms do
   for ynr:=1 to nyperms do
   begin
       inc(permnr);
       for i:=1 to c do xperm(.i.):=xperms^(.xnr,i.); xperm(.c+1.):=c+1;
       for i:=1 to r do yperm(.i.):=yperms^(.ynr,i.); yperm(.r+1.):=r+1;
       //permute the table Newtab->permtab

       for i:=1 to C+1 do
       for j:=1 to R+1 do
       permtab(.i,j.):=newtab(.xperm(.i.),yperm(.j.).);

       //calculate

       RCGAMMA(C,R,permtab,GAMMA,PDUMMY,PPQ,PMQ,SDUMMY,false); // no pvalues
       ppqpmq^(.permnr,1.):=ppqpmq^(.permnr,1.)+ppq;
       ppqpmq^(.permnr,2.):=ppqpmq^(.permnr,2.)+pmq;
   end;

END;

//------------------------------------------------------------------------------

PROCEDURE SEQ_INIT;

VAR N : INTEGER;
    PROCEDURE MESSAGE(VAR UDFIL : TEXTfile; NR : BYTE);

        PROCEDURE LINIE;
        BEGIN
            WRITELN(UDFIL);
        END;

    BEGIN
        CASE NR OF
             1: BEGIN
                    LINIE;
                    WRITELN(UDFIL,'Repeated Monte Carlo tests:');
                    LINIE;
                    WRITELN(UDFIL,'  Critical level = ',P0:5:3);
                    WRITELN(UDFIL,'  NSIM =           ',NSIM:5);
                    WRITELN(UDFIL,'  B =              ',SEQ_B:5:3);
                    LINIE;
                    WRITELN(UDFIL,'  Risk of accepting a significant result < ',ALPHA:5:3);
                    LINIE;
                END;
         END;
    END;

BEGIN
    IF P0<0.10 THEN P0:=0.05 ELSE
    IF P0<0.25 THEN P0:=0.10 ELSE P0:=0.25;

    N:=NSIM DIV 100;
    N:=N*100;
    IF NSIM-N>0 THEN N:=N+100;

    IF N>1000 THEN N:=1000;

    IF ALPHA<0.005 THEN ALPHA:=0.001 ELSE
    IF ALPHA<0.010 THEN ALPHA:=0.005 ELSE
    IF ALPHA<0.020 THEN ALPHA:=0.010 ELSE
    IF ALPHA<0.050 THEN ALPHA:=0.020 ELSE ALPHA:=0.05;

    SEQ_B:=2;

    IF (P0<=0.051) AND (ALPHA<=0.0011) THEN
    BEGIN
        SEQ_B:=1.058;
    END
    ELSE
    IF (P0<=0.051) AND (ALPHA<=0.0051) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=0.849;
            200: SEQ_B:=0.870;
            300: SEQ_B:=0.872;
            400: SEQ_B:=0.874;
            500: SEQ_B:=0.875;
            600: SEQ_B:=0.882;
            700: SEQ_B:=0.883;
            800: SEQ_B:=0.883;
            900: SEQ_B:=0.883;
           1000: SEQ_B:=0.883;

        ELSE  SEQ_B:=0.883;

        END;
    END
    ELSE
    IF (P0<=0.051) AND (ALPHA<=0.011) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=0.777;
            200: SEQ_B:=0.810;
            300: SEQ_B:=0.815;
            400: SEQ_B:=0.823;
            500: SEQ_B:=0.827;
            600: SEQ_B:=0.828;
            700: SEQ_B:=0.828;
            800: SEQ_B:=0.828;
            900: SEQ_B:=0.828;
           1000: SEQ_B:=0.829;

        ELSE  SEQ_B:=0.883;

        END;
    END
    ELSE
    IF (P0<=0.051) AND (ALPHA<=0.021) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=0.702;
            200: SEQ_B:=0.725;
            300: SEQ_B:=0.739;
            400: SEQ_B:=0.739;
            500: SEQ_B:=0.743;
            600: SEQ_B:=0.744;
            700: SEQ_B:=0.749;
            800: SEQ_B:=0.749;
            900: SEQ_B:=0.750;
           1000: SEQ_B:=0.752;

        ELSE  SEQ_B:=0.752;

        END;
    END
    ELSE
    IF (P0<=0.051) AND (ALPHA<=0.051) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=0.573;
            200: SEQ_B:=0.603;
            300: SEQ_B:=0.621;
            400: SEQ_B:=0.633;
            500: SEQ_B:=0.639;
            600: SEQ_B:=0.642;
            700: SEQ_B:=0.645;
            800: SEQ_B:=0.645;
            900: SEQ_B:=0.645;
           1000: SEQ_B:=0.646;

        ELSE  SEQ_B:=0.646;

        END;
    END
    ELSE
    IF (P0<=0.101) AND (ALPHA<=0.0011) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=1.287;
            200: SEQ_B:=1.313;
            300: SEQ_B:=1.317;
            400: SEQ_B:=1.317;
            500: SEQ_B:=1.317;
            600: SEQ_B:=1.317;
            700: SEQ_B:=1.321;
            800: SEQ_B:=1.347;
            900: SEQ_B:=1.347;
           1000: SEQ_B:=1.347;

        ELSE  SEQ_B:=1.347;

        END;
    END
    ELSE
    IF (P0<=0.101) AND (ALPHA<=0.0051) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=1.095;
            200: SEQ_B:=1.116;
            300: SEQ_B:=1.137;
            400: SEQ_B:=1.147;
            500: SEQ_B:=1.147;
            600: SEQ_B:=1.162;
            700: SEQ_B:=1.163;
            800: SEQ_B:=1.163;
            900: SEQ_B:=1.163;
           1000: SEQ_B:=1.163;

        ELSE  SEQ_B:=1.163;

        END;
    END
    ELSE
    IF (P0<=0.101) AND (ALPHA<=0.011) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=1.000;
            200: SEQ_B:=1.025;
            300: SEQ_B:=1.042;
            400: SEQ_B:=1.053;
            500: SEQ_B:=1.061;
            600: SEQ_B:=1.067;
            700: SEQ_B:=1.067;
            800: SEQ_B:=1.068;
            900: SEQ_B:=1.068;
           1000: SEQ_B:=1.068;

        ELSE  SEQ_B:=1.068;

        END;
    END
    ELSE
    IF (P0<=0.101) AND (ALPHA<=0.021) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=0.904;
            200: SEQ_B:=0.941;
            300: SEQ_B:=0.956;
            400: SEQ_B:=0.971;
            500: SEQ_B:=0.980;
            600: SEQ_B:=0.983;
            700: SEQ_B:=0.983;
            800: SEQ_B:=0.990;
            900: SEQ_B:=0.991;
           1000: SEQ_B:=0.992;

        ELSE  SEQ_B:=0.992;

        END;
    END
    ELSE
    IF (P0<=0.101) AND (ALPHA<=0.051) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=0.772;
            200: SEQ_B:=0.811;
            300: SEQ_B:=0.828;
            400: SEQ_B:=0.837;
            500: SEQ_B:=0.850;
            600: SEQ_B:=0.852;
            700: SEQ_B:=0.855;
            800: SEQ_B:=0.858;
            900: SEQ_B:=0.861;
           1000: SEQ_B:=0.863;

        ELSE  SEQ_B:=0.863;

        END;
    END
    ELSE
    IF (P0<=0.251) AND (ALPHA<=0.0011) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=1.641;
            200: SEQ_B:=1.686;
            300: SEQ_B:=1.714;
            400: SEQ_B:=1.721;
            500: SEQ_B:=1.724;
            600: SEQ_B:=1.724;
            700: SEQ_B:=1.724;
            800: SEQ_B:=1.724;
            900: SEQ_B:=1.724;
           1000: SEQ_B:=1.724;

        ELSE  SEQ_B:=1.724;

        END;
    END
    ELSE
    IF (P0<=0.251) AND (ALPHA<=0.0051) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=1.471;
            200: SEQ_B:=1.526;
            300: SEQ_B:=1.539;
            400: SEQ_B:=1.541;
            500: SEQ_B:=1.552;
            600: SEQ_B:=1.563;
            700: SEQ_B:=1.563;
            800: SEQ_B:=1.565;
            900: SEQ_B:=1.565;
           1000: SEQ_B:=1.566;

        ELSE  SEQ_B:=1.566;

        END;
    END
    ELSE
    IF (P0<=0.251) AND (ALPHA<=0.011) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=1.383;
            200: SEQ_B:=1.427;
            300: SEQ_B:=1.443;
            400: SEQ_B:=1.461;
            500: SEQ_B:=1.466;
            600: SEQ_B:=1.470;
            700: SEQ_B:=1.473;
            800: SEQ_B:=1.477;
            900: SEQ_B:=1.485;
           1000: SEQ_B:=1.488;

        ELSE  SEQ_B:=1.488;

        END;
    END
    ELSE
    IF (P0<=0.251) AND (ALPHA<=0.021) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=1.257;
            200: SEQ_B:=1.304;
            300: SEQ_B:=1.337;
            400: SEQ_B:=1.350;
            500: SEQ_B:=1.354;
            600: SEQ_B:=1.362;
            700: SEQ_B:=1.371;
            800: SEQ_B:=1.371;
            900: SEQ_B:=1.376;
           1000: SEQ_B:=1.379;

        ELSE  SEQ_B:=1.379;

        END;
    END
    ELSE
    IF (P0<=0.251) AND (ALPHA<=0.051) THEN
    BEGIN
        CASE N OF
            100: SEQ_B:=1.066;
            200: SEQ_B:=1.133;
            300: SEQ_B:=1.157;
            400: SEQ_B:=1.174;
            500: SEQ_B:=1.186;
            600: SEQ_B:=1.192;
            700: SEQ_B:=1.200;
            800: SEQ_B:=1.205;
            900: SEQ_B:=1.207;
           1000: SEQ_B:=1.210;

        ELSE  SEQ_B:=1.210;

        END;
    END;

    MESSAGE(TMPfile,1);

END; (* SEQ_INIT *)

(*---------------------------------------------------*)

FUNCTION SEQ_T(S,N : INTEGER; P0 : REAL): REAL;
VAR SQRTN : EXTENDED;
BEGIN
    IF N<21 THEN SEQ_T:=0.0 ELSE
    BEGIN
        SQRTN:=SQRT(N);
        SEQ_T:=S/SQRTN-SQRTN*P0;
    END;
END; (* OF SEQ_T *)

(*----------------------------------------------------*)

Function Random_norm01: extended;
var x: extended;
begin
    x:=random;
    Random_norm01:=pNormalInv(x);
end;

//-----------------------------------------------------

Function Random_polynom(nkat: byte; CumP: Evector): Byte;
label slut;
var i,n  : byte;
    x: Extended;
begin
    //Random_polynom:=0;
    x:=random*cump(.nkat.);
    n:=nkat-1;
    for i:=1 to n do
    if x<=cump(.i.) then
    begin
        random_polynom:=i;
        goto slut;
    end;
    random_polynom:=nkat;
slut:
end;

//-------------------------------------------------------


BEGIN
     XD0 :=0;
     XD1 :=0;
     XD2 :=0;
     XD3 :=0;
     MODULO :=0.0;
     MODULO2:=0.0;
     MODULO3:=0.0;

    (** Initialization of LNF **)

     LNSQR2PI:= LN(SQR(2*PI));

     NEW(LNFAK);

     LNFAK^(.0.):=0;
     LNFAK^(.1.):=0;

     FOR I:=2 TO 500 DO LNFAK^(.I.):=LNFAK^(.I-1.)+LN(I);
END. (** of Random **)
