UNIT SkStat;

(* Stepwise comparison of meanvalue tests have been removed from this version *)

INTERFACE

Uses SkTYPES, SKMatrix;//,skcmds;

CONST CO=0.3989422804014327;
      BIGX=170;

TYPE
    TwoByTwoSubtable = Object
        A,B,C,D   : Longint;
        I,J,K,L   : Byte;
        Alpha,Beta,Gamma : real;
        PPQ,PMQ    : real;
        Area       : Integer;
        Alpha1,Beta1,Gamma1 : Real;

        Procedure CalculateStatistics;
    end;

VAR TWO_THIRD : REAL;
    CHI_SQUARE_USED,
    POWER_DIVERGENCE : BOOLEAN;

  procedure calculate_Etab2(c,r : integer; tab2 : twowaytableArray; var etab2 : EtwowaytableArray);

  procedure conflimit95(n : integer;
                        p : REAL;
                    VAR LOW  : REAL;
                    VAR HIGH : REAL);

  procedure conflimit99(n : integer;
                        p : REAL;
                    VAR LOW  : REAL;
                    VAR HIGH : REAL);

  procedure conflimit999(n : integer;
                        p : REAL;
                    VAR LOW  : REAL;
                    VAR HIGH : REAL);

  Function GammaFromBeta(Beta:real):real;

  Function BetaFromGamma(Gamma:real): real;

  FUNCTION TAILNORM(X:REAL;UPPER:BOOLEAN):REAL;

  FUNCTION PFCHI(DF:INTEGER; X:REAL) : REAL;

  FUNCTION PNORMAL(U: REAL): REAL;

  FUNCTION GAMMA_PVALUE(GAMMA,PPQ,S : REAL) : REAL;

  PROCEDURE PREPARE_GAMMA_STATISTICS(C,R          : INTEGER;
                                     TAB          : TWOWAYTABLEarray;
                               VAR   AIJ,DIJ      : ETWOWAYTABLEarray;
                               VAR   P,Q,PPQ,PMQ  : REAL);

  PROCEDURE PREPARE_Expected_GAMMA_STATISTICS(C,R : INTEGER;
                                     TAB          : ETWOWAYTABLEarray;
                               VAR   AIJ,DIJ      : ETWOWAYTABLEarray;
                               VAR   P,Q,PPQ,PMQ  : REAL);

  PROCEDURE PREPARE_Long_GAMMA_STATISTICS(C,R          : INTEGER;
                                     TAB          : LongTWOWAYTABLEarray;
                               VAR   AIJ,DIJ      : LongETWOWAYTABLEarray;
                               VAR   P,Q,PPQ,PMQ  : REAL);

  PROCEDURE RCGAMMA(C,R          : INTEGER;
                    TAB          : TWOWAYTABLEarray;
              VAR   GAMMA,PGAMMA : REAL;
              VAR   PPQ,PMQ      : REAL;
              VAR   S            : REAL;
                    PVALUES      : BOOLEAN);

  PROCEDURE Expected_RCGAMMA(C,R : INTEGER;
                    TAB          : ETWOWAYTABLEarray;
              VAR   GAMMA,PGAMMA : REAL;
              VAR   PPQ,PMQ      : REAL;
              VAR   S            : REAL;
                    PVALUES      : BOOLEAN);

// The input to this procedure must be a table of expected counts


  PROCEDURE LongRCGAMMA(C,R      : INTEGER;
                    TAB          : LongTWOWAYTABLEarray;
              VAR   GAMMA,PGAMMA : REAL;
              VAR   PPQ,PMQ      : REAL;
              VAR   S            : REAL;
                    PVALUES      : BOOLEAN);
  // Gamma for long tables R<= LongMaxDim

  PROCEDURE ESTIMATE_RCGAMMA(C,R     : INTEGER;
                             TAB     : TWOWAYTABLEarray;
                       VAR   GAMMA   : REAL;
                       VAR   PPQ,PMQ : REAL;
                       VAR   S1      : REAL;
                       VAR   SUCCESS : BOOLEAN);

  (* THIS PROCEDURE ESTIMATES THE VALUE OF GAMMA *)
  (* AND THE VARIANCE, S1, OF THIS ESTIMATE      *)

  PROCEDURE RCCHI(C,R     : INTEGER;
                  TAB     : TWOWAYTABLEarray;
                  ETAB    : ETWOWAYTABLEArray;
             VAR  CHI     : REAL;
             VAR  DF      : INTEGER;
             VAR  PCHI    : REAL;
                  PVALUES : BOOLEAN);

  procedure LongRCCHI(C,R     : INTEGER;
                  TAB     : LongTWOWAYTABLEarray;
                  ETAB    : LongETWOWAYTABLEArray;
             VAR  CHI     : REAL;
             VAR  DF      : INTEGER;
             VAR  PCHI    : REAL;
                  PVALUES : BOOLEAN);



PROCEDURE STANDARDTESTS(    DIMX,DIMY,DIMZ    : BYTE;
                            XTYPE,YTYPE       : BYTE;
                            TAB3              : THREEWAYTABLEArray;
                        VAR KITOT    : REAL;
                        VAR DFTOT    : INTEGER;
                        VAR PKITOT   : REAL;
                        VAR GAMMATOT : REAL;
                       VAR PGAMMATOT : REAL);

PROCEDURE DETERMINE_SIGNIFICANCE(SIGNIFICANCE_LEVEL : REAL;
                                 PCHI               : REAL;
                                 PGAMMA             : REAL;
                                 ORDINALS           : BOOLEAN;
                             VAR ACCEPT             : BOOLEAN);


PROCEDURE MEANVALUE_TEST(var outfile    : textfile;
                             NGROUPS    : BYTE;
                             MEANVALUES : RVECTOR;
                             VARIANCES  : RVECTOR;
                         VAR MEAN       : REAL;
                         VAR VARIANCE   : REAL;
                         VAR WEIGHTS    : RVECTOR;
                         VAR RESIDUALS  : RVECTOR;
                         VAR CHI        : REAL;
                         VAR DF         : INTEGER;
                         VAR P          : REAL;
                         VAR SUCCESS    : BOOLEAN);

(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* THIS PROCEDURE CALCULATES A TEST FOR HOMOGENEITY OF MEANS *)
(* IN DIFFERENT GROUPS WITH DIFFERENT VARIANCES             *)
(*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

PROCEDURE COMPARE_TWO_MEANS(MEAN1,VAR1,MEAN2,VAR2 : REAL;
                       VAR  MEAN,VARIANCE,U,P     : REAL;
                       VAR  SUCCESS               : BOOLEAN);

Procedure ztest(obs,exp,se : real; var z,pz : real);

procedure rckappa(r     : byte;
                  TAB   : TWOWAYTABLEarray;
             VAR  kappa : REAL);

procedure scorekappa(nitems    : byte;
                     scoredist : Ivector;
                 var kappa     : real);

procedure permutate(n : byte; var Nperm: integer; var perms: perm_ptr);

// perms is a ptr to a 40320*8 table that has to be assigned before the procedure is called

Procedure OneSample_polynomial_tests(var outfile : textfile;
                                     k        : byte;
                                     p        : Evector;
                                     n        : integer;
                                     Observed : Ivector;
                                     Nsim     : integer;
                                     Limits   : Lvector;
                                     Seed     : integer;
                                     Uweights : Evector;
                                     var U,plessU,pgtrU,pUasymp,
                                         Gam,plessGam,pgtrGam,PgamAsymp,
                                         Skewness,plessSkew,pgtrSkew : extended);

procedure Gamma2OddsRatio(gamma         : real;
                      var Oddsratio     : real;
                      var plusinfinity  : boolean;
                      var minusinfinity : boolean);


IMPLEMENTATION
procedure calculate_Etab2(c,r : integer; tab2 : twowaytableArray; var etab2 : EtwowaytableArray);
var i,j : integer;
begin
    if tab2(.c+1,r+1.)>0 then
    for i:=1 to c+1 do
    for j:=1 to r+1 do Etab2(.i,j.):=tab2(.i,r+1.)*tab2(.c+1,j.)/tab2(.c+1,r+1.);
end;

//----------------------------------------------------------------------------------------------

      Procedure TwoByTwoSubtable.CalculateStatistics;
      begin
          Area:=(J-I)*(L-K);

          If a+b+c+d=0 then
          begin
              PPQ:=0;
              PMQ:=0;
              ALPHA:=0;
              BETA:=0;
              GAMMA:=0;
              ALPHA1:=0;
              BETA1:=0;
              GAMMA1:=0;
          end
          else
          begin
              PPQ:=A*D+B*C;
              PMQ:=A*D-B*C;
              Gamma:=PMQ/PPQ;
              If B*C=0 then
              begin
                  alpha:=0.0;
                  alpha1:=0.0;
                  beta:=-32000;
                  beta1:=-32000;
                  Gamma:=-1;
                  Gamma1:=-1;
              end
              else
              If A*D=0 then
              begin
                  alpha:=32000.0;
                  alpha1:=32000.0;
                  beta:=32000;
                  beta1:=32000;
                  Gamma:=1;
                  Gamma1:=1;
              end
              else
              begin
                  Alpha:=(1+Gamma)/(1-Gamma);
                  Beta:=ln(Alpha);
                  Beta1:=Beta/Area;
                  Alpha1:=exp(Beta1);
                  Gamma1:=(Alpha1-1)/(Alpha1+1);
              end;
          end;

      end;

      procedure conflimit95;
      var a,r,z,zsquare,b,q,c : real;
      begin
          r:=p*n;
          z:=1.96;
          zsquare:=z*z;

          a:=2*r+zsquare;

          q:=1-p;

          b:=z*sqrt(zsquare+4*r*q);

          c:=2*(n+zsquare);

          low:=(a-b)/c;
          high:=(a+b)/c;
      end;

      procedure conflimit99;
      var a,r,z,zsquare,b,q,c : real;
      begin
          r:=p*n;
          z:=2.5758;
          zsquare:=z*z;

          a:=2*r+zsquare;

          q:=1-p;

          b:=z*sqrt(zsquare+4*r*q);

          c:=2*(n+zsquare);

          low:=(a-b)/c;
          high:=(a+b)/c;
      end;


      procedure conflimit999;
      var a,r,z,zsquare,b,q,c : real;
      begin
          r:=p*n;
          z:=3.8906;
          zsquare:=z*z;

          a:=2*r+zsquare;

          q:=1-p;

          b:=z*sqrt(zsquare+4*r*q);

          c:=2*(n+zsquare);

          low:=(a-b)/c;
          high:=(a+b)/c;
      end;

      Function GammaFromBeta;
      begin
          if Beta<=-31.9999999 then GammaFromBeta:=-1.0
          else
          if Beta<>+31.9999999 then GammaFromBeta:=+1.0
          else
          GammaFromBeta:=(exp(Beta)-1)/(exp(Beta)+1);
      end;

      Function BetaFromGamma;
      begin
          If Gamma<=-0.99999999 then BetaFromGamma:=-32000.0
          else
          If Gamma>=+0.99999999 then BetaFromGamma:=+32000.0
          else
          BetaFromGamma:=ln(1+Gamma)-ln(1-Gamma);
      end;

      FUNCTION TAILNORM(X:REAL;UPPER:BOOLEAN):REAL;
        CONST C=0.3989422804014327;
        VAR N,P1,P2,Q1,Q2,M,X2,Y,S,T : extended;
            I:LONGINT;
      BEGIN
        IF X=0 THEN TAILNORM:=0.5 ELSE
        BEGIN
          IF X<0 THEN UPPER:=NOT UPPER;
          X:=ABS(X);
          X2:=X*X;
          IF (X2/2)<BIGX THEN Y:=C*EXP(-0.5*X2) ELSE Y:=0;
          N:=Y/X;
          IF NOT UPPER AND (N=0) THEN TAILNORM:=1 ELSE
          IF UPPER AND (N=0) THEN TAILNORM:=0 ELSE
          BEGIN
            IF UPPER AND (X>2.32) OR NOT UPPER AND (X>3.5) THEN
            BEGIN
              Q1:=X; P2:=Y*X; I:=1; P1:=Y; Q2:=X2+I;
              IF UPPER THEN
              BEGIN
                S:=P1/Q1; M:=S; T:=P2/Q2
              END ELSE
              BEGIN
                S:=I-P1/Q1; M:=S; T:=I-P2/Q2
              END;
              WHILE (M<>T) AND (S<>T) DO
              BEGIN
                INC(I);
                S:=X*P2+I*P1;
                P1:=P2;
                P2:=S;
                S:=X*Q2+I*Q1;
                Q1:=Q2;
                Q2:=S;
                S:=M;
                M:=T;
                IF UPPER THEN T:=P2/Q2 ELSE T:=1-P2/Q2
              END;
              TAILNORM:=T
            END ELSE
            BEGIN
              S:=Y*X;
              X:=Y*X;
              I:=1;
              T:=0;
              WHILE S<>T DO
              BEGIN
                I:=I+2;
                T:=S;
                X:=X*X2/I;
                S:=S+X
              END;
              IF UPPER THEN TAILNORM:=0.5-S ELSE TAILNORM:=0.5+S
            END
          END
        END
      END;   (** TAILNORM **)



(*-----------------------------------------------------------*)

    FUNCTION PFCHI;

      VAR P,C        :REAL;
          I,DF_DIV_2 :INTEGER;

    BEGIN
      DF_DIV_2:=DF DIV 2;

      IF (DF=0) OR (X<=0.0) THEN P:=1.0 ELSE
      IF DF>100 THEN
      BEGIN
        X:=SQRT(4.5*DF)*(EXP(LN(X/DF)/3)+1/(4.5*DF)-1);
        P:=TAILNORM(X,TRUE)
      END ELSE
      IF (DF MOD 2)=0 THEN
      BEGIN
        IF X<BIGX THEN P:=EXP(-0.5*X) ELSE P:=0;
        C:=P;
        FOR I:=1 TO DF_DIV_2-1 DO
        BEGIN
          C:=C*0.5*X/I;
          P:=P+C
        END
      END ELSE
      BEGIN

          P:=2*TAILNORM(SQRT(X),true);

          IF X<BIGX THEN C:=EXP(-0.5*X)*2*CO/SQRT(X) ELSE C:=0; (*ERROR*)
          FOR I:=1 TO DF_DIV_2 DO
          BEGIN
            C:=C*X/(2*I-1);
            P:=P+C
          END
      END;
      PFCHI:=P

    END (* of PFCHI *);
(*---------------------------------------------------------------------*)

    FUNCTION PNORMAL;
    BEGIN
        PNORMAL:=TAILNORM(U,TRUE);
    END; (**PNORMAL**)

(*------------------------------------------------------*)

    FUNCTION GAMMA_PVALUE;
    VAR U: REAL;
    BEGIN
        IF S>0.0 THEN
        BEGIN
            U:=ABS(GAMMA/(SQRT(S)/PPQ));
            GAMMA_PVALUE:=PNORMAL(U);
        END
        ELSE GAMMA_PVALUE:=1;

    END; (* of GAMMA_PVALUE *)

    (*-------------------------------------------*)

    PROCEDURE PREPARE_GAMMA_STATISTICS;
    VAR
        I,J,K,L           : BYTE;

    BEGIN
        FOR I:=1 TO DimTop DO FOR J:=1 TO DimTop DO AIJ(.I,J.):=0;
        DIJ:=AIJ;

        P:=0;  Q:=0;

        FOR I:=1 TO C DO FOR J:=1 TO R DO
        BEGIN
            FOR K:=1 TO C DO FOR L:=1 TO R DO
            IF ((I>K) AND (J>L))
            OR ((I<K) AND (J<L)) THEN AIJ(.I,J.):=AIJ(.I,J.)+TAB(.K,L.) ELSE
            IF ((I<K) AND (J>L))
            OR ((I>K) AND (J<L)) THEN DIJ(.I,J.):=DIJ(.I,J.)+TAB(.K,L.);

            P:=P+TAB(.I,J.)*AIJ(.I,J.);
            Q:=Q+TAB(.I,J.)*DIJ(.I,J.);
        END;

        PPQ:=P+Q;  PMQ:=P-Q;
    END; (* of PREPARE_GAMMA_STATISTICS *)

    //--------------------------------------------------------------------------

    PROCEDURE PREPARE_Expected_GAMMA_STATISTICS;
    VAR I,J,K,L : BYTE;
    BEGIN
        FOR I:=1 TO DimTop DO FOR J:=1 TO DimTop DO AIJ(.I,J.):=0;
        DIJ:=AIJ;

        P:=0;  Q:=0;

        FOR I:=1 TO C DO FOR J:=1 TO R DO
        BEGIN
            FOR K:=1 TO C DO FOR L:=1 TO R DO
            IF ((I>K) AND (J>L))
            OR ((I<K) AND (J<L)) THEN AIJ(.I,J.):=AIJ(.I,J.)+TAB(.K,L.) ELSE
            IF ((I<K) AND (J>L))
            OR ((I>K) AND (J<L)) THEN DIJ(.I,J.):=DIJ(.I,J.)+TAB(.K,L.);

            P:=P+TAB(.I,J.)*AIJ(.I,J.);
            Q:=Q+TAB(.I,J.)*DIJ(.I,J.);
        END;

        PPQ:=P+Q;  PMQ:=P-Q;
    END; (* of PREPARE_Expected_GAMMA_STATISTICS *)

    (*-------------------------------------------*)

    PROCEDURE PREPARE_Long_GAMMA_STATISTICS;
    VAR I,J,K,L : BYTE;
    BEGIN
        FOR I:=1 TO DimTop DO FOR J:=1 TO LongDimTop DO AIJ(.I,J.):=0;
        DIJ:=AIJ;

        P:=0;  Q:=0;

        FOR I:=1 TO C DO FOR J:=1 TO R DO
        BEGIN
            FOR K:=1 TO C DO FOR L:=1 TO R DO
            IF ((I>K) AND (J>L))
            OR ((I<K) AND (J<L)) THEN AIJ(.I,J.):=AIJ(.I,J.)+TAB(.K,L.) ELSE
            IF ((I<K) AND (J>L))
            OR ((I>K) AND (J<L)) THEN DIJ(.I,J.):=DIJ(.I,J.)+TAB(.K,L.);

            P:=P+TAB(.I,J.)*AIJ(.I,J.);
            Q:=Q+TAB(.I,J.)*DIJ(.I,J.);
        END;

        PPQ:=P+Q;  PMQ:=P-Q;
    END; (* of PREPARE_Long_GAMMA_STATISTICS *)

    (*-------------------------------------------*)

    PROCEDURE RCGAMMA;
    VAR I,J     : BYTE;
        M,P,Q   : REAL;
        AIJ,DIJ : ETWOWAYTABLEArray;
    BEGIN
        PREPARE_GAMMA_STATISTICS(C,R,TAB,AIJ,DIJ,P,Q,PPQ,PMQ);

        IF PVALUES THEN
        BEGIN
            IF PPQ>0 THEN
            BEGIN
                GAMMA:=PMQ/PPQ;
                S:=-PMQ*(PMQ/TAB(.C+1,R+1.));
                FOR I:=1 TO C DO FOR J:=1 TO R DO
                BEGIN
                   M:=AIJ(.I,J.)-DIJ(.I,J.);
                   S:=S+TAB(.I,J.)*M*M;
                END;
            END
            ELSE
            BEGIN
                GAMMA:=0;
                S:=0;
            END;

            S:=4*S;

            PGAMMA:=GAMMA_PVALUE(GAMMA,PPQ,S);
        END;
    END;   (*** RCGAMMA ****)

    (*-------------------------------------------*)

    PROCEDURE Expected_RCGAMMA;
    VAR
        I,J     : BYTE;
        M,P,Q   : REAL;
        AIJ,DIJ : ETWOWAYTABLEArray;
    BEGIN
        PREPARE_Expected_GAMMA_STATISTICS(C,R,TAB,AIJ,DIJ,P,Q,PPQ,PMQ);

        IF PVALUES THEN
        BEGIN
            IF PPQ>0 THEN
            BEGIN
                GAMMA:=PMQ/PPQ;
                S:=-PMQ*(PMQ/TAB(.C+1,R+1.));
                FOR I:=1 TO C DO FOR J:=1 TO R DO
                BEGIN
                   M:=AIJ(.I,J.)-DIJ(.I,J.);
                   S:=S+TAB(.I,J.)*M*M;
                END;
            END
            ELSE
            BEGIN
                GAMMA:=0;
                S:=0;
            END;

            S:=4*S;

            PGAMMA:=GAMMA_PVALUE(GAMMA,PPQ,S);
            //s:=s/(ppq*ppq);
        END;
    END;   (*** Expected_RCGAMMA ****)

    (*-------------------------------------------*)

    PROCEDURE LongRCGAMMA;

    VAR
        I,J     : BYTE;
        M,P,Q   : REAL;
        AIJ,DIJ : LongETWOWAYTABLEArray;

    BEGIN
        PREPARE_Long_GAMMA_STATISTICS(C,R,TAB,AIJ,DIJ,P,Q,PPQ,PMQ);

        IF PVALUES THEN
        BEGIN
            IF PPQ>0 THEN
            BEGIN
                GAMMA:=PMQ/PPQ;
                if tab(.c+1,r+1.)<>0 then
                S:=-PMQ*(PMQ/TAB(.C+1,R+1.))
                else s:=0; ///why!!!
                FOR I:=1 TO C DO FOR J:=1 TO R DO
                BEGIN
                   M:=AIJ(.I,J.)-DIJ(.I,J.);
                   S:=S+TAB(.I,J.)*M*M;
                END;
            END
            ELSE
            BEGIN
                GAMMA:=0;
                S:=0;
            END;

            S:=4*S;

            PGAMMA:=GAMMA_PVALUE(GAMMA,PPQ,S);
        END;
    END;   (*** RCGAMMA ****)

    (*-------------------------------------------*)

    PROCEDURE ESTIMATE_RCGAMMA;
    VAR
        I,J                  : BYTE;
        P,Q                  : REAL;
        M,FAKTOR,PPQ_SQUARED : EXTENDED;
        AIJ,DIJ              : ETWOWAYTABLEArray;
    BEGIN
        PREPARE_GAMMA_STATISTICS(C,R,TAB,AIJ,DIJ,P,Q,PPQ,PMQ);

        IF PPQ>0 THEN
        BEGIN
            PPQ_SQUARED:=PPQ*PPQ;
            FAKTOR:=16/(PPQ_SQUARED*PPQ_SQUARED);

            SUCCESS:=TRUE;
            GAMMA:=PMQ/PPQ;
            S1:=0;
            FOR I:=1 TO C DO FOR J:=1 TO R DO
            BEGIN
                M:=Q*AIJ(.I,J.)-P*DIJ(.I,J.);
                S1:=S1+TAB(.I,J.)*M*M;
            END;
            S1:=FAKTOR*S1;
        END
        ELSE
        BEGIN
            SUCCESS:=FALSE;
            GAMMA:=0;
            S1:=0;
        END;

    END;   (*** ESTIMATE_RCGAMMA ****)

(*-----------------------------------------------------------------------*)

    PROCEDURE RCCHI;

    (* NOTICE THAT THIS UNIT USES THE FOLLOWING *)
    (* GLOBAL VARIABLES, FROM VARSUNIT:         *)
    (*       CHI_SQUARE_USED                    *)
    (*       POWER_DIVERGENCE                   *)
    (*       TWO_THIRD                          *)

    LABEL SLUT;

    VAR  I,J   : BYTE;
         NR,NC : INTEGER;

    FUNCTION POWDIV(OBSERVED : INTEGER; EXPECTED : REAL) : REAL;
    BEGIN
        POWDIV:=EXP(TWO_THIRD*LN(OBSERVED/EXPECTED))-1;
    END;

    BEGIN
        CHI:=0;

        IF TAB(.C+1,R+1.)=0 THEN
        BEGIN
            DF:=0;
            PCHI:=1.0;
            GOTO SLUT;
        END;

        NR:=0; NC:=0;

        FOR I:= 1 TO C DO FOR J:=1 TO R DO
        IF ETAB(.I,J.)>0 THEN
        BEGIN
            IF CHI_SQUARE_USED THEN
            CHI:=CHI+(TAB(.I,J.)-ETAB(.I,J.))*
                ((TAB(.I,J.)-ETAB(.I,J.))/ETAB(.I,J.))
            ELSE
            IF TAB(.I,J.)>0 THEN
            BEGIN
                IF POWER_DIVERGENCE THEN
                CHI:=CHI+TAB(.I,J.)*POWDIV(TAB(.I,J.),ETAB(.I,J.))
                ELSE
                CHI:=CHI+TAB(.I,J.)*LN(TAB(.I,J.)/ETAB(.I,J.));
            END;
        END;

        IF NOT CHI_SQUARE_USED THEN
        BEGIN
            IF POWER_DIVERGENCE THEN CHI:=1.8*CHI ELSE CHI:=2*CHI;
        END;

        IF PVALUES THEN
        BEGIN
            FOR I:=1 TO C DO
            IF TAB(.I,R+1.)>0 THEN INC(NC);
            FOR J:=1 TO R DO
            IF TAB(.C+1,J.)>0 THEN INC(NR);

            DF:=(NR-1)*(NC-1);

            IF DF=0 THEN PCHI:=1 ELSE PCHI:=PFCHI(DF,CHI);

        END;
SLUT:
    END;  (*** RCCHI ***)

(*---------------------------------------------------------------------*)

procedure LongRCCHI;
    LABEL SLUT;

    VAR  I,J   : BYTE;
         NR,NC : INTEGER;

    FUNCTION POWDIV(OBSERVED : INTEGER; EXPECTED : REAL) : REAL;
    BEGIN
        POWDIV:=EXP(TWO_THIRD*LN(OBSERVED/EXPECTED))-1;
    END;

    BEGIN
        CHI:=0;

        IF TAB(.C+1,R+1.)=0 THEN
        BEGIN
            DF:=0;
            PCHI:=1.0;
            GOTO SLUT;
        END;

        NR:=0; NC:=0;

        FOR I:= 1 TO C DO FOR J:=1 TO R DO
        IF ETAB(.I,J.)>0 THEN
        BEGIN
            IF CHI_SQUARE_USED THEN
            CHI:=CHI+(TAB(.I,J.)-ETAB(.I,J.))*
                ((TAB(.I,J.)-ETAB(.I,J.))/ETAB(.I,J.))
            ELSE
            IF TAB(.I,J.)>0 THEN
            BEGIN
                IF POWER_DIVERGENCE THEN
                CHI:=CHI+TAB(.I,J.)*POWDIV(TAB(.I,J.),ETAB(.I,J.))
                ELSE
                CHI:=CHI+TAB(.I,J.)*LN(TAB(.I,J.)/ETAB(.I,J.));
            END;
        END;

        IF NOT CHI_SQUARE_USED THEN
        BEGIN
            IF POWER_DIVERGENCE THEN CHI:=1.8*CHI ELSE CHI:=2*CHI;
        END;

        IF PVALUES THEN
        BEGIN
            FOR I:=1 TO C DO
            IF TAB(.I,R+1.)>0 THEN INC(NC);
            FOR J:=1 TO R DO
            IF TAB(.C+1,J.)>0 THEN INC(NR);

            DF:=(NR-1)*(NC-1);

            IF DF=0 THEN PCHI:=1 ELSE PCHI:=PFCHI(DF,CHI);

        END;
SLUT:
    END;  (*** LongRCCHI ***)

(*---------------------------------------------------------------------*)



PROCEDURE STANDARDTESTS;

VAR Z,X,Y           : BYTE;
    TAB2            : TWOWAYTABLEArray;
    ETAB2           : ETWOWAYTABLEArray;
    KI,PKI,GAMMA,PGAMMA,PPQ,PMQ,PPQTOT,PMQTOT,S,STOT,U : REAL;
    DF              : INTEGER;
    PVALUES,ORDINAL : BOOLEAN;
    C,R             : INTEGER;

BEGIN

        KITOT:=0;
        DFTOT:=0;
        PPQ:=0;
        PMQ:=0;
        STOT:=0;
        PVALUES:= FALSE;
        PPQTOT:=0;
        PMQTOT:=0;

        C:=DIMX;
        R:=DIMY;

        KI:=0;
        DF:=0;
        PKI:=1;
        GAMMA:=0;
        PGAMMA:=1;
        PPQ:=0;
        PMQ:=0;
        S:=0;

        ORDINAL:=TRUE;
        IF (DIMX>2) AND (XTYPE<3) THEN ORDINAL:=FALSE ELSE
        IF (DIMY>2) AND (YTYPE<3) THEN ORDINAL:=FALSE;

        FOR Z:=1 TO DIMZ DO
        BEGIN
            FOR X:=1 TO DIMX+1 DO
            FOR Y:=1 TO DIMY+1 DO
            BEGIN
                TAB2(.X,Y.):=TAB3(.X,Y,Z.);
                IF TAB3(.DIMX+1,DIMY+1,Z.) > 0 THEN
                ETAB2(.X,Y.):=TAB3(.X,DIMY+1,Z.)*
                             (TAB3(.DIMX+1,Y,Z.)/TAB3(.DIMX+1,DIMY+1,Z.))
                ELSE
                ETAB2(.X,Y.):=0.0;
            END;
            RCCHI(C,R,TAB2,ETAB2,KI,DF,PKI,TRUE);
            KITOT:=KITOT+KI;
            DFTOT:=DFTOT+DF;

            IF ORDINAL THEN
            BEGIN
                RCGAMMA(C,R,TAB2,GAMMA,PGAMMA,PPQ,PMQ,S,true);
                PPQTOT:=PPQTOT+PPQ;
                PMQTOT:=PMQTOT+PMQ;
                STOT:=STOT+S;
            END;

        END;

        PKITOT:=PFCHI(DFTOT,KITOT);
        IF ORDINAL AND (PPQTOT>0) THEN
        BEGIN
            GAMMATOT:=PMQTOT/PPQTOT;
            STOT:=STOT/PPQTOT; STOT:=STOT/PPQTOT;
            IF STOT=0 THEN U:=0 ELSE
            U:=GAMMATOT/SQRT(STOT);
            IF U<0.0 THEN U:=-U;
            IF U>4.0 THEN PGAMMATOT:=0.000001 ELSE
            PGAMMATOT:=PNORMAL(U);
        END
        ELSE
        BEGIN
            PGAMMATOT:=1.0;
            GAMMATOT:=0;
        END;

END;   (*STANDARDTESTS *)

(*--------------------------------------------------------*)

PROCEDURE DETERMINE_SIGNIFICANCE;

BEGIN

    IF PCHI>SIGNIFICANCE_LEVEL THEN ACCEPT:=TRUE ELSE ACCEPT:=FALSE;

    IF ACCEPT AND ORDINALS
    AND (PGAMMA<=SIGNIFICANCE_LEVEL) THEN ACCEPT:=FALSE;

END; (* of DETERMINE_SIGNIFICANCE *)

(*-----------------------------------------------------------*)

PROCEDURE MEANVALUE_TEST;
LABEL SLUT;
VAR
    VMATRIX           : TNMATRIX;
    V_INVERS          : TNMATRIX;
    W,TOTWEIGHT       : REAL;
    I,J,NGRP,FEJL     : BYTE;
    MID,VARI,VGT,RES  : RVECTOR;
    GRPNR             : BVECTOR;

BEGIN

    (* TRANSFER MEANS AND VARIANCES TO MID AND VAR *)
    (* AND REMOVE EMPTY GROUPS                     *)

    NGRP:=0;
    TOTWEIGHT:=0.0;

    FOR I:=1 TO NGROUPS DO
    BEGIN
        WEIGHTS(.I.):=0;
        RESIDUALS(.I.):=0;
    END;

    FOR I:=1 TO NGROUPS DO
    IF VARIANCES(.I.)>0 THEN
    BEGIN
        NGRP:=NGRP+1;
        MID(.NGRP.)  := MEANVALUES(.I.);
        VARI(.NGRP.) := VARIANCES(.I.);
        VGT(.NGRP.)  := 1/VARI(.NGRP.);
        GRPNR(.NGRP.):= I;

        TOTWEIGHT:=TOTWEIGHT+VGT(.NGRP.);
    END;

    DF:=NGRP-1;

    //IF NGRP<NGROUPS THEN WRITELN(outfile,'Groups with zero variance');

    IF DF<1 THEN
    BEGIN
        //WRITELN(outfile,'No degrees of freedom');
        GOTO SLUT;
    END;

    (*--------------------*)
    (* CALCULATE WEIGHTS  *)
    (* AND ESTIMATES      *)
    (*--------------------*)

    MEAN:=0;
    VARIANCE:=0;

    FOR I:=1 TO NGRP DO
    BEGIN
        W:=VGT(.I.)/TOTWEIGHT;
        VGT(.I.):=W;
        WEIGHTS(.GRPNR(.I.).):=W;

        MEAN:=MEAN+W*MID(.I.);

        VARIANCE:=VARIANCE+VARI(.I.)*W*W;
    END;

    (*-------------------------------------*)
    (* CALCULATE DIFFERENCE BETWEEN GRAND  *)
    (* AND LOCAL MEANS                     *)
    (*-------------------------------------*)

    FOR I:=1 TO NGRP DO RES(.I.):=MID(.I.)-MEAN;

    (*** CALCULATE TEST STATISTIC ***)

    (*** THE VARIANCE MATRIX ***)

    FILLCHAR(VMATRIX,SIZEOF(VMATRIX),0);
    V_INVERS:=VMATRIX;

    FOR I:=1 TO NGRP DO
    VMATRIX(.I,I.):=VARIANCE+VARI(.I.)-2*VGT(.I.)*VARI(.I.);

    FOR I:=1 TO NGRP-1 DO
    FOR J:=I+1 TO NGRP DO
    BEGIN
        VMATRIX(.I,J.):=VARIANCE-VGT(.I.)*VARI(.I.)-VGT(.J.)*VARI(.J.);
        VMATRIX(.J,I.):=VMATRIX(.I,J.);
    END;

    (*** THE INVERSE MATRIX ***)

    INVERSE(DF,VMATRIX,V_INVERS,FEJL);

    CHI:=0;
    IF FEJL=0 THEN
    BEGIN
        FOR I:=1 TO NGRP-1 DO
        FOR J:=1 TO NGRP-1 DO
        CHI:=CHI+RES(.I.)*RES(.J.)*V_INVERS(.I,J.);

        P:=PFCHI(DF,CHI);
        SUCCESS:=TRUE;

    END
    ELSE
    BEGIN
        P:=2;
        SUCCESS:=FALSE;
    END;

    FOR I:=1 TO NGRP DO
    IF VMATRIX(.I,I.)>0 THEN
    RESIDUALS(.GRPNR(.I.).):=RES(.I.)/SQRT(VMATRIX(.I,I.));

SLUT:
END; (* of MEANVALUE_TEST *)

(*--------------------------------------------------------*)

PROCEDURE COMPARE_TWO_MEANS;
LABEL SLUT;
VAR   W1,W2,S : EXTENDED;
BEGIN

    SUCCESS:=FALSE;

    IF VAR1<=0.0 THEN
    BEGIN
        MEAN:=MEAN2;
        VARIANCE:=VAR2;
        GOTO SLUT;
    END
    ELSE
    IF VAR2<=0.0 THEN
    BEGIN
        MEAN:=MEAN1;
        VARIANCE:=VAR1;
        GOTO SLUT;
    END;

    SUCCESS:=TRUE;

    W1:=1/VAR1;
    W2:=1/VAR2;
    S:=W1+W2;
    W1:=W1/S;
    W2:=W2/S;

    MEAN:=W1*MEAN1+W2*MEAN2;
    VARIANCE:=W1*W1*VAR1+W2*W2*VAR2;

    U:=(MEAN1-MEAN2)/SQRT(VAR1+VAR2);

    P:=2*PNORMAL(ABS(U)); (* TWO-SIDED PVALUES *)

SLUT:
END;  (* of COMPARE_TWO_MEANS *)

(*--------------------------------------*)

Procedure ztest(obs,exp,se : real; var z,pz : real);
begin
    if se>0.0 then
    begin
        z:=(obs-exp)/se;
        pz:=2*Pnormal(abs(z)); // 2-sided p-value
    end
    else
    if obs=exp then
    begin
        z:=0;
        pz:=1.0;
    end
    else
    begin
        z:=(obs-exp)*999999.9;
        pz:=0.00;
    end;
end;

//-----------------------------------------------------------

procedure rckappa(r     : byte;
                  TAB   : TWOWAYTABLEarray;
             VAR  kappa : REAL);
var i,j : byte;
    cmarg,rmarg : rvector;
    A,e,tot   : real;
    s1,s2 : string;
begin
    a:=0.0;
    e:=0.0;
    for i:=1 to r do cmarg(.i.):=0;
    rmarg:=cmarg;
    tot:=0.0;

    for i:=1 to r do
    for j:=1 to r do
    begin
        cmarg(.i.):=cmarg(.i.)+tab(.i,j.);
        rmarg(.j.):=rmarg(.j.)+tab(.i,j.);
        tot:=tot+tab(.i,j.);
    end;

    for i:=1 to r do
    begin
        a:=a+tab(.i,i.)/tot;
        e:=e+rmarg(.i.)*cmarg(.i.)/(tot*tot);
    end;
    //str(a:10:4,s1);
    //str(e:10:4,s2);
    //okmessage(s1+'   '+s2);
    if e<1 then
    kappa:=(a-e)/(1.0-e)
    else kappa:=0;
end;

//------------------------------------------------------------

procedure scorekappa(nitems    : byte;
                     scoredist : Ivector;
                 var kappa     : real);
var i,j : byte;
    a,p,tot,e : real;

    S : STRING;
begin
    a :=0; p:=0; tot:=0;
    for i:=0 to nitems do
    begin
        a:=a+scoredist(.i.)*(i*(i-1)+(nitems-i)*(nitems-i-1))/2;
        tot:=tot+scoredist(.i.);
        p:=p+i*scoredist(.i.);
    end;
    a:=a/tot;
    p:=p/(tot*nitems);
    a:=2*a/(nitems*(Nitems-1));

    //STR(A:6:2,S);
    //OKMESSAGE('a= '+S);
    //STR(p:6:2,S);
    //OKMESSAGE('p = '+S);
    e:=p*p+(1-p)*(1-p);
    //STR(e:6:2,S);
    //OKMESSAGE('e = '+S);
    kappa:=(a-e)/(1-e);
end;

//------------------------------------------------------------

procedure permutate;
var x : bvector;

    procedure localpermutate(n : byte; var x: bvector);
    var i : byte;

        procedure process(x:bvector);
        var s,s1 : string;
            i : byte;
            one,two : byte;
        begin
            for i:=1 to n do
            if x(.i.)=1 then one:=i else
            if x(.i.)=2 then two:=i;
            if one<two then
            begin
                inc(nperm);
                for i:=1 to n do
                begin
                    perms^(.nperm,i.):=x(.i.);
                end;
            end;
        end;

        procedure choose(d:byte);
        var i,e : byte;
        begin
            if d<>n then choose(d+1) else process(x);
            for i:=d+1 to n do
            begin
                e:=x(.d.); x(.d.):=x(.i.); x(.i.):=e;
                if d<>n then choose(d+1) else process(x);
                e:=x(.i.); x(.i.):=x(.d.); x(.d.):=e;
            end;
        end;
    begin
        for i:=1 to n do x(.i.):=i;
        choose(1);
    end;

begin
    nperm:=0;
    localpermutate(n,x);
end;

//-------------------------------------------------------------

Procedure OneSample_polynomial_tests;
label nextp,videre,igen;
var i,i1,j,simnr,catnr,nrejected    : integer;
    psort,cumpsort,alpha,beta    : evector;
    a0,b0,c0,se_gam,se_u         : extended;
    covar                        : ematrix;
    value                        : bvector;
    x                            : real;
    obs0,zeros                   : ivector;
    u0,gam0,skewness0            : extended;
    lessu,gtru,lessgam,gtrgam,lessskew,gtrskew : integer;
    below,above,obsfreq       : evector;
    emean,esd,eskew,omean,osd,oskew,mean0,sd0,skew0   : extended;

    procedure Moments(freqs : evector; var mean,sd,skew : extended);
    var i : Byte;
        x : extended;
    begin
        mean:=0;
        for i:=1 to k do mean:=mean+i*freqs(.i.);
        sd:=0;
        skew:=0;
        for i:=1 to k do
        begin
            x:=(i-mean)*(i-mean);
            sd:=sd+x*freqs(.i.);
            skew:=skew+x*(i-mean)*freqs(.i.);
        end;
        sd:=sqrt(sd);
    end;

    procedure TestStatistics(observed : ivector;
                             var u,gam,mean,sd,skewness : extended;
                             print_freqs : boolean;
                             calc_asymp  : boolean);
    var i,j   : byte;
        c,d   : extended;
        freqs : evector;
        x     : extended;
    begin
        u:=0.0;
        gam:=0.0;
        Skewness:=0.0;
        c:=0;
        d:=0;

        for i:=1 to k do
        begin
            c:=c+observed(.i.)*below(.i.);
            d:=d+observed(.i.)*above(.i.);
            freqs(.i.):=observed(.i.)/n;
            x:=(observed(.i.)-n*p(.i.))*uweights(.i.);
            //if calc_asymp then
            //writeln(outfile,i:3,x:10:5);
            u:=u+x;
        end;

        if c+d>0 then gam:=(c-d)/(c+d);


        //for i:=1 to k do write(outfile,observed(.i.):4); writeln(outfile,gam:7:3);

        moments(freqs,mean,sd,skewness);
        if print_freqs then
        begin
            for i:=1 to k do write(outfile,freqs(.i.):6:3);
            writeln(outfile,gam:8:3,skewness:7:3);
        end;

        if calc_asymp then
        begin
            se_Gam:=(a0-2*b0+c0);
            if se_gam>0 then
            begin
                se_Gam:=n*sqrt(se_Gam)/(c+d); // multiplication wrt n because below and above are
                                             // multiplied by n
                pgamasymp:=2*pnormal(abs(gam/se_gam));
            end
            else
            begin
                writeln(outfile,'**** non positive standard error = ',se_gam:15:7);
                pgamasymp:=0;
            end;

            se_u:=0.0;
            for i:=1 to k do
            for j:=1 to k do se_u:=se_u+uweights(.i.)*uweights(.j.)*covar(.i,j.);
            if se_u>=0 then
            begin
                se_u:=sqrt(se_u);
                puasymp:=2*pnormal(abs(u/se_u));
            end
            else
            begin
                writeln(outfile,'**** non positive standard error of u= ',se_gam:15:7);
                puasymp:=0;
            end;

        end;

    end;
begin

    for i:=0 to largedim do zeros(.i.):=0;

    for i:=1 to k do
    begin
        below(.i.):=0;
    end;
    above:=below;

    for i:=1 to k do
    begin
        if i>1 then
        for j:=1 to i-1 do below(.i.):=below(.i.)+n*p(.j.);

        if i<k then
        for j:=i+1 to k do above(.i.):=above(.i.)+n*p(.j.);
    end;

    // statistics required for evaluation of p-values for the gamma coefficient

    alpha(.k.):=0.0;
    for i:=k-1 downto 1 do alpha(.i.):=alpha(.i+1.)+p(.i+1.);

    beta(.1.):=0.0;
    for i:=2 to k do beta(.i.):=beta(.i-1.)+p(.i-1.);


    for i:=1 to k do covar(.i,i.):=n*p(.i.)*(1-p(.i.));

    for i:=1 to k-1 do
    for j:=i+1 to k do
    begin
        covar(.i,j.):=-n*p(.i.)*p(.j.);
        covar(.j,i.):=covar(.i,j.);
    end;

    a0:=0.0;
    b0:=0.0;
    c0:=0.0;

    for i:=1 to k do
    for j:=1 to k do
    begin
        a0:=a0+alpha(.i.)*alpha(.j.)*covar(.i,j.);
        b0:=b0+alpha(.i.)*beta(.j.)*covar(.i,j.);
        c0:=a0+beta(.i.)*beta(.j.)*covar(.i,j.);
    end;

    moments(p,emean,esd,eskew);

    writeln(outfile);
    write(outfile,'Expected score distribution: ');
    for i:=1 to k do write(outfile,p(.i.):6:3);writeln(outfile);
    writeln(outfile);
    //writeln(outfile,' ':8,eskew:7:3);

    //writeln(outfile);
    //writeln(outfile,'Expected moments:  mean = ',emean:5:2,'  s.d. = ',esd:5:3,'  skewness = ',eskew:6:3);

    // calculate test statistics

    //writeln(outfile);
    //writeln(outfile,'Observed freqs:');

    Teststatistics(observed,u,gam,omean,osd,oskew,false,true);

    //writeln(outfile);
    //writeln(outfile,'Observed moments:  mean = ',omean:5:2,'  s.d. = ',osd:5:3,'  skewness = ',oskew:6:3);
    //writeln(outfile);
    //prepare Monte Carlo tests - sort outcomes in decreasing order

    psort(.0.):=2;
    value(.0.):=0;

    if p(.1.)>=p(.2.) then
    begin
        psort(.1.):=p(.1.);
        value(.1.):=1;
        psort(.2.):=p(.2.);
        value(.2.):=2;
    end
    else
    begin
        psort(.1.):=p(.2.);
        value(.1.):=2;
        psort(.2.):=p(.1.);
        value(.2.):=1;
    end;

    for i:=3 to k do
    begin
        for j:=i-1 downto 0 do
        if p(.i.)>psort(.j.) then
        begin
            psort(.j+1.):=psort(.j.);
            value(.j+1.):=value(.j.);
        end
        else
        begin
            psort(.j+1.):=p(.i.);
            value(.j+1.):=i;
            goto nextp;
        end;
        nextp:
    end;

    cumpsort(.1.):=psort(.1.);
    for i:=2 to k-1 do cumpsort(.i.):=cumpsort(.i-1.)+psort(.i.);
    cumpsort(.k.):=1.0;

    randseed:=seed;

    lessu     := 0;
    gtru      := 0;
    lessgam   := 0;
    gtrgam    := 0;
    lessskew  := 0;
    gtrskew   := 0;
    nrejected :=0;

    for simnr:=1 to nsim do
    begin
    igen:
        obs0:=zeros;

        for i:=1 to n do
        begin
            x:=random;
            for j:=1 to k do
            begin
                if x<=cumpsort(.j.) then
                begin
                    catnr:=value(.j.);
                    inc(obs0(.catnr.));
                    if obs0(.catnr.)>limits(.catnr.) then
                    begin
                        inc(nrejected);
                        if nrejected<6 then
                        begin
                            write(outfile,'Rejected: ');
                            for i1:=1 to k do write(outfile,obs0(.i1.):5);writeln(outfile);
                        end;
                        goto igen;
                    end;
                    goto videre;
                end;
            end;
        videre:
        end;

        //if simnr<=10 then
        //Teststatistics(obs0,u0,gam0,mean0,sd0,skew0,true,false)
        //else

        Teststatistics(obs0,u0,gam0,mean0,sd0,skew0,false,false);

        if (u<0) and (u0<=u)
        or (u>=0) and (u0>=u) then inc(lessu); //1-sided

        if abs(u0)>=abs(u) then inc(gtru); //2-sided

        if (gam<0) and (gam0<=gam)
        or (gam>=0) and (gam0>=gam) then inc(lessgam);

        if abs(gam0)>=abs(gam)      then inc(gtrgam);

        //if skewness<=skewness0 then inc(lessskew);
        //if skewness>=skewness0 then inc(gtrskew);

    end;

    plessU:=lessU/nsim;
    pgtrU:=gtrU/nsim;
    plessGam:=lessGam/nsim;
    pgtrGam:=gtrGam/nsim;
    //plessSkew:=lessSkew/nsim;
    //pgtrSkew:=gtrSkew/nsim;

    writeln(outfile);
    writeln(outfile,nrejected,' rejected samples during parametric bootstrapping to obtain ',nsim,' tests');
    writeln(outfile);
end;

//---------------------------------------------------------------

procedure Gamma2OddsRatio(gamma         : real;
                      var Oddsratio     : real;
                      var plusinfinity  : boolean;
                      var minusinfinity : boolean);
begin
    if gamma=1 then
    begin
        oddsratio:=9999999;
        plusinfinity:=true;
        minusinfinity:=false;
    end
    else
    if gamma=-1 then
    begin
        oddsratio:=-9999999;
        plusinfinity:=false;
        minusinfinity:=true;
    end
    else
    begin
        oddsratio:=(1+gamma)/(1-gamma);
        plusinfinity:=false;
        minusinfinity:=false;
    end

end;

BEGIN

    TWO_THIRD        := 2.0/3.0;
    CHI_SQUARE_USED  :=TRUE;
    POWER_DIVERGENCE :=FALSE;

END. (** of STAT **)
