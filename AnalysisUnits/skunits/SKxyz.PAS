{$O+,F+}

UNIT SKxyz;

INTERFACE

Uses forms,SKTYPES,skstat,skexa,skrandom,Umain;

// TAB1 procedures modified for epidata - the DIGRAM procedures were dated 21.1.2006

PROCEDURE FILLTAB(VAR TABLE        : LONGINT_DATA_ARRAY;
                      TABLE_ARRAYS : BYTE;
                      THE_LONGINT  : LONGINT);

PROCEDURE FILLETAB(VAR ETAB        : REAL_DATA_ARRAY;
                       ETAB_ARRAYS : BYTE;
                       THE_REAL    : REAL);

PROCEDURE INITIALIZE_TABLE(VAR TABLE        : LONGINT_DATA_ARRAY;
                           VAR TABLE_ARRAYS : BYTE);

PROCEDURE INITIALIZE_ETAB(VAR ETAB        : REAL_DATA_ARRAY;
                          VAR ETAB_ARRAYS : BYTE);


PROCEDURE DISPOSE_TABLE(VAR TABLE        : LONGINT_DATA_ARRAY;
                            VAR TABLE_ARRAYS : BYTE);

PROCEDURE DISPOSE_ETAB(VAR ETAB        : REAL_DATA_ARRAY;
                       VAR ETAB_ARRAYS : BYTE);

PROCEDURE CELLEINIT(NVAR       : BYTE;
                    DIM        : BVECTOR;
                VAR CFACTOR    : IVECTOR;
                VAR CFACTORSUM : Integer);

FUNCTION BTCELLENR(BTSTEP     : BVECTOR;
                   T_NVAR     : BYTE;
                   CFACTOR    : IVECTOR;
                   CFACTORSUM : Integer) : Integer;

PROCEDURE INVERS_BTCELLENR(CELLENR : Integer;
                           T_NVAR  : BYTE;
                           CFACTOR : LVECTOR;
                       VAR BTstep  : BVECTOR);


// xyz_procedures modified for epidata - the DIGRAM procedures were dated 21.1.2006

FUNCTION XYZ_LENGTH(DIMX,DIMY : BYTE): LONGINT;

PROCEDURE INIT_XYZ(VAR ZMAX,NZ             : integer;
                   VAR DIMX,DIMY           : BYTE;
                   VAR TDIMX,TDIMY,XY_SIZE : BYTE;
                   VAR XYZ_TABLE           : LONGINT_DATA_ARRAY;
                   VAR XYZ_ARRAYS          : BYTE;
                   VAR EXYZ_TABLE          : REAL_DATA_ARRAY;
                   VAR EXYZ_ARRAYS         : BYTE);

PROCEDURE DISPOSE_XYZ(VAR ZMAX,NZ             : integer;
                      VAR TDIMX,TDIMY,XY_SIZE : BYTE;
                      VAR XYZ_TABLE           : LONGINT_DATA_ARRAY;
                      VAR XYZ_ARRAYS          : BYTE;
                      VAR EXYZ_TABLE          : REAL_DATA_ARRAY;
                      VAR EXYZ_ARRAYS         : BYTE);

Procedure Init_XYZ_test(var outfile           : Textfile;
                        var Tab2              : TwowayTableArray;
                        var Rtab2             : ETwowayTableArray;
                        var Effective_N       : Longint;
                        var Effective_sim     : integer;
                        var Chi_sprt_status   : Integer;
                        var Gamma_sprt_status : Integer;
                        var CHITOT            : real;
                        var GammaTot          : real;
                        var STOT              : real;
                        var PPQtot            : real;
                        var PMQtot            : real;
                        var DFtot             : Integer;
                            Xdim,Xtype,Ydim,Ytype : byte;
                        var Ordinal           : Boolean;
                        var C                 : Integer;
                        var R                 : Integer;
                            Exact             : boolean);

FUNCTION XYZCELL(X,Y : BYTE; Z: LONGINT;
                 XYZ_TABLE  : LONGINT_DATA_ARRAY;
                 XYZ_ARRAYS : BYTE;
                 TDIMX      : BYTE;
                 XY_SIZE    : BYTE;
             VAR ARRAY_NO   : BYTE;
             VAR POINT      : LONGINT): integer;

FUNCTION EXYZCELL(X,Y : BYTE; Z: LONGINT;
                  EXYZ_TABLE : REAL_DATA_ARRAY;
                  EXYZ_ARRAYS: BYTE;
                  TDIMX      : BYTE;
                  XY_SIZE    : BYTE;
              VAR ARRAY_NO   : BYTE;
              VAR POINT      : LONGINT): REAL;

PROCEDURE TRANSFER_XYZ_SLICE(Z         : LONGINT;
                             XYZ_TABLE : LONGINT_DATA_ARRAY;
                             XYZ_ARRAYS: BYTE;
                            EXYZ_TABLE : REAL_DATA_ARRAY;
                            EXYZ_ARRAYS: BYTE;
                            TDIMX,TDIMY: BYTE;
                            XY_SIZE    : BYTE;
                        VAR   XY_TABLE : TWOWAYTABLEarray;
                        VAR  EXY_TABLE : ETWOWAYTABLEarray);

PROCEDURE TRANSFER_EXYZ_SLICE(Z         : LONGINT;
                            EXYZ_TABLE : REAL_DATA_ARRAY;
                            EXYZ_ARRAYS: BYTE;
                            TDIMX,TDIMY: BYTE;
                            XY_SIZE    : BYTE;
                        VAR  EXY_TABLE : ETWOWAYTABLEarray);

PROCEDURE STORE_XYZ_SLICE(Z          : LONGINT;
                     VAR  XYZ_TABLE  : LONGINT_DATA_ARRAY;
                          XYZ_ARRAYS : BYTE;                   // no longer a VAR
                     VAR EXYZ_TABLE  : REAL_DATA_ARRAY;
                         EXYZ_ARRAYS : BYTE;                   // no longer a Var
                         TDIMX,TDIMY : BYTE;
                         XY_SIZE     : BYTE;
                         XY_TABLE  : TWOWAYTABLEarray;
                         EXY_TABLE  : ETWOWAYTABLEarray);

Procedure TwoWay_tests(var outfile :textfile;
                       c,r   : Byte;
                       ordinal : boolean;
                       Tab2  : TwowaytableArray;
                       RTab2 : ETwowaytableArray;
                       var chi,pchi,chitot,Gamma,Pgamma,ppq,pmq,s,ppqtot,pmqtot,stot : real;
                       var df,dftot,effective_n  : integer);

Procedure Calculate_gamma_statistics(Pmqtot,ppqtot : real;
                                     var Gammatot,AbsGammatot,STOT,U,Pgammatot : real);
procedure Transfer_results(Hypnr : integer;
                           Chitot,DFtot,PchiTot,GammaTot,AbsGammatot,Pgammatot: real;
                           var Results : Resarray);

Procedure start_exact_tests(Hypnr,Number_of_sim           : integer;
                            Var Results                   : ResArray;
                            var Sim,Nchi,Ngamma,NAbsGamma : Integer);

Procedure Evaluate_simulated_results(Sequential,ordinal,twosided : boolean;
                                     chi,chitot,pmq,ppq,Gammatot,AbsGammatot : real;
                                     Sim                : integer;
                                     Seq_p0,Seq_b       : real;
                                     var gamma          : real;
                                     var Nchi,Ngamma,NAbsGamma             : Integer;
                                     var Chi_sprt_status,Gamma_sprt_status : Integer);

PROCEDURE CHEAP_XYZ_TEST(var outfile : textfile;
                   XDIM,YDIM     : BYTE;
                   TDIMX,TDIMY   : BYTE;
                   XY_SIZE       : BYTE;
                   NZ            : LONGINT;
                   XTYPE,YTYPE   : BYTE;
                   XLABEL,YLABEL : CHAR;
                   VLABEL        : CVECTOR;
                   XYZ_TABLE     : LONGINT_DATA_ARRAY;
                   XYZ_ARRAYS    : BYTE;
                   EXYZ_TABLE    : REAL_DATA_ARRAY;
                   EXYZ_ARRAYS   : BYTE;
                   EXACT         : BOOLEAN;
                   NUMBER_OF_SIM : INTEGER;
                   SIMSTART      : INTEGER;
                   SEQUENTIAL    : BOOLEAN;
                   SEQ_P0        : REAL;
                   SEQ_ALPHA     : REAL;
                   SEQ_B         : REAL;
                   EXACT_P_LEVEL : REAL;
                   NVAR          : BYTE;
                   HYPNR         : INTEGER;
                   HYPOTESE      : HYPARRAY;
              VAR  EFFECTIVE_N   : LONGINT;
              VAR  EFFECTIVE_SIM : INTEGER;
              VAR  RESULTS       : RESARRAY);

PROCEDURE XYZ_MH(var outfile : textfile;
                   XDIM,YDIM     : BYTE;
                   TDIMX,TDIMY   : BYTE;
                   XY_SIZE       : BYTE;
                   NZ            : LONGINT;
                   XTYPE,YTYPE   : BYTE;
                   XLABEL,YLABEL : CHAR;
                   VLABEL        : CVECTOR;
                   XYZ_TABLE     : LONGINT_DATA_ARRAY;
                   XYZ_ARRAYS    : BYTE;
                   EXYZ_TABLE    : REAL_DATA_ARRAY;
                   EXYZ_ARRAYS   : BYTE;
                   NVAR          : BYTE;
                   HYPNR         : INTEGER;
                   HYPOTESE      : HYPARRAY;
                   (****
              VAR  EFFECTIVE_N   : LONGINT;
              VAR  EFFECTIVE_SIM : INTEGER;
              VAR  RESULTS       : RESARRAY;
              ****)
              var  PartGam       : Evector;
              var  MH            : Evector);


IMPLEMENTATION


PROCEDURE FILLTAB;
VAR I : BYTE;
    J : INTEGER;
BEGIN
    FOR I:=1 TO TABLE_ARRAYS DO
    FOR J:=1 TO MAX_NUMBER_OF_LONGINT DO
    TABLE(.I.)^(.J.):=THE_LONGINT;
END;

(*------------------------------------------*)

PROCEDURE FILLETAB;
VAR I : BYTE;
    J : INTEGER;
BEGIN
    FOR I:=1 TO ETAB_ARRAYS DO
    FOR J:=1 TO MAX_NUMBER_OF_REALS DO
    ETAB(.I.)^(.J.):=THE_REAL;
END;

(*------------------------------------------*)

PROCEDURE INITIALIZE_TABLE;
VAR I : BYTE;
BEGIN
    TABLE_ARRAYS:=MAX_TAB_ARRAYS;
    FOR I:=1 TO TABLE_ARRAYS DO NEW(TABLE(.I.));
    FOR I:=TABLE_ARRAYS+1 TO MAX_DATA_ARRAYS DO TABLE(.I.):=NIL;
    FILLTAB(TABLE,TABLE_ARRAYS,0);
END;

(*------------------------------------------*)

PROCEDURE INITIALIZE_ETAB;
VAR I : BYTE;
BEGIN
    ETAB_ARRAYS:=MAX_ETAB_ARRAYS;
    FOR I:=1 TO ETAB_ARRAYS DO NEW(ETAB(.I.));
    FOR I:=ETAB_ARRAYS+1 TO MAX_DATA_ARRAYS DO ETAB(.I.):=NIL;
    FILLETAB(ETAB,ETAB_ARRAYS,0.0);
END;

(*---------------------------------------------*)

PROCEDURE DISPOSE_TABLE;
VAR I : BYTE;
BEGIN
    FOR I:=1 TO TABLE_ARRAYS DO
    BEGIN
        DISPOSE(TABLE(.I.));
        TABLE(.I.):=NIL;
    END;
    FOR I:=TABLE_ARRAYS TO MAX_DATA_ARRAYS DO TABLE(.I.):=NIL;
    TABLE_ARRAYS:=0;
END;

(*---------------------------------------------*)

PROCEDURE DISPOSE_ETAB;
VAR I : BYTE;
BEGIN
    FOR I:=1 TO ETAB_ARRAYS DO
    BEGIN
        DISPOSE(ETAB(.I.));
        ETAB(.I.):=NIL;
    END;
    FOR I:=ETAB_ARRAYS TO MAX_DATA_ARRAYS DO ETAB(.I.):=NIL;
    ETAB_ARRAYS:=0;
END;

(*-----------------------------------------------*)

PROCEDURE CELLEINIT;
VAR
    I: BYTE;
BEGIN
    CFACTOR(.1.) := 1;
    CFACTORSUM:=0;

    FOR I:=2 TO NVAR DO
    BEGIN
        CFACTOR(.I.):=CFACTOR(.I-1.)*DIM(.I-1.);
        CFACTORSUM:=CFACTORSUM+CFACTOR(.I.);
    END;

    FOR I:=NVAR+1 TO Vectorlength DO CFACTOR(.I.):=0;
END; (*** CELLEINIT ***)


(*---------------------------------------------------------*)

FUNCTION BTCELLENR;
VAR CNR : Integer;
    I   : BYTE;
BEGIN
    CNR:=BTSTEP(.1.)-CFACTORSUM;
    FOR I:=2 TO T_NVAR DO CNR:=CNR+BTSTEP(.I.)*CFACTOR(.I.);
    BTCELLENR:=CNR;
END; (** BTCELLENR **)

(*-----------------------------------------------------------------*)

PROCEDURE INVERS_BTCELLENR;
VAR I       : BYTE;
    N,K,DIF : integer;
BEGIN

    //FILLCHAR(BTSTEP,SIZEOF(BTSTEP),1);
    BTstep:=OneBytes;

    N:=CELLENR;
    FOR I:=T_NVAR DOWNTO 2 DO
    BEGIN
        K:=N DIV CFACTOR(.I.);
        DIF:=N-K*CFACTOR(.I.);
        IF DIF>0 THEN
        BEGIN
            N:=DIF;
            BTSTEP(.I.):=K+1;
        END
        ELSE
        BEGIN
            N:=CFACTOR(.I.);
            BTSTEP(.I.):=K;
        END;
    END;
    BTstep(.1.):=N;

END; (* of INVERS_BTCELLENR *)

(*------------------------------------------*)

FUNCTION XYZ_LENGTH;
VAR N : BYTE;
BEGIN
    N:=(DIMX+1)*(DIMY+1);
    XYZ_LENGTH:=(MAX_NUMBER_OF_REALS*MAX_TAB_ARRAYS) DIV N;
END; (* of XYZ_LENGTH *)

(*-------------------------------------------------------*)

PROCEDURE INIT_XYZ;
BEGIN
    INITIALIZE_TABLE(XYZ_TABLE,XYZ_ARRAYS);
    INITIALIZE_ETAB(EXYZ_TABLE,EXYZ_ARRAYS);

    TDIMX:=DIMX+1;
    TDIMY:=DIMY+1;
    XY_SIZE:=TDIMX*TDIMY;
    NZ:=0;

    ZMAX:=XYZ_LENGTH(DIMX,DIMY);
END;

(*------------------------------------------*)

PROCEDURE DISPOSE_XYZ;
BEGIN
    DISPOSE_TABLE(XYZ_TABLE,XYZ_ARRAYS);
    DISPOSE_ETAB(EXYZ_TABLE,EXYZ_ARRAYS);
    ZMAX:=0;    (* DIMZ_MAX  *)
    TDIMY:=0;
    TDIMX:=0;
    NZ:=0;
    XY_SIZE:=0;
END;

//---------------------------------------------

Procedure Init_XYZ_test(var outfile           : Textfile;
                        var Tab2              : TwowayTableArray;
                        var Rtab2             : ETwowayTableArray;
                        var Effective_N       : Longint;
                        var Effective_sim     : integer;
                        var Chi_sprt_status   : Integer;
                        var Gamma_sprt_status : Integer;
                        var CHITOT            : real;
                        var GammaTot          : real;
                        var STOT              : real;
                        var PPQtot            : real;
                        var PMQtot            : real;
                        var DFtot             : Integer;
                            Xdim,Xtype,Ydim,Ytype : byte;
                        var Ordinal           : Boolean;
                        var C                 : Integer;
                        var R                 : Integer;
                            Exact             : boolean);
begin
    FILLCHAR(TAB2,SIZEOF(TAB2),0);
    FILLCHAR(RTAB2,SIZEOF(RTAB2),0);

    EFFECTIVE_N   := 0;
    EFFECTIVE_SIM := 0;

    INIT_EXACT_TEST(Outfile,EXACT,SIMSTART);

    CHI_SPRT_STATUS:=0;
    GAMMA_SPRT_STATUS:=0;

    CHITOT:=0;  GAMMATOT:=0;  STOT:=0;  PPQTOT:=0;  PMQTOT:=0;
    DFTOT:=0;

    ORDINAL:=TRUE;

    IF (XDIM>2) AND (XTYPE=2) THEN ORDINAL:=FALSE ELSE
    IF (YDIM>2) AND (YTYPE=2) THEN ORDINAL:=FALSE;

    IF NOT ORDINAL THEN GAMMA_SPRT_STATUS:=1;

    C:=XDIM;   R:=YDIM;
end;

//--------------------------------------------------------

FUNCTION XYZCELL;
VAR N: integer;
BEGIN
    N:=(Z-1)*XY_SIZE+(Y-1)*TDIMX+X;

    ARRAY_NO := N DIV MAX_NUMBER_OF_LONGINT;
    POINT    := N MOD MAX_NUMBER_OF_LONGINT;

    IF POINT=0 THEN POINT:=MAX_NUMBER_OF_LONGINT
               ELSE INC(ARRAY_NO);

    XYZCELL:=XYZ_TABLE(.ARRAY_NO.)^(.POINT.);
END; (* of XYZCELL *)

(*-------------------------------------------------------*)

FUNCTION EXYZCELL;
VAR N: integer;
BEGIN
    N:=(Z-1)*XY_SIZE+(Y-1)*TDIMX+X;

    ARRAY_NO := N DIV MAX_NUMBER_OF_REALS;
    POINT    := N MOD MAX_NUMBER_OF_REALS;

    IF POINT=0 THEN POINT:=MAX_NUMBER_OF_REALS
               ELSE INC(ARRAY_NO);
    EXYZCELL:=EXYZ_TABLE(.ARRAY_NO.)^(.POINT.);
END; (* of EXYZCELL *)

(*-------------------------------------------------------*)

PROCEDURE TRANSFER_XYZ_SLICE;

VAR  POINT        : integer;
     ARRAY_NO,X,Y : BYTE;

BEGIN

    FOR Y:=1 TO TDIMY DO
    FOR X:=1 TO TDIMX DO
    BEGIN
        XY_TABLE(.X,Y.):=XYZCELL(X,Y,Z,XYZ_TABLE,XYZ_ARRAYS,
                                 TDIMX,XY_SIZE,ARRAY_NO,POINT);
       EXY_TABLE(.X,Y.):=EXYZCELL(X,Y,Z,EXYZ_TABLE,EXYZ_ARRAYS,
                                  TDIMX,XY_SIZE,ARRAY_NO,POINT);
    END;
END;

(*---------------------------------------------------*)

PROCEDURE TRANSFER_EXYZ_SLICE;
VAR  POINT        : integer;
     ARRAY_NO,X,Y : BYTE;
BEGIN

    FOR Y:=1 TO TDIMY DO
    FOR X:=1 TO TDIMX DO
    EXY_TABLE(.X,Y.):=EXYZCELL(X,Y,Z,EXYZ_TABLE,EXYZ_ARRAYS,
                               TDIMX,XY_SIZE,ARRAY_NO,POINT);
END;

(*---------------------------------------------------*)

PROCEDURE STORE_XYZ_SLICE;
VAR  N,POINT      : integer;
     ARRAY_NO,X,Y : BYTE;
     A            : REAL;
BEGIN
    // N:=(Z-1)*XY_SIZE; why was this included???

    FOR Y:=1 TO TDIMY DO
    FOR X:=1 TO TDIMX DO
    BEGIN
        // Note N and A is determined here in order to get array numbers and points
        // This can probably be optimized in some way
        N:=XYZCELL(X,Y,Z,XYZ_TABLE,XYZ_ARRAYS,
                   TDIMX,XY_SIZE,ARRAY_NO,POINT);
        XYZ_TABLE(.ARRAY_NO.)^(.POINT.):=XY_TABLE(.X,Y.);
        A:=EXYZCELL(X,Y,Z,EXYZ_TABLE,EXYZ_ARRAYS,
                               TDIMX,XY_SIZE,ARRAY_NO,POINT);
        EXYZ_TABLE(.ARRAY_NO.)^(.POINT.):=EXY_TABLE(.X,Y.);
    END;
END;

//------------------------------------------------------------------

Procedure TwoWay_tests(var outfile: Textfile;
                       c,r   : Byte;
                       ordinal : boolean;
                       Tab2  : TwowaytableArray;
                       RTab2 : ETwowaytableArray;
                       var chi,pchi,chitot,Gamma,Pgamma,ppq,pmq,s,ppqtot,pmqtot,stot : real;
                       var df,dftot,effective_n  : integer);
begin
    //writeln(outfile,'Input to two_way_tests: ',c:2,r:3);
    RCCHI(C,R,TAB2,RTAB2,CHI,DF,PCHI,TRUE);
    CHITOT:=CHITOT+CHI;   DFTOT:=DFTOT+DF;

    IF DF>0 THEN EFFECTIVE_N:=EFFECTIVE_N+TAB2(.C+1,R+1.);

    IF ORDINAL THEN
    BEGIN
        RCGAMMA(C,R,TAB2,GAMMA,PGAMMA,PPQ,PMQ,S,TRUE);
        PPQTOT:=PPQTOT+PPQ;
        PMQTOT:=PMQTOT+PMQ;
        STOT:=STOT+S;
    END;
end;

//-------------------------------------------------------------------

Procedure Calculate_gamma_statistics(Pmqtot,ppqtot : real;
                                     var Gammatot,AbsGammatot,STOT,U,Pgammatot : real);
begin
    IF PPQTOT>0 THEN
    BEGIN
        GAMMATOT:=PMQTOT/PPQTOT;
        AbsGammaTot := abs(gammatot);
        STOT:=STOT/PPQTOT;   STOT:=STOT/PPQTOT;
        IF STOT<=0.0 THEN U:=4.0 ELSE
        U:=GAMMATOT/SQRT(STOT);
        IF U<0.0 THEN U:=-U;
        IF U>4.0 THEN PGAMMATOT:=0.000001 ELSE
        PGAMMATOT:=PNORMAL(U);
    END
    ELSE PGAMMATOT:=1.0;
end;

//------------------------------------------------------------------

procedure Transfer_results(Hypnr : integer;
                           Chitot,DFtot,PchiTot,GammaTot,AbsGammatot,Pgammatot: real;
                           var Results : Resarray);
begin
     RESULTS(.HYPNR,1.):=CHITOT;
     RESULTS(.HYPNR,2.):=DFTOT;
     RESULTS(.HYPNR,3.):=PCHITOT;
     RESULTS(.HYPNR,5.):=GAMMATOT;
     RESULTS(.HYPNR,6.):=PGAMMATOT;
     RESULTS(.HYPNR,8.):=0;

end;

//-------------------------------------------------------------------

Procedure start_exact_tests(Hypnr,Number_of_sim           : integer;
                            Var Results                   : ResArray;
                            var Sim,Nchi,Ngamma,NAbsGamma : Integer);
begin
    RESULTS(.HYPNR,4.):=0;
    RESULTS(.HYPNR,7.):=0;

    SIM:=0; NCHI:=0; NGAMMA:=0; NabsGamma:=0;

    (************************  not used in epidata *********************
    If sequential and (seq_b<1000) then
    InitProgressBox(0,number_of_sim,1,'Exact test','Repeated MC test')
    else
    If sequential and (seq_limit<number_of_sim) then
    InitProgressBox(0,number_of_sim,1,'Exact test','Sequential MC test')
    else
    InitProgressBox(0,number_of_sim,1,'Exact test','Monte Carlo test');
    *******************************************************************)
end;

//---------------------------------------------------------------------

Procedure Evaluate_simulated_results(Sequential,ordinal,twosided : boolean;
                                     chi,chitot,pmq,ppq,Gammatot,AbsGammatot : real;
                                     Sim                : integer;
                                     Seq_p0,Seq_b       : real;
                                     var gamma          : real;
                                     var Nchi,Ngamma,NAbsGamma             : Integer;
                                     var Chi_sprt_status,Gamma_sprt_status : Integer);
var ng : integer;
begin
    IF CHI>=CHITOT THEN INC(NCHI);

    chi_sprt_status:=0;
    gamma_sprt_status:=0;

    (*****  not used in epidata ***********
    IF SEQUENTIAL THEN
    BEGIN
        IF SEQ_T(NCHI,SIM,SEQ_P0)>=SEQ_B THEN CHI_SPRT_STATUS:=1 else
        if nchi>=seq_limit then CHI_SPRT_STATUS:=1;

    END;
    ***************************************)

    IF ORDINAL THEN
    BEGIN
        IF PPQ>0 THEN GAMMA:=PMQ/PPQ ELSE GAMMA:=0;
        IF ((GAMMATOT>0) AND (GAMMA>=GAMMATOT))
        OR ((GAMMATOT<0) AND (GAMMA<=GAMMATOT))
        OR (GAMMATOT=0) THEN INC(NGAMMA);

        if abs(gamma)>=absgammatot then inc(NAbsGamma);

        if twosided then ng:=nabsgamma else ng:=ngamma;

        (*********** not used in epidata **************
        IF SEQUENTIAL then
        BEGIN
            //IF SEQ_T(NGAMMA,SIM,SEQ_P0)>=SEQ_B
            //IF SEQ_T(NG,SIM,SEQ_P0)>=SEQ_B THEN GAMMA_SPRT_STATUS:=1;
            IF SEQ_T(NG,SIM,SEQ_P0)>=SEQ_B THEN GAMMA_SPRT_STATUS:=1 else
            if ng>=seq_limit then gamma_sprt_status:=1;
        END;
        ***********************************************)
    END;

end;

//------------------------------------------------------------------

PROCEDURE XYZ_MH;
LABEL    HYPOTESESLUT;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)

      X,Y             : BYTE;
      SUCCESS,ORDINAL : BOOLEAN;
      HYP             : BVECTOR;

      TAB2            : TWOWAYTABLEarray;
      RTAB2           : ETWOWAYTABLEarray;

      Z,I,J,K,N,M,C,R,V1,V2 : INTEGER;
      V                     : BVECTOR;
      FRA,TIL,THELEVEL      : BYTE;
      SIM,LOCAL_NSIM,NSIM,NCHI,NGAMMA,NAbsGamma,
      CHI_SPRT_STATUS,GAMMA_SPRT_STATUS : INTEGER;
      DUMMYCHAR                         : CHAR;

      CHITOT,GAMMATOT,AbsGammaTot,
      STOT,PPQTOT,PMQTOT,PMIN,
      PCHI,CHI,CHI1,PPQ,PPQ1,PMQ,PMQ1,GAMMA,GAMMA1,
      PGAMMA,S,PCHITOT,U,PGAMMATOT                   : Real;
      DF,DFTOT,ACCEPTNR,NSIM1,NSIM2,SIM1,SIM2        : INTEGER;

      xxx,yyy : integer;

      s1,s2 :string;

      twosided : boolean;

      a,b,cc,d,nsquare,ad,bc : integer;
      mhcounter,mhdenominator,a1,a2,a3,b1,b2,b3,mhvar : extended;

      procedure GamtoOR(gamma : extended; var Oddsrat : extended);
      begin
          if gamma<1 then oddsrat:=(1+gamma)/(1-gamma)
          else oddsrat:=999999.999;
      end;

      procedure ORtoGam(var gamma : extended; Oddsrat : extended);
      begin
          Gamma:=(oddsrat-1)/(oddsrat+1);
      end;
BEGIN
     if (xdim>2) or (ydim>2) then
     begin
         Writeln(outfile,'2x2 table required for Mantel-Haenszel tests');
         goto hypoteseslut;
     end;

     Init_XYZ_test(outfile,Tab2,Rtab2,xxx,yyy,//Effective_N,Effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R,false);

     MHcounter:=0; MHdenominator:=0;
     a1:=0; a2:=0; a3:=0;
     b1:=0; b2:=0; b3:=0;

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);


         TwoWay_tests(outfile,xdim,ydim,ordinal,tab2,rtab2,chi,pchi,chitot,gamma,pgamma,
                      ppq,pmq,s,ppqtot,pmqtot,stot,df,dftot,xxx);

         if tab2(.3,3.)>0 then
         begin
             a:=tab2(.1,1.);
             b:=tab2(.1,2.);
             cc:=tab2(.2,1.);
             d:=tab2(.2,2.);
             n:=tab2(.3,3.);
             nsquare:=n*n;
             ad:=a*d;
             bc:=b*cc;

             mhCounter:=mhCounter+tab2(.1,1.)*tab2(.2,2.)/tab2(.3,3.);
             mhDenominator:=mhDenominator+tab2(.1,2.)*tab2(.2,1.)/tab2(.3,3.);

             a1:=a1+(a+d)*(ad/nsquare);
             a2:=a2+((a+d)*bc+(b+cc)*ad)/nsquare;
             a3:=a3+(b+cc)*(bc/nsquare);

             b1:=b1+ad/n;
             b3:=b3+bc/n;
         end;
     END; // OF A 2-WAY TABLE

     PCHITOT:=PFCHI(DFTOT,CHITOT);
     PMIN:=PCHITOT;

     IF ORDINAL THEN Calculate_gamma_statistics(PMQtot,PPQtot,GammaTot,AbsGammatot,Stot,U,Pgammatot);

     if pgammatot<pmin then pmin:=pgammatot;

     partgam(.6.):=gammatot;
     partgam(.7.):=sqrt(stot)/ppqtot;

     GamtoOR(partgam(.6.),partgam(.1.));
     GamtoOR(partgam(.6.)-1.96*partgam(.7.),partgam(.2.));
     GamtoOR(partgam(.6.)+1.96*partgam(.7.),partgam(.3.));

     PartGam(.4.):=ln(Partgam(.1.));

     Partgam(.5.):=(ln(partgam(.3.))-partgam(.4.))/1.96;

     if MHdenominator>0.0 then
     begin
         MH(.1.):=MHcounter/MHdenominator;
         if MHcounter>0.0 then
         begin
             MH(.4.):=ln(MH(.1.));
             mhvar:=0.5*(a1/(b1*b1)+a2/(b1*b3)+a3/(b3*b3));
             mh(.5.):=sqrt(mhvar);
             mh(.2.):=exp(mh(.4.)-1.96*mh(.5.));
             mh(.3.):=exp(mh(.4.)+1.96*mh(.5.));
             ORtoGAM(MH(.6.),MH(.1.));
             ORtoGAM(MH(.7.),MH(.3.));
             mh(.7.):=(mh(.7.)-mh(.6.))/1.96;
         end
         else
         begin
             mh(.4.):=-999999.99;
             mh(.5.):=0;
             mh(.6.):=-1;
             mh(.7.):=0;
         end;

     end
     else
     begin
         MH(.1.):=999999.99;
         MH(.2.):=999999.99;
         MH(.3.):=999999.99;
         MH(.4.):=999999.99;
         MH(.5.):=0;
         MH(.6.):=1;
         MH(.7.):=0;
     end;

HYPOTESESLUT:
END;  (*** OF CHEAP_XYZ_TEST ***)

(*-------------------------------------------------------*)

PROCEDURE CHEAP_XYZ_TEST;
LABEL    HYPOTESESLUT,SIM_START,SIM_FINISH,SIM_FINISH1;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)

      X,Y             : BYTE;
      SUCCESS,ORDINAL : BOOLEAN;
      HYP             : BVECTOR;

      TAB2            : TWOWAYTABLEarray;
      RTAB2           : ETWOWAYTABLEarray;

      Z,I,J,K,N,M,C,R,V1,V2 : INTEGER;
      V                     : BVECTOR;
      FRA,TIL,THELEVEL      : BYTE;
      SIM,LOCAL_NSIM,NSIM,NCHI,NGAMMA,NAbsGamma,
      CHI_SPRT_STATUS,GAMMA_SPRT_STATUS : INTEGER;
      DUMMYCHAR                         : CHAR;

      CHITOT,GAMMATOT,AbsGammaTot,
      STOT,PPQTOT,PMQTOT,PMIN,
      PCHI,CHI,CHI1,PPQ,PPQ1,PMQ,PMQ1,GAMMA,GAMMA1,
      PGAMMA,S,PCHITOT,U,PGAMMATOT                   : Real;
      DF,DFTOT,ACCEPTNR,NSIM1,NSIM2,SIM1,SIM2        : INTEGER;

      s1,s2 :string;

      twosided : boolean;

BEGIN
     Init_XYZ_test(outfile,Tab2,Rtab2,Effective_N,Effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R,Exact);

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         (*******
         writeln(outfile,'slice no. ',z);
         for i:=1 to tdimy do
         begin
             for j:=1 to tdimx do write(outfile,tab2(.j,i.):5);;
             writeln(outfile);
         end;
         writeln(outfile,'expected');
         for i:=1 to tdimy do
         begin
             for j:=1 to tdimx do write(outfile,rtab2(.j,i.):5:1);;
             writeln(outfile);
         end;
         (********)


         TwoWay_tests(outfile,xdim,ydim,ordinal,tab2,rtab2,chi,pchi,chitot,gamma,pgamma,
                      ppq,pmq,s,ppqtot,pmqtot,stot,df,dftot,effective_n);

         //writeln(outfile,z:3,chi:6:1,df:3,pchi:6:3,gamma:8:3,2*pgamma:6:3);

     END; // OF A 2-WAY TABLE

     PCHITOT:=PFCHI(DFTOT,CHITOT);
     PMIN:=PCHITOT;

     IF ORDINAL THEN Calculate_gamma_statistics(PMQtot,PPQtot,GammaTot,AbsGammatot,Stot,U,Pgammatot);

     if pgammatot<pmin then pmin:=pgammatot;
     //writeln(outfile,'tot',chitot:6:1,dftot:3,pchitot:6:3,gammatot:8:3,2*pgammatot:6:3);

     (********)
     Transfer_results(Hypnr,ChiTot,DfTot,PchiTot,GammaTot,AbsGammaTot,PGammaTot,Results);
     (****)
     //*** CALCULATION OF EXACT P-VALUES ***

     //goto sim_finish;

     s2:=Amainform.StatusBar.panels(.2.).Text;

     IF EXACT then //AND (PMIN<=EXACT_P_LEVEL) THEN
     BEGIN
         Start_exact_tests(hypnr,number_of_sim,results,sim,Nchi,Ngamma,NabsGamma);
SIM_START:
         Inc(SIM);
         IF SIM>NUMBER_OF_SIM THEN
         BEGIN
             SIM:=SIM-1;
             GOTO SIM_FINISH;
         END;
         str(sim,s1);
         Amainform.StatusBar.panels(.2.).Text:=s1;
         Application.ProcessMessages;

         CHI:=0;
         PPQ:=0;
         PMQ:=0;

         FOR Z:=1 TO NZ DO
         BEGIN
             TRANSFER_EXYZ_SLICE(Z,EXYZ_TABLE,EXYZ_ARRAYS,
                                 TDIMX,TDIMY,XY_SIZE,RTAB2);

             GENTAB(outfile,XDIM,YDIM,RTAB2,ORDINAL,CHI1,PPQ1,PMQ1);
             CHI:=CHI+CHI1;
             IF ORDINAL THEN
             BEGIN
                 PPQ:=PPQ+PPQ1;
                 PMQ:=PMQ+PMQ1;
             END;
         END;

         //************************************************************
         // *** TESTSTATISTICS HAVE BEEN GENERATED FOR ALL SEPARATE  ***
         // *** TABLES. WHAT REMAINS IS JUST TO  CALCULATE THE TOTAL ***
         // *** STATISTICS AND TO COMPARE THE GENERATED STATISTICS   ***
         // *** WITH THE OBSERVED                                    ***
         // ************************************************************

         twosided:=true;

         Evaluate_simulated_results(Sequential,Ordinal,twosided,chi,chitot,
                                    pmq,ppq,Gammatot,AbsGammatot,sim,Seq_p0,Seq_b,
                                    Gamma,Nchi,Ngamma,NAbsGamma,Chi_sprt_status,Gamma_sprt_status);

         (**********************************************************************
         if CancelAfterProgressBoxStep then goto sim_finish;

         if stop_exact_tests(sequential,chi_sprt_status,gamma_sprt_status,sim,nsim)
         then goto sim_finish1;
         ************************************)
         goto sim_start;

SIM_FINISH:
         NSIM:=SIM;
SIM_FINISH1:
         //HideProgressBox;

         RESULTS(.HYPNR,4.):=NCHI/NSIM;

         IF ORDINAL THEN
         begin
             RESULTS(.HYPNR,7.):= NGAMMA/NSIM;
             Results(.hypnr,9.):= NabsGamma/Nsim;
         end;

         EFFECTIVE_SIM      := SIM;
         RESULTS(.HYPNR,8.) := SIM;
     END;  // OF EXACT-PVALUES

HYPOTESESLUT:
    Amainform.StatusBar.panels(.2.).Text:=s2;
END;  (*** OF CHEAP_XYZ_TEST ***)

(*-------------------------------------------------------*)

BEGIN
end. (** of SKXYZ **)
