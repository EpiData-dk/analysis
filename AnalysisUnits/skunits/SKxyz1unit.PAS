{$O+,F+}

UNIT SKxyz1Unit;

// xyz_procedures modified for epidata - the DIGRAM procedures were dated 21.1.2006

INTERFACE

Uses forms,SKTYPES,SKVARS,DGRvars,SKcat,SKCMDS,SKout,SKdata1,SKTAB1,
     SKSTAT,SKmca,SKRANDom,SKEXA1,skqsort;


(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* NOTICE THAT THIS UNIT USES THE FOLLOWING GLOBAL  *)
(* VARIABLES DEFINED IN VARSUNIT:                   *)
(*                DIMX,DIMY,NZ                      *)
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)

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
                        var R                 : Integer);

procedure NoResults(hypnr: integer; var Results : ResArray);

FUNCTION XYZ_CELL_number(X,Y           : BYTE;
                         Z             : LONGINT;
                         TDIMX,XY_SIZE : BYTE): LONGINT;

// XYZ_cell_number finds the cell number in the XYZ table.
// it is used in this unit and in Bias3 and Bias7

FUNCTION TABCOUNT_VALUE(CASENO          : LONGINT;
                        TABCOUNT        : LONGINT_DATA_ARRAY;
                        TABCOUNT_ARRAYS : BYTE;
                        VAR ARRAY_NO    : BYTE;
                        VAR POINT       : LONGINT): LONGINT;

PROCEDURE ADD_TABCOUNT_TO_XYZ(CASENO          : LONGINT;
                            TABCOUNT          : LONGINT_DATA_ARRAY;
                            TABCOUNT_ARRAYS   : BYTE;
                            X,Y               : BYTE;
                            Z                 : LONGINT;
                           TDIMX,TDIMY,XY_SIZE : BYTE;
                           ZMAX               : LONGINT;
                        VAR XYZ_TABLE         : LONGINT_DATA_ARRAY;
                        VAR XYZ_ARRAYS        : BYTE;
                        VAR ARRAY_NO          : BYTE;
                        VAR POINT             : LONGINT);

FUNCTION XYZ_LENGTH(DIMX,DIMY : BYTE): LONGINT;

FUNCTION XYZCELL(X,Y : BYTE; Z: LONGINT;
                 XYZ_TABLE  : LONGINT_DATA_ARRAY;
                 XYZ_ARRAYS : BYTE;
                 TDIMX      : BYTE;
                 XY_SIZE    : BYTE;
             VAR ARRAY_NO   : BYTE;
             VAR POINT      : LONGINT): LONGINT;

FUNCTION EXYZCELL(X,Y : BYTE; Z: LONGINT;
                  EXYZ_TABLE : REAL_DATA_ARRAY;
                  EXYZ_ARRAYS: BYTE;
                  TDIMX      : BYTE;
                  XY_SIZE    : BYTE;
              VAR ARRAY_NO   : BYTE;
              VAR POINT      : LONGINT): REAL;

Procedure Set_XYZCELL(X,Y : BYTE; Z: LONGINT;
                 value      : Longint;
            var  XYZ_TABLE  : LONGINT_DATA_ARRAY;
                 XYZ_ARRAYS : BYTE;
                 TDIMX      : BYTE;
                 XY_SIZE    : BYTE;
             VAR ARRAY_NO   : BYTE;
             VAR POINT      : LONGINT);

Procedure Set_EXYZCELL(X,Y : BYTE; Z: LONGINT;
                  value      : real;
              var EXYZ_TABLE : REAL_DATA_ARRAY;
                  EXYZ_ARRAYS: BYTE;
                  TDIMX      : BYTE;
                  XY_SIZE    : BYTE;
              VAR ARRAY_NO   : BYTE;
              VAR POINT      : LONGINT);

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

PROCEDURE STORE_EXYZ_SLICE(Z          : LONGINT;
                      VAR EXYZ_TABLE  : REAL_DATA_ARRAY;
                          EXYZ_ARRAYS : BYTE;                  // no longer a Var
                          TDIMX,TDIMY : BYTE;
                          XY_SIZE     : BYTE;
                          EXY_TABLE  : ETWOWAYTABLEarray);

PROCEDURE INIT_XYZ(VAR ZMAX,NZ             : LONGINT;
                   VAR DIMX,DIMY           : BYTE;
                   VAR TDIMX,TDIMY,XY_SIZE : BYTE;
                   VAR XYZ_TABLE           : LONGINT_DATA_ARRAY;
                   VAR XYZ_ARRAYS          : BYTE;
                   VAR EXYZ_TABLE          : REAL_DATA_ARRAY;
                   VAR EXYZ_ARRAYS         : BYTE);

PROCEDURE DISPOSE_XYZ(VAR ZMAX,NZ             : LONGINT;
                      VAR TDIMX,TDIMY,XY_SIZE : BYTE;
                      VAR XYZ_TABLE           : LONGINT_DATA_ARRAY;
                      VAR XYZ_ARRAYS          : BYTE;
                      VAR EXYZ_TABLE          : REAL_DATA_ARRAY;
                      VAR EXYZ_ARRAYS         : BYTE);

FUNCTION XYZ_FREQ(X,Y           : BYTE;
                  Z             : LONGINT;
                  XYZ_TABLE     : LONGINT_DATA_ARRAY;
                  XYZ_ARRAYS    : BYTE;
                  TDIMX,XY_SIZE : BYTE): LONGINT;

FUNCTION XYZ_EXPECTED(X,Y           : BYTE;
                      Z             : LONGINT;
                      EXYZ_TABLE    : REAL_DATA_ARRAY;
                      EXYZ_ARRAYS   : BYTE;
                      TDIMX,XY_SIZE : BYTE): REAL;


// Make_XYZ_Table creates an XYZ table for a specific hypothesis
// from an ordinary not-more-that-8-dimensional table

PROCEDURE MAKE_XYZ_TABLE(TABLE        : LONGINT_DATA_ARRAY;
                         TABLE_ARRAYS : BYTE;
                         NVAR         : BYTE;
                         DIM          : BVECTOR;
                         TDIM         : BVECTOR;
                         VTYPE        : BVECTOR;
                         VLABEL       : CVECTOR;
                         HYPOTESE     : BVECTOR;
                         CFACTOR      : LVECTOR;
                         CFACTORSUM   : LONGINT;
                    VAR  OLDVAR       : BVECTOR;
                    VAR  DIMX,DIMY    : BYTE;
                    VAR  TDIMX,TDIMY  : BYTE;
                    VAR  XY_SIZE      : BYTE;
                    VAR  ZMAX,NZ      : LONGINT;
                    VAR  XYZ_TABLE    : LONGINT_DATA_ARRAY;
                    VAR  XYZ_ARRAYS   : BYTE;
                    VAR  EXYZ_TABLE   : REAL_DATA_ARRAY;
                    VAR  EXYZ_ARRAYS  : BYTE;
                    VAR  NVAR1        : BYTE;
                    VAR  DIM1         : BVECTOR;
                    VAR  TDIM1        : BVECTOR;
                    VAR  VTYPE1       : BVECTOR;
                    VAR  VLABEL1      : CVECTOR;
                    VAR  SUCCESS      : BOOLEAN);

// Make_XYZ_BTable creates an XYZ table for a specific hypothesis
// from a big table

PROCEDURE MAKE_XYZ_BTABLE(var outfile : textfile;
                         TABLE        : LONGINT_DATA_ARRAY;
                         TABLE_ARRAYS : BYTE;
                         NVAR         : BYTE;
                         DIM          : BVECTOR;
                         TDIM         : BVECTOR;
                         VTYPE        : BVECTOR;
                         VLABEL       : CVECTOR;
                         HYPOTESE     : BVECTOR;
                         CFACTOR      : LVECTOR;
                         CFACTORSUM   : LONGINT;
                    VAR  OLDVAR       : BVECTOR;
                    VAR  DIMX,DIMY    : BYTE;
                    VAR  TDIMX,TDIMY  : BYTE;
                    VAR  XY_SIZE      : BYTE;
                    VAR  ZMAX,NZ      : LONGINT;
                    VAR  XYZ_TABLE    : LONGINT_DATA_ARRAY;
                    VAR  XYZ_ARRAYS   : BYTE;
                    VAR  EXYZ_TABLE   : REAL_DATA_ARRAY;
                    VAR  EXYZ_ARRAYS  : BYTE;
                    VAR  NVAR1        : BYTE;
                    VAR  DIM1         : BVECTOR;
                    VAR  TDIM1        : BVECTOR;
                    VAR  VTYPE1       : BVECTOR;
                    VAR  VLABEL1      : CVECTOR;
                    VAR  SUCCESS      : BOOLEAN);


          (*++++++++++++++++++++++++++++++*)
          (* THE XYZ_TEST PROCEDURE       *)
          (* PERFORMS THE BASIC ANALYSIS  *)
          (* OF THE TABLES EXCLUDING LOCAL*)
          (* TESTS                        *)
          (*++++++++++++++++++++++++++++++*)

// XYZ_test is used in EXA3, EXA4, EXA5, Big1 and BIG3

PROCEDURE XYZ_TEST(var outfile : textfile;
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
                   NVAR          : BYTE;
                   HYPNR         : INTEGER;
                   HYPOTESE      : HYPARRAY;
              VAR  EFFECTIVE_N   : LONGINT;
              VAR  EFFECTIVE_SIM : INTEGER;
              VAR  RESULTS       : RESARRAY);


          (*++++++++++++++++++++++++++++++*)
          (* The Cheap_XYZ_test procedure *)
          (* CALCULATES THE EXACT P-VALUES*)
          (* BUT WILL NOT REPORT ANYTHING *)
          (* ON ITS OWN                   *)
          (*++++++++++++++++++++++++++++++*)

// Cheap_XYZ_test is used in BIG1 and BIG2

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

          (*++++++++++++++++++++++++++++++*)
          (* THE XYZ_TEST PROCEDURE       *)
          (* PERFORMS THE BASIC ANALYSIS  *)
          (* OF THE TABLES EXCLUDING LOCAL*)
          (* TESTS                        *)
          (*++++++++++++++++++++++++++++++*)

          (*++++++++++++++++++++++++++++++*)
          (* THIS VERY CHEAP PROCEDURE    *)
          (* CALCULATES THE EXACT P-VALUES*)
          (* BUT WILL NOT REPORT ANYTHING *)
          (* ON ITS OWN                   *)
          (*++++++++++++++++++++++++++++++*)

          (*++++++++++++++++++++++++++++++*)
          (* EXACT P-VALUES WILL ONLY BE  *)
          (* CALCULATED IF ASYMPTOTIC     *)
          (* P-VALUES ARE SIGNIFICANT AT  *)
          (* A EXACT_P_LEVEL              *)
          (*++++++++++++++++++++++++++++++*)

// Pairwise_XYZ_test calculates test statistics in reduced tables with a subset of
// X and/or Y values.

// If permute_x then the test compares x1 & x2
// If permute_y then the test compares y1 & y2

PROCEDURE Pairwise_XYZ_TEST(var outfile : textfile;
                   permute_x,permute_y : boolean;
                   XDIM,YDIM     : BYTE;
                   x1,x2,y1,y2   : byte;
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
              VAR  RESULTS       : RESARRAY;
                   PrintOutput   : Boolean);

          (*++++++++++++++++++++++++++++++*)
          (* THE XYZ_TEST PROCEDURE       *)
          (* PERFORMS THE BASIC ANALYSIS  *)
          (* OF THE TABLES EXCLUDING LOCAL*)
          (* TESTS                        *)
          (*++++++++++++++++++++++++++++++*)

          (*++++++++++++++++++++++++++++++*)
          (* THIS VERY CHEAP PROCEDURE    *)
          (* CALCULATES THE EXACT P-VALUES*)
          (* BUT WILL NOT REPORT ANYTHING *)
          (* ON ITS OWN                   *)
          (*++++++++++++++++++++++++++++++*)

          (*++++++++++++++++++++++++++++++*)
          (* EXACT P-VALUES WILL ONLY BE  *)
          (* CALCULATED IF ASYMPTOTIC     *)
          (* P-VALUES ARE SIGNIFICANT AT  *)
          (* A EXACT_P_LEVEL              *)
          (*++++++++++++++++++++++++++++++*)

PROCEDURE Pseudogamma(var outfile : textfile;
                   permute_x     : boolean;
                   x_restricted   : boolean;
                   x_restrictions : bmatrix;
                   permute_y     : boolean;
                   y_restricted   : boolean;
                   y_restrictions : bmatrix;
                   use_x         : boolean;
                   use_y         : boolean;
                   recode_x      : Bvector;
                   recode_y      : Bvector;
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
              var  pseudo_gamma_ready : boolean;
              var  pseudo_gamma    : real;
              var  pseudo_gamma_p  : real;
              var  xorder          : bvector;
              var  yorder          : bvector;
                   printoutput     : boolean);

PROCEDURE PairWisePseudogamma(var outfile : textfile;
                   compare_x     : boolean;
                   x_restricted   : boolean;
                   x_restrictions : bmatrix;
                   compare_y     : boolean;
                   y_restricted   : boolean;
                   y_restrictions : bmatrix;
                   use_x         : boolean;
                   use_y         : boolean;
                   recode_x      : Bvector;
                   recode_y      : Bvector;
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
              var  pseudo_gamma_ready : boolean;
              var  pseudo_gamma: real;
              var  pseudo_gamma_p  : real;
              var xorder           : bvector;
              var yorder           : bvector;
                  printoutput    : boolean);

PROCEDURE Pseudogamma_XYZ_TEST(var outfile : textfile;
                   AnalysisType  : byte;   // 1 = pseudo   2 = pairwise
                   permute_x     : boolean;
                   x_restricted   : boolean;
                   x_restrictions : bmatrix;
                   permute_y     : boolean;
                   y_restricted   : boolean;
                   y_restrictions : bmatrix;
                   use_x         : boolean;
                   use_y         : boolean;
                   recode_x      : Bvector;
                   recode_y      : Bvector;
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
              VAR  RESULTS       : RESARRAY;
              var  permuted_gamma_ready : boolean;
              var  permuted_gamma   : real;
              var  permuted_gamma_p : real;
              var  xorder           : bvector;
              var  yorder           : bvector;
              var  lr2              : real;
              var  df2              : integer;
              var  lr2_p            : real;
                  printoutput    : boolean);

// a copy of cheap_xyz_test

FUNCTION XYZ_GAMMA(XDIM,YDIM     : BYTE;
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
                   HYPNR         : INTEGER;
                   HYPOTESE      : HYPARRAY): REAL;

(* CALCULATION OF PARTIAL GAMMA COEFFICIENTS FROM XYZ-TABLES *)

FUNCTION XYZ_GAMMA2(GammaType    : byte;
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
                   HYPNR         : INTEGER;
                   HYPOTESE      : HYPARRAY): REAL;

(* CALCULATION OF PARTIAL GAMMA COEFFICIENTS FROM XYZ-TABLES *)
FUNCTION EXYZ_GAMMA(XDIM,YDIM     : BYTE;
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
                   HYPNR         : INTEGER;
                   HYPOTESE      : HYPARRAY): REAL;

(* CALCULATION OF PARTIAL GAMMA COEFFICIENTS FROM AN EXYZ-TABLE *)

FUNCTION XYZ_RecodedGAMMA(var outfile : textfile;
                   xperm         : bvector;
                   yperm         : bvector;
                   XDIM,YDIM     : BYTE;
                   TDIMX,TDIMY   : BYTE;
                   XY_SIZE       : BYTE;
                   NZ            : LONGINT;
                   XYZ_TABLE     : LONGINT_DATA_ARRAY;
                   XYZ_ARRAYS    : BYTE;
                   EXYZ_TABLE    : REAL_DATA_ARRAY;
                   EXYZ_ARRAYS   : BYTE): Real;

(* CALCULATION OF PARTIAL GAMMA COEFFICIENTS FROM XYZ-TABLES AFTER RECODING *)
(* P-values are not calculated by this procedure                            *)
(* rows and cols where xperm(.x.)=0 and yperm(.y.)=0 is disregarded         *)

PROCEDURE EXPENSIVE_XYZ_TEST(var outfile : textfile;
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
                   ZCELL         : BYTE_DATA_ARRAY;
                   ZCELL_ARRAYS  : BYTE;
                   EXACT         : BOOLEAN;
                   NUMBER_OF_SIM : INTEGER;
                   SIMSTART      : INTEGER;
                   SEQUENTIAL    : BOOLEAN;
                   SEQ_P0        : REAL;
                   SEQ_ALPHA     : REAL;
                   SEQ_B         : REAL;
                   NVAR          : BYTE;
                   DIM           : BVECTOR;
                   VNAMES        : VNAME_PTR;
                   VNAMES_EXIST  : BOOL_VECTOR;
                   HYPNR         : INTEGER;
                   HYPOTESE      : HYPARRAY;
              VAR  EFFECTIVE_N   : LONGINT;
              VAR  EFFECTIVE_SIM : INTEGER;
              VAR  RESULTS       : RESARRAY);

(* THIS PROCEDURE PRODUCES LOCAL BIGTEST RESULTS *)


Procedure Print_XYZ_table(var outfile : textfile;
                              tekst   : string;
                              TDIMX,TDIMY : Byte;
                              NZ      : integer;
                              XY_size : byte;
                             XYZ_TABLE : LONGINT_DATA_ARRAY;
                             XYZ_ARRAYS: BYTE;
                            EXYZ_TABLE : REAL_DATA_ARRAY;
                            EXYZ_ARRAYS: BYTE);

Procedure Print_recoded_XYZ_table(var outfile : textfile;
                                  tekst   : string;
                                  Xlabel,Ylabel : Char;
                                  TDIMX,TDIMY : Byte;
                                  NXgroups,NYgroups : byte;
                                  Xcodes,Ycodes : bvector;
                                  NZ      : integer;
                                  XY_size : byte;
                                  XYZ_TABLE : LONGINT_DATA_ARRAY;
                                  XYZ_ARRAYS: BYTE;
                                  EXYZ_TABLE : REAL_DATA_ARRAY;
                                  EXYZ_ARRAYS: BYTE);

Procedure Print_EXYZ_table(var outfile : textfile;
                              tekst   : string;
                              TDIMX,TDIMY : Byte;
                              NZ      : integer;
                              XY_size : byte;
                             XYZ_TABLE : LONGINT_DATA_ARRAY;
                             XYZ_ARRAYS: BYTE;
                            EXYZ_TABLE : REAL_DATA_ARRAY;
                            EXYZ_ARRAYS: BYTE);

PROCEDURE XYZ_Gamma2_TEST(var outfile : textfile;
                   gammatype     : byte;
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
                   hypnr         : INTEGER;
                   HYPOTESE      : HYPARRAY;
              VAR  GAMMA2        : REAL;
              VAR  PGAMMA2       : REAL;
              var  PAbsGamma2    : real;
              var  effective_n   : Longint;
              var  effective_sim : integer);


IMPLEMENTATION

uses DIGRAM1f,SKbigtab,SKXYZ0,skxyz2;

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
                        var R                 : Integer);
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

//------------------------------------------------------------------

Procedure TwoWay_tests(c,r   : Byte;
                       ordinal : boolean;
                       Tab2  : TwowaytableArray;
                       RTab2 : ETwowaytableArray;
                       var chi,pchi,chitot,Gamma,Pgamma,ppq,pmq,s,ppqtot,pmqtot,stot : real;
                       var df,dftot,effective_n  : integer);
begin
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

//---------------------------------------------------------------------

Procedure start_exact_tests(Hypnr,Number_of_sim           : integer;
                            Var Results                   : ResArray;
                            var Sim,Nchi,Ngamma,NAbsGamma : Integer);
begin
    RESULTS(.HYPNR,4.):=0;
    RESULTS(.HYPNR,7.):=0;

    SIM:=0; NCHI:=0; NGAMMA:=0; NabsGamma:=0;

    If sequential and (seq_b<1000) then
    InitProgressBox(0,number_of_sim,1,'Exact test','Repeated MC test')
    else
    If sequential and (seq_limit<number_of_sim) then
    InitProgressBox(0,number_of_sim,1,'Exact test','Sequential MC test')
    else
    InitProgressBox(0,number_of_sim,1,'Exact test','Monte Carlo test');
end;

//---------------------------------------------------------------------

Procedure Evaluate_simulated_results(Sequential,ordinal : boolean;
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

    IF SEQUENTIAL THEN
    BEGIN
        IF SEQ_T(NCHI,SIM,SEQ_P0)>=SEQ_B THEN CHI_SPRT_STATUS:=1 else
        if nchi>=seq_limit then CHI_SPRT_STATUS:=1;

    END;

    IF ORDINAL THEN
    BEGIN
        IF PPQ>0 THEN GAMMA:=PMQ/PPQ ELSE GAMMA:=0;
        IF ((GAMMATOT>0) AND (GAMMA>=GAMMATOT))
        OR ((GAMMATOT<0) AND (GAMMA<=GAMMATOT))
        OR (GAMMATOT=0) THEN INC(NGAMMA);

        if abs(gamma)>=absgammatot then inc(NAbsGamma);

        if two_sided then ng:=nabsgamma else ng:=ngamma;

        IF SEQUENTIAL then
        BEGIN
            //IF SEQ_T(NGAMMA,SIM,SEQ_P0)>=SEQ_B
            //IF SEQ_T(NG,SIM,SEQ_P0)>=SEQ_B THEN GAMMA_SPRT_STATUS:=1;
            IF SEQ_T(NG,SIM,SEQ_P0)>=SEQ_B THEN GAMMA_SPRT_STATUS:=1 else
            if ng>=seq_limit then gamma_sprt_status:=1;
        END;
    END;

end;

//------------------------------------------------------------------

Procedure Evaluate_simulated_Gamma(Sequential                            : boolean;
                                   pmq,ppq,Gammatot,AbsGammatot          : real;
                                   Sim                                   : integer;
                                   Seq_p0,Seq_b                          : real;
                                   var gamma                             : real;
                                   var Ngamma,NAbsGamma                  : Integer;
                                   var Chi_sprt_status,Gamma_sprt_status : Integer);
var ng : integer;
begin
    chi_sprt_status:=1;
    gamma_sprt_status:=0;

    IF PPQ>0 THEN GAMMA:=PMQ/PPQ ELSE GAMMA:=0;
    IF ((GAMMATOT>0) AND (GAMMA>=GAMMATOT))
    OR ((GAMMATOT<0) AND (GAMMA<=GAMMATOT))
    OR (GAMMATOT=0) THEN INC(NGAMMA);

    if abs(gamma)>=absgammatot then inc(NAbsGamma);

    if two_sided then ng:=nabsgamma else ng:=ngamma;

    IF SEQUENTIAL then
    BEGIN
        IF SEQ_T(NG,SIM,SEQ_P0)>=SEQ_B THEN GAMMA_SPRT_STATUS:=1 else
        if ng>=seq_limit then gamma_sprt_status:=1;
    END;
end;

//------------------------------------------------------------------

procedure NoResults(hypnr: integer; var Results : ResArray);
label slut;
begin
    IF HYPNR>MAXHYP THEN goto slut;
    (***)
    RESULTS(.HYPNR,1.):=0;
    RESULTS(.HYPNR,2.):=0;
    RESULTS(.HYPNR,3.):=2;
    RESULTS(.HYPNR,4.):=2;
    RESULTS(.HYPNR,5.):=0;
    RESULTS(.HYPNR,6.):=2;
    RESULTS(.HYPNR,7.):=2;
    RESULTS(.HYPNR,8.):=0;
    (***)
slut:
end;

//----------------------------------------------------------

FUNCTION XYZ_CELL_number;
BEGIN
    XYZ_CELL_number:=(Z-1)*XY_SIZE+(Y-1)*TDIMX+X;
END; (* of XYZ_CELL_number *)

//-----------------------------------------------------------

FUNCTION TABCOUNT_VALUE;
BEGIN
    LOCATE_TABLE_POINT(CASENO,ARRAY_NO,POINT);
    IF ARRAY_NO<=TABCOUNT_ARRAYS
    THEN
    TABCOUNT_VALUE:=TABCOUNT(.ARRAY_NO.)^(.POINT.)
    ELSE
    TABCOUNT_VALUE:=0;
END;

(*----------------------------------*)

PROCEDURE ADD_TABCOUNT_TO_XYZ;
VAR N,COUNT : LONGINT;
BEGIN

    COUNT:=TABCOUNT_VALUE(CASENO,TABCOUNT,TABCOUNT_ARRAYS,ARRAY_NO,POINT);
    N:=XYZ_CELL_number(X,Y,Z,TDIMX,XY_SIZE);
    IF (N>0) AND (N<=ZMAX) THEN
    BEGIN
        LOCATE_TABLE_POINT(N,ARRAY_NO,POINT);
        XYZ_TABLE(.ARRAY_NO.)^(.POINT.):=
        XYZ_TABLE(.ARRAY_NO.)^(.POINT.)+COUNT;
    END;
END;

(*-------------------------------------------------------*)

FUNCTION XYZ_LENGTH;
VAR N : BYTE;
BEGIN
    N:=(DIMX+1)*(DIMY+1);
    XYZ_LENGTH:=(MAX_NUMBER_OF_REALS*MAX_TAB_ARRAYS) DIV N;
END; (* of XYZ_LENGTH *)

(*-------------------------------------------------------*)

FUNCTION XYZCELL;
VAR N: LONGINT;
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
VAR N: LONGINT;
BEGIN
    N:=(Z-1)*XY_SIZE+(Y-1)*TDIMX+X;

    ARRAY_NO := N DIV MAX_NUMBER_OF_REALS;
    POINT    := N MOD MAX_NUMBER_OF_REALS;

    IF POINT=0 THEN POINT:=MAX_NUMBER_OF_REALS
               ELSE INC(ARRAY_NO);
    EXYZCELL:=EXYZ_TABLE(.ARRAY_NO.)^(.POINT.);
END; (* of EXYZCELL *)

(*-------------------------------------------------------*)

Procedure Set_XYZCELL(X,Y : BYTE; Z: LONGINT;
                 value      : Longint;
            var  XYZ_TABLE  : LONGINT_DATA_ARRAY;
                 XYZ_ARRAYS : BYTE;
                 TDIMX      : BYTE;
                 XY_SIZE    : BYTE;
             VAR ARRAY_NO   : BYTE;
             VAR POINT      : LONGINT);
VAR N: LONGINT;
begin
    N:=(Z-1)*XY_SIZE+(Y-1)*TDIMX+X;

    ARRAY_NO := N DIV MAX_NUMBER_OF_LONGINT;
    POINT    := N MOD MAX_NUMBER_OF_LONGINT;

    IF POINT=0 THEN POINT:=MAX_NUMBER_OF_LONGINT
               ELSE INC(ARRAY_NO);

    XYZ_TABLE(.ARRAY_NO.)^(.POINT.):=value;

end;

//--------------------------------------------------------

Procedure Set_EXYZCELL(X,Y : BYTE; Z: LONGINT;
                  value      : real;
              var EXYZ_TABLE : REAL_DATA_ARRAY;
                  EXYZ_ARRAYS: BYTE;
                  TDIMX      : BYTE;
                  XY_SIZE    : BYTE;
              VAR ARRAY_NO   : BYTE;
              VAR POINT      : LONGINT);
VAR N: LONGINT;
begin
    N:=(Z-1)*XY_SIZE+(Y-1)*TDIMX+X;

    ARRAY_NO := N DIV MAX_NUMBER_OF_REALS;
    POINT    := N MOD MAX_NUMBER_OF_REALS;

    IF POINT=0 THEN POINT:=MAX_NUMBER_OF_REALS
               ELSE INC(ARRAY_NO);
    EXYZ_TABLE(.ARRAY_NO.)^(.POINT.):=value;
end;

//---------------------------------------------------------

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

(*------------------------------------------*)

FUNCTION XYZ_FREQ;
VAR ARRAY_NO : BYTE;
    POINT    : LONGINT;
BEGIN
    XYZ_FREQ:=XYZCELL(X,Y,Z,XYZ_TABLE,XYZ_ARRAYS,TDIMX,XY_SIZE,ARRAY_NO,POINT);
END;


(*-----------------------------------------------------------*)

FUNCTION XYZ_EXPECTED;
VAR ARRAY_NO : BYTE;
    POINT    : LONGINT;
BEGIN
    XYZ_EXPECTED:=EXYZCELL(X,Y,Z,EXYZ_TABLE,EXYZ_ARRAYS,TDIMX,XY_SIZE,ARRAY_NO,POINT);
END;

(*-----------------------------------------------------------*)

PROCEDURE TRANSFER_XYZ_SLICE;

VAR  POINT        : LONGINT;
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
VAR  POINT        : LONGINT;
     ARRAY_NO,X,Y : BYTE;
BEGIN

    FOR Y:=1 TO TDIMY DO
    FOR X:=1 TO TDIMX DO
    EXY_TABLE(.X,Y.):=EXYZCELL(X,Y,Z,EXYZ_TABLE,EXYZ_ARRAYS,
                               TDIMX,XY_SIZE,ARRAY_NO,POINT);
END;

(*---------------------------------------------------*)

PROCEDURE STORE_XYZ_SLICE;
VAR  N,POINT      : LONGINT;
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

(*------------------------------------------------------*)
PROCEDURE STORE_EXYZ_SLICE;
VAR  N,POINT      : LONGINT;
     ARRAY_NO,X,Y : BYTE;
     A            : REAL;
BEGIN
    // N:=(Z-1)*XY_SIZE; why was this included???

    FOR Y:=1 TO TDIMY DO
    FOR X:=1 TO TDIMX DO
    BEGIN
        // Note N and A is determined here in order to get array numbers and points
        // This can probably be optimized in some way
        A:=EXYZCELL(X,Y,Z,EXYZ_TABLE,EXYZ_ARRAYS,
                               TDIMX,XY_SIZE,ARRAY_NO,POINT);
        EXYZ_TABLE(.ARRAY_NO.)^(.POINT.):=EXY_TABLE(.X,Y.);
    END;
END;

(*------------------------------------------------------*)

PROCEDURE MAKE_XYZ_TABLE;
LABEL  NEXTSLICE,SLUT;
VAR I,J,K,I1,I2,I3,I4,I5,I6,I7,I8,ST,R,C,
    v1,v2,v3,v4,v5,v6,v7,v8   : BYTE;
    N                          : ARRAY(.1..3.) OF BYTE;
    BOT,TOP                    : BVECTOR;
    V,W                        : Bvector; //ARRAY(.1..MAXVAR.) OF BYTE;
    DF                         : INTEGER;
    Z,GL,POINT,M               : LONGINT;
    X,Y,ARRAY_NO               : BYTE;
    A,F1,F2,F3                 : REAL;
    TAB2                       : TWOWAYTABLEarray;
    RTAB2                      : ETWOWAYTABLEarray;

BEGIN
    FILLCHAR(TAB2,SIZEOF(TAB2),0);
    FILLCHAR(RTAB2,SIZEOF(RTAB2),0);

    SUCCESS:=TRUE;
    NZ:=0;

    NVAR1:=0;
    FOR I:=1 TO NVAR DO IF HYPOTESE(.I.)<>3 THEN INC(NVAR1);
    N(.1.):=0; N(.2.):=2; N(.3.):=NVAR1;

    // Shuffle variables for the XYZ table.
    // The two variables with Status 1 (variables of interest) comes first
    // The control variables (Status 2) comes second
    // The collapsed variables (Status 3) comes last

    FOR I:=1 TO NVAR DO
    BEGIN
        ST:=HYPOTESE(.I.);   (* ST= STATUS OF VARIABLE I *)
        Inc(N(.ST.));
        K:=N(.ST.);          (** THE POSITION OF THE VARIABLE IN THE NEW TABLE *)

        OLDVAR(.K.):=I;

        IF ST=3 THEN
        BEGIN
             DIM1(.K.):=1;
             TDIM1(.K.):=1;
             BOT(.I.):=TDIM(.I.);
             TOP(.I.):=TDIM(.I.);
        END ELSE
        IF ST=2 THEN
        BEGIN
            VLABEL1(.K.):=VLABEL(.I.);
            DIM1(.K.):=DIM(.I.);
            TDIM1(.K.):=TDIM(.I.);
            BOT(.I.):=1;
            TOP(.I.):=DIM(.I.);
            VTYPE1(.K.):=VTYPE(.I.);
        END ELSE
        BEGIN
            VLABEL1(.K.):=VLABEL(.I.);
            DIM1(.K.):=DIM(.I.);
            TDIM1(.K.):=TDIM(.I.);
            BOT(.I.):=TDIM(.I.);
            TOP(.I.):=TDIM(.I.);
            VTYPE1(.K.):=VTYPE(.I.);
        END;
    END;


    FOR I:=NVAR+1 TO MAXVAR DO
    BEGIN
        DIM1(.I.):=1; TDIM1(.I.):=1;
        BOT(.I.):=TDIM(.I.); TOP(.I.):=TDIM(.I.);
        OLDVAR(.I.):=I; (** THE POSITION IS UNCHANGED **)
    END;

    DIMX:=DIM1(.1.);
    DIMY:=DIM1(.2.);

    INIT_XYZ(ZMAX,NZ,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,
             XYZ_TABLE,XYZ_ARRAYS,
             EXYZ_TABLE,EXYZ_ARRAYS);

    FOR V8:=BOT(.8.) TO TOP(.8.) DO
    FOR V7:=BOT(.7.) TO TOP(.7.) DO
    FOR V6:=BOT(.6.) TO TOP(.6.) DO
    FOR V5:=BOT(.5.) TO TOP(.5.) DO
    FOR V4:=BOT(.4.) TO TOP(.4.) DO
    FOR V3:=BOT(.3.) TO TOP(.3.) DO
    FOR V2:=BOT(.2.) TO TOP(.2.) DO
    FOR V1:=BOT(.1.) TO TOP(.1.) DO
    BEGIN
        (* CHECK WHETHER THIS SLICE CONTAINS ANYTHING *)
        R:=0; C:=0;
        M:=TABCELL(V1,V2,V3,V4,V5,V6,V7,V8,
                    CFACTOR,CFACTORSUM,
                    TABLE,TABLE_ARRAYS,ARRAY_NO,POINT);
        IF M>0 THEN
        BEGIN
            putintoV(v1,v2,v3,v4,v5,v6,v7,v8,V);
            W:=V;
            FOR I1:=1 TO DIM1(.1.) DO
            BEGIN
                W(.OLDVAR(.1.).):=I1;
                GL:=TABCELL(W(.1.),W(.2.),W(.3.),W(.4.),
                            W(.5.),W(.6.),W(.7.),W(.8.),
                            CFACTOR,CFACTORSUM,
                            TABLE,TABLE_ARRAYS,ARRAY_NO,POINT);
                IF GL>0 THEN INC(C); (* A COLUMN WITH SOMETHING *)
            END;
            W:=V;
            IF C>1 THEN
            FOR I2:=1 TO DIM1(.2.) DO
            BEGIN
                W(.OLDVAR(.2.).):=I2;
                GL:=TABCELL(W(.1.),W(.2.),W(.3.),W(.4.),
                            W(.5.),W(.6.),W(.7.),W(.8.),
                            CFACTOR,CFACTORSUM,
                            TABLE,TABLE_ARRAYS,ARRAY_NO,POINT);
                IF GL>0 THEN INC(R); (* A COLUMN WITH SOMETHING *)
            END;
        END;
        IF (C<2) OR (R<2) THEN GOTO NEXTSLICE;

        INC(NZ);

        IF NZ>ZMAX THEN
        BEGIN
            okMessage('XYZ1: Failure creating 3-way table - too many slices ');
            SUCCESS:=FALSE;
            NZ:=ZMAX;
            GOTO SLUT;
        END;

        FOR y:=1 TO TDIMY DO
        FOR x:=1 TO TDIMX DO
        BEGIN
            W(.OLDVAR(.1.).):=X;
            W(.OLDVAR(.2.).):=Y;
            GL:=TABCELL(W(.1.),W(.2.),W(.3.),W(.4.),
                        W(.5.),W(.6.),W(.7.),W(.8.),
                        CFACTOR,CFACTORSUM,
                        TABLE,TABLE_ARRAYS,ARRAY_NO,POINT);
            TAB2(.x,y.):=GL;
        END;

        IF TAB2(.TDIMX,TDIMY.)>0 THEN
        BEGIN
            FOR X:=1 TO TDIMX DO
            FOR Y:=1 TO TDIMY DO
            BEGIN
                //F1:=TAB2(.X,TDIMY.);
                //F2:=TAB2(.TDIMX,Y.);
                //F3:=TAB2(.TDIMX,TDIMY.);
                //RTAB2(.X,Y.):=F1*(F2/F3);
                RTAB2(.X,Y.):=TAB2(.X,TDIMY.)*(TAB2(.TDIMX,Y.)/TAB2(.TDIMX,TDIMY.));
            END;
            STORE_XYZ_SLICE(NZ,XYZ_TABLE,XYZ_ARRAYS,
                        EXYZ_TABLE,EXYZ_ARRAYS,
                        TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);
        END
        ELSE
        BEGIN
            FOR X:=1 TO TDIMX DO
            FOR Y:=1 TO TDIMY DO
            BEGIN
                RTAB2(.X,Y.):=0;
            END;
            okMessage('XYZ1: Warning - an empty slice has been found ');
            NZ:=NZ-1;
        END;
        //STORE_XYZ_SLICE(NZ,XYZ_TABLE,XYZ_ARRAYS,
        //                EXYZ_TABLE,EXYZ_ARRAYS,
        //                TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);
NEXTSLICE:
    END;
SLUT:

END; (* of  MAKE_XYZ_TABLE *)

(*------------------------------------------------------*)

PROCEDURE MAKE_XYZ_BTABLE;
LABEL  NEXTSLICE,igen,SLUT;
VAR I,J,K,I1,I2,I3,I4,I5,I6,I7,I8,ST,R,C,
    v1,v2,v3,v4,v5,v6,v7,v8   : BYTE;
    N                          : ARRAY(.1..3.) OF BYTE;
    BOT,TOP                    : BVECTOR;
    V,W,Vfra,Vtil              : Bvector;
    DF                         : INTEGER;
    Z,GL,POINT,M               : LONGINT;
    X,Y,ARRAY_NO               : BYTE;
    A,F1,F2,F3                 : REAL;
    TAB2                       : TWOWAYTABLEarray;
    RTAB2                      : ETWOWAYTABLEarray;

      Function ContinueWithV: Boolean;
      label slut;
      var i: byte;
      Begin
          ContinueWithV:=false;
          for i:=1 to nvar1 do
          begin
              if v(.i.)<Vtil(.i.) then
              begin
                  inc(v(.i.));
                  ContinueWithV:=true;
                  goto slut;
              end
              else v(.i.):=vfra(.i.);
          end;
      slut:
      end;
BEGIN
    FILLCHAR(TAB2,SIZEOF(TAB2),0);
    FILLCHAR(RTAB2,SIZEOF(RTAB2),0);

    SUCCESS:=TRUE;
    NZ:=0;

    NVAR1:=0;
    FOR I:=1 TO NVAR DO IF HYPOTESE(.I.)<>3 THEN INC(NVAR1);
    N(.1.):=0; N(.2.):=2; N(.3.):=NVAR1;

    // Shuffle variables for the XYZ table.
    // The two variables with Status 1 (variables of interest) comes first
    // The control variables (Status 2) comes second
    // The collapsed variables (Status 3) comes last

    FOR I:=1 TO NVAR DO
    BEGIN
        ST:=HYPOTESE(.I.);   (* ST= STATUS OF VARIABLE I *)
        Inc(N(.ST.));
        K:=N(.ST.);          (** THE POSITION OF THE VARIABLE IN THE NEW TABLE *)

        OLDVAR(.K.):=I;

        IF ST=3 THEN
        BEGIN
             DIM1(.K.):=1;
             TDIM1(.K.):=1;
             BOT(.I.):=TDIM(.I.);
             TOP(.I.):=TDIM(.I.);
        END ELSE
        IF ST=2 THEN
        BEGIN
            VLABEL1(.K.):=VLABEL(.I.);
            DIM1(.K.):=DIM(.I.);
            TDIM1(.K.):=TDIM(.I.);
            BOT(.I.):=1;
            TOP(.I.):=DIM(.I.);
            VTYPE1(.K.):=VTYPE(.I.);
        END ELSE
        BEGIN
            VLABEL1(.K.):=VLABEL(.I.);
            DIM1(.K.):=DIM(.I.);
            TDIM1(.K.):=TDIM(.I.);
            BOT(.I.):=TDIM(.I.);
            TOP(.I.):=TDIM(.I.);
            VTYPE1(.K.):=VTYPE(.I.);
        END;
    END;

    FOR I:=NVAR+1 TO largedim DO
    BEGIN
        DIM1(.I.):=1; TDIM1(.I.):=1;
        BOT(.I.):=TDIM(.I.); TOP(.I.):=TDIM(.I.);
        OLDVAR(.I.):=I; (** THE POSITION IS UNCHANGED **)
    END;

    DIMX:=DIM1(.1.);
    DIMY:=DIM1(.2.);

    INIT_XYZ(ZMAX,NZ,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,
             XYZ_TABLE,XYZ_ARRAYS,
             EXYZ_TABLE,EXYZ_ARRAYS);


    vfra:=Bot;
    Vtil:=top;
    v:=vfra;

igen:
    (* CHECK WHETHER THIS SLICE CONTAINS ANYTHING *)

    R:=0; C:=0;
    M:=BTCELL(v,nvar,CFACTOR,CFACTORSUM,TABLE,TABLE_ARRAYS,ARRAY_NO,POINT);

    IF M>0 THEN
    BEGIN
        W:=V;
        FOR I1:=1 TO DIM1(.1.) DO
        BEGIN
            W(.OLDVAR(.1.).):=I1;
            GL:=BTCELL(W,nvar,CFACTOR,CFACTORSUM,TABLE,TABLE_ARRAYS,ARRAY_NO,POINT);
            IF GL>0 THEN INC(C); (* A COLUMN WITH SOMETHING *)
        END;

        W:=V;
        IF C>1 THEN
        FOR I2:=1 TO DIM1(.2.) DO
        BEGIN
            W(.OLDVAR(.2.).):=I2;
            GL:=BTCELL(W,nvar,CFACTOR,CFACTORSUM,TABLE,TABLE_ARRAYS,ARRAY_NO,POINT);
            IF GL>0 THEN INC(R); (* A COLUMN WITH SOMETHING *)
        END;
    END;

    IF (C<2) OR (R<2) THEN GOTO NEXTSLICE;

    INC(NZ);

    IF NZ>ZMAX THEN
    BEGIN
        okMessage('XYZ1: Failure creating 3-way table - too many slices ');
        SUCCESS:=FALSE;
        NZ:=ZMAX;
        GOTO SLUT;
    END;

    FOR y:=1 TO TDIMY DO
    FOR x:=1 TO TDIMX DO
    BEGIN
        W(.OLDVAR(.1.).):=X;
        W(.OLDVAR(.2.).):=Y;
        GL:=BTCELL(W,Nvar,CFACTOR,CFACTORSUM,TABLE,TABLE_ARRAYS,ARRAY_NO,POINT);
        TAB2(.x,y.):=GL;
    END;

    IF TAB2(.TDIMX,TDIMY.)>0 THEN
    BEGIN
        FOR X:=1 TO TDIMX DO
        FOR Y:=1 TO TDIMY DO
        BEGIN
            RTAB2(.X,Y.):=TAB2(.X,TDIMY.)*(TAB2(.TDIMX,Y.)/TAB2(.TDIMX,TDIMY.));
        END;
        STORE_XYZ_SLICE(NZ,XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS,
                    TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);
    END
    ELSE
    BEGIN
        FOR X:=1 TO TDIMX DO
        FOR Y:=1 TO TDIMY DO
        BEGIN
            RTAB2(.X,Y.):=0;
        END;
        writeln(outfile,'XYZ1: Warning - an empty slice has been found ');
        NZ:=NZ-1;
    END;


NEXTSLICE:
    // END of a slice
    If ContinueWithV then goto igen;

SLUT:
END; (* of  MAKE_XYZ_BTABLE *)

(*------------------------------------------------------*)

PROCEDURE XYZ_TEST;
LABEL    HYPOTESESLUT,SIM_START,SIM_FINISH,SIM_FINISH1;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)

      X,Y,FRA,TIL,THELEVEL,
      X_COORDINATE,Y_COORDINATE : BYTE;
      SUCCESS,ORDINAL           : BOOLEAN;
      HYP,V                     : BVECTOR;
      TAB2                      : TWOWAYTABLEarray;
      RTAB2                     : ETWOWAYTABLEarray;
      Z                         : LONGINT;
      DUMMYCHAR                 : CHAR;

      I,J,K,N,M,C,R,V1,V2,
      SIM,LOCAL_NSIM,NSIM,NCHI,NGAMMA,NAbsGamma,
      CHI_SPRT_STATUS,GAMMA_SPRT_STATUS,
      DF,DFTOT,ACCEPTNR,NSIM1,NSIM2,SIM1,SIM2        : INTEGER;

      CHITOT,GAMMATOT,AbsGammaTot,
      GMTOT,STOT,PPQTOT,PMQTOT,
      PCHI,CHI,CHI1,PPQ,PPQ1,PMQ,PMQ1,GAMMA,GAMMA1,
      PGAMMA,S,PCHITOT,U,PGAMMATOT                   : REAL;


      PROCEDURE UDTAB(VAR UDFIL: TEXT);
      VAR X,Y : BYTE;
      BEGIN
          WRITELN(UDFIL,'Z =',Z);
          WRITE(UDFIL,'Y X ');
          FOR X:=1 TO XDIM DO WRITE(UDFIL,X:3,'  ');
          WRITELN(UDFIL,'TOTAL');
          FOR Y:=1 TO YDIM+1 DO
          BEGIN
              IF Y<YDIM+1 THEN WRITE(UDFIL,Y,'   ')
                          ELSE WRITE(UDFIL,'TOT ');
              FOR X:=1 TO XDIM DO WRITE(UDFIL,TAB2(.X,Y.):3,'  ');
              WRITELN(UDFIL,TAB2(.TDIMX,Y.):3);
              WRITE(UDFIL,'    ');
              FOR X:=1 TO XDIM DO WRITE(UDFIL,RTAB2(.X,Y.):5:1);
              WRITELN(UDFIL,RTAB2(.TDIMX,Y.):5:1);
              WRITELN(UDFIL);
          END;
      END;

BEGIN

     Init_XYZ_test(outfile,Tab2,Rtab2,Effective_N,Effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R);

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         TwoWay_tests(c,r,ordinal,tab2,rtab2,chi,pchi,chitot,gamma,pgamma,
                      ppq,pmq,s,ppqtot,pmqtot,stot,df,dftot,effective_n);
     END; (*** OF A 2-WAY TABLE ***)

     PCHITOT:=PFCHI(DFTOT,CHITOT);

     IF ORDINAL THEN Calculate_gamma_statistics(PMQtot,PPQtot,GammaTot,AbsGammatot,Stot,U,Pgammatot);

     IF EFFECTIVE_N=0 THEN
     BEGIN
         WRITE_HYPOTESE(Outfile,NVAR,HYPNR,HYPOTESE,VLABEL,FALSE);
         WRITELN(outfile,'   Empty table');
         IF HYPNR<=MAXHYP THEN
         NoResults(hypnr,Results);
         GOTO HYPOTESESLUT;
     END
     ELSE
     IF DFTOT=0 THEN
     BEGIN
         WRITE_HYPOTESE(Outfile,NVAR,HYPNR,HYPOTESE,VLABEL,FALSE);
         WRITELN(outfile,'   No degrees of freedom');
         IF hYPNR<=MAXHYP THEN
         NoResults(hypnr,Results);
         GOTO HYPOTESESLUT;
     END;

     Transfer_results(Hypnr,ChiTot,DfTot,PchiTot,GammaTot,AbsGammaTot,PGammaTot,Results);

     (*** CALCULATION OF EXACT P-VALUES ***)

     IF EXACT AND (NUMBER_OF_SIM>0) THEN
     BEGIN
         Start_exact_tests(hypnr,number_of_sim,results,sim,Nchi,Ngamma,NabsGamma);
SIM_START:
        Inc(SIM);
        IF SIM>NUMBER_OF_SIM THEN
        BEGIN
            SIM:=SIM-1;
            GOTO SIM_FINISH;
        END;

        CHI:=0;
        PPQ:=0;
        PMQ:=0;

        FOR Z:=1 TO NZ DO
        BEGIN
            TRANSFER_EXYZ_SLICE(Z,EXYZ_TABLE,EXYZ_ARRAYS,
                                TDIMX,TDIMY,XY_SIZE,RTAB2);

            GENTAB(Outfile,XDIM,YDIM,RTAB2,ORDINAL,CHI1,PPQ1,PMQ1);
            CHI:=CHI+CHI1;
            IF ORDINAL THEN
            BEGIN
                PPQ:=PPQ+PPQ1;
                PMQ:=PMQ+PMQ1;
            END;
        END;

        (************************************************************
         *** TESTSTATISTICS HAVE BEEN GENERATED FOR ALL SEPARATE  ***
         *** TABLES. WHAT REMAINS IS JUST TO  CALCULATE THE TOTAL ***
         *** STATISTICS AND TO COMPARE THE GENERATED STATISTICS   ***
         *** WITH THE OBSERVED                                    ***
         ************************************************************)

        Evaluate_simulated_results(Sequential,Ordinal,chi,chitot,
                                   pmq,ppq,Gammatot,AbsGammatot,sim,Seq_p0,Seq_b,
                                   Gamma,Nchi,Ngamma,NAbsGamma,Chi_sprt_status,Gamma_sprt_status);

        if CancelAfterProgressBoxStep then goto sim_finish;


        if stop_exact_tests(sequential,chi_sprt_status,gamma_sprt_status,sim,nsim)
        then goto sim_finish1;

        goto sim_start;

SIM_FINISH:
        NSIM:=SIM;

SIM_FINISH1:
        HideProgressBox;

        RESULTS(.HYPNR,4.):=NCHI/NSIM;

        IF ORDINAL THEN
        BEGIN
            RESULTS(.HYPNR,7.):=NGAMMA/NSIM;
            Results(.hypnr,9.):=NabsGamma/Nsim;
        END;

        EFFECTIVE_SIM:=SIM;
        RESULTS(.HYPNR,8.):=SIM;
     END;  (** OF EXACT-PVALUES **)

HYPOTESESLUT:

END;  (*** OF XYZ_TEST ***)

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

BEGIN

     Init_XYZ_test(outfile,Tab2,Rtab2,Effective_N,Effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R);

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);
         TwoWay_tests(c,r,ordinal,tab2,rtab2,chi,pchi,chitot,gamma,pgamma,
                      ppq,pmq,s,ppqtot,pmqtot,stot,df,dftot,effective_n);

     END; (*** OF A 2-WAY TABLE ***)

     PCHITOT:=PFCHI(DFTOT,CHITOT);
     PMIN:=PCHITOT;

     IF ORDINAL THEN Calculate_gamma_statistics(PMQtot,PPQtot,GammaTot,AbsGammatot,Stot,U,Pgammatot);

     if pgammatot<pmin then pmin:=pgammatot;

     Transfer_results(Hypnr,ChiTot,DfTot,PchiTot,GammaTot,AbsGammaTot,PGammaTot,Results);

     (*** CALCULATION OF EXACT P-VALUES ***)

     IF EXACT AND (PMIN<=EXACT_P_LEVEL) THEN
     BEGIN
         Start_exact_tests(hypnr,number_of_sim,results,sim,Nchi,Ngamma,NabsGamma);
SIM_START:
         Inc(SIM);
         IF SIM>NUMBER_OF_SIM THEN
         BEGIN
             SIM:=SIM-1;
             GOTO SIM_FINISH;
         END;

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

         (************************************************************
          *** TESTSTATISTICS HAVE BEEN GENERATED FOR ALL SEPARATE  ***
          *** TABLES. WHAT REMAINS IS JUST TO  CALCULATE THE TOTAL ***
          *** STATISTICS AND TO COMPARE THE GENERATED STATISTICS   ***
          *** WITH THE OBSERVED                                    ***
          ************************************************************)

         Evaluate_simulated_results(Sequential,Ordinal,chi,chitot,
                                    pmq,ppq,Gammatot,AbsGammatot,sim,Seq_p0,Seq_b,
                                    Gamma,Nchi,Ngamma,NAbsGamma,Chi_sprt_status,Gamma_sprt_status);

         if CancelAfterProgressBoxStep then goto sim_finish;

         if stop_exact_tests(sequential,chi_sprt_status,gamma_sprt_status,sim,nsim)
         then goto sim_finish1;
         goto sim_start;

SIM_FINISH:
         NSIM:=SIM;
SIM_FINISH1:
         HideProgressBox;

         RESULTS(.HYPNR,4.):=NCHI/NSIM;

         IF ORDINAL THEN
         begin
             RESULTS(.HYPNR,7.):= NGAMMA/NSIM;
             Results(.hypnr,9.):= NabsGamma/Nsim;
         end;

         EFFECTIVE_SIM      := SIM;
         RESULTS(.HYPNR,8.) := SIM;
     END;  (** OF EXACT-PVALUES **)

HYPOTESESLUT:
END;  (*** OF CHEAP_XYZ_TEST ***)

(*-------------------------------------------------------*)

PROCEDURE Pairwise_XYZ_TEST;
// Pairwise_XYZ_test calculates test statistics in reduced tables with a subset of
// X and/or Y values.

// If permute_x then the test compares x1 & x2
// If permute_y then the test compares y1 & y2

LABEL    HYPOTESESLUT,SIM_START,SIM_FINISH,SIM_FINISH1;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)

      X,Y             : BYTE;
      SUCCESS,ORDINAL : BOOLEAN;
      HYP             : BVECTOR;

      TAB2,newtab2    : TWOWAYTABLEarray;
      RTAB2,newRtab2  : ETWOWAYTABLEarray;

      Z,I,J,K,N,M,C,R,V1,V2,newC,newR : INTEGER;
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

      Procedure ExtractTables(var success: boolean);
      var i : byte;
      begin
         success:=true;
         if permute_x then
         begin
             if permute_y then
             begin
                 NewC:=2;
                 NewR:=2;
                 Newtab2(.1,1.):=tab2(.x1,y1.);
                 Newtab2(.2,1.):=tab2(.x2,y1.);
                 Newtab2(.1,2.):=tab2(.x1,y2.);
                 Newtab2(.2,2.):=tab2(.x2,y2.);
                 Newtab2(.1,3.):=tab2(.x1,r+1.);
                 Newtab2(.2,3.):=tab2(.x2,r+1.);
                 Newtab2(.3,1.):=tab2(.c+1,y1.);
                 Newtab2(.3,2.):=tab2(.c+1,y2.);
                 Newtab2(.3,3.):=tab2(.c+1,r+1.);
                 NewRtab2(.1,1.):=Rtab2(.x1,y1.);
                 NewRtab2(.2,1.):=Rtab2(.x2,y1.);
                 NewRtab2(.1,2.):=Rtab2(.x1,y2.);
                 NewRtab2(.2,2.):=Rtab2(.x2,y2.);
                 NewRtab2(.1,3.):=Rtab2(.x1,r+1.);
                 NewRtab2(.2,3.):=Rtab2(.x2,r+1.);
                 NewRtab2(.3,1.):=Rtab2(.c+1,y1.);
                 NewRtab2(.3,2.):=Rtab2(.c+1,y2.);
                 NewRtab2(.3,3.):=Rtab2(.c+1,r+1.);
             end
             else
             begin
                 NewC:=2;
                 NewR:=R;
                 for i:=1 to r+1 do Newtab2(.1,i.):=tab2(.x1,i.);
                 for i:=1 to r+1 do Newtab2(.2,i.):=tab2(.x2,i.);
                 for i:=1 to r+1 do Newtab2(.3,i.):=tab2(.c+1,i.);
                 for i:=1 to r+1 do NewRtab2(.1,i.):=Rtab2(.x1,i.);
                 for i:=1 to r+1 do NewRtab2(.2,i.):=Rtab2(.x2,i.);
                 for i:=1 to r+1 do NewRtab2(.3,i.):=Rtab2(.c+1,i.);
             end;
         end
         else
         if permute_y then
         begin
             NewC:=C;
             NewR:=2;
             for i:=1 to c+1 do Newtab2(.i,1.):=tab2(.i,y1.);
             for i:=1 to c+1 do Newtab2(.i,2.):=tab2(.i,y2.);
             for i:=1 to c+1 do Newtab2(.i,3.):=tab2(.i,r+1.);
             for i:=1 to c+1 do NewRtab2(.i,1.):=Rtab2(.i,y1.);
             for i:=1 to c+1 do NewRtab2(.i,2.):=Rtab2(.i,y2.);
             for i:=1 to c+1 do NewRtab2(.i,3.):=Rtab2(.i,r+1.);
         end
         else
         begin
             okmessage('pairwise tests abandoned');
             success:=false;
         end;
      end;

BEGIN

     Init_XYZ_test(outfile,Tab2,Rtab2,Effective_N,Effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R);

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         ExtractTables(success);
         if not success then goto hypoteseslut;

         TwoWay_tests(newc,newr,true,newtab2,newrtab2,chi,pchi,chitot,gamma,pgamma,
                      ppq,pmq,s,ppqtot,pmqtot,stot,df,dftot,effective_n);

     END; (*** OF A 2-WAY TABLE ***)

     PCHITOT:=PFCHI(DFTOT,CHITOT);
     PMIN:=PCHITOT;

     Calculate_gamma_statistics(PMQtot,PPQtot,GammaTot,AbsGammatot,Stot,U,Pgammatot);

     //writeln(outfile,'In pairwise_xyz_test: Gammatot = ',gammatot:6:3);

     //if pgammatot<pmin then pmin:=pgammatot;

     Transfer_results(Hypnr,ChiTot,DfTot,PchiTot,GammaTot,AbsGammaTot,PGammaTot,Results);

     (*** CALCULATION OF EXACT P-VALUES ***)

     IF EXACT AND (PMIN<=EXACT_P_LEVEL) THEN
     BEGIN
         Start_exact_tests(hypnr,number_of_sim,results,sim,Nchi,Ngamma,NabsGamma);
SIM_START:
         Inc(SIM);
         IF SIM>NUMBER_OF_SIM THEN
         BEGIN
             SIM:=SIM-1;
             GOTO SIM_FINISH;
         END;

         CHI:=0;
         PPQ:=0;
         PMQ:=0;

         FOR Z:=1 TO NZ DO
         BEGIN
             TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                                EXYZ_TABLE,EXYZ_ARRAYS,
                                TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

             ExtractTables(success);

             GENTAB(outfile,NewC,NewR,NewRTAB2,true,CHI1,PPQ1,PMQ1);

             CHI:=CHI+CHI1;
             PPQ:=PPQ+PPQ1;
             PMQ:=PMQ+PMQ1;
         END;

         (************************************************************
          *** TESTSTATISTICS HAVE BEEN GENERATED FOR ALL SEPARATE  ***
          *** TABLES. WHAT REMAINS IS JUST TO  CALCULATE THE TOTAL ***
          *** STATISTICS AND TO COMPARE THE GENERATED STATISTICS   ***
          *** WITH THE OBSERVED                                    ***
          ************************************************************)
         Evaluate_simulated_results(Sequential,Ordinal,chi,chitot,
                                    pmq,ppq,Gammatot,AbsGammatot,sim,Seq_p0,Seq_b,
                                    Gamma,Nchi,Ngamma,NAbsGamma,Chi_sprt_status,Gamma_sprt_status);

         if CancelAfterProgressBoxStep then goto sim_finish;

         if stop_exact_tests(sequential,chi_sprt_status,gamma_sprt_status,sim,nsim)
         then goto sim_finish1;
         goto sim_start;

SIM_FINISH:
         NSIM:=SIM;
SIM_FINISH1:
         HideProgressBox;

         RESULTS(.HYPNR,4.):=NCHI/NSIM;

         IF ORDINAL THEN
         begin
             RESULTS(.HYPNR,7.):= NGAMMA/NSIM;
             Results(.hypnr,9.):= NabsGamma/Nsim;
         end;

         EFFECTIVE_SIM      := SIM;
         RESULTS(.HYPNR,8.) := SIM;
     END;  (** OF EXACT-PVALUES **)

HYPOTESESLUT:
END;  (*** OF pairwise_XYZ_TEST ***)

(*-------------------------------------------------------*)

PROCEDURE Pseudogamma(var outfile : textfile;
                   permute_x     : boolean;
                   x_restricted   : boolean;
                   x_restrictions : bmatrix;
                   permute_y     : boolean;
                   y_restricted   : boolean;
                   y_restrictions : bmatrix;
                   use_x         : boolean;
                   use_y         : boolean;
                   recode_x      : Bvector;
                   recode_y      : Bvector;
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
              var  pseudo_gamma_ready : boolean;
              var  pseudo_gamma  : real;
              var  pseudo_gamma_p  : real;
              var xorder         : bvector;
              var yorder         : bvector;
                  printoutput    : boolean);

LABEL analyse,HYPOTESESLUT;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)

      X,Y             : BYTE;
      SUCCESS,ORDINAL : BOOLEAN;
      HYP             : BVECTOR;

      TAB2,permtab2   : TWOWAYTABLEarray;
      RTAB2,permRtab2 : ETWOWAYTABLEarray;

      Z,I,J,K,N,M,C,R,V1,V2 : INTEGER;
      V                     : BVECTOR;
      FRA,TIL,THELEVEL      : BYTE;
      SIM,LOCAL_NSIM,NSIM,NCHI,NGAMMA,NAbsGamma,nn,
      CHI_SPRT_STATUS,GAMMA_SPRT_STATUS : INTEGER;
      DUMMYCHAR                         : CHAR;

      CHITOT,GAMMATOT,AbsGammaTot,
      STOT,PPQTOT,PMQTOT,PMIN,
      PCHI,CHI,CHI1,PPQ,PPQ1,PMQ,PMQ1,GAMMA,GAMMA1,
      PGAMMA,S,PCHITOT,U,PGAMMATOT                   : Real;
      DF,DFTOT,ACCEPTNR,NSIM1,NSIM2,SIM1,SIM2        : INTEGER;

      // the following variables are used for permutated tests

      xperm,yperm,sorted_x,sorted_y  : bvector;
      maxgam,signedgam,simgam : real;
      xperms,yperms           : perm_ptr;
      ppqpmq,totppqpmq        : perm_ppqpmq_ptr;
      nxperms,nyperms,nperms,xnr,ynr,nsig : integer;
      gammarecs,gammasequence : Ptr_LongRvector;

      NEWXYZ  : Longint_data_array;
      ENEWXyz : Real_data_array;
      NewArrays,EnewArrays : byte;

      effective_n : longint;
      effective_sim : integer;

      s1,s0 : string;

      // recs(.0.) and recs(.ncases+1.) must be initialized with low and high numbers
      // sequence contains reference to the recnumbers

      procedure WriteOutput(nr : integer);
      var i : integer;
      begin
          if printOutput then
          case nr of
               0 : begin
                       writeln(outfile);
                       writeln(outfile,'Pseudo gamma: no permutations');
                   end;
               1 : begin
                      writeln(outfile);
                      writeln(outfile,nxperms,'  column permutations');
                   end;
               2:  begin
                       writeln(outfile);
                       writeln(outfile,nyperms,' row permutations');
                   end;
               3 : begin
                       writeln(outfile,nperms,' combinations');
                   end;
               10: begin
                       writeln(outfile);
                       write(outfile,'Optimal order:');

                       if permute_x then
                       begin
                           write(outfile,'   rows :');
                           for i:=1 to c do write(outfile,sorted_x(.i.):3);
                       end;

                       if permute_y then
                       begin
                           write(outfile,'   cols :');
                           for i:=1 to r do write(outfile,sorted_y(.i.):3);
                       end;
                       writeln(outfile);
                       writeln(outfile);
                       writeln(outfile);
                       writeln(outfile,'   Gamma = ',SignedGam:7:3);
                       writeln(outfile);
                   end;
          end;
      end;
BEGIN
     new(xperms);
     new(yperms);
     new(ppqpmq);
     new(totppqpmq);

     for xnr:=1 to 20160 do
     for ynr:=1 to 2 do ppqpmq^(.xnr,ynr.):=0;

     totppqpmq^:=ppqpmq^;

     if use_x or Use_y then
     begin
         nxperms:=1;
         for i:=1 to xdim do xperms^(.1,i.):=Recode_x(.i.);
         nyperms:=1;
         for i:=1 to ydim do yperms^(.1,i.):=Recode_y(.i.);

         //writeoutput(0);

         goto analyse;
     end;

     if ((xdim>9) and permute_x) or ((ydim>9) and permute_y) then
     begin
         if printoutput then writeln(outfile,'too many categories for this procedure:',xdim:3,ydim:3);
         pseudo_gamma:=2;
         pseudo_gamma_ready:=false;
         goto hypoteseslut;
     end;

     if not permute_x then
     begin
         nxperms:=1;
         for i:=1 to xdim  do xperms^(.1,i.):=i;
     end
     else
     begin
         DIGRAMStatusPanel(4,'Permute columns');
         permutate(xdim,nxperms,xperms,x_restricted,x_restrictions);
         writeOutput(1);
     end;

     if not permute_y then
     begin
         nyperms:=1;
         for i:=1 to ydim  do yperms^(.1,i.):=i;
     end
     else
     begin
         DIGRAMStatusPanel(4,'Permute rows');
         permutate(ydim,nyperms,yperms,y_restricted,y_restrictions);
         WriteOutput(2);
     end;

analyse:
     nperms:=nxperms*nyperms;

     //WriteOutput(3);

     Init_XYZ_test(outfile,Tab2,Rtab2,effective_n,effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R);

     // the table is ready - now we permutate

     maxgam             := 0;
     signedgam          := 0;
     pseudo_gamma_ready := false;

     for xnr:=1 to nxperms do
     for ynr:=1 to nyperms do
     begin
         if nxperms>1 then str(xnr,s0) else s0:='';
         if nyperms>1 then str(ynr,s1) else s1:='';
         s0:=s0+' '+s1;
         DIGRAMStatusPanel(5,s0);

         for i:=1 to c do xperm(.i.):=xperms^(.xnr,i.); xperm(.c+1.):=c+1;
         for i:=1 to r do yperm(.i.):=yperms^(.ynr,i.); yperm(.r+1.):=r+1;


         GammaTot:=xyz_recodedGamma(outfile,xperm,yperm,xdim,ydim,Tdimx,Tdimy,xy_size,nz,
                                    XYZ_TABLE,XYZ_ARRAYS,
                                    EXYZ_TABLE,EXYZ_ARRAYS);

         if abs(Gammatot)>maxgam then
         begin
             maxgam:=abs(Gammatot);
             signedGam:=Gammatot;
             sorted_x:=xperm;
             sorted_y:=yperm;
             //writeln(outfile,'  new maxgam =   ',gammatot:6:3,Signedgam:6:3);
         end;
         //else writeln(outfile,'  old maxgam =   ',maxgam:6:3);
     end;


     if signedGam>=0 then
     begin
         if permute_x then
         begin
             for i:=1 to c do xorder(.i.):=sorted_x(.i.);
         end
         else xorder:=zerobytes;

         if permute_y then
         begin
             for i:=1 to r do yorder(.i.):=sorted_y(.i.);
         end
         else yorder:=zerobytes;
     end
     else
     begin
         SignedGam:=-signedGam;
         if permute_x then
         begin
             for i:=1 to c do xorder(.i.):=sorted_x(.c-i+1.);
             if permute_y then
             begin
                 for i:=1 to r do yorder(.i.):=sorted_y(.i.);
             end
             else yorder:=zerobytes;
         end
         else
         begin
             xorder:=zerobytes;
             if permute_y then
             begin
                 for i:=1 to r do yorder(.i.):=sorted_y(.r-i+1.);
             end
             else yorder:=zerobytes;
         end;
     end;

     pseudo_gamma:=SignedGam;
     pseudo_gamma_ready:=true;

     goto hypoteseslut;

HYPOTESESLUT:
     dispose(xperms);
     dispose(yperms);
     dispose(ppqpmq);
     dispose(totppqpmq);

     //writeln(outfile,'pseudo gamma = ',pseudo_gamma:6:3);
end;

//---------------------------------------------------------------

PROCEDURE PairwisePseudogamma(var outfile : textfile;
                   compare_x     : boolean;
                   x_restricted   : boolean;
                   x_restrictions : bmatrix;
                   compare_y     : boolean;
                   y_restricted   : boolean;
                   y_restrictions : bmatrix;
                   use_x         : boolean;
                   use_y         : boolean;
                   recode_x      : Bvector;
                   recode_y      : Bvector;
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
              var  pseudo_gamma_ready : boolean;
              var  pseudo_gamma  : real;
              var  pseudo_gamma_p  : real;
              var xorder         : bvector;
              var yorder         : bvector;
                  printoutput    : boolean);

LABEL analyse,HYPOTESESLUT;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)

      X,Y             : BYTE;
      SUCCESS,ORDINAL : BOOLEAN;
      HYP             : BVECTOR;

      TAB2,permtab2   : TWOWAYTABLEarray;
      RTAB2,permRtab2 : ETWOWAYTABLEarray;

      Z,I,J,K,N,M,C,R,V1,V2 : INTEGER;
      V                     : BVECTOR;
      FRA,TIL,THELEVEL,Xtop,Ytop      : BYTE;
      SIM,LOCAL_NSIM,NSIM,NCHI,NGAMMA,NAbsGamma,nn,
      CHI_SPRT_STATUS,GAMMA_SPRT_STATUS : INTEGER;
      DUMMYCHAR                         : CHAR;

      CHITOT,GAMMATOT,AbsGammaTot,
      STOT,PPQTOT,PMQTOT,PMIN,
      PCHI,CHI,CHI1,PPQ,PPQ1,PMQ,PMQ1,GAMMA,GAMMA1,
      PGAMMA,S,PCHITOT,U,PGAMMATOT                   : Real;
      DF,DFTOT,ACCEPTNR,NSIM1,NSIM2,SIM1,SIM2        : INTEGER;

      // the following variables are used for permutated tests

      xperm,yperm,sorted_x,sorted_y       : bvector;
      maxgam,signedgam,simgam             : real;
      xperms,yperms                       : perm_ptr;
      ppqpmq,totppqpmq                    : perm_ppqpmq_ptr;
      nxperms,nyperms,nperms,xnr,ynr,nsig : integer;
      gammarecs,gammasequence             : Ptr_LongRvector;

      nxpairs,nypairs,npairs              : integer;

      NEWXYZ                              : Longint_data_array;
      ENEWXyz                             : Real_data_array;
      NewArrays,EnewArrays                : byte;

      effective_n                         : longint;
      effective_sim                       : integer;

      s1,s0,s2                             : string;
      cat1,cat2,nr                        : byte;

      nxlargest,nylargest                 : bvector;

      compareRows,CompareClos             : Boolean;

      // recs(.0.) and recs(.ncases+1.) must be initialized with low and high numbers
      // sequence contains reference to the recnumbers

      procedure WriteOutput(nr : integer);
      var i : integer;
      begin
          if printOutput then
          case nr of
               0 : begin
                       writeln(outfile);
                       writeln(outfile,'Pseudo gamma by pairwise comparisons: no permutations');
                   end;
               1 : begin
                      writeln(outfile);
                      writeln(outfile,nxpairs,'  pairs of columns');
                   end;
               2:  begin
                       writeln(outfile);
                       writeln(outfile,nypairs,' pairs of rows');
                   end;
               3 : begin
                       writeln(outfile,'Calculation of pseudo gamma by pairwise comparisons is inapropriate');
                       writeln(outfile,'both rows and columns should be compared');
                   end;
               10: begin
                       writeln(outfile);
                       write(outfile,'Optimal order:');

                       if compare_x then
                       begin
                           write(outfile,'   cols :');
                           for i:=1 to c do write(outfile,xorder(.i.):3);
                       end;

                       if compare_y then
                       begin
                           write(outfile,'   rows :');
                           for i:=1 to r do write(outfile,yorder(.i.):3);
                       end;

                       writeln(outfile);
                       writeln(outfile);
                       writeln(outfile);
                       writeln(outfile,'   Gamma = ',SignedGam:7:3);
                       writeln(outfile);
                   end;
          end;
      end;

      procedure findcategories(ynr: integer; ydim: byte;var cat1,cat2 : byte);
      label slut;
      var i,j,nr : integer;
      // dette må kunne gøres bedre
      begin
          nr:=0;
          for i:=1 to ydim-1 do
          for j:=i+1 to ydim do
          begin
              inc(nr);
              if nr=ynr then
              begin
                  cat1:=i;
                  cat2:=j;
                  goto slut;
              end;
          end;
      slut:
      end;

      Procedure countresult(var nlargest : bvector);
      begin
          if gammatot>0 then inc(nlargest(.cat2.)) else
          if gammatot<0 then inc(nlargest(.cat1.));
      end;

      procedure sortcategories(c : byte; nxlargest : bvector; var xorder : bvector);
      var i,j,nr : byte;
      begin
          xorder:=zerobytes;
          nr:=0;
          for i:=0 to c-1 do
          for j:=1 to c do
          if nxlargest(.j.)=i then
          begin
              inc(nr);
              xorder(.nr.):=j;
          end;
      end;

BEGIN
     new(ppqpmq);
     new(totppqpmq);

     for xnr:=1 to 20160 do
     for ynr:=1 to 2 do ppqpmq^(.xnr,ynr.):=0;

     totppqpmq^:=ppqpmq^;


     if not compare_x then
     begin
         nxpairs:=1;
     end
     else
     begin
         nxpairs:=(xdim*(xdim-1)) div 2;
         writeOutput(1);
     end;

     if not compare_y then
     begin
         nypairs:=1;
     end
     else
     begin
         nypairs:=(ydim*(ydim-1)) div 2;
         WriteOutput(2);
     end;

     if compare_x and compare_y then
     begin
         Writeoutput(3);
         goto hypoteseslut;
     end;

analyse:

     Init_XYZ_test(outfile,Tab2,Rtab2,effective_n,effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R);

     // the table is ready - now we permutate

     maxgam             := 0;
     signedgam          := 0;
     pseudo_gamma_ready := false;


     nxlargest := zerobytes;
     nylargest := zerobytes;

     //writeln(outfile,'nypairs = ',nypairs);

     for xnr:=1 to nxpairs do
     for ynr:=1 to nypairs do
     begin
         if not compare_x then for i:=1 to c do xperm(.i.):=i
         else
         begin
             findcategories(xnr,xdim,cat1,cat2);
             for i:=1 to c do
             if (i<>cat1) and (i<>cat2) then xperm(.i.):=0 else xperm(.i.):=i;
         end;

         if not compare_y then for i:=1 to r do yperm(.i.):=i
         else
         begin
             findcategories(ynr,ydim,cat1,cat2);
             for i:=1 to r do
             if (i<>cat1) and (i<>cat2) then yperm(.i.):=0 else yperm(.i.):=i;
         end;

         //if nxperms>1 then str(xnr,s0) else s0:='';
         //if nyperms>1 then str(ynr,s1) else s1:='';
         //s0:=s0+' '+s1;
         //DIGRAMStatusPanel(5,s0);

         // øv

         //writeln(OUTFILE);
         //write(outfile,'xperm = '); for i:=1 to xdim do write(outfile,xperm(.i.):3);writeln(outfile);
         //write(outfile,'yperm = '); for i:=1 to ydim do write(outfile,yperm(.i.):3);writeln(outfile);
         GammaTot:=xyz_recodedGamma(outfile,xperm,yperm,xdim,ydim,Tdimx,Tdimy,xy_size,nz,
                                    XYZ_TABLE,XYZ_ARRAYS,
                                    EXYZ_TABLE,EXYZ_ARRAYS);


         //writeln(outfile,cat1:3,cat2:3,gammatot:9:5);

         if compare_x then countresult(nxlargest) else countresult(nylargest);
     end;

     //for i:=1 to c do write(outfile,nxlargest(.i.):3); writeln(outfile);

     //for i:=1 to r do write(outfile,nylargest(.i.),' '); writeln(outfile);

     if compare_x then
     begin
         sortcategories(c,nxlargest,xorder);
         for i:=1 to r do yorder(.i.):=i;
     end
     else
     begin
         sortcategories(r,nylargest,yorder);
         for i:=1 to c do xorder(.i.):=i;
     end;


     pseudo_gamma:=xyz_recodedGamma(outfile,xorder,yorder,xdim,ydim,Tdimx,Tdimy,xy_size,nz,
                                    XYZ_TABLE,XYZ_ARRAYS,
                                    EXYZ_TABLE,EXYZ_ARRAYS);

     pseudo_gamma_ready:=true;

     goto hypoteseslut;

HYPOTESESLUT:
     dispose(ppqpmq);
     dispose(totppqpmq);

     //writeln(outfile,'pseudo gamma = ',pseudo_gamma:6:3);
end;

//---------------------------------------------------------------

PROCEDURE Pseudogamma_XYZ_TEST;
LABEL    analyse,HYPOTESESLUT,SIM_START,SIM_FINISH,SIM_FINISH1;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)

      X,Y             : BYTE;
      SUCCESS,ORDINAL : BOOLEAN;
      HYP             : BVECTOR;

      TAB2,permtab2   : TWOWAYTABLEarray;
      RTAB2,permRtab2 : ETWOWAYTABLEarray;

      Z,I,J,K,N,M,C,R,V1,V2 : INTEGER;
      V,xorder1,yorder1                             : BVECTOR;
      FRA,TIL,THELEVEL                              : BYTE;
      SIM,LOCAL_NSIM,NSIM,NCHI,NGAMMA,NAbsGamma,nn,
      CHI_SPRT_STATUS,GAMMA_SPRT_STATUS : INTEGER;
      DUMMYCHAR                                     : CHAR;

      CHITOT,GAMMATOT,AbsGammaTot,
      STOT,PPQTOT,PMQTOT,PMIN,
      PCHI,CHI,CHI1,PPQ,PPQ1,PMQ,PMQ1,GAMMA,GAMMA1,
      PGAMMA,S,PCHITOT,U,PGAMMATOT                  : Real;
      DF,DFTOT,ACCEPTNR,NSIM1,NSIM2,SIM1,SIM2       : INTEGER;

      // the following variables are used for permutated tests

      xperm,yperm,sorted_x,sorted_y  : bvector;
      maxgam,signedgam,simgam,absgam : real;
      xperms,yperms           : perm_ptr;
      ppqpmq,totppqpmq        : perm_ppqpmq_ptr;
      nxperms,nyperms,nperms,xnr,ynr,nsig : integer;
      gammarecs,gammasequence : Ptr_LongRvector;

      NEWXYZ  : Longint_data_array;
      ENEWXyz : Real_data_array;
      NewArrays,EnewArrays : byte;
      lr2x,lr2x_p,fittedgamma,p_fitgam,lrdelta : real;
      df2x                                    : integer;
      simgamp                                : real;
      xyz_fit                                : real_data_array;
      FIT_arrays                             : byte;

      nlr2                                   : integer;

      s1,s0 : string;
      firstcat,lastcat : string;

      // recs(.0.) and recs(.ncases+1.) must be initialized with low and high numbers
      // sequence contains reference to the recnumbers

    procedure comparereport(v : byte; categories : bvector; w : byte; firstcat,lastcat : string);
    var i : byte;
        ngrp : byte;
        grplabel,label2 : char;
    begin
        if v=1 then
        begin
            ngrp:=dimx;
            grplabel:=xlabel;
            label2:=ylabel;
            stringbox(outfile,'Optimal order of categories for '+xlabel+' - '+variablename(xlabel)+' relative to '+ylabel+' - '+variablename(ylabel));
        end
        else
        begin
            ngrp:=ydim;
            grplabel:=ylabel;
            label2:=xlabel;
            stringbox(outfile,'Optimal order of categories for '+ylabel+' - '+variablename(ylabel)+' relative to '+xlabel+' - '+variablename(xlabel));
        end;

        writeln(outfile,' ':11,variablename(label2):9,' categories');
        writeln(outfile,' ':18,'|':4,firstcat);
        writeln(outfile,' ':18,'|':4);
        for i:= 1 to ngrp do
        writeln(outfile,categories(.i.):2,category(grplabel,categories(.i.)):9,'|':11);
        writeln(outfile,' ':18,'|':4);
        writeln(outfile,' ':18,'|':4,lastcat);
        writeln(outfile);
    end;


    procedure pseudogammareport;
    begin
        if permute_x then
        begin
            if not permute_y then
            begin
                if (permuted_gamma>=0) then
                begin
                    firstcat:=category(ylabel,1);
                    lastcat:=category(ylabel,dimy);
                end
                else
                begin
                    lastcat:=category(ylabel,1);
                    firstcat:=category(ylabel,dimy);
                end;
            end
            else
               begin
                   firstcat:=' na';
                   lastcat:=' na';
               end;
               comparereport(1,xorder,2,firstcat,lastcat);
           end;
           if permute_y then
           begin
               if not permute_x then
               begin
                   if permuted_gamma>=0 then
                   begin
                       firstcat:=category(xlabel,1);
                       lastcat:=category(xlabel,dimx);
                   end
               else
               begin
                   lastcat:=category(xlabel,1);
                   firstcat:=category(xlabel,dimx);
               end;
           end
           else
           begin
               firstcat:=' na';
               lastcat:=' na';
           end;
           comparereport(2,yorder,1,firstcat,lastcat);
       end;
    end;


      procedure WriteOutput(nr : integer);
      var i : integer;
      begin
          if printOutput then
          case nr of
               0:  begin
                       Stringbox(outfile,'****  Analysis by pairwise comparisons of categories ****');
                   end;
               1 : begin
                      writeln(outfile);
                      writeln(outfile,nxperms,'  column permutations');
                   end;
               2:  begin
                       writeln(outfile);
                       writeln(outfile,nyperms,' row permutations');
                   end;
               10: begin
                       writeln(outfile);
                       write(outfile,'Optimal order:');

                       if permute_x then
                       begin
                           write(outfile,'   rows :');
                           for i:=1 to c do write(outfile,sorted_x(.i.):3);
                       end;

                       if permute_y then
                       begin
                           write(outfile,'   cols :');
                           for i:=1 to r do write(outfile,sorted_y(.i.):3);
                       end;
                       writeln(outfile);
                       writeln(outfile);
                       writeln(outfile);
                       writeln(outfile,'   Gamma = ',Permuted_gamma:7:3);
                       writeln(outfile);
                   end;
               11: begin
                       writeln(outfile);
                       write(outfile,'Optimal order:');

                       if permute_x then
                       begin
                           write(outfile,'   cols :');
                           for i:=1 to c do write(outfile,xorder(.i.):3);
                       end;

                       if permute_y then
                       begin
                           write(outfile,'   rows :');
                           for i:=1 to r do write(outfile,yorder(.i.):3);
                       end;
                       writeln(outfile);
                       writeln(outfile);
                       writeln(outfile);
                       writeln(outfile,'   Gamma = ',Permuted_gamma:7:3);
                       writeln(outfile);
                       pseudogammareport;
                   end;
               12: begin
                       writeln(outfile,'   Median gamma under independence = ',gammarecs^(.nn div 2.):6:3);
                       writeln(outfile);
                       Writeln(outfile,'    p = ',Ngamma/Nsim:7:3,'   nsim = ',nsim);
                       writeln(outfile);
                       writeln(outfile,'    Test against two-factor alternative:  LR = ',lr2:6:1,'   p = ',Nlr2/nsim:7:3);
                       writeln(outfile);
                   end;
               20: begin
                       stringbox(outfile,'Evaluation of significance by Monte Carlo tests');
                   end;
          end;
      end;
BEGIN
     INITIALIZE_ETAB(XYZ_fit,fit_ARRAYS);
     C := Xdim;
     R := Ydim;
     if analysistype<>2 then
     begin
     //okmessage('pseudogamma');
     pseudogamma(outfile,permute_x,x_restricted,x_restrictions,
                         permute_y,y_restricted,y_restrictions,
                         use_x,use_y,recode_x,recode_y,
                         xdim,ydim,tdimx,tdimy,xy_size,nz,
                         xtype,ytype,xlabel,ylabel,vlabel,
                         xyz_table,xyz_arrays,Exyz_table,Exyz_arrays,
                         nvar,hypnr,hypotese,
                         permuted_gamma_ready,permuted_gamma,permuted_gamma_p,
                         xorder,yorder,printoutput);
     end
     else
     begin
         //okmessage('pairwise');
         writeoutput(0);
         PairwisePseudogamma(outfile,permute_x,x_restricted,x_restrictions,
                             permute_y,y_restricted,y_restrictions,
                             use_x,use_y,recode_x,recode_y,
                             xdim,ydim,tdimx,tdimy,xy_size,nz,
                             xtype,ytype,xlabel,ylabel,vlabel,
                             xyz_table,xyz_arrays,Exyz_table,Exyz_arrays,
                             nvar,hypnr,hypotese,
                             permuted_gamma_ready,permuted_gamma,permuted_gamma_p,
                             xorder,yorder,printoutput);
     end;

     fit_xyz(outfile,xdim,ydim,tdimx,tdimy,xy_size,nz,xtype,ytype,xlabel,ylabel,vlabel,
             xyz_table,xyz_arrays,exyz_table,exyz_arrays,100,0.001,xyz_fit,lr2,df2,lr2_p,fittedgamma,p_fitgam,lrdelta,false);

     writeoutput(11);

     absgam:=abs(permuted_gamma);

     (*** CALCULATION OF EXACT P-VALUES ***)

     IF EXACT THEN
     BEGIN

         writeoutput(20);

         Start_exact_tests(hypnr,number_of_sim,results,sim,Nchi,Ngamma,NabsGamma);
         nlr2:=0;
         new(gammarecs);
         gammarecs^(.0.):=-5;
         gammarecs^(.number_of_sim.):=5;
         new(gammasequence);

         initialize_table(NewXYZ,x);
         Initialize_etab(EnewXYZ,Y);

SIM_START:
         Inc(SIM);

         IF SIM>NUMBER_OF_SIM THEN
         BEGIN
             SIM:=SIM-1;
             GOTO SIM_FINISH;
         END;

         randomize;

         Generate_XYZ_table(outfile,C,R,TdimX,TdimY,XY_size,NZ,XYZ_table,XYZ_arrays,EXYZ_table,EXYZ_arrays,NewXYZ,EnewXYZ);

         if analysistype<>2 then
         pseudogamma(outfile,permute_x,x_restricted,x_restrictions,
                             permute_y,y_restricted,y_restrictions,
                             use_x,use_y,recode_x,recode_y,
                             xdim,ydim,tdimx,tdimy,xy_size,nz,
                             xtype,ytype,xlabel,ylabel,vlabel,
                             NewXYZ,xyz_arrays,ENewXYZ,Exyz_arrays,
                             nvar,hypnr,hypotese,
                             permuted_gamma_ready,simgam,simgamp,xorder1,yorder1,false)
         else
         pairwisepseudogamma(outfile,permute_x,x_restricted,x_restrictions,
                             permute_y,y_restricted,y_restrictions,
                             use_x,use_y,recode_x,recode_y,
                             xdim,ydim,tdimx,tdimy,xy_size,nz,
                             xtype,ytype,xlabel,ylabel,vlabel,
                             NewXYZ,xyz_arrays,ENewXYZ,Exyz_arrays,
                             nvar,hypnr,hypotese,
                             permuted_gamma_ready,simgam,simgamp,xorder1,yorder1,false);

         fit_xyz(outfile,xdim,ydim,tdimx,tdimy,xy_size,nz,xtype,ytype,xlabel,ylabel,vlabel,
                 newxyz,xyz_arrays,enewxyz,exyz_arrays,100,0.001,xyz_fit,lr2x,df2x,lr2x_p,fittedgamma,p_fitgam,lrdelta,false);

         if abs(simgam)>=absgam then inc(ngamma);

         if lr2x>=lr2 then inc(nlr2);

         if sim<=LongVectorlength then gammarecs^(.sim.):=simgam;

         if CancelAfterProgressBoxStep then goto sim_finish;

         goto sim_start;

SIM_FINISH:
         NSIM:=SIM;
SIM_FINISH1:
         dispose_table(NewXyz,x); //okmessage('dispose 1');
         dispose_etab(EnewXYZ,y); //okmessage('dispose 2');
         if nsim<Longvectorlength then nn:=nsim else nn:=longvectorlength-1;

         gammarecs^(.0.):=-5;
         gammarecs^(.nn+1.):=5;

         SortLongRvector(nn,gammarecs,gammasequence); //okmessage('sorted');

         writeoutput(12);

         dispose(gammarecs);  //okmessage('dispose 3');
         dispose(gammasequence); //okmessage('dispose 4');
         HideProgressBox;

         if nsim>0 then
         begin
             permuted_gamma_p:=ngamma/nsim;
             lr2_p:=nlr2/nsim;
         end
         else permuted_gamma_p:=0;
     END;  (** OF EXACT-PVALUES **)

HYPOTESESLUT:
    DISPOSE_ETAB(XYZ_fit,Fit_arrays);
END;  (*** OF Permuted_XYZ_TEST ***)

(*-----------------------------------------------------------*)

FUNCTION XYZ_GAMMA;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)
      R,C        : BYTE;
      Z          : INTEGER;

      TAB2       : TWOWAYTABLEarray;
      RTAB2      : ETWOWAYTABLEarray;

      GAMMATOT,STOT,PPQTOT,PMQTOT,PPQ,PMQ,GAMMA,PGAMMA,S   : REAL;
BEGIN
     FILLCHAR(TAB2,SIZEOF(TAB2),0);
     FILLCHAR(RTAB2,SIZEOF(RTAB2),0);

     GAMMATOT:=0;  PPQTOT:=0;  PMQTOT:=0;

     C:=XDIM;   R:=YDIM;

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         (*++++++++++++++++++++++++*)
         (*++ SEPARATE TEST HERE ++*)
         (*++++++++++++++++++++++++*)

         RCGAMMA(C,R,TAB2,GAMMA,PGAMMA,PPQ,PMQ,S,TRUE);
         PPQTOT:=PPQTOT+PPQ;
         PMQTOT:=PMQTOT+PMQ;
         STOT:=STOT+S;

     END; (*** OF A 2-WAY TABLE ***)

     IF PPQTOT>0 THEN
     BEGIN
         GAMMATOT:=PMQTOT/PPQTOT;
     END
     ELSE GAMMATOT:=2.0;

     XYZ_GAMMA:=GAMMATOT;

END;  (*** of XYZ_GAMMA ***)

(*-----------------------------------------------------------*)

FUNCTION XYZ_GAMMA2;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)
      R,C        : BYTE;
      Z          : INTEGER;

      TAB2       : TWOWAYTABLEarray;
      RTAB2      : ETWOWAYTABLEarray;

      GAMMATOT,STOT,PPQTOT,PMQTOT,PPQ,PMQ,GAMMA,PGAMMA,S   : REAL;
BEGIN
     FILLCHAR(TAB2,SIZEOF(TAB2),0);
     FILLCHAR(RTAB2,SIZEOF(RTAB2),0);

     GAMMATOT:=0;  PPQTOT:=0;  PMQTOT:=0;

     C:=XDIM;   R:=YDIM;

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         (*++++++++++++++++++++++++*)
         (*++ SEPARATE TEST HERE ++*)
         (*++++++++++++++++++++++++*)
         if gammatype=3 then
         RCGAMMA3(C,R,TAB2,GAMMA,PPQ,PMQ)
         else
         RCGAMMA2(C,R,TAB2,GAMMA,PPQ,PMQ);
         PPQTOT:=PPQTOT+PPQ;
         PMQTOT:=PMQTOT+PMQ;
         STOT:=STOT+S;

     END; (*** OF A 2-WAY TABLE ***)

     IF PPQTOT>0 THEN
     BEGIN
         GAMMATOT:=PMQTOT/PPQTOT;
     END
     ELSE GAMMATOT:=2.0;

     XYZ_GAMMA2:=GAMMATOT;

END;  (*** of XYZ_GAMMA ***)

(*-----------------------------------------------------------*)

FUNCTION EXYZ_GAMMA;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)
      R,C        : BYTE;
      Z          : INTEGER;

      TAB2       : TWOWAYTABLEarray;
      RTAB2      : ETWOWAYTABLEarray;

      GAMMATOT,STOT,PPQTOT,PMQTOT,PPQ,PMQ,GAMMA,PGAMMA,S   : REAL;
BEGIN
     FILLCHAR(TAB2,SIZEOF(TAB2),0);
     FILLCHAR(RTAB2,SIZEOF(RTAB2),0);

     GAMMATOT:=0;  PPQTOT:=0;  PMQTOT:=0;

     C:=XDIM;   R:=YDIM;

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         (*++++++++++++++++++++++++*)
         (*++ SEPARATE TEST HERE ++*)
         (*++++++++++++++++++++++++*)

         Expected_RCGAMMA(C,R,RTAB2,GAMMA,PGAMMA,PPQ,PMQ,S,TRUE);
         PPQTOT:=PPQTOT+PPQ;
         PMQTOT:=PMQTOT+PMQ;
         STOT:=STOT+S;

     END; (*** OF A 2-WAY TABLE ***)

     IF PPQTOT>0 THEN
     BEGIN
         GAMMATOT:=PMQTOT/PPQTOT;
     END
     ELSE GAMMATOT:=2.0;

     EXYZ_GAMMA:=GAMMATOT;

END;  (*** of XYZ_GAMMA ***)

(*-----------------------------------------------------------*)

FUNCTION XYZ_RecodedGAMMA(var outfile : textfile;
                   xperm         : bvector;
                   yperm         : bvector;
                   XDIM,YDIM     : BYTE;
                   TDIMX,TDIMY   : BYTE;
                   XY_SIZE       : BYTE;
                   NZ            : LONGINT;
                   XYZ_TABLE     : LONGINT_DATA_ARRAY;
                   XYZ_ARRAYS    : BYTE;
                   EXYZ_TABLE    : REAL_DATA_ARRAY;
                   EXYZ_ARRAYS   : BYTE): Real;
VAR
      R,C,i,j    : BYTE;
      Z          : INTEGER;

      TAB2               : TWOWAYTABLEarray;
      RTAB2,AIJ,DIJ      : ETWOWAYTABLEarray;

      perm_TAB2  : TWOWAYTABLEarray;
      perm_RTAB2 : ETWOWAYTABLEarray;

      GAMMATOT,STOT,PPQTOT,PMQTOT,PPQ,PMQ,GAMMA,PGAMMA,S,p,q   : REAL;
BEGIN
     FILLCHAR(TAB2,SIZEOF(TAB2),0);
     FILLCHAR(RTAB2,SIZEOF(RTAB2),0);

     PPQTOT:=0;  PMQTOT:=0;

     C:=XDIM;   R:=YDIM;

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         (*****
         for j:=1 to r+1 do
         begin
             for i:=1 to c+1 do write(outfile,tab2(.i,j.):4);writeln(outfile);
         end;
         (****)


         perm_tab2:=EmptyTwoWaytable;

         for j:=1 to r do
         if yperm(.j.)>0 then
         begin
             for i:=1 to c do
             if xperm(.i.)>0 then
             perm_tab2(.i,j.):=tab2(.xperm(.i.),yperm(.j.).);
             //for i:=1 to c do write(outfile,perm_tab2(.i,j.):4); writeln(outfile); //øvøv
         end;


         (****** not used if we just want the gamma coefficients *****
         for i:=1 to c do
         for j:=1 to r do
         begin
             perm_tab2(.i,r+1.):=perm_tab2(.i,r+1.)+perm_tab2(.i,j.);
             perm_tab2(.c+1,j.):=perm_tab2(.c+1,j.)+perm_tab2(.i,j.);
             perm_tab2(.c+1,r+1.):=perm_tab2(.c+1,r+1.)+perm_tab2(.i,j.);
         end;
         (************************************************************)

         (*++++++++++++++++++++++++*)
         (*++ SEPARATE TEST HERE ++*)
         (*++++++++++++++++++++++++*)

         PREPARE_GAMMA_STATISTICS(C,R,Perm_tab2,AIJ,DIJ,P,Q,PPQ,PMQ);
         // instead of RCGAMMA(C,R,perm_TAB2,GAMMA,PGAMMA,PPQ,PMQ,S,TRUE);

         PPQTOT:=PPQTOT+PPQ;
         PMQTOT:=PMQTOT+PMQ;

         // STOT:=STOT+S;      not used at all here

     END; (*** OF A 2-WAY TABLE ***)

     IF PPQTOT>0 THEN
     BEGIN
         GAMMATOT:=PMQTOT/PPQTOT;
     END
     ELSE GAMMATOT:=2.0;

     XYZ_RecodedGAMMA:=GAMMATOT;

END;  (*** of XYZ_GAMMA ***)

//-----------------------------------------------------------

PROCEDURE EXPENSIVE_XYZ_TEST;
LABEL    HYPOTESESLUT,SIM_START,SIM_FINISH,SIM_FINISH1;
TYPE RTYPE = ARRAY(.1..LARGEDIM,1..DIMTOP.) OF REAL;
     RPTR  = ^RTYPE;
     ITYPE = ARRAY(.1..LARGEDIM,1..DIMTOP.) OF INTEGER;
     IPTR  = ^ITYPE;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)

      X,Y        : BYTE;
      SUCCESS,ORDINAL : BOOLEAN;

      HYP        : BVECTOR;
      G,SS       : RVECTOR;

      TAB2       : TWOWAYTABLEarray;
      RTAB2      : ETWOWAYTABLEarray;

      Z          : LONGINT;

      I,J,K,N,M,C,R,
      SIM,NSIM,NCHI,NGAMMA,NAbsGamma,
      CHI_SPRT_STATUS,GAMMA_SPRT_STATUS,
      DF,DFTOT        : INTEGER;

      V,V0                : BVECTOR;
      FRA,TIL,THELEVEL    : BYTE;
      DUMMYCHAR           : CHAR;

      CHITOT,GAMMATOT,AbsGammaTot,
      GMTOT,STOT,PPQTOT,PMQTOT,
      PCHI,CHI,CHI1,PPQ,PPQ1,PMQ,PMQ1,GAMMA,GAMMA1,
      PGAMMA,S,PCHITOT,U,PGAMMATOT                   : REAL;

      X_COORDINATE,Y_COORDINATE : BYTE;

      LOCAL_CHITOT,LOCAL_PPQTOT,LOCAL_PMQTOT,
      LOCAL_CHI,LOCAL_PPQ,LOCAL_PMQ,LOCAL_GAMMA,
      LOCAL_STOT,LOCAL_GAMMATOT,
      LOCAL_PCHITOT,LOCAL_PGAMMATOT        : RPTR;

      LOCAL_DFTOT,LOCAL_NCHI,LOCAL_NGAMMA  : IPTR;

      collaps_gamma : boolean;

      PROCEDURE UDTAB(VAR UDFIL: TEXT);
      VAR X,Y : BYTE;
      BEGIN
          WRITELN(UDFIL,'Z =',Z);
          WRITE(UDFIL,'Y X ');
          FOR X:=1 TO XDIM DO WRITE(UDFIL,X:3,'  ');
          WRITELN(UDFIL,'TOTAL');
          FOR Y:=1 TO YDIM+1 DO
          BEGIN
              IF Y<YDIM+1 THEN WRITE(UDFIL,Y,'   ')
                          ELSE WRITE(UDFIL,'TOT ');
              FOR X:=1 TO XDIM DO WRITE(UDFIL,TAB2(.X,Y.):3,'  ');
              WRITELN(UDFIL,TAB2(.TDIMX,Y.):3);
              WRITE(UDFIL,'    ');
              FOR X:=1 TO XDIM DO WRITE(UDFIL,RTAB2(.X,Y.):5:1);
              WRITELN(UDFIL,RTAB2(.TDIMX,Y.):5:1);
              WRITELN(UDFIL);
          END;
      END;

      PROCEDURE INIT_R(VAR X : RPTR);
      VAR I,J : BYTE;
      BEGIN
          NEW(X);
          FOR I:=1 TO largedim DO
          FOR J:=1 TO DIMTOP DO
          BEGIN
              X^(.I,J.):=0.0;
          END;
      END;

      PROCEDURE INIT_I(VAR X : IPTR);
      VAR I,J : BYTE;
      BEGIN
          NEW(X);
          FOR I:=1 TO largedim DO
          FOR J:=1 TO DIMTOP DO
          BEGIN
              X^(.I,J.):=0;
          END;
      END;

      PROCEDURE CALCULATE_PVALUES(VAR PCHITOT        : REAL;
                                      DFTOT          : INTEGER;
                                      CHITOT         : REAL;
                                  VAR GAMMATOT       : REAL;
                                      PPQTOT,PMQTOT  : REAL;
                                  VAR STOT,PGAMMATOT : REAL);
      BEGIN
          PCHITOT:=PFCHI(DFTOT,CHITOT);
          IF ORDINAL THEN Calculate_gamma_statistics(PMQtot,PPQtot,GammaTot,AbsGammatot,Stot,U,Pgammatot);
      END;

      PROCEDURE WRITE_TEST_HEADING(VAR UDFIL : TEXT);
      BEGIN
          WRITELN(UDFIL);
          WRITELN(UDFIL,'Hypotheses       ',TEST_STATISTIC,'  df asymp exact  gamma asymp exact');
          WRITELN(UDFIL,'------------------------------------------------------');
      END;

      PROCEDURE WRITE_TEST_TOP(VAR UDFIL : TEXT);
      BEGIN
          WRITELN(UDFIL,'------------------------------------------------------');
      END;

      PROCEDURE WRITE_TESTRESULTS(VAR UDFIL : TEXT;
                                  CHI : REAL; DF: INTEGER;
                                  PCHI, EXACT_PCHI : REAL;
                                  GAMMA, PGAMMA,EXACT_PGAMMA : REAL;
                                  INCLUDE_HYP : BOOLEAN);
      BEGIN
          IF INCLUDE_HYP THEN
          WRITE_HYPOTESE(UDFIL,NVAR,HYPNR,HYPOTESE,VLABEL,FALSE);

          IF DF=0 THEN WRITE(UDFIL,' ':6,0:4) ELSE
          BEGIN
              WRITE(UDFIL,CHI:6:1,DF:4,PCHI:6:3);
              IF EXACT AND (NUMBER_OF_SIM>0) THEN WRITE(UDFIL,EXACT_PCHI:6:3)
                                             ELSE WRITE(UDFIL,' ':6);
              IF ORDINAL THEN
              BEGIN
                  WRITE(UDFIL,GAMMA:7:2,PGAMMA:6:3);
                  IF EXACT AND (NUMBER_OF_SIM>0)
                  THEN WRITE(UDFIL,EXACT_PGAMMA:6:3);
              END;
          END;
      END;

      PROCEDURE WRITE_LOCAL_TESTRESULTS(VAR UDFIL: TEXT; V: BYTE);
      VAR I   : BYTE;
          CAT : CATEGORY_ARRAY;
          EXACT_PCHI,EXACT_PGAMMA : REAL;
      BEGIN
          WRITELN(UDFIL);
          WRITE_VNAME(UDFIL,V,VNAMES,VNAMES_EXIST);
          WRITELN(UDFIL,'(',VLABEL(.V.),')');

          GET_CATEGORIES(V,NVAR,DIM,VTYPE,
                         CATNAME,CATFILE,CATFILE_EXISTS,
                         VLABEL,CAT);

          FOR I:=1 TO DIM(.V.) DO
          BEGIN
              WRITE(UDFIL,I:2,':',CAT(.I.):8,' ':2);
              IF NSIM>0 THEN
              BEGIN
                  EXACT_PCHI:=LOCAL_NCHI^(.V,I.)/NSIM;
                  EXACT_PGAMMA:=LOCAL_NGAMMA^(.V,I.)/NSIM;
              END
              ELSE
              BEGIN
                  EXACT_PCHI:=2;
                  EXACT_PGAMMA:=2;
              END;
              WRITE_TESTRESULTS(UDFIL,LOCAL_CHITOT^(.V,I.),LOCAL_DFTOT^(.V,I.),
                                      LOCAL_PCHITOT^(.V,I.),EXACT_PCHI,
                                      LOCAL_GAMMATOT^(.V,I.),
                                      LOCAL_PGAMMATOT^(.V,I.),
                                      EXACT_PGAMMA,FALSE);
              WRITELN(UDFIL);
          END;
      END;


BEGIN
     if responseToQuestion('Do you want an analysis of interactions?')=6 then collaps_gamma:=true else collaps_gamma:=false;

     // the following arrays are all (.1..nvar,1..dimtop.) arrays

     // first some reals

     INIT_R(LOCAL_GAMMATOT);
     INIT_R(LOCAL_GAMMA);
     INIT_R(LOCAL_CHITOT);
     INIT_I(LOCAL_DFTOT);
     INIT_R(LOCAL_PPQTOT);
     INIT_R(LOCAL_PMQTOT);
     INIT_R(LOCAL_STOT);
     INIT_R(LOCAL_PCHITOT);
     INIT_R(LOCAL_PGAMMATOT);
     INIT_R(LOCAL_CHI);
     INIT_R(LOCAL_PPQ);
     INIT_R(LOCAL_PMQ);

     // then some integers

     INIT_I(LOCAL_NCHI);
     INIT_I(LOCAL_NGAMMA);

     Fillchar(V0,sizeof(V0),0);

     FILLCHAR(G,SIZEOF(G),0);
     SS:=G;

     write_TEST_top(outfile);

     Init_XYZ_test(outfile,Tab2,Rtab2,Effective_N,Effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R);

     FOR Z:=1 TO NZ DO
     BEGIN
         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         V:=V0;

         FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
         IF CONTROL_VARIABLES(.I.) THEN
         GET_BYTE_DATA(NVAR,ZCELL,ZCELL_ARRAYS,Z,I,V(.I.),SUCCESS);

         (*++++++++++++++++++++++++*)
         (*++ SEPARATE TEST HERE ++*)
         (*++++++++++++++++++++++++*)

         RCCHI(C,R,TAB2,RTAB2,CHI,DF,PCHI,TRUE);

         IF DF>0 THEN
         BEGIN
             CHITOT:=CHITOT+CHI;   DFTOT:=DFTOT+DF;
             FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
             IF CONTROL_VARIABLES(.I.) THEN
             BEGIN
                 (* COUNT LOCAL_CHISQUARES AND DF'S *)
                 LOCAL_CHITOT^(.I,V(.I.).):=LOCAL_CHITOT^(.I,V(.I.).)+CHI;
                 LOCAL_DFTOT^(.I,V(.I.).):=LOCAL_DFTOT^(.I,V(.I.).)+DF;
             END;

             EFFECTIVE_N:=EFFECTIVE_N+TAB2(.C+1,R+1.);

             IF ORDINAL THEN
             BEGIN
                 RCGAMMA(C,R,TAB2,GAMMA,PGAMMA,PPQ,PMQ,S,TRUE);
                 PPQTOT:=PPQTOT+PPQ;
                 PMQTOT:=PMQTOT+PMQ;
                 STOT:=STOT+S;
                 FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
                 IF CONTROL_VARIABLES(.I.) THEN
                 BEGIN
                     (* COUNT LOCAL_CHISQUARES AND DF'S *)
                     LOCAL_PPQTOT^(.I,V(.I.).):=LOCAL_PPQTOT^(.I,V(.I.).)+PPQ;
                     LOCAL_PMQTOT^(.I,V(.I.).):=LOCAL_PMQTOT^(.I,V(.I.).)+PMQ;
                     LOCAL_STOT^(.I,V(.I.).):=LOCAL_STOT^(.I,V(.I.).)+S;
                 END;
             END;
         END; (* DF>0 *)
     END; (*** OF A 2-WAY TABLE ***)

     CALCULATE_PVALUES(PCHITOT,DFTOT,CHITOT,
                      GAMMATOT,PPQTOT,PMQTOT,
                      STOT,PGAMMATOT);

     FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
     IF CONTROL_VARIABLES(.I.) THEN
     FOR J:=1 TO DIM(.I.) DO
     BEGIN
         CALCULATE_PVALUES(LOCAL_PCHITOT^(.I,J.),LOCAL_DFTOT^(.I,J.),
                           LOCAL_CHITOT^(.I,J.),
                           LOCAL_GAMMATOT^(.I,J.),
                           LOCAL_PPQTOT^(.I,J.),LOCAL_PMQTOT^(.I,J.),
                           LOCAL_STOT^(.I,J.),LOCAL_PGAMMATOT^(.I,J.));
     END;

     IF EFFECTIVE_N=0 THEN
     BEGIN
         write_hypotese(outfile,nvar,hypnr,hypotese,vlabel,false);
         WRITELN(outfile,'   Empty table');
         IF HYPNR<=MAXHYP THEN
         NoResults(hypnr,results);
         GOTO HYPOTESESLUT;
     END
     ELSE
     IF DFTOT=0 THEN
     BEGIN
         write_hypotese(outfile,nvar,hypnr,hypotese,vlabel,false);
         WRITELN(outfile,'   No degrees of freedom');
         IF HYPNR<=MAXHYP THEN
         NoResults(hypnr,results);
         GOTO HYPOTESESLUT;
     END;

     Transfer_results(Hypnr,ChiTot,DfTot,PchiTot,GammaTot,AbsGammaTot,PGammaTot,Results);

     (*** CALCULATION OF EXACT P-VALUES ***)

     IF EXACT AND (NUMBER_OF_SIM>0) THEN
     BEGIN
         Start_exact_tests(hypnr,number_of_sim,results,sim,Nchi,Ngamma,NabsGamma);
SIM_START:
         INC(SIM);
         IF SIM>NUMBER_OF_SIM THEN
         BEGIN
             SIM:=SIM-1;
             GOTO SIM_FINISH;
         END;

         BEGIN
             CHI:=0;
             PPQ:=0;
             PMQ:=0;

             FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
             IF CONTROL_VARIABLES(.I.) THEN
             FOR J:=1 TO DIM(.I.) DO
             BEGIN
                 LOCAL_CHI^(.I,J.):=0.0;
                 LOCAL_PPQ^(.I,J.):=0.0;
                 LOCAL_PMQ^(.I,J.):=0.0;
             END;

             FOR Z:=1 TO NZ DO
             BEGIN
                 TRANSFER_EXYZ_SLICE(Z,EXYZ_TABLE,EXYZ_ARRAYS,
                                     TDIMX,TDIMY,XY_SIZE,RTAB2);
                 FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
                 IF CONTROL_VARIABLES(.I.) THEN
                 GET_BYTE_DATA(NVAR,ZCELL,ZCELL_ARRAYS,Z,I,V(.I.),SUCCESS);
                 GENTAB(outfile,XDIM,YDIM,RTAB2,ORDINAL,CHI1,PPQ1,PMQ1);
                 CHI:=CHI+CHI1;

                 FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
                 IF CONTROL_VARIABLES(.I.) THEN
                 LOCAL_CHI^(.I,V(.I.).):=LOCAL_CHI^(.I,V(.I.).)+CHI1;

                 IF ORDINAL THEN
                 BEGIN
                     PPQ:=PPQ+PPQ1;
                     PMQ:=PMQ+PMQ1;
                     FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
                     IF CONTROL_VARIABLES(.I.) THEN
                     BEGIN
                         LOCAL_PPQ^(.I,V(.I.).):=LOCAL_PPQ^(.I,V(.I.).)+PPQ1;
                         LOCAL_PMQ^(.I,V(.I.).):=LOCAL_PMQ^(.I,V(.I.).)+PMQ1;
                     END;
                 END;
             END;

             (************************************************************
              *** TESTSTATISTICS HAVE BEEN GENERATED FOR ALL SEPARATE  ***
              *** TABLES. WHAT REMAINS IS JUST TO  CALCULATE THE TOTAL ***
              *** STATISTICS AND TO COMPARE THE GENERATED STATISTICS   ***
              *** WITH THE OBSERVED                                    ***
              ************************************************************)

             IF CHI>=CHITOT THEN INC(NCHI);

             FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
             IF CONTROL_VARIABLES(.I.) THEN
             FOR J:=1 TO DIM(.I.) DO
             BEGIN
                 IF LOCAL_CHI^(.I,J.)>=LOCAL_CHITOT^(.I,J.)
                 THEN INC(LOCAL_NCHI^(.I,J.));
             END;

             IF SEQUENTIAL THEN
             BEGIN
                 IF SEQ_T(NCHI,SIM,SEQ_P0)>=SEQ_B
                 THEN
                 CHI_SPRT_STATUS:=1;
             END;

             IF ORDINAL THEN
             BEGIN
                 IF PPQ>0 THEN GAMMA:=PMQ/PPQ ELSE GAMMA:=0;
                 IF ((GAMMATOT>0) AND (GAMMA>=GAMMATOT))
                 OR ((GAMMATOT<0) AND (GAMMA<=GAMMATOT))
                 OR (GAMMATOT=0) THEN INC(NGAMMA);

                 If Abs(gamma)>=AbsGammatot then inc(NAbsGamma);

                 FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
                 IF CONTROL_VARIABLES(.I.) THEN
                 FOR J:=1 TO DIM(.I.) DO
                 BEGIN
                     IF LOCAL_PPQ^(.I,J.)>0
                     THEN LOCAL_GAMMA^(.I,J.):=
                     LOCAL_PMQ^(.I,J.)/LOCAL_PPQ^(.I,J.)
                     ELSE
                     LOCAL_GAMMA^(.I,J.):=0;

                     IF  ((LOCAL_GAMMATOT^(.I,J.)>0)
                     AND (LOCAL_GAMMA^(.I,J.)>=LOCAL_GAMMATOT^(.I,J.)))
                     OR ((LOCAL_GAMMATOT^(.I,J.)<0)
                     AND (LOCAL_GAMMA^(.I,J.)<=LOCAL_GAMMATOT^(.I,J.)))
                     OR (LOCAL_GAMMATOT^(.I,J.)=0)
                     THEN INC(LOCAL_NGAMMA^(.I,J.));
                 END;
                 IF SEQUENTIAL
                 THEN
                 BEGIN
                     IF SEQ_T(NGAMMA,SIM,SEQ_P0)>=SEQ_B
                     THEN GAMMA_SPRT_STATUS:=1;
                 END;
             END;
         END;

         if CancelAfterProgressBoxStep then goto sim_finish;

         if stop_exact_tests(sequential,chi_sprt_status,gamma_sprt_status,sim,nsim)
         then goto sim_finish1;

         goto sim_start;

SIM_FINISH:
         NSIM:=SIM;

SIM_FINISH1:
         HideProgressBox;

         RESULTS(.HYPNR,4.):=NCHI/NSIM;

         IF ORDINAL THEN
         BEGIN
             RESULTS(.HYPNR,7.):=NGAMMA/NSIM;
             Results(.hypnr,9.):=NabsGamma/Nsim;
         END;

         EFFECTIVE_SIM:=SIM;
         RESULTS(.HYPNR,8.):=SIM;
     END;  (** OF EXACT-PVALUES **)

HYPOTESESLUT:

     WRITE_TEST_HEADING(outfile);
     If Nsim>0 then
     WRITE_TESTRESULTS(outfile,CHITOT,DFTOT,PCHITOT,NCHI/NSIM,
                       GAMMATOT,PGAMMATOT,NGAMMA/NSIM,TRUE)
     else
     WRITE_TESTRESULTS(outfile,CHITOT,DFTOT,PCHITOT,0.0,
                       GAMMATOT,PGAMMATOT,0.0,TRUE);

     WRITELN(outfile);

     WRITELN(outfile);
     FOR I:=CONTROL_LIMITS(.1.) TO CONTROL_LIMITS(.2.) DO
     IF CONTROL_VARIABLES(.I.) THEN
     BEGIN
         WRITE_LOCAL_TESTRESULTS(outfile,I);
         IF ORDINAL THEN
         begin
             FOR J:=1 TO DIM(.I.) DO
             BEGIN
                 G(.J.) :=LOCAL_GAMMATOT^(.I,J.);
                 SS(.J.):=LOCAL_STOT^(.I,J.);
             END;
             if collaps_gamma then
             COLLAPSE_GAMMAVALUES(outfile,DIM(.I.),VTYPE(.I.),G,SS);
         end;
     END;

     DISPOSE(LOCAL_GAMMATOT);
     DISPOSE(LOCAL_GAMMA);
     DISPOSE(LOCAL_CHITOT);
     DISPOSE(LOCAL_DFTOT);
     DISPOSE(LOCAL_PPQTOT);
     DISPOSE(LOCAL_PMQTOT);
     DISPOSE(LOCAL_STOT);
     DISPOSE(LOCAL_PCHITOT);
     DISPOSE(LOCAL_PGAMMATOT);
     DISPOSE(LOCAL_CHI);
     DISPOSE(LOCAL_PPQ);
     DISPOSE(LOCAL_PMQ);
     DISPOSE(LOCAL_NCHI);
     DISPOSE(LOCAL_NGAMMA);

END;  (*** OF EXPENSIVE_XYZ_TEST ***)

//------------------------------------------------------------

Procedure Print_XYZ_table(var outfile : textfile;
                              tekst   : string;
                              TDIMX,TDIMY : Byte;
                              NZ      : integer;
                              XY_size : byte;
                             XYZ_TABLE : LONGINT_DATA_ARRAY;
                             XYZ_ARRAYS: BYTE;
                            EXYZ_TABLE : REAL_DATA_ARRAY;
                            EXYZ_ARRAYS: BYTE);
var z : integer;
    x,y : byte;
    XY_TABLE : TWOWAYTABLEarray;
    EXY_TABLE : ETWOWAYTABLEarray;
begin
     Stringbox(outfile,tekst);
     //writeln(outfile,'TDIMX & TDIMY & NZ = ',TDIMX:3,TDIMY:3,NZ:5);
     for z:=1 to nz do
     begin
         writeln(outfile);
         writeln(outfile,' Z = ',z);
         writeln(outfile);
         transfer_xyz_slice(z,XYZ_table,XYZ_arrays,EXYZ_table,EXYZ_arrays,TDIMX,TDIMY,XY_size,XY_table,EXY_table);

         if xy_table(.tdimx,tdimy.)>0 then
         begin
             write(outfile,'   ');
             for x:=1 to tdimx-1 do
             if xy_table(.x,tdimy.)>0 then write(outfile,x:7);
             writeln(outfile,'      TOT');
         end;

         for y:=1 to tdimy do
         if xy_table(.tdimx,y.)>0 then
         begin
             if (y<tdimy) and (xy_table(.tdimx,y.)>0) then write(outfile,y:3) else
             if (y=tdimy) and (xy_table(.tdimx,y.)>0) then
             begin
                 writeln(outfile);
                 write(outfile,'tot');
             end;

             for x:=1 to tdimx-1 do
             if xy_table(.x,tdimy.)>0 then write(outfile,XY_table(.x,y.):7);

             if xy_table(.tdimx,y.)>0 then writeln(outfile,xy_table(.tdimx,y.):9);

             if (xy_table(.tdimx,tdimy.)>0) and (y=tdimy-1) then writeln(outfile);
         end;

         writeln(outfile);
         writeln(outfile);

         for y:=1 to tdimy do
         if xy_table(.tdimx,y.)>0 then
         begin
             if (y<tdimy) and (xy_table(.tdimx,y.)>0) then write(outfile,y:3) else
             if (y=tdimy) and (xy_table(.tdimx,y.)>0) then
             begin
                 writeln(outfile);
                 write(outfile,'tot');
             end;

             for x:=1 to tdimx-1 do
             if xy_table(.x,tdimy.)>0 then write(outfile,XY_table(.x,y.)/xy_table(.tdimx,y.):7:3);

             if xy_table(.tdimx,y.)>0 then writeln(outfile,xy_table(.tdimx,y.)/xy_table(.tdimx,y.):9:3);

             if (xy_table(.tdimx,tdimy.)>0) and (y=tdimy-1) then writeln(outfile);
         end;
     end;
end;

//------------------------------------------------------------------------------


Procedure Print_recoded_XYZ_table;
(*****************************************************
                             (var outfile : textfile;
                              tekst   : string;
                              Xlabel,Ylabel : char;
                              TDIMX,TDIMY : Byte;
                              NXgroups,NYgroups : byte;
                              Xcodes,Ycodes : byte;
                              NZ      : integer;
                              XY_size : byte;
                             XYZ_TABLE : LONGINT_DATA_ARRAY;
                             XYZ_ARRAYS: BYTE;
                            EXYZ_TABLE : REAL_DATA_ARRAY;
                            EXYZ_ARRAYS: BYTE);
******************************************************)

var z : integer;
    x,y,c,r : byte;
    XY_TABLE : TWOWAYTABLEarray;
    tab2     : TwowaytableArray;
    EXY_TABLE : ETWOWAYTABLEarray;

    Procedure PresentVariable(tekst: string;Xlabel: char;C,NXgroups: byte;Xcodes: bvector);
    var i,j : byte;
    begin
        (****)
        writeln(outfile,tekst,Xlabel:2,' ',Variablename(Xlabel));
        for i:=1 to NXgroups do
        begin
            write(outfile,i:5);
            for j:=1 to C do
            if xcodes(.j.)=i then write(outfile,' ',category(Xlabel,j));
            writeln(outfile);
        end;
        writeln(outfile);
        (****)
    end;
begin
     Stringbox(outfile,tekst);

     c:=Tdimx-1;
     r:=Tdimy-1;

     PresentVariable('Column variable',Xlabel,C,NXgroups,Xcodes);
     PresentVariable('Row variable   ',Ylabel,R,NYgroups,Ycodes);

     for z:=1 to nz do
     begin
         tab2:=emptyTwoWaytable;
         writeln(outfile);
         writeln(outfile,' Z = ',z);
         writeln(outfile);
         transfer_xyz_slice(z,XYZ_table,XYZ_arrays,EXYZ_table,EXYZ_arrays,TDIMX,TDIMY,XY_size,XY_table,EXY_table);

         for x:=1 to C do
         for y:=1 to r do
         tab2(.xcodes(.x.),ycodes(.y.).):=
         tab2(.xcodes(.x.),ycodes(.y.).)+
         XY_table(.x,y.);

         for x:=1 to NXgroups do
         for y:=1 to NYgroups do
         begin
             tab2(.x,NYgroups+1.):=tab2(.x,NYgroups+1.)+tab2(.x,y.);
             tab2(.NXgroups+1,y.):=tab2(.NXgroups+1,y.)+tab2(.x,y.);
             tab2(.NXgroups+1,NYgroups+1.):=tab2(.NXgroups+1,NYgroups+1.)+tab2(.x,y.);
         end;

         if tab2(.NXgroups+1,NYgroups+1.)>0 then
         begin
             write(outfile,'   ');
             for x:=1 to NXgroups do
             if tab2(.x,NYgroups.)>0 then write(outfile,x:7);
             writeln(outfile,'      TOT');
         end;

         for y:=1 to NYgroups+1 do
         if tab2(.NXgroups+1,y.)>0 then
         begin
             if (y<NYgroups+1) and (tab2(.NXgroups+1,y.)>0) then write(outfile,y:3) else
             if (y=NYgroups+1) and (tab2(.NXgroups+1,y.)>0) then
             begin
                 writeln(outfile);
                 write(outfile,'tot');
             end;

             for x:=1 to NXgroups do
             if tab2(.x,NYgroups+1.)>0 then write(outfile,tab2(.x,y.):7);

             if tab2(.NXgroups+1,y.)>0 then writeln(outfile,tab2(.NXgroups+1,y.):9);

             if (tab2(.NXgroups+1,NYgroups+1.)>0) and (y=NYgroups+1) then writeln(outfile);
         end;

         writeln(outfile);
         writeln(outfile);

         
         (***)
         for y:=1 to NYgroups+1 do
         if tab2(.NXgroups+1,y.)>0 then
         begin
             if (y<NYgroups+1) and (tab2(.NXgroups+1,y.)>0) then write(outfile,y:3) else
             if (y=NYgroups+1) and (tab2(.NXgroups+1,y.)>0) then
             begin
                 writeln(outfile);
                 write(outfile,'tot');
             end;

             for x:=1 to NXgroups do
             if tab2(.NXgroups+1,y.)>0 then write(outfile,tab2(.x,y.)/tab2(.NXgroups+1,y.):7:3);

             if tab2(.NXgroups+1,y.)>0 then writeln(outfile,tab2(.NXgroups+1,y.)/tab2(.NXgroups+1,y.):9:3);

             if (tab2(.NXgroups+1,NYgroups+1.)>0) and (y=NYgroups+1) then writeln(outfile);
         end;
         (***)

     end;
end;

//------------------------------------------------------------------------------

Procedure Print_eXYZ_table(var outfile : textfile;
                              tekst   : string;
                              TDIMX,TDIMY : Byte;
                              NZ      : integer;
                              XY_size : byte;
                             XYZ_TABLE : LONGINT_DATA_ARRAY;
                             XYZ_ARRAYS: BYTE;
                            EXYZ_TABLE : REAL_DATA_ARRAY;
                            EXYZ_ARRAYS: BYTE);
var z : integer;
    x,y : byte;
    XY_TABLE : TWOWAYTABLEarray;
    EXY_TABLE : ETWOWAYTABLEarray;
begin
     Stringbox(outfile,tekst);
     writeln(outfile,'TDIMX & TDIMY & NZ = ',TDIMX:3,TDIMY:3,NZ:5);
     for z:=1 to nz do
     begin
         writeln(outfile,' Z = ',z);
         writeln(outfile);
         transfer_exyz_slice(z,EXYZ_table,EXYZ_arrays,TDIMX,TDIMY,XY_size,EXY_table);
         for y:=1 to tdimy do
         begin
             for x:=1 to tdimx-1 do write(outfile,eXY_table(.x,y.):7:1);
             writeln(outfile,exy_table(.tdimx,y.):9:1);
             if y=tdimy-1 then writeln(outfile);
         end;
         writeln(outfile);
     end;
end;

//------------------------------------------------------------------------------

PROCEDURE XYZ_Gamma2_TEST;
LABEL    HYPOTESESLUT,SIM_START,SIM_FINISH,SIM_FINISH1;
VAR
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

      newtab : TwowaytableArray;

      s1,s2,s3 : string;

BEGIN

     Init_XYZ_test(outfile,Tab2,Rtab2,Effective_N,Effective_sim,
                   Chi_sprt_status,Gamma_sprt_status,
                   CHITOT,GammaTot,STOT,PPQTOT,PMQTOT,DFTOT,
                   Xdim,Xtype,Ydim,Ytype,Ordinal,C,R);

     FOR Z:=1 TO NZ DO
     BEGIN
         Transfer_xyz_slice(z,xyz_table,XYZ_arrays,Exyz_table,Exyz_arrays,Tdimx,Tdimy,xy_size,tab2,Rtab2);
         //TRANSFER_EXYZ_SLICE(Z,EXYZ_TABLE,EXYZ_ARRAYS,
         //                    TDIMX,TDIMY,XY_SIZE,RTAB2);

         if gammatype=3 then rcgamma3(c,r,tab2,gamma,ppq,pmq)
         else rcgamma2(c,r,tab2,gamma,ppq,pmq);

         PPQTOT:=PPQTOT+PPQ;
         PMQTOT:=PMQTOT+PMQ;
     END;

     str(pmqtot:10:0,s1);
     str(ppqtot:10:0,s2);
     str(gammatot:6:3,s3);

     if ppqtot>0 then gammatot:=pmqtot/ppqtot;
     absgammatot:=abs(gammatot);

     //str(pmqtot:10:0,s1);
     //str(ppqtot:10:0,s2);
     //str(gammatot:6:3,s3);
     //okmessage(s1+'/'+s2+'='+s3);

     (*** CALCULATION OF EXACT P-VALUES ***)

     IF EXACT AND (PMIN<=EXACT_P_LEVEL) THEN
     BEGIN
         Start_exact_tests(hypnr,number_of_sim,results,sim,Nchi,Ngamma,NabsGamma);
SIM_START:
         Inc(SIM);
         IF SIM>NUMBER_OF_SIM THEN
         BEGIN
             SIM:=SIM-1;
             GOTO SIM_FINISH;
         END;

         PPQ:=0;
         PMQ:=0;

         FOR Z:=1 TO NZ DO
         BEGIN
             TRANSFER_EXYZ_SLICE(Z,EXYZ_TABLE,EXYZ_ARRAYS,
                                 TDIMX,TDIMY,XY_SIZE,RTAB2);

             GENTAB0(outfile,XDIM,YDIM,RTAB2,Newtab);
             if gammatype=3 then rcgamma3(c,r,newtab,gamma,ppq1,pmq1)
             else rcgamma2(c,r,newtab,gamma,ppq1,pmq1);
             PPQ:=PPQ+PPQ1;
             PMQ:=PMQ+PMQ1;
         END;

         (************************************************************
          *** TESTSTATISTICS HAVE BEEN GENERATED FOR ALL SEPARATE  ***
          *** TABLES. WHAT REMAINS IS JUST TO  CALCULATE THE TOTAL ***
          *** STATISTICS AND TO COMPARE THE GENERATED STATISTICS   ***
          *** WITH THE OBSERVED                                    ***
          ************************************************************)

         Evaluate_simulated_gamma(Sequential,pmq,ppq,gammatot,absgammatot,sim,
                                  seq_p0,seq_b,gamma,ngamma,nAbsgamma,
                                  CHI_sprt_status,Gamma_sprt_status);

         if CancelAfterProgressBoxStep then goto sim_finish;

         if stop_exact_tests(sequential,chi_sprt_status,gamma_sprt_status,sim,nsim)
         then goto sim_finish1;
         goto sim_start;

SIM_FINISH:
         NSIM:=SIM;
SIM_FINISH1:
         HideProgressBox;

         Gamma2:=Gammatot;

         Pgamma2  := NGAMMA/NSIM;
         PAbsgamma2:= NabsGamma/Nsim;

         EFFECTIVE_SIM      := SIM;
     END;  (** OF EXACT-PVALUES **)

HYPOTESESLUT:
END;  (*** OF CHEAP_XYZ_TEST ***)

(*-------------------------------------------------------*)

BEGIN
end. (** of SKXYZ1 **)
