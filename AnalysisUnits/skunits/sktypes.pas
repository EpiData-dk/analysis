{$F+}

UNIT SKTYPES;

INTERFACE
Uses Graphics;


CONST

    (*--------------------------------------*)
    (* Constants defining data matrices     *)
    (*--------------------------------------*)

    MAX_DATA_ARRAYS        = 5;   // 11
    MAX_TAB_ARRAYS         = 5;   // 8
    MAX_ETAB_ARRAYS        = 5;   // 8

    MAX_NUMBER_OF_BYTES    = 655000;
    MAX_NUMBER_OF_LONGINT  = 162230;
    MAX_NUMBER_OF_INTEGERS = 162230;
    MAX_NUMBER_OF_REALS    = 81890;
    max_number_of_extended = 65400;

    MAXWINDOWS     = 3;
    MAXVAR         = 8;   (* the maximal number of variables in a table *)
    LARGEDIM       = 58; // 40; (* 39 *) (* the maximal number of variables in a screening *)
    VECTORLENGTH   = 59; // 41; (* 40 *) (* MUST BE LARGER THAN LARGEDIM *)
    MAXVERT        = 58; // 40;  (* The maximal number of vertices *)
    MAXCOMMENTS    = 10;  (* maximal number of comments *)

    DATALENGTH     = 700;  (*600 - 400 - 255*)

    (*---------------------------*)
    (* Constants defining tables *)
    (*---------------------------*)

    VARTOP    = 40;//98; //40 //32; //15;      (** VARTOP+1 = number of categories incl. 'total' **)

    MAXDIM    = 40; //98;  //40; //32; //15;  (** MAXIMUM NUMBER OF CATEGORIES PER VARIABLE **)
    DIMTOP    = 100; //41; //99;  //41 //33; //16; (** MAXDIM+1 - FOR DEFINITION OF TABLES WITH MARGINALS **)

    LongMaxDim = 75 ; //50;
    LongDimTop = 76;  //51;

    DIMZ_MAX = 100;

    (*-------------------------------------------------*)
    (* constants defining graphs and cliques.          *)
    (*-------------------------------------------------*)


    MAXKLIKEANT    =  375; (* maximal number of cliques which will be handled *)
    DOUBLEKLIKEANT =  750; (* must be equal to 2*MAXKLIKEQANT *)

    MAXGENERATORANT = 256;  (* MUST BE EQUAL TO 2**MAXVAR *)


    MAXMARGINALS = 250;
    MAXINTERACTIONS = 250;   (*300*)
    MAXNLEVELS = 20;
    MAXVEJE =150;  (*75*)
    MAXCUTS =400;  (*800*)
    MAXNVERT=500;
    MAXSEP=8;

    MAXHYP = 200; (*168;*)

    (*--------------------------------------*)
    (* Constants connected with screening   *)
    (*--------------------------------------*)

    NINTERACTIONS = 300; (* the maximal number of interactions *)

    RESULT3_BYTES_MAX = 3000;  (* The maximal number of summary results *)
    RESULT3_REALS_MAX = 3000;   (* which will be read from file          *)

    (*---------------------------------------*)
    (* Constants connected with special 2-way*)
    (* tables                                *)
    (*---------------------------------------*)

    MAXC       = Maxdim; //9;
    TOPC       = Dimtop; //10;
    MAXR       = 174; //50; (*40*)
    TOPR       = 175; //51; (*41*)
    Maxcollaps = round((topc*(topc-1))/2+(maxr*(maxr-1))/2);
    //MAXCOLLAPS = 15087;
                       (* the maximal number of collapses *)
                       (* 9*10/2 + 174*173/2 = 15087*)

    // constants for model search

    LMAXHYP = 1590; (* 1590 hvis largedim = 39 *)
    MAXHIST = 25;   (* 25 -------------------  *)

    MAXITEMS   = 40;
    MaxItemsqr = 1600;
    ITEM_TOPSCORE = 7;
    TOPSCORE = 280;   (* MAXITEMS * ITEM_TOPSCORE *)

    Longvectorlength = 70000;

TYPE
    STRING3        = STRING(.3.);
    STRING8        = STRING(.8.);
    STRING15       = STRING(.20.);
    STRING60       = STRING(.60.);
    STRING80       = STRING(.120.);

    STRING60S      = ARRAY(.1..MAXCOMMENTS.) OF STRING60;
    CATEGORY_ARRAY = ARRAY(.1..DIMTOP.) OF STRING8;

    DOUBLE500      = ARRAY(.0..500.) OF DOUBLE;
    PTR_DOUBLE500  = ^DOUBLE500;

    IVECTOR        = ARRAY(.0..VECTORLENGTH.) OF INTEGER;
    LVECTOR        = ARRAY(.0..VECTORLENGTH.) OF LONGINT;
    RVECTOR        = ARRAY(.0..VECTORLENGTH.) OF REAL;
    EVECTOR        = ARRAY(.0..VECTORLENGTH.) OF EXTENDED;
    BVECTOR        = ARRAY(.0..VECTORLENGTH.) OF BYTE;
    CVECTOR        = ARRAY(.0..VECTORLENGTH.) OF CHAR;
    BOOL_VECTOR    = ARRAY(.0..VECTORLENGTH.) OF BOOLEAN;
    Color_VECTOR    = ARRAY(.0..VECTORLENGTH.) OF Tcolor;

    INTEGER_DATAVECTOR = ARRAY(.1..DATALENGTH.) OF INTEGER;
    DATAVECTOR         = ARRAY(.1..DATALENGTH.) OF REAL;

    IDOL_VEKTOR        = ARRAY(.0..DATALENGTH.) OF LONGINT;
    IDOL_GRAENSER      = ARRAY(.0..DATALENGTH,1..2.) OF LONGINT;

    bool_matrix    = array(.1..largedim,1..largedim.) of boolean;

    BMATRIX        = array(.1..largedim,1..largedim.) of byte;
    BMATRIX_PTR    = ^BMATRIX;
    SMATRIX        = ARRAY(.1..LARGEDIM,1..LARGEDIM.) OF SINGLE;
    SMATRIX_PTR    = ^SMATRIX;
    RMATRIX        = ARRAY(.1..LARGEDIM,1..LARGEDIM.) OF REAL;
    RMATRIX_PTR    = ^RMATRIX;
    RMATRIX0        = ARRAY(.0..LARGEDIM,0..LARGEDIM.) OF REAL;
    RMATRIX0_PTR   = ^RMATRIX0;

    TILMATRIX      = ARRAY(.1..LARGEDIM,0..MAXDIM.) OF REAL;
    TILMATRIX_PTR   = ^TILMATRIX;

    IMATRIX        = ARRAY(.1..LARGEDIM,1..LARGEDIM.) OF INTEGER;
    IMATRIX_PTR    = ^IMATRIX;
    IMATRIX0       = ARRAY(.0..VECTORLENGTH,0..VECTORLENGTH.) OF INTEGER;
    IMATRIX0_PTR   = ^IMATRIX0;

    LMATRIX        = ARRAY(.1..LARGEDIM,1..LARGEDIM.) OF INTEGER;
    LMATRIX_PTR    = ^IMATRIX;
    LMATRIX0       = ARRAY(.0..VECTORLENGTH,0..VECTORLENGTH.) OF INTEGER;
    LMATRIX0_PTR   = ^IMATRIX0;

    EMATRIX        = ARRAY(.0..LARGEDIM,0..LARGEDIM.) OF EXTENDED;
    EMATRIX_PTR    = ^EMATRIX;

    RCube        = ARRAY(.1..LARGEDIM,1..LARGEDIM,1..LARGEDIM.) OF REAL;

    MaxdimBvector = array(.1..maxdim.) of byte;
    MaxdimBmatrix = array(.1..maxdim,1..maxdim.) of Byte;
    MaxdimRvector = array(.1..maxdim.) of real; 
    MaxdimRmatrix = array(.1..maxdim,1..maxdim.) of real;
    MaxdimBoolVector = array(.1..maxdim.) of boolean;

    Penstyle_matrix = array(.1..vectorlength,1..vectorlength.) of Tpenstyle;
                  // psSolid,psDash,psDot,psDashDot,psDasDotDot


    BOX_CHAR_TYPE  = ARRAY(.1..3,1..3.) OF CHAR;
    STOP_CHAR_TYPE = ARRAY(.1..4.) OF CHAR;

    TABRECORD      = RECORD
                       CELL : BVECTOR;
                       COUNT: LONGINT;
                     END;

    (*------------------------*)
    (* types used for windows *)
    (*------------------------*)

    WTYPE          = ARRAY(.1..MAXWINDOWS,1..6.) OF BYTE;

    VNAME_ARRAY    = ARRAY(.1..LARGEDIM.) OF STRING8;
    VNAME_PTR      = ^VNAME_ARRAY;
    TAB_VNAME_ARRAY    = ARRAY(.1..MAXVAR.) OF STRING8;
    TAB_VNAME_PTR      = ^TAB_VNAME_ARRAY;
    CNAME_PTR      = ^VNAME_ARRAY;

    (*----------------------------------*)
    (* These types are used by EXAunit1 *)
    (* for arrays containing results    *)
    (* from tests of ZPA hypotheses     *)
    (*----------------------------------*)

    (* CHANGED TO SINGLE INSTEAD OF REALS *)

    RESARRAY =  ARRAY(.1..MAXHYP,1..9.) OF real;   // no. 9 contains NAbsGamma/Nsim
    LRESARRAY = ARRAY(.1..MAXVAR,1..MAXDIM,1..11.) OF SINGLE;
    B_LRESARRAY = ARRAY(.1..largedim,1..MAXDIM,1..11.) OF SINGLE;

    (* local testresults contains the following information :
              8 : PPQ
              9 : PMQ
             10 : S
             11 : NAbsGamma/Nsim

    **********************************************************)


    Ttestresult = Record
             value : single;
             df    : byte;
             p     : single;
    end;

    Ttestresults = array(.0..Maxhyp.) of Ttestresult;

    (*--------------------------------------------------------*)
    (* Tables are now defined by LONGINT and REAL_DATA_ARRAYs *)
    (*--------------------------------------------------------*)


    ZTABTYPE = ARRAY(.0..1,0..1,0..1,0..1,0..1,0..1,0..1,0..1.) OF INTEGER;

    (*------------------------------------------------*)
    (* 2- and 3-way tables for statistical routines   *)
    (*------------------------------------------------*)

    LongTwowayTableArray = Array(.1..DimTop,1..LongDimTop.) of longint;
   LongETWOWAYTABLEarray = ARRAY(.1..dimtop,1..LongDIMTOP.) OF Double; (* EXPECTED VALUES *)

    TWOWAYTABLEarray = ARRAY(.1..DIMTOP,1..DIMTOP.) OF LONGINT;
   ETWOWAYTABLEarray = ARRAY(.1..DIMTOP,1..DIMTOP.) OF Double; (* EXPECTED VALUES *)
  THREEWAYTABLEarray = ARRAY(.1..DIMTOP,1..DIMTOP,1..DIMTOP.) OF LONGINT;
  THREEWAY_PTR  = ^THREEWAYTABLEarray;
 ETHREEWAYTABLEarray = ARRAY(.1..DIMTOP,1..DIMTOP,1..DIMTOP.) OF double;
 ETHREEWAY_PTR  = ^ETHREEWAYTABLEarray;

    TWOWAY_ARRAY  = ARRAY(.1..TOPC,1..TOPR.) OF LONGINT;
    ETWOWAY_ARRAY = ARRAY(.1..TOPC,1..TOPR.) OF REAL;
    KOMBI_ARRAY   = ARRAY(.1..TOPR,2..MAXVAR.) OF BYTE;


    (*-----------------------------------*)
    (* Types defining graphs and cliques *)
    (*-----------------------------------*)

    GRAPH_MATRIX   = ARRAY(.1..MAXVERT,1..MAXVERT.) OF BYTE;
    GRAPH_MATRIX_PTR = ^GRAPH_MATRIX;
    I_GRAPH_MATRIX = ARRAY(.1..MAXVERT,1..MAXVERT.) OF INTEGER;
    I_GRAPH_MATRIX_PTR = ^I_GRAPH_MATRIX;

    CLIQUE_ARRAY   = ARRAY(.1..MAXKLIKEANT,0..MAXVERT.) OF BYTE;
    CLIQUE_PTR     = ^CLIQUE_ARRAY;
    Clique_indicators = array(.1..MaxKlikeant.) of boolean;

    MARGIN_ARRAY   = ARRAY(.1..326,0..MAXVERT.) OF BYTE;

    DOUBLE_C_ARRAY = ARRAY(.1..DOUBLEKLIKEANT,0..MAXVERT.) OF INTEGER;
    DOUBLE_C_PTR   = ^DOUBLE_C_ARRAY;

    (*-------------------------------------------------*)
    (* Types defining generators for log-linear models *)
    (* for marginal tables with up to MAXVAR variables *)
    (*-------------------------------------------------*)

    GENERATOR_ARRAY = ARRAY(.1..MAXGENERATORANT,1..MAXVAR.) OF BYTE;

    (*------------------------------------*)
    (* types connected with hypotheses    *)
    (*------------------------------------*)

    VAR_PAIRS = ARRAY(.1..MAXINTERACTIONS,1..2.) OF BYTE;
    HYPARRAY  = ARRAY(.1..MAXHYP,0..LARGEDIM.) OF BYTE;
    WMARGARRAY= ARRAY(.1..MAXMARGINALS,1..LARGEDIM.) OF BYTE;
    WMARGLIMITS=ARRAY(.1..MAXINTERACTIONS.) OF INTEGER;
    SMARGARRAY= ARRAY(.1..MAXINTERACTIONS,1..LARGEDIM.) OF BYTE;
    PATHARRAY = ARRAY(.0..MAXVEJE,0..MAXVERT.) OF BYTE;
    BOOL      = ARRAY(.1..MAXVERT.) OF BOOLEAN;
    C_BYTES   = ARRAY(.1..MAXVERT,1..MAXNLEVELS.) OF BYTE;

    VERTvector = ARRAY(.1..MAXVERT.) OF BYTE;


    (*-------------------------*)
    (* Types for datamatrices  *)
    (*-------------------------*)

    BYTE_DATA_TYPE  = ARRAY(.1..MAX_NUMBER_OF_BYTES.) OF BYTE;
    BYTE_DATA       = ^BYTE_DATA_TYPE;
    BYTE_DATA_ARRAY = ARRAY(.1..MAX_DATA_ARRAYS.) OF BYTE_DATA;

    INTEGER_DATA_TYPE  = ARRAY(.1..MAX_NUMBER_OF_INTEGERS.) OF INTEGER;
    INTEGER_DATA       = ^INTEGER_DATA_TYPE;
    INTEGER_DATA_ARRAY = ARRAY(.1..MAX_DATA_ARRAYS.) OF INTEGER_DATA;

    LONGINT_DATA_TYPE  = ARRAY(.1..MAX_NUMBER_OF_LONGINT.) OF INTEGER;
    LONGINT_DATA       = ^LONGINT_DATA_TYPE;
    LONGINT_DATA_ARRAY = ARRAY(.1..MAX_DATA_ARRAYS.) OF LONGINT_DATA;

    LONGINT0_DATA_TYPE  = ARRAY(.0..MAX_NUMBER_OF_LONGINT.) OF LONGINT;
    LONGINT0_DATA       = ^LONGINT0_DATA_TYPE;
    LONGINT0_DATA_ARRAY = ARRAY(.1..MAX_DATA_ARRAYS.) OF LONGINT0_DATA;

    INTEGER0_DATA_TYPE  = ARRAY(.0..MAX_NUMBER_OF_INTEGERS.) OF INTEGER;
    INTEGER0_DATA       = ^INTEGER0_DATA_TYPE;
    INTEGER0_DATA_ARRAY = ARRAY(.1..MAX_DATA_ARRAYS.) OF INTEGER0_DATA;

    REAL_DATA_TYPE  = ARRAY(.0..MAX_NUMBER_OF_REALS.) OF REAL;
    REAL_DATA       = ^REAL_DATA_TYPE;
    REAL_DATA_ARRAY = ARRAY(.1..MAX_DATA_ARRAYS.) OF REAL_DATA;

    Extended_DATA_TYPE  = ARRAY(.0..MAX_NUMBER_OF_extended.) OF extended;
    Extended_DATA       = ^extended_DATA_TYPE;
    Extended_DATA_ARRAY = ARRAY(.1..MAX_DATA_ARRAYS.) OF extended_DATA;

    (*--------------------------------*)
    (* Types connected with screening *)
    (*--------------------------------*)

    MARGTAB_TYPE1 = ARRAY(.1..LARGEDIM,0..DIMTOP.) OF INTEGER;
    REAL3         = ARRAY(.1..3.) OF REAL;
    INTEGER3      = ARRAY(.1..3.) OF INTEGER;
    BYTE3         = ARRAY(.1..3.) OF BYTE;
    BOOLEAN3      = ARRAY(.1..3.) OF BOOLEAN;


    BYTES_FOR_RESULTS3_ARRAY  = ARRAY(.1..RESULT3_BYTES_MAX,1..5.) OF BYTE;
    REALS_FOR_RESULTS3_ARRAY  = ARRAY(.1..RESULT3_REALS_MAX,1..4.) OF SINGLE;
    BYTES_FOR_RESULTS3  = ^BYTES_FOR_RESULTS3_ARRAY;
    REALS_FOR_RESULTS3  = ^REALS_FOR_RESULTS3_ARRAY;

    (*+++++++++++++++++*)
    (* types for plots *)
    (*+++++++++++++++++*)

    COORDINATE_ARRAY = ARRAY(.1..LARGEDIM,1..2.) OF INTEGER;

    (*+++++++++++++++++++++++++*)
    (* types for item analysis *)
    (*+++++++++++++++++++++++++*)

    PARAMETER_ARRAY = ARRAY(.1..LARGEDIM,0..MAXDIM.) OF REAL;
    ITEM_PARAMETERS = ^PARAMETER_ARRAY;

    ITEM_MARGIN_ARRAY   = ARRAY(.1..LARGEDIM,-1..LARGEDIM.) OF INTEGER;
    ITEM_MARGINALS      = ^ITEM_MARGIN_ARRAY;

    // types for model search

       LRES_ARRAY = ARRAY(.1..LMAXHYP,1..8.)        OF SINGLE;
       LHYP_ARRAY = ARRAY(.1..LMAXHYP,0..LARGEDIM.) OF BYTE;

       LRES_PTR = ^LRES_ARRAY;
       LHYP_PTR = ^LHYP_ARRAY;

       HYPBOOL_ARRAY = ARRAY(.1..MAXHYP.) OF BOOLEAN;

       HISTORY_ARRAY = ARRAY(.0..MAXHIST,1..LARGEDIM,1..LARGEDIM.) OF BYTE;
       HISTORY_PTR   =^HISTORY_ARRAY;

    // types for permutations of up to 8 numbers

       Tpermutations = Array(.1..20160,1..8.) of byte;
       Perm_ptr      = ^Tpermutations;

       Tperm_ppqpmq    = Array(.1..20160,1..2.) of real;
       perm_ppqpmq_ptr     = ^Tperm_ppqpmq;

       LongIvector        = Array(.0..Longvectorlength+1.) of int64;
       PTR_LongIvector    = ^LongIvector;
       LongRvector        = Array(.0..Longvectorlength+1.) of real;
       PTR_LongRvector    = ^LongRvector;
       LongCvector        = Array(.0..Longvectorlength+1.) of char;
       PTR_LongCvector    = ^LongCvector;

       StackedTrecord     = RECORD
                           x       : byte;
                           y       : byte;
                           Stacknr : int64;
                           COUNT   : LONGINT;
                          END;

       STvector          = array(.0..Longvectorlength+1.) of StackedTrecord;
       PTR_STvector      = ^STvector;
       I64VECTOR        = ARRAY(.0..VECTORLENGTH.) OF int64;

VAR
       EmptyTwoWaytable : TWOwayTableArray;

       zerointegers          : Ivector;
       zerobytes,onebytes    : Bvector;

       c_i,given    : char;

       i,j            : byte;

IMPLEMENTATION

BEGIN
    c_i := '&';
    GIVEN := '|';
    for i:=0 to vectorlength do
    begin
        zerobytes(.i.):=0;
        onebytes(.i.):=1;
        zerointegers(.i.):=0;
    end;

    For i:=1 to dimtop do
    for j:=1 to dimtop do EmptyTwowayTable(.i,j.):=0;

END. (* OF TYPEUNIT *)

