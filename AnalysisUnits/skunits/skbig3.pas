{$O+,F+}
UNIT skbig3;

INTERFACE

Uses SKTYPES,SKVARS,DGRVARS,SKOUT,SKCMDS,SKrandom,
     SKDATA0,SKDATA1,SKdata4,SKdata5,
     SKTAB1,SKXYZ1,SKEXA1,SKstat,skqsort;

(* a new variable ZCELL : byte_data_array and ZCELL_ARRAYS    *)
(* has BEEN included in the VARS file. It will be initialized *)
(* and disposed during expensive_bigtest                      *)

// bigtest was used in DIGRAM1f but does not seem to be used anywhere
// cheap_bigtest is used in sksrch and in procedures in SKbig3
// (inexpensive_tests and very_inexpensive_tests)
// expensive_bigtest is used in DGRexe (Markov chains) and SKdes3

// inexpensive amd very_inexpensive tests have been replaced by quick
// and very_quick tests. The old tests are still here and still - as comments -
// in the c0des in DIGRAM1f, GRAPH1f and  SearchD1

Type
    TwoBytes = Array(.1..2.) of byte;

VAR Nxyz : LONGINT;
    i,j  : byte;
    bzeros : bvector;
    tab2zeros : Twowaytablearray;

    checkchar : string;

function less_XYZ_tabcell(x,y : TabRecord): Boolean;

function equal_XYZ_tabcells(x,y : TabRecord): Boolean;

// VX and VY are included in the following two routines

function less_XYZ_tabcell1(x,y : TabRecord): Boolean;

function equal_XYZ_tabcells1(x,y : TabRecord): Boolean;

Function XYZtabcell(N : integer;
                 Nvar           : Byte;
                 TABDATA        : BYTE_DATA_ARRAY;
                 TABDATA_ARRAYS : BYTE;
                 TABCOUNT       : LONGINT_DATA_ARRAY;
                 TABCOUNT_ARRAYS: BYTE;
                 NTABcells      : longint;
             var fejl: byte): TABrecord;

// this function finds TABdata cell no. N
// TABDATA is assumed to be ready - if not then fejl:=99
// If N>NTABcells then fejl:=199 else fejl i not changed
// Init_XYZ_TABrecords shouls be used before TABcell is called

Function XYZtabcell1(N : integer;
                 Nvar           : Byte;
                 TABDATA        : BYTE_DATA_ARRAY;
                 TABDATA_ARRAYS : BYTE;
                 TABCOUNT       : LONGINT_DATA_ARRAY;
                 TABCOUNT_ARRAYS: BYTE;
                 NTABcells      : longint;
             var fejl: byte): TABrecord;

// Like XYZtabcell but faster and without check for errors

procedure Init_XYZ_TABRECORDs(var fra,til,fejl : byte; var valid : boolean);

procedure Sort_XYZ_records(NTABcells : longint;
                           Nvar : byte;
                           TABdata : Byte_data_array;
                           TABdata_arrays : Byte;
                           TABcount : Longint_data_array;
                           TABcount_arrays : byte;
                       var Nxyz : longint;
                       var XYZdata        : byte_data_array;
                       var XYZdata_arrays : byte;
                       var XYZcount       : Longint_data_array;
                       var XYZcount_arrays : byte;
                       var XYZdata_ready : boolean);

// this procedure sorts a TABdata set according to the controlvariables defining Z in an XYZ-table

procedure QSort_XYZ_records(NTABcells : longint;
                           Nvar : byte;
                           TABdata : Byte_data_array;
                           TABdata_arrays : Byte;
                           TABcount : Longint_data_array;
                           TABcount_arrays : byte;
                       var Nxyz : longint;
                       var XYZdata        : byte_data_array;
                       var XYZdata_arrays : byte;
                       var XYZcount       : Longint_data_array;
                       var XYZcount_arrays : byte;
                       var XYZdata_ready : boolean);

// this procedure quicksorts a TABdata set according to the controlvariables defining Z in an XYZ-table

procedure QuickSort_XYZ_records(NTABcells : longint;
                           Nvar : byte;
                           TABdata : Byte_data_array;
                           TABdata_arrays : Byte;
                           TABcount : Longint_data_array;
                           TABcount_arrays : byte;
                       var Nxyz : longint;
                       var XYZdata        : byte_data_array;
                       var XYZdata_arrays : byte;
                       var XYZcount       : Longint_data_array;
                       var XYZcount_arrays : byte;
                       var XYZdata_ready : boolean;
                       var inrec         : ptr_stvector;
                       var outrec        : ptr_stvector;
                       var sortsuccess   : boolean);

// this procedure quicksorts a TABdata set according to the controlvariables defining Z in an XYZ-table

// inrec and outrec are used for sorting. We assume that they have been allocated before this procedure
// is invoked

Function XYZ_TabCell(N       : longint;
                     Nvar           : Byte;
                     TABDATA_READY  : BOOLEAN;
                     TABDATA        : BYTE_DATA_ARRAY;
                     TABDATA_ARRAYS : BYTE;
                     TABCOUNT       : LONGINT_DATA_ARRAY;
                     TABCOUNT_ARRAYS: BYTE;
                     NTABCELLS      : LONGINT;
                     FRA,TIL : byte;
                 var valid   : boolean): TABrecord;

// Init_XYZ_tabrecords must be executed before this procedure is used
// This is the same as XYZtabcell but XYZ_TABcell.cell(.v.)=0
// if To_be_used_in_XYZ(.v.)= false

procedure create_XYZ_records(var outfile : textfile;
                                 Nvar           : byte;
                                 TABDATA_READY  : BOOLEAN;
                                 TABDATA        : BYTE_DATA_ARRAY;
                                 TABDATA_ARRAYS : BYTE;
                                 TABCOUNT       : LONGINT_DATA_ARRAY;
                                 TABCOUNT_ARRAYS: BYTE;
                                 NTABCELLS      : LONGINT;
                                 include_cells : boolean;
                             var XYZ_ready : boolean);

//create_XYZ_cells creates both records and cells

function bigtest_ready(HYPOTHESIS_EXIST : BOOLEAN;
                        NHYP             : INTEGER;
                        HYPOTESE         : HYPARRAY;
                        HYPNR            : INTEGER): boolean;

procedure define_control_variables(Nvar        : byte;
                                   DIM         : Bvector;
                                   HYPOTESE    : HYPARRAY;
                                   HYPNR       : INTEGER;
                                   var XY      : TwoBytes;
                                   var success : boolean);

(*******************************************************************************
This procedure defines the followin global variables (in SKVARS) associated
with XYZ-tables:

      VX and VY
      DIMX and DIMY

      Control_variables
      Control_limits

********************************************************************************)
procedure define_control_variables1(var outfile : textfile;
                                    Nvar        : byte;
                                    DIM         : Bvector;
                                    HYPOTESE    : HYPARRAY;
                                    HYPNR       : INTEGER;
                                    var XY      : TwoBytes;
                                    var success : boolean);


PROCEDURE BIGTEST(var   Outfile        : textfile;
                        TABFILE_EXISTS : BOOLEAN;
                   VAR  TABFILE        : TEXTfile;
                        TABDATA_READY  : BOOLEAN;
                        TABDATA_POSSIBLE : BOOLEAN;
                        TABDATA        : BYTE_DATA_ARRAY;
                        TABDATA_ARRAYS : BYTE;
                        TABCOUNT       : LONGINT_DATA_ARRAY;
                        TABCOUNT_ARRAYS: BYTE;
                        NTABCELLS      : LONGINT;
                        NVAR         : BYTE;
                        DIM          : BVECTOR;
                        VTYPE        : BVECTOR;
                        VLABEL       : CVECTOR;

                        HYPOTHESIS_EXIST : BOOLEAN;
                        NHYP             : INTEGER;
                   VAR  HYPOTESE         : HYPARRAY;
                        HYPNR            : INTEGER;

                        EXACT            : BOOLEAN;
                        NSIM             : INTEGER;
                        SIMSTART         : INTEGER;
                        SEQUENTIAL    : BOOLEAN;
                        SEQ_P0        : REAL;
                        SEQ_ALPHA     : REAL;
                        SEQ_B         : REAL;

                   VAR  RESULTS          : RESARRAY;
                   VAR  SUCCESS    : BOOLEAN);

PROCEDURE CHEAP_BIGTEST(var outfile    : textfile;
                        TABFILE_EXISTS : BOOLEAN;
                   VAR  TABFILE        : TEXTfile;
                        TABDATA_READY  : BOOLEAN;
                        TABDATA_POSSIBLE : BOOLEAN;
                        TABDATA        : BYTE_DATA_ARRAY;
                        TABDATA_ARRAYS : BYTE;
                        TABCOUNT       : LONGINT_DATA_ARRAY;
                        TABCOUNT_ARRAYS: BYTE;
                        NTABCELLS      : LONGINT;
                        NVAR         : BYTE;
                        DIM          : BVECTOR;
                        VTYPE        : BVECTOR;
                        VLABEL       : CVECTOR;

                        HYPOTHESIS_EXIST : BOOLEAN;
                        NHYP             : INTEGER;
                   VAR  HYPOTESE         : HYPARRAY;
                        HYPNR            : INTEGER;

                        EXACT            : BOOLEAN;
                        NSIM             : INTEGER;
                        SIMSTART         : INTEGER;
                        SEQUENTIAL    : BOOLEAN;
                        SEQ_P0        : REAL;
                        SEQ_ALPHA     : REAL;
                        SEQ_B         : REAL;
                        EXACT_P_LEVEL    : REAL;

                   VAR  RESULTS          : RESARRAY;
                   VAR  SUCCESS    : BOOLEAN);

//EXA summary will only be printed if NYPNR=NHYP


PROCEDURE Pseudogamma_BIGTEST(var outfile    : textfile;
                        method         : byte;
                        permute_x      : boolean;
                        restrict_x     : boolean;
                        xrestrictions  : Bmatrix;
                        permute_y      : boolean;
                        restrict_y     : boolean;
                        yrestrictions  : Bmatrix;
                        use_x,use_y : boolean;
                        xcoding,ycoding : bvector;
                        TABFILE_EXISTS : BOOLEAN;
                   VAR  TABFILE        : TEXTfile;
                        TABDATA_READY  : BOOLEAN;
                        TABDATA_POSSIBLE : BOOLEAN;
                        TABDATA        : BYTE_DATA_ARRAY;
                        TABDATA_ARRAYS : BYTE;
                        TABCOUNT       : LONGINT_DATA_ARRAY;
                        TABCOUNT_ARRAYS: BYTE;
                        NTABCELLS      : LONGINT;
                        NVAR         : BYTE;
                        DIM          : BVECTOR;
                        VTYPE        : BVECTOR;
                        VLABEL       : CVECTOR;

                        HYPOTHESIS_EXIST : BOOLEAN;
                        NHYP             : INTEGER;
                   VAR  HYPOTESE         : HYPARRAY;
                        HYPNR            : INTEGER;

                        EXACT            : BOOLEAN;
                        NSIM             : INTEGER;
                        SIMSTART         : INTEGER;
                        SEQUENTIAL    : BOOLEAN;
                        SEQ_P0        : REAL;
                        SEQ_ALPHA     : REAL;
                        SEQ_B         : REAL;
                        EXACT_P_LEVEL    : REAL;

                   VAR  RESULTS          : RESARRAY;
                   VAR  SUCCESS          : BOOLEAN;
                   var  permuted_gamma_ready : boolean;
                   var  permuted_gamma : real;
                   var permuted_gamma_p : real;
                   var xorder           : bvector;
                   var yorder           : bvector;
                       PrintOutput      : boolean);

// this procedure creates the XYZ table and
// calculates the maximum gamma coefficient over permutations of nominal variables

PROCEDURE Pairwise_comparisons(var outfile    : textfile;
                        permute_x      : boolean;
                        restrict_x     : boolean;
                        xrestrictions  : Bmatrix;
                        permute_y      : boolean;
                        restrict_y     : boolean;
                        yrestrictions  : Bmatrix;
                        TABFILE_EXISTS : BOOLEAN;
                   VAR  TABFILE        : TEXTfile;
                        TABDATA_READY  : BOOLEAN;
                        TABDATA_POSSIBLE : BOOLEAN;
                        TABDATA        : BYTE_DATA_ARRAY;
                        TABDATA_ARRAYS : BYTE;
                        TABCOUNT       : LONGINT_DATA_ARRAY;
                        TABCOUNT_ARRAYS: BYTE;
                        NTABCELLS      : LONGINT;
                        NVAR         : BYTE;
                        DIM          : BVECTOR;
                        VTYPE        : BVECTOR;
                        VLABEL       : CVECTOR;

                        HYPOTHESIS_EXIST : BOOLEAN;
                        NHYP             : INTEGER;
                   VAR  HYPOTESE         : HYPARRAY;
                        HYPNR            : INTEGER;

                        //EXACT            : BOOLEAN;
                        //NSIM             : INTEGER;
                        //SIMSTART         : INTEGER;
                        //SEQUENTIAL    : BOOLEAN;
                        //SEQ_P0        : REAL;
                        //SEQ_ALPHA     : REAL;
                        //SEQ_B         : REAL;
                        //EXACT_P_LEVEL    : REAL;

                   VAR  RESULTS          : RESARRAY;
                   VAR  SUCCESS    : BOOLEAN;
                   var permuted_gamma_ready : boolean;
                   var permuted_gamma :real;
                   var permuted_gamma_p : real;
                   var xorder           : bvector;
                   var yorder           : bvector;
                       printOutput      : boolean);

//EXA summary will only be printed if NYPNR=NHYP

// this procedure calculates the maximum gamma coefficient over permutations of nominal variables

FUNCTION BIG_GAMMA(var outfile    : textfile;
                   TABFILE_EXISTS : BOOLEAN;
              VAR  TABFILE        : TEXT;
                   TABDATA_READY  : BOOLEAN;
                   TABDATA_POSSIBLE : BOOLEAN;
                   TABDATA        : BYTE_DATA_ARRAY;
                   TABDATA_ARRAYS : BYTE;
                   TABCOUNT       : LONGINT_DATA_ARRAY;
                   TABCOUNT_ARRAYS: BYTE;
                   NTABCELLS      : INTEGER;
                   NVAR         : BYTE;
                   DIM          : BVECTOR;
                   VTYPE        : BVECTOR;
                   VLABEL       : CVECTOR;

                   HYPOTHESIS_EXIST : BOOLEAN;
                   NHYP             : INTEGER;
              VAR  HYPOTESE         : HYPARRAY;
                   HYPNR            : INTEGER;

              VAR  SUCCESS    : BOOLEAN) : REAL;


PROCEDURE EXPENSIVE_BIGTEST(var outfile : textfile;
                        TABFILE_EXISTS : BOOLEAN;
                   VAR  TABFILE            : TEXT;
                        TABDATA_READY      : BOOLEAN;
                        TABDATA_POSSIBLE   : BOOLEAN;
                        TABDATA            : BYTE_DATA_ARRAY;
                        TABDATA_ARRAYS     : BYTE;
                        TABCOUNT           : LONGINT_DATA_ARRAY;
                        TABCOUNT_ARRAYS    : BYTE;
                        NTABCELLS          : LONGINT;
                        NVAR               : BYTE;
                        DIM                : BVECTOR;
                        VTYPE              : BVECTOR;
                        VLABEL             : CVECTOR;
                        VNAMES             : VNAME_PTR;
                        VNAMES_EXIST       : BOOL_VECTOR;

                        HYPOTHESIS_EXIST   : BOOLEAN;
                        NHYP               : INTEGER;
                   VAR  HYPOTESE           : HYPARRAY;
                        HYPNR              : INTEGER;

                        EXACT              : BOOLEAN;
                        NSIM               : INTEGER;
                        SIMSTART           : INTEGER;
                        SEQUENTIAL         : BOOLEAN;
                        SEQ_P0             : REAL;
                        SEQ_ALPHA          : REAL;
                        SEQ_B              : REAL;

                   VAR  RESULTS            : RESARRAY;
                   VAR  SUCCESS            : BOOLEAN);


procedure inexpensive_tests(var outfile          : textfile;
                                nhyp             : integer;
                            var hypotese         : hyparray;
                                hypotheses_exist : boolean;
                            var results          : resarray;
                                print_results    : boolean;
                            var success          : boolean);

// this procedure tests a set of already defined hypotheses
// the procedure is a partial copy of EXAstep in SKsrch0

procedure quick_tests(var outfile          : textfile;
                          nhyp             : integer;
                      var hypotese         : hyparray;
                          hypotheses_exist : boolean;
                      var results          : resarray;
                          print_results    : boolean;
                      var success          : boolean);

procedure pseudogamma_tests(var outfile        : textfile;
                          method               : byte; //1 permute  2 = pairwise  3 = both
                          Permute_x        : boolean;
                          restrict_x     : boolean;
                          xrestrictions  : Bmatrix;
                          permute_y        : boolean;
                          restrict_y     : boolean;
                          yrestrictions  : Bmatrix;
                          nhyp             : integer;
                      var hypotese         : hyparray;
                          hypotheses_exist : boolean;
                      var results          : resarray;
                          print_results    : boolean;
                      var success          : boolean;
                      var permuted_gamma_ready : boolean;
                      var permuted_gamma   : real;
                      var permuted_gamma_p : real;
                      var xorder           : bvector;
                      var yorder           : bvector;
                          PrintOutput      : boolean);

// This procedure calculates the max gamma coefficients over all permutations of a nominal variable

procedure pairwise_tests(var outfile        : textfile;
                          Permute_x        : boolean;
                          restrict_x     : boolean;
                          xrestrictions  : Bmatrix;
                          permute_y        : boolean;
                          restrict_y     : boolean;
                          yrestrictions  : Bmatrix;
                          nhyp             : integer;
                      var hypotese         : hyparray;
                          hypotheses_exist : boolean;
                      var results          : resarray;
                          print_results    : boolean;
                      var success          : boolean;
                      var permuted_gamma_ready : boolean;
                      var permuted_gamma   : real;
                      var permuted_gamma_p : real;
                      var xorder           : bvector;
                      var yorder           : bvector;
                          Printoutput      : boolean);

// This procedure compares all pairs of categories for nominal variables

Procedure collaps_tests(var outfile        : textfile;
                            Permute_x        : boolean;
                            restrict_x     : boolean;
                            xrestrictions  : Bmatrix;
                            permute_y        : boolean;
                            restrict_y     : boolean;
                            yrestrictions  : Bmatrix;
                            nhyp             : integer;
                        var hypotese         : hyparray;
                            hypotheses_exist : boolean;
                        var results          : resarray;
                            print_results    : boolean;
                        var success          : boolean);


Procedure XYZ_MH(var outfile     : textfile;
                 TABFILE_EXISTS : BOOLEAN;
            VAR  TABFILE        : TEXTfile;
                 TABDATA_READY  : BOOLEAN;
                 TABDATA_POSSIBLE : BOOLEAN;
                 TABDATA        : BYTE_DATA_ARRAY;
                 TABDATA_ARRAYS : BYTE;
                 TABCOUNT       : LONGINT_DATA_ARRAY;
                 TABCOUNT_ARRAYS: BYTE;
                 NTABCELLS      : LONGINT;
                 HYPNR         : INTEGER;
                 HYPOTESE      : HYPARRAY);

(* CALCULATION OF mantel-Haenszel estimates and partial gamma coefficients FROM XYZ-TABLES *)


procedure Mantel_Haenszel(var outfile          : textfile;
                             nhyp             : integer;
                         var hypotese         : hyparray;
                             hypotheses_exist : boolean;
                         var results          : resarray;
                             print_results    : boolean;
                         var success          : boolean);

// this procedure is a copy of inexpensive tests that includes
// calculation of Mantel-Haenszel estimates for 2x2 relationships

procedure very_inexpensive_tests(var outfile     : textfile;
                                local            : boolean;
                                nhyp             : integer;
                            var hypotese         : hyparray;
                                hypotheses_exist : boolean;
                                parameters_unchanged : boolean;
                                recalculate      : boolean;
                                old_results_are_ready : boolean;
                            var old_nhyp         : integer;
                            var old_hypotese     : Lhyp_ptr;
                            var old_results      : Lres_ptr;
                            var results          : resarray;
                            var success          : boolean);

// this procedure tests a set of already defined hypotheses
// but first looks for results in old_hypotheses
// the procedure is a partial copy of EXAstep in SKsrch0

procedure very_quick_tests(var outfile     : textfile;
                               local            : boolean;
                               nhyp             : integer;
                           var hypotese         : hyparray;
                               hypotheses_exist : boolean;
                               parameters_unchanged : boolean;
                               recalculate      : boolean;
                               old_results_are_ready : boolean;
                           var old_nhyp         : integer;
                           var old_hypotese     : Lhyp_ptr;
                           var old_results      : Lres_ptr;
                           var results          : resarray;
                           var success          : boolean);

// this procedure tests a set of already defined hypotheses
// but first looks for results in old_hypotheses
// the procedure is a partial copy of EXAstep in SKsrch0

IMPLEMENTATION

Uses DIGRAM1f,SKmarkov,SKsrch0,SKreslts;

//----------------------------------------------------------

function less_XYZ_tabcell(x,y : TabRecord): Boolean;
label slut;
var i : byte;
begin

    less_XYZ_tabcell:=false;
    for i:=control_limits(.1.) to control_limits(.2.) do
    begin
        if control_variables(.i.) and (x.cell(.i.)<y.cell(.i.)) then
        begin
            less_XYZ_tabcell:=true;
            goto slut;
        end
        else
        if control_variables(.i.) and (x.cell(.i.)>y.cell(.i.)) then goto slut;
    end;
slut:
end;

//-----------------------------------------------------------

function equal_XYZ_tabcells(x,y : TabRecord): Boolean;
label slut;
var i : byte;
begin
    equal_XYZ_tabcells:=true;
    for i:=control_limits(.1.) to control_limits(.2.) do
    if control_variables(.i.) and (x.cell(.i.)<>y.cell(.i.)) then
    begin
        equal_XYZ_tabcells:=false;
        goto slut;
    end;
slut:
end;

//----------------------------------------------------------

function less_XYZ_tabcell1(x,y : TabRecord): Boolean;
label slut;
var i : byte;
    s : string;
begin

    //less_XYZ_tabcell1:=false;

    for i:=control_limits(.1.) to control_limits(.2.) do
    begin
        if control_variables(.i.) and (x.cell(.i.)<y.cell(.i.)) then
        begin
            less_XYZ_tabcell1:=true;
            goto slut;
        end
        else
        if control_variables(.i.) and (x.cell(.i.)>y.cell(.i.)) then
        begin
            less_XYZ_tabcell1:=false;
            goto slut;
        end
    end;

    // check vx

    if x.CELL(.vx.)<y.cell(.vx.) then
    begin
        less_xyz_tabcell1:=true;
        goto slut;
    end
    else
    if x.cell(.vx.)>y.cell(.vx.) then
    begin
        less_xyz_tabcell1:=false;
        goto slut;
    end;

    //checl vy

    if x.CELL(.vy.)<y.cell(.vy.) then
    begin
        less_xyz_tabcell1:=true;
        goto slut;
    end
    else
    //if x.CELL(.vy.)>y.cell(.vy.) then;
    begin
        less_xyz_tabcell1:=false;
        goto slut;
    end;

slut:
end;

//-----------------------------------------------------------

function equal_XYZ_tabcells1(x,y : TabRecord): Boolean;
label slut;
var i : byte;
begin
    //equal_XYZ_tabcells1:=true;
    for i:=control_limits(.1.) to control_limits(.2.) do
    if control_variables(.i.) and (x.cell(.i.)<>y.cell(.i.)) then
    begin
        equal_XYZ_tabcells1:=false;
        goto slut;
    end;

    // check vx and vy

    if (x.cell(.vx.)<>y.cell(.vx.))
    or (x.cell(.vy.)<>y.cell(.vy.))
    then equal_XYZ_tabcells1:=false
    else equal_XYZ_tabcells1:=true;

slut:
end;

//-------------------------------------------------------------

Function XYZtabcell;
label slut;
var TC    : tabrecord;
    v     : byte;
    valid : boolean;
begin
    TC.CELL  :=Bzeros;
    TC.count :=0;

    if not tabdata_ready then
    begin
        fejl:=99;
        goto slut;
    end;

    if n>ntabcells then
    begin
        fejl:=199;
        goto slut;
    end;

    for v:=1 to nvar do
    get_byte_data(nvar,tabdata,tabdata_arrays,N,v,TC.cell(.v.),valid);

    Locate_table_point(n,array_no,point);
    TC.count:=TABcount(.array_no.)^(.point.);
slut:
    XYZtabcell:=TC;
end;

//-------------------------------------------------------------

Function XYZtabcell1;
label slut;
var TC    : tabrecord;
    v,i   : byte;
    valid : boolean;
begin
    XYZtabcell1.CELL  :=Bzeros;
    XYZtabcell1.count :=0;

    get_byte_data1(nvar,TABdata,TABData_arrays,N,1,Array_no,point,i);
    XYZtabcell1.cell(.1.):=i;

    if point+nvar-1<=max_number_of_bytes then
    for v:=2 to nvar do
    begin
        inc(point);
        XYZtabcell1.cell(.v.):=TABdata(.array_no.)^(.point.);
    end
    else
    for v:=2 to nvar do
    begin
        get_byte_data1(nvar,tabdata,tabdata_arrays,N,v,array_no,point,i);
        XYZtabcell1.cell(.v.):=i;
    end;

    Locate_table_point(n,array_no,point);
    XYZtabcell1.count:=TABcount(.array_no.)^(.point.);

slut:
end;

//----------------------------------------------------------

procedure Init_XYZ_TABRECORDs(var fra,til,fejl : byte; var valid : boolean);
var i : byte;
begin
    Nxyz:=0;
    for i:=1 to largedim do to_be_used_in_XYZ(.i.):=false;

    to_be_used_in_XYZ(.vx.):=true;
    to_be_used_in_XYZ(.vy.):=true;

    for i:=1 to nvar do
    if control_variables(.i.) then to_be_used_in_XYZ(.i.):=true;

    if vx<control_limits(.1.) then fra:=vx else fra:=control_limits(.1.);
    if vy>control_limits(.2.) then til:=vy else til:=control_limits(.2.);

    fejl:=0;
    valid:=true;
end;

//------------------------------------------------------------------------

procedure Sort_XYZ_records(NTABcells : longint;
                           Nvar : byte;
                           TABdata : Byte_data_array;
                           TABdata_arrays : Byte;
                           TABcount : Longint_data_array;
                           TABcount_arrays : byte;
                       var Nxyz : longint;
                       var XYZdata        : byte_data_array;
                       var XYZdata_arrays : byte;
                       var XYZcount       : Longint_data_array;
                       var XYZcount_arrays : byte;
                       var XYZdata_ready : boolean);
label slut;
var cell,z    : longint;
    i,j,fejl  : byte;
    valid     : boolean;
    glT,nyT,T : Tabrecord;
    b0        : bvector;

    //...............................................

    procedure init_transfer;
    begin
        fillchar(glT.cell,SizeOf(glT.cell),0);
        glT.count:=0;
        T:=glT;
        Nxyz :=0;
        z := 0;
    end;

    //................................................

    procedure process_record;
    label vslut;
    var i : longint;
        w,v : bvector;

        procedure put;
        begin
            PushTABdataUp(z,nvar,xyzdata,xyzdata_arrays,xyzcount,xyzcount_arrays,Nxyz);
            defineTABdatacell1(z,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
        end;

     begin
        if Nxyz=0 then
        begin
            Nxyz:=1;
            defineTABdatacell1(Nxyz,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
            z:=Nxyz;
        end
        else
        if equal_xyz_tabcells(glT,nyT) then
        begin
            inc(z);
            put;
        end
        else
        if less_xyz_tabcell(nyT,glT) then
        begin
            // move down
            for i:=z-1 downto 1 do
            begin
                getTABdataCell1(XYZdata,XYZdata_arrays,i,Nvar,w);
                T.cell:=w;
                if not less_xyz_tabcell(nyT,T) then
                begin
                    z:=i+1;
                    put;
                    goto Vslut;
                end;
            end;
            z:=1;
            put;
            goto Vslut;
        end
        else
        begin
            // move up
            for i:=z+1 to Nxyz do
            begin
                getTABdataCell1(XYZdata,XYZdata_arrays,i,Nvar,w);
                T.cell:=w;
                if not less_xyz_tabcell(T,nyT) then
                begin
                    z:=i;
                    put;
                    goto Vslut;
                end;
            end;
            // We are at the ende. Therefore:

            inc(Nxyz);
            defineTABdatacell1(Nxyz,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
            z:=Nxyz;
        end;
    vslut:
    end;

    //.................................................

begin // main procedure

    // initialize the XYZdata

    //writeln(tmpfile,'*** sort start ',NTABcells,' records ***');

    init_tabdata(NTABcells,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays,XYZdata_ready);

    if not XYZdata_ready then okmessage('Warning: Initialization of temporary data files failed');

    init_transfer;

    for cell:=1 to NTABcells do
    begin
        nyT:=XYZtabcell1(cell,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,NTABcells,fejl);
        valid:=true;

        for i:=1 to nvar do
        if to_be_used_in_XYZ(.i.) and (nyT.cell(.i.)=0) then valid:=false;

        if valid then
        begin
            process_record;
            glT:=nyT;
        end;
    end;
slut:
   (******* test output ******
   writeln(TMPfile,'*** sort finished ',Nxyz,' records ***');
   write(TMPfile,'To be used :');
   for i:=1 to NVAR do write(TMPfile,to_be_used_in_XYZ(.i.));writeln(tmpfile);
   for cell:=1 to Nxyz do
   begin
        nyT:=XYZtabcell(cell,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays,Nxyz,fejl);
        for i:=1 to Nvar do write(TMPfile,nyT.cell(.i.),' ');writeln(TMPfile,nyT.count);
   end;
   (****************************)
end;

//------------------------------------------------------------------------

procedure QSort_XYZ_records(NTABcells : longint;
                           Nvar : byte;
                           TABdata : Byte_data_array;
                           TABdata_arrays : Byte;
                           TABcount : Longint_data_array;
                           TABcount_arrays : byte;
                       var Nxyz : longint;
                       var XYZdata        : byte_data_array;
                       var XYZdata_arrays : byte;
                       var XYZcount       : Longint_data_array;
                       var XYZcount_arrays : byte;
                       var XYZdata_ready : boolean);
label slut;
var cell,z,bund,top,midt       : longint;
    i,j,fejl                   : byte;
    valid                      : boolean;
    glT,nyT,T,Tbund,Ttop,Tmidt : Tabrecord;
    b0                         : bvector;

    //...............................................

    procedure init_transfer;
    begin
        fillchar(glT.cell,SizeOf(glT.cell),0);
        glT.count:=0;
        T:=glT;
        Nxyz :=0;
        z := 0;
    end;

    //................................................

    procedure process_record;
    label vslut;
    var i,j : longint;
        w,v : bvector;
        c   : string;

        procedure put;
        begin
            PushTABdataUp(z,nvar,xyzdata,xyzdata_arrays,xyzcount,xyzcount_arrays,Nxyz);
            defineTABdatacell1(z,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
        end;

        procedure add;
        begin
            AddToTabcell(z,nyT.count,XYZcount,XYZcount_arrays);
        end;

        procedure testoutput;
        var i : byte;
        begin
            write(tmpfile,z:6,vx:3,vy:3);
            write(tmpfile,' glT: ');
            for i:=1 to nvar do
            if to_be_used_in_xyz(.i.) then write(TMPfile,glT.cell(.i.):2);
            write(tmpfile,'    nyT: ');
            for i:=1 to nvar do
            if to_be_used_in_xyz(.i.) then write(TMPfile,nyT.cell(.i.):2);writeln(TMPfile);
        end;

     begin
        if Nxyz=0 then
        begin
            Nxyz:=1;
            defineTABdatacell1(Nxyz,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
            z:=Nxyz;
        end
        else
        if equal_xyz_tabcells1(glT,nyT) then
        begin
            //inc(z);
            //put;
            add;
            //writeln(tmpfile,'add to current ',z);
            goto Vslut;
        end
        else
        if less_xyz_tabcell1(nyT,glT) then
        begin
            // move down
            for i:=z-1 downto 1 do
            begin
                getTABdataCell1(XYZdata,XYZdata_arrays,i,Nvar,w);
                T.cell:=w;

                if equal_xyz_tabcells1(nyT,T) then
                begin
                    z:=i;
                    add;
                    //writeln(tmpfile,'add to record ',z);
                    goto Vslut;
                end
                else
                //if not less_xyz_tabcell1(nyT,T) then
                if less_xyz_tabcell1(T,nyT) then
                begin
                    z:=i+1;
                    put;
                    //write(tmpfile,'  put after ',z-1,': ');
                    //for j:=1 to nvar do
                    //if to_be_used_in_xyz(.j.) then write(TMPfile,T.cell(.j.):2);writeln(TMPfile);
                    goto Vslut;
                end;
            end;

            z:=1;
            put;
            //writeln(tmpfile,'put as ',z);
            goto Vslut;
        end
        else
        begin
            // move up
            c:=checkchar;
            for i:=z+1 to Nxyz do
            begin
                getTABdataCell1(XYZdata,XYZdata_arrays,i,Nvar,w);
                T.cell:=w;

                if equal_xyz_tabcells1(nyT,T) then
                begin
                    z:=i;
                    add;
                    //writeln(tmpfile,'add to current above',z);
                    goto Vslut;
                end
                else
                if less_xyz_tabcell1(nyT,T) then
                begin
                    z:=i;
                    put;
                    //write(tmpfile,'put before ',z+1,': ');
                    //for j:=1 to nvar do
                    //if to_be_used_in_xyz(.j.) then write(TMPfile,T.cell(.j.):2);writeln(TMPfile,'    ',c);
                    goto Vslut;
                end;
            end;

            // We are at the end. Therefore:

            inc(Nxyz);
            defineTABdatacell1(Nxyz,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
            z:=Nxyz;
            //writeln(tmpfile,'put at the end ',z);

        end;
    vslut:
        //testoutput;
    end;

    //................................................

    procedure Qprocess_record;
    label vslut,videre1,videre2;
    var i   : longint;
        w,v : bvector;
        s,s1 : string;

        procedure snxyz(s: string; var s1 : string);
        begin
            str(nxyz,s1);
            s1:=s+': '+s1;
            okmessage(s1);
        end;

        procedure put;
        begin
            PushTABdataUp(z,nvar,xyzdata,xyzdata_arrays,xyzcount,xyzcount_arrays,Nxyz);
            defineTABdatacell1(z,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
        end;

        procedure add;
        begin
            AddToTabcell(z,nyT.count,XYZcount,XYZcount_arrays);
        end;


    begin
        if equal_xyz_tabcells1(glT,nyT) then
        begin
            //inc(z);
            //put;
            add;
            //snxyz('equal',s);
            goto vslut;
        end
        else
        if less_xyz_tabcell1(nyT,glT) then
        begin
            // look down
            if z=1 then
            begin
                put; // because we are at the bottom
                //snxyz('below bottom',s);
                goto Vslut;
            end
            else
            begin
                bund:=1;
                getTABdataCell1(XYZdata,XYZdata_arrays,bund,Nvar,Tbund.cell);

                if equal_xyz_tabcells1(nyT,Tbund) then
                begin
                    z:=bund;
                    add;
                    goto Vslut;
                end
                else
                if less_xyz_tabcell1(nyT,Tbund) then
                begin
                    z:=1;
                    put;
                    //snxyz('bottom',s);
                    goto Vslut;
                end;

                top:=z;
                midt:=(bund+top) div 2;

                getTABdataCell1(XYZdata,XYZdata_arrays,midt,Nvar,Tmidt.cell);
                getTABdataCell1(XYZdata,XYZdata_arrays,top,Nvar,Ttop.cell);
                goto videre1;
            end;
        end
        else
        begin
            // look up
            if z=Nxyz then
            begin
                inc(Nxyz);
                defineTABdatacell1(Nxyz,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
                z:=Nxyz;
                //snxyz('above top',s);
                goto Vslut;
            end
            else
            begin
                top:=Nxyz;
                getTABdataCell1(XYZdata,XYZdata_arrays,top,Nvar,Ttop.cell);

                if equal_xyz_tabcells1(nyT,Ttop) then
                begin
                    z:=top;
                    add;
                    goto Vslut;
                end
                else
                if less_xyz_tabcell1(Ttop,nyT) then
                begin
                    inc(Nxyz);
                    defineTABdatacell1(Nxyz,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
                    z:=Nxyz;
                    //snxyz('top',s);
                    goto Vslut;
                end;

                bund:=z;
                midt:=(top+bund) div 2;
                getTABdataCell1(XYZdata,XYZdata_arrays,bund,Nvar,Tbund.cell);
                getTABdataCell1(XYZdata,XYZdata_arrays,midt,Nvar,Tmidt.cell);
            end;
        end;

    videre1:

        // search between bund and top for a place to put the nyT


        if equal_xyz_tabcells1(Tmidt,nyT) then
        begin
            z:=midt;
            add;
            goto Vslut;
        end
        else
        if less_xyz_tabcell1(nyT,Tmidt) then
        begin
            if equal_xyz_tabcells1(Tbund,nyT) then
            begin
                z:=bund;
                add;
                goto Vslut;
            end
            else
            if midt-bund=1 then
            begin
                z:=bund+1;
                put;
                goto Vslut;
            end
            else
            begin
                Top:=Midt;
                Ttop:=Tmidt;
                Midt:=(bund+top) div 2;
                getTABdataCell1(XYZdata,XYZdata_arrays,midt,Nvar,Tmidt.cell);
                goto videre1;
            end;
        end
        else
        begin
            if equal_xyz_tabcells1(Ttop,nyT) then
            begin
                z:=top;
                add;
                goto Vslut;
            end
            else
            if top-midt=1 then
            begin
                z:=midt+1;
                put;
                goto Vslut;
            end
            else
            begin
                Bund:=Midt;
                Tbund:=Tmidt;
                Midt:=(bund+top) div 2;
                getTABdataCell1(XYZdata,XYZdata_arrays,midt,Nvar,Tmidt.cell);
                goto videre1;
            end;
        end;

        // We are at the end. Therefore:

        inc(Nxyz);
        defineTABdatacell1(Nxyz,nyT.cell,nyT.count,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);
        z:=Nxyz;

    videre2:
        //snxyz('slut',s);
    vslut:
    end;

    //.................................................

begin // main procedure

    // initialize the XYZdata

    init_tabdata(NTABcells,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays,XYZdata_ready);

    if not XYZdata_ready then okmessage('Warning: Initialization of temporary data files failed');

    init_transfer;

    for cell:=1 to NTABcells do
    begin
        nyT:=XYZtabcell1(cell,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,NTABcells,fejl);
        valid:=true;

        for i:=1 to nvar do
        if to_be_used_in_XYZ(.i.) and (nyT.cell(.i.)=0) then valid:=false;

        if valid then
        begin
            if Nxyz<10 then process_record else Qprocess_record;
            glT:=nyT;
        end;
    end;

slut:
end;

//-----------------------------------------------------------------------------

procedure QuickSort_XYZ_records(NTABcells : longint;
                           Nvar : byte;
                           TABdata : Byte_data_array;
                           TABdata_arrays : Byte;
                           TABcount : Longint_data_array;
                           TABcount_arrays : byte;
                       var Nxyz : longint;
                       var XYZdata        : byte_data_array;
                       var XYZdata_arrays : byte;
                       var XYZcount       : Longint_data_array;
                       var XYZcount_arrays : byte;
                       var XYZdata_ready : boolean;
                       var inrec         : ptr_stvector;
                       var outrec        : ptr_stvector;
                       var sortsuccess   : boolean);

// this procedure quicksorts a TABdata set according to the controlvariables defining Z in an XYZ-table
// inrec and outrec are used for sorting. We assume that they have been allocated before this procedure
// is invoked
// Remember that Nxyz may be zero!
label slut,videre;
var cell,z,bund,top,midt       : longint;
    i,j,fejl                   : byte;
    valid                      : boolean;
    nyT                        : Tabrecord;
    b0                         : bvector;

    XYZdim,xyz                 : Bvector;
    NxyzVars                   : Byte;
    Nvalid, glnr,nytnr         : integer;
    s,s1,sx,sy                 : string;


    //...............................................

    procedure init_transfer;
    var i: integer;
    begin
        for i:=0 to ntabcells+1 do
        begin
            inrec^(.i.).stacknr:=0;
            inrec^(.i.).count:=0;
        end;

        outrec^:=inrec^;

        Nxyz :=0;
        z := 0;

        XYZdim:=onebytes;
        Nxyzvars:=2;
        for i:=control_limits(.1.) to control_limits(.2.) do
        if control_variables(.i.) then
        begin
            inc(NxyzVars);
            XYZdim(.i.):=dim(.i.);
        end;
        stackinit(Nvar,XYZdim,stackfactor,stackfactorsum);
    end;

    //................................................

    procedure process_record;
    var i,k : integer;
    begin
        XYZ:=onebytes;
        //k:=0;
        for i:=control_limits(.1.) to control_limits(.2.) do
        if control_variables(.i.) then
        begin
            //inc(k);
            xyz(.i.):=nyT.cell(.i.);
        end;

        inrec^(.nvalid.).stacknr:=stack(xyz,Nvar,stackfactor,stackfactorsum);
        inrec^(.nvalid.).count:=nyT.count;
        inrec^(.nvalid.).x:=nyT.cell(.vx.);
        inrec^(.nvalid.).y:=nyT.cell(.vy.);
    end;

    //................................................

    procedure collect_records;
    var i,count : integer;
        x,y,glx,gly : byte;
        udfil : textfile;

        procedure pakud;
        var i : integer;
            v : bvector;
        begin
            inc(Nxyz);
            undostack(glnr,Nvar,stackfactor,v);

            v(.vx.):=glx;
            v(.vy.):=gly;

            defineTabdatacell1(Nxyz,v,count,nvar,
                               XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays);

        end;

    begin
        //assignfile(udfil,'pakud.rep');
        //rewrite(udfil);
        if nvalid>0 then
        begin
            glnr  :=outrec^(.1.).stacknr;
            count := outrec^(.1.).count;
            glx := outrec^(.1.).x;
            gly := outrec^(.1.).y;

            for i:=2 to nvalid do
            begin
                nytnr:=outrec^(.i.).stacknr;
                x := outrec^(.i.).x;
                y := outrec^(.i.).y;
                if (glnr=nytnr) and (x=glx) and (y=gly) then count:=count+outrec^(.i.).count else
                begin
                    pakud;
                    glnr:=nytnr;
                    count:=outrec^(.i.).count;
                    glx := outrec^(.i.).x;
                    gly := outrec^(.i.).y;
                end;
            end;
            pakud;
            //closefile(udfil);
        end
        else
        begin
            nxyz:=0;
        end;
    end;

begin
    init_transfer;
    sortSuccess:=StackPossible(nvar,xyzdim);

    init_tabdata(NTABcells,Nvar,XYZdata,XYZdata_arrays,XYZcount,XYZcount_arrays,XYZdata_ready);

    if not XYZdata_ready then okmessage('Warning: Initialization of temporary data files failed');


    nvalid:=0;
    for cell:=1 to NTABcells do
    begin
        nyT:=XYZtabcell1(cell,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,NTABcells,fejl);
        valid:=true;

        for i:=1 to nvar do
        if to_be_used_in_XYZ(.i.) and (nyT.cell(.i.)=0) then
        begin
            valid:=false;
            goto videre;
        end;

        if valid then
        begin
            inc(nvalid);
            process_record;
        end;
    videre:
    end;

    //str(nvalid,s);
    //okmessage(s+' valid records found');

    if nvalid>0 then
    begin
        SortStackedTrecords(nvalid,inrec,outrec);
        collect_records;
    end;

    //str(nvalid,s);
    //str(nxyz,s1);

    //okmessage('collect finished '+s+' valid '+s1+' xyz-records');

slut:
end;

//--------------------------------------------------------------------------

Function XYZ_TabCell;
label slut;
var TC    : tabrecord;
    v     : byte;
begin
    TC.CELL  :=Bzeros;
    TC.count :=0;
    valid:=true;

    for v:=fra to til do
    begin
        if to_be_used_in_XYZ(.v.) then
        begin
            get_byte_data(nvar,tabdata,tabdata_arrays,N,v,TC.cell(.v.),valid);
            if TC.cell(.v.)=0 then
            begin
                valid:=false;
                goto slut;
            end;
        end;
    end;

    Locate_table_point(n,array_no,point);
    TC.count:=TABcount(.array_no.)^(.point.);

slut:
    XYZ_TABcell:=TC;
end;

//-----------------------------------------------------------

procedure create_XYZ_records;
label slut;
var i,j,fra,til,fejl,x,y,df : byte;
    cell                    : longint;
    valid                   : boolean;
    glT,nyT                 : Tabrecord;
    TAB2                    : TwowayTableArray;

    procedure init_transfer;
    begin
        fillchar(glT.cell,sizeof(glT.cell),0);
        glT.count:=0;
        nyT:=glT;

        DimZ := 0;

        tab2:=tab2zeros;
    end;

    //..........................................................................

    procedure count_marginals;
    var ix,iy : byte;
        nx,ny : integer;
    begin
        (* test output * write(outfile,'dimx,dimy = ',dimx:3,dimy:3); (***)
        df:=0; nx:=-1; ny:=-1;
        for ix:=1 to dimx do
        begin
            for iy:=1 to dimy do
            begin
                tab2(.ix,dimy+1.):=tab2(.ix,dimy+1.)+tab2(.ix,iy.);
                tab2(.dimx+1,iy.):=tab2(.dimx+1,iy.)+tab2(.ix,iy.);
            end;
            if tab2(.ix,dimy+1.)>0 then
            begin
                inc(nx);
                tab2(.dimx+1,dimy+1.):=tab2(.dimx+1,dimy+1.)+tab2(.ix,dimy+1.);
            end;
        end;
        (* test output *  write(outfile,' total = ',tab2(.dimx+1,dimy+1.):6); (***)
        for iy:=1 to dimy do
        if tab2(.dimx+1,iy.)>0 then inc(ny);
        df:=ny*nx;
        (* test output *  writeln(outfile,'   nx,ny,df = ',nx:2,ny:2,df:2); (***)
    end;

    //..........................................................................

    procedure transfer_table;
    var x,y,i : byte;
        rtab2 : Etwowaytablearray;
        n,pointer : longint;
        array_no  : byte;
    begin
        fillchar(rtab2,sizeof(rtab2),0);

        if tab2(.dimx+1,dimy+1.)>0 then
        for x:=1 to dimx+1 do
        for y:=1 to dimy+1 do
        rtab2(.x,y.):=tab2(.x,dimy+1.)*(tab2(.dimx+1,y.)/tab2(.dimx+1,dimy+1.));

        store_xyz_slice(dimz,xyz_table,xyz_arrays,exyz_table,exyz_arrays,
                        tdimx,tdimy,xy_size,tab2,rtab2);

        // store Zcells here
        if include_cells then
        begin
            for i:=1 to nvar do
            put_byte_data(nvar,zcell,Zcell_arrays,dimz,i,nyT.cell(.i.),success);
        end;
    end;

    //..........................................................................

    procedure add_count_to_tab2;
    begin
        x:=nyT.cell(.vx.);
        y:=nyT.cell(.vy.);
        tab2(.x,y.):=tab2(.x,y.)+nyT.count;
    end;

    //..........................................................................

    procedure process_xyz_record;
    begin
        // We have a new xyz_tabrecord (nyT). Compare with the old one.

        //if cell =1 then

        if (Nxyz=1) then     // the first valid cell
        begin
            add_count_to_tab2;
            glT:=nyT;
        end
        else
        if equal_XYZ_tabcells(nyT,glT) then  // this has to be equal_xyz_tabcells without vx and vy
        begin
            add_count_to_tab2;
        end
        else
        begin
            count_marginals;

            if (df>0) and (DimZ<Zmax) then
            begin
                inc(DIMZ);
                transfer_table;
            end;
            tab2:=tab2zeros;
            add_count_to_tab2;
            glT:=nyT;
        end;
    end;

begin

    (*************************************************************
    ** the following global variables are assumed to be ready:  **
    **                                                          **
    **      control_variables(.i.)                              **
    **      control_limits(.1.) and (.2.)                       **
    **      vx and vy                                           **
    *************************************************************)

    (**********************************
    writeln(outfile);
    writeln(outfile,'Test output from Create_XYZ_records');
    writeln(outfile);
    writeln(outfile,'vx,vy =         ',vx:3,vy:3);
    writeln(outfile,'control limits =',control_limits(.1.):3,control_limits(.2.):3);
    for i:=1 to nvar do write(outfile,control_variables(.i.));writeln(outfile);
    DeliverTMPoutput;
    (**** ok so far *******************)

    Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready);

    // this procedure initialized Nxyz, to_be_used_in_XYZ,
    // fra, til, fejl and xyz_ready

    if not TABdata_ready then
    begin
        okMessage('TABdata is not available');
        XYZ_ready:=false;
        goto slut;
    end;

    valid:=true;

    init_transfer;

    (* test output * writeln(outfile,'Tabulating from ',Ntabcells,' tabcells'); (**)

    for cell:=1 to Ntabcells do
    begin
        //write(outfile,'cell no. ',cell:3,':');

        nyT:=XYZ_tabcell(cell,nvar,TABDATA_READY,TABDATA,TABDATA_arrays,TABcount,TABcount_arrays,NTABcells,
                         fra,til,valid);

        //for i:=1 to nvar do write(outfile,nyT.cell(.i.));writeln(outfile,nyT.count:6);DeliverTMPoutput;

        if valid then
        begin
            inc(Nxyz);
            process_xyz_record;
        end;
    end;

    count_marginals;
    if (df>0) and (DimZ<Zmax) then
    begin
        inc(dimz);
        transfer_table;
    end;

    //nyT.CELL:=bzeros;
    //inc(Nxyz);
    //process_xyz_record;

    (* test output * writeln(outfile,Nxyz,' used TABcells'); (**)
    (* test output * writeln(outfile,dimz,' Z-cells');  (**)
    (* test output * writeln(outfile,xy_size,' xy_size');  (**)
slut:
end;

//----------------------------------------------------------

function bigtest_ready(HYPOTHESIS_EXIST : BOOLEAN;
                        NHYP             : INTEGER;
                        HYPOTESE         : HYPARRAY;
                        HYPNR            : INTEGER): boolean;
begin
     bigtest_ready:=true;

     IF NOT HYPOTHESIS_EXIST THEN
     BEGIN
         OKmessage('No hypothesis.');
         bigtest_ready:=FALSE;
     END;

     IF NOT TABFILE_EXISTS THEN
     BEGIN
         OKmessage('There is no TABfile.');
         bigtest_ready:=FALSE;
     END
     else
     if not tabdata_ready then
     begin
         OKmessage('TABdata is not available.');
         bigtest_ready:=FALSE;
     end;
end;

//-------------------------------------------------------

procedure define_control_variables(Nvar        : byte;
                                   DIM         : Bvector;
                                   HYPOTESE    : HYPARRAY;
                                   HYPNR       : INTEGER;
                                   var XY      : TwoBytes;
                                   var success : boolean);
label videre,slut;
var i,k : byte;
begin
    success:=true;
    FOR I:=1 TO LARGEDIM DO CONTROL_VARIABLES(.I.):=FALSE;

    K:=0;

    FOR I:=1 TO NVAR DO
    BEGIN
        IF HYPOTESE(.HYPNR,I.)=2 THEN CONTROL_VARIABLES(.I.):=TRUE ELSE
        IF HYPOTESE(.HYPNR,I.)=1 THEN
        BEGIN
            inc(K);
            IF K>2 THEN
            BEGIN
                OKmessage('Too many variables of interest.'); // PROGRAMMING ERROR
                SUCCESS:=FALSE;
                GOTO SLUT;
            END;
            XY(.K.):=I;
        END;
    END;
    IF K<2 THEN
    BEGIN
        OKmessage('Too FEW variables of interest.'); //PROGRAMMING ERROR
        SUCCESS:=FALSE;
        GOTO SLUT;
     END;

     VX:=XY(.1.); DIMX:=DIM(.VX.); TDIMX:=TDIM(.VX.);  Xtype:=Vtype(.vx.); Xlabel:=vlabel(.vx.);
     VY:=XY(.2.); DIMY:=DIM(.VY.); TDIMY:=TDIM(.VY.);  Ytype:=Vtype(.vy.); Ylabel:=vlabel(.vy.);

     FOR I:=1 TO NVAR DO
     IF CONTROL_VARIABLES(.I.) THEN
     BEGIN
         CONTROL_LIMITS(.1.):=I;
         GOTO videre;
     END;

videre:

     FOR I:=NVAR DOWNTO CONTROL_LIMITS(.1.) DO
     IF CONTROL_VARIABLES(.I.) THEN
     BEGIN
         CONTROL_LIMITS(.2.):=I;
         GOTO slut;
     END;

slut:
end;

//-------------------------------------------------------

procedure define_control_variables1;
label videre,slut;
var i,k : byte;
begin
    success:=true;
    FOR I:=1 TO LARGEDIM DO CONTROL_VARIABLES(.I.):=FALSE;

    K:=0;

    FOR I:=1 TO NVAR DO
    BEGIN
        IF HYPOTESE(.HYPNR,I.)=2 THEN CONTROL_VARIABLES(.I.):=TRUE ELSE
        IF HYPOTESE(.HYPNR,I.)=1 THEN
        BEGIN
            inc(K);
            IF K>2 THEN
            BEGIN
                writeln(outfile,'To many variables of interest');
                // PROGRAMMING ERROR
                SUCCESS:=FALSE;
                GOTO SLUT;
            END;
            XY(.K.):=I;
        END;
    END;
    IF K<2 THEN
    BEGIN
        writeln(outfile,'To many variables of interest'); // probably programming error
        SUCCESS:=FALSE;
        GOTO SLUT;
     END;

     VX:=XY(.1.); DIMX:=DIM(.VX.); TDIMX:=TDIM(.VX.);  Xtype:=Vtype(.vx.); Xlabel:=vlabel(.vx.);
     VY:=XY(.2.); DIMY:=DIM(.VY.); TDIMY:=TDIM(.VY.);  Ytype:=Vtype(.vy.); Ylabel:=vlabel(.vy.);

     FOR I:=1 TO NVAR DO
     IF CONTROL_VARIABLES(.I.) THEN
     BEGIN
         CONTROL_LIMITS(.1.):=I;
         GOTO videre;
     END;

videre:

     FOR I:=NVAR DOWNTO CONTROL_LIMITS(.1.) DO
     IF CONTROL_VARIABLES(.I.) THEN
     BEGIN
         CONTROL_LIMITS(.2.):=I;
         GOTO slut;
     END;

slut:
end;

//-------------------------------------------------------

PROCEDURE BIGTEST;

(***********************************************)
(** This procedure creates a datamatrix       **)
(** according to dataspecifications given by  **)
(** DEFINE_DATA (in DATAunit) and stores the  **)
(** matrix as BYTE_DATA for PCSCREEN          **)
(***********************************************)

LABEL SLUT;
VAR
   TABCELL  : TABRECORD;  // The TABrecord includes TABRECORD.Cell (Bvector) and TABRECORD.count (longint);
   M,N,I,Z   : INTEGER;
   J,K,L,X,Y : BYTE;
   XY        : TwoBytes;
   fra,til,fejl   : byte;
   xyz_ready      : boolean;
   EFFECTIVE_N    :LONGINT;
   EFFECTIVE_SIM: INTEGER;
BEGIN (* Main procedure *)
    success:=bigtest_ready(HYPOTHESIS_EXIST,Nhyp,Hypotese,Hypnr);
    if not success then goto slut;

    (* TRANSFER INFORMATION FROM HYPOTESE TO CONTROL_VARIABLES AND X,Y *)

    define_control_variables(NVAR,DIM,HYPOTESE,hypnr,xy,success);
    if not success then goto slut;
    Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready);

    INIT_XYZ(ZMAX,NZ,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS);

    create_XYZ_records(outfile,nvar,TABDATA_READY,TABDATA,TABDATA_arrays,TABcount,TABcount_arrays,NTABcells,
                       false,success);

    if not success then
    begin
        okMessage('Failure creating XYZ-records for XYZ_TEST');
        DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                    EXYZ_TABLE,EXYZ_ARRAYS);
        goto slut;
    end;

    XYZ_TEST(outfile,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,
             DIMZ,VTYPE(.VX.),VTYPE(.VY.),
             VLABEL(.VX.),VLABEL(.VY.),VLABEL,
             XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS,
             EXACT,NSIM,SIMSTART,
             SEQUENTIAL,SEQ_P0,SEQ_ALPHA,SEQ_B,
             NVAR,HYPNR,HYPOTESE,
             EFFECTIVE_N,EFFECTIVE_SIM,RESULTS);

    IF ORDINALS(VX,VY,DIM,VTYPE) THEN HYPOTESE(.HYPNR,0.):=3 ELSE HYPOTESE(.HYPNR,0.):=2;

    if not two_sided then
    EXA_SUMMARY1(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT)
    else
    EXA_SUMMARY1_2(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT);

    DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                EXYZ_TABLE,EXYZ_ARRAYS);

SLUT:
END;  (* of BIGTEST *)

(*---------------------------------------------*)

PROCEDURE CHEAP_BIGTEST;

(***********************************************)
(** This procedure creates a datamatrix       **)
(** according to dataspecifications given by  **)
(** DEFINE_DATA (in DATAunit) and stores the  **)
(** matrix as BYTE_DATA for PCSCREEN          **)
(***********************************************)

LABEL SLUT;
VAR
   TABCELL  : TABRECORD;
   M,N,I,Z   : INTEGER;
   J,K,L,X,Y : BYTE;
   XY        : TwoBytes;
   EFFECTIVE_N   : LONGINT;
   EFFECTIVE_SIM : INTEGER;
   fra,til,fejl : byte;
   xyz_ready : boolean;

BEGIN (* Main procedure *)

    success:=bigtest_ready(HYPOTHESIS_EXIST,Nhyp,Hypotese,Hypnr);
    if not success then
    begin
        okMessage('BIGtest failure: not ready');
        goto slut;
    end;

    define_control_variables(NVAR,DIM,HYPOTESE,hypnr,xy,success);
    if not success then
    begin
        okMessage('BIGtest failure: no control variables');
        goto slut;
    end;

    Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready); // defines certain global variables.
                                                 // tHis procedure is probably superfluos
                                                 // because initialization must have happened before
                                                 // cheap bigtest was called

    INIT_XYZ(ZMAX,NZ,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS);

    // Initializes tables
    // (Re)defines Tdimx and TdimY and calculates xy_size and sets NZ:=0; Calculates Zmax.

    create_XYZ_records(outfile,nvar,TABDATA_READY,TABDATA,TABDATA_arrays,TABcount,
                       TABcount_arrays,NTABcells,
                       false,success);

    if not success then
    begin
        okMessage('Failure creating XYZ-records for CHEAP_XYZ_TEST');
        DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                    EXYZ_TABLE,EXYZ_ARRAYS);
        goto slut;
    end;

    (** hertil **)

    CHEAP_XYZ_TEST(outfile,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,
                   DIMZ,VTYPE(.VX.),VTYPE(.VY.),
                   VLABEL(.VX.),VLABEL(.VY.),VLABEL,
                   XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS,
                   EXACT,NSIM,SIMSTART,
                   SEQUENTIAL,SEQ_P0,SEQ_ALPHA,SEQ_B,Exact_p_level,
                   NVAR,HYPNR,HYPOTESE,
                   EFFECTIVE_N,EFFECTIVE_SIM,RESULTS);

    IF ORDINALS(VX,VY,DIM,VTYPE) THEN HYPOTESE(.HYPNR,0.):=3 ELSE HYPOTESE(.HYPNR,0.):=2;

    DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                EXYZ_TABLE,EXYZ_ARRAYS);
SLUT:
END;  (* of CHEAP_BIGTEST *)

(*---------------------------------------------*)

PROCEDURE pseudogamma_BIGTEST;

// This procedure is a modified version of Cheap_bigtest

// This is where the XYZ-table is created

LABEL SLUT;
VAR
   TABCELL  : TABRECORD;
   M,N,I,Z   : INTEGER;
   J,K,L,X,Y : BYTE;
   XY        : TwoBytes;
   EFFECTIVE_N   : LONGINT;
   EFFECTIVE_SIM : INTEGER;
   fra,til,fejl : byte;
   xyz_ready : boolean;

   xorder1,yorder1 : bvector;

   permuted_gamma1,permuted_gamma1_p : real;

   pseudogamma_possible : boolean;

   lr2,lr2_p : real;
   df2       : integer;

   procedure WriteOutput(nr: integer);
   begin
       if printoutput then
       case nr of
           1: begin
              end;
       end;
   end;

BEGIN (* Main procedure *)

    //if use_x then writeln(outfile,'*** use_x') else writeln(outfile,'*** do not use x');
    //if use_y then writeln(outfile,'*** use_y') else writeln(outfile,'*** do not use y');

    success:=bigtest_ready(HYPOTHESIS_EXIST,Nhyp,Hypotese,Hypnr);
    if not success then
    begin
        okMessage('BIGtest failure: not ready');
        goto slut;
    end;

    define_control_variables(NVAR,DIM,HYPOTESE,hypnr,xy,success);
    if not success then
    begin
        okMessage('BIGtest failure: no control variables');
        goto slut;
    end;

    Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready); // defines certain global variables.
                                                 // tHis procedure is probably superfluos
                                                 // because initialization must have happened before
                                                 // cheap bigtest was called

    INIT_XYZ(ZMAX,NZ,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS);

    // Initializes tables
    // (Re)defines Tdimx and TdimY and calculates xy_size and sets NZ:=0; Calculates Zmax.

    create_XYZ_records(outfile,nvar,TABDATA_READY,TABDATA,TABDATA_arrays,TABcount,
                       TABcount_arrays,NTABcells,false,success);

    if not success then
    begin
        okMessage('Failure creating XYZ-records for CHEAP_XYZ_TEST');
        DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                    EXYZ_TABLE,EXYZ_ARRAYS);
        goto slut;
    end;

    pseudogamma_possible:=true;
    if permute_x and (dimx>8)  then
    begin
        pseudogamma_possible:=false;
        method:=2;
    end;

    if permute_y and (dimy>8)  then
    begin
        pseudogamma_possible:=false;
        method:=2;
    end;

    if method<>2 then
    begin
        Pseudogamma_XYZ_TEST(outfile,1,permute_x,restrict_x,xrestrictions,
                   permute_y,restrict_y,yrestrictions,
                   use_x,use_y,Xcoding,Ycoding,
                   DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,
                   DIMZ,VTYPE(.VX.),VTYPE(.VY.),
                   VLABEL(.VX.),VLABEL(.VY.),VLABEL,
                   XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS,
                   EXACT,NSIM,SIMSTART,
                   SEQUENTIAL,SEQ_P0,SEQ_ALPHA,SEQ_B,Exact_p_level,
                   NVAR,HYPNR,HYPOTESE,
                   EFFECTIVE_N,EFFECTIVE_SIM,RESULTS,
                   permuted_gamma_ready,permuted_gamma,permuted_gamma_p,xorder,yorder,lr2,df2,lr2_p,printoutput);
        writeln(outfile);
        writeln(outfile,'-------------------------------------------------------------');
        writeln(outfile);
    end;

    if method<>1 then
    begin
        Pseudogamma_XYZ_TEST(outfile,2,permute_x,restrict_x,xrestrictions,
                       permute_y,restrict_y,yrestrictions,
                       use_x,use_y,Xcoding,Ycoding,
                       DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,
                       DIMZ,VTYPE(.VX.),VTYPE(.VY.),
                       VLABEL(.VX.),VLABEL(.VY.),VLABEL,
                       XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS,
                       EXACT,NSIM,SIMSTART,
                       SEQUENTIAL,SEQ_P0,SEQ_ALPHA,SEQ_B,Exact_p_level,
                       NVAR,HYPNR,HYPOTESE,
                       EFFECTIVE_N,EFFECTIVE_SIM,RESULTS,
                       permuted_gamma_ready,permuted_gamma1,permuted_gamma1_p,xorder1,yorder1,lr2,df2,lr2_p,printoutput);

        writeln(outfile);
        writeln(outfile,'------------------------------------------------------------------');
        writeln(outfile);
    end;
    
    if not pseudogamma_possible then
    begin
        xorder:=xorder1;
        yorder:=yorder1;
        permuted_gamma:=permuted_gamma1;
    end;

    // The pseudogamma coefficient and the exact p-value are calculated by PseudoGamma_XYZ_Test

    IF ORDINALS(VX,VY,DIM,VTYPE) THEN HYPOTESE(.HYPNR,0.):=3 ELSE HYPOTESE(.HYPNR,0.):=2;

    DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                EXYZ_TABLE,EXYZ_ARRAYS);
SLUT:
END;  (* of permuted_BIGTEST *)

(*---------------------------------------------*)

PROCEDURE pairwise_comparisons;

// This procedure performs pairwise comparisons of all rows or columns
//                sorts row or columns in the correct order
//                calculates the gamma coefficient for the recoded variables

LABEL SLUT;
VAR
   TABCELL  : TABRECORD;
   M,N,I,Z   : INTEGER;
   J,K,L,X,Y : BYTE;
   XY        : TwoBytes;
   EFFECTIVE_N   : LONGINT;
   EFFECTIVE_SIM : INTEGER;
   fra,til,fejl : byte;
   xyz_ready : boolean;

   ix,iy,jx,jy : byte;
   pairs : array(.1..maxhyp,1..4.) of byte;
   nxpairs,nypairs,npairs,pairnr : integer;
   Pairwise_results: resarray;

   xscore,yscore : bvector;

   use_x,use_y : boolean;
   xcoding,ycoding : bvector;
   s,s1 : string;

   procedure WriteOutput(nr: integer);
   begin
       if printoutput then
       case nr of
           1: begin
                  writeln(outfile,pairs(.pairnr,1.):3,pairs(.pairnr,2.):3,pairs(.pairnr,3.):3,pairs(.pairnr,4.):3,
                          pairwise_results(.pairnr,5.):7:3,' p = ',pairwise_results(.pairnr,6.):6:3);
              end;
       end;
   end;

BEGIN (* Main procedure *)

    success:=bigtest_ready(HYPOTHESIS_EXIST,Nhyp,Hypotese,Hypnr);
    if not success then
    begin
        okMessage('BIGtest failure: not ready');
        goto slut;
    end;

    define_control_variables(NVAR,DIM,HYPOTESE,hypnr,xy,success);
    if not success then
    begin
        okMessage('BIGtest failure: no control variables');
        goto slut;
    end;

    Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready); // defines certain global variables.
                                                 // tHis procedure is probably superfluos
                                                 // because initialization must have happened before
                                                 // cheap bigtest was called

    INIT_XYZ(ZMAX,NZ,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS);

    // Initializes tables
    // (Re)defines Tdimx and TdimY and calculates xy_size and sets NZ:=0; Calculates Zmax.

    create_XYZ_records(outfile,nvar,TABDATA_READY,TABDATA,TABDATA_arrays,TABcount,
                       TABcount_arrays,NTABcells,
                       false,success);

    if not success then
    begin
        okMessage('Failure creating XYZ-records for CHEAP_XYZ_TEST');
        DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                    EXYZ_TABLE,EXYZ_ARRAYS);
        goto slut;
    end;

    if permute_x then nxpairs:=DimX*(dimX-1) div 2 else nxpairs:=1;
    if permute_y then nypairs:=Dimy*(dimy-1) div 2 else nypairs:=1;

    npairs:=nxpairs*nypairs;

    if npairs>maxhyp then
    begin
        str(npairs,s); s:=s+' pairwise comparisons requested. The maximum is ';
        str(maxhyp,s1); s:=s+s1;
        okmessage(s);
        goto slut;
    end;

    if (nxpairs>1) and (nypairs>1) then
    begin
        pairnr:=0;
        for ix:=1 to dimx-1 do
        for jx:=ix+1 to dimx do
        for iy:=1 to dimy-1 do
        for jy:=iy+1 to dimy do
        begin
            inc(pairnr);
            pairs(.pairnr,1.):=ix;
            pairs(.pairnr,2.):=jx;
            pairs(.pairnr,3.):=iy;
            pairs(.pairnr,4.):=jy;
        end;
    end
    else
    if nxpairs>1 then
    begin
        pairnr:=0;
        for ix:=1 to dimx-1 do
        for jx:=ix+1 to dimx do
        begin
            inc(pairnr);
            pairs(.pairnr,1.):=ix;
            pairs(.pairnr,2.):=jx;
            pairs(.pairnr,3.):=0;
            pairs(.pairnr,4.):=0;
        end;
    end
    else
    begin
        pairnr:=0;
        for iy:=1 to dimy-1 do
        for jy:=iy+1 to dimy do
        begin
            inc(pairnr);
            pairs(.pairnr,1.):=0;
            pairs(.pairnr,2.):=0;
            pairs(.pairnr,3.):=iy;
            pairs(.pairnr,4.):=jy;
        end;
    end;


    xscore:=zerobytes;
    yscore:=zerobytes;

    for pairnr:=1 to npairs do
    begin
        // calculate test results for the specific pair
        Pairwise_XYZ_TEST(outfile,permute_x,permute_y,DIMX,DIMY,
                   pairs(.pairnr,1.),pairs(.pairnr,2.),pairs(.pairnr,3.),pairs(.pairnr,4.),
                   TDIMX,TDIMY,XY_SIZE,
                   DIMZ,VTYPE(.VX.),VTYPE(.VY.),
                   VLABEL(.VX.),VLABEL(.VY.),VLABEL,
                   XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS,
                   false,0,simstart, //EXACT,NSIM,SIMSTART,
                   false,SEQ_P0,SEQ_ALPHA,SEQ_B,Exact_p_level,//SEQUENTIAL,SEQ_P0,SEQ_ALPHA,SEQ_B,Exact_p_level,
                   NVAR,Pairnr,HYPOTESE,
                   EFFECTIVE_N,EFFECTIVE_SIM,pairwise_RESULTS,PrintOutput);

        //writeln(outfile,'In Pairwise_bigtest: Pairwise_xyz_test -> pairnr ',pairnr:3,'  gamma = ',pairwise_results(.pairnr,5.):6:3);
        //writeOutput(1);

        if pairs(.pairnr,1.)*pairs(.pairnr,2.)>0 then
        begin
            if pairwise_results(.pairnr,5.)>0 then inc(xscore(.pairs(.pairnr,2.).)) else
            if pairwise_results(.pairnr,5.)<0 then inc(xscore(.pairs(.pairnr,1.).));
        end;

        if pairs(.pairnr,3.)*pairs(.pairnr,4.)>0 then
        begin
            if pairwise_results(.pairnr,5.)>0 then inc(yscore(.pairs(.pairnr,4.).)) else
            if pairwise_results(.pairnr,5.)<0 then inc(yscore(.pairs(.pairnr,3.).));
        end;
    end;

    if permute_x then     // to define new codes for X in the right order
    begin
        use_x:=true;
        k:=0;
        for i:=0 to DIMX do
        for j:=1 to dimx do
        if xscore(.j.)=i then
        begin
            inc(K);
            Xcoding(.k.):=j;
        end;
    end
    else
    begin
        use_x:=false;
        Xcoding:=zerobytes;
        for i:=1 to dimX do Xcoding(.i.):=i;
    end;

    if permute_y then  // to define new codes for Y  in the right order
    begin
        use_y:=true;
        k:=0;
        for i:=0 to DIMY do
        for j:=1 to dimY do
        if yscore(.j.)=i then
        begin
            inc(k);
            Ycoding(.k.):=j;
        end;
    end
    else
    begin
        use_y:=false;
        Ycoding:=zerobytes;
        for i:=1 to dimY do Ycoding(.i.):=i;
    end;

    hypotheses_exist:=true;

    // Finally, calculate Gamma for the recoded variables

    //writeln(outfile,' X : '); for i:=1 to dimX do write(outfile,Xcoding(.i.):3);writeln(outfile);
    //writeln(outfile,' Y : '); for i:=1 to dimY do write(outfile,Ycoding(.i.):3);writeln(outfile);

    Pseudogamma_BIGTEST(outfile,2,permute_x,restrict_x,xrestrictions,
                     permute_y,restrict_y,yrestrictions,
                     use_x,use_y,Xcoding,ycoding,
                     TABFILE_EXISTS,
                     TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                     TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                     NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                     HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                     false,0,simstart, //EXACT,NSIM,SIMSTART,
                     false,seq_P0,seq_ALPHA,seq_b, //SEQUENTIAL,seq_P0,seq_ALPHA,seq_b,//seq_Beta,
                     1.20, // 0.20
                     RESULTS,SUCCESS,permuted_gamma_ready,permuted_gamma,permuted_gamma_p,xorder,yorder,printOutput);

    DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                EXYZ_TABLE,EXYZ_ARRAYS);
SLUT:
   //writeln(tmpfile,'pg2 = ',permuted_gamma:7:3);
END;  (* of pairwise_BIGTEST *)

(*---------------------------------------------*)

FUNCTION BIG_GAMMA;
LABEL SLUT;
VAR
   TABCELL  : TABRECORD;
   M,N,I,Z   : INTEGER;
   J,K,L,X,Y : BYTE;
   XY        : TwoBytes;
BEGIN (* Main procedure *)

    success:=bigtest_ready(HYPOTHESIS_EXIST,Nhyp,Hypotese,Hypnr);
    if not success then goto slut;

    define_control_variables(NVAR,DIM,HYPOTESE,hypnr,xy,success);
    if not success then goto slut;

    INIT_XYZ(ZMAX,NZ,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS);

    create_XYZ_records(outfile,nvar,TABDATA_READY,TABDATA,TABDATA_arrays,TABcount,TABcount_arrays,NTABcells,
                       false,success);

    if not success then
    begin
        okMessage('Failure creating XYZ-records for XYZ_GAMMA');
        DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                    EXYZ_TABLE,EXYZ_ARRAYS);
        goto slut;
    end;

    BIG_GAMMA:=
    XYZ_GAMMA(DIMX,DIMY,DIMZ,TDIMX,TDIMY,XY_SIZE,
                   VTYPE(.VX.),VTYPE(.VY.),
                   VLABEL(.VX.),VLABEL(.VY.),VLABEL,
                   XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS,
                   HYPNR,HYPOTESE);
SLUT:
END; (* of BIG_GAMMA *)

(*---------------------------------------------------*)

PROCEDURE EXPENSIVE_BIGTEST;

(* THIS IS BIGTEST WITH REFERENCE TO Z-CELLS *)

LABEL SLUT;
VAR
   TABCELL  : TABRECORD;
   M,N,I,Z   : INTEGER;
   J,K,L,X,Y : BYTE;
   XY        : TwoBytes;
   EFFECTIVE_N    :LONGINT;
   EFFECTIVE_SIM : INTEGER;

BEGIN
    INIT_BYTE_DATA(NTABCELLS,NVAR,ZCELL,ZCELL_ARRAYS,SUCCESS);
    IF NOT SUCCESS THEN
    BEGIN
        okMessage('Failure assigning ZCELL');
        GOTO SLUT;
    END;

    success:=bigtest_ready(HYPOTHESIS_EXIST,Nhyp,Hypotese,Hypnr);
    if not success then goto slut;

    define_control_variables(NVAR,DIM,HYPOTESE,hypnr,xy,success);
    if not success then goto slut;

    INIT_XYZ(ZMAX,NZ,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,EXYZ_TABLE,EXYZ_ARRAYS);

    create_XYZ_records(outfile,nvar,TABDATA_READY,TABDATA,TABDATA_arrays,TABcount,TABcount_arrays,NTABcells,
                       true,success);

    if not success then
    begin
        okMessage('Failure creating XYZ-records for EXPENSIVE_XYZ_TEST');
        DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                    EXYZ_TABLE,EXYZ_ARRAYS);
        goto slut;
    end;

    EXPENSIVE_XYZ_TEST(outfile,DIMX,DIMY,TDIMX,TDIMY,XY_SIZE,
                       DIMZ,VTYPE(.VX.),VTYPE(.VY.),
                       VLABEL(.VX.),VLABEL(.VY.),VLABEL,
                       XYZ_TABLE,XYZ_ARRAYS,
                       EXYZ_TABLE,EXYZ_ARRAYS,
                       ZCELL,ZCELL_ARRAYS,
                       EXACT,NSIM,SIMSTART,
                       SEQUENTIAL,SEQ_P0,SEQ_ALPHA,SEQ_B,
                       NVAR,DIM,VNAMES,VNAMES_EXIST,
                       HYPNR,HYPOTESE,
                       EFFECTIVE_N,EFFECTIVE_SIM,RESULTS);

    IF ORDINALS(VX,VY,DIM,VTYPE) THEN HYPOTESE(.HYPNR,0.):=3 ELSE HYPOTESE(.HYPNR,0.):=2;

    //EXA_SUMMARY(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT);

    DISPOSE_XYZ(ZMAX,NZ,TDIMX,TDIMY,XY_SIZE,XYZ_TABLE,XYZ_ARRAYS,
                EXYZ_TABLE,EXYZ_ARRAYS);

SLUT:
    IF ZCELL_ARRAYS>0 THEN DISPOSE_BYTE_DATA(ZCELL,ZCELL_ARRAYS);
END;  (* of EXPENSIVE_BIGTEST *)

//=======================================================================

procedure inexpensive_tests;
label slut,slut_hyp;
var n,hypnr,old_hypnr,Nevidence : integer;
    u1,u2                       : byte;
    save_it,replace_it          : hypbool_array;
    oldtest_found               : boolean;
    old_nsim                    : longint;
    s1,s2                       : string;

    //NTmpcells : Longint;
    //TmpData  : Byte_data_array;
    //TmpCount : Longint_data_array;
    //TmpData_arrays, TMPcount_arrays : byte;
    //Tmp_ready : boolean;

    PROCEDURE INEXPENSIVE_TEST;
    LABEL SLUT;
    var xy : Twobytes;
        i  : byte;
        fra,til,fejl : byte;
        xyz_ready : boolean;
        temp_success :boolean;
    BEGIN
	IF TABDATA_POSSIBLE AND NOT TABDATA_READY THEN
        BEGIN
            IF DATA_READY THEN
            DISPOSE_DATASET(DATA_READY,
                            NRESULTS3,RESULTS3_1,RESULTS3_2,
                            DATASET,DATASET_ARRAYS);

            INIT_TABDATA(NTABCELLS,NVAR,TABDATA,TABDATA_ARRAYS,
                         TABCOUNT,TABCOUNT_ARRAYS,TABDATA_POSSIBLE);

            GET_TABDATA(TABFILE,TABFILE_EXISTS,TABDATA_POSSIBLE,
                        NTABCELLS,NVAR,
                        TABDATA,TABDATA_ARRAYS,
                        TABCOUNT,TABCOUNT_ARRAYS,TABDATA_READY);

        END;

        define_control_variables(Nvar,DIM,Hypotese,hypnr,xy,success);

        Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready);

        // this defines the to_be_used_in_xyz bool_vector
        // using global variables vx, vy, and control_limits
        // global variable Nxyz form SKbig3 is set to 0

        Qsort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                         NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,TMP_ready);

        tmp_ready:=true;

        // TABdata is sorted according to variables defined in to_be_used_in_xyz

        CHEAP_BIGTEST(outfile,TABFILE_EXISTS,TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                EXACT,NSIM,SIMSTART,
                SEQUENTIAL,seq_P0,seq_ALPHA,seq_b,//seq_Beta,
                1.20, // 0.20
                RESULTS,SUCCESS);

        // The XYZ table must be cóunted and analyzed in the procedure above

        IF NOT SUCCESS THEN
        BEGIN
            okMessage('Cheap_BigTest failure');
            GOTO SLUT;
        END;
    SLUT:

    END; (* of INEXPENSIVE_TEST *)


    //...........................................................................

begin
    IF (NHYP=0) or not hypotheses_exist THEN
    BEGIN
        //okMessage('No hypotheses to be tested');
        goto slut;
    end;

    if print_results then DigramStatusPanel(4,'Testing');

    s1:='Hypothesis no. ';
    FOR HYPNR:=1 TO NHYP DO
    BEGIN
        str(hypnr,s2);
        s2:=s1+s2;
        if print_results then DigramStatusPanel(5,s2);
        SUCCESS:=TRUE;

        INEXPENSIVE_TEST;

        IF NOT SUCCESS THEN
        begin
            okMessage('test failure');
            GOTO SLUT_HYP;
        end;
    SLUT_HYP:
    END;

    if print_results then
    begin
        if not two_sided then
        EXA_SUMMARY1(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT)
        else
        EXA_SUMMARY1_2(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT);
    end;
slut:
end; // of inexpensive tests

(*-----------------------------------------------------------*)

procedure quick_tests;
label slut,slut_hyp;
var n,hypnr,old_hypnr,Nevidence : integer;
    u1,u2                       : byte;
    save_it,replace_it          : hypbool_array;
    oldtest_found               : boolean;
    old_nsim                    : longint;
    s1,s2                       : string;

    inrec,outrec                : ptr_stvector;

    sortSuccess                 : boolean;

    PROCEDURE INEXPENSIVE_TEST;
    LABEL SLUT;
    var xy : Twobytes;
        i  : byte;
        fra,til,fejl : byte;
        xyz_ready : boolean;
        temp_success :boolean;
    BEGIN
	IF TABDATA_POSSIBLE AND NOT TABDATA_READY THEN
        BEGIN
            IF DATA_READY THEN
            DISPOSE_DATASET(DATA_READY,
                            NRESULTS3,RESULTS3_1,RESULTS3_2,
                            DATASET,DATASET_ARRAYS);

            INIT_TABDATA(NTABCELLS,NVAR,TABDATA,TABDATA_ARRAYS,
                         TABCOUNT,TABCOUNT_ARRAYS,TABDATA_POSSIBLE);

            GET_TABDATA(TABFILE,TABFILE_EXISTS,TABDATA_POSSIBLE,
                        NTABCELLS,NVAR,
                        TABDATA,TABDATA_ARRAYS,
                        TABCOUNT,TABCOUNT_ARRAYS,TABDATA_READY);

        END;

        define_control_variables(Nvar,DIM,Hypotese,hypnr,xy,success);

        Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready);

        // this defines the to_be_used_in_xyz bool_vector
        // using global variables vx, vy, and control_limits
        // global variable Nxyz form SKbig3 is set to 0

        //okmessage('quicksort follows');

        Quicksort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                         NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,
                         TMP_ready,inrec,outrec,sortsuccess);

        if not sortsuccess then
        begin
            okmessage('old procedure will be used');
            Qsort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                              NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,
                              TMP_ready);
        end;

        tmp_ready:=true;

        //okmessage('quicksort finished');

        //writeln(outfile);
        //writeln(outfile,'After Quicksort');
        //writeln(outfile,NtmpCells,' cells');

        (***
        for i:=1 to Ntmpcells do
        begin
            for j:=1 to nvar do write(outfile,TMPdata(.1.)^(.(i-1)*nvar+j.):2);
            writeln(outfile,TMPcount(.1.)^(.i.):5);
        end;
        writeln(outfile);
        ***)

        // TABdata is sorted according to variables defined in to_be_used_in_xyz

        CHEAP_BIGTEST(outfile,TABFILE_EXISTS,TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                EXACT,NSIM,SIMSTART,
                SEQUENTIAL,seq_P0,seq_ALPHA,seq_b,//seq_Beta,
                1.20, // 0.20
                RESULTS,SUCCESS);

        // The XYZ table must be cóunted and analyzed in the procedure above

        IF NOT SUCCESS THEN
        BEGIN
            okMessage('Cheap_BigTest failure');
            GOTO SLUT;
        END;
    SLUT:

        //okmessage('check1');
        //writeln(outfile,'tmpdata_arrays = ',tmpdata_arrays);
        //writeln(outfile,'tmpcount_arrays = ',tmpcount_arrays);

        //Dispose_Tabdata(Tmpdata,Tmpdata_arrays,Tmpcount,Tmpcount_arrays,temp_success);
        //Dispose_Tabdata(Tmpdata,Tmpdata_arrays,Tmpcount,Tmpcount_arrays,tmp_ready);
    END; (* of INEXPENSIVE_TEST *)


    //...........................................................................

begin
    IF (NHYP=0) or not hypotheses_exist THEN
    BEGIN
        //okMessage('No hypotheses to be tested');
        goto slut;
    end;

    if print_results then DigramStatusPanel(4,'Testing');

    new(inrec);
    new(outrec);

    s1:='Hypothesis no. ';
    FOR HYPNR:=1 TO NHYP DO
    BEGIN
        str(hypnr,s2);
        s2:=s1+s2;
        if print_results then DigramStatusPanel(5,s2);
        SUCCESS:=TRUE;

        INEXPENSIVE_TEST;

        IF NOT SUCCESS THEN
        begin
            okMessage('test failure');
            GOTO SLUT_HYP;
        end;
    SLUT_HYP:
    END;

    if print_results then
    begin
        if not two_sided then
        EXA_SUMMARY1(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT)
        else
        EXA_SUMMARY1_2(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT);
    end;


    try
       dispose(inrec);
       dispose(outrec);
    except
       okmessage('Warning: Momery was not released');
    end;
slut:
end; // of inexpensive tests

(*-----------------------------------------------------------*)

procedure pseudogamma_tests;
label slut,slut_hyp;
var n,hypnr,old_hypnr,Nevidence : integer;
    u1,u2                       : byte;
    save_it,replace_it          : hypbool_array;
    oldtest_found               : boolean;
    old_nsim                    : longint;
    s1,s2                       : string;

    inrec,outrec                : ptr_stvector;
    sortSuccess                 : boolean;

    xperm,yperm                 : bvector;
    maxgam                      : real;
    use_x,use_y                 : boolean;
    Xcoding,ycoding             : bvector;

    PROCEDURE INEXPENSIVE_TEST;
    LABEL SLUT;
    var xy : Twobytes;
        i  : byte;
        fra,til,fejl : byte;
        xyz_ready : boolean;
        temp_success :boolean;
    BEGIN
	IF TABDATA_POSSIBLE AND NOT TABDATA_READY THEN
        BEGIN
            IF DATA_READY THEN
            DISPOSE_DATASET(DATA_READY,
                            NRESULTS3,RESULTS3_1,RESULTS3_2,
                            DATASET,DATASET_ARRAYS);

            INIT_TABDATA(NTABCELLS,NVAR,TABDATA,TABDATA_ARRAYS,
                         TABCOUNT,TABCOUNT_ARRAYS,TABDATA_POSSIBLE);

            GET_TABDATA(TABFILE,TABFILE_EXISTS,TABDATA_POSSIBLE,
                        NTABCELLS,NVAR,
                        TABDATA,TABDATA_ARRAYS,
                        TABCOUNT,TABCOUNT_ARRAYS,TABDATA_READY);

        END;

        define_control_variables(Nvar,DIM,Hypotese,hypnr,xy,success);

        Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready);

        // this defines the to_be_used_in_xyz bool_vector
        // using global variables vx, vy, and control_limits
        // global variable Nxyz form SKbig3 is set to 0

        Quicksort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                         NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,
                         TMP_ready,inrec,outrec,sortsuccess);

        if not sortsuccess then
        begin
            okmessage('old procedure will be used');
            Qsort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                              NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,
                              TMP_ready);
        end;

        tmp_ready:=true;

        // TABdata is sorted according to variables defined in to_be_used_in_xyz

        use_x:=false;
        use_y:=false;

        xcoding:=zerobytes;
        ycoding:=zerobytes;

        permuted_gamma:=2.0;
        permuted_gamma_ready:=false;

        Pseudogamma_BIGTEST(outfile,method,permute_x,restrict_x,xrestrictions,
                permute_y,restrict_y,yrestrictions,
                use_x,use_y,Xcoding,ycoding,
                TABFILE_EXISTS,
                TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                EXACT,NSIM,SIMSTART,
                SEQUENTIAL,seq_P0,seq_ALPHA,seq_b,//seq_Beta,
                1.20, // 0.20
                RESULTS,SUCCESS,
                permuted_gamma_ready,permuted_gamma,permuted_gamma_p,
                xorder,yorder,true);

        // The XYZ table is counted and analyzed in the procedure above

        IF NOT SUCCESS THEN
        BEGIN
            okMessage('Permute_tests failure');
            GOTO SLUT;
        END;
    SLUT:
    END; (* of permute_TESTs *)


    //...........................................................................

begin
    IF (NHYP=0) or not hypotheses_exist THEN
    BEGIN
        //okMessage('No hypotheses to be tested');
        goto slut;
    end;

    if print_results then DigramStatusPanel(4,'Testing');

    new(inrec);
    new(outrec);

    s1:='Hypothesis no. ';
    FOR HYPNR:=1 TO NHYP DO
    BEGIN
        str(hypnr,s2);
        s2:=s1+s2;
        if print_results then DigramStatusPanel(5,s2);
        SUCCESS:=TRUE;

        INEXPENSIVE_TEST;

        IF NOT SUCCESS THEN
        begin
            okMessage('test failure');
            GOTO SLUT_HYP;
        end;
    SLUT_HYP:
    END;


    try
       dispose(inrec);
       dispose(outrec);
    except
       okmessage('Warning: Memory was not released');
    end;
slut:
end; // of permute_tests

//------------------------------------------------------------------------------

procedure pairwise_tests;
// Reorganizes data for each hypothesis such that pairwise tests can be performed
// and executes pairwise_comparisons

label slut,slut_hyp;
var n,hypnr,old_hypnr,Nevidence : integer;
    u1,u2                       : byte;
    save_it,replace_it          : hypbool_array;
    oldtest_found               : boolean;
    old_nsim                    : longint;
    s1,s2                       : string;

    inrec,outrec                : ptr_stvector;
    sortSuccess                 : boolean;

    xperm,yperm                 : bvector;
    maxgam                      : real;

    PROCEDURE INEXPENSIVE_TEST;
    LABEL SLUT;
    var xy : Twobytes;
        i  : byte;
        fra,til,fejl : byte;
        xyz_ready : boolean;
        temp_success :boolean;
    BEGIN
	IF TABDATA_POSSIBLE AND NOT TABDATA_READY THEN
        BEGIN
            IF DATA_READY THEN
            DISPOSE_DATASET(DATA_READY,
                            NRESULTS3,RESULTS3_1,RESULTS3_2,
                            DATASET,DATASET_ARRAYS);

            INIT_TABDATA(NTABCELLS,NVAR,TABDATA,TABDATA_ARRAYS,
                         TABCOUNT,TABCOUNT_ARRAYS,TABDATA_POSSIBLE);

            GET_TABDATA(TABFILE,TABFILE_EXISTS,TABDATA_POSSIBLE,
                        NTABCELLS,NVAR,
                        TABDATA,TABDATA_ARRAYS,
                        TABCOUNT,TABCOUNT_ARRAYS,TABDATA_READY);

        END;

        define_control_variables(Nvar,DIM,Hypotese,hypnr,xy,success);

        Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready);

        // this defines the to_be_used_in_xyz bool_vector
        // using global variables vx, vy, and control_limits
        // global variable Nxyz form SKbig3 is set to 0

        Quicksort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                         NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,
                         TMP_ready,inrec,outrec,sortsuccess);

        if not sortsuccess then
        begin
            //okmessage('old procedure will be used');
            Qsort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                              NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,
                              TMP_ready);
        end;

        tmp_ready:=true;

        // TABdata is sorted according to variables defined in to_be_used_in_xyz

        Pairwise_comparisons(outfile,
                permute_x,restrict_x,xrestrictions,
                permute_y,restrict_y,yrestrictions,
                TABFILE_EXISTS,
                TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                //false,0,simstart, //EXACT,NSIM,SIMSTART,
                //SEQUENTIAL,seq_P0,seq_ALPHA,seq_b,//seq_Beta,
                //false,seq_P0,seq_ALPHA,seq_b,//seq_Beta,
                //1.20, // 0.20
                RESULTS,SUCCESS,permuted_gamma_ready,permuted_gamma,permuted_gamma_p,xorder,yorder,printOutput);

        //writeln(outfile,'In pairwise_test: Pairwise_bigtest -> ',permuted_gamma:6:3);
        // The XYZ table must be cóunted and analyzed in the procedure above

        IF NOT SUCCESS THEN
        BEGIN
            okMessage('Permute_tests failure');
            GOTO SLUT;
        END;
    SLUT:
    END; (* of Inexpensive tests *)


    //...........................................................................

begin
    IF (NHYP=0) or not hypotheses_exist THEN
    BEGIN
        //okMessage('No hypotheses to be tested');
        goto slut;
    end;

    if print_results then DigramStatusPanel(4,'Testing');

    new(inrec);
    new(outrec);

    s1:='Hypothesis no. ';
    FOR HYPNR:=1 TO NHYP DO
    BEGIN
        str(hypnr,s2);
        s2:=s1+s2;
        if print_results then DigramStatusPanel(5,s2);
        SUCCESS:=TRUE;

        INEXPENSIVE_TEST; // calls pairwise_comparisons where categories are sorted and gamma calculated

        IF NOT SUCCESS THEN
        begin
            okMessage('test failure');
            GOTO SLUT_HYP;
        end;
    SLUT_HYP:
    END;


    try
       dispose(inrec);
       dispose(outrec);
    except
       okmessage('Warning: Memory was not released');
    end;
slut:
end; // of inexpensive tests

//------------------------------------------------------------------------------

Procedure collaps_tests(var outfile        : textfile;      //øv
                            Permute_x        : boolean;
                            restrict_x     : boolean;
                            xrestrictions  : Bmatrix;
                            permute_y        : boolean;
                            restrict_y     : boolean;
                            yrestrictions  : Bmatrix;
                            nhyp             : integer;
                        var hypotese         : hyparray;
                            hypotheses_exist : boolean;
                        var results          : resarray;
                            print_results    : boolean;
                        var success          : boolean);
label slut,tests,finish_collaps;
var i,j                       : byte;
    hypnr                     : integer;
    collaps_rows,collaps_cols : boolean;
    s,s1,s2                   : string;
    inrec,outrec              : ptr_stvector;
    sortSuccess               : boolean;


    procedure inexpensive_test;
    LABEL SLUT;
    var xy : Twobytes;
        i,ny,nx             : byte;
        fra,til,fejl        : byte;
        xyz_ready           : boolean;
        temp_success        :boolean;
        collaps_x,Collaps_y : boolean;
        Use_x,Use_y         : bool_vector;
        Xcats,Ycats         : Bmatrix;
        
    BEGIN
	IF TABDATA_POSSIBLE AND NOT TABDATA_READY THEN
        BEGIN
            IF DATA_READY THEN
            DISPOSE_DATASET(DATA_READY,
                            NRESULTS3,RESULTS3_1,RESULTS3_2,
                            DATASET,DATASET_ARRAYS);

            INIT_TABDATA(NTABCELLS,NVAR,TABDATA,TABDATA_ARRAYS,
                         TABCOUNT,TABCOUNT_ARRAYS,TABDATA_POSSIBLE);

            GET_TABDATA(TABFILE,TABFILE_EXISTS,TABDATA_POSSIBLE,
                        NTABCELLS,NVAR,
                        TABDATA,TABDATA_ARRAYS,
                        TABCOUNT,TABCOUNT_ARRAYS,TABDATA_READY);

        END;

        define_control_variables(Nvar,DIM,Hypotese,hypnr,xy,success);

        // this procedure defined a number of global xyz variables from SKVARS: VX,DIMX,Xtype,TDIMX + the same for Y

        if DIMX=2 then collaps_x:=false else collaps_x:=true;
        if DIMY=2 then collaps_y:=false else collaps_y:=true;

        Use_x:=falsevector;
        Use_y:=falsevector;

        xcats:=zeroByteMatrix;
        ycats:=zeroByteMatrix;

        for i:=1 to dimx do
        begin
            use_x(.i.):=true;
            xcats(.i,i.):=1;
        end;

        ny:=dimy;
        for i:=1 to dimy do
        begin
            use_y(.i.):=true;
            ycats(.i,i.):=1;
        end;
        ny:=dimy;


        Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready);

        // this defines the to_be_used_in_xyz bool_vector
        // using global variables vx, vy, and control_limits
        // global variable Nxyz form SKbig3 is set to 0

        Quicksort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                         NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,
                         TMP_ready,inrec,outrec,sortsuccess);

        if not sortsuccess then
        begin
            okmessage('old procedure will be used');
            Qsort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                              NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,
                              TMP_ready);
        end;

        tmp_ready:=true;

        // TABdata is sorted according to variables defined in to_be_used_in_xyz


        collaps_rows:=false;
        collaps_cols:=false;

        CHEAP_BIGTEST(outfile,TABFILE_EXISTS,TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                EXACT,NSIM,SIMSTART,
                SEQUENTIAL,seq_P0,seq_ALPHA,seq_b,//seq_Beta,
                1.20, // 0.20
                RESULTS,SUCCESS);

        IF NOT SUCCESS THEN
        BEGIN
            okMessage('Permute_tests failure');
            GOTO SLUT;
        END;

    try
       dispose(inrec);
       dispose(outrec);
    except
       okmessage('Warning: Momery was not released');
    end;
    SLUT:
    END; (* of INEXPENSIVE_TESTs *)

begin
    // we assume that quick tests have been applied before this procedure is called. This procedure is only concerned with the collaps analysis
    IF (NHYP=0) or not hypotheses_exist THEN
    BEGIN
        //okMessage('No hypotheses to be tested');
        goto slut;
    end;

    if print_results then DigramStatusPanel(4,'Collaps analysis');

    //Initialialize

    new(inrec);
    new(outrec);

    s1:='Hypothesis no. ';
    for hypnr:=1 to nhyp do
    begin
        str(hypnr,s2);
        s2:=s1+s2;
        if print_results then DigramStatusPanel(5,s2);
        SUCCESS:=TRUE;

        INEXPENSIVE_TEST;

        IF NOT SUCCESS THEN
        begin
            okMessage('test failure');
            GOTO Finish_collaps;
        end;
        // count the table for this hypothesis

        // perform standard tests

        // initialize collaps analysis
    tests:
        // compare columns

        // compare rows

        // collaps table and return to tests or goto finish_collaps

    Finish_collaps:
    end;
slut:
end;

//------------------------------------------------------------------------------

Procedure XYZ_MH;
label slut;
VAR
      (* DECLARATIONS HANDLING THE XYZ-TABLE *)
      R,C        : BYTE;
      Z          : INTEGER;

      TAB2       : TWOWAYTABLEarray;
      a,b,cc,d,n,nsquare,ad,bc : longint;
      RTAB2      : ETWOWAYTABLEarray;

      GAMMATOT,STOT,PPQTOT,PMQTOT,PPQ,PMQ,GAMMA,PGAMMA,S   : REAL;

      mhcounter,mhdenominator,a1,a2,a3,b1,b2,b3,mhvar : extended;

      PartGam,MH : Evector;
      // vectors with the following information
      // 1: MH
      // 2: MH low
      // 3: MH high
      // 4: Beta = ln(MH)
      // 5: seBeta
      // 6: Gamma
      // 7: seGamma
      // Partgam contains values based on partial gamma
      // MH contains values based on the Mantel-Haenszel estimate


      fra,til,fejl : byte;
      xyz_ready    : boolean;

    PROCEDURE WRITE_MESSAGE(VAR UDFIL : TEXT; NR : BYTE);

    VAR i,j  : BYTE;
        S    : STRING;

        PROCEDURE LINIE;
        BEGIN
            WRITELN(UDFIL);
        END;

    BEGIN
        CASE NR OF
             1: BEGIN
                    linie;linie;
                    StringBox(UDFIL,'Mantel-Haenszel estimates and partial gamma coefficients');
                    linie;
                    writeln(udfil,'    Method    odds-ratio     low   high     ln(OR)   s.e.     Gamma   s.e');
                    writeln(udfil,'--------------------------------------------------------------------------');
                    linie;
                    writeln(udfil,'Mantel-Haenszel',mh(.1.):10:2,mh(.2.):7:2,mh(.3.):7:2,
                                                    mh(.4.):10:2,mh(.5.):7:2,
                                                    mh(.6.):10:2,mh(.7.):7:2);
                    writeln(udfil,'Partial gamma  ',Partgam(.1.):10:2,Partgam(.2.):7:2,Partgam(.3.):7:2,
                                                    Partgam(.4.):10:2,Partgam(.5.):7:2,
                                                    Partgam(.6.):10:2,Partgam(.7.):7:2);
                    linie;
                    linie;
                END;
         END;
    END;

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
     if (dimx<>2) or (dimx<>2) then
     begin
         okMessage('Variables has to be binary');
         goto slut;
     end;

     init_xyz_tabrecords(fra,til,fejl,xyz_ready);

     init_xyz(zmax,nz,dimx,dimy,tdimx,tdimy,xy_size,xyz_table,xyz_arrays,exyz_table,Exyz_arrays);

     Create_xyz_records(outfile,nvar,tabdata_ready,tabdata,tabdata_arrays,
                        tabcount,tabcount_arrays,ntabcells,false,success);

     nz:=dimz;     // two variables with the same number???

     FILLCHAR(TAB2,SIZEOF(TAB2),0);
     FILLCHAR(RTAB2,SIZEOF(RTAB2),0);

     GAMMATOT:=0;  PPQTOT:=0;  PMQTOT:=0;
     MHcounter:=0; MHdenominator:=0;
     a1:=0; a2:=0; a3:=0;
     b1:=0; b2:=0; b3:=0;

     C:=DIMX;   R:=DIMY;

     for i:=1 to 7 do
     begin
         mh(.i.):=0;
         partgam(.i.):=0;
     end;

     FOR Z:=1 TO NZ DO
     BEGIN

         TRANSFER_XYZ_SLICE(Z,XYZ_TABLE,XYZ_ARRAYS,
                            EXYZ_TABLE,EXYZ_ARRAYS,
                            TDIMX,TDIMY,XY_SIZE,TAB2,RTAB2);

         (*++++++++++++++++++++++++*)
         (*++ SEPARATE TEST HERE ++*)
         (*++++++++++++++++++++++++*)
         //writeln(outfile,z:4,nz:4,tab2(.3,3.):10);

         if tab2(.3,3.)>0 then
         begin

             a:=tab2(.1,1.);
             b:=tab2(.1,2.);
             cc:=tab2(.2,1.);
             d:=tab2(.2,2.);
             n:=tab2(.3,3.);
             //writeln(outfile,z:5,nz:5,a:7,b:5,cc:5,d:5,n:7);
             nsquare:=n*n;
             ad:=a*d;
             bc:=b*cc;
             //writeln(outfile,nsquare:7,ad:7,bc:7);
             //RCGAMMA(C,R,TAB2,GAMMA,PGAMMA,PPQ,PMQ,S,TRUE);
             //STOT:=STOT+S;

             Estimate_rcgamma(c,r,tab2,gamma,ppq,pmq,s,success);
             STOT:=STOT+S*ppq*ppq;

             PPQTOT:=PPQTOT+PPQ;
             PMQTOT:=PMQTOT+PMQ;

             mhCounter:=mhCounter+tab2(.1,1.)*tab2(.2,2.)/tab2(.3,3.);
             mhDenominator:=mhDenominator+tab2(.1,2.)*tab2(.2,1.)/tab2(.3,3.);

             a1:=a1+(a+d)*(ad/nsquare);
             a2:=a2+((a+d)*bc+(b+cc)*ad)/nsquare;
             a3:=a3+(b+cc)*(bc/nsquare);

             b1:=b1+ad/n;
             b3:=b3+bc/n;
         end;
     END; (*** OF A 2-WAY TABLE ***)

     IF PPQTOT>0 THEN
     BEGIN
         GAMMATOT:=PMQTOT/PPQTOT;
     END
     ELSE GAMMATOT:=2.0;

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


     write_message(outfile,1);

     //XYZ_GAMMA:=GAMMATOT;
     dispose_xyz(zmax,nz,tdimx,tdimy,xy_size,
                 xyz_table,xyz_arrays,
                 exyz_table,exyz_arrays);
slut:
END;  (*** of XYZ_MH ***)

//=======================================================================

procedure Mantel_Haenszel;
label slut,slut_hyp;
var n,hypnr,old_hypnr,Nevidence : integer;
    u1,u2                       : byte;
    save_it,replace_it          : hypbool_array;
    oldtest_found               : boolean;
    old_nsim                    : longint;
    s1,s2                       : string;

    //NTmpcells : Longint;
    //TmpData : Byte_data_array;
    //TmpCount : Longint_data_array;
    //TmpData_arrays, TMPcount_arrays : byte;

    PROCEDURE INEXPENSIVE_TEST;
    LABEL SLUT;
    var xy : Twobytes;
        i  : byte;
        fra,til,fejl : byte;
        xyz_ready : boolean;
        temp_success :boolean;
    BEGIN
	IF TABDATA_POSSIBLE AND NOT TABDATA_READY THEN
        BEGIN
            IF DATA_READY THEN
            DISPOSE_DATASET(DATA_READY,
                            NRESULTS3,RESULTS3_1,RESULTS3_2,
                            DATASET,DATASET_ARRAYS);

            INIT_TABDATA(NTABCELLS,NVAR,TABDATA,TABDATA_ARRAYS,
                         TABCOUNT,TABCOUNT_ARRAYS,TABDATA_POSSIBLE);

            GET_TABDATA(TABFILE,TABFILE_EXISTS,TABDATA_POSSIBLE,
                        NTABCELLS,NVAR,
                        TABDATA,TABDATA_ARRAYS,
                        TABCOUNT,TABCOUNT_ARRAYS,TABDATA_READY);

        END;

        define_control_variables(Nvar,DIM,Hypotese,hypnr,xy,success);
        // This procedure defines global xyz variables defined in SKvars

        Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready); // this defines to_be_used_in_xyz

        Qsort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                         NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,TMP_ready);


        xyz_mh(outfile,tabfile_exists,tabfile,tabdata_ready,tabdata_possible,
               tmpdata,tmpdata_arrays,tmpcount,tmpcount_arrays,ntmpcells,hypnr,hypotese);
        (*****
        CHEAP_BIGTEST(outfile,TABFILE_EXISTS,TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                EXACT,NSIM,SIMSTART,
                SEQUENTIAL,seq_P0,seq_ALPHA,seq_b,//seq_Beta,
                1.20, // 0.20
                RESULTS,SUCCESS);
        IF NOT SUCCESS THEN
        BEGIN
            okMessage('Cheap_BigTest failure');
            GOTO SLUT;
        END;
        ****)

        //XYZ_MH(outfile,dimx,dimy,tdimx,tdimy,xy_size,nxyz,xtype,ytype,xlabel,ylabel,vlabel,
        //       xyz_table,xyz_arrays,exyz_table,exyz_arrays,hypnr,hypotese);

        // XYZ_MH must work like Cheap_bigtest

    SLUT:
        //Dispose_Tabdata(Tmpdata,Tmpdata_arrays,Tmpcount,Tmpcount_arrays,temp_success);
    END; (* of INEXPENSIVE_TEST *)


    //...........................................................................

begin
    IF (NHYP=0) or not hypotheses_exist THEN
    BEGIN
        //okMessage('No hypotheses to be tested');
        goto slut;
    end;

    if print_results then DigramStatusPanel(4,'Testing');

    s1:='Hypothesis no. ';
    FOR HYPNR:=1 TO NHYP DO
    BEGIN
        str(hypnr,s2);
        s2:=s1+s2;
        if print_results then DigramStatusPanel(5,s2);
        SUCCESS:=TRUE;

        INEXPENSIVE_TEST;

        IF NOT SUCCESS THEN
        begin
            okMessage('test failure');
            GOTO SLUT_HYP;
        end;
    SLUT_HYP:
    END;
slut:
end; // of Mantel-Haenszel

//=======================================================================

procedure very_inexpensive_tests;
label slut,slut_hyp,tests_ready;
var n,hypnr,old_hypnr,Nevidence : integer;
    u1,u2                       : byte;
    save_it,replace_it          : hypbool_array;
    oldtest_found               : boolean;
    old_nsim                    : longint;
    s1,s2                       : string;

    //NTmpcells : Longint;
    //TmpData : Byte_data_array;
    //TmpCount : Longint_data_array;
    //TmpData_arrays, TMPcount_arrays : byte;

    PROCEDURE INEXPENSIVE_TEST;
    LABEL SLUT;
    var xy : Twobytes;
        i  : byte;
        fra,til,fejl : byte;
        xyz_ready : boolean;
        temp_success :boolean;
    BEGIN
	IF TABDATA_POSSIBLE AND NOT TABDATA_READY THEN
        BEGIN
            IF DATA_READY THEN
            DISPOSE_DATASET(DATA_READY,
                            NRESULTS3,RESULTS3_1,RESULTS3_2,
                            DATASET,DATASET_ARRAYS);

            INIT_TABDATA(NTABCELLS,NVAR,TABDATA,TABDATA_ARRAYS,
                         TABCOUNT,TABCOUNT_ARRAYS,TABDATA_POSSIBLE);

            GET_TABDATA(TABFILE,TABFILE_EXISTS,TABDATA_POSSIBLE,
                        NTABCELLS,NVAR,
                        TABDATA,TABDATA_ARRAYS,
                        TABCOUNT,TABCOUNT_ARRAYS,TABDATA_READY);

        END;

        define_control_variables(Nvar,DIM,Hypotese,hypnr,xy,success);

        Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready); // this defines to_be_used_in_xyz

        Qsort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                         NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,TMP_ready);

        if local then
        CHEAP_BIGTEST(outfile,TABFILE_EXISTS,TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                local_EXACT,local_NSIM,local_SIMSTART,
                local_SEQUENTIAL,local_P0,local_ALPHA,local_B,
                1.20, // 0.20
                RESULTS,SUCCESS)
        else
        CHEAP_BIGTEST(outfile,TABFILE_EXISTS,TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                EXACT,NSIM,SIMSTART,
                SEQUENTIAL,seq_P0,seq_ALPHA,seq_B,
                1.20, // 0.20
                RESULTS,SUCCESS);

        IF NOT SUCCESS THEN
        BEGIN
            okMessage('Cheap_BigTest failure');
            GOTO SLUT;
        END;
    SLUT:
        //Dispose_Tabdata(Tmpdata,Tmpdata_arrays,Tmpcount,Tmpcount_arrays,temp_success);
    END; (* of INEXPENSIVE_TEST *)

    //...........................................................................

begin
    IF (NHYP=0) or not hypotheses_exist THEN
    BEGIN
        //okMessage('No hypotheses to be tested');
        goto slut;
    end;

    str(nhyp,s1);
    DigramStatusPanel(4,'Testing '+s1+' hypotheses');

    s1:='Hypothesis no. ';

    FOR HYPNR:=1 TO NHYP DO
    BEGIN
        str(hypnr,s2);
        s2:=s1+s2;
        DigramStatusPanel(5,s2);
        FIND_VARIABLES(HYPNR,HYPOTESE,U1,U2);

        (************************)
        OLDTEST_FOUND:=FALSE;
        save_it(.hypnr.):=true;

        IF old_results_are_ready and PARAMETERS_UNCHANGED and not recalculate THEN
        RECOVER_OLD_TESTS(outfile,OLD_HYPNR,OLDTEST_FOUND,OLD_RESULTS_ARE_READY,
                          NVAR,HYPOTESE,HYPNR,RESULTS,OLD_NHYP,
                          OLD_HYPOTESE,OLD_RESULTS);

        IF OLDTEST_FOUND THEN
        BEGIN
            SAVE_IT(.HYPNR.):=FALSE;
            old_nsim:=round(old_results^(.old_hypnr,8.));
            if local_exact and (local_nsim>old_nsim)
            then replace_it(.hypnr.):=true
            else GOTO TESTS_READY;
        END;

        (************************)

        SUCCESS:=TRUE;
        INEXPENSIVE_TEST;

        IF NOT SUCCESS THEN
        begin
            okMessage('test failure');
            GOTO SLUT_HYP;
        end;

    TESTS_READY:

        //TAKE_CARE_OF_TESTRESULTS;
        Save_tests(next_hypnr,nhyp,hypotese,results,save_it,
                   old_nhyp,old_hypotese,old_results,
                   old_results_are_ready);


        if replace_it(.hypnr.) then
        begin
            writeln(outfile,'Results for hypothesis no. ',hypnr,' replaces old results no. ',old_hypnr);
            for n:=1 to 8 do old_results^(.old_hypnr,n.):=results(.hypnr,n.);
        end;

    SLUT_HYP:
    END;

    if local then
    begin
        if not two_sided then
        EXA_SUMMARY1(outfile,NVAR,NHYP,local_NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,local_EXACT)
        else
        EXA_SUMMARY1_2(outfile,NVAR,NHYP,local_NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,local_EXACT);
    end
    else
    begin
        if not two_sided then
        EXA_SUMMARY1(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT)
        else
        EXA_SUMMARY1_2(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT);
    end;

slut:
    DigramStatusPanel(5,s);
end; // of very inexpensive tests

//=======================================================================

procedure very_quick_tests;
label slut,slut_hyp,tests_ready;
var n,hypnr,old_hypnr,Nevidence : integer;
    u1,u2                       : byte;
    save_it,replace_it          : hypbool_array;
    oldtest_found               : boolean;
    old_nsim                    : longint;
    s1,s2                       : string;

    inrec,outrec                : ptr_stvector;
    sortsuccess                 : boolean;

    PROCEDURE INEXPENSIVE_TEST;
    LABEL SLUT;
    var xy : Twobytes;
        i  : byte;
        fra,til,fejl : byte;
        xyz_ready : boolean;
        temp_success :boolean;
    BEGIN
	IF TABDATA_POSSIBLE AND NOT TABDATA_READY THEN
        BEGIN
            IF DATA_READY THEN
            DISPOSE_DATASET(DATA_READY,
                            NRESULTS3,RESULTS3_1,RESULTS3_2,
                            DATASET,DATASET_ARRAYS);

            INIT_TABDATA(NTABCELLS,NVAR,TABDATA,TABDATA_ARRAYS,
                         TABCOUNT,TABCOUNT_ARRAYS,TABDATA_POSSIBLE);

            GET_TABDATA(TABFILE,TABFILE_EXISTS,TABDATA_POSSIBLE,
                        NTABCELLS,NVAR,
                        TABDATA,TABDATA_ARRAYS,
                        TABCOUNT,TABCOUNT_ARRAYS,TABDATA_READY);

        END;

        define_control_variables(Nvar,DIM,Hypotese,hypnr,xy,success);

        Init_XYZ_TABRECORDs(fra,til,fejl,xyz_ready); // this defines to_be_used_in_xyz

        Quicksort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                         NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,TMP_ready,
                         inrec,outrec,sortsuccess);

        if not sortsuccess then
        Qsort_xyz_records(Ntabcells,Nvar,TABdata,TABdata_arrays,TABcount,TABcount_arrays,
                          NTMPcells,TmpData,TMPdata_arrays,TMPcount,TMPcount_arrays,TMP_ready);



        if local then
        CHEAP_BIGTEST(outfile,TABFILE_EXISTS,TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                local_EXACT,local_NSIM,local_SIMSTART,
                local_SEQUENTIAL,local_P0,local_ALPHA,local_B,
                1.20, // 0.20
                RESULTS,SUCCESS)
        else
        CHEAP_BIGTEST(outfile,TABFILE_EXISTS,TABFILE,TABDATA_READY,TABDATA_POSSIBLE,
                TMPDATA,TMPDATA_ARRAYS,TMPCOUNT,TMPCOUNT_ARRAYS,
                NTMPCELLS,NVAR,DIM,VTYPE,VLABEL,
                HYPOTHESES_EXIST,NHYP,HYPOTESE,HYPNR,
                EXACT,NSIM,SIMSTART,
                SEQUENTIAL,seq_P0,seq_ALPHA,seq_B,
                1.20, // 0.20
                RESULTS,SUCCESS);

        IF NOT SUCCESS THEN
        BEGIN
            okMessage('Cheap_BigTest failure');
            GOTO SLUT;
        END;
    SLUT:
        //Dispose_Tabdata(Tmpdata,Tmpdata_arrays,Tmpcount,Tmpcount_arrays,temp_success);
    END; (* of INEXPENSIVE_TEST *)

    //...........................................................................

begin
    IF (NHYP=0) or not hypotheses_exist THEN
    BEGIN
        //okMessage('No hypotheses to be tested');
        goto slut;
    end;

    str(nhyp,s1);
    DigramStatusPanel(4,'Testing '+s1+' hypotheses');

    new(inrec);
    new(outrec);

    s1:='Hypothesis no. ';

    FOR HYPNR:=1 TO NHYP DO
    BEGIN
        str(hypnr,s2);
        s2:=s1+s2;
        DigramStatusPanel(5,s2);
        FIND_VARIABLES(HYPNR,HYPOTESE,U1,U2);

        (************************)

        OLDTEST_FOUND:=FALSE;
        save_it(.hypnr.):=true;
        replace_it(.hypnr.):=false;

        IF old_results_are_ready and PARAMETERS_UNCHANGED and not recalculate THEN
        RECOVER_OLD_TESTS(outfile,OLD_HYPNR,OLDTEST_FOUND,OLD_RESULTS_ARE_READY,
                          NVAR,HYPOTESE,HYPNR,RESULTS,OLD_NHYP,
                          OLD_HYPOTESE,OLD_RESULTS);

        if oldtest_found and (old_hypnr<1) or (old_hypnr>old_nhyp) then
        begin
            oldtest_found:=false;
            writeln(outfile,'Error recovering old test results');
        end;

        IF OLDTEST_FOUND THEN
        BEGIN
            SAVE_IT(.HYPNR.):=FALSE;
            old_nsim:=round(old_results^(.old_hypnr,8.));
            if local_exact and (local_nsim>old_nsim)
            then replace_it(.hypnr.):=true
            else
            GOTO TESTS_READY;
        END;

        (************************)

        SUCCESS:=TRUE;
        INEXPENSIVE_TEST;

        IF NOT SUCCESS THEN
        begin
            okMessage('test failure');
            GOTO SLUT_HYP;
        end;

    TESTS_READY:

        if replace_it(.hypnr.) then
        begin
            writeln(outfile,'Results for hypothesis no. ',hypnr,' replaces old results no. ',old_hypnr);
            for n:=1 to 8 do old_results^(.old_hypnr,n.):=results(.hypnr,n.);
        end
        else
        Save_tests(next_hypnr,nhyp,hypotese,results,save_it,
                   old_nhyp,old_hypotese,old_results,
                   old_results_are_ready);

    SLUT_HYP:
    END;

    if local then
    begin
        if not two_sided then
        EXA_SUMMARY1(outfile,NVAR,NHYP,local_NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,local_EXACT)
        else
        EXA_SUMMARY1_2(outfile,NVAR,NHYP,local_NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,local_EXACT);
    end
    else
    begin
        if not two_sided then
        EXA_SUMMARY1(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT)
        else
        EXA_SUMMARY1_2(outfile,NVAR,NHYP,NSIM,LEVNR,VLABEL,HYPOTESE,RESULTS,EXACT);
    end;

slut:
    Try
       dispose(inrec);
       dispose(outrec);
    except
       okMessage('Warning: memory was not released');
    end;
    DigramStatusPanel(5,s);
end; // of very quick tests

//------------------------------------------------------------------------------

BEGIN
    fillchar(bzeros,sizeof(bzeros),0);
    for i:=1 to dimtop do
    for j:=1 to dimtop do tab2zeros(.i,j.):=0;
end.  (** of SKbig3 **)
