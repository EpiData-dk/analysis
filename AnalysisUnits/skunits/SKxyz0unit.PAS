{$O+,F+}

UNIT SKxyzUnit0;

// generates random XYZ tables

INTERFACE

Uses forms,SKTYPES,SKVARS,DGRvars,SKcat,SKCMDS,SKout,SKdata1,SKTAB1,
     SKSTAT,SKmca,SKRANDom,SKEXA1,skqsort,SKXYZ1;


(*++++++++++++++++++++++++++++++++++++++++++++++++++*)
(* NOTICE THAT THIS UNIT USES THE FOLLOWING GLOBAL  *)
(* VARIABLES DEFINED IN VARSUNIT:                   *)
(*                DIMX,DIMY,NZ                      *)
(*++++++++++++++++++++++++++++++++++++++++++++++++++*)

Function Stop_exact_tests(sequential : boolean;
                          Chi_sprt_status,Gamma_sprt_status,sim : integer;
                          var Nsim : integer): Boolean;


Procedure Generate_XYZ_table(var outfile         : TextFile;
                             DIMx,DIMy,TdimX,TdimY,XY_size : byte;
                             NZ                  : longint;
                             XYZtable            : Longint_data_array;
                             XYZ_arrays          : byte;
                             EXYZtable           : Real_data_array;
                             EXYZ_arrays         : byte;
                         var NewXYZ              : LongInt_Data_array;
                         Var ENewXYZ             : Real_data_array);

// we assum4e that NewXYZ has been initilalized and the NEW.._rrays = YYZ_arrays


IMPLEMENTATION

Function Stop_exact_tests(sequential : boolean;
                          Chi_sprt_status,Gamma_sprt_status,sim : integer;
                          var Nsim : integer): Boolean;
begin
    IF SEQUENTIAL
    AND (CHI_SPRT_STATUS<>0)
    AND (GAMMA_SPRT_STATUS<>0)
    THEN
    BEGIN
        NSIM:=SIM;
        Stop_exact_tests:=true;
    END
    else Stop_exact_tests:=false;
end;

//-------------------------------------------------------------------

Procedure Generate_XYZ_table;

// we assum4e that NewXYZ has been initilalized and the NEW.._rrays = YYZ_arrays
// and that randomized has been executed before this happens

var i,j,x,y,c,r : byte;
    z       : integer;
    ETAB2   : EtwowaytableArray;
    Newtab  : TwowayTableArray;
    ss,s1       : string;
begin
     //str(exyz_table(.1.)^(.1.):6:2,s1);
     //okmessage('the first cell (8) = '+s1);

    for z:=1 to NZ do
    begin
        //okmessage('transfer exyz');
        //str(exyz_arrays,s); okmessage(s+' exyz_arrays');
        //str(exyz_table(.1.)^(.1.),s); okmessage('the first cell '+s);

        Transfer_EXYZ_slice(z,EXYZtable,EXYZ_arrays,TdimX,TdimY,XY_size,Etab2);

        //okmessage('generate');

        Gentab0(outfile,dimx,dimy,Etab2,Newtab);
        //okmessage('store');

        store_XYZ_slice(z,NewXYZ,XYZ_arrays,ENewXYZ,EXYZ_arrays,TDIMX,TDIMY,XY_Size,Newtab,Etab2);
    end;
end;

BEGIN
end. (** of SKXYZ0 **)
