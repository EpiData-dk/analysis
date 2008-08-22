{$O+,F+}

UNIT SKxyz2;


INTERFACE

Uses forms,SKTYPES,SKVARS,DGRvars,SKcat,SKCMDS,SKout,SKdata1,SKTAB1,
     SKSTAT,SKmca,SKRANDom,SKEXA1,skqsort,SKxyz1;


PROCEDURE Fit_xyz(var outfile : textfile;
                  XDIM,YDIM     : BYTE;
                  TDIMX,TDIMY   : BYTE;
                  XY_SIZE       : BYTE;
                  NZ            : LONGINT;
                  XTYPE,YTYPE   : BYTE;
                  XLABEL,YLABEL : CHAR;
                  VLABEL        : CVECTOR;
                  XYZ_TABLE     : LONGINT_DATA_ARRAY;
                  XYZ_ARRAYS    : BYTE;
                  EXYZ_table    : Real_data_array;
                  EXYZ_arrays   : byte;
                  maxstep       : integer;
                  deltalimit    : extended;
              var XYZ_fit       : real_data_array;
              var lr            : real;
              var df            : integer;
              var p             : real;
              var FittedGamma   : real;
              var p_fitgam      : real;
              var delta         : real;
                  printoutput   : boolean);

// this procedure fits the (XY,XZ,YZ) model to the table and test conditional independence

// we assume that all data arrays have been initialized and the length of XYZ_fit is EXYZ_arrays



IMPLEMENTATION

uses DIGRAM1f,SKbigtab,SKXYZ0;

PROCEDURE fit_XYZ;
label slut,itstep;
var x,y,nrows,ncols                             : byte;
    z,i,j,stepnr,n                              : integer;
    s,s1,s2,s3                                  : string;
    d,maxdelta,faktor                           : real;
    xy_fittab,zeroEtab,etab2,fittab2,xy_factors : Etwowaytablearray;
    xy_tab,tab2                                 : TwowaytableArray;

    procedure writeoutput(nr : integer);
    var x,y : integer;
        s   : string;
    begin
        if printoutput then
        case nr of
              0 : begin
                      print_xyz_table(outfile,'The table in fit_xyz',tdimx,tdimy,nz,xy_size,
                                      xyz_table,xyz_arrays,Exyz_table,Exyz_arrays);
                      print_exyz_table(outfile,'Etab in fit_xyz',tdimx,tdimy,nz,xy_size,
                                      xyz_table,xyz_arrays,Exyz_table,Exyz_arrays);
                      Stringbox(outfile,'The xy_table');
                      for y:=1 to tdimy do
                      begin
                          for x:=1 to tdimx do write(outfile,xy_tab(.x,y.):5);
                          writeln(outfile);
                      end;
                  end;                                             
              1 : begin
                      writeln(outfile);
                      writeln(outfile,'XYZ_fit:  Ncols = ',ncols,'  Nrows = ',nrows,'   df = ',df);
                      writeln(outfile,'          n = ',n,'    nz = ',nz);
                  end;
              2 : begin
                      str(stepnr,s);
                      print_exyz_table(outfile,'Fitted table - step '+s,tdimx,tdimy,nz,xy_size,
                                      xyz_table,xyz_arrays,XYZ_fit,Exyz_arrays);

                  end;
        end;
    end;

begin
    maxdelta:=100;
    for x:=1 to tdimx do
    for y:=1 to tdimy do
    begin
        zeroEtab(.x,y.):=0.0;
        xy_tab(.x,y.):=0
    end;

    // count xy_tab

    n := 0;

    for z:=1 to nz do
    begin
        transfer_xyz_slice(z,xyz_table,xyz_arrays,exyz_table,exyz_arrays,Tdimx,Tdimy,xy_size,tab2,etab2);
        for x:=1 to tdimx do
        for y:=1 to tdimy do
        xy_tab(.x,y.):=xy_tab(.x,y.)+tab2(.x,y.);
        n:=n+tab2(.tdimx,tdimy.);
    end;

    writeoutput(0);

    ncols:=0;
    for x:=1 to xdim do
    if xy_tab(.x,tdimy.)>0 then
    begin
        inc(ncols);
        //writeln(outfile,xy_tab(.x,tdimy.):5,ncols);
    end
    else
    begin
        //writeln(outfile,xy_tab(.x,tdimy.):5,ncols,'   empty!');
    end;

    nrows:=0;
    for y:=1 to ydim do
    if xy_tab(.tdimx,y.)>0 then inc(nrows);

    df:=(nrows-1)*(ncols-1);

    //writeOutput(1);

    if df<1 then goto slut;

    // initialize xyz_fit(.x,y,z.):=xy_tab(.x,y.)/nz;

    for z:=1 to nz do
    begin
        for x:=1 to tdimx do
        for y:=1 to tdimy do
        fittab2(.x,y.):=xy_tab(.x,y.)/nz;
        store_exyz_slice(z,xyz_fit,exyz_arrays,tdimx,tdimy,xy_size,fittab2);
    end;

    stepnr:=0;

itstep:
    inc(stepnr);

    xy_fittab:=zeroetab;

    for z:=1 to nz do
    begin
        // get tab2 and fittab2

        transfer_xyz_slice(z,xyz_table,xyz_arrays,xyz_fit,exyz_arrays,Tdimx,Tdimy,xy_size,tab2,fittab2);

        // rake columns

        for x:=1 to xdim do
        begin
            fittab2(.x,tdimy.):=fittab2(.x,1.);
            for y:=2 to ydim do fittab2(.x,tdimy.):=fittab2(.x,tdimy.)+fittab2(.x,y.);
            if fittab2(.x,tdimy.)=0 then faktor:=0.0 else faktor:=tab2(.x,tdimy.)/fittab2(.x,tdimy.);
            for y:=1 to ydim do fittab2(.x,y.):=fittab2(.x,y.)*faktor;
        end;

        // rake rows

        for y:=1 to ydim do
        begin
            fittab2(.tdimx,y.):=fittab2(.1,y.);
            for x:=2 to xdim do fittab2(.tdimx,y.):=fittab2(.tdimx,y.)+fittab2(.x,y.);
            if fittab2(.tdimx,y.)=0 then faktor:=0.0 else faktor:=tab2(.tdimx,y.)/fittab2(.tdimx,y.);
            for x:=1 to xdim do fittab2(.x,y.):=fittab2(.x,y.)*faktor;
        end;

        // add to xy_fittab

        for x:=1 to tdimx do
        for y:=1 to tdimy do
        xy_fittab(.x,y.):=xy_fittab(.x,y.)+fittab2(.x,y.);

        // store the raked two-way table

        store_exyz_slice(z,xyz_fit,exyz_arrays,tdimx,tdimy,xy_size,fittab2);
    end;

    // Rake across z-levels and evaluate maxdelta

    for x:=1 to xdim do
    for y:=1 to ydim do
    if xy_fittab(.x,y.)>0.0
    then xy_factors(.x,y.):=xy_tab(.x,y.)/xy_fittab(.x,y.)
    else xy_factors(.x,y.):=0.0;

    maxdelta:=0;

    for z:=1 to nz do
    begin
        transfer_xyz_slice(z,xyz_table,xyz_arrays,xyz_fit,exyz_arrays,tdimx,tdimy,xy_size,tab2,fittab2);
        for x:=1 to xdim do
        for y:=1 to ydim do fittab2(.x,y.):=fittab2(.x,y.)*xy_factors(.x,y.);

        for x:=1 to xdim do
        begin
            fittab2(.x,tdimy.):=fittab2(.x,1.);
            for y:=2 to ydim do fittab2(.x,tdimy.):=fittab2(.x,tdimy.)+fittab2(.x,y.);
            d:=abs(fittab2(.x,tdimy.)-tab2(.x,tdimy.));
            if d>maxdelta then maxdelta:=d;
        end;

        fittab2(.tdimx,tdimy.):=0;

        for y:=1 to ydim do
        begin
            fittab2(.tdimx,y.):=fittab2(.1,y.);
            for x:=2 to xdim do fittab2(.tdimx,y.):=fittab2(.tdimx,y.)+fittab2(.x,y.);
            d:=abs(fittab2(.tdimx,y.)-tab2(.tdimx,y.));
            if d>maxdelta then maxdelta:=d;
            fittab2(.tdimx,tdimy.):=fittab2(.tdimx,tdimy.)+fittab2(.tdimx,y.);
        end;

        store_exyz_slice(z,xyz_fit,exyz_arrays,tdimx,tdimy,xy_size,fittab2);
    end;

    writeOutput(2);

    //writeln(outfile,'*** maxdelta = ',maxdelta:10:3);

    if (maxdelta <= deltalimit) or (stepnr=maxstep) then goto slut else goto itstep;

slut:
    // calculate lr,df,p

    lr:=0.0;
    if df<1 then p:=1 else
    begin
        for z:=1 to nz do
        begin
            transfer_xyz_slice(z,xyz_table,xyz_arrays,exyz_table,exyz_arrays,tdimx,tdimy,xy_size,tab2,etab2);
            transfer_exyz_slice(z,xyz_fit,exyz_arrays,tdimx,tdimy,xy_size,fittab2);
            for x:=1 to xdim do
            for y:=1 to ydim do
            if (tab2(.x,y.)>0) then
            begin
                if (fittab2(.x,y.)>0) and (etab2(.x,y.)>0) then lr:=lr+tab2(.x,y.)*ln(fittab2(.x,y.)/etab2(.x,y.));
            end;
        end;
        lr:=2*lr;
        p:=pfchi(df,lr);
    end;

    // calculate fitteedGamma and p_fitgam

    delta:=maxdelta;

    //writeln(outfile,stepnr,' steps   delta = ',delta:10:5);
end;

BEGIN
end. (** of SKXYZ1 **)
