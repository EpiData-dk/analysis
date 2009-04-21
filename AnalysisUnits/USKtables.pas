unit USKtables;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ansDatatypes,
  uvectors, uvectorop, uoutput, ucommands, uepidatatypes, utables,sktypes ,skstat, skxyz;

//const
//type

function SKTableAnalysis( var Table     : TsumTable;
                          //Varnames      : TStrings;
                          //Nvar          : integer;
                          cmd: TCommand ;
                          var stat : string): boolean;

// function Outtable(C,R : byte; Tab2 : twowaytablearray; statistics : string): string;


// Nedenstående er standard implementeringen i epidata analysis i henhold til
// Object Orienteret kode stil.

type
  TSKTableAnalysis = class (TObject)
  private
    // Til interne beregninger
  protected
    // Ikke brugt
  public
    // Tilgængelige beregninger fra Tabel modulet.
    function CalcChi2(const TwowayTable: TTwoWayTable): EpiFloat;
    function ExactP(const TwowayTable: TTwoWayTable): EpiFloat;
    function GammaCI(const TwowayTable: TTwoWayTable; var LL, UL: EpiFloat): EPiFloat;
    constructor Create();
    destructor Destroy(); override;
  end;

var
  OSKTableAnalysis : TSKTableAnalysis;


implementation

uses UCmdProcessor,Ustatfunctions, UTableStat;


constructor TSKTableAnalysis.Create();
begin
  Inherited Create();
end;

destructor TSKTableAnalysis.Destroy();
begin
  Inherited Destroy();
end;

function TSKTableAnalysis.CalcChi2(const TwowayTable: TTwoWayTable): EpiFloat;
begin
  Result := 1.0;
end;

function TSKTableAnalysis.ExactP(const TwowayTable: TTwoWayTable): EpiFloat;
begin
  Result := 1.0;
end;

function TSKTableAnalysis.GammaCI(const TwowayTable: TTwoWayTable; var LL, UL: EpiFloat): EPiFloat;
begin
  LL := 1.0;
  UL := 1.0;
  Result := 1.0;
end;
//------------------------------------------------------------------------------


function SKtableAnalysis;
label slut;
var Vectorlist : TEpiVectors;
   v,Deselected,categ   : TepiVector;
   i,co,j,cco,varco,n,lbnr     : integer;
   np     :    EpiFloat;

   s,s1,s2                     : string;
   p                           : pchar;
   // arrays for tables

   tab2                   : TwowayTableArray;
   etab2                  : eTwowayTablearray;
   bottom,top,tal,oldtal  : Ivector;
   count,totcount         : integer;
   C,R,row,col            : Byte;

   gamma,pgamma,ppq,pmq,
   gammall, gammaul,
   ss,chi,pchi,stot,
   gammatot,u,pgammatot,
   absgammatot,
   totppq,totpmq,totchi,
   gamma1,ppq1,pmq1,
   var1,totvar1,sd1       : real;

   success                : boolean;
   df,totdf               : integer;
   NTABcells,cellno       : Integer; // the number of cells with at least 1 person
   Ncases                 : Integer; // the number of persons with compledte information on all variables

   Dim,Tdim               : Bvector;
   Vmin,Vmax              : Ivector;

   // declaration for xyz tables

   ZMAX,NZ             : Integer;
   DIMX,DIMY           : BYTE;
   TDIMX,TDIMY,XY_SIZE : BYTE;
   XYZ_TABLE           : LONGINT_DATA_ARRAY;
   XYZ_ARRAYS          : BYTE;
   EXYZ_TABLE          : REAL_DATA_ARRAY;
   EXYZ_ARRAYS         : BYTE;
   ztal,zdim           : bvector;
   ZFACTOR             : IVECTOR;
   ZFACTORSUM          : Integer;
   x,y                 : byte;
   z,znr,oldznr        : integer;

   // declarations for exact tests

   exafile : textfile;

   XTYPE,YTYPE   : BYTE;
   XLABEL,YLABEL : CHAR;
   VLABEL        : CVECTOR;
   Exact         : boolean;
   Nsim,Simstart : integer;

   SEQUENTIAL    : BOOLEAN;
   SEQ_P0        : REAL;
   SEQ_ALPHA     : REAL;
   SEQ_B         : REAL;
   EXACT_P_LEVEL : REAL;
   HYPNR         : INTEGER;
   HYPOTESE      : HYPARRAY;
   EFFECTIVE_N   : LONGINT;
   EFFECTIVE_SIM : INTEGER;
   RESULTS       : RESARRAY;

   oddsratio     : real;
   plusinfinity,minusinfinity : boolean;

   nvar : byte;

    opt: TEpiOption;

   Procedure InitializeTables;
   var i,j : byte;
   begin
       top:=zerointegers;
       bottom:=zerointegers;
       for i:=1 to dimtop do
       begin
           for j:=1 to dimtop do
           begin
               tab2(.i,j.):=0;
               etab2(.i,j.):=0.0;
           end;
       end;
   end;

   //---------------------------------------------------------------------------

   Function Recode(tal,bottom : integer):byte;
   begin
       recode:=tal-bottom+1;
   end;


  Procedure SaveTableEstimates(tab : TTwowaytable;
               var CHI, Pchi     : Real;
               var DF      : EpiInt;
               var Gam, pGam , GamLL, GamUL, odds : Real;
               Plusinfinity, Minusinfinity: Boolean);
   var
     inexact : Boolean;
   begin
       tab.df  := df;
       tab.chi := Chi;
       tab.pchi:= pchi;
       tab.gamma := gam;
       tab.pgamma2  := pgam;
       
   {    if plusinfinity then tab.oddsratio := 9999
           else if minusinfinity then tab.oddsratio := -9999
           else
             begin
               tab.oddsratio := odds;
               tab.ORUL :=otables.GetConfidenceIntervalsOR(tab,1,inexact);
               tab.ORLL :=otables.GetConfidenceIntervalsOR(tab,-1,inexact);
             end;
    }   //Tab.FishP2  := otables.GetFishersExact(tab);

       tab.GammaLL:=gamll;
       tab.GammaUL:=gamul;

   end;

   Procedure SaveSumTableEstimates(
               var CHI, Pchi : Real;
               var DF      : EpiInt;
               var Gam, pGam , GamLL, GamUL, odds : Real;
               Plusinfinity, Minusinfinity: Boolean );
   begin
       // dm.info('Check Summary Df, Chi<sup>2</sup> and p - could have a problem this version');  //TODO - check Df in n*k tables

       table.SumDF := df;
       table.Sumchi := Chi;
       table.pSumChi:= pchi;
       table.PartGamma := gam;
       table.pPartgamma2  := pgam;
       if plusinfinity then table.SumOR := 9999
           else if minusinfinity then table.SumOR := -9999
           else table.SumOR  := odds;
       table.PartGammaUL:=GamUL;
       table.PartGammaLL:=GamLL;
   end;

   //    SaveSumTabelExactEstimates(effective_sim, MCChi,MCDf, MCp, MCGamma,MCpGamma);

   Procedure SaveSumTableExactEstimates(nsim : epiint; var cHI, Pchi, DF, Gam, pGam : Real);
   begin
       table.NSIM := round(nsim);
       table.SumDF := round(df);
       table.MCchi := Chi;
       table.pMCChi:= pchi;
       table.MCgamma := gam;
       table.pMCpgamma  := pgam;
   end;

   //---------------------------------------------------------------------------

   Procedure Analyze_twowaytables(var s : string; ThisTable : integer);
   label slut;
   var i,j : byte;
    //gammall, gammaul : Real;
   begin

       (******
       for i:=1 to c do
       for j:=1 to r do
       begin
           tab2(.c+1,j.):=tab2(.c+1,j.)+tab2(.i,j.) ;
           tab2(.i,r+1.):=tab2(.i,r+1.)+tab2(.i,j.);
           tab2(.c+1,r+1.):=tab2(.c+1,r+1.)+tab2(.i,j.);
       end;
       *******)


       if tab2(.c+1,r+1.)=0 then
       begin
           dm.error('%s The table appears to be empty!', [s], 40001);
           goto slut;
       end;

       for i:=1 to c+1 do
       for j:=1 to r+1 do etab2(.i,j.):=tab2(.c+1,j.)*tab2(.i,r+1.)/tab2(.c+1,r+1.);

       rcgamma(c,r,tab2,gamma,pgamma,ppq,pmq,ss,true);

       Estimate_rcgamma(c,r,tab2,gamma1,ppq1,pmq1,var1,success);

       if success and (var1>0) then sd1:=sqrt(var1) else sd1:=sqrt(var1);

       // TODO: CI for gamma:

       gammall := gamma-1.96*sd1;  gammaul := gamma+1.96*sd1;

       rcchi(c,r,tab2,etab2,chi,df,pchi,true);

       if df>0 then
       begin
          //gammall := 0;  gammaul := 0;
              //
 {          s:=s+'Chi=';
           str(chi:7:2,s1); s:=s+s1+'  df(';
           str(df,s1); s:=s+s1+')  p=';
           str(pchi:7:4,s1); s:=s+s1+'   - gamma=';
           str(gamma:5:2,s1); s:=s+s1+'  p=';
           str(pgamma:7:4,s1); s:=s+s1;
           if (c=2) and (r=2) then s:=s+'    Odds ratio ='
           else s:=s+'    Generalized odds ratio ='; }

           gamma2oddsratio(gamma,oddsratio,plusinfinity,minusinfinity);

{           if plusinfinity then s:=s+' infinity' else
           if minusinfinity then s:=s+' minus infinity' else
           begin
               str(oddsratio:6:2,s1); s:=s+s1;
           end; }
         end
       else s:=s+'   No degrees of freedom';

//       outtable(c,r,tab2,s);

       // transfer to sumtable

       if (thistable = 0)
           then SavetableEstimates(table.margtable,Chi,pChi,df, gamma, pgamma, gammaLL, gammaUL, Oddsratio, Plusinfinity, Minusinfinity)
           else SavetableEstimates(Table.Subtable[thistable],Chi,pChi,df, gamma, pgamma, gammaLL, gammaUL, Oddsratio,Plusinfinity, Minusinfinity);
    slut:
  end;

   procedure Collect_Twowayresults;
   begin
       totchi:=totchi+chi;
       totdf:=totdf+df;
       totppq:=totppq+ppq;
       totpmq:=totpmq+pmq;
       stot:=stot+ss;
       TOTvar1:=Totvar1+ppq*var1;
   end;

begin
    // analysis of the marginal 2-way table

    InitializeTables;

    R:=table.MargTable.RowCount;
    C:=table.MargTable.ColumnCount;

    for i:=1 to c do
    for j:=1 to r do tab2(.i,j.):=table.MargTable.Cell(.i,j.).n;

    for i:=1 to c do tab2(.i,r+1.):=table.MargTable.ColTotal(.i.);
    for j:=1 to r do tab2(.c+1,j.):=table.MargTable.rowTotal(.j.);

    tab2(.c+1,r+1.):=table.MargTable.Total;
    totcount:=table.MargTable.Total;
    str(totcount,s);
    dm.Sysinfo('<hr><br>SKtableanalysis v1.0 <br>Total number of persons = '+s);

{    dm.Info('The table');
    for j:=1 to r+1 do
    begin
        s:='';
        for i:=1 to c+1 do
        begin
            str(tab2(.i,j.):7,s1); s:=s+s1;
        end;
        dm.Info(s);
    end;
    dm.Info('');
 }

 //   s:='Marginal association.';

    if tab2(.c+1,r+1.)=0 then
    begin
        dm.info('%s The table appears to be empty!', [''], 116001);
        goto slut;
    end;

    Analyze_twowaytables(s,0); // marginal table
 //   if ((r = 2) and (c=2) and (cmd.ParamByName['EX'] <> nil)) then  // do a fishers exact test
//           Table.Margtable.FishP2 := oTables.GetFishersExact(Table.Margtable);

    dimx:=c;
    dimy:=r;

    init_xyz(zmax,nz,dimx,dimy,tdimx,tdimy,xy_size,
             xyz_table,xyz_arrays,exyz_table,exyz_arrays);

    nz:=table.TableCount;
    if nz=0 then goto slut;

    z       :=0;   // this is the number of the slice
    totchi  :=0;
    totppq  :=0;
    totpmq  :=0;
    totdf   :=0;
    stot    :=0;
    totvar1 :=0;

    if nz > 1 then
    begin     // stratified analysis
     for z:=1 to nz do
      begin
//        str(z,s); s:='strata '+s;  dm.Info(s);
        InitializeTables;

        for x:=1 to c do
          for y:=1 to r do tab2(.x,y.):=Table.SubTable(.z.).Cell(.x,y.).N;

        for x:=1 to c do
          tab2(.x,r+1.)  :=Table.SubTable(.z.).ColTotal(.x.);

        for y:=1 to r do
          tab2(.c+1,y.)  :=Table.SubTable(.z.).RowTotal(.y.);

        tab2(.c+1,r+1.):=Table.SubTable(.z.).total;

        Analyze_Twowaytables(s,z);
        Collect_Twowayresults;

        // save the slice
        Store_xyz_slice(z,xyz_table,xyz_arrays,exyz_table,exyz_arrays,
                    tdimx,tdimy,xy_size,tab2,etab2);
        if ((r = 2) and (c=2) and (cmd.ParamByName['EX'] <> nil)) then  // do a fishers exact test
//           Table.Subtable(.z.).FishP2 :=  oTables.GetFishersExact(Table.SubTable(.z.));
       end;  // strata
    end; // stratified analysis

////---------------------------------------------------------------------------------------------------------///
///-----Copy results to overall table  a---------------------------------------------------------------------///
//    s:='----------------------------------------';
    str(nz,s);
    Calculate_gamma_statistics(totpmq,totppq,gammatot,absgammatot,stot,u,pgammatot);
    PCHI:=PFCHI(totDF,totCHI);
    gamma2oddsratio(gammatot,oddsratio,plusinfinity,minusinfinity);
    pgammatot := 2*pgammatot ; //Two sided values

    if totppq>0 then totvar1:=totvar1/totppq else totvar1:=0;
    // todo: add gamma ci here:
    gammall := gammatot-1.96*sqrt(totvar1); gammaUL := gammatot+1.96*sqrt(totvar1);
    SaveSumTableEstimates(TotChi, pChi, TotDf, Gammatot,PGammatot,GammaLL, GammaUL,oddsratio,plusinfinity,minusinfinity);

////---------------------------------------------------------------------------------------------------------///
    if dm.showsysinfo then
    begin
      dm.sysInfo('<hr>Global test results collected over '+s+' strata');
      str(totchi:7:1,s);
      str(pchi:6:3,s1);
      str(totdf,s2);
      dm.SysInfo('Chi('+s2+')  = '+s+'   p = '+s1);
      str(gammatot:6:3,s);
      str(2*pgammatot:6:3,s1);
      dm.SysInfo('Partial gamma = '+s+'   2-sided p = '+s1);
      if plusinfinity then
        dm.SysInfo('Generalized MH = infinity')
      else
      if minusinfinity then dm.SysInfo('Generalized MH = minus infinity')
        else
          begin
              str(oddsratio:6:2,s);
              dm.sysInfo('Generalized MH = '+s);
          end;
    end; // showsysinfo
////---------------------------------------------------------------------------------------------------------///

    // exact test follows for complete table system of all subtables:
  if (cmd.ParamByName['EX'] <> nil) and (nz > 1) and  (cmd.ParamByName['CT'] = nil) {and (cmd.ParamByName['OA'] = nil)} THEN
  begin
  // open exafile

    XTYPE:=3;
    YTYPE:=3;
    XLABEL:='X';
    YLABEL:='Y';
    vlabel(.1.):='X';
    vlabel(.2.):='Y';
    vlabel(.3.):='Z';
    Nvar:=3;

    (****
    vlabel(.4.):='D';
    vlabel(.5.):='E';
    vlabel(.6.):='F';
    vlabel(.7.):='G';
    vlabel(.8.):='H';
    ******)

    Exact:=true;
    if (DM.GetOptionValue('RANDOM SIMULATIONS', opt)) THEN   NSIM := strtoint(opt.value);
//    Nsim := 1000;   // so far
    if (DM.GetOptionValue('RANDOM SEED', opt)) THEN   SimStart := strtoint(opt.value);
//     SIMstart := 9; // so far

    SEQUENTIAL:=false;
    SEQ_P0:= 0.001;
    SEQ_ALPHA:= 0.05;
    SEQ_B:= 1;
    EXACT_P_LEVEL:=0.05;

    HYPNR:=1;
    for i:=1 to nvar do hypotese(.1,i.):=2;
    HYPOTESE(.1,1.):=1;
    HYPOTESE(.1,2.):=1;

    //str(tdimx,s); dm.Info('tdimx = '+s);
    //str(tdimy,s); dm.Info('tdimy = '+s);
    //str(xy_size,s); dm.Info('xy_size = '+s);

    cheap_xyz_test(exafile,dimx,dimy,tdimx,tdimy,xy_size,nz,
                   Xtype,Ytype,Xlabel,Ylabel,Vlabel,
                   XYZ_table,XYZ_arrays,EXYZ_table,EXYZ_arrays,
                   Exact,Nsim,simstart,sequential,seq_p0,seq_alpha,seq_b,exact_p_level,
                   Nvar,Hypnr,hypotese,effective_n,effective_sim,results);

//    SaveSumTabelExactEstimates(effective_sim, MCChi,MCDf, MCp, MCGamma,MCpGamma);
    SaveSumTableExactEstimates( effective_sim,results(.1,1.),results(.1,4.),results(.1,2.),results(.1,5.),results(.1,9.));

    if dm.showsysinfo then
    begin
      s:='----------------------------------------';
      s:=s + '<br>Exact Monte Carlo tests. Nsim = ';
      str(effective_sim,s1); s:=s+s1;
      s:=s + '<br>Chi square = ';
      str(results(.1,1.):6:1,s1);  s:=s+s1+'  df =';
      str(results(.1,2.):4:0,s1);    s:=s+s1+'   p =';
      str(results(.1,4.):6:3,s1);  s:=s+s1;
      s:=s + '<br>Gamma      = ';
      str(results(.1,5.):6:3,s1);  s:=s+s1+'  p = ';
      str(results(.1,9.):6:3,s1);  s:=s+s1;
      dm.sysInfo(s);
    end;
  end;  // Moved this end from below dispose_xyz to ALWAYS release memory.

  // dispose XYZ tables
  dispose_xyz(zmax,nz,tdimx,tdimy,xy_size,xyz_table,xyz_arrays,exyz_table,exyz_arrays);

  {  closefile(exafile);
      reset(exafile);
      while not eof(exafile) do
      begin
        readln(exafile,s);
        dm.info('sktables ' + s);
      end;}

slut:

end;

//------------------------------------------------------------------------------

{function  Outtable(C,R : byte; Tab2 : twowaytablearray; statistics : string): string;  //was boolean
var
   s : string;
   co,i,j,total  : integer;
   v : TEpiVector;
   tab : TStatTable;

begin
  tab := nil;
try
  tab:=dm.Outputlist.NewTable(c+2,r+4);
  tab.TableType := sttNormal;    // possibly sttPercents also ?

  tab.cell(.c+2,2.):='total';

  tab.cell(.2,1.):='var1';

  tab.cell(.1,r+3.):='total';

  for i:=1 to c+1 do
  for j:=1 to r+1 do
  if (tab2(.i,r+1.)>0) and (tab2(.c+1,j.)>0) then
  begin
      str(tab2(.i,j.),s);
      tab.cell(.i+1,j+2.):=s;
  end;

  tab.cell(.1,1.):='var2';
  dm.info(statistics);
  dm.codemaker.OutputTable(tab,statistics);
  dm.Sendoutput;
  result := statistics;
finally
   tab.free;
end;
end;
}

end.
