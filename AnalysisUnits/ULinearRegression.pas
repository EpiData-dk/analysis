unit ULinearRegression;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UVectorOp, UOutput, UCommands, Windows, UAnaToken,
     RegComp;

// ============================================================================
// Declaration of:
// Main methodes, functions and procedures.
// ============================================================================

type

  TLinearRegression = class
  private
    // For internally available methods!
    function InternalEpiRegress(Varnames: TStrings;Dataframe:TEpiDataFrame; var RegObj : TRegComp; var MVStatRec: TMultStatRec): boolean;
  protected
    //
  public
    // For Externaly available methods.
    function EpiRegress(Varnames: TStrings;Dataframe:TEpiDataFrame;var RegRec: TMultStatRec): boolean;
    function OutRegress(Varnames: TStrings;df: TEpiDataFrame;const RegRec: TMultStatRec; Ntotal: string): boolean;
  end;


var
  OLinearRegression: TLinearRegression;

implementation

uses UCmdProcessor, UFormats, UStatFunctions, TypInfo;


// ============================================================================
// Public methodes.
// ============================================================================
function TLinearRegression.EpiRegress(Varnames: TStrings;Dataframe:TEpiDataFrame;var RegRec: TMultStatRec): boolean;
var
  RegObj : TRegComp;
begin
  Result := InternalEpiRegress(Varnames,Dataframe,RegObj,RegRec);
  if Assigned(RegObj) then FreeAndNil(RegObj);
end;

function TLinearRegression.OutRegress(Varnames: TStrings;df: TEpiDataFrame;const RegRec: TMultStatRec; NTotal : string):boolean;
var
  i,co,j,cco,varco       : integer;
  v,categ   : TepiVector;
  s,st         :string;
  tab : TStatTable;
  conlim : EpiFloat;
begin
  varco := varnames.count;
  tab :=nil;
try

//                        7 cols    one row per parameter + 8 others
  tab := dm.OutputList.NewTable(7,RegRec.K+8);
  tab.TableType := sttNormal;
(*
     Source |       SS       df       MS              Number of obs =     800
-------------+------------------------------           F(  2,   797) =   40.24
       Model |  9788774.39     2  4894387.20           Prob > F      =  0.0000
    Residual |  96940373.6   797  121631.585           R-squared     =  0.0917
-------------+------------------------------           Adj R-squared =  0.0894
       Total |   106729148   799  133578.408           Root MSE      =  348.76
*)
//first row
//        col, row
  tab.cell[1,1]:='Source';
  tab.cell[2,1]:='Sum Sq';
  tab.cell[3,1]:='Mean Sq';
  tab.cell[4,1]:='df';
  tab.cell[5,1]:='';
  tab.cell[6,1]:='Number of obs';
  tab.cell[7,1]:=inttostr(RegRec.n);
//second row --model
//        col, row
  tab.cell[1,2]:='Model';
  tab.cell[2,2]:=Epiformat(RegRec.SSR);
  tab.cell[3,2]:=Epiformat(RegRec.MSR);
  tab.cell[4,2]:=inttostr(RegRec.DFModel);
  tab.cell[5,2]:='';
  tab.cell[6,2]:=format('F(%d,%d)',[RegRec.DFModel,RegRec.DFError]);
  tab.cell[7,2]:=Epiformat(RegRec.FStat);

//Third row --> residual
//        col, row
  tab.cell[1,3]:='Residual';
  tab.cell[2,3]:=Epiformat(RegRec.SSE);
  tab.cell[3,3]:=Epiformat(RegRec.MSE);
  tab.cell[4,3]:=inttostr(RegRec.DFError);
  tab.cell[5,3]:='';
  tab.cell[6,3]:='Prob > F';
  tab.cell[7,3]:=Epiformat(fdist(RegRec.FStat,RegRec.DFModel,RegRec.DFError));
//Forth row --> Total
  tab.cell[1,4]:='Total';
  tab.cell[2,4]:=Epiformat(RegRec.SST);
  tab.cell[3,4]:=Epiformat(RegRec.SST/RegRec.DFTotal);
  tab.cell[4,4]:=inttostr(RegRec.DFTotal);
  tab.cell[5,4]:='';
  tab.cell[6,4]:='R-squared';
  tab.cell[7,4]:=Epiformat(RegRec.R2);
//Fifth row --> Empty
  for i :=1 to 5 do
    tab.cell[i,5]:='';
  tab.cell[6,5]:='Root MSE';
  tab.cell[7,5]:=Epiformat(RegRec.RMSE);
//Sixth row --> Titles
    for i :=1 to 7 do
    tab.cell[i,6]:='';

//Seventh row> Titles
  tab.cell[1,7]:='Variable';
  tab.cell[2,7]:='Beta';
  tab.cell[3,7]:='LCL';
  tab.cell[4,7]:='UCL';
  tab.cell[5,7]:='SE';
  tab.cell[6,7]:='t';
  tab.cell[7,7]:='P>|t|';

  for i:=2 to RegRec.K+1 do
  begin
     if i <= varco then
        tab.cell[1,i+6]:=varnames[i-1];
      tab.cell[2,i+6]:=Epiformat(RegRec.beta[i]);
      conlim:=GetMLRConflim(RegRec.sebeta[i],RegRec.dftotal);
      tab.cell[3,i+6]:=Epiformat(RegRec.beta[i]-conlim);
      tab.cell[4,i+6]:=Epiformat(RegRec.beta[i]+conlim);
      tab.cell[5,i+6]:=Epiformat(RegRec.sebeta[i]);
      tab.cell[6,i+6]:=Epiformat(RegRec.Tvalue[i]);
      tab.cell[7,i+6]:=Epiformat(tdist(RegRec.Tvalue[i],RegRec.n));
      // result variables:
      dm.AddResult('$var'+inttostr(i-1),EpiTyString,varnames[i-1],0,0);
      dm.AddResult('$beta'+inttostr(i-1),EpiTyFloat,RegRec.Beta[i],0,0);
      dm.AddResult('$betaUCi'+inttostr(i-1),EpiTyFloat,RegRec.beta[i]+conlim,0,0);
      dm.AddResult('$betaLCi'+inttostr(i-1),EpiTyFloat,RegRec.beta[i]-conlim,0,0);
      dm.AddResult('$pbeta'+inttostr(i-1),EpiTyFloat,tdist(RegRec.Tvalue[i],RegRec.n),0,0);
  end;
  tab.cell[1,i+6]:='Intercept';
  tab.cell[2,i+6]:=Epiformat(RegRec.beta[1]);
  conlim:=GetMLRConflim(RegRec.sebeta[1],RegRec.dftotal);
  tab.cell[3,i+6]:=Epiformat(RegRec.beta[1]-conlim);
  tab.cell[4,i+6]:=Epiformat(RegRec.beta[1]+conlim);
  tab.cell[5,i+6]:=Epiformat(RegRec.sebeta[1]);
  tab.cell[6,i+6]:=Epiformat(RegRec.Tvalue[1]);
  tab.cell[7,i+6]:=Epiformat(tdist(RegRec.Tvalue[1],RegRec.n));

  dm.AddResult('$ICbeta',EpiTyFloat,RegRec.Beta[1],0,0);
  dm.AddResult('$ICbetaUCi',EpiTyFloat,RegRec.beta[1]+conlim,0,0);
  dm.AddResult('$ICbetaLCi',EpiTyFloat,RegRec.beta[1]-conlim,0,0);
  dm.AddResult('$ICpbeta',EpiTyFloat,tdist(RegRec.Tvalue[1],RegRec.n),0,0);

  dm.AddResult('$N', EpiTyInteger, (RegRec.n), 5, 0);
  dm.AddResult('$NTotal', EpiTyInteger, (Ntotal), 5, 0);

  dm.CodeMaker.OutputTable(tab,'Total N = ' + Ntotal + ' Included: N= ' + Inttostr(RegRec.n));
  dm.Sendoutput;
  finally
    tab.free;
  end;
end;


// ============================================================================
// Private/Protected methodes.
// ============================================================================

function TLinearRegression.InternalEpiRegress(Varnames: TStrings;Dataframe:TEpiDataFrame; var RegObj : TRegComp; var  MVStatRec: TMultStatRec): boolean;
var
   Vectorlist :TEpiVectors;
   y,v,categ   : TepiVector;
   i,co,j,cco,varco,n       : integer;
   t, p                   : EpiFloat;
begin
    Vectorlist:=nil;
    RegObj     :=nil;
try
    varco := Varnames.Count;
    if varco < 2 then
       dm.error('Minimum 2 variables for %s', ['Regresse'], 113006);
    Vectorlist:= Dataframe.GetVectorListByName(varnames);
//check for data types
   for i := 0 to varco-1 do
      if Vectorlist[i].datatype in  [EpiTyDate,EpiTyUppercase,EpiTyString] then
           dm.error('Only numeric variables are allowed', [], 114001);
try
  RegObj := TRegComp.create(nil);
  co := Dataframe.rowcount;
  RegObj.MVPrepare;  // Initialize container array(s)
  y := Vectorlist[0];
  n:=0;
  for i:= 1 to co do
  begin
      if (not dataframe.selected[i]) then continue;
      if (y.IsMissing[i] or y.IsMissingValue[i]) then continue;
      for j:= 1 to varco-1 do
         if Vectorlist[j].IsMissing[i] or Vectorlist[J].IsMissingValue[i] then
            break
          else               //col,row
           RegObj.PRegressData^[j,n]:=  Vectorlist[j].AsFloat[i];
      if j =varco then {no missing values found}
      begin
        RegObj.Y[n]:=y.AsFloat[i] ;
        inc(n);
      end;
   end;
   MVStatRec:= RegObj.MVRegression(RegObj.PRegressData,RegObj.Y,varco-1,n);
except
  RegObj.free;
  RegObj     :=nil;
end;
finally
   Vectorlist.free;
end;
end;

end.
