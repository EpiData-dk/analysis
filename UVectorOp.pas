unit UVectorOp;

{ this unit contains general functions for statistics}

interface
uses sysutils,classes,Math,Uvectors,ansDataTypes, UstatFunctions, RegComp, EpiInfoSTATS;

  function EpiSum(pV:TObject):Epifloat;
  function EpiSummary(Varnames: TStrings;Dataframe:TEpiDataFrame; var Descriptors: TEpiDescriptivesArray;var df : TEpiDataFrame): boolean;
  function EpiMeans(Varnames: TStrings;Dataframe:TEpiDataFrame; var Descriptors: TEpiDescriptivesArray;var df : TEpiDataFrame): boolean;
  function IntervalDescrptives(const df: TEpiDataFrame;var Descriptors: TEpiDescriptivesArray):Epibool;
  function FreqDescrptives(const df: TEpiDataFrame;var Descriptors: TEpiFreqDescriptiveArray):Epibool;

  function GetFreqDescriptiveData(Descriptors: TEpiFreqDescriptiveArray;var GrandTotal:EpiFloat):Epibool;

  function ttest(Descriptors: TEpiDescriptivesArray; var t,df,prob: Epifloat):EpiBool;
  function pttest(Descriptors: TEpiDescriptivesArray; var t,df,prob: Epifloat):EpiBool;

  function ANOVA(Descriptors: TEpiDescriptivesArray; var AnovaRec: TEpiAnovaRecord):EpiBool;
  function Bartlett (Descriptors: TEpiDescriptivesArray;AnovaRec: TEpiAnovaRecord;var chi,pval:EpiFloat):EpiBool;

  function GetMLRConflim(se: EpiFloat; df : integer): EpiFloat;

  function EpiCorrelate(Varnames: TStrings;Dataframe:TEpiDataFrame;var CorMatrix : XMatrix): boolean;

implementation

uses UVectorVar,Ucmdprocessor, GeneralUtils;

function EpiSum(pV:TObject):Epifloat;
var
   Varnames: TStringList;
   Descriptors: TEpiDescriptivesArray;
   df : TEpiDataFrame;
   v  :TEpiVector;
begin
try
  result:=NA_FLOAT;
//  vv:=TVectorVar(pv);
  v :=TEpiVector(pv);
  Varnames:= TStringList.create;
  Varnames.Add(V.name);
  if (v.Vectors<>nil) and (V.Vectors.DataFrame<>nil) then
     EpiMeans(Varnames,V.Vectors.DataFrame,Descriptors,df);
  if length(Descriptors) > 0 then
    result:= Descriptors[0].sum;
finally
   Varnames.free;
   //df.free;
end;
end;

function IntervalDescrptives(const df: TEpiDataFrame;var Descriptors: TEpiDescriptivesArray):Epibool;
  LABEL Stop;
var
  i,co,temporary:  integer;
  selected,categ,V   : TEpiVector;
  catVal           : integer;
  catNo, imin, imax,imed,i5,i10,i25, i75, i90,i95            : integer;
  CatCreated   :boolean;
  val,sum,SumSq,s,p,ave,adev,sdev,svar,skew,curt,f: EpiFloat;

  function getpercentile(min,max,d: integer):EpiFloat ;
  var
    w : extended;
    ix: integer;
  begin
    // Try new method.
    ix := PercentileIndex(max-min+1, d/100, w);
    if w = 0 then
      result:= V.AsFloat[ix + min-1]
    else
      result := V.AsFloat[ix + min-1] + (V.AsFloat[ix+ min]-V.AsFloat[ix+ min-1])*w;
  end;

  procedure FillDescriptors();
  var
    j: integer;
  begin
    Descriptors[catNo].level:=categ.AsString[imax];
    Descriptors[catNo].nmn:=imax-imin+1;
    Descriptors[catNo].max:=V.asFloat[imax];
    Descriptors[catNo].min:=V.asFloat[imin];

    // Now get the percentiles.
    Descriptors[catno].p5  :=  getpercentile(imin,imax,5);
    Descriptors[catno].p10 :=  getpercentile(imin,imax,10);
    Descriptors[catno].p25 :=  getpercentile(imin,imax,25);
    Descriptors[catno].med := getpercentile(imin,imax,50);
    Descriptors[catno].p75 :=  getpercentile(imin,imax,75);
    Descriptors[catno].p90 :=  getpercentile(imin,imax,90);
    Descriptors[catno].p95 :=  getpercentile(imin,imax,95);

    ave := sum/Descriptors[catNo].nmn;
    adev := 0.0;
    svar := 0.0;
    skew := 0.0;
    curt := 0.0;
    FOR j := imin TO imax DO
    BEGIN
      s := V.asFloat[j]-ave;
      adev := adev+abs(s);
      p := s*s;
      svar := svar+p;
    END;

    Descriptors[catNo].SumSS := svar;
    adev := adev/(Descriptors[catNo].nmn);
    if Descriptors[catNo].nmn < 2 then
    begin
      svar :=NA_FLOAT;
      sdev :=NA_FLOAT;
      Descriptors[catNo].variance:=svar;
    end
    else
    begin
      Descriptors[catNo].variance:=svar/(Descriptors[catNo].nmn-1);
      svar := svar/(Descriptors[catNo].nmn-1);
      sdev := sqrt(svar);
    end;
    Descriptors[catNo].mean:=ave;
    Descriptors[catNo].sum:=sum;
    Descriptors[catNo].sumSq:=sumSq;
    Descriptors[catNo].avgdev:=adev;
    Descriptors[catNo].stdvar:=svar;
    Descriptors[catNo].stddev:=sdev;
    //cfi:
    Descriptors[catNo].cfiL := NA_FLOAT;
    Descriptors[catNo].cfiH := NA_FLOAT;

    // TORSTEN ADDED:
    if ((sqrt(Descriptors[catno].nmn)) <> 0) and (svar <> NA_FLOAT) then
    begin
      f := sqrt(svar/Descriptors[catno].nmn);
      //       f := 1.96* f;
      f:=   PTDISTRINV((Descriptors[catno].nmn-1),0.025) * f; //           PTDISTRINV(F:LONGINT;P:EXTENDED)
      Descriptors[catNo].cfiL := Descriptors[catno].mean-f;
      Descriptors[catNo].cfiH := Descriptors[catno].mean+f;
    end;
  end;

begin
  setLength(Descriptors,20);//assume 20 categories
  catNo  :=0;
  categ  :=nil;
//  selected := df.vectors[0];
  if df.VectorCount > 1 then
  begin
    categ    := df.vectors[0];
     v        :=df.vectors[1];
     CatCreated :=false;
  end
  else
  begin
    categ    := TEpiIntVector.Create('__c',df.rowcount);
    v        :=df.vectors[0];
    CatCreated :=true;
  end;

  try
    //df.SelectVector:=selected;
      co := df.SelectedRowCount;
    if co < 1 then
         raise exception.create('No data in '+ v.Name);
    imin:=1;
    imax:=1;
    i:=2;
    val:= V.asFloat[1];
    sum:=val;
    sumSq:=val*val;
  {#TODO2 deal with co =1 and with many categories in categ}
    if co = 1 then
      raise exception.create('Insufficient data (only 1 observation) in '+ v.Name);
    while i<=co do
    begin
      if (categ.compare(i,i-1)<>0) then
      begin
        if catno >= length(Descriptors) then
        raise exception.create('Too many categories');
        imax:=i-1;

        FillDescriptors();

        inc(catNo);
        imin:=i;
        val:= V.asFloat[i];
        sum:=val;
        sumSq:=val*val;

        // We need to handle situation when last record is also its own category!
        if (i=co) then
        begin
          imax:=i;
          FillDescriptors();
          inc(catno);
        end;
      end
      else if (i=co) and (categ.compare(i,i-1)=0) then
      begin
        imax:=i;
        val:= V.asFloat[i];
        sum:=sum+val;
        sumSq:=SumSq+(val*val);
        FillDescriptors();
        inc(catno);
      end
      else
      begin
        val:= V.asFloat[i];
        sum:=sum+val;
        sumSq:=SumSq+(val*val);
      end;
      inc(i);
    end;//while
    setlength(Descriptors,catno);
  finally
    if CatCreated then Categ.free;
  end;
end;


function ttest(Descriptors: TEpiDescriptivesArray; var t,df,prob: Epifloat):EpiBool;
VAR
   i,n1,n2,levels: EpiInt;
   var2,var1,svar,ave2,ave1: EpiFloat;
BEGIN
   result:=true;
   levels:=length(Descriptors);
   if levels<> 2  then
      raise exception.create('Two levels are needed');
   n1 := Descriptors[0].nmn;
   n2 := Descriptors[1].nmn;
   df := n1+n2-2;
   svar := ((n1-1)*Descriptors[0].stdvar+(n2-1)*Descriptors[1].stdvar)/df;
   t := (Descriptors[0].mean-Descriptors[1].mean)/sqrt(svar*(1.0/n1+1.0/n2));
   prob := betai(0.5*df,0.5,df/(df+sqr(t)))
END;


function pttest(Descriptors: TEpiDescriptivesArray; var t,df,prob: Epifloat):EpiBool;
VAR
   n,levels: EpiInt;
   std,stdErr: EpiFloat;
BEGIN
   result:=true;
   levels:=length(Descriptors);
   if levels<> 1  then
      raise exception.create('only one variable allowed');
   n := Descriptors[0].nmn;
   df := n-1;
   std := sqrt(Descriptors[0].stdvar);
   stdErr := std/sqrt(n);
   if stdErr= 0 then
        t :=abs(Descriptors[0].mean)
   else
        t := abs(Descriptors[0].mean/stdErr);
   prob := betai(0.5*df,0.5,df/(df+sqr(t)))
end;


function ANOVA(Descriptors: TEpiDescriptivesArray; var AnovaRec: TEpiAnovaRecord):EpiBool;
VAR
   i,n,levels: EpiInt;
   total,totalSQ{,var2,var1,svar,ave2,ave1}: EpiFloat;
BEGIN
   result:=false;
   levels:=length(Descriptors);
   if levels< 2  then
      raise exception.create('Two levels at least are needed');
   n := 0;
   total := 0;
   totalSQ := 0;
   AnovaRec.SSB:=0.0;
   AnovaRec.MSB:=0.0;
   AnovaRec.SSW:=0.0;
   AnovaRec.MSW:=0.0;
   for i:= 0 to levels-1 do
   begin
      n:=n+ Descriptors[i].nmn;
      total:=total+ Descriptors[i].sum;
      totalSQ:=totalSQ+ Descriptors[i].sumSQ;
      AnovaRec.SSW:= AnovaRec.SSW+ Descriptors[i].SumSS;
      AnovaRec.SSb:= AnovaRec.SSB+ ((Descriptors[i].Sum*Descriptors[i].Sum)/Descriptors[i].nmn);
   end;
   AnovaRec.SSB :=AnovaRec.SSB-((total*total)/n);
   AnovaRec.SST:=AnovaRec.SSB+AnovaRec.SSW;
   AnovaRec.dfb :=levels-1;
   AnovaRec.dfw :=n-levels;
   AnovaRec.dft:=n-1;
   AnovaRec.MSB:= AnovaRec.SSB/AnovaRec.dfb;
   AnovaRec.MSW:= AnovaRec.SSW/AnovaRec.dfw;
   AnovaRec.MST:= AnovaRec.SST/AnovaRec.dft;
   AnovaRec.f := AnovaRec.MSB/AnovaRec.MSW;
   AnovaRec.prob := fdist(AnovaRec.f,AnovaRec.dfb,AnovaRec.dfw);
//   AnovaRec.prob := betai(0.5*df,0.5,df/(df+sqr(t)))
END;

function Bartlett (Descriptors: TEpiDescriptivesArray;AnovaRec: TEpiAnovaRecord;var chi,pval:EpiFloat):EpiBool;
var
    i, levels,AllTotal       : EpiUnInt;
    Lambda,c,temp    : EpiFloat;
Begin
    Result:=false;
    If (AnovaRec.SSW <= 0) or (AnovaRec.MSW <= 0) Then  Exit;
    Temp := 0;
    Lambda := 0;
    levels := length( Descriptors);
    For I := 0 to levels-1 Do
      Begin
        If (Descriptors[i].Variance = 0) or (Descriptors[i].Variance = NA_FLOAT) Then   Exit;
        Lambda := Lambda + (Descriptors[i].nmn - 1) * Ln(AnovaRec.MSW/ Descriptors[i].Variance);
        If Descriptors[i].sum > 1 Then
          Temp := Temp + (1 / (Descriptors[i].nmn - 1));
        AllTotal:=AllTotal+Descriptors[i].nmn;
      End (*For*);
    C := 1 + (1 / (3 * (levels - 1))) * (Temp -  1 / (AllTotal - levels));
    chi := Lambda / C;
    pval := ChiPValue (chi, levels - 1);
end;

function FreqDescrptives(const df: TEpiDataFrame;var Descriptors: TEpiFreqDescriptiveArray):Epibool;
var
  i,j,co,vco:  integer;
  selected,V   : TEpiVector;
  catVal           : integer;
  catNo, imin, imax,imed            : integer;
  CatCreated, same12   :boolean;
  vlist        : array of TEpiVector;
  s,s1            : string;

  Function LevelChanged:boolean;
  var
    iv:  integer;
  begin
     result:=true;
     for iv:= 0 to vco-1 do
        if vlist[iv].compare(i,i-1)<>0 then exit;
     result:=false;
  end;

begin
  Result:=false;
  setLength(Descriptors,600);
  //selected := df.vectors[0];
  vco := df.VectorCount;
  setlength(vlist,vco);
  for i:= 0 to vco-1 do
      vlist[i] := df.vectors[i];
  try
//  df.SelectVector:=selected;
  co := df.SelectedRowCount;
  same12:=true;
  if co = 0 then raise exception.create('No data in '+ df.Vectors[0].Name)
  else if co=1 then
  begin
      Descriptors[0].nmn:=1;
       for j:=0 to vco-1 do
       begin
           s := vlist[j].AsString[1];
           strLcopy(Descriptors[0].level+j*20,pchar(s),19);
        end;
     imin   :=2;
     catNo  :=1;                          
  end
  else
  begin
     imin:=1;
     catNo  :=0;
  end;
  imax:=1;
  i:=2;
{#TODO2 deal with many categories in categ}
  while i<=co do
  begin
    if (i=co) or LevelChanged then
    begin
        if catno >= length(Descriptors) then
             raise exception.create('Too many categories');
        imax:=i-1;
//did we hit the end?
        if (i=co) then
        begin
//is the end value is the same as the previous row?
           same12 :=true;
           for j:= 0 to vco-1 do
             if vlist[j].compare(i,i-1)<>0 then
             begin
                same12:=false;
                break;
             end;
           if same12=false then
           begin
              for j:=0 to vco-1 do
              begin
                 s := vlist[j].AsString[imax];
//                 s1 := vlist[j].GetValueLabel(s);
//                 if s1<>'' then s:=s1;
                 strLcopy(Descriptors[catNo].level+j*20,pchar(s),19);
              end;
              Descriptors[catNo].nmn:=imax-imin+1;
              inc(catNo);
              imin:=co;
           end;
           imax:=i;
        end;//end?
        for j:=0 to vco-1 do
        begin
           s := vlist[j].AsString[imax];
//           s1 := vlist[j].GetValueLabel(s);
//           if s1<>'' then s:=s1;
           strLcopy(Descriptors[catNo].level+j*20,pchar(s),19);
        end;
        Descriptors[catNo].nmn:=imax-imin+1;
        inc(catNo);
        imin:=i;
    end;
   inc(i);
  end;//while
  setlength(Descriptors,catno);
  Result:=true;
finally
 vlist :=nil;
end;
end;


function GetFreqDescriptiveData(Descriptors: TEpiFreqDescriptiveArray;var GrandTotal:EpiFloat):Epibool;
var
 co,i : integer;
begin
  Grandtotal:=0;
  co :=length(Descriptors);
  for i:= 0 to co-1 do
  begin
     Grandtotal:= Grandtotal + Descriptors[i].nmn;
  end;//for
end;


function EpiSummary(Varnames: TStrings;Dataframe:TEpiDataFrame; var Descriptors: TEpiDescriptivesArray;var df : TEpiDataFrame): boolean;
var
   Vectorlist :TEpiVectors;
   VectorDesc: TEpiDescriptivesArray;
   v : TepiVector;
   ThisVector : Tstringlist;
   i,co,j,cco,varco, nmn, median   : integer;
   df1 : TEpiDataFrame;
   begin
    vectorlist:=nil;
    df1:=nil;
try
     varco := Varnames.Count;
    if varco < 1 then
       raise exception.create('Minimum one variable');
    df := Dataframe.Clone(Varnames);
   // prepare 0utput
   setLength(Descriptors,varco);// one for each vector
   Vectorlist:= dataframe.GetVectorListByName(varnames);

   // collect information for each vector:
  thisvector:= TStringList.create;
 for i:= 1 to varco do
 //N
  begin
  //  vv:=TVectorVar(pv);
   v :=df.vectors[i-1];
  thisvector.Add(V.name);
        descriptors[i-1].level := v.name;
   if not (v.datatype in [EpiTyDate,EpiTyUppercase,EpiTyString, EpiTyByte]) then
   begin
     EpiMeans(thisvector,Dataframe,VectorDesc,df1);
//      descriptors[i-1].level := v.name;
     descriptors[i-1].nmn := VectorDesc[0].nmn;
     descriptors[i-1].med := VectorDesc[0].med;
     descriptors[i-1].max := VectorDesc[0].max;
     descriptors[i-1].min := VectorDesc[0].min;
     descriptors[i-1].sum := VectorDesc[0].sum;
     descriptors[i-1].mean :=VectorDesc[0].mean;
     descriptors[i-1].stddev :=VectorDesc[0].stddev;
     descriptors[i-1].stdvar :=VectorDesc[0].stdvar;
     descriptors[i-1].p5 :=VectorDesc[0].p5;
     descriptors[i-1].p10 :=VectorDesc[0].p10;
     descriptors[i-1].p25 :=VectorDesc[0].p25;
     descriptors[i-1].p75 :=VectorDesc[0].p75;
     descriptors[i-1].p90 :=VectorDesc[0].p90;
     descriptors[i-1].p95 :=VectorDesc[0].p95;
     descriptors[i-1].cfil :=VectorDesc[0].cfil;
     descriptors[i-1].cfih := VectorDesc[0].cfih;
  //   descriptors[i-1].curt := VectorDesc[0].curt;
  //   descriptors[i-1].skew :=VectorDesc[0].skew;
   end; // field types
     thisvector.clear;
  end ; // this vector
 finally
   Vectorlist.free;
   df1.free;
end;
end;


function EpiMeans(Varnames: TStrings;Dataframe:TEpiDataFrame; var Descriptors: TEpiDescriptivesArray;var df : TEpiDataFrame): boolean;
var
   Vectorlist :TEpiVectors;
   v,selected,categ   : TepiVector;
   i,co,j,cco,varco       : integer;
   t, p                   : EpiFloat;
begin
    Vectorlist:=nil;
try
    varco := Varnames.Count;
    if varco < 1 then
       raise exception.create('Minimum one variable');
    if varco > 2 then
       raise exception.create('Maximum two variables');
    if varco=2 then Varnames.Exchange(0,1);
    //Varnames.insert(0,'__s_');
    Vectorlist:= Dataframe.GetVectorListByName(varnames);
//check for data types
    if varco=2 then
    begin
      if Vectorlist[0].datatype =EpiTyDate then
        raise exception.create('A date variable cannot be used as a category variable');
      if Vectorlist[0].datatype in [EpiTyString,EpiTyUppercase] then
        raise exception.create('Category variable must be numerical<br>gen numerical variable');
    end;
    if Vectorlist[varco-1].datatype in [EpiTyDate,EpiTyString,EpiTyUppercase,EpiTyByte] then
      raise exception.create('A date/string variable cannot be processed using this command');

   df := Dataframe.PrepareDataframe(varnames, nil);
   Vectorlist.free;
   Vectorlist:=nil;
   co := df.rowcount;
   cco := df.vectorcount;
   Vectorlist:= df.GetVectorListByName(varnames);

  //first vector is the selector
//   selected := df.vectors[0];
   if varco=2 then
   begin
     categ    := df.VectorByName[varnames[0]];
     v        := df.VectorByName[varnames[1]];
     for i:= 1 to co do
       if  v.IsMissing[i] or categ.IsMissing[i] then df.Selected[i] := false;
   end
   else
    begin
      v        :=df.VectorByName[varnames[0]];
      for i:= 1 to co do
       if  v.IsMissing[i] or v.IsmissingValue[i] then df.Selected[i] := false;
   end;
//   df.SelectVector := selected;
   co := df.SelectedRowCount;
   if co = 0  then       // skal være 0
      raise exception.create('No data in '+ v.Name);
   df.Sort(Vectorlist);
   IntervalDescrptives(df,Descriptors);
finally
   Vectorlist.free;
end;
end;

function InternalEpiCorrelate(Varnames: TStrings;Dataframe:TEpiDataFrame; var RegObj : TRegComp; var CorMatrix : XMatrix): boolean;
var
   Vectorlist :TEpiVectors;
   y,v,selected,categ   : TepiVector;
   i,co,j,cco,varco,n       : integer;
   t, p                   : EpiFloat;
begin
    Vectorlist:=nil;
    RegObj     :=nil;
try
    varco := Varnames.Count;
    if varco < 2 then
       dm.error('Minimum of two variables allowed', [], 28001, 0);
    Vectorlist:= Dataframe.GetVectorListByName(varnames);
//check for data types
   for i := 0 to varco-1 do
      if Vectorlist[i].datatype in  [EpiTyDate,EpiTyUppercase,EpiTyString] then
           dm.error('Only numeric variables are allowed', [], 28002, 0);
try
  RegObj := TRegComp.create(nil);
  co := Dataframe.SelectedRowCount;
  RegObj.MVPrepare;  // Initialize container array(s)
  //selected:= dataframe.VectorByName['__s_'];
  n:=0;
  for i:= 1 to co do
  begin
      //if selected[i]>0 then continue;
      for j:= 0 to varco-1 do
         if (Vectorlist[j].IsMissing[i] or Vectorlist[j].IsMissingValue[i]) then
            break
          else               //col,row
           RegObj.PRegressData^[j+1,n]:=  Vectorlist[j].AsFloat[i];
      if j =varco then {no missing values found}
      begin
//Y not needed for correlation
//        RegObj.Y[n]:=y.AsFloat[i] ;
        inc(n);
      end;
   end;
   CorMatrix:= RegObj.CorrelationMatrix(RegObj.PRegressData,varco,n);
except
  RegObj.free;
  RegObj     :=nil;
end;
finally
   Vectorlist.free;
end;
end;

function EpiCorrelate(Varnames: TStrings;Dataframe:TEpiDataFrame;var CorMatrix : XMatrix): boolean;
var
  RegObj : TRegComp;
begin
  Result :=InternalEpiCorrelate(Varnames,Dataframe,RegObj,CorMatrix);
  if RegObj <> nil then
  begin
//     RegRec
     RegObj.free;
   end;
end;

function inverseT(x : EpiFloat; df : integer) : EpiFloat;
   function pow(base: extended; expon: integer): extended;
   begin
     pow := exp(expon * ln(base));
   end;

const
     sv : array[1..7] of EpiFloat = (12.706, 4.3027, 3.1825, 2.7764,
                                  2.5706, 2.4469, 2.2346);
var
   T           : EpiFloat;
   x3, x5, x7  : EpiFloat;
begin
     if ( df < 8 ) then
        begin
        inverseT := sv[df];
        exit;
        end;

     x3 := pow(x, 3);
     x5 := pow(x, 5);
     x7 := pow(x, 7);
     T := x + ((x3 + x)/4)/df +
              ((5*x5 + 16*x3 + 3*x) / 96)/pow(df,2) +
              ((3*x7 + 19*x5 + 17*x3 - 15*x) / 384)/pow(df,3) +
              ((79*pow(x,9) + 776*x7 + 1482*x5 -1920*x3 - 945*x)/92160)/pow(df,4);
     inverseT := T;
end;
{
    NumConf     = 5;
    ConfInfo    : Packed Array [1 .. NumConf] of ConfRec =
                     ((Pct:  90; Score: 1.645),
                      (Pct:  95; Score: 1.960),
                      (Pct:  98; Score: 2.326),
                      (Pct:  99; Score: 2.576),
                      (Pct: 999; Score: 3.291));}

function GetMLRConflim(se: EpiFloat; df : integer): EpiFloat;
begin
//  Temp2 := ConfScore * Temp  (* ConfScore confidence limits on Beta *);}
  Result := inverseT(1.960, df) * se;
end;



end.


