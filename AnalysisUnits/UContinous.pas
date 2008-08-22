unit UContinous;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UVectorOp, UOutput, UCommands, Windows, UAnaToken;

  // Main:
  function DoKWallis(df: TEpiDataframe; Varnames: TStrings; Cmd: TCommand):boolean;
  function DoMeans(Dataframe: TEpiDataframe; Varnames: TStrings; cmd: TCommand): boolean;
  function DoDescribe(Dataframe: TEpiDataframe; VarNames: TStrings; cmd: TCommand): boolean;

  // Secondary:
  function OutMeans(Descriptors: TEpiDescriptivesArray; df: TEpiDataFrame; quiet: boolean; foot:string; cmd: TCommand):boolean;
  function OutDescribe(Descriptors: TEpiDescriptivesArray; df: TEpiDataFrame; quiet: boolean; cmd: TCommand):boolean;

implementation

uses UCmdProcessor, UStatfunctions, UFormats, Math;


// ===========================================================================
// Main methodes, functions and procedures.
// ===========================================================================
function DoKWallis(df: TEpiDataframe; Varnames: TStrings; Cmd: TCommand):boolean;
label cleanup, Check1;
var
  Vectorlist :TEpiVectors;
  v,categ   : TepiVector;
  i,j,k, varco,n       : integer;
  t, p                   : EpiFloat;
  min_grp, max_grp, grp_diff, grp_diff_pre, group : integer;
  NoTies, NoTieGroups, grpCount : integer;
  score, SumT, Avg, Probchi,  ProbchiH, H, CorrectedH : extended;
  Correction, Temp, TieSum : extended;
  outline : string;
  Ranks : array of array of Epifloat;
  group_counts :array of integer;
  RankSums : array of Epifloat;
  tab : TStatTable;
  s : string;
begin
  Vectorlist:=nil;
  tab := Nil;
  try
    n := df.rowcount;
    Vectorlist:= df.GetVectorListByName(varnames);
    v        := df.vectors[0];
    categ    := df.vectors[1];
    n := df.SelectedRowCount;
    if n < 2 then
       raise exception.create('No observations');
    df.Sort(categ.Name);
    min_grp := 10000;
    max_grp := -10000;
    grp_diff := 0;
    grp_diff_pre := 0;
    // From previous sorting we know that the categ is in rising order!
    for i := 1 to n do
    begin
      group := categ.AsInteger[i];
      if (i>1) and (group > categ.AsInteger[i-1]) then
        grp_diff := group - categ.AsInteger[i-1];
      if (grp_diff_pre > 0) and (grp_diff_pre <> grp_diff) then
        dm.Error('Group codes must be positive sequential numbers like 1, 2, 3...', [], 110001);
      if (grp_diff > 0) then
        grp_diff_pre := grp_diff;
      if (group < min_grp) then min_grp := group;
      if (group > max_grp) then max_grp := group;
    end;

    grpcount := (max_grp - min_grp) div grp_diff + 1;
    setLength(group_counts,grpcount+1);
    NoTieGroups := 0;
    SumT := 0.0;
    H := 0.0;
    setLength(RankSums,grpcount+1);
    for i := 1 to n do
    begin
      group := categ.AsInteger[i];
      group := (group - min_grp) div grp_diff + 1;
      if (group > grpcount) or (group< 1) then
        dm.Error('Group codes must be positive sequential numbers like 1, 2, 3...', [], 110001);
      group_counts[group] := group_counts[group] + 1;
    end;

    df.sort(vectorlist);
    // Store ranks
    SetLength(Ranks,2+1,n+1);
    for i := 1 to n do
    begin
      Ranks[1,i] := i;
      Ranks[2,i] := categ.AsInteger[i];
    end;
     //Check for ties in ranks - replace with average rank and calculate
    //T for each tie and sum of the T's
    i := 1;
    while i < n do
    begin
      j := i + 1;
      TieSum := 0;
      NoTies := 0;
      while (j < n) do
      begin
        if (v.asfloat[j] > v.asfloat[i]) then goto Check1;
        if (v.asfloat[j] = v.asfloat[i]) then // match
        begin
          TieSum := TieSum + round(Ranks[1,j]);
          NoTies := NoTies + 1;
        end;
        j := j + 1;
      end;
      Check1:
      if (NoTies > 0) then //At least one tie found
      begin
        TieSum := TieSum + Ranks[1,i];
        NoTies := NoTies + 1;
        Avg := TieSum / NoTies;
        for j := i to i + NoTies - 1 do Ranks[1,j] := Avg;
        t := IntPower(NoTies,3) - NoTies;
        SumT := SumT + t;
        NoTieGroups := NoTieGroups + 1;
        i := i + (NoTies - 1);
      end;
      i := i + 1;
    end; // next i
     // Calculate sum of ranks in each group and report
    for i := 1 to n do
    begin
      group := round(Ranks[2,i]);
      group := (group - min_grp) div grp_diff + 1;
      RankSums[group] := RankSums[group] + Ranks[1,i];
    end;
     // Calculate statistics
    for j := 1 to grpcount do
      if group_counts[j] <> 0 then
        H := H + (RankSums[j]) * (RankSums[j] / group_counts[j]);
    H := H * (12.0 / ( n * (n + 1)) );
    H := H - (3.0 * (n + 1));
    Correction := 1.0 - ( SumT / (IntPower(n,3) - n));
    CorrectedH := H / Correction;
    k := max_grp - min_grp;
    Probchi := 1.0 - chisquaredprob(H, k);
    Probchih := 1.0 - chisquaredprob(correctedH, k);

    // initiate output table
    tab:=dm.OutputList.NewTable(3,grpcount+1);
    tab.TableType := sttNormal;

    //vLab:= TStringList.create;
    if V.GetVariableLabel(cmd.ParameterList) = '' then
          tab.caption := 'Ranks of ' + V.name
          else tab.caption := 'Ranks of ' + V.GetVariableLabel(cmd.ParameterList);
    tab.caption := 'Kruskal - Wallis One-Way Analysis of Variance <br>' +tab.caption;
    tab.cell[1,1]:=categ.name ;
    tab.cell[2,1]:='N';
    tab.cell[3,1]:='Sum of Ranks';

    for i := 1 to grpcount do
    begin
    tab.cell[1,i+1]:= trim(format('%3d',[(i-1)*grp_diff + min_grp]));
    tab.cell[2,i+1]:=trim(format('%8d',[group_counts[i]]));
    tab.cell[3,i+1]:=trim(format('%10.2f',[RankSums[i]]));
    end;
    if categ.HasValueLabels() then
      for i := 1 to grpcount do
         tab.cell[1,i+1]:= categ.GetValueLabel(tab.cell[1,i+1], cmd.ParameterList);
    outline := format('Chi<sup>2</sup> = %8.4f df(%3d) p= %7.4f',[H,k,ProbChi]);
    s:= '<hr>' + outline;
    if NoTieGroups = 0
       then  s:= s + ' - No ties'
    else
    begin
      s := s + format( ' - %3d ties ',[NoTieGroups]);
      s := s +  '<br><br>' + format( 'Corrected for Ties: Chi<sup>2</sup> = %8.4f Df(%3d) p= %7.4f',[CorrectedH,k,ProbChiH]);

    end;
    dm.codemaker.OutputTable(tab,s);
    dm.Sendoutput;
  finally
    If Assigned(Vectorlist) then FreeAndNil(Vectorlist);
    If Assigned(tab) then FreeAndNil(tab);
  end;
end;

function DoMeans(Dataframe: TEpiDataframe; Varnames: TStrings; cmd: TCommand): boolean;
var
   dframe         : TEpiDataFrame;
   varco,levels       : integer;
   t,df, p, diff                   : EpiFloat;
   Descriptors: TEpiDescriptivesArray;
   tab : TStatTable;
   AnovaRec: TEpiAnovaRecord;
   s :string;
   opt : TEpiOption;
   quiet: boolean;

begin
  dframe:=nil;
  quiet := (cmd.ParamByName['QUIET']<>nil) or (cmd.ParamByName['Q']<>nil);
  try
    {#TODO1 : test for zero mean and zero SD}
    varco := Varnames.Count;
    EpiMeans(Varnames,Dataframe, Descriptors,dframe);

    if (varco=1){ and not quiet }then
    begin
      pttest(Descriptors, t,df,p);
      diff:= Descriptors[0].mean;
      s := '';

//      if (dm.GetOptionValue('STATISTICS', Opt) and (Opt.Value = 'ON')) then
      if (cmd.ParamByName['T']<>nil) then
      begin
        s := 'Students T-test for mean=0: T='+ epiformat(t, dm.outfmt(t)) + ' df(' + trim(epiformat(df,'%6.0f')) + format(') p=%6.5f ', [p]);
//        dm.Printresult(s);
      end;
      OutMeans(Descriptors,dframe, quiet,s,cmd);
    end
    else if (varco=2) {and not quiet} then
    begin
      levels:= length(Descriptors);
      if levels > 1 then
      begin
        ANOVA(Descriptors, AnovaRec);
        tab :=nil;
        try
        if (cmd.ParamByName['T']<>nil) then
          begin
          tab := dm.Outputlist.NewTable(6,4);
          tab.Caption := ' ';
          tab.TableType := sttNormal;

          tab.cell[1,1]:='Source';
          tab.cell[2,1]:='SS';
          tab.cell[3,1]:='df';
          tab.cell[4,1]:='MS';
          tab.cell[5,1]:='F';
          tab.cell[6,1]:='p Value';

          tab.cell[1,2]:='Between';
          tab.cell[2,2]:=Epiformat(AnovaRec.SSB );
          tab.cell[3,2]:=Epiformat(AnovaRec.dfb,'%6.0f');
          tab.cell[4,2]:=Epiformat(AnovaRec.MSB);
          tab.cell[5,2]:=Epiformat(AnovaRec.f);
          tab.cell[6,2]:=Epiformat(AnovaRec.prob, dm.outfmt(AnovaRec.prob));

          tab.cell[1,3]:='Within';
          tab.cell[2,3]:=Epiformat(AnovaRec.SSW);
          tab.cell[3,3]:=Epiformat(AnovaRec.dfw,'%6.0f');
          tab.cell[4,3]:=Epiformat(AnovaRec.MSW);
          tab.cell[5,3]:='';
          tab.cell[6,3]:='';

          tab.cell[1,4]:='Total';
          tab.cell[2,4]:=Epiformat(AnovaRec.SST);
          tab.cell[3,4]:=Epiformat(AnovaRec.dft,'%6.0f');
          tab.cell[4,4]:=Epiformat(AnovaRec.MST);
          tab.cell[5,4]:='';
          tab.cell[6,4]:='';

//          if (dm.GetOptionValue('STATISTICS', Opt) and (Opt.Value = 'ON')) then
         s := '';

            Bartlett(Descriptors,AnovaRec, diff,p);
            s := 'Bartlett''s test for homogeneity of variance <br>' + 'Chi<sup>2</sup>='+ dm.FloatFormat+ ' df(%d) p= '+ dm.FloatFormat;
            s:= format(s, [diff,levels-1,p]);      //dm.PrintResult(format(s, [diff,levels-1,p]));
          end;
          OutMeans(Descriptors,dframe, quiet,'',cmd);
          if not quiet then
          begin
            dm.codemaker.OutputTable(tab,s);
            dm.Sendoutput;
          end;
        finally
          tab.free;
        end;
      end;
    end;
  finally
    dframe.free;
    Descriptors:=nil;
  end;
end;

function DoDescribe(Dataframe: TEpiDataframe; VarNames: TStrings; cmd: TCommand): boolean;
var
   i,co,n      : integer;
   t: DWord;
   s      :string;
   Descriptors: TEpiDescriptivesArray;
   df         : TEpiDataFrame;
   Vectorlist :TEpiVectors;
   quiet: boolean;
begin
  df:=nil;
  vectorlist := nil;
  quiet := (cmd.ParamByName['QUIET']<>nil) or (cmd.ParamByName['Q']<>nil);
  Try
    EpiSummary(Varnames,dataframe, Descriptors, df);
    OutDescribe(Descriptors,df, quiet,cmd);
  finally
    Descriptors := nil;
    df.free;
  end;
end;



// ===========================================================================
// Secondary methodes, functions and procedures.
// ===========================================================================

function OutMeans(Descriptors: TEpiDescriptivesArray; df: TEpiDataFrame; quiet: boolean; foot: string; cmd: TCommand):boolean;
var
  i,co,j,cco,varco       : integer;
  v,categ   : TepiVector;
  s         :string;
  tab : TStatTable;
  opt : TEpiOption;

begin
  s:= '%6.0f';
  if cco > 100000 then  s:= '%12.0f';
  varco := df.Vectorcount;
  if varco=2 then
     begin
       categ    :=df.vectors[0];
       v        :=df.vectors[1];
     end
   else
       v        :=df.vectors[0];

  co := length(Descriptors);
  tab :=nil;
  try
    tab := dm.Outputlist.NewTable(10,co*2+3);
    tab.TableType := sttNormal;
    if (V.GetVariableLabel = '') then
        tab.caption := V.name
        else     tab.caption := V.GetVariableLabel(cmd.parameterlist);

    if varco=2 then
         tab.cell[1,1]:=categ.name
       else
         tab.cell[1,1]:='';  //was v.name;

    tab.cell[2,1]:='Obs.';
    tab.cell[3,1]:='Sum';
    tab.cell[4,1]:='Mean';
    tab.cell[5,1]:='Variance';
    tab.cell[6,1]:='Std Dev';
    tab.cell[7,1]:='( 95% CI';
    tab.cell[8,1]:='mean )';
    tab.cell[9,1]:='Std Err';
//    tab.cell[9,1]:='Skewness';
//    tab.cell[10,1]:='Curtosis';

    for i:=2 to co+1 do
    begin
      tab.cell[1,i]:=Descriptors[i-2].level;
      if varco=2 then
        begin
          s := categ.GetValueLabel(tab.cell[1,i],cmd.parameterlist);
          if s<>'' then tab.cell[1,i] :=s;
        end
          else tab.cell[1,i] := '';

      dm.AddResult('$variable'+inttostr(i-1),EpiTyString,v.name,0,0);
      dm.AddResult('$level'+inttostr(i-1),EpiTyString,tab.cell[1,i],0,0);
      dm.AddResult('$Obs'+inttostr(i-1),EpiTyInteger,Descriptors[i-2].nmn,0,0);
      dm.AddResult('$Sum'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].sum,0,0);
      dm.AddResult('$mean'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].mean,0,0);
      dm.AddResult('$var'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].stdvar,0,0);
      dm.AddResult('$sd'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].stddev,0,0);
      dm.AddResult('$cfiL'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].cfiL,0,0);
      dm.AddResult('$cfiH'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].cfiH,0,0);

      dm.AddResult('$stdErr'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].stddev/sqrt(Descriptors[i-2].nmn),0,0);
//      dm.AddResult('$Kurtosis'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].curt,0,0);
//      dm.AddResult('$Skewness'+inttostr(i-1),EpiTyFloat,Descriptors[i-2].skew,0,0);

      tab.cell[2,i]:=Epiformat(Descriptors[i-2].nmn,'%6.0f');
      tab.cell[3,i]:=Epiformat(Descriptors[i-2].sum,dm.outfmt(Descriptors[i-2].sum));
      tab.cell[4,i]:=Epiformat(Descriptors[i-2].mean,dm.outfmt(Descriptors[i-2].mean));
      tab.cell[5,i]:=Epiformat(Descriptors[i-2].stdvar,dm.outfmt(Descriptors[i-2].stdvar));
      tab.cell[6,i]:=Epiformat(Descriptors[i-2].stddev,dm.outfmt(Descriptors[i-2].stddev));
      tab.cell[7,i]:=Epiformat(Descriptors[i-2].cfiL,dm.outfmt(Descriptors[i-2].cfiL));
      tab.cell[8,i]:=Epiformat(Descriptors[i-2].cfiH,dm.outfmt(Descriptors[i-2].cfiH));
      tab.cell[9,i]:=Epiformat(Descriptors[i-2].stddev/sqrt(Descriptors[i-2].nmn));
//      tab.cell[9,i]:=Epiformat(Descriptors[i-2].skew,dm.outfmt(Descriptors[i-2].skew));
//      tab.cell[10,i]:=Epiformat(Descriptors[i-2].curt,dm.outfmt(Descriptors[i-2].curt));
   end;

    cco:=co+3;
    if varco=2 then
      tab.cell[1,cco]:=categ.name
    else
      tab.cell[1,cco]:='';

    tab.cell[2,cco]:='Minimum';
    tab.cell[3,cco]:='p5';
    tab.cell[4,cco]:='p10';
    tab.cell[5,cco]:='p25';
    tab.cell[6,cco]:='Median';
    tab.cell[7,cco]:='p75';
    tab.cell[8,cco]:='p90';
    tab.cell[9,cco]:='p95';
    tab.cell[10,cco]:='Max';
    inc(cco);
    for i:=0 to co-1 do
     begin
      tab.cell[1,i+cco]:=Descriptors[i].level;
      if varco=2 then
        begin
          s := categ.GetValueLabel(tab.cell[1,i+cco],cmd.parameterlist);
          if s<>'' then tab.cell[1,i+cco] :=s;
        end
      else tab.cell[1,i+cco] :=' ';

      dm.AddResult('$min'+inttostr(i+1),EpiTyFloat,Descriptors[i].min,0,0);
      dm.AddResult('$p05'+inttostr(i+1),EpiTyFloat,Descriptors[i].p5,0,0);
      dm.AddResult('$p10'+inttostr(i+1),EpiTyFloat,Descriptors[i].p10,0,0);
      dm.AddResult('$p25'+inttostr(i+1),EpiTyFloat,Descriptors[i].p25,0,0);
      dm.AddResult('$p50'+inttostr(i+1),EpiTyFloat,Descriptors[i].med,0,0);
      dm.AddResult('$p75'+inttostr(i+1),EpiTyFloat,Descriptors[i].p75,0,0);
      dm.AddResult('$p90'+inttostr(i+1),EpiTyFloat,Descriptors[i].p90,0,0);
      dm.AddResult('$p95'+inttostr(i+1),EpiTyFloat,Descriptors[i].p95,0,0);
      dm.AddResult('$max'+inttostr(i+1),EpiTyFloat,Descriptors[i].max,0,0);

      tab.cell[2,i+cco]:=Epiformat(Descriptors[i].min,dm.outfmt(Descriptors[i].min));
      tab.cell[3,i+cco]:=Epiformat(Descriptors[i].p5,dm.outfmt(Descriptors[i].p5));
      tab.cell[4,i+cco]:=Epiformat(Descriptors[i].p10,dm.outfmt(Descriptors[i].p10));
      tab.cell[5,i+cco]:=Epiformat(Descriptors[i].p25,dm.outfmt(Descriptors[i].p25));
      tab.cell[6,i+cco]:=Epiformat(Descriptors[i].med,dm.outfmt(Descriptors[i].med));
      tab.cell[7,i+cco]:=Epiformat(Descriptors[i].p75,dm.outfmt(Descriptors[i].p75));
      tab.cell[8,i+cco]:=Epiformat(Descriptors[i].p90,dm.outfmt(Descriptors[i].p90));
      tab.cell[9,i+cco]:=Epiformat(Descriptors[i].p95,dm.outfmt(Descriptors[i].p95));
      tab.cell[10,i+cco]:=Epiformat(Descriptors[i].max,dm.outfmt(Descriptors[i].max));
   end;

  if not quiet then
  begin
    dm.codemaker.OutputTable(tab,foot);
    // results of OUtPUT MEANS:
    dm.Sendoutput;
  end;
  finally
    tab.free;
  end;
end;

function OutDescribe(Descriptors: TEpiDescriptivesArray; df: TEpiDataFrame; quiet: boolean;cmd: TCommand):boolean;
var
  i,co,j,cco,varco       : integer;
  //v   : TepiVector;
  tab : TStatTable;
  cfi : extended;
  s : string;
begin
  varco := df.Vectorcount-2;
  cco := df.rowcount;
  co := length(Descriptors);
  tab :=nil;
  try
    tab := dm.OutputList.NewTable(15,co+1);
    tab.TableType := sttNormal;

    tab.cell[1,1]:='Variable';
    s := 'N=' + Epiformat(cco);
    tab.cell[2,1]:= s;
    tab.cell[3,1]:='Sum';
    tab.cell[4,1]:='Mean';
    tab.cell[5,1]:='(95%';
    tab.cell[6,1]:='cfi)';
    tab.cell[7,1]:='Min' ;
    tab.cell[8,1]:='p5';
    tab.cell[9,1]:='p10';
    tab.cell[10,1]:='p25';
    tab.cell[11,1]:='Median';
    tab.cell[12,1]:='p75';
    tab.cell[13,1]:='p90';
    tab.cell[14,1]:='p95';
    tab.cell[15,1]:='Max';
    for i:=0 to varco+1 do
    begin
      tab.cell[1,i+2]:=Descriptors[i].level;

      if df.VectorByName[Descriptors[i].level].DataType in [EpiTyFloat,EpiTyInteger] then
      begin
        dm.addresult('$variable'+inttostr(i+1),EpiTyString,Descriptors[i].level,0,0);
        dm.addresult('$Missing'+inttostr(i+1),EpiTyInteger,(cco-Descriptors[i].nmn),0,0);
        dm.addresult('$Obs'+inttostr(i+1),EpiTyInteger,Descriptors[i].nmn,0,0);
        dm.addresult('$Sum'+inttostr(i+1),EpiTyFloat,Descriptors[i].sum,0,0);
        dm.addresult('$mean'+inttostr(i+1),EpiTyFloat,Descriptors[i].mean,0,0);
        dm.AddResult('$var'+inttostr(i+1),EpiTyFloat,Descriptors[i].stdvar,0,0);
        dm.addresult('$sd'+inttostr(i+1),EpiTyFloat,Descriptors[i].stddev,0,0);
        dm.addresult('$cfil'+inttostr(i+1),EpiTyFloat,Descriptors[i].cfiL,0,0);
        dm.addresult('$cfih'+inttostr(i+1),EpiTyFloat,Descriptors[i].cfiH,0,0);
        dm.addresult('$stdErr'+inttostr(i+1),EpiTyFloat,Descriptors[i].stddev/sqrt(Descriptors[i].nmn),0,0);

        //        dm.AddResult('$Kurtosis'+inttostr(i+1),EpiTyFloat,Descriptors[i].curt,0,0);
//        dm.AddResult('$Skewness'+inttostr(i+1),EpiTyFloat,Descriptors[i].skew,0,0);

        dm.addresult('$min'+inttostr(i+1),EpiTyFloat,Descriptors[i].min,0,0);
        dm.addresult('$p05'+inttostr(i+1),EpiTyFloat,Descriptors[i].p5,0,0);
        dm.addresult('$p10'+inttostr(i+1),EpiTyFloat,Descriptors[i].p10,0,0);
        dm.addresult('$p25'+inttostr(i+1),EpiTyFloat,Descriptors[i].p25,0,0);
        dm.addresult('$p50'+inttostr(i+1),EpiTyFloat,Descriptors[i].med,0,0);
        dm.addresult('$p75'+inttostr(i+1),EpiTyFloat,Descriptors[i].p75,0,0);
        dm.addresult('$p90'+inttostr(i+1),EpiTyFloat,Descriptors[i].p90,0,0);
        dm.addresult('$p95'+inttostr(i+1),EpiTyFloat,Descriptors[i].p95,0,0);
        dm.addresult('$max'+inttostr(i+1),EpiTyFloat,Descriptors[i].max,0,0);

        tab.cell[2,i+2]:=Epiformat(Descriptors[i].nmn,'%6.0f');
        tab.cell[3,i+2]:=Epiformat(Descriptors[i].sum,dm.outfmt(Descriptors[i].sum));
        tab.cell[4,i+2]:=Epiformat(Descriptors[i].mean,dm.outfmt(Descriptors[i].mean));
        tab.cell[5,i+2]:=Epiformat(Descriptors[i].cfiL,dm.outfmt(Descriptors[i].cfiL));
        tab.cell[6,i+2]:=Epiformat(Descriptors[i].cfiH,dm.outfmt(Descriptors[i].cfiH)) ;
        tab.cell[7,i+2]:=Epiformat(Descriptors[i].min,dm.outfmt(Descriptors[i].min));
        tab.cell[8,i+2]:=Epiformat(Descriptors[i].p5,dm.outfmt(Descriptors[i].p5));
        tab.cell[9,i+2]:=Epiformat(Descriptors[i].p10,dm.outfmt(Descriptors[i].p10));
        tab.cell[10,i+2]:=Epiformat(Descriptors[i].P25,dm.outfmt(Descriptors[i].p25));
        tab.cell[11,i+2]:=Epiformat(Descriptors[i].med,dm.outfmt(Descriptors[i].med));
        tab.cell[12,i+2]:=Epiformat(Descriptors[i].P75,dm.outfmt(Descriptors[i].p75));
        tab.cell[13,i+2]:=Epiformat(Descriptors[i].p90,dm.outfmt(Descriptors[i].p90));
        tab.cell[14,i+2]:=Epiformat(Descriptors[i].p95,dm.outfmt(Descriptors[i].p95));
        tab.cell[15,i+2]:=Epiformat(Descriptors[i].max,dm.outfmt(Descriptors[i].max));
      end
      else
      begin
        tab.cell[3,i+2]:='--';
        tab.cell[3,i+2]:='str/date';
      end;
    end;

    if not quiet then
    begin
      dm.codemaker.OutputTable(tab,'');
      dm.Sendoutput;
    end;
  finally
    tab.free;
  end;
end;

end.

