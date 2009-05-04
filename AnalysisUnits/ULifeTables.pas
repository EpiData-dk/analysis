unit ULifeTables;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UVectorOp, UOutput, UCommands, Windows, UAnaToken;

// ============================================================================
// Declaration of:
// Main methodes, functions and procedures.
// ============================================================================

type

  TLifeTables = class
  private
    // For internally available methods!
    WeightName: string;
    Weighted:  boolean;
    procedure Stats(dataframe: TEpiDataframe; xTab: TStatTable);
    procedure AddPercentile(dataframe: TEpiDataframe; xTab: TStatTable; const Percentile: EpiFloat);
    procedure AddTime(dataframe: TEpiDataframe; xTab: TStatTable; const Time: Integer);
    procedure Percentiles(dataframe: TEpiDataframe; xTab: TStatTable);
    procedure Times(dataframe: TEpiDataframe; xTab: TStatTable);
  protected
    //
  public
    Cmd: TCommand;
    // For Externaly available methods.
    procedure OutLifeTable(df: TEpiDataframe; KMPrinciple: boolean = True);
    function CreateLifeTable(df: TEpiDataFrame; Varnames: TStrings; var OutputTable: TStatTable): TEpiDataFrame;
    function DoLifeTables(dataframe: TEpiDataframe; varnames: TStrings; cmd: TCommand): TEpiDataFrame;
  end;


var
  OLifeTables: TLifeTables;

implementation

uses UCmdProcessor, UTables, UFormats, UAggregate, EpiDataUtils, UCmdTypes, UDateUtils, Math, GeneralUtils,
     UStatFunctions;


// ============================================================================
// Public methodes.
// ============================================================================

function TLifeTables.DoLifeTables(dataframe: TEpiDataframe; varnames: TStrings; cmd: TCommand): TEpiDataframe;
var
  xTab: TStatTable;
begin
  self.Cmd := cmd;
  result := nil;
  xtab := nil;
  try
    result := CreateLifeTable(dataframe, varnames, xtab);
    if Cmd.ParamExists['T'] then
      Stats(result, xtab);
    Percentiles(result, xtab);
    Times(result, xtab);

    if Assigned(xtab) and (not Cmd.ParamExists['NT']) and
       (not (Cmd.ParamExists['Q'])) then
      dm.CodeMaker.OutputTable(xtab, '');
    if (not Cmd.ParamExists['NOLT']) and (not (Cmd.ParamExists['Q'])) then
      OutLifeTable(result, not (Dataframe.VectorByName[varnames[1]].DataType = EpiTyFloat));
    dm.Sendoutput();
  finally
    if Assigned(xTab) then FreeAndNil(xTab);
    WeightName := '';
    Weighted := False;
    self.Cmd := nil;
  end;
end;

procedure TLifeTables.OutLifeTable(df: TEpiDataframe; KMPrinciple: boolean = True);
var
  tf: TTableFormats;
  tab: TStatTable;
  i, j, r: integer;
  V: TEpiVector;
begin
  OTables.Getformats(Cmd, tf);

  if Cmd.ParamExists['NOCI'] then
    tab := dm.CodeMaker.Output.NewTable(6,1)
  else
    tab := dm.CodeMaker.Output.NewTable(8,1);
  tab.TableType := sttStat;

  if Cmd.ParamExists['I'] then
    tab.Footer := 'Outcome and time aggregated to intervals shown'
  else
    tab.Footer := 'Recorded time used for all observations (Kaplan-Meier principle)';

  if not KMPrinciple then
    tab.Footer := 'Warning: Time variable has type float, interval=1 assumed';

  tab.Footer := tab.Footer +  '<br><font class=small>Time intervals from left number up to right number</font>';

  if Cmd.ParamExists['ADJ'] then
    tab.Footer := '<br>Censored observation contribute half of final period (Adjusment).';

  // Headers.
  tab.Cell[2,1] := tf.LTHdr.IntVal;
  tab.Cell[3,1] := tf.LTHdr.Beg;
  tab.Cell[4,1] := tf.LTHdr.Deaths;
  tab.Cell[5,1] := tf.LTHdr.Lost;
  tab.Cell[6,1] := tf.LTHdr.Survival;
  if (not Cmd.ParamExists['NOCI']) then
  begin
    tab.Cell[7,1] := tf.LTHdr.StdErr;
    tab.Cell[8,1] := tf.CIHdr;
  end;

  for i := 1 to df.RowCount do
  begin
    if Cmd.ParamExists['BY'] then
    begin
      V := df.VectorByName[Cmd.ParamByName['BY'].AsString];
      if (i = 1) or ((i < df.RowCount) and (V.compare(i-1,i) <> 0)) then
      begin
        tab.AddRow;
        r := Tab.RowCount;
        tab.Cell[1,r] := V.GetValueLabel(V.AsString[i], Cmd.ParameterList);
        tab.Cell[2,r] := '';
        tab.Cell[3,r] := '';
        tab.Cell[4,r] := '';
        tab.Cell[5,r] := '';
        tab.Cell[6,r] := '';
        if (not Cmd.ParamExists['NOCI']) then
        begin
          tab.Cell[7,r] := '';
          tab.Cell[8,r] := '';
        end;
      end;
    end;

    tab.AddRow;
    r := Tab.RowCount;
    tab.Cell[1,r] := ' '; //df.VectorByName['$INTERBEG'].AsString[i];
    tab.Cell[2,r] := df.VectorByName['$INTERBEG'].AsString[i] + '-' + df.VectorByName['$INTEREND'].AsString[i];
    tab.Cell[3,r] := df.VectorByName['$NUMATSTRT'].AsString[i];
    tab.Cell[4,r] := df.VectorByName['$DEATHS'].AsString[i];
    tab.Cell[5,r] := df.VectorByName['$WITHDRAWN'].AsString[i];
    tab.Cell[6,r] := Epiformat(df.VectorByName['$CMPRSURV'].AsFloat[i], tf.EFmt);
    if (not Cmd.ParamExists['NOCI']) then
    begin
      tab.Cell[7,r] := Epiformat(df.VectorByName['$STDERR'].AsFloat[i], tf.EFmt);
      tab.Cell[8,r] := EpiCIformat(0, df.VectorByName['$CILO'].AsFloat[i], df.VectorByName['$CIHI'].AsFloat[i], tf.EFmt, tf.CIFmt, tf.CIHdr,1);
    end;
  end;

  dm.CodeMaker.OutputTable(tab);
end;

function TLifeTables.CreateLifeTable(df: TEpiDataFrame; Varnames: TStrings; var OutputTable: TStatTable): TEpiDataFrame;
var
  agl: TAggrList;
  AggDF: TEpiDataFrame;
  LocalVarnames, IntervalNums: TStrings;
  V1, V2, RangeBeg, RangeEnd, OutcomeV, ByV, CountV,
  GroupV, IvBeg, IvEnd, NumStV, WithDrwV, RiskV, DthV, PrDthV,
  PrSrvV, CmPrSrvV, StdErr, NEff, CILO, CIHI: TEpiVector;
  i, j, k, St, En, total,
  DC, CeC,
  Cases, SumTime, SumCens, GrandCases, GrandTotal,
  DeadInd: Integer;
  First: Boolean;
  s: string;
  Opt: TEpiOption;
  ExitTime, AdjVal: EpiFloat;

begin
  LocalVarnames := nil;
  Agl := nil;
  try
    LocalVarnames := TStringList.Create;
    LocalVarnames.Assign(Varnames);

    // Strip weight variable - only valid for aggregation.
    if Cmd.ParamExists['W'] then
    begin
      WeightName := (Cmd.ParamByName['W'].AsString);
      Weighted := true;
      LocalVarnames.Delete(LocalVarnames.IndexOf(WeightName));
    end;

    if Cmd.ParamExists['O'] then
      DeadInd := Cmd.ParamByName['O'].AsInteger
    else
      DeadInd := 1;

    // Define posible exit value.
    ExitTime := -MaxInt;
    if Cmd.ParamExists['EXIT'] then
    begin
      V1 := Df.FindVector(Cmd.ParamByName['$TVAR'].Value);
      V2 := Df.FindVector(LocalVarnames[0]);
      s := Cmd.ParamByName['EXIT'].AsString;
      if Trim(s) = '' then
      begin
        dm.Info('EXIT value not specified - ignoring');
        s := IntToStr(-MaxInt)
      end;
      if MibIsDate(s, ftInteger) then
        ExitTime := EpiStrToDatefmt(s, '%DMY')
      else
        ExitTime := StrToFloat(s);
      s := '';
    end;

    // Correct timevar if needed by /MT
    if Cmd.ParamExists['MT'] then
    begin
      V1 := Df.FindVector(Cmd.ParamByName['$TVAR'].Value);
      V2 := Df.FindVector(LocalVarnames[0]);
      if ExitTime = -MaxInt then
        for i := 1 to Df.RowCount do
          if not (V1.IsMissing[i] or V1.IsMissingValue[i]) then
            ExitTime := Math.Max(ExitTime, V1.AsFloat[i]);
      for i := 1 to Df.RowCount do
      begin
        if (V1.IsMissing[i] or V1.IsMissingValue[i]) then
          V1.AsFloat[i] := ExitTime;
      end;
    end;

    if ExitTime <> -MaxInt then
      for i := 1 to Df.RowCount do
        if (V1.AsFloat[i] > ExitTime) then
        begin
          V1.AsFloat[i] := ExitTime;
          V2.AsInteger[i] := DeadInd +1;
        end;


    // Varnames Layout:     Range variable      |   Start/End variables
    //  ===========================================================
    //   LocalVarnames[0] = Outcome variable    |   Outcome variable
    //   LocalVarnames[1] = Range variable      |   Start variable
    //   LocalVarnames[2] = (BY)                |   End Variable
    //   LocalVarnames[3] = N/A                 |   (BY)

    // Calculate posible range
    if ((not Cmd.ParamExists['BY']) and (LocalVarnames.Count = 3)) or
        (LocalVarnames.Count = 4) then
    begin
      RangeBeg := df.VectorByName[LocalVarnames[1]].Clone(df, true);
      RangeBeg.Name := '$RANGE';
      df.Vectors.Add(RangeBeg);
      V1 := df.VectorByName[LocalVarnames[1]];
      V2 := df.VectorByName[LocalVarnames[2]];
      j := 0;
      for i := 1 to df.RowCount do
      begin
        if (V2.AsFloat[i] - V1.AsFloat[i]) < 0 then
        begin
          if j < 10 then
            dm.Info('Warning: Negative interval %s=%s and %s=%s - observation excluded', [v1.Name, v1.AsString[i], v2.Name, v2.AsString[i]]);
          inc(j);
        end;
        RangeBeg.AsFloat[i] := V2.AsFloat[i] - V1.AsFloat[i];
      end;
      LocalVarnames.Delete(2);
      LocalVarnames.Delete(1);
      LocalVarnames.Insert(1, RangeBeg.Name);
    end else begin
      RangeBeg := df.VectorByName[LocalVarnames[1]];
      j := 0;
      for i := 1 to df.RowCount do
      begin
        if (RangeBeg.AsFloat[i] < 0) then
        begin
          if (j < 10) then
            dm.Info('Warning: Negative time = %s  - observation excluded', [RangeBeg.AsString[i]]);
          inc(j);
          RangeBeg.IsMissing[i] := true;
        end;
      end;
    end;

    if j > 0 then
      dm.Info('%d obervations excluded due to negative time', [j]);

    df := df.PrepareDataframe(RangeBeg.Name);
    RangeBeg := df.VectorByName[LocalVarnames[1]];

    // =============================
    // = Find intervals.
    if Cmd.ParamExists['I'] then
      s := AnsiUpperCase(Trim(Cmd.ParamByName['I'].AsString))
    else
      s := 'B1';

    IvBeg := TEpiIntVector.Create('$INTERBEG', df.RowCount, df.CheckProperties);
    IvEnd := TEpiIntVector.Create('$INTEREND', df.RowCount, df.CheckProperties);
    df.Vectors.Add(IvBeg);
    df.Vectors.Add(IvEnd);
    df.Sort(LocalVarnames[1]);
    if Length(S) = 0 then
    begin
      dm.GetOptionValue('LIFETABLE INTERVAL', opt);
      s := opt.Value;
    end;
    if s[1] = 'B' then
    begin
      j := StrToInt(Copy(s, 2, Length(s)));
      for i := 1 to df.RowCount do
      begin
        IvBeg.AsInteger[i] := ((RangeBeg.AsInteger[i] div j) * j);
        IvEnd.AsInteger[i] := ((RangeBeg.AsInteger[i] div j) * j) + j;
        if j > 1 then
          IvEnd.AsInteger[i] := IvEnd.AsInteger[i] - 1;
      end;
    end else begin
      IntervalNums := TStringList.Create;
      SplitString(s, IntervalNums, [',']);
      St := 1;
      for i := 0 to IntervalNums.Count do
      begin
        if i < IntervalNums.Count then
          k := StrToInt(IntervalNums[i])-1;
        if (i = 0) then
        begin
          while (St <= df.RowCount) and (RangeBeg.AsInteger[St] <= k) do
          begin
            IvBeg.IsMissing[St] := true;
            IvEnd.AsInteger[St] := k;
            inc(st);
          end;
        end
        else if (i = IntervalNums.Count) then
        begin
          k := StrToInt(IntervalNums[i-1])-1;
          while (St <= df.RowCount) and (RangeBeg.AsInteger[St] > k) do
          begin
            IvBeg.AsInteger[St] := (k+1);
            IvEnd.IsMissing[St] := true;
            inc(st);
          end;
        end else begin
          while (St <= df.RowCount) and (RangeBeg.AsInteger[St] <= k) do
          begin
            IvBeg.AsInteger[St] := StrToInt(IntervalNums[i-1]);
            IvEnd.AsInteger[St] := k;
            inc(st);
          end;
        end;
      end;
      FreeAndNil(IntervalNums);
    end;
    LocalVarnames.Delete(1);
    LocalVarnames.Insert(1, IvEnd.Name);
    LocalVarnames.Insert(1, IvBeg.Name);
    df.Vectors.Remove(RangeBeg);
    FreeAndNil(RangeBeg);
    // End Intervals.
    // =============================

    // Varnames Layout:    
    //  =======================================
    //   LocalVarnames[0] = Outcome variable
    //   LocalVarnames[1] = Inteval begin
    //   LocalVarnames[2] = Interval end
    //   LocalVarnames[3] = (BY)
    s := '';
    agl := TAggrList.Create();
    if not Weighted then
      agl.Insert(0, TAggrCount.Create('$S', '', acAll))
    else
      agl.Insert(0, TAggrSum.Create('$S', WeightName));

    AggDF := OAggregate.AggregateDataframe(df, TStringList(LocalVarnames), agl, Cmd);

    if Cmd.ParamExists['BY'] then
    begin
      s := LocalVarnames[3] + ',';
      ByV := AggDF.FindVector(Cmd.ParamByName['BY'].AsString);
    end else begin
      ByV := TEpiIntVector.Create('$BY', AggDF.RowCount);
      For i := 1 to ByV.Length do
        ByV.AsInteger[i] := 1;
    end;

    AggDF.Sort(s + LocalVarnames[1] + ',' + LocalVarnames[0]);
//    OAggregate.OutAggregate(AggDF);

    // This might be larger than required, but the final size
    // is not known at this stage. However the final size is never
    // larger than the original size.
    Result := TEpiDataFrame.CreateTemp(AggDF.RowCount);
    AggDF.CheckProperties.Clone(result.CheckProperties);
    Result.FileName := AggDF.FileName;
    Result.Modified := true;

    OutcomeV   := AggDF.FindVector(LocalVarnames[0]);
    RangeBeg   := AggDF.FindVector(LocalVarnames[1]);
    RangeEnd   := AggDF.FindVector(LocalVarnames[2]);
    CountV     := AggDF.FindVector('$S');
    IvBeg      := TEpiIntVector.Create('$INTERBEG', AggDF.RowCount, df.CheckProperties);
    IvEnd      := TEpiIntVector.Create('$INTEREND', AggDF.RowCount, df.CheckProperties);
    NumStV     := TEpiIntVector.Create('$NUMATSTRT', AggDF.RowCount, df.CheckProperties);
    WithDrwV   := TEpiIntVector.Create('$WITHDRAWN', AggDF.RowCount, df.CheckProperties);
    RiskV      := TEpiFloatVector.Create('$RISK', AggDF.RowCount, df.CheckProperties);
    DthV       := TEpiIntVector.Create('$DEATHS', AggDF.RowCount, df.CheckProperties);
    PrDthV     := TEpiFloatVector.Create('$PRDEATHS', AggDF.RowCount, df.CheckProperties);
    PrSrvV     := TEpiFloatVector.Create('$PRSURV', AggDF.RowCount, df.CheckProperties);
    CmPrSrvV   := TEpiFloatVector.Create('$CMPRSURV', AggDF.RowCount, df.CheckProperties);
    NEff       := TEpiIntVector.Create('$NEFFECT', AggDF.RowCount, df.CheckProperties);

    if Cmd.ParamExists['BY'] then
    begin
      GroupV   := ByV.Clone(result, true);
      result.Vectors.Add(GroupV);
    end;
    result.Vectors.Add(IvBeg);
    result.Vectors.Add(IvEnd);
    result.Vectors.Add(NumStV);
    result.Vectors.Add(WithDrwV);
    result.Vectors.Add(RiskV);
    result.Vectors.Add(DthV);
    result.Vectors.Add(PrDthV);
    result.Vectors.Add(PrSrvV);
    result.Vectors.Add(CmPrSrvV);
    result.Vectors.Add(NEff);

    if (not Cmd.ParamExists['NOCI']) then
    begin
      StdErr     := TEpiFloatVector.Create('$STDERR', AggDF.RowCount, df.CheckProperties);
      CILO       := TEpiFloatVector.Create('$CILO', AggDF.RowCount, df.CheckProperties);
      CIHI       := TEpiFloatVector.Create('$CIHI', AggDF.RowCount, df.CheckProperties);
      result.Vectors.Add(StdErr);
      result.Vectors.Add(CILO);
      result.Vectors.Add(CIHI);
    end;

    St := 1;
    En:= 1;
    k := 1;

    // Summation table:
    OutputTable := dm.CodeMaker.Output.NewTable(6);
    OutputTable.TableType := sttNormal;
    OutputTable.Cell[1,1] := '  ';
    OutputTable.Cell[2,1] := 'Total<br><small>N</small>';
    OutputTable.Cell[3,1] := 'Cases<br><small>n</small>';
    OutputTable.Cell[4,1] := 'Time<br><small>Minimum</small>';
    OutputTable.Cell[5,1] := '<br><small>Maximum</small>';
    OutputTable.Cell[6,1] := '<br><small>Sum <sub>time</sub></small>';
    OutputTable.Caption := 'Life Table: ' + OutcomeV.GetVariableLabel(Cmd.ParameterList);
    if Cmd.ParamExists['BY'] then
      OutputTable.Caption := OutputTable.Caption + ' by ' + ByV.GetVariableLabel(Cmd.ParameterList);

    s := 'Outcome: ' + OutcomeV.GetVariableLabel(Cmd.ParameterList) + ' = ' +
         OutcomeV.GetValueLabel(IntToStr(DeadInd), Cmd.ParameterList);
    OutputTable.Footer := s;

    GrandCases := 0;
    GrandTotal := 0;
    dm.AddResult('$OUTCOME', EpiTyInteger, DeadInd, 8,0);

    while true do
    begin
      total := 0;
      for En := St to AggDF.RowCount do begin
        inc(total, CountV.AsInteger[En]);
        if (En = result.RowCount) or (ByV.compare(En, En+1)<>0) then
          break;
      end;

      i := St;
      First := true;
      Cases := 0;
      SumTime := 0;
      SumCens := 0;
      OutputTable.AddRow;

      while i <= En do
      begin
        DC := 0;
        CeC := 0;
        if Cmd.ParamExists['BY'] then
        begin
          GroupV.AsInteger[k] := ByV.AsInteger[i];
          OutputTable.Cell[1, OutputTable.RowCount] := ByV.GetValueLabel(ByV.AsString[i], Cmd.ParameterList);
        end else
          OutputTable.Cell[1, OutputTable.RowCount] := '';

        IvBeg.AsInteger[k] := RangeBeg.AsInteger[i];
        IvEnd.AsInteger[k] := RangeEnd.AsInteger[i];
        repeat
          if OutcomeV.AsInteger[i] = DeadInd then
            inc(DC, CountV.AsInteger[i])
          else
            inc(CeC, CountV.AsInteger[i]);
          inc(i);
        until (i > En) or (RangeBeg.compare(i-1, i) <> 0);
        WithDrwV.AsInteger[k]    := CeC;
        DthV.AsInteger[k]        := DC;
        if not Cmd.ParamExists['ADJ'] then
          AdjVal := CeC
        else
          AdjVal := CeC / 2;
        Inc(SumTime, (CeC + DC) * IvBeg.AsInteger[k]);
        if First then
        begin
          NumStV.AsInteger[k]      := total;
          RiskV.AsFloat[k]         := NumStV.AsInteger[k] - AdjVal;
          PrDthV.AsFloat[k]        := DC / (NumStV.AsInteger[k]);
          PrSrvV.AsFloat[k]        := 1 - PrDthV.AsFloat[k];
          CmPrSrvV.AsFloat[k]      := PrSrvV.AsFloat[k];
          First := false;
          OutputTable.Cell[2, OutputTable.RowCount] := NumStV.AsString[k];
          OutputTable.Cell[4, OutputTable.RowCount] := IvBeg.AsString[k];
        end else begin
          NumStV.AsInteger[k]      := NumStV.AsInteger[k-1] - (WithDrwV.AsInteger[k-1] + DthV.AsInteger[k-1]);
          RiskV.AsFloat[k]         := NumStV.AsInteger[k] - AdjVal;
          PrDthV.AsFloat[k]        := DC / (NumStV.AsInteger[k]);
          PrSrvV.AsFloat[k]        := 1 - PrDthV.AsFloat[k];
          CmPrSrvV.AsFloat[k]      := CmPrSrvV.AsFloat[k-1] * PrSrvV.AsFloat[k];
        end;
        NEff.AsInteger[k]          := Total - SumCens;
        if (not Cmd.ParamExists['NOCI']) then
        begin
          StdErr.AsFloat[k] := System.Sqrt((CmPrSrvV.AsFloat[k] * (1 - CmPrSrvV.AsFloat[k])) / NEff.AsInteger[k]);
          CILO.AsFloat[k]   := CmPrSrvV.AsFloat[k] - (1.96 * StdErr.AsFloat[k]);
          CIHI.AsFloat[k]   := CmPrSrvV.AsFloat[k] + (1.96 * StdErr.AsFloat[k]);
        end;
        Inc(Cases, DC);
        Inc(SumCens, CeC);
        inc(k);
      end;
      dm.AddResult('$CASE' + ByV.AsString[St], EpiTyInteger, Cases, 8, 0);
      dm.AddResult('$TOTAL' + ByV.AsString[St], EpiTyInteger, Total, 8, 0);
      inc(GrandCases, Cases);
      inc(GrandTotal, Total);
      OutputTable.Cell[3, OutputTable.RowCount] := IntToStr(Cases);
      OutputTable.Cell[5, OutputTable.RowCount] := IvEnd.AsString[k-1];
      OutputTable.Cell[6, OutputTable.RowCount] := IntToStr(SumTime);
      if En = AggDF.RowCount then break;
      St := i;
    end;
      dm.AddResult('$CASES', EpiTyInteger, GrandCases, 8, 0);
      dm.AddResult('$NONCASES', EpiTyInteger, GrandTotal - GrandCases, 8, 0);
      dm.AddResult('$TOTAL', EpiTyInteger, GrandTotal, 8, 0);
    Result.Capacity := k-1;
  finally
    if Assigned(LocalVarnames) then FreeAndNil(LocalVarnames);
    if Assigned(Agl) then FreeAndNil(Agl);
  end;
end;

// ============================================================================
// Private/Protected methodes.
// ============================================================================

procedure TLifeTables.Stats(dataframe: TEpiDataframe; xTab: TStatTable);
var
  df: TEpiDataframe;
  i, j, st, en, idx, ref,
  Dths, WthD, STotal: integer;
  ByVec, TimeVec, DthVec, WthDrwVec, NumVec: TEpiVector;
  Pd, V, X, Y: EpiFloat;
  ESum, VHaz: Array of EpiFloat;
  ATotal, ADeath, AWithD, SDeath: Array of Integer;
  Varnames: TStrings;
  Fmts: TTableFormats;

  function SumTotal(): integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to Length(ATotal)-1 do
      if ATotal[i] <> NA_INT then
        inc(Result, ATotal[i]);
  end;

  function SumDeath(): integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to Length(ADeath)-1 do
      if ADeath[i] <> NA_INT then
        inc(Result, ADeath[i]);
  end;

  function SumWithD(): integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to Length(AWithD)-1 do
      if AWithD[i] <> NA_INT then
        inc(Result, AWithD[i]);
  end;

  function GetIndex(Idx: integer): integer;
  begin
    if ByVec.IsMissing[idx] then
      result := (en -st)
    else
      result := ByVec.AsInteger[idx] - st;
  end;

begin

  if (not Cmd.ParamExists['BY']) then exit;

  OTables.Getformats(Cmd, Fmts);

  Varnames := nil;
  df := Dataframe.PrepareDataframe(varnames, nil);
  ByVec := Df.VectorByName[Cmd.ParamByName['BY'].AsString];

  // Find stratification levels.
  st := ByVec.AsInteger[1];
  i := df.RowCount;
  while ByVec.IsMissing[i] do dec(i);
  en := ByVec.AsInteger[i];
  if (i <> df.RowCount) then inc(en);

  if (st - en) = 0 then
  begin
    dm.Info('To few groups for lifetable statistics');
    exit;
  end;

  if Cmd.ParamExists['REF'] then
    Ref := (Cmd.ParamByName['REF'].AsInteger - st)
  else
    Ref := 0;

  // Intialize arrays based on stratas
  SetLength(ESum, (en - st)+1);
  SetLength(VHaz, (en - st)+1);
  SetLength(ATotal, (en - st)+1);
  SetLength(SDeath, (en - st)+1);
  SetLength(ADeath, (en - st)+1);
  SetLength(AWithD, (en - st)+1);
  for i := 0 to (en - st) do
  begin
    ESum[i] := 0;
    ATotal[i] := NA_INT;
    SDeath[i] := 0;
    ADeath[i] := NA_INT;
    AWithD[i] := NA_INT;
  end;

  // Sort and prepare for counting.
  df.Sort('$INTERBEG,' + ByVec.Name);
//  df.SendToOutput(nil);

  TimeVec := df.VectorByName['$INTERBEG'];
  DthVec  := df.VectorByName['$DEATHS'];
  WthDrwVec := df.VectorByName['$WITHDRAWN'];
  NumVec := df.VectorByName['$NUMATSTRT'];

  // Find initial total
  for i := 1 to df.RowCount do
  begin
    Idx := GetIndex(i);
    if ATotal[idx] = NA_INT then
      ATotal[idx] := NumVec.AsInteger[i];
  end;

  i := 2;
  Idx := GetIndex(1);
  ADeath[Idx] := DthVec.AsInteger[1];
  SDeath[Idx] := ADeath[Idx];
  AWithD[Idx] := WthDrwVec.AsInteger[1];
  while (true) do
  begin
    while (i<=df.RowCount) and (TimeVec.compare(i-1, i) = 0) do
    begin
      Idx := GetIndex(i);
      ADeath[idx] := DthVec.AsInteger[i];
      SDeath[Idx] := SDeath[Idx] + ADeath[Idx];
      AWithD[Idx] := WthDrwVec.AsInteger[i];
      inc(i);
    end;

    // Calculate estimated and hazard ratio.
    Dths := SumDeath();
    STotal := SumTotal();
    Pd := Dths / STotal;
    for j := 0 to Length(ESum)-1 do
    begin
      if ATotal[j] = NA_INT then continue;
      ESum[j] := ESum[j] + (Pd * ATotal[j]);
      if STotal > 1 then
        VHaz[j] := VHaz[j] + (ATotal[j] * ATotal[ref] * Dths * (STotal - Dths)) /
                   (IntPower(STotal, 2) * (STotal - 1));
    end;

    //Reset
    for j := 0 to Length(ESum)-1 do
    begin
      if ADeath[j] <> NA_INT then
        ATotal[j] := ATotal[j] - ADeath[j];
      if AWithD[j] <> NA_INT then
        ATotal[j] := ATotal[j] - AWithD[j];
      ADeath[j] := NA_INT;
      AWithD[j] := NA_INT;
    end;

    if i > df.RowCount then break;
    Idx := GetIndex(i);
    ADeath[idx] := DthVec.AsInteger[i];
    SDeath[Idx] := SDeath[Idx] + ADeath[Idx];
    AWithD[Idx] := WthDrwVec.AsInteger[i];
    inc(i);
  end;
  j := 2;
  xtab.InsertColumn(3);
  xtab.Cell[4,1] := 'Expected<br>Cases';
  for i := 0 to Length(ESum)-1 do
  begin
    if ESum[i] <> 0 then
    begin
      xtab.Cell[4,j] := Format(Fmts.EFmt, [ESum[i]]);
      dm.AddResult('$ECASE' + IntToStr(i+st), EpiTyFloat, ESum[i], 8,4);
      inc(j);
    end;
  end;

  // Log-Rank: Chi2 test.
  Pd := 0; // Reuse of Pd for Chi2 result.
  en := 0; // Count of groups.
  for i := 0 to Length(SDeath)-1 do
    if SDeath[i] > 0 then
    begin
      Pd := Pd + (IntPower(SDeath[i] - ESum[i], 2) / ESum[i]);
      inc(en);
    end;
  xTab.Footer := xTab.Footer +
                 Format('<br>Log Rank test of equality of survivor function: Chi<sup>2</sup>(%d)=', [en-1]) +
                 EpiFormat(Pd,'%7.3f ') + '  P= ' +
                 EpiFormat(PCHI2(en-1, Pd),'%7.4f ');
  Dm.AddResult('$LRANKCHI2', EpiTyFloat, Pd, 7, 3);
  Dm.AddResult('$LRANKP', EpiTyFloat, PCHI2(en-1, Pd), 7, 3);

  // Log-likelihood: Chi2 test.
  Dths := 0;
  for i := 0 to Length(SDeath)-1 do
    if SDeath[i] > 0 then
      inc(Dths, SDeath[i]);      // Sum of deaths
  Pd := 0;
  For i := 2 to xTab.RowCount do
    Pd := Pd + StrToFLoat(xTab.Cell[xTab.ColCount, i]);   // Sum of times
  Pd := Dths * System.Ln(Pd / Dths);

  j := 2; // Index of first group.
  for i := 0 to Length(SDeath)-1 do
    if SDeath[i] > 0 then
    begin
      Pd := Pd - SDeath[i] * System.Ln(StrToFLoat(xTab.Cell[xTab.ColCount, j]) / SDeath[i]);
      inc(j);
    end;
  Pd := Pd * 2;

  xTab.Footer := xTab.Footer +
                 Format('<br>LR test of homogeneity among groups Chi<sup>2</sup>(%d)=', [en-1]) +
                 EpiFormat(Pd,'%7.3f ') + '  p= ' +
                 EpiFormat(PCHI2(en-1, Pd),'%7.4f ');
  Dm.AddResult('$LLIKECHI2', EpiTyFloat, Pd, 7, 3);
  Dm.AddResult('$LLIKEP', EpiTyFloat, PCHI2(en-1, Pd), 7, 3);

  j := 2;
  // Hazard ratio
  xtab.AddColumn;
  xtab.Cell[xTab.ColCount,1] := 'Hazard<br>Ratio';
  idx := 0;
  if not Cmd.ParamExists['NOCI'] then
  begin
    inc(idx);
    xTab.AddColumn;
    xtab.Cell[xTab.ColCount,1] := '<br>' + Fmts.CIHdr;
  end;
  for i := 0 to Length(ESum)-1 do
  begin
    if ESum[i] <> 0 then
    begin
      X := (SDeath[i] - ESum[i]) / VHaz[i];
      Y := 1.96 / (Sqrt(VHaz[i]));
      xtab.Cell[xTab.ColCount-idx,j] := Format(Fmts.EFmt, [Exp(X)]);
      if not Cmd.ParamExists['NOCI'] then
        xtab.Cell[xTab.ColCount,j] := EpiCIFormat(0, Exp(X-Y), Exp(X+Y), Fmts.EFmt, Fmts.CIFmt,'',0);
      if i = ref then
      begin
        xtab.Cell[xTab.ColCount-idx,j] := 'Ref.';
        if not Cmd.ParamExists['NOCI'] then
          xtab.Cell[xTab.ColCount,j] := ' ';
      end;
      inc(j);
    end;
  end;
end;

procedure TLifeTables.Percentiles(dataframe: TEpiDataframe; xTab: TStatTable);
var
  Df: TEpiDataframe;
  NEff: TEpiVector;
  Varnames: TStrings;
begin
  Varnames := nil;
  df := Dataframe.PrepareDataframe(varnames, nil);

  if Cmd.ParamExists['P25'] then
    AddPercentile(df, xtab, 0.25);
  if Cmd.ParamExists['P50'] then
    AddPercentile(df, xtab, 0.50);
  if Cmd.ParamExists['P75'] then
    AddPercentile(df, xtab, 0.75);
end;

procedure TLifeTables.AddPercentile(Dataframe: TEpiDataframe; xTab: TStatTable; const Percentile: EpiFloat);
var
  ByVec, NEffVec, SurvVec, TimeVec: TEpiVector;
  Idx, i, j, s, St, En, Ref: Integer;
  Dummy, M, MRef, StdErr, StdErrRef: EpiFloat;
  Fmts: TTableFormats;
  str: string;

  function FindCensIndex(var Factor: EpiFloat): integer;
  var
    i: integer;
  begin
    Factor := 0;
    for result := St to En do
      if SurvVec.AsFloat[Result] <= Percentile then exit;
    Factor := -1;
  end;

  function StdErrFactor(): EpiFloat;
  var
    p, t: EpiFloat;
    i: integer;
  begin
    for i := En downto St do
      if SurvVec.AsFloat[i] > (Percentile - 0.05) then break;
    t := TimeVec.AsFloat[Math.Min(i+1, En)];
    p := SurvVec.AsFloat[Math.Min(i+1, En)];
    for i := St to En do
      if SurvVec.AsFloat[i] < (Percentile + 0.05) then break;
    t := t - TimeVec.AsFloat[Math.Max(i-1, St)];
    p := SurvVec.AsFloat[Math.Max(i-1, St)] - p;
    result := t / p;
  end;

begin
  OTables.Getformats(Cmd, Fmts);

  s := 0;
  xTab.AddColumn;
  xTab.Cell[xTab.ColCount, 1] := 'Time<br>S=0.' + IntToStr(Trunc(Percentile*100));

  if not Cmd.ParamExists['NOCI'] then
  begin
    xTab.AddColumn;
    xTab.Cell[Xtab.ColCount, 1] := '<br>' + Fmts.CIHdr;
    inc(s);
  end;

  if Cmd.ParamExists['BY'] then
  begin
    ByVec := Dataframe.VectorByName[Cmd.ParamByName['BY'].AsString];
    xTab.AddColumn;
    xTab.Cell[xTAb.ColCount, 1] := 'Time<br>Diff.';
    inc(s);
    if not Cmd.ParamExists['NOCI'] then
    begin
      xTab.AddColumn;
      xTab.Cell[Xtab.ColCount, 1] := '<br>' + Fmts.CIHdr;
      inc(s);
    end;
  end else
    ByVec := TEpiIntVector.Create('$BY', Dataframe.RowCount);

  if Cmd.ParamExists['REF'] then
    Ref := (Cmd.ParamByName['REF'].AsInteger)
  else
    Ref := ByVec.AsInteger[1];

  SurvVec := Dataframe.VectorByName['$CMPRSURV'];
  TimeVec := Dataframe.VectorByName['$INTERBEG'];
  NEffVec := Dataframe.VectorByName['$NEFFECT'];

  if Cmd.ParamExists['BY'] then
  begin
    // Find and calculate Reference value.
    i := 1;
    st := 1;
    MRef := 0;
    dec(ref);
    repeat
      inc(ref);
      while ByVec.AsInteger[st] <> Ref do inc(st);
      for i := St+1 to Dataframe.RowCount do
        if ByVec.compare(i-1,i) <> 0 then break;
      En := i-1;
      Dummy := 0;
      if NEffVec.compare(St, En) = 0 then
        Idx := PercentileIndex(NEffVec.Length, Percentile, Dummy) + St
      else
        Idx := FindCensIndex(Dummy);
      if (Dummy > 0) and (Idx >= St) and (Idx+1 <= En) then
        MRef := (TimeVec.AsInteger[idx]+TimeVec.AsInteger[idx+1])/2
      else if Idx <= en then
        MRef := TimeVec.AsInteger[idx];
      if Idx <= en then
       StdErrRef := System.Sqrt((Percentile * (1 - Percentile)) / NEffVec.AsInteger[Idx]) * StdErrFactor();
      st := en + 1;
      if Cmd.ParamExists['REF'] then
        break
    until (MRef > 0) or (st > Dataframe.RowCount);
  end;

  if Mref > 0 then
    dm.AddResult('$P' + IntToStr(Trunc(Percentile*100)) + 'REF', EpiTyInteger, Ref, 2, 0);

  St := 1;
  j := 2;
  while (true) do
  begin
    for i := St+1 to Dataframe.RowCount do
      if ByVec.compare(i-1,i) <> 0 then break;
    En := i-1;

    Dummy := 0;
    if NEffVec.compare(St, En) = 0 then
      Idx := PercentileIndex(NEffVec.Length, Percentile, Dummy) + St
    else
      Idx := FindCensIndex(Dummy);

    if (Dummy >= 0) then
    begin
      if (Dummy > 0) and (Idx+1 <= En) then
        M := (TimeVec.AsInteger[idx]+TimeVec.AsInteger[idx+1])/2
      else
        M := TimeVec.AsInteger[idx];

      xTab.Cell[xTab.ColCount - S, j] := Epiformat(M, Fmts.EFmt);
      str := '';
      if Cmd.ParamExists['BY'] then
        str := 'G' + ByVec.AsString[idx];
      dm.AddResult('$P' + IntToStr(Trunc(Percentile*100)) + str, EpiTyFloat, M, 6, 4);
    end else begin
      xTab.Cell[xTab.ColCount - S, j] := '';
      xTab.Footer := xTab.Footer + '<br>No valid time for S(time)=0.' + IntToStr(Trunc(Percentile*100));
      if Cmd.ParamExists['BY'] then
        xTab.Footer := xTab.Footer + ' with ' + ByVec.GetVariableLabel(Cmd.ParameterList) + '=' +
        ByVec.GetValueLabel(ByVec.AsString[idx-1], Cmd.ParameterList);
    end;

    if not Cmd.ParamExists['NOCI'] then
    begin
      if dummy < 0 then
        xTab.Cell[xTab.ColCount - S+1, j] := ''
      else begin
        StdErr := System.Sqrt((Percentile * (1 - Percentile)) / NEffVec.AsInteger[Idx]) * StdErrFactor();
        xTab.Cell[xTab.ColCount - S+1, j] := EpiCIFormat(0, M-(1.95*StdErr), M+(1.95*StdErr), Fmts.EFmt, Fmts.CIFmt, '', 0);
      end;
    end;

    if cmd.ParamExists['BY'] then
    begin
      if Dummy >= 0 then
      begin
        dm.AddResult('$P'+ IntToStr(Trunc(Percentile*100)) + 'DIFF'+ByVec.AsString[idx], EpiTyFloat, M - MRef, 6, 4);
        if Cmd.ParamExists['NOCI'] then
        begin
          if ByVec.AsInteger[idx] = Ref then
            xTab.Cell[Xtab.ColCount - s + 1, j] := 'Ref.'
          else
            xTab.Cell[Xtab.ColCount - s + 1, j] := Epiformat(M - Mref, Fmts.EFmt);
        end else begin
          if ByVec.AsInteger[idx] = Ref then
            xTab.Cell[Xtab.ColCount - s + 2, j] := 'Ref.'
          else
            xTab.Cell[Xtab.ColCount - s + 2, j] := Epiformat(M - Mref, Fmts.EFmt);
        end;

        if not Cmd.ParamExists['NOCI'] then
        begin
          StdErr := System.Sqrt(StdErr*StdErr + StdErrRef*StdErrRef);
          if dummy < 0 then
            xTab.Cell[xTab.ColCount, j] := ''
          else
            if xTab.Cell[xTab.ColCount-1, j] <> 'Ref.' then
              xTab.Cell[xTab.ColCount, j] := EpiCIFormat(0, (M-MRef)-(1.95*StdErr), (M-MRef)+(1.95*StdErr), Fmts.EFmt, Fmts.CIFmt, '', 0);
        end;
      end;
      inc(j);
    end;
    St := En + 1;
    if En >= Dataframe.RowCount then break;
  end;
  for i := 2 to xTab.RowCount do
    if Trim(xTab.Cell[xTab.ColCount-S, i]) <> '' then break;
  if i > xTab.RowCount then
  begin
    // There are no entrie in the table.
    for i := 0 to s do
      xtab.DeleteColumn(xTab.ColCount);
  end;
end;

procedure TLifeTables.AddTime(dataframe: TEpiDataframe; xTab: TStatTable; const Time: Integer);
var
  ByVec, NEffVec, SurvVec, TimeVec, CIHIVec, CILOVec: TEpiVector;
  Idx, i, j, s, St, En, Ref: Integer;
  Dummy, P, PRef, StdErr, StdErrRef: EpiFloat;
  Fmts: TTableFormats;
  str: string;

  function FindIndex(var Factor: EpiFloat): integer;
  var
    i: integer;
  begin
    Factor := 0;
    for result := St to En do
      if TimeVec.AsFloat[Result] >= Time then exit;
    Factor := -1;
  end;

begin
  OTables.Getformats(Cmd, Fmts);

  s := 0;
  xTab.AddColumn;
  xTab.Cell[xTab.ColCount, 1] := 'Survival<br>T=' + IntToStr(Time);

  if not Cmd.ParamExists['NOCI'] then
  begin
    xTab.AddColumn;
    xTab.Cell[Xtab.ColCount, 1] := '<br>' + Fmts.CIHdr;
    inc(s);
  end;

  if Cmd.ParamExists['BY'] then
  begin
    ByVec := Dataframe.VectorByName[Cmd.ParamByName['BY'].AsString];
    xTab.AddColumn;
    xTab.Cell[xTAb.ColCount, 1] := 'Survival<br>Diff.';
    inc(s);
    if not Cmd.ParamExists['NOCI'] then
    begin
      xTab.AddColumn;
      xTab.Cell[Xtab.ColCount, 1] := '<br>' + Fmts.CIHdr;
      inc(s);
    end;
  end else
    ByVec := TEpiIntVector.Create('$BY', Dataframe.RowCount);

  if Cmd.ParamExists['REF'] then
    Ref := (Cmd.ParamByName['REF'].AsInteger)
  else
    Ref := ByVec.AsInteger[1];

  SurvVec := Dataframe.VectorByName['$CMPRSURV'];
  TimeVec := Dataframe.VectorByName['$INTERBEG'];
  NEffVec := Dataframe.VectorByName['$NEFFECT'];
  if not Cmd.ParamExists['NOCI'] then
  begin
    CIHIVec := Dataframe.VectorByName['$CIHI'];
    CILOVec := Dataframe.VectorByName['$CILO'];
  end;

  if Cmd.ParamExists['BY'] then
  begin
    // Calculate Reference value.
    i := 1;
    St := 1;
    PRef := 0;
    dec(ref);
    repeat
      inc(ref);
      while ByVec.AsInteger[st] <> Ref do inc(st);
      for i := St+1 to Dataframe.RowCount do
        if ByVec.compare(i-1,i) <> 0 then break;
      En := i-1;
      Dummy := 0;
      Idx := FindIndex(Dummy);
      if (Dummy >= 0) then
      begin
        PRef := SurvVec.AsFloat[idx];
        StdErrRef := (PRef * (1 - PRef)) / NEffVec.AsInteger[idx];
      end;
      st := en + 1;
      if Cmd.ParamExists['REF'] then
        break;
    until (PRef > 0) or (st > Dataframe.RowCount);
  end;

  if Pref > 0 then
    dm.AddResult('$T' + IntToStr(Time) + 'REF', EpiTyInteger, Ref, 2, 0);

  St := 1;
  j := 2;
  while (true) do
  begin
    for i := St+1 to Dataframe.RowCount do
      if ByVec.compare(i-1,i) <> 0 then break;
    En := i-1;

    Dummy := 0;
    Idx := FindIndex(Dummy);

    if (Dummy >= 0) then
      P := SurvVec.AsFloat[idx];


    if Dummy >= 0 then
    begin
      xTab.Cell[xTab.ColCount - S, j] := Epiformat(P, Fmts.EFmt);
      str := '';
      if Cmd.ParamExists['BY'] then
        str := 'G' + ByVec.AsString[idx];
      dm.AddResult('$T' + IntToStr(Time) + str, EpiTyFloat, P, 6, 4);
    end else begin
      xTab.Cell[xTab.ColCount - S, j] := '';
      xTab.Footer := xTab.Footer + '<br>No valid survival for T=' + IntToStr(Time);
      if Cmd.ParamExists['BY'] then
        xTab.Footer := xTab.Footer + ' with ' + ByVec.GetVariableLabel(Cmd.ParameterList) + '=' +
        ByVec.GetValueLabel(ByVec.AsString[idx-1], Cmd.ParameterList);
    end;

    if not Cmd.ParamExists['NOCI'] then
    begin
      if dummy < 0 then
        xTab.Cell[xTab.ColCount - S+1, j] := ''
      else
        xTab.Cell[xTab.ColCount - S+1, j] := EpiCIFormat(0, CILOVec.AsFloat[idx], CIHIVec.AsFloat[idx], Fmts.EFmt, Fmts.CIFmt, '', 0);
    end;

    if cmd.ParamExists['BY'] then
    begin
      if Dummy >= 0 then
      begin
        dm.AddResult('$T'+IntToStr(Time)+'DIFF'+ByVec.AsString[idx], EpiTyFloat, P - PRef, 6, 4);
        if Cmd.ParamExists['NOCI'] then
        begin
          if ByVec.AsInteger[idx] = Ref then
            xTab.Cell[Xtab.ColCount - s + 1, j] := 'Ref.'
          else
            xTab.Cell[Xtab.ColCount - s + 1, j] := Epiformat(P - Pref, Fmts.EFmt);
        end else begin
          if ByVec.AsInteger[idx] = Ref then
            xTab.Cell[Xtab.ColCount - s + 2, j] := 'Ref.'
          else
            xTab.Cell[Xtab.ColCount - s + 2, j] := Epiformat(P - Pref, Fmts.EFmt);
        end;

        if not Cmd.ParamExists['NOCI'] then
        begin
          StdErr := System.Sqrt(((P * (1 - P)) / NEffVec.AsInteger[idx]) + StdErrRef);
          if dummy < 0 then
            xTab.Cell[xTab.ColCount, j] := ''
          else
            if xTab.Cell[xTab.ColCount-1, j] <> 'Ref.' then
              xTab.Cell[xTab.ColCount, j] := EpiCIFormat(0, (P-PRef)-(1.95*StdErr), (P-PRef)+(1.95*StdErr), Fmts.EFmt, Fmts.CIFmt, '', 0);
        end;
      end;
      inc(j);
    end;
    St := En + 1;
    if En >= Dataframe.RowCount then break;
  end;
  for i := 2 to xTab.RowCount do
    if Trim(xTab.Cell[xTab.ColCount-S, i]) <> '' then break;
  if i > xTab.RowCount then
  begin
    // There are no entrie in the table.
    for i := 0 to s do
      xtab.DeleteColumn(xTab.ColCount);
  end;
end;

procedure TLifeTables.Times(dataframe: TEpiDataframe; xTab: TStatTable);
var
  i: integer;
begin
  for i := 0 to Cmd.ParameterCount-1 do
    if AnsiUpperCase(Cmd.Param[i].VarName) = 'TIME' then
      AddTime(dataframe, xTab, Cmd.Param[i].AsInteger);
end;

end.
