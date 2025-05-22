unit means;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  executor, result_variables, interval_types, epifields_helper, ana_globals,
  outputcreator;

type

  { TMeansDatafile }

  TMeansDatafile = class(TIntervalDecriptivesDatafile)
  private
    FAnovaRecord: TAnovaRecord;
    FCountVarText: UTF8String;
    FStratifyVarText: UTF8String;
    FTotalObs: Integer;
    FTotalSSQ: EpiFloat;
    FTotalSum: EpiFloat;
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    property AnovaRecord: TAnovaRecord read FAnovaRecord;
    property CountVarText: UTF8String read FCountVarText;
    property StratifyVarText: UTF8String read FStratifyVarText;
    property TotalSum: EpiFloat read FTotalSum;
    property TotalObs: Integer read FTotalObs;
    property TotalSSQ: EpiFloat read FTotalSSQ;
  end;

  { TMeans }

  TMeans = class
  private
    FDecimals: Integer;
    FConf: Integer;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    procedure FillDescriptor(ResultDF: TMeansDatafile; CountVar: TEpiField; SIdx, EIdx: Integer; ASum: EpiFloat);
    procedure DoCalcAnova(ResultDF: TMeansDatafile; CountVar: TEpiField);
  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;

    function  DoCalcMeans(InputDF: TEpiDataFile; Const CountVarName, StratifyVarName: UTF8String): TMeansDatafile; virtual;
    procedure DoResultVariables(ResultDF: TMeansDatafile); virtual;
    procedure DoOutputMeans(ResultDF: TMeansDatafile); virtual;
    procedure DoOutputAnova(AnovaRec: TAnovaRecord); virtual;
    procedure DoOutputTTest(AnovaRec: TAnovaRecord); virtual;
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;

    // Method called from Executor, does calculation + result vars + output
    procedure ExecMeans(ST: TMeansCommand);
//    procedure ExecMeans(DataFile: TEpiDataFile; ST: TMeansCommand);
    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function CalcMeans(DataFile: TEpiDataFile; Const CountVarName, StratifyVarName: UTF8String;
      ValueLabelOutput: TEpiGetValueLabelType = gvtValue; VariableLabelOutput: TEpiGetVariableLabelType = gvtVarName): TMeansDatafile;
  end;

implementation

uses
  generalutils, Math, statfunctions, options_utils;

{ TMeansDatafile }

constructor TMeansDatafile.Create(AOwner: TEpiCustomBase; const ASize: integer);
begin
  inherited Create(AOwner, ASize);
end;


{ TMeans }

procedure TMeans.FillDescriptor(ResultDF: TMeansDatafile; CountVar: TEpiField;
  SIdx, EIdx: Integer; ASum: EpiFloat);
var
  Idx, Obs, i: Integer;
  lMean, lAvgDev, lStdDev, lSSqDev, lStdErr, lSkew, lKurt, Val, lTvalue: EpiFloat;
  lCfiVal: Extended;
  lConf: Extended;

  function GetPercentile(min, max, d: integer):EpiFloat;
  var
    w : EpiFloat;
    ix: integer;
  begin
    // Try new method.
    ix := PercentileIndexNIST(max-min+1, d/100, w);
    if w = 0 then
      result := CountVar.AsFloat[ix + min-1]
    else
      result := CountVar.AsFloat[ix + min-1] + (CountVar.AsFloat[ix+ min]-CountVar.AsFloat[ix+ min-1])*w;
  end;

begin
  Idx := ResultDF.NewRecords();
  FConf := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  with ResultDF do
  begin
    // Observation found in category
    Obs                    := EIdx - SIdx + 1;
    N.AsInteger[Idx]       := Obs;

    // Percentiles
    Min.AsFloat[Idx]       := CountVar.AsFloat[SIdx];
    P05.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 5);
    P10.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 10);
    P25.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 25);
    Median.AsFloat[Idx]    := GetPercentile(SIdx, EIdx, 50);
    P75.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 75);
    P90.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 90);
    P95.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 95);
    Max.AsFloat[Idx]       := CountVar.AsFloat[EIdx];

    // Mean and Sum
    lMean                  := ASum / Obs;
    Mean.AsFloat[Idx]      := lMean;
    Sum.AsFloat[Idx]       := ASum;

    lAvgDev := 0;
    lSSqDev := 0;
    lStdErr := 0;
    lSkew   := 0;
    lKurt   := 0;

    FOR i := SIdx TO EIdx DO
    BEGIN
      Val     := CountVar.AsFloat[i] - lMean;
      lAvgDev += abs(Val);
      lSSqDev += (Val * Val);
      lSkew   += (Val * Val) * Val;
      lKurt   += (Val * Val) * (Val * Val);
    END;

    // Watch for data where there is no variation (min = max)
    // because extended precision variables may accumulate rounding errors

    if SameValue(CountVar.AsFloat[EIdx], CountVar.AsFloat[SIdx]) then
      begin
        lSSqDev := 0;
        lSkew   := 0;
        lKurt   := 0;
        lAvgDev := 0;
      end;

    lAvgDev                := lAvgDev / Obs;
    AvgDev.AsFloat[Idx]    := lAvgDev;
    SumSS.AsFloat[Idx]     := lSSqDev;

    // Cannot calculate these statistics with 1 observation or if there is no variation
    if (Obs > 1) then
      begin
        if ((Obs > 2) and (lSSqDev > 0)) then
          begin
            Skew.AsFloat[Idx]   := (Obs * sqrt(Obs - 1) * lSkew) / ((Obs - 2) * sqrt(lSSqDev * lSSqDev * lSSqDev));
          if (Obs > 3) then
            begin
              lKurt             := (Obs * (Obs + 1) * (Obs - 1) * lKurt) / ((Obs - 2) * (Obs - 3) * lSSqDev * lSSqDev);
              Kurt.AsFloat[Idx] := lKurt - (3 * (Obs - 1) * (Obs - 1)) / ((Obs - 2) * (Obs - 3)) // excess kurtosis
            end;
        end;
        lSSqDev               := lSSqDev / (Obs - 1);
        StdVar.AsFloat[Idx]   := lSSqDev;
        lStdDev               := sqrt(lSSqDev);
        StdDev.AsFloat[Idx]   := lStdDev;
        lStdErr               := sqrt(lSSqDev / Obs);
        StdErr.AsFloat[Idx]   := lStdErr;
        lConf                 := 0.005 * (100 - float(FConf));
        lCfiVal               := PTDISTRINV(Obs - 1, lConf) * lStdErr;
        CfiL.AsFloat[Idx]     := lMean - lCfiVal;
        CfiH.AsFloat[Idx]     := lMean + lCfiVal;
      end;
  end;
end;

procedure TMeans.DoCalcAnova(ResultDF: TMeansDatafile; CountVar: TEpiField);
var
  i, j, k, idx, lStrata: Integer;
  Obs: Int64;
  ySum, yTot, val,
  m, z1, z2, zz,
  ASSW, ASST, Tval: EpiFloat;
  z, zbar: array of EpiFloat;
  TPoolVar, TSumLogs, TSumNs: EpiFloat;
begin
  if ResultDF.StratifyVarText = '' then
    begin
      // t-test of mean=0 if only one result
      with ResultDF do
        begin
          Obs  := N.AsInteger[0];
          if (Obs > 1) and (StdErr.AsFloat[0] > 0) then
              Tval := Mean.AsFloat[0] / (StdErr.AsFloat[0]);
        end;

      with ResultDF.AnovaRecord do
        begin
          if (Obs > 1) and (ResultDF.StdErr.AsFloat[0] > 0) then
            begin
              F := Tval;
              PROB := tdist(F,Obs-1);
              Valid := true;
            end
          else
            begin
              Valid := false;
              F := TEpiFloatField.DefaultMissing;
              Prob := TEpiFloatField.DefaultMissing;
            end;
        end;
      Exit;
    end;

  // F-test if more than one stratum
  Obs    := 0;
  ASSW   := 0;

  with ResultDF do
  begin
    lStrata := Size - 1;

    for i := 0 to lStrata do
      ASSW += SumSS.AsFloat[i];

    ASST := TotalSSQ;
    Obs  := TotalObs;
  end;

  with ResultDF.AnovaRecord do
  begin
    SST := ASST;
    SSW := ASSW;
    SSB := SST - SSW;
    DFT := Obs - 1;
    DFB := lStrata;
    DFW := DFT - DFB;
    // validity check on denominators to avoid divison by zero
    Valid := (SSW > 0) and (DFB > 0) and (DFW > 0) and (DFT > 0);
    if (not Valid) then exit;
    MSB := SSB / DFB;
    MSW := SSW / DFW;
    MST := SST / DFT;
    F   := MSB / MSW;

    if (isInfinite(F) or isNaN(F)) then
      begin
        PROB := TEpiFloatField.DefaultMissing;
        F := TEpiFloatField.DefaultMissing;
        BART := TEpiFloatField.DefaultMissing;
        PBART := TEpiFloatField.DefaultMissing;
        W := TEpiFloatField.DefaultMissing;
        PW := TEpiFloatField.DefaultMissing;
        exit; // Skip remaining statistics
      end
    else
      PROB := fdist(F, DFB, DFW);
  end;

  k := lStrata + 1; // number of strata; lStrata fills in for k-1

  // Levene's test of homogeneity of variances
  // ref: https://www.itl.nist.gov/div898/handbook/eda/section3/eda35a.htm
  // This is the 'medians' version of Levene's test
  // Calculate Zi
  with ResultDF do begin
    setLength(zbar, k);
    setLength(z, Obs);
    idx := 0;
    ytot := 0;
    for i := 0 to lStrata do begin
      ysum := 0;
      m := Median.AsFloat[i];
      for j := 0 to N.AsInteger[i] - 1 do begin
        z[idx] := abs(CountVar.AsFloat[idx] - m);
        ysum += z[idx];
        inc(idx);
      end;
      ytot += ysum;
      if (N.AsInteger[i] > 0) then
        zbar[i] := ysum / N.AsFloat[i]
      else
        FExecutor.Error('Levene test error - n=1 in stratum ' + i.ToString);
    end;
    zz := ytot / float(obs);
    // numerator sum
    z1 := 0;
    // denominator sum
    z2 := 0;
    idx := 0;
    for i := 0 to lStrata do begin
      val := zbar[i] - zz;
      z1 += N.AsFloat[i] * val * val;
      m := zbar[i];
      for j := 0 to N.AsInteger[i] -1 do
      begin
        val := z[idx] - m;
        z2 += val * val;
        inc(idx);
      end;
    end;
    z1 := float(obs - k) * z1;
    z2 := float(lStrata) * z2;
    if (z2 > 0) then
      with AnovaRecord do
      begin
        W := (float(obs - k) * z1) / (float(lStrata) * z2);
        pW := fdist(W, lStrata, obs - k);
      end;
  end; // with ResultDF

  // Bartlett's test is sensitive to non-normality of the data
  // ref: Engineering Statistics Handbook 1.3.5.7
  //      https://www.itl.nist.gov/div898/handbook/eda/section3/eda357.htm
  TPoolVar := 0;
  TSumLogs := 0;
  TSumNs   := 0;

  with ResultDF do
    begin
      with AnovaRecord do
        begin
          BART := TEpiFloatField.DefaultMissing;
          PBART := TEpiFloatField.DefaultMissing;
        end;
      for i := 0 to lStrata do
        begin
          if (N.AsInteger[i] <= 1) then
            exit      // skip Bartlett test if any stratum has a single observation
          else begin
            TSumLogs += (N.AsInteger[i] - 1) * ln(StdVar.AsFloat[i]); // (N[i]-1) * var[i]
            TSumNs   += (1/(N.AsInteger[i] - 1));                     // 1 / (N[i]-1)
            TPoolVar += (N.AsInteger[i] - 1) * StdVar.AsFloat[i]      // (N[i]-1) * var[i]
          end;
        end;
      TPoolVar := TPoolVar / (Obs - k);                  // sum / (N-k)

    with AnovaRecord do
      begin
        BART  := ((Obs - k) * ln(TPoolVar) - TSumLogs) / (1 + (TSumNs - 1/(Obs - k)) / (3*(lStrata)));
        PBART := ChiPValue(BART , lStrata)
      end;
    end;
end;

function TMeans.DoCalcMeans(InputDF: TEpiDataFile; const CountVarName,
  StratifyVarName: UTF8String): TMeansDatafile;
var
  CountVar, CategVar: TEpiField;
  SortList: TEpiFields;
  StartIdx, i, EndIdx, Obs: Integer;
  Sum, TotalSum, Val, SSQ: EpiFloat;
begin
  Result := TMeansDatafile.Create(nil, 0);

  CountVar := InputDF.Fields.FieldByName[CountVarName];

  if (StratifyVarName <> '') then
    begin
        // stratified data
        // Algorithm:
        // * sort according to (Category, Values)
        // * run through datafile
        // * collect StartIdx, EndIndex, Value, Sum
        // * whenever a change in category record values and change
        CategVar := InputDF.Fields.FieldByName[StratifyVarName];

        SortList := TEpiFields.Create(nil);
        SortList.AddItem(CategVar);
        SortList.AddItem(CountVar);
        InputDF.SortRecords(SortList);
        SortList.Free;

        StartIdx := 0;
        Sum := CountVar.AsFloat[0];
        TotalSum := 0;
        for i := 1 to InputDF.Size - 1 do
          begin
            EndIdx := i - 1;

            if CategVar.Compare(i - 1, i) <> 0 then
              begin
                // New category, so fill descriptors
                FillDescriptor(Result, CountVar, StartIdx, EndIdx, Sum);

                // Category name
                Result.Category.AsString[Result.Size - 1] := CategVar.GetValueLabel(StartIdx, FValuelabelOutput);

                // keep running sum for full data set
                TotalSum += Sum;

                // Reset values
                StartIdx := i;
                Sum      := CountVar.AsFloat[i];
              end
            else
              Sum        += CountVar.AsFloat[i];
          end;

      // Final stratum
      FillDescriptor(Result, CountVar, StartIdx, InputDF.Size -1, Sum);
      Result.Category.AsString[Result.Size - 1] := CategVar.GetValueLabel(StartIdx, FValuelabelOutput);
      TotalSum += Sum;
      Result.FStratifyVarText := CategVar.GetVariableLabel(FVariableLabelOutput);
    end
  else
    begin
      // no 'by' option - just sort data by value and save sum of data
//      CategVar := InputDF.NewField(ftInteger);      // not necessary
      SortList := TEpiFields.Create(nil);
      SortList.AddItem(CountVar);
      InputDF.SortRecords(SortList);
      SortList.Free;

      TotalSum := 0;
      for i := 0 to InputDF.Size -1 do
        TotalSum += CountVar.AsFloat[i];

      FillDescriptor(Result, CountVar, 0, InputDF.Size - 1, TotalSum);

      // Category name     ** Not necessary **
//      Result.Category.AsString[Result.Size - 1] := CategVar.GetValueLabel(0, FValuelabelOutput);
    end;

  Result.FCountVarText    := CountVar.GetVariableLabel(FVariableLabelOutput);

  Obs := InputDF.Size;
  SSQ := 0;
  for i := 0 to Obs - 1 do
    begin
      Val := CountVar.AsFloat[i] - (TotalSum / Obs);
      SSQ += (Val * Val);
    end;

  Result.FTotalSum  := TotalSum;
  Result.FTotalObs  := InputDF.Size;
  Result.FTotalSSQ  := SSQ;

  DoCalcAnova(Result, CountVar);
end;

procedure TMeans.DoResultVariables(ResultDF: TMeansDatafile);
var
  CatV, ObsV, SumV, MeanV, SvV, SdV, SerrV, CfilV, CfihV, SkewV, KurtV,
  MinV, P05V, P10V, P25V, MedV, P75V, P90V, P95V, MaxV: TCustomExecutorDataVariable;
  Sz, i: Integer;
  Prefix: String;
begin
  Sz := ResultDF.Size;
  Prefix := '$means_';
  with FExecutor do
  begin
    if Sz = 1 then
      begin
        CatV  := AddResultConst(Prefix + 'category', ftString);
        ObsV  := AddResultConst(Prefix + 'obs',      ftInteger);
        SumV  := AddResultConst(Prefix + 'sum',      ftFloat);
        MeanV := AddResultConst(Prefix + 'mean',     ftFloat);
        MinV  := AddResultConst(Prefix + 'min',      ftFloat);
        P05V  := AddResultConst(Prefix + 'p05',      ftFloat);
        P10V  := AddResultConst(Prefix + 'p10',      ftFloat);
        P25V  := AddResultConst(Prefix + 'p25',      ftFloat);
        MedV  := AddResultConst(Prefix + 'median',   ftFloat);
        P75V  := AddResultConst(Prefix + 'p75',      ftFloat);
        P90V  := AddResultConst(Prefix + 'p90',      ftFloat);
        P95V  := AddResultConst(Prefix + 'p95',      ftFloat);
        MaxV  := AddResultConst(Prefix + 'max',      ftFloat);
        SvV   := AddResultConst(Prefix + 'variance', ftFloat);
        SdV   := AddResultConst(Prefix + 'sd',       ftFloat);
        SerrV := AddResultConst(Prefix + 'stderr',   ftFloat);
        CfilV := AddResultConst(Prefix + 'cfil',     ftFloat);
        CfihV := AddResultConst(Prefix + 'cfih',     ftFloat);
        SkewV := AddResultConst(Prefix + 'skew',     ftFloat);
        KurtV := AddResultConst(Prefix + 'kurt',     ftFloat);
        AddResultConst(Prefix + 'catvar', ftString).AsStringVector[0]  := '';
      end
    else
      begin
        CatV  := AddResultVector(Prefix + 'category', ftString, Sz);
        ObsV  := AddResultVector(Prefix + 'obs',      ftInteger, Sz);
        SumV  := AddResultVector(Prefix + 'sum',      ftFloat, Sz);
        MeanV := AddResultVector(Prefix + 'mean',     ftFloat, Sz);
        MinV  := AddResultVector(Prefix + 'min',      ftFloat, Sz);
        P05V  := AddResultVector(Prefix + 'p05',      ftFloat, Sz);
        P10V  := AddResultVector(Prefix + 'p10',      ftFloat, Sz);
        P25V  := AddResultVector(Prefix + 'p25',      ftFloat, Sz);
        MedV  := AddResultVector(Prefix + 'median',   ftFloat, Sz);
        P75V  := AddResultVector(Prefix + 'p75',      ftFloat, Sz);
        P90V  := AddResultVector(Prefix + 'p90',      ftFloat, Sz);
        P95V  := AddResultVector(Prefix + 'p95',      ftFloat, Sz);
        MaxV  := AddResultVector(Prefix + 'max',      ftFloat, Sz);
        SvV   := AddResultVector(Prefix + 'sv',       ftFloat, Sz);
        SdV   := AddResultVector(Prefix + 'sd',       ftFloat, Sz);
        SerrV := AddResultVector(Prefix + 'stderr',   ftFloat, Sz);
        CfilV := AddResultVector(Prefix + 'cfil',     ftFloat, Sz);
        CfihV := AddResultVector(Prefix + 'cfih',     ftFloat, Sz);
        SkewV := AddResultVector(Prefix + 'skew',     ftFloat, Sz);
        KurtV := AddResultVector(Prefix + 'kurt',     ftFloat, Sz);

        AddResultConst(Prefix + 'catvar', ftString).AsStringVector[0]  := ResultDF.FStratifyVarText;
     end;

    AddResultConst(Prefix + 'size',  ftInteger).AsIntegerVector[0] := Sz;
//    AddResultConst(Prefix + 'var',  ftString).AsStringVector[0]    := ResultDF.CountVarText;
 end;

  for i := 0 to ResultDF.Size - 1 do
  with ResultDF do
    begin
      CatV.AsStringVector[i]  := Category.AsString[i];
      ObsV.AsIntegerVector[i] := N.AsInteger[i];
      SumV.AsFloatVector[i]   := Sum.AsFloat[i];
      MeanV.AsFloatVector[i]  := Mean.AsFloat[i];
      MinV.AsFloatVector[i]   := Min.AsFloat[i];
      P05V.AsFloatVector[i]   := P05.AsFloat[i];
      P10V.AsFloatVector[i]   := P10.AsFloat[i];
      P25V.AsFloatVector[i]   := P25.AsFloat[i];
      MedV.AsFloatVector[i]   := Median.AsFloat[i];
      P75V.AsFloatVector[i]   := P75.AsFloat[i];
      P90V.AsFloatVector[i]   := P90.AsFloat[i];
      P95V.AsFloatVector[i]   := P95.AsFloat[i];
      MaxV.AsFloatVector[i]   := Max.AsFloat[i];

      SvV.AsFloatVector[i]    := StdVar.AsFloat[i];
      SdV.AsFloatVector[i]    := StdDev.AsFloat[i];
      SerrV.AsFloatVector[i]  := StdErr.AsFloat[i];
      CfilV.AsFloatVector[i]  := CfiL.AsFloat[i];
      CfihV.AsFloatVector[i]  := CfiH.AsFloat[i];
      SkewV.AsFloatVector[i]  := Skew.AsFloat[i];
      KurtV.AsFloatVector[i]  := Kurt.AsFloat[i];

    end;

  With ResultDF.AnovaRecord do
    if ResultDF.Size > 1 then
      begin
        FExecutor.AddResultConst(Prefix + 'DFB',   ftInteger).AsIntegerVector[0] := DFB;
        FExecutor.AddResultConst(Prefix + 'SSB',   ftFloat).AsFloatVector[0]     := SSB;
        FExecutor.AddResultConst(Prefix + 'MSB',   ftFloat).AsFloatVector[0]     := MSB;
        FExecutor.AddResultConst(Prefix + 'F',     ftFloat).AsFloatVector[0]     := F;
        FExecutor.AddResultConst(Prefix + 'PROB',  ftFloat).AsFloatVector[0]     := PROB;
        FExecutor.AddResultConst(Prefix + 'DFW',   ftInteger).AsIntegerVector[0] := DFW;
        FExecutor.AddResultConst(Prefix + 'SSW',   ftFloat).AsFloatVector[0]     := SSW;
        FExecutor.AddResultConst(Prefix + 'MSW',   ftFloat).AsFloatVector[0]     := MSW;
        FExecutor.AddResultConst(Prefix + 'BART',  ftFloat).AsFloatVector[0]     := BART;
        FExecutor.AddResultConst(Prefix + 'pBART', ftFloat).AsFloatVector[0]     := PBART;
        FExecutor.AddResultConst(Prefix + 'Levene', ftFloat).AsFloatVector[0]     := W;
        FExecutor.AddResultConst(Prefix + 'pW', ftFloat).AsFloatVector[0]     := pW;
      end
    else
      begin
        FExecutor.AddResultConst(Prefix + 'T',     ftFloat).AsFloatVector[0]     := F;
        FExecutor.AddResultConst(Prefix + 'PROB',  ftfloat).AsFloatVector[0]     := PROB;
      end;
end;

procedure TMeans.DoOutputMeans(ResultDF: TMeansDatafile);
var
  T: TOutputTable;
  SmallNumFmt, StatFmt: String;
  Sz, Offset, i, Idx: Integer;
//  maxVforDisplay: Extended;

  function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;
  begin
    if (val = TEpiFloatField.DefaultMissing) then
      Result := TEpiStringField.DefaultMissing
    else
      Result := Format(fmt, [val]);
  end;

begin
  T := FOutputCreator.AddTable;
  T.Header.Text := ResultDF.CountVarText;
  SmallNumFmt := '%8.2F';
  Sz := ResultDF.Size;

  if (ResultDF.StratifyVarText = '') then
    begin
      Offset := 0;
      T.ColCount := 11;
      T.RowCount := 5;
//      maxVforDisplay := ResultDF.Max.AsFloat[0];
    end
  else
    begin

      Offset     := 1;
      T.ColCount := 12;
      T.RowCount := 3 + (Sz * 2);
//      maxVforDisplay := ResultDF.Max.AsFloat[Sz - 1];
    end;

  // Column headers  (first section)
  if Offset > 0 then T.Cell[0,0].Text := ResultDF.StratifyVarText;

  T.Cell[0 + Offset, 0].Text := 'Obs';
  T.Cell[1 + Offset, 0].Text := 'Sum';
  T.Cell[2 + Offset, 0].Text := 'Mean';
  T.Cell[3 + Offset, 0].Text := 'Variance';
  T.Cell[4 + Offset, 0].Text := 'Std. Dev.';
  T.Cell[5 + Offset, 0].Text := '( ' + IntToStr(FConf) + '% CI';
  T.Cell[6 + Offset, 0].Text := 'mean )';
  T.Cell[7 + Offset, 0].Text := 'Std. Err.';
  T.Cell[8 + Offset, 0].Text := 'Skewness';
  T.Cell[9 + Offset, 0].Text := 'Kurtosis';
  T.SetRowAlignment(0, taRightJustify);
  T.SetRowBoxBorder(0);

  StatFmt := '%8.' + IntToStr(FDecimals) + 'F';

  // if option !dx was not specified and max value < 10, then show 2 decimal places instead of the default
  {if (maxVforDisplay < 10.0) and
     (not (ST.HasOption('d' + IntToStr(FDecimals))))
  then
    StatFmt := SmallNumFmt;   }

  with ResultDF do
    begin
      for i := 0 to Sz - 1 do
        begin
          if Offset > 0 then T.Cell[0, i + 1].Text := Category.AsString[i];
          T.Cell[0 + Offset, i + 1].Text := N.AsString[i];
          T.Cell[1 + Offset, i + 1].Text := Format(StatFmt, [Sum.AsFloat[i]]);
          T.Cell[2 + Offset, i + 1].Text := Format(StatFmt, [Mean.AsFloat[i]]);

          T.Cell[3 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, StdVar.AsFloat[i]);
          T.Cell[4 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, StdDev.AsFloat[i]);
          T.Cell[5 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, CfiL.AsFloat[i]);
          T.Cell[6 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, CfiH.AsFloat[i]);
          T.Cell[7 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, StdErr.AsFloat[i]);
          T.Cell[8 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, Skew.AsFloat[i]);
          T.Cell[9 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, Kurt.AsFloat[i]);
          T.SetRowAlignment(i + 1, taRightJustify);
          // Need to set this after the entire row has been set to right justify
          if Offset > 0 then T.Cell[0, i + 1].Alignment := taLeftJustify;
        end;
    end;

  Idx := Sz + 1;

  if Offset > 0 then
    T.Cell[0, Idx].Text := ResultDF.StratifyVarText;
  T.Cell[0 + Offset, Idx].Text := 'Min';
  T.Cell[1 + Offset, Idx].Text := 'p05';
  T.Cell[2 + Offset, Idx].Text := 'p10';
  T.Cell[3 + Offset, Idx].Text := 'p25';
  T.Cell[4 + Offset, Idx].Text := 'Median';
  T.Cell[5 + Offset, Idx].Text := 'p75';
  T.Cell[6 + Offset, Idx].Text := 'p90';
  T.Cell[7 + Offset, Idx].Text := 'p95';
  T.Cell[8 + Offset, Idx].Text := 'Max';
  T.SetRowAlignment(Idx, taRightJustify);
  T.SetRowBoxBorder(Idx);

  Inc(Idx);

  with ResultDF do
  begin
    for i := 0 to Sz - 1 do
      begin
        if Offset > 0 then T.Cell[0, Idx + i].Text := Category.AsString[i];
        T.Cell[0 + Offset, Idx + i].Text := Format(StatFmt, [Min.AsFloat[i]]);
        T.Cell[1 + Offset, Idx + i].Text := Format(StatFmt, [P05.AsFloat[i]]);
        T.Cell[2 + Offset, Idx + i].Text := Format(StatFmt, [P10.AsFloat[i]]);
        T.Cell[3 + Offset, Idx + i].Text := Format(StatFmt, [P25.AsFloat[i]]);
        T.Cell[4 + Offset, Idx + i].Text := Format(StatFmt, [Median.AsFloat[i]]);
        T.Cell[5 + Offset, Idx + i].Text := Format(StatFmt, [P75.AsFloat[i]]);
        T.Cell[6 + Offset, Idx + i].Text := Format(StatFmt, [P90.AsFloat[i]]);
        T.Cell[7 + Offset, Idx + i].Text := Format(StatFmt, [P95.AsFloat[i]]);
        T.Cell[8 + Offset, Idx + i].Text := Format(StatFmt, [Max.AsFloat[i]]);
        T.SetRowAlignment(Idx + i, taRightJustify);
        // Need to set this after the entire row has been set to right justify
        if Offset > 0 then T.Cell[0, Idx + i].Alignment := taLeftJustify;
      end;
  end;
end;

constructor TMeans.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
end;

destructor TMeans.Destroy;
begin
  inherited Destroy;
end;

procedure TMeans.DoOutputAnova(AnovaRec: TAnovaRecord);
var
  T: TOutputTable;
  B: TOutputTable;
  L: TOutputTable;
begin
  T := FOutputCreator.AddTable;
  T.Header.Text := 'Analysis of Variance';
  if (not AnovaRec.Valid) then
//  if (isNaN(AnovaRec.F) or isInfinite(AnovaRec.F) or (AnovaRec.F = TEpiFloatField.DefaultMissing)) then
    begin
      T.ColCount := 1;
      T.RowCount := 1;
      T.Cell[0,0].Text := 'Cannot do ANOVA with these values';
      exit
    end;
  T.ColCount := 6;
  T.RowCount := 4;
  T.Cell[0,0].Text := 'Source';
  T.Cell[1,0].Text := 'DF';
  T.Cell[2,0].Text := 'SS';
  T.Cell[3,0].Text := 'MS';
  T.Cell[4,0].Text := 'F';
  T.Cell[5,0].Text := 'p Value';
  T.SetRowAlignment(0, taCenter);

  B := FOutputCreator.AddTable;
  B.Header.Text    := 'Bartlett''s Test of homogeneity of variances';
  B.ColCount       := 3;
  B.RowCount       := 2;
  B.Cell[0,0].Text := 'DF';
  B.Cell[1,0].Text := 'Chi-square';
  B.Cell[2,0].Text := 'p Value';
  B.SetRowAlignment(0, taCenter);

  L := FOutputCreator.AddTable;
  L.Header.Text    := 'Levene''s Test of homogeneity of variances';
  L.ColCount       := 3;
  L.RowCount       := 2;
  L.Cell[0,0].Text := 'DF';
  L.Cell[1,0].Text := 'W';
  L.Cell[2,0].Text := 'p Value';
  L.SetRowAlignment(0, taCenter);

  with AnovaRec do
  begin
    T.Cell[0,1].Text := 'Between';
    T.Cell[1,1].Text := IntToStr(DFB);
    T.Cell[2,1].Text := Format('%8.2f', [SSB]);
    T.Cell[3,1].Text := Format('%8.2f', [MSB]);
    T.Cell[4,1].Text := Format('%8.2f', [F]);
    T.Cell[5,1].Text := Format('%8.3f', [PROB]);
    if ((PROB < 0.0) or (PROB > 1.0)) then
      T.Cell[5,1].Text := ' (Invalid p=' + T.Cell[5,1].Text + ' - Program error)';

    T.Cell[0,2].Text := 'Within';
    T.Cell[1,2].Text := IntToStr(DFW);
    T.Cell[2,2].Text := Format('%8.2f', [SSW]);
    T.Cell[3,2].Text := Format('%8.2f', [MSW]);

    T.Cell[0,3].Text := 'Total';
    T.Cell[1,3].Text := IntToStr(DFT);
    T.Cell[2,3].Text := Format('%8.2f', [SST]);
    T.Cell[3,3].Text := Format('%8.2f', [MST]);

    if (BART <> TEpiFloatField.DefaultMissing) then
      begin
        B.Cell[0,1].Text := IntToStr(DFB);
        B.Cell[1,1].Text := Format('%8.2f', [BART]);
        B.Cell[2,1].Text := Format('%8.3f', [PBART]);
      end
    else
      B.Footer.Text:= 'Cannot calculate Bartlett''s test with a stratum of 1';

    L.Cell[0,1].Text := IntToStr(DFB) + ', ' + IntToStr(DFW);
    L.Cell[1,1].Text := Format('%8.2f', [W]);
    L.Cell[2,1].Text := Format('%8.3f', [PW]);

  end;
end;

procedure TMeans.DoOutputTTest(AnovaRec: TAnovaRecord);
var
  T: TOutputTable;
begin
  T := FOutputCreator.AddTable;
  T.Header.Text := 'T-Test of Ho: mean=zero';
//  if (isNaN(AnovaRec.F) or isInfinite(AnovaRec.F) or (not AnovaRec.Valid)) then
  if (not AnovaRec.Valid) then
    begin
      T.ColCount := 1;
      T.RowCount := 1;
      T.Cell[0,0].Text := 'Cannot do a t-test with these values';
      exit
    end;
  T.ColCount       := 3;
  T.RowCount       := 2;
  T.Cell[0,0].Text := '';
  T.Cell[1,0].Text := 't';
  T.Cell[2,0].Text := 'p Value';
  T.SetRowAlignment(0, taCenter);

  with AnovaRec do
  begin
    T.Cell[1,1].Text := Format('%8.2f', [F]);
    T.Cell[2,1].Text := Format('%8.3f', [PROB]);
  end;
end;

procedure TMeans.ExecMeans(ST: TMeansCommand);
var
  Variables: TStrings;
  Variable: String;
  ResultDF: TMeansDatafile;
  CountVarName, StratifyVarName: UTF8String;
  Opt: TOption;
  HasBy: Boolean;
  DataFile: TEpiDataFile;
begin
// reset result variables;
  FExecutor.ClearResults('$means');

  FDecimals := DecimalFromOption(ST.Options);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  Variables := ST.VariableList.GetIdentsAsList;
  HasBy := false;
  StratifyVarName := '';
  ST.ExecResult := csrFailed;

  // check for more than one !by
  try
    for Opt in ST.Options do
    begin
     if (Opt.Ident = 'by') then
       if (HasBy) then
         begin
           FExecutor.Error('Can only stratify by one variable; !by:=' + Opt.Expr.AsIdent + ' is invalid');
           exit;
         end
      else
        begin
          HasBy := true;
          if (Variables[0] = Opt.Expr.AsIdent) then
            begin
              FExecutor.Error('Cannot stratify by the same variable: ' + Opt.expr.AsIdent);
              Exit;
            end;
          StratifyVarName := Opt.Expr.AsIdent;
          Variables.Add(StratifyVarName);
         end;
      end;

    DataFile := FExecutor.PrepareDatafile(Variables, Variables);

    if DataFile.Size = 0 then
      begin
        FExecutor.Error('No data!');
        DataFile.Free;
        Exit;
      end;

    CountVarName := ST.VariableList[0].Ident;

    ResultDF := DoCalcMeans(DataFile, CountVarName, StratifyVarName);
    DoResultVariables(ResultDF);

    if (not ST.HasOption('q')) then
      begin
        DoOutputMeans(ResultDF);

        if (ST.HasOption('t')) then
          if (ResultDF.StratifyVarText = '') then
            DoOutputTTest(ResultDF.AnovaRecord)
          else
            DoOutputAnova(ResultDF.AnovaRecord);
      end;
    ST.ExecResult := csrSuccess;
    ResultDF.Free;
    DataFile.Free;
  finally
    Variables.Free;
end;

end;

function TMeans.CalcMeans(DataFile: TEpiDataFile; const CountVarName,
  StratifyVarName: UTF8String; ValueLabelOutput: TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType): TMeansDatafile;
begin
  FValuelabelOutput := ValueLabelOutput;
  FVariableLabelOutput := VariableLabelOutput;

  Result := DoCalcMeans(DataFile, CountVarName, StratifyVarName);
end;

end.
