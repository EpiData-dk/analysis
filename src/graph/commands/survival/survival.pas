unit survival;

{$codepage UTF-8}
{$mode objfpc}{$H+}

{
  References
  Most methods:
  Machin D, Gardner MJ. in Altman DG, Nacgub D, Bryant TN, Gardner MJ. Statistics with Confidence. BMJ Books. 2000.
  Adjusted survival with intervals:
  Hosmer DW, Lemeshow S. Applied survival Analysis. SCRIBO.
}
interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  tables_types, tables,
  executor, result_variables, epifields_helper, ana_globals,
  outputcreator, options_utils, chart_options,
  TAGraph, TASeries, TATypes, TASources, Graphics, FPCanvas,
  chartcommandresult, chartcommand, chartfactory, chartconfiguration, charttitles;

resourcestring
  sAllSubjects = 'All subjects';
  sAt = 'at';
  sBy = 'by';
  sConfIntervalAbbr = 'CI';
  sLowerLimitAbbr = 'LL';
  sMax = 'Maximum';
  sMedian = 'Median';
  sMin = 'Minimum';
  sNoData = 'No data';
  sOptionInvalid = 'Option invalid';
  sOutcome = 'Outcome';
  sRatio = 'Ratio';
  sReferenceAbbr = 'Ref.';
  sStratify = 'Stratify';
  sTime = 'Time';
  sTotal = 'Total';
  sUpperLimitAbbr = 'UL';
  sVariable = 'Variable';
  sWeight = 'Weight';
  sWeighted = 'Weighted';
  sSurAtRisk = 'At risk';
  sSurAtRisk1 = 'at';
  sSurAtRisk2 = 'risk';
  sSurCommand = 'Survival';
  sSurFailure = 'Failure';
  sSurFailures = 'Failures';
  sSurFollowup = 'Followup';
  sSurGraphHead = 'KM Plot for outcome';
  sSurHazard = 'Hazard';
  sSurHazardRatio = 'Hazard Ratio';
  sSurHeader = 'Kaplan-Meier Survival Analysis';
  sSurHeader1 = 'Life Tables';
  sSurHeader2 = 'Summary';
  sSurIgnoreAdj = '!adj ignored when !int is not specified';
  sSurIntErr = 'Invalid interval';
  sSurIntNotSort = 'Intervals must be in ascending order';
  sSurLogRankChi = 'Log-Rank Chi-square';
  sSurNoRec =  'No records with ';
  sSurPlotHead = 'KM Plot for';
  sSurPlotInfo = 'Plot points for KM plots were saved to the clipboard';
  sSurTempVar = '_survivaldays';
  sSurTempVarQuestion = 'Days from ';
  sSurTimeVarNeg = 'The time variable, $1, has negative values';
type

  { TSurvival }

  TSurvival = class(TInterfacedObject, IChartCommand)
  private
    FExecutor:      TExecutor;
    FOutputCreator: TOutputCreator;
    FChartFactory:  IChartFactory;

    FDecimals,
    FConf:                Integer;
    FConfz:               EpiFloat;
    FValueLabelOutput:    TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
  // table of outcome by time
    FSurvivalTable:       TTwoWayTables;
    FStrata,
    FIntervals:           Integer;
    FRefStratum:          Integer;
    FRefValue:            EpiString;
    FintFlag:             Boolean;
    FintervalString:      String;
    FAdjFlag:             Boolean;
  // survival table results
    FInterval:            Array of Array of UTF8String;
    FTime,
    FAtRisk,
    FFail:                Array of Array of Integer;
    FSurvival,
    FLowCI,
    FHighCI:              Array of Array of EpiFloat;
    FMaxRow:              Array of Integer;
  // summary results
    FTotal,
    FTotalFailures,
    FMinTime,
    FMaxTime,
    FSumTime:             Array of Integer;
    FMedian:              Array of UTF8String;
    FLRChi,
    FLRP:                 EpiFloat;
    FHazRatio,
    FHRCILo,
    FHRCIHi:              array of EpiFloat;
  // labels
    FFailOutcomeValue,
    FFailOutcomeText,
    FWeightVarName,
    FStratVarName,
    FOutcomeVarLabel,
    FTimeVarLabel:        UTF8String;
    FStratLabels:         array of UTF8String;
  // KM Plots
    FChart:               TChart;
    FChartConfiguration:  IChartConfiguration;
    FTitles:              IChartTitleConfiguration;
    FCIType:              Integer;
    FColors:              TColorMap;
  // plot points for one KM graph series
    FPlotT,
    FPlotS,
    FPlotLL,
    FPlotUL:              array of EpiFloat;
//    FPlotLost:            array of Integer;
  // string to hold clipboard contents (plot points)
    FCBPlot:              UTF8String;

  protected
    function CalcTime(ST: TCustomGraphCommand; VarList: TStrings): TStringList;
    procedure DoCalcSurvival(InputDF: TEpiDataFile; Variables: TStrings;
                StratVariable: TStringList; ST: TCustomVariableCommand);
    procedure DoResultVariables(ST: TCustomVariableCommand); virtual;
    procedure DoOneResult(Stratum: Integer; Name: UTF8String); virtual;
    procedure DoOutputSurvival(ST:TCustomVariableCommand); virtual;
    procedure DoOutputSummary(ST:TCustomVariableCommand); virtual;
    procedure DoLogRank(); virtual;
    procedure DoCalcPlotPoints(Stratum: Integer); virtual;
    procedure DoAddPlotPoints(t, s, ll, ul: EpiFloat);
    function  SurvivalGraphData(data: array of EpiFloat;
                lineTitle: UTF8String; lineStyle: TFPPenStyle;
                lineWidth: Integer; lineColor: TColor;
                inLegend: Boolean = true): TLineSeries;
    function  SurvivalGraphData(data: array of EpiFloat;
                ll: array of EpiFloat; ul: array of EpiFloat;
                lineTitle: UTF8String; lineStyle: TFPPenStyle;
                lineWidth: Integer; lineColor: TColor;
                inLegend: Boolean = true): TLineSeries;
    function  SurvivalBand(bandTitle: UTF8String;
                bandPattern: TFPBrushStyle; bandColor: TColor;
                inLegend: Boolean = true): TAreaSeries;
    procedure DoAddCBPlotPoints(Stratum: Integer); virtual;
    procedure DoOutputCBPlotPoints(); virtual;
    procedure DoInitializeKMPlot(); virtual;
    procedure DoAddGraphSeries(Stratum: Integer);
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function  Execute(Command: TCustomGraphCommand): IChartCommandResult;

  end;

implementation

uses
  generalutils, Math, statfunctions, strutils, Clipbrd,
  ast_types, forms, graphformfactory, TACustomSource;

{ TSurvival }

procedure TSurvival.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory  := ChartFactory;
  FExecutor      := Executor;
  FOutputCreator := OutputCreator;
end;

function TSurvival.CalcTime(ST: TCustomGraphCommand; VarList: TStrings): TStringList;
var
  days: TEpiField;
  t1, t2: TEpiField;
  replaceDate: Integer;
  opt: TOption;
  addMissingTime: Boolean;
  DF: TEpiDataFile;
  i: Integer;
begin
  result             := TStringList.Create;
  days               := FExecutor.DataFile.NewField(ftInteger);
  days.Name          := sSurTempVar;
  days.Question.Text := sSurTempVarQuestion + FExecutor.DataFile.Fields.FieldByName[VarList[1]].GetVariableLabel(FVariableLabelOutput);
  DF                 := FExecutor.PrepareDatafile(VarList, nil);
  t1                 := DF.Fields.FieldByName[VarList.Strings[1]];
  t2                 := DF.Fields.FieldByName[VarList.Strings[2]];
  addMissingTime     := false;

  if (ST.HasOption('exit',opt)) then
    begin
      // date specified to replace missing
      addMissingTime := true;
      replaceDate    := opt.Expr.AsInteger;
    end
  else if (ST.HasOption('mt')) then
    begin
      // replacement date is largest dates
      addMissingTime := true;
      replaceDate    := 0;
      for i := 0 to DF.Size - 1 do
        if (not t2.IsMissing[i]) then
          if (replaceDate < t2.AsInteger[i]) then
            replaceDate := t2.AsInteger[i];
    end;
  // replace missing date2
  for i := 0 to DF.Size - 1 do
    begin
      if (t1.IsMissing[i]) then
        days.IsMissing[i] := true
      else
        if (t2.IsMissing[i]) then
          if (addMissingTime) then
            days.AsInteger[i] := replaceDate - t1.AsInteger[i]
          else
            days.IsMissing[i] := true
        else
          days.AsInteger[i] := t2.AsInteger[i] - t1.AsInteger[i];
    end;
  result.Add(Varlist.Strings[0]);
  result.Add(sSurTempVar);
  // modify VarList as well
  VarList.Delete(2);
  VarList.Delete(1);
  VarList.Add(sSurTempVar);
end;

procedure TSurvival.DoCalcSurvival(InputDF: TEpiDataFile;
  Variables: TStrings; StratVariable: TStringList;
  ST: TCustomVariableCommand);

var
  i,
  time,
  FailIx,
  Row,
  Col,
  Stratum,
  NAtRisk,
  NEffective,
  iLost,
  iFail,
  iTotal:     Integer;
  iLo,
  iHi,
  iTime:      array of Integer;
  iLabel:     array of UTF8String;
  sIntervals: array of String;
  nIntervals: array of Integer;
  S, SE:      EpiFloat;
//  SumF:       EpiFloat;  // for Greenwood CI only
  nAdj:       EpiFloat;
  GetMedian:  Boolean;
  T:          TTables;
  ATable:     TTwowayTable;
  NilStats:   TTableStatistics;
  NilRefMap:  TEpiReferenceMap;

  function accumTotals(i1, i2: Integer): Integer;
  var
    ix: Integer;
  begin
    if (i1 < 0) then exit(0);
    if (not FintFlag) then exit(ATable.RowTotal[i1]);
    result := 0;
    for ix := i1 to i2 do
      result += ATable.RowTotal[ix];
  end;

  function accumFailures(i1, i2: Integer): Integer;
  var
    ix: Integer;
  begin
    if (i1 < 0) then exit(0);
    if (not FintFlag) then exit(ATable.Counts[Failix, i1]);
    result := 0;
    for ix := i1 to i2 do
      result += ATable.Counts[Failix, ix];
  end;

begin
// Use TABLES to get counts of outcomes by time for each stratum
  T := TTables.Create(FExecutor, FOutputCreator);
  FSurvivalTable  := T.CalcTables(InputDF, Variables,
    StratVariable, FWeightVarName, ST.Options, NilRefMap, NilStats);

  with FSurvivalTable.UnstratifiedTable do
  begin
    // find index of outcome = fail
    FailIx := -1;
    for Col := 0 to ColCount - 1 do
      if (ColVariable.AsString[Col] = FFailOutcomeValue) then
        begin
          FailIx := Col;
          Continue;
        end;
    if (FailIx < 0) then
      begin
        FExecutor.Error(sSurNoRec + Variables[0] + ' = ' + FFailOutcomeValue);
        exit;
      end;
    FFailOutcomeText  := ColVariable.GetValueLabelFormatted(FailIx,FValueLabelOutput);

    // validate that time value has only positive integers
    if (RowVariable.AsInteger[0] < 0) then
      begin
        FExecutor.Error(StringReplace(sSurTimeVarNeg, '$1', Variables[1], [rfIgnoreCase]));
        exit;
      end;
  end;

  FStrata    := FSurvivalTable.Count;

  if (FintFlag) then
    begin
      sIntervals := SplitString(FIntervalString, ',');  // no check on contents at this point
      FIntervals := Length(sIntervals) + 1;       // one more interval than numbers specified
      // validate intervals
      try
        SetLength(nIntervals, Length(sIntervals));
        time := -1;
        for i := 0 to Length(sIntervals) - 1 do
          begin
            nIntervals[i] := strToInt(sIntervals[i]);
            // specified intervals must be monotonic increasing
            if (nIntervals[i] <= time) then
              begin
                FExecutor.Error(sSurIntNotSort);
                exit;
              end
            else
              time := nIntervals[i];
          end;
      except
        On E : EConvertError do
          begin
            FExecutor.Error(sSurIntErr + ': ' + sIntervals[i]);
            exit;
          end;
      end;
      // if first interval value is zero, remove it
      if (nIntervals[0] = 0) then
        begin
          FIntervals := FIntervals - 1;
          for i := 1 to FIntervals do
            begin
              nIntervals[i-1] := nIntervals[i];
              sIntervals[i-1] := sIntervals[i];
            end;
        end;
    end
  else
    FIntervals := FSurvivalTable.UnstratifiedTable.RowCount;

  SetLength(iLo,    FIntervals);
  SetLength(iHi,    FIntervals);
  SetLength(iTime,  FIntervals);
  SetLength(iLabel, FIntervals);

  if (FintFlag) then
    begin
      // create times and labels for intervals
      iTime[FIntervals - 1] := nIntervals[FIntervals - 2];
      iLabel[FIntervals - 1] := sIntervals[FIntervals - 2] + '+';
      for i := FIntervals - 2 downto 1 do
        begin
          iTime[i] := nIntervals[i-1];
          iLabel[i] := sIntervals[i-1] + '-' + (iTime[i+1] - 1).ToString;
        end;
      iTime[0] := 0;
      iLabel[0] := '0-' + (iTime[1] - 1).ToString;
      // iLo and iHi map table rows to intervals
      i := -1;
      iLo[0] := 0;
      for Row := 0 to FSurvivalTable.UnstratifiedTable.RowCount - 1 do
        begin
          time := FSurvivalTable.UnstratifiedTable.RowVariable.AsInteger[Row];
          if (i < FIntervals) then
            begin
              if (time < iTime[i+1]) then
                begin
                  if (i >= 0) then iHi[i] := row;
                end
              else
                begin
                  i += 1;
                  iLo[i] := row;
                  iHi[i] := row;
                end;
            end
          else
            begin
              iHi[FIntervals - 1] := Row;
            end;
        end;
    end
  else  // no intervals
    begin
      for i := 0 to FIntervals - 1 do
        begin
          iTime[i] := FSurvivalTable.UnstratifiedTable.RowVariable.AsInteger[i];
          iLabel[i] := FSurvivalTable.UnstratifiedTable.RowVariable.GetValueLabel(i, FValuelabelOutput);
          iLo[i] := i;
          iHi[i] := i;
        end;
    end;

  SetLength(FStratlabels, FStrata);
  SetLength(FInterval,    FStrata + 1, FIntervals);
  SetLength(FTime,        FStrata + 1, FIntervals);
  SetLength(FAtRisk,      FStrata + 1, FIntervals);
  SetLength(FFail,        FStrata + 1, FIntervals);
  SetLength(FSurvival,    FStrata + 1, FIntervals);
  SetLength(FLowCI,       FStrata + 1, FIntervals);
  SetLength(FHighCI,      FStrata + 1, FIntervals);
  SetLength(FMedian,      Fstrata + 1);
  SetLength(FMaxRow,      FStrata + 1);
  SetLength(FTotal,       FStrata + 1);
  SetLength(FTotalFailures, FStrata + 1);
  SetLength(FMinTime,     FStrata + 1);
  SetLength(FMaxTime,     FStrata + 1);
  SetLength(FSumTime,     FStrata + 1);
  SetLength(FHazRatio,    FStrata + 1);
  SetLength(FHRCILo,      FStrata + 1);
  SetLength(FHRCIHi,      FStrata + 1);

  // set up confidence interval
  FConf  := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  FConfz := PNORMALINV((Float(100 - FConf) / 200.0));

  // unstratified table (0) first

  for Stratum := 0 to FStrata do
    begin
      if (Stratum = 0) then
        begin
          ATable := FSurvivalTable.UnstratifiedTable;
        end
      else
        begin
          ATable := FSurvivalTable.Tables[Stratum - 1];
          FStratLabels[Stratum - 1] := FSurvivalTable.StratifyVariables.Field[0].GetValueLabel(Stratum - 1);
          // identify the reference stratum by value
          if (FRefStratum < 0) then
              if (FSurvivalTable.StratifyVariables.Field[0].AsString[Stratum-1] = FRefValue) then
                FRefStratum := Stratum;
        end;

       with ATable do
        begin
          // get summary data for this stratum
          FTotal[Stratum]   := Total;
          FTotalFailures[Stratum] := ColTotal[FailIx];
          for Row := 0 to RowCount - 1 do
            if (RowTotal[Row] > 0) then
              begin
                FMinTime[Stratum] := RowVariable.AsInteger[Row];
                break;
              end;
          for Row := RowCount - 1 downto 0 do
            if (RowTotal[Row] > 0) then
              begin
                FMaxTime[Stratum] := RowVariable.AsInteger[Row];
                break;
              end;
        end;

      FSumTime[Stratum]  := 0;
      NAtRisk   := ATable.Total;
      NEffective:= NAtRisk;
      S         := 1.0;
//      SumF      := 0.0;
      GetMedian := true;
      // here, Row is the index for saved values (output table rows with NAtRisk>0)
      Row       := 0;

      for i := 0 to FIntervals - 1 do
        begin
          if (NAtRisk > 0) then
            begin
              iTotal := accumTotals(iLo[i], iHi[i]);
              FSumTime[Stratum] += iTotal * iTime[i];
              if (iTotal > 0) then
                FMaxRow[Stratum] := Row;
              iFail := accumFailures(iLo[i], iHi[i]);
              iLost := iTotal - iFail;
              if (FAdjFlag) then
                nAdj := Float(NAtRisk) - (Float(iLost) / 2)
              else
                nAdj := Float(NAtRisk);
              S := S * (nAdj - Float(iFail)) / nAdj;
// Greennwood's method
//              SumF     += Float(iFail) / Float(NAtRisk*(NAtRisk - iFail));
//              SE       := S * SQRT(SumF);
// Altman's method
              SE       := sqrt(S*(1-S)/Float(NEffective));
              FInterval[Stratum, Row] := iLabel[i];
              FTime    [Stratum, Row] := iTime[i];
              FAtRisk  [Stratum, Row] := NAtRisk;
              FFail    [Stratum, Row] := iFail;
              FSurvival[Stratum, Row] := S;
              FLowCI   [Stratum, Row] := max(S - SE * FConfz , 0);
              FHighCI  [Stratum, Row] := min(S + SE * FConfz , 1);
              if (GetMedian) then
                if (S <= 0.5) then
                  begin
                    FMedian[Stratum] := FInterval[Stratum, Row];
                    GetMedian        := false;
                  end;
              Row += 1;
            end;

          NAtRisk  := NAtRisk - iTotal;
          NEffective := NEffective - iLost
        end;

      // was median found? It won't be if final survival > 0.5
      if (GetMedian) then
        FMedian[Stratum] := '>' + FInterval[Stratum, FMaxRow[Stratum]];
    end;
  ST.ExecResult := csrSuccess;

  T.Free;
end;

procedure TSurvival.DoLogRank();
var
  SumExp:  EpiFloat;
  SumFail: Integer;
  r,
  d,
  v,
  e1,
  e2,
  x,
  y:       EpiFloat;
  o1,
  o2,
  i,
  Stratum: Integer;
begin
  if (FStrata = 0) then exit;  // Error - should not happen
// log rank test
  FLRChi := 0;
  for Stratum := 1 to FStrata do
    begin
      SumExp := 0;
      SumFail := 0;
      for i := 0 to FIntervals - 1 do
        if (FFail[0,i] > 0) then
          begin
            SumExp  += float(FFail[0, i] * FAtRisk[Stratum, i])/float(FAtRisk[0, i]);
            SumFail += FFail[Stratum, i];
          end;

      d := float(SumFail) - SumExp;
      FLRChi += (d * d) / SumExp;
    end;
  FLRP := ChiPValue(FLRChi , FStrata - 1);

  // was refStratum set?
  if (FRefStratum < 0) then
    FRefStratum := 1;

  for Stratum := 1 to FStrata do
    if (Stratum <> FRefStratum) then
      begin
        r := 0;
        d := 0;
        v    := 0;
        e1   := 0;
        e2   := 0;
        o1   := 0;
        o2   := 0;
        for i := 0 to FIntervals - 1 do
          if (FFail[0, i] > 0) then
            begin
              o1 += FFail[FRefStratum, i];
              o2 += FFail[Stratum, i];
              d  := FFail[0, i];
              r  := FAtRisk[0, i];
              x  := float(d) / float(r);
              e1 += float(FAtRisk[FRefStratum, i]) * x;
              e2 += float(FAtRisk[Stratum, i]) * x;
              v  += float(FAtRisk[FRefStratum, i] * FAtRisk[Stratum, i] * d * (r - d)) /
                    float(r * r * (r - 1));
            end;
        FHazRatio[Stratum] := exp((float(o2) - e2)/v);
        x := (o2-e2)/v;
        y := FConfz/sqrt(v);
        FHRCILo[Stratum] := exp(x - y);
        FHRCIHi[Stratum] := exp(x + y);
      end
    else
      begin
        // provide legal / printable values for reference stratum
        FHazRatio[Stratum] := 1;
        FHRCILo[Stratum]   := 0;
        FHRCIHi[Stratum]   := 0;
      end;
end;

procedure TSurvival.DoOneResult(Stratum: Integer; Name: UTF8String);
var
  i, rSz: Integer;
  rVt, rVs: TExecVarVector;
begin
  // get size of result vectors
  // since we cannot change the vector size after creating it
  rSz := 0;
  for i := 0 to high(FFail[Stratum]) do
    if FFail[Stratum, i] > 0 then
      rSz += 1;
  // fill result vectors
  with FExecutor do
  begin
    rVt := AddResultVector('$survival_time_' + Name, ftFloat, rSz);
    rVs := AddResultVector('$survival_estimate_' + Name, ftFloat, rSz);
    AddResultConst('survival_subjects_' + Name , ftInteger).AsIntegerVector[0] := FTotal[Stratum];
    AddResultConst('survival_failures_' + Name, ftInteger).AsIntegerVector[0] := FTotalFailures[Stratum];
    AddResultConst('survival_minTime_' + Name, ftInteger).AsIntegerVector[0] := FMinTime[Stratum];
    AddResultConst('survival_maxTime_' + Name, ftInteger).AsIntegerVector[0] := FMaxTime[Stratum];
    AddResultConst('survival_sumTime_' + Name, ftInteger).AsIntegerVector[0] := FSumTime[Stratum];
    rSz := 0;
    for i := 0 to high(FSurvival[Stratum]) do
      if FFail[Stratum, i] > 0 then
        begin
          rVt.AsFloatVector[rSz] := FTime[Stratum, i];
          rVs.AsFloatVector[rSz] := FSurvival[Stratum, i];
          rSz += 1;
        end;
  end;
end;

procedure TSurvival.DoResultVariables(ST:TCustomVariableCommand);
var
  Stratum: Integer;
  sNames,
  sHazR, sHRCILo, sHRCIHi: TExecVarVector;
begin
  FExecutor.ClearResults('survival');
  DoOneResult(0, 'all');

  if FStrata > 0 then
  begin
    sNames  := FExecutor.AddResultVector('$survival_strata', ftString, FStrata);
    sHazR   := FExecutor.AddResultVector('$survival_HR', ftFloat, FStrata);
    sHRCILo := FExecutor.AddResultVector('$survival_HRCILo', ftFloat, FStrata);
    sHRCIHi := FExecutor.AddResultVector('$survival_HRCIHi', ftFloat, FStrata);
    if (ST.HasOption('t')) then
    begin
      FExecutor.AddResultConst('$survival_chi2',   ftFloat).AsFloatVector[0] := FLRChi;
      FExecutor.AddResultConst('$survival_chip',   ftFloat).AsFloatVector[0] := FLRP;
      for Stratum := 1 to FStrata do
        begin
          sHazR.AsFloatVector[Stratum-1] := FHazRatio[Stratum];
          sHRCILo.AsFloatVector[Stratum-1] := FHRCILo[Stratum];
          sHRCIHi.AsFloatVector[Stratum-1] := FHRCIHi[Stratum];
        end;
    end;
    for Stratum := 1 to FStrata do
      begin
        sNames.AsStringVector[Stratum-1] := FStratLabels[Stratum-1];
        DoOneResult(Stratum, (Stratum).ToString);
      end;
  end;
end;
procedure TSurvival.DoOutputSurvival(ST:TCustomVariableCommand);
var
  T: TOutputTable;
  StatFmt: String;
  Sz, i: Integer;
  Stratum, FirstStratum, LastStratum: Integer;

  function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;
  begin
    if (val = TEpiFloatField.DefaultMissing) then
      Result := TEpiStringField.DefaultMissing
    else
      Result := Format(fmt, [val]);
  end;

begin
  FirstStratum := 0;
  LastStratum  := FStrata;
  if (ST.HasOption('nou')) then
    begin
      if (ST.HasOption('nos')) then exit;
      FirstStratum := 1;
    end;
  if (ST.HasOption('nos')) then
    LastStratum := 0;

  StatFmt := '%' + IntToStr(3 + FDecimals) + '.' + IntToStr(FDecimals) + 'F';
  // Column headers
  for Stratum := FirstStratum to LastStratum do
    begin
      T             := FOutputCreator.AddTable;
      T.ColCount    := 5;
      T.RowCount    := 1;
      if (Stratum = 0) then
        T.Header.Text := sSurHeader + ' - ' +sSurHeader1 + LineEnding + sAllSubjects
      else
        T.Header.Text := sSurHeader + ' - ' +sSurHeader1 + LineEnding + FStratVarName + ' = ' + FStratLabels[Stratum-1];
      T.Cell[0, 0].Text := sSurFollowup      + LineEnding + FTimeVarLabel;
      T.Cell[1, 0].Text := '#' + sSurAtRisk1 + LineEnding + sSurAtRisk2;
      T.Cell[2, 0].Text := FOutcomeVarLabel  + LineEnding + FFailOutcomeText;
      T.Cell[3, 0].Text :=                     LineEnding + sSurCommand;;
      T.Cell[4, 0].Text :=                     LineEnding + '(' + IntToStr(FConf) + '% ' + sConfIntervalAbbr + ')';

  T.SetRowAlignment(0, taRightJustify);
  Sz := T.RowCount;
  // show stratum results with failures only
  for i := 0 to Length(FFail[0]) - 1 do
    if (FFail[0, i] > 0) then
    begin
      if (FFail[Stratum, i] > 0) then
        begin
          T.RowCount := Sz + 1;
          T.Cell[0, Sz].Text := FInterval[0, i];
          T.Cell[1, Sz].Text := IntToStr(FAtRisk[Stratum, i]);
          T.Cell[2, Sz].Text := IntToStr(FFail[Stratum, i]);
          T.Cell[3, Sz].Text := Format(StatFmt, [FSurvival[Stratum, i]]);
          T.Cell[4, Sz].Text := FormatCI(FLowCI[Stratum, i], FHighCI[Stratum, i], 0, ST.Options);
          T.SetRowAlignment(Sz, taRightJustify);
          Sz +=1;
        end;
    end;
  T.SetRowBorders(0, [cbTop, cbBottom]);
//  T.SetRowBorders(0, [cbBottom]);
  end;
end;

procedure TSurvival.DoOutputSummary(ST:TCustomVariableCommand);
var
  T: TOutputTable;
  StatFmt: String;
  i:  Integer;
  line1: array of UTF8String = ('',sTotal,sTotal,sMin,sMax,sTotal,sMedian,sSurHazard,'');
  line2: array of UTF8String = ('',sSurAtRisk,sSurFailures,sTime,sTime,sTime,sSurCommand,sRatio,'');

  procedure outputStratumResults(s, r: Integer);
  begin
    if (s > 0) then
      T.Cell[0, r].Text := FStratLabels[s-1]
    else
        T.Cell[0, r].Text := sAllSubjects;
    T.Cell[1, r].Text := FTotal[s].ToString;
    T.Cell[2, r].Text := FTotalFailures[s].ToString;
    T.Cell[3, r].Text := FMinTime[s].ToString;
    T.Cell[4, r].Text := FMaxTime[s].ToString;
    T.Cell[5, r].Text := FSumTime[s].ToString;
    T.Cell[6, r].Text := FMedian[s];
    if (s = 0) or (not ST.HasOption('t')) then exit;
    if (s = FRefStratum) then
      T.Cell[7, r].Text := sReferenceAbbr
    else
      begin
        T.Cell[7, r].Text := Format(StatFmt, [FHazRatio[s]]);
        T.Cell[8, r].Text := '(' + Format(StatFmt, [FHRCILo[s]]) + ',' + Format(StatFmt, [FHRCIHi[s]]) + ')';
      end;
  end;

begin
  StatFmt := '%' + IntToStr(3 + FDecimals) + '.' + IntToStr(FDecimals) + 'F';
  line2[high(line2)] :=  '(' + IntToStr(FConf) + '% ' + sConfIntervalAbbr + ')';
  T                 := FOutputCreator.AddTable;
  T.Header.Text     := sSurHeader + ' - ' + sSurHeader2;
  T.ColCount        := 9;
  T.RowCount        := FStrata + 2;
  // table headings
  if (FStrata > 0) then
    T.Cell[0, 0].Text := sBy + LineEnding + FStratVarName;
  for i := 1 to high(line1) do
      if (i < 7) or
         ((FStrata > 0) and (ST.HasOption('t'))) then
        T.Cell[i, 0].Text := line1[i] + LineEnding +line2[i];
  // table contents
  for i := 1 to FStrata do
    outputStratumResults(i, i);
  outputStratumResults(0, FStrata + 1);

  if ((FStrata > 0) and (ST.HasOption('t'))) then
    T.Footer.Text := sSurLogRankChi + ' =' + Format(StatFmt, [FLRChi]) + ' ' + FormatP(FLRP, true);
  T.SetRowBorders(0, [cbTop, cbBottom]);
end;

procedure TSurvival.DoCalcPlotPoints(Stratum: Integer);
var
  i: Integer;

begin
  FPlotT  := [0.0];
  FPlotS  := [1.0];
  FPlotLL := [1.0];
  FPlotUL := [1.0];
//  FPlotLost := [];

  for i := low(FAtRisk[Stratum]) to high(FAtRisk[Stratum]) do
    if (FFail[Stratum,i] > 0) then
      DoAddPlotPoints(float(FTime[Stratum, i]), FSurvival[Stratum, i], FLowCI[Stratum, i], FHighCI[Stratum, i]);
// add final plot point based on maximum time for this stratum
  i := FMaxRow[Stratum];
  DoAddPlotPoints(float(FTime[Stratum, i]), FSurvival[Stratum, i], FLowCI[Stratum, i], FHighCI[Stratum, i]);
end;

procedure TSurvival.DoAddPlotPoints(t, s, ll, ul: EpiFloat);

begin
  FPlotT  := concat(FPlotT,  [t ]);
  FPlotS  := concat(FPlotS,  [s ]);
  FPlotLL := concat(FPlotLL, [ll]);
  FPlotUL := concat(FPlotUL, [ul]);
// Do we want to mark times where subjects were censored?
// If so, we need to put single plot points everwhere there was a loss (need time and survival estimate pairs)
// But these points get added separately!! NOT as part of adding points where there were losses.
//  FPlotLost := concat(FPlotLost, [t, FPlotS [l]]);
end;

procedure TSurvival.DoInitializeKMPlot();
var
  aTitle, aFoot: UTF8String;
begin
// create graph and set titles, etc
  aTitle := sSurGraphHead + ' ' + FOutcomeVarLabel;
  if (FStrata > 0) then
    aTitle += ' ' + sBy + ' ' + FStratVarName;
  aFoot := sSurFailure + ': ' + FOutcomeVarLabel + ' = ' + FFailOutcomeText;
  if (FWeightVarName <> '') then
    aFoot += ' (' + sWeighted + ' ' + sBy + ' ' + FWeightVarName + ')';
  FChart := FChartFactory.NewChart();
  FChartConfiguration := FChartFactory.NewChartConfiguration();
  FTitles := FChartConfiguration.GetTitleConfiguration()
    .SetTitle(aTitle)
    .SetFootnote(aFoot)
    .SetXAxisTitle(sTime + ': ' + FTimeVarLabel)
    .SetYAxisTitle(sSurCommand);
  with FChart do
  begin
    LeftAxis.Range.Min    := 0;
    LeftAxis.Range.Max    := 1;
    LeftAxis.Range.UseMin := true;
    LeftAxis.Range.UseMax := true;
    LeftAxis.Grid.Style   := psClear;
    BottomAxis.Grid.Style := psClear;
    Legend.Visible        := true;
    Legend.UseSidebar     := true;
    Legend.Frame.Visible  := false;
    Frame.Visible         := true;
    Margins.Left          := 0;
    Margins.Bottom        := 0;
  end;
end;

procedure TSurvival.DoAddGraphSeries(Stratum: Integer);

var
  aText:      UTF8String;
  aColor:     TColor;
  aPattern:   TFPBrushStyle;
  sPattern:   array of TFPBrushStyle = (bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
begin
  if (Stratum > 0) then
    begin
      aText  := FStratVarname + '=' + FStratLabels[Stratum - 1];
      aColor   := FColors[min(Stratum-1,8)];
      aPattern := sPattern[min(Stratum-1,3)];
    end
  else
  begin
    aText  := sSurCommand;
    if (FCIType = 0) then
      aText := '| ' + aText + LineEnding + '| ' + IntToStr(FConf) + '% ' + sConfIntervalAbbr;
    aColor   := FColors[0];
    aPattern := sPattern[0];
  end;
  case FCIType of
    0:     // default - verticle bars
      begin
        FChart.AddSeries(SurvivalGraphData(FPlotS, FPlotLL, FPlotUL, aText, psSolid, 2, aColor));
      end;
    1 :    // line
      begin
        // the order of adding series matters because of a bug in TAChart
        // that does not always respect psDot, depending on the series index
        FChart.AddSeries(SurvivalGraphData(FPlotUL, '', psDot, 2, aColor, false));
        FChart.AddSeries(SurvivalGraphData(FPlotS,  aText, psSolid, 2, aColor));
        FChart.AddSeries(SurvivalGraphData(FPlotLL, IntToStr(FConf) + '% ' + sConfIntervalAbbr, psDot, 2, aColor));
      end;
    2 :    // band
      begin
        FChart.AddSeries(SurvivalGraphData(FPlotS, aText, psSolid, 2, aColor));
        FChart.AddSeries(SurvivalBand(IntToStr(FConf) + '% ' + sConfIntervalAbbr, aPattern, aColor));
      end;
    else   // none
      FChart.AddSeries(SurvivalGraphData(FPlotS, aText, psSolid, 2, aColor));
  end;
end;

function TSurvival.SurvivalGraphData(data: array of EpiFloat;
          lineTitle: UTF8String; lineStyle: TFPPenStyle;
          lineWidth: Integer; lineColor: TColor; inLegend: Boolean = true): TLineSeries;
var
  plotSource: TListChartSource;
  i:          Integer;
begin
  plotSource := TListChartSource.Create(FChart);
  for i := 0 to high(FPlotT) do
    plotSource.Add(FPlotT[i], data[i]);
    plotSource.Sorted := true;
  result := TLineSeries.Create(FChart);
  with result do
  begin
    LinePen.Color := lineColor;
    LinePen.Style := lineStyle;
    LinePen.Width := lineWidth;
    LineType      := ltStepXY;
    Pointer.Style := psNone;
    ShowInLegend  := inLegend;
    ShowPoints    := false;
    Source        := plotSource;
    Title         := lineTitle;
  end;
end;

function  TSurvival.SurvivalGraphData(data: array of EpiFloat;
            ll: array of EpiFloat; ul: array of EpiFloat;
            lineTitle: UTF8String; lineStyle: TFPPenStyle;
            lineWidth: Integer; lineColor: TColor;
            inLegend: Boolean = true): TLineSeries;
var
  plotSource: TListChartSource;
  i:          Integer;
begin
  plotSource := TListChartSource.Create(FChart);
  plotSource.YCount := 3;
  for i := 0 to high(FPlotT) do
    plotSource.AddXYList(FPlotT[i], [data[i], data[i] - ll[i], ul[i] - data[i]] );
  plotsource.YErrorBarData.Kind := ebkChartSource;
  plotsource.YErrorBarData.IndexMinus := 1;
  plotsource.YErrorBarData.IndexPlus  := 2;
  plotSource.Sorted := true;
  result := TLineSeries.Create(FChart);
  with result do
  begin
    LinePen.Color := lineColor;
    LinePen.Style := lineStyle;
    LinePen.Width := lineWidth;
    LineType      := ltStepXY;
    Pointer.Style := psNone;
    ShowInLegend  := inLegend;
    ShowPoints    := false;
    Source        := plotSource;
    Title         := lineTitle;
    YErrorBars.Visible   := true;
    YErrorBars.Width     := 2;
    YErrorBars.Pen.Color := lineColor;
  end;
end;

function TSurvival.SurvivalBand(bandTitle: UTF8String; bandPattern: TFPBrushStyle;
          bandColor: TColor; inLegend: Boolean = true): TAreaSeries;
var
  plotSource: TListChartSource;
  i:          Integer;
begin
  plotSource := TListChartSource.Create(FChart);
  plotSource.YCount := 2;
  for i := 0 to high(FPlotT) do
    begin
      plotSource.AddXYList(FPlotT[i], [FPlotLL[i],FPlotUL[i]-FPlotLL[i]]);
    end;
  result := TAreaSeries.Create(FChart);
  with result do
  begin
    AreaBrush.Color      := bandColor;
    AreaBrush.Style      := bandpattern;
    AreaContourPen.Style := psDot;
    AreaLinesPen.Style   := psClear;
    Banded               := true;
    ConnectType          := ctStepXY;
    ShowInLegend         := inLegend;
    Source               := plotSource;
    Stacked              := true;
    Title                := bandTitle;
    Transparency         := 240;
  end;
end;

procedure TSurvival.DoAddCBPlotPoints(Stratum: Integer);
var
  i: Integer;
  d: UTF8String;
begin
  d := FExecutor.SetOptions.GetValue(ANA_SO_CLIPBOARD_DELIMITER).Value;
  FCBplot += sSurPlotHead + FOutcomeVarLabel + ' ' + sAt + ' ' + sBy + ' ' + FTimeVarLabel + lineending;
  if (Stratum = 0) then
    FCBPlot += sAllSubjects + lineending
  else
    FCBPlot += FStratVarName + ' = ' + FStratLabels[Stratum - 1] + lineending;
  FCBPlot +=  sTime + d + sSurCommand + d + sConfIntervalAbbr + sLowerLimitAbbr + d + sConfIntervalAbbr + sUpperLimitAbbr + lineending;
  for i := 0 to high(FPlotT) do
    FCBPlot += trim(Format('%6.0f', [FPlotT[i]]))  + d +
               trim(Format('%6.3f', [FPlotS[i]]))  + d +
               trim(Format('%6.3f', [FPlotLL[i]])) + d  +
               trim(Format('%6.3f', [FPlotUL[i]])) + lineending;
end;

procedure TSurvival.DoOutputCBPlotPoints();
begin
    Clipboard.AsText:=FCBPlot;
    FOutputCreator.DoInfoShort(sSurPlotInfo);
end;

function TSurvival.Execute(Command: TCustomGraphCommand) : IChartCommandResult;
var
  VarNames:         TStrings;
  AllVariables:     TStrings;
  StratVariable:    TStringList;
  Opt,
  refOpt:           TOption;
  DF:               TEpiDataFile;
  Stratum,
  Stratum1,
  vCount:           Integer;
  showKMPlot:       Boolean;
  intervalArray: array of integer;
  msg:                 UTF8String;
begin
  FVariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FColors              := ChartColorsFromOptions(Command.Options, FExecutor.SetOptions, msg);
  if (msg <> '') then
    begin
      FExecutor.Error(msg);
      exit;
    end;
  FExecutor.ClearResults('$survival');

  VarNames             := Command.VariableList.GetIdentsAsList;
  StratVariable        := TStringList.Create;
  FDecimals            := DecimalFromOption(Command.Options, 3);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FintFlag             := false;
  FAdjFlag             := false;
  showKMPlot           := true;
  // time variables specified?
  vCount := VarNames.Count;
  if (vCount = 3) then
    AllVariables := CalcTime(Command, VarNames) // will replace time variables with days between them
  else
    AllVariables := Command.VariableList.GetIdentsAsList;

  Result := FChartFactory.NewGraphCommandResult(); // always create chart object
  Command.ExecResult := csrFailed; // for statistical command
  FCIType            := 0; // default - error bars
  try
    for Opt in Command.Options do
    case Opt.Ident of

      'o':  // failure outcome value
        begin
          if (FFailOutcomeValue <> '') then
            begin
              FExecutor.Error(sOptionInvalid + ': ' + Opt.Ident + ':=' + Opt.Expr.AsIdent);
              exit;
            end;
          FFailOutcomeValue := Opt.Expr.AsString;
        end;

      'by':
        begin
          if (FStratVarName <> '') then
            begin
              FExecutor.Error(sOptionInvalid + ': ' + Opt.Ident + ':=' + Opt.Expr.AsIdent);
              exit;
            end;
          if ( (VarNames[0] = Opt.Expr.AsIdent) or (VarNames[1] = Opt.Expr.AsIdent) ) then
            begin
              FExecutor.Error(sOptionInvalid + ': ' + Opt.Ident + ':=' + Opt.Expr.AsIdent);
              Exit;
            end;
          FRefStratum := 1;
          if (Command.HasOption('ref',refOpt)) then
            begin
              FRefValue := refOpt.Expr.AsString;
              FRefStratum := -1; // signals need to look for stratum with FRefValue
            end;
          FStratVarName := Opt.Expr.AsIdent;
          StratVariable.Add(FStratVarName);
          AllVariables.AddStrings(FStratVarName);
        end;

      'w':  // weight variable
        begin
          if (FWeightVarName <> '') then
            begin
              FExecutor.Error(sOptionInvalid + ': ' + Opt.Ident + ':=' + Opt.Expr.AsIdent);;
              exit;
            end;
          if ( (VarNames[0] = Opt.Expr.AsIdent) or (VarNames[1] = Opt.Expr.AsIdent) ) then
            begin
              FExecutor.Error(sOptionInvalid + ': ' + Opt.Ident + ':=' + Opt.Expr.AsIdent);
              Exit;
            end;
          FWeightVarName := Opt.Expr.AsIdent;
          AllVariables.Add(FWeightVarName);
        end;

      'i':  // intervals
        begin
          if (FintFlag) then
            begin
              FExecutor.Error(sOptionInvalid + ': ' + Opt.Ident + ':=' + Opt.Expr.AsIdent);
              exit;
            end;
          if (Opt.Expr = nil) then
            FintervalString := FExecutor.SetOptions.GetValue(ANA_SO_LIFETABLE_INTERVAL).Value
          else
            FintervalString := Opt.Expr.AsIdent;
          FintFlag := FintervalString <> '';
        end;

      'adj': // adjust intervals
        begin
          if not (Command.HasOption('i')) then
            FOutputCreator.DoInfoShort(sSurIgnoreAdj)
          else
            FAdjFlag := true;
        end;
    end;

    if (FFailOutcomeValue = '') then
        FFailOutcomeValue := '0';

    DF := FExecutor.PrepareDatafile(AllVariables, AllVariables);

    if DF.Size = 0 then
      begin
        FExecutor.Error(sNoData);
        DF.Free;
        Exit;
      end;

    // save labels for other procedures
    FTimeVarLabel    := DF.Fields.FieldByName[VarNames[1]].GetVariableLabel(FVariableLabelOutput);
    FOutcomeVarLabel := DF.Fields.FieldByName[VarNames[0]].GetVariableLabel(FVariableLabelOutput);

    DoCalcSurvival(DF, VarNames, StratVariable, Command);

    if (Command.ExecResult = csrSuccess) then
      begin

        showKMPlot := not Command.HasOption(['ng','noKMPlot'],Opt);

        if (not Command.HasOption('q')) then
          begin
            if (not Command.HasOption(['nt','notab'], Opt)) then
              DoOutputSurvival(Command);

            if (Command.HasOption(['t','test'], Opt) and (FStrata > 0)) then
              DoLogRank();

            if (not Command.HasOption('ns')) then
              DoOutputSummary(Command);

            if (Command.HasOption(['cin','cinone'],Opt)) then
              FCIType := -1
            else if (Command.HasOption(['cib','ciband'],Opt)) then
              FCIType := 2
            else if (Command.HasOption(['cil','ciline'], Opt)) then
              FCIType := 1
            else
              FCIType := 0;
            if (Command.HasOption('cb')) then
              FCBPlot := '';
            if (showKMPlot) then
              DoInitializeKMPlot;
            Stratum1 := 0;
            if (FStrata > 0) then
              Stratum1 := 1;
            for Stratum := Stratum1 to FStrata do
            begin
              DoCalcPlotPoints(Stratum);
              if (showKMPlot) then
                DoAddGraphSeries(Stratum);
              if (Command.HasOption('cb')) then
                DoAddCBPlotPoints(Stratum);
            end;
            if (showKMPlot) then
              Result.AddChart(FChart, FChartConfiguration);
            if (Command.HasOption('cb')) then
              DoOutputCBPlotPoints();
        end;
        DoResultVariables(Command);
      end;
    DF.Free;
    // if calculated date, then delete the field
    if (vCount = 3) then
      FExecutor.Datafile.Fields.FieldByName[sSurTempVar].Free;
  finally
    StratVariable.Free;
    AllVariables.Free;
  end;

end;

initialization
  RegisterChartCommand(stSurvival, TSurvival);

end.
