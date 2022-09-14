unit survival;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  tables_types, tables,
  executor, result_variables, epifields_helper, ana_globals,
  outputcreator,
  TAGraph, TASeries, TATypes, TASources, Graphics, FPCanvas,
  chartcommandresult, chartcommand, chartfactory, chartconfiguration, charttitles;

resourcestring
  sTotal = 'Total';
  sMin = 'Minumum';
  sMax = 'Maximum';
  sTime = 'Time';
  sMedian = 'Median';
  sBy = 'by';
  sAt = 'at';
  sAllData = 'All data';
  sNoData = 'No data';
  sVariable = 'Variable';
  sWeighted = 'Weighted';
  sConfIntervalAbbr = 'CI';
  sUpperLimitAbbr = 'UL';
  sLowerLimitAbbr = 'LL';
  sNoMoreThanOne =  'Cannot specify more than one';
  sOutcome = 'Outcome';
  sWeight = 'Weight';
  sStratify = 'Stratify';
  //sCannotStratifyBy = 'Cannot stratify by';
  //sCannotWeightBy = 'Cannot weight by';
  sOptionInvalid = 'Option invalid';
  //sIsInvalid = 'is invalid';
  sSurCommand = 'Survival';
  sSurTempVar = '_survivaldays';
  sSurTempVarPrefix = 'Days from ';
  sSurNoRec =  'No records with ';
  sSurTimeVarNegPrefix = 'The time variable,';
  sSurTimeVarNegSuffix = ', has negative values';
  sSurHeader1 = 'Kaplan Meier Survival Analysis - Life Tables';
  sSurHeader2 = 'Kaplan Meier Survival Analysis - Summary';
  sSurGraphHead = 'KM Plot for outcome';
  sSurPlotHead = 'KM Plot for';
  sSurPlotInfo = 'Plot points for KM plots were saved to the clipboard';
  sSurAtRisk = 'At risk';
  sSurAtRisk1 = 'at';
  sSurAtRisk2 = 'risk';
  sSurFailure = 'Failure';
  sSurFailures = 'Failures';
  sSurAllData = 'All Data';
  sSurFollowup = 'Followup';
  sSurLogRankChi = 'Log-Rank Chi-square';
  sSurHazardRatio = 'Hazard Ratio';
type

  { TSurvival }

  TSurvival = class(TInterfacedObject, IChartCommand)
  private
    FExecutor:      TExecutor;
    FOutputCreator: TOutputCreator;
    FChartFactory:  IChartFactory;

    FDecimals,
    FConf:                Integer;
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
    FIntervalCount:       Integer;
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
    FLRP,
    FHazRatio:            EpiFloat;
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
  generalutils, Math, statfunctions, options_utils, strutils, Clipbrd,
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
  missingDate: Integer;
  opt: TOption;
  addMissingTime: Boolean;
  DF: TEpiDataFile;
  i: Integer;
begin
  result := TStringList.Create;
  days := FExecutor.DataFile.NewField(ftInteger);
  days.Name := sSurTempVar;
  days.Question.Text := sSurTempVarPrefix + FExecutor.DataFile.Fields.FieldByName[VarList[1]].GetVariableLabel(FVariableLabelOutput);
  DF := FExecutor.PrepareDatafile(VarList, nil);
  t1 := DF.Fields.FieldByName[VarList.Strings[1]];
  t2 := DF.Fields.FieldByName[VarList.Strings[2]];
  addMissingTime := false;
  if (ST.HasOption('exit',opt)) then
    begin
      missingDate := opt.Expr.AsInteger;
      addMissingTime := true;
    end
  else if (ST.HasOption('mt')) then
    begin
      missingDate := 0;
      addMissingTime := true;
      for i := 0 to DF.Size - 1 do
        if ((missingDate < t2.AsInteger[i]) and
            (not t2.IsMissing[i])) then
          missingDate := t2.AsInteger[i];
    end;
  for i := 0 to DF.Size - 1 do
    begin
      if (t1.IsMissing[i]) then
        days.IsMissing[i] := true
      else
        if (t2.IsMissing[i]) then
          if (addMissingTime) then
            days.AsInteger[i] := missingDate - t1.AsInteger[i]
          else
            days.IsMissing[i] := true
        else
          days.AsInteger[i] := t2.AsInteger[i] - t1.AsInteger[i];
    end;
  result.Add(Varlist.Strings[0]);
  result.Add(sSurTempVar);
  VarList.Delete(2);
  VarList.Delete(1);
  VarList.Add(sSurTempVar);
end;

procedure TSurvival.DoCalcSurvival(InputDF: TEpiDataFile;
  Variables: TStrings; StratVariable: TStringList;
  ST: TCustomVariableCommand);

var
  i, time, FailIx, Row, Col, Stratum, NAtRisk, NEffective: Integer;
  iLost, iFail, iTotal: Integer;
  iLo, iHi, iTime: array of Integer;
  iLabel: array of UTF8String;
  sIntervals: array of String;
  S, SE, SumF, CIMult: EpiFloat;
  T: TTables;
  Statistics: TTableStatistics;
  ASurvivalTable: TTwowayTable;
  TablesRefMap: TEpiReferenceMap;
  GetMedian: Boolean;

  function accumTotals(i1, i2: Integer): Integer;
  var
    ix: Integer;
  begin
    if (i1 < 0) then exit(0);
    if (not FintFlag) then exit(ASurvivalTable.RowTotal[i1]);
    result := 0;
    for ix := i1 to i2 do
      result += ASurvivalTable.RowTotal[ix];
  end;

  function accumFailures(i1, i2: Integer): Integer;
  var
    ix: Integer;
  begin
    if (i1 < 0) then exit(0);
    if (not FintFlag) then exit(ASurvivalTable.Counts[Failix, i1]);
    result := 0;
    for ix := i1 to i2 do
      result += ASurvivalTable.Counts[Failix, ix];
  end;

begin
// Use TABLES to get counts of outcomes by time for each stratum
  T := TTables.Create(FExecutor, FOutputCreator);
  FSurvivalTable  := T.CalcTables(InputDF, Variables,
    StratVariable, FWeightVarName, ST.Options, TablesRefMap, Statistics);

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
        FExecutor.Error(sSurTimeVarNegPrefix + ' ' + Variables[1] + sSurTimeVarNegSuffix);
        exit;
      end;
  end;

  FStrata    := FSurvivalTable.Count;

  if (FintFlag) then
    begin
      sIntervals := SplitString(FIntervalString, ',');  // no check on contents at this point
      FIntervals := Length(sIntervals) + 1;       // one more interval than numbers specified
    end
  else
    FIntervals := FSurvivalTable.UnstratifiedTable.RowCount;

  SetLength(iLo,    FIntervals);
  SetLength(iHi,    FIntervals);
  SetLength(iTime,  FIntervals);
  SetLength(iLabel, FIntervals);

  if (FintFlag) then
    begin
      iTime[FIntervals - 1] := strToInt(SIntervals[FIntervals - 2]);
      iLabel[FIntervals - 1] := SIntervals[FIntervals - 2] + '+';
      for i := FIntervals - 2 downto 1 do
        begin
          iTime[i] := strToInt(SIntervals[i-1]);
          iLabel[i] := SIntervals[i-1] + '-' + (iTime[i] - 1).ToString;
        end;
      iTime[0] := 0;
      iLabel[0] := '0-' + (iTime[1] - 1).ToString;
      // now get iLo and iHi to map intervals to rows
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

  // set up confidence interval
  FConf  := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  CIMult := PNORMALINV((Float(100 - FConf) / 200.0));

  // unstratified table (0) first

  for Stratum := 0 to FStrata do
    begin
      if (Stratum = 0) then
        begin
          ASurvivalTable := FSurvivalTable.UnstratifiedTable;
        end
      else
        begin
          ASurvivalTable := FSurvivalTable.Tables[Stratum - 1];
          FStratLabels[Stratum - 1] := FSurvivalTable.StratifyVariables.Field[0].GetValueLabel(Stratum - 1);
          if (FRefStratum < 0) then
              if (FSurvivalTable.StratifyVariables.Field[Stratum - 1].AsString[0] = FRefValue) then
                FRefStratum := Stratum - 1;
        end;
  // with intervals, need to aggregate atrisk, fail,  lost and then calculate survival, etc

  // Summary variables
      with ASurvivalTable do
        begin
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
      NAtRisk   := ASurvivalTable.Total;
      NEffective:= NAtRisk;
      S         := 1.0;
      SumF      := 0.0;
      GetMedian := true;
      Row       := 0;   // index for saved values (# table rows with NAtRisk>0)

      for i := 0 to FIntervals - 1 do                                                                         // OK
        begin
          if (NAtRisk > 0) then
            begin
              iTotal := accumTotals(iLo[i], iHi[i]);
              FSumTime[Stratum] += iTotal * iTime[i]; // iTime is 'time of record' for this interval
//              FSumTime[Stratum] += ASurvivalTable.RowTotal[i] * ASurvivalTable.RowVariable.AsInteger[i];      //**
              if (iTotal > 0) then //(ASurvivalTable.RowTotal[i] > 0) then                                                        //**
                FMaxRow[Stratum] := Row;                   // track last row with data
              iFail := accumFailures(iLo[i], iHi[i]); //ASurvivalTable.Counts[FailIx, i];                                                   //**
              S        := S * Float((NAtRisk - iFail)) / Float(NAtRisk);
// Greennwood's method
//              SumF     += Float(Failures) / Float(NAtRisk*(NAtRisk - Failures));
//              SE       := S * SQRT(SumF);
// Altman's method
              SE       := sqrt(S*(1-S)/Float(NEffective));
              FInterval[Stratum, Row] := iLabel[i]; //ASurvivalTable.RowVariable.GetValueLabel(i, FValuelabelOutput);      //**
              FTime    [Stratum, Row] := iTime[i];  //ASurvivalTable.RowVariable.AsInteger[i];                             //**
              FAtRisk  [Stratum, Row] := NAtRisk;
              FFail    [Stratum, Row] := iFail;
              iLost                   := iTotal - iFail; //ASurvivalTable.RowTotal[i] - Failures;                               //**
              FSurvival[Stratum, Row] := S;
              FLowCI   [Stratum, Row] := max(S - SE * CIMult , 0);
              FHighCI  [Stratum, Row] := min(S + SE * CIMult , 1);
              if (GetMedian) then
                if (S <= 0.5) then
                  begin
                    FMedian[Stratum] := FInterval[Stratum, Row];
                    GetMedian        := false;
                  end;
              Row += 1;
            end;

          NAtRisk  := NAtRisk - iTotal; //ASurvivalTable.RowTotal[i];
          NEffective := NEffective - iLost;                                                                         //**
        end;

      // was median found? It won't be if final survival > 0.5
      if (GetMedian) then
        FMedian[Stratum] := '>' + FInterval[Stratum, FMaxRow[Stratum]]; //ASurvivalTable.RowCount - 1];
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
  x:       EpiFloat;
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
// hazard ratio for two strata only
  if (FStrata <> 2) then exit;
  // find refStratum if necessary
  if (FRefValue <> '') then
    begin

    end;
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
        o1 += FFail[1, i];
        o2 += FFail[2, i];
        d  := FFail[0, i];
        r  := FAtRisk[0, i];
        x  := float(d) / float(r);
        e1 += float(FAtRisk[1, i]) * x;
        e2 += float(FAtRisk[2, i]) * x;
        v  += float(FAtRisk[1, i] * FAtRisk[2, i] * d * (r - d)) /
              float(r * r * (r - 1));
      end;
  FHazRatio := exp((float(o2) - e2)/v);
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
  sNames: TExecVarVector;
begin
  FExecutor.ClearResults('survival');
  DoOneResult(0, 'all');

  if FStrata > 0 then
  begin
    sNames := FExecutor.AddResultVector('$survival_strata', ftString, FStrata);
    if (ST.HasOption('t') and (FStrata > 0)) then
    begin
      FExecutor.AddResultConst('$survival_' + 'chi2', ftFloat).AsFloatVector[0] := FLRChi;
      FExecutor.AddResultConst('$survival_' + 'chip', ftFloat).AsFloatVector[0] := FLRP;
      FExecutor.AddResultConst('$survival_' + 'HR', ftFloat).AsFloatVector[0] := FHazRatio;
    end;
    for Stratum := 1 to FStrata do
      begin
        sNames.AsStringVector[Stratum-1] := FStratLabels[Stratum-1];
        DoOneResult(Stratum, FStratLabels[Stratum-1]);
      end;
  end;
end;
procedure TSurvival.DoOutputSurvival(ST:TCustomVariableCommand);
var
  T: TOutputTable;
  StatFmt: String;
  Sz, Offset, i: Integer;
  Stratum, FirstStratum, LastStratum, ColPerStratum: Integer;

  function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;
  begin
    if (val = TEpiFloatField.DefaultMissing) then
      Result := TEpiStringField.DefaultMissing
    else
      Result := Format(fmt, [val]);
  end;

begin
  T             := FOutputCreator.AddTable;
  T.Header.Text := sSurHeader1;
  // show only rows with failures
  Sz := 0;
  for i := 0 to Length(FFail[0]) - 1 do
    if (FFail[0,1] > 0) then
      Sz += 1;
  T.RowCount    := 0;
  ColPerStratum := 4;

  // set up output table size based on strata and options

  if (ST.HasOption('nou')) then
    begin
      if (ST.HasOption('nos')) then exit;
      T.RowCount   := 3;
      T.ColCount   := 1;
      FirstStratum := 1;
      Offset       := 1;
    end
  else
    begin
      T.RowCount       := 3;
      T.ColCount       := 1 + ColPerStratum;
      T.Cell[1,0].Text := sSurAllData;
      FirstStratum     := 0;
      Offset           := 1 + ColPerStratum;
    end;

  if (ST.HasOption('nos')) then
    LastStratum := 0
  else
    begin
      T.ColCount := T.ColCount + ColPerStratum * FStrata;
      for i := 0 to FStrata -1 do
        begin
          T.Cell[Offset + ColPerStratum * i,     0].Text := FStratVarName + ' = ';
          T.Cell[Offset + ColPerStratum * i + 1, 0].Text := FStratLabels[i];
        end;
      LastStratum := FStrata;
    end;

  T.Cell[0, 1].Text := sSurFollowup;
  T.Cell[0, 2].Text := FTimeVarLabel;
  Offset            := 1;

  // Column headers
  for Stratum := FirstStratum to LastStratum do
    begin
      T.Cell[    Offset, 1].Text := '#' + sSurAtRisk1;
      T.Cell[    Offset, 2].Text := sSurAtRisk2;
      T.Cell[1 + Offset, 1].Text := FOutcomeVarLabel;
      T.Cell[1 + Offset, 2].Text := FFailOutcomeText;
      T.Cell[2 + Offset, 1].Text := ' ';
      T.Cell[2 + Offset, 2].Text := sSurCommand;
      T.Cell[3 + Offset, 1].Text := ' ';
      T.Cell[3 + Offset, 2].Text := '(' + IntToStr(FConf) + '% ' + sConfIntervalAbbr + ')';
      Offset += ColPerStratum;
    end;
  T.SetRowAlignment(1, taRightJustify);
  StatFmt := '%' + IntToStr(3 + FDecimals) + '.' + IntToStr(FDecimals) + 'F';

  // show stratum results with failures only
  Sz := 3;
  for i := 0 to Length(FFail[0]) - 1 do
    if (FFail[0, i] > 0) then
    begin
      T.RowCount := Sz + 1;
      T.Cell[0, Sz].Text := FInterval[0, i];
      Offset := 1;
      for Stratum := FirstStratum to LastStratum do
        begin
          if (FFail[Stratum, i] > 0) then
            begin
              T.Cell[Offset    , Sz].Text := IntToStr(FAtRisk[Stratum, i]);
              T.Cell[Offset + 1, Sz].Text := IntToStr(FFail[Stratum, i]);
              T.Cell[Offset + 2, Sz].Text := Format(StatFmt, [FSurvival[Stratum, i]]);
              T.Cell[Offset + 3, Sz].Text := FormatCI(FLowCI[Stratum, i], FHighCI[Stratum, i], 0, ST.Options);
              T.SetRowAlignment(Sz, taRightJustify);
            end;
          Offset += ColPerStratum;
        end;
      Sz +=1;
    end;

  Offset := 0;
{  for Stratum := FirstStratum to LastStratum do   // causes double-spaced output!
    begin
      T.SetColBorders(Offset, [cbRight]);
      Offset += ColPerStratum;
    ; }
  T.SetRowBorders(1, [cbTop]);
  T.SetRowBorders(2, [cbBottom]);
end;

procedure TSurvival.DoOutputSummary(ST:TCustomVariableCommand);
var
  T: TOutputTable;
  StatFmt: String;
  i:  Integer;
  line1: array of UTF8String = ('',sTotal,sTotal,sMin,sMax,sTotal,sMedian);
  line2: array of UTF8String = ('',sSurAtRisk,sSurFailures,sTime,sTime,sTime,sSurCommand);

  procedure outputStratumResults(s, r: Integer);
  begin
    if (s > 0) then
      T.Cell[0, r].Text := FStratLabels[s-1]
    else
        T.Cell[0, r].Text := sSurAllData;
    T.Cell[1, r].Text := FTotal[s].ToString;
    T.Cell[2, r].Text := FTotalFailures[s].ToString;
    T.Cell[3, r].Text := FMinTime[s].ToString;
    T.Cell[4, r].Text := FMaxTime[s].ToString;
    T.Cell[5, r].Text := FSumTime[s].ToString;
    T.Cell[6, r].Text := FMedian[s];
  end;

begin
  StatFmt := '%' + IntToStr(3 + FDecimals) + '.' + IntToStr(FDecimals) + 'F';

  T                 := FOutputCreator.AddTable;
  T.Header.Text     := sSurHeader2;
  T.ColCount        := 7;
  T.RowCount        := FStrata + 3;
  for i := 0 to high(line1) do
    begin
      T.Cell[i, 0].Text := line1[i];
      T.Cell[i, 1].Text := line2[i];
    end;
  if (FStrata > 0) then
    begin
      T.Cell[0, 0].Text := sBy;
      T.Cell[0, 1].Text := FStratVarName;
    end;

  for i := 1 to FStrata do
    outputStratumResults(i, i + 1);
  outputStratumResults(0, FStrata + 2);

  if ((FStrata > 0) and (ST.HasOption('t'))) then
    T.Footer.Text := sSurLogRankChi + ' = ' + Format(StatFmt, [FLRChi]) + ' ' + FormatP(FLRP, true) +
                     sLineBreak + sSurHazardRatio + ' = ' + Format(StatFmt, [FHazRatio]);
  T.SetRowBorders(0, [cbTop]);
  T.SetRowBorders(1, [cbBottom]);
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
    aTitle += sBy + ' ' + FStratVarName;
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
  sColor:     array of TColor = (clBlack, clBlue, clGreen, clRed, clMaroon);
  aPattern:   TFPBrushStyle;
  sPattern:   array of TFPBrushStyle = (bsDiagCross, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
begin
  if (Stratum > 0) then
    aText  := FStratVarname + '=' + FStratLabels[Stratum - 1]
  else
    aText  := sSurCommand;
  aColor   := sColor[min(Stratum,4)];
  aPattern := sPattern[min(Stratum,4)];
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
    FCBPlot += sAllData + lineending
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

begin
  FExecutor.ClearResults('$survival');

  VarNames             := Command.VariableList.GetIdentsAsList;
  StratVariable        := TStringList.Create;
  FDecimals            := DecimalFromOption(Command.Options, 3);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FintFlag              := false;
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
          FRefStratum := 0;
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
          FintFlag := true;
          FintervalString := Opt.Expr.AsIdent;
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
            DoInitializeKMPlot;
            Stratum1 := 0;
            if (FStrata > 0) then
              Stratum1 := 1;;
            for Stratum := Stratum1 to FStrata do
            begin
              DoCalcPlotPoints(Stratum);
              DoAddGraphSeries(Stratum);
              if (Command.HasOption('cb')) then
                DoAddCBPlotPoints(Stratum);
            end;
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
