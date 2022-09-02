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
  // survival table results
    FInterval:            Array of Array of UTF8String;
    FTime,
    FAtRisk,
    FLost,
    FFail:                Array of Array of Integer;
    FSurvival,
    FLowCI,
    FHighCI:              Array of Array of EpiFloat;
    FMaxRow:              Array of Integer;
  // summary results
    FMedian:              Array of UTF8String;
    FLRChi, FLRP:         EpiFloat;
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
  generalutils, Math, statfunctions, options_utils, Clipbrd,
  ast_types, forms, graphformfactory, TACustomSource;

{ TSurvival }

procedure TSurvival.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory  := ChartFactory;
  FExecutor      := Executor;
  FOutputCreator := OutputCreator;
end;

procedure TSurvival.DoCalcSurvival(InputDF: TEpiDataFile;
  Variables: TStrings; StratVariable: TStringList;
  ST: TCustomVariableCommand);

var
  i, Failures, FailIx, Row, Col, Stratum, NAtRisk, NEffective: Integer;
  S, SE, SumF, CIMult: EpiFloat;
  T: TTables;
  Statistics: TTableStatistics;
  ASurvivalTable: TTwowayTable;
  TablesRefMap: TEpiReferenceMap;
  GetMedian: Boolean;
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
          FailIx := Col;
    if (FailIx < 0) then
      begin
        FExecutor.Error('No records with ' + Variables[0] + ' = ' + FFailOutcomeValue);
        exit;
      end;
    FFailOutcomeText  := ColVariable.GetValueLabelFormatted(FailIx,FValueLabelOutput);

    // validate that time value has only positive integers
    if (RowVariable.AsInteger[0] < 0) then
      begin
        FExecutor.Error('The time variable, ' + Variables[1] + ', has negative values');
        exit;
      end;
  end;

  FStrata    := FSurvivalTable.Count;
  FIntervals := FSurvivalTable.UnstratifiedTable.RowCount;
  SetLength(FStratlabels, FStrata);
  SetLength(FInterval,    FStrata + 1, FIntervals);
  SetLength(FTime,        FStrata + 1, FIntervals);
  SetLength(FAtRisk,      FStrata + 1, FIntervals);
  SetLength(FFail,        FStrata + 1, FIntervals);
  SetLength(FLost,        FStrata + 1, FIntervals);
  SetLength(FSurvival,    FStrata + 1, FIntervals);
  SetLength(FLowCI,       FStrata + 1, FIntervals);
  SetLength(FHighCI,      FStrata + 1, FIntervals);
  SetLength(FMedian,      Fstrata + 1);
  SetLength(FMaxRow,      FStrata + 1);


  // set up confidence interval
  FConf  := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  CIMult := PNORMALINV((Float(100 - FConf) / 200.0));

  // unstratified table (0) first

  for Stratum := 0 to FStrata do
    begin
      if (Stratum = 0) then
        ASurvivalTable := FSurvivalTable.UnstratifiedTable
      else
        begin
          ASurvivalTable := FSurvivalTable.Tables[Stratum - 1];
          FStratLabels[Stratum - 1] := FSurvivalTable.StratifyVariables.Field[0].GetValueLabel(Stratum - 1);
        end;
      NAtRisk   := ASurvivalTable.Total;
      NEffective:= NAtRisk;
      S         := 1.0;
      SumF      := 0.0;
      GetMedian := true;
      Row       := 0;   // index for saved values (# table rows with NAtRisk>0)
      for i := 0 to ASurvivalTable.RowCount - 1 do
        begin
          if (NAtRisk > 0) then
            begin
              if (ASurvivalTable.RowTotal[i] > 0) then
                FMaxRow[Stratum] := Row;                   // track last row with data
              Failures := ASurvivalTable.Cell[FailIx, i].N;
              S        := S * Float((NAtRisk - Failures)) / Float(NAtRisk);
// Greennwood's method
//              SumF     += Float(Failures) / Float(NAtRisk*(NAtRisk - Failures));
//              SE       := S * SQRT(SumF);
// Altman's method
              SE       := sqrt(S*(1-S)/Float(NEffective));
              FInterval[Stratum, Row] := ASurvivalTable.RowVariable.GetValueLabel(i, FValuelabelOutput);
              FTime    [Stratum, Row] := ASurvivalTable.RowVariable.AsInteger[i];
              FAtRisk  [Stratum, Row] := NAtRisk;
              FFail    [Stratum, Row] := Failures;
              FLost    [Stratum, Row] := ASurvivalTable.RowTotal[i] - Failures;
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

          NAtRisk  := NAtRisk - ASurvivalTable.RowTotal[i];
          NEffective := NEffective - FLost[Stratum, Row];
        end;

      // was median found? It won't be if final survival > 0.5
      if (GetMedian) then
        FMedian[Stratum] := '> ' + FInterval[Stratum, ASurvivalTable.RowCount - 1];
    end;
  ST.ExecResult := csrSuccess;

  T.Free;
end;

procedure TSurvival.DoLogRank();
var
  SumExp: EpiFloat;
  SumFail: Integer;
  d: EpiFloat;
  i, Stratum: Integer;
begin
  if (FStrata = 0) then exit;  // Error - should not happen

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
  DoOneResult(0, 'all');

  if FStrata > 0 then
  begin
    sNames := FExecutor.AddResultVector('$survival_strata', ftString, FStrata);
    if (ST.HasOption('t') and (FStrata > 0)) then
    begin
      FExecutor.AddResultConst('$survival_' + 'chi2', ftFloat).AsFloatVector[0] := FLRChi;
      FExecutor.AddResultConst('$survival_' + 'chip', ftFloat).AsFloatVector[0] := FLRP;
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
  T.Header.Text := 'Kaplan Meier Survival Analysis - Life Tables';
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
      T.Cell[1,0].Text := 'All data';
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

  T.Cell[0, 1].Text := 'Followup';
  T.Cell[0, 2].Text := FTimeVarLabel;
  Offset            := 1;

  // Column headers
  for Stratum := FirstStratum to LastStratum do
    begin
      T.Cell[    Offset, 1].Text := '# At';
      T.Cell[1 + Offset, 1].Text := FOutcomeVarLabel;
      T.Cell[2 + Offset, 1].Text := ' ';
      T.Cell[3 + Offset, 1].Text := ' ';
      T.Cell[    Offset, 2].Text := 'Risk';
      T.Cell[1 + Offset, 2].Text := FFailOutcomeText;
      T.Cell[2 + Offset, 2].Text := 'Survival';
      T.Cell[3 + Offset, 2].Text := '(' + IntToStr(FConf) + '% CI)';
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
begin
  StatFmt := '%' + IntToStr(3 + FDecimals) + '.' + IntToStr(FDecimals) + 'F';

  T                 := FOutputCreator.AddTable;
  T.Header.Text     := 'Kaplan-Meier Survival Analysis - Summary';
  T.ColCount        := FStrata + 2;
  T.RowCount        := 3;
  T.Cell[1, 1].Text := 'All Data';
  T.Cell[0, 2].Text := 'Median Survival';
  if (FStrata > 0) then
    T.Cell[2, 0].Text := FStratVarName;

  for i := 0 to FStrata do
    begin
      if (i > 0) then
        T.Cell[i + 1, 1].Text := FStratLabels[i-1];
      T.Cell[i + 1, 2].Text := FMedian[i];
    end;
    T.SetRowBorders(1, [cbBottom]);

  if ((FStrata > 0) and (ST.HasOption('t'))) then
    T.Footer.Text := 'Log-Rank Chi-square = ' + Format(StatFmt, [FLRChi]) + ' ' + FormatP(FLRP, true);
  T.SetRowBorders(1, [cbTop]);
  T.SetRowBorders(2, [cbBottom]);
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
  aTitle := 'KM Plot for outcome ' + FOutcomeVarLabel;
  if (FStrata > 0) then
    aTitle += ' by ' + FStratVarName;
  aFoot := 'Failure: ' + FOutcomeVarLabel + ' = ' + FFailOutcomeText;
  if (FWeightVarName <> '') then
    aFoot += ' (weighted by ' + FWeightVarName + ')';
  FChart := FChartFactory.NewChart();
  FChartConfiguration := FChartFactory.NewChartConfiguration();
  FTitles := FChartConfiguration.GetTitleConfiguration()
    .SetTitle(aTitle)
    .SetFootnote(aFoot)
    .SetXAxisTitle('Time: ' + FTimeVarLabel)
    .SetYAxisTitle('Survival');
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
    aText  := '';
  aColor   := sColor[min(Stratum,4)];
  aPattern := sPattern[min(Stratum,4)];
  case FCIType of
    0:     // default - verticle bars
      begin
        FChart.AddSeries(SurvivalGraphData(FPlotS, FPlotLL, FPlotUL, 'survival ' + aText, psSolid, 2, aColor));
      end;
    1 :    // line
      begin
        // the order of adding series matters because of a bug in TAChart
        // that does not always respect psDot, depending on the series index
        FChart.AddSeries(SurvivalGraphData(FPlotUL, 'CI UL ' + aText, psDot, 2, aColor, false));
        FChart.AddSeries(SurvivalGraphData(FPlotS,  'survival ' + aText, psSolid, 2, aColor));
        FChart.AddSeries(SurvivalGraphData(FPlotLL, IntToStr(FConf) + '% CI ' + aText, psDot, 2, aColor));
      end;
    2 :    // band
      begin
        FChart.AddSeries(SurvivalGraphData(FPlotS,  'survival ' + aText, psSolid, 2, aColor));
        FChart.AddSeries(SurvivalBand(IntToStr(FConf) + '% CI ' + aText, aPattern, aColor));
      end;
    else   // none
      FChart.AddSeries(SurvivalGraphData(FPlotS,  'survival ' + aText, psSolid, 2, aColor));
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
    YErrorBars.Visible := true;
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
  FCBplot += 'KM Plot for ' + FOutcomeVarLabel + ' at time ' + FTimeVarLabel + lineending;
  if (Stratum = 0) then
    FCBPlot += 'all data' + lineending
  else
    FCBPlot += FStratVarName + ' = ' + FStratLabels[Stratum - 1] + lineending;
  FCBPlot +=  'Time' + d + 'Survival' + d + 'CIlow' + d + 'CIHigh' + lineending;
  for i := 0 to high(FPlotT) do
    FCBPlot += trim(Format('%6.0f', [FPlotT[i]]))  + d +
               trim(Format('%6.3f', [FPlotS[i]]))  + d +
               trim(Format('%6.3f', [FPlotLL[i]])) + d  +
               trim(Format('%6.3f', [FPlotUL[i]])) + lineending;
end;

procedure TSurvival.DoOutputCBPlotPoints();
begin
    Clipboard.AsText:=FCBPlot;
    FOutputCreator.DoInfoShort('Plot points for KM plots were saved to the clipboard');
end;

function TSurvival.Execute(Command: TCustomGraphCommand) : IChartCommandResult;
var
  VarNames:         TStrings;
  AllVariables:     TStrings;
  StratVariable:    TStringList;
  Opt:              TOption;
  DF:               TEpiDataFile;
  Stratum, Stratum1:Integer;

begin
  FExecutor.ClearResults('$survival');

  VarNames             := Command.VariableList.GetIdentsAsList;
  StratVariable        := TStringList.Create;
  FDecimals            := DecimalFromOption(Command.Options, 3);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  AllVariables         := Command.VariableList.GetIdentsAsList;

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
              FExecutor.Error('Cannot specify more than one outcome. !o:=' + Opt.Expr.AsIdent + ' is invalid');
              exit;
            end;
          FFailOutcomeValue := Opt.Expr.AsString;
        end;

      'by':
        begin
          if (FStratVarName <> '') then
            begin
              FExecutor.Error('Can only stratify by one variable; !by:=' + Opt.Expr.AsIdent + ' is invalid');
              exit;
            end;
          if ( (VarNames[0] = Opt.Expr.AsIdent) or (VarNames[1] = Opt.Expr.AsIdent) ) then
            begin
              FExecutor.Error('Cannot stratify by ' + Opt.expr.AsIdent);
              Exit;
            end;
          FStratVarName := Opt.Expr.AsIdent;
          StratVariable.Add(FStratVarName);
          AllVariables.AddStrings(FStratVarName);
        end;

      'w':  // weight variable
        begin
          if (FWeightVarName <> '') then
            begin
              FExecutor.Error('Can only use one weight variable; !w:=' + Opt.Expr.AsIdent + ' is invalid');
              exit;
            end;
          if ( (VarNames[0] = Opt.Expr.AsIdent) or (VarNames[1] = Opt.Expr.AsIdent) ) then
            begin
              FExecutor.Error(Opt.expr.AsIdent + ' cannot be used as a weight');
              Exit;
            end;
          FWeightVarName := Opt.Expr.AsIdent;
          AllVariables.Add(FWeightVarName);
        end;
    end;

    if (FFailOutcomeValue = '') then
        FFailOutcomeValue := '0';

    DF := FExecutor.PrepareDatafile(AllVariables, AllVariables);

    if DF.Size = 0 then
      begin
        FExecutor.Error('No data!');
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

  finally
    StratVariable.Free;
    AllVariables.Free;
  end;

end;

initialization
  RegisterChartCommand(stSurvival, TSurvival);

end.
