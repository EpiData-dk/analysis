unit epicurve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TASources, TACustomSource, TALegend, tables_types, tables, freq;

type

  floatArray = array of Double;
  freqArray  = array of array of Double;
  { TEpicurveChart }

  TEpicurveChart = class(TInterfacedObject, IChartCommand)
  private
    FChartFactory: IChartFactory;
    FChart: TChart;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FValueLabelOutput:    TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FEpicurveSource: TListChartSource;
    FxLow, FxHigh: Integer;
    FLabel: array of Integer;
    FCateg: array of UTF8String;
    FmaxCount: Integer;
    FByVarName: UTF8String;
    FxAxisSource, FyAxisSource: TListChartSource;
    function doBoxes(n: Double): floatArray;
    function Table2Array(T: TTwoWayTable): freqArray;
    function Freq2Array(F: TFreqDataFile): freqArray;
    procedure doAddSeries(Freqs: freqArray);
    procedure doAddAxisScales();
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  TASeries, TATypes, TAStyles, Graphics, charttitles, ast_types,
  options_utils, math;

{ TEpicurveChart }

procedure TEpicurveChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

procedure TEpicurveChart.doAddSeries(Freqs: freqArray);
var
  TimeSeries: TBarSeries;
  SeriesStyles: TChartStyles;
  aStyle:TChartStyle;
  aLegend: TChartLegendItems;
  aColor:     TColor;
  sColor:     array of TColor = (clRed, clBlue, clGreen, clYellow, clGray);
  i,j: Integer;
  Boxes: boolean;
  xLow: Double;
begin
  TimeSeries := TBarSeries.Create(FChart);
  SeriesStyles := TChartStyles.Create(FChart);
  FEpicurveSource := TListChartSource.Create(FChart);
  aLegend := TChartLegendItems.Create;
  FEpicurveSource.YCount := FmaxCount;
  Boxes := length(Freqs[0]) = 1;
  xLow := FLabel[0];
  for i := 0 to length(Freqs) - 1 do
    begin
      while xLow < FLabel[i] do
        begin
          FEpicurveSource.Add(xLow, 0);
          xLow := xLow + 1;
        end;
      if (Boxes) then
        FEpicurveSource.AddXYList(xLow, doBoxes(Freqs[i, 0]))
      else
        FEpicurveSource.AddXYList(xLow, Freqs[i]);
      xLow := xLow + 1;
    end;
  TimeSeries.Source := FEpicurveSource;
  TimeSeries.Stacked := true;
  TimeSeries.BarWidthPercent := 100;
  // Add series to the chart
  FChart.AddSeries(TimeSeries);
  if (not Boxes) then
    begin
      for i := 0 to length(Freqs[0]) - 1 do
        begin
          aStyle := SeriesStyles.Add;
          aStyle.Text := FByVarName + ' = ' + FCateg[i];
          aStyle.Brush.Color:=sColor[i];;
        end;
      TimeSeries.Styles := SeriesStyles;
      TimeSeries.Legend.Multiplicity:=lmStyle;
      TimeSeries.Legend.GroupIndex  := 1;
      aLegend.Add(TimeSeries.Legend);

      FChart.Legend.Visible        := true;
      FChart.Legend.UseSidebar     := true;
      FChart.Legend.Frame.Visible  := false;
      FChart.Legend.AddGroups(aLegend);
      FChart.Legend.GroupTitles.Add(FByVarName);
    end;
end;

function TEpicurveChart.Freq2Array(F: TFreqDataFile): freqArray;
var
  i: Integer;
begin
  setLength(result, F.Size, 1);
  setLength(FLabel, F.Size);
  FMaxCount := 0;
  for i := 0 to F.Size - 1 do
    begin
      result[i, 0] := F.Count.AsFloat[i];
      FLabel[i]    := F.Categ.AsInteger[i];
      FmaxCount    := Math.Max(FmaxCount, F.Count.AsInteger[i]);
    end;
end;

function TEpicurveChart.Table2Array(T: TTwoWayTable): freqArray;
var
  i, j, k: Integer;
begin
  setLength(result, T.ColCount, T.RowCount);
  setLength(FLabel, T.ColCount);
  setLength(FCateg, T.RowCount);
  FMaxCount := 0;
  for i := 0 to T.ColCount - 1 do
    begin
      FMaxCount := Math.Max(FMaxCount, T.ColTotal[i]);
      FLabel[i] := T.ColVariable.AsInteger[i];
      k := FLabel[i];
      for j := 0 to T.RowCount - 1 do
        begin
          result[i, j] := T.Cell[i, j].N.ToDouble;
        end;
      end;
  for j := 0 to T.RowCount - 1 do
    begin
      FCateg[j] := T.RowVariable.AsString[j];
    end;
end;

function TEpicurveChart.doBoxes(n: double): floatArray;
var
  i, j: Integer;
begin
  j := Trunc(n);
  setLength(result, j);
  for i := 0 to j - 1 do
    result[i] := 1.0;
end;

procedure TEpicurveChart.doAddAxisScales();
var
  i: Integer;
  tick: Double;
begin
  FxAxisSource := TListChartSource.Create(FChart);
  FyAxisSource := TListChartSource.Create(FChart);
  for i := FLabel[0] to FLabel[Length(FLabel)-1] do
    begin
      tick := i.ToDouble;
      FxAxisSource.Add(tick, tick);
    end;
  tick := 0;
  for i := 0 to (FmaxCount div 5) do
    begin
      FyAxisSource.Add(tick, tick);
      tick += 5;
    end;
end;

function TEpicurveChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  VarNames: TStrings;
  Titles: IChartTitleConfiguration;
  DataFile: TEpiDataFile;
  XVar: TEpiField;
  ChartConfiguration: IChartConfiguration;
  VariableLabelType: TEpiGetVariableLabelType;
  T: TTables;
  F: TFreqCommand;
  Freqs: array of array of double;
  Statistics: TTableStatistics;
  TablesRefMap: TEpiReferenceMap;
  EpicurveTable: TTwowayTables;
  EpicurveFreq: TFreqDatafile;
  StratVariable: TStringList;
  WeightVarName: UTF8String;
  AllVariables: TStrings;
  Opt: TOption;
begin
  FmaxCount := 0;
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;
  AllVariables := VarNames;
  StratVariable := TStringList.Create;
  FVariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);

  // check for weight variable
  WeightVarName := '';
  if (Command.HasOption(['w'],Opt)) then
    begin
      WeightVarName := Opt.Expr.AsIdent;
      AllVariables.Add(WeightVarName);
    end;

  {// check for stratifying variable
  if (Command.HasOption(['by'],Opt)) then
    begin
      VarNames.Add(Opt.Expr.AsIdent);
      AllVariables.Add(Opt.Expr.AsIdent);
    end;
  }
  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(AllVariables, AllVariables);
  XVar := Datafile.Fields.FieldByName[VarNames[0]];

  // Create the chart
  FChart := FChartFactory.NewChart();

  // add series for the time variable
  // method depends on stratification or not

  // *** Stratification won't work with boxes ***
  // so revert to histogram

  if (Varnames.Count > 1) then
    begin
  // with stratification
  // Note: this does NOT call CalcTables with stratification
      T := TTables.Create(FExecutor, FOutputCreator);
      EpicurveTable  := T.CalcTables(Datafile, VarNames,
                    StratVariable, WeightVarName, Command.Options, TablesRefMap, Statistics);
        Freqs := Table2Array(EpicurveTable.UnstratifiedTable);
      T.Free;
      FByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(FVariableLabelOutput);
    end
  else
    begin
      F := TFreqCommand.Create(FExecutor, FOutputCreator);
      EpicurveFreq := F.CalcFreq(Datafile, VarNames[0],TablesRefMap);
      Freqs := Freq2Array(EpicurveFreq);
      F.Free;
    end;

  doAddSeries(Freqs);
  doAddAxisScales();

  // Create the titles
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  VariableLabelType := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle('Count by ' + XVar.GetVariableLabel(VariableLabelType))
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelType))
    .SetYAxisTitle('Count');

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);
  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration();
  with FChart do
    begin
      BottomAxis.Marks.Source := FxAxisSource;
      BottomAxis.Grid.Style := psClear;
      LeftAxis.Grid.Style := psClear;
      LeftAxis.Marks.Source := FyAxisSource;
      Frame.Visible := false;
    end;
  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(FChart, ChartConfiguration);
  AllVariables.Free;
end;

initialization
  RegisterChartCommand(stEpicurve, TEpicurveChart);

end.
