unit epicurve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TASources, tables_types, tables, freq;

type

  floatArray = array of Double;
  { TEpicurveChart }

  TEpicurveChart = class(TInterfacedObject, IChartCommand)
  private
    FChartFactory: IChartFactory;
    FChart: TChart;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FEpicurveSource: TListChartSource;
    function doBoxes(n: Integer): floatArray;
    procedure doAddTableSeries(F: TTwoWayTable; index: Integer);
    procedure doAddFreqSeries(F: TFreqDataFile);
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  TASeries, TATypes, Graphics, charttitles, ast_types,
  epifields_helper, options_utils;

{ TEpicurveChart }

procedure TEpicurveChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

procedure TEpicurveChart.doAddTableSeries(F: TTwoWayTable; index: Integer);
var
  TimeSeries: TBarSeries;
  i: Integer;
begin
  TimeSeries := TBarSeries.Create(FChart);
  FEpicurveSource := TListChartSource.Create(FChart);
  FEpicurveSource.YCount := F.RowCount;
  for i := 0 to F.RowCount - 1 do
    FEpicurveSource.AddXYList(F.RowVariable.AsFloat[index], doBoxes(F.RowTotal[i]));
  TimeSeries.Source := FEpicurveSource;
  TimeSeries.Stacked := true;

  // Add series to the chart
  FChart.AddSeries(TimeSeries);
end;

procedure TEpicurveChart.doAddFreqSeries(F: TFreqDataFile);
var
  TimeSeries: TBarSeries;
  i, n: Integer;
  xLow: Double;
begin
  TimeSeries := TBarSeries.Create(FChart);
  FEpicurveSource := TListChartSource.Create(FChart);
  FEpicurveSource.YCount := F.Size;
  xLow := F.Categ.AsFloat[0];
  for i := 0 to F.Size - 1 do
    begin
      while xLow < F.Categ.AsFloat[i] do
      begin
        FEpiCurveSource.Add(xLow, 0.0);
        xLow := xLow + 1;
      end;
      FEpicurveSource.AddXYList(F.Categ.AsFloat[i], doBoxes(F.Count.AsInteger[i]));
      xLow := xLow + 1;
    end;
  TimeSeries.Source := FEpicurveSource;
  TimeSeries.Stacked := true;
  TimeSeries.BarWidthPercent := 100;

  // Add series to the chart
  FChart.AddSeries(TimeSeries);
end;

function TEpicurveChart.doBoxes(n: Integer): floatArray;
var
  i: Integer;
begin
  setLength(result, n);
  for i := 0 to n - 1 do
    result[i] := 1.0;
end;

function TEpicurveChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  TimeSeries: TBarSeries;
  VarNames: TStrings;
  Titles: IChartTitleConfiguration;
  DataFile: TEpiDataFile;
  XVar, ByVar: TEpiField;
  ChartConfiguration: IChartConfiguration;
  VariableLabelType: TEpiGetVariableLabelType;
  T: TTables;
  F: TFreqCommand;
  Freqs: TTwoWayTable;
  Statistics: TTableStatistics;
  TablesRefMap: TEpiReferenceMap;
  EpicurveTable: TTwowayTables;
  EpicurveFreq: TFreqDatafile;
  StratVariable: TStringList;
  WeightVarName: UTF8String;
  AllVariables: TStrings;
  Opt: TOption;
  Boxes: array of EpiFloat;
  i, ix, iy: Integer;
  xLow, xHigh: Integer;
begin
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;
  AllVariables := VarNames;
  StratVariable := TStringList.Create;

  // check for weight variable
  WeightVarName := '';
  if (Command.HasOption(['w'],Opt)) then
    begin
      WeightVarName := Opt.Expr.AsIdent;
      AllVariables.Add(WeightVarName);
    end;

  // check for stratifying variable
  if (Command.HasOption(['by'],Opt)) then
    begin
//      StratVariable.Add(Opt.Expr.AsIdent);
      VarNames.Add(Opt.Expr.AsIdent);
      AllVariables.Add(Opt.Expr.AsIdent);
    end;

  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(AllVariables, AllVariables);
  XVar := Datafile.Fields.FieldByName[VarNames[0]];

  // Create the chart
  FChart := FChartFactory.NewChart();

  // add series for the time variable
  // method depends on stratification or not

  if (Command.HasOption('by')) then
    begin
  // with stratification
      T := TTables.Create(FExecutor, FOutputCreator);
      EpicurveTable  := T.CalcTables(Datafile, VarNames,
                    StratVariable, WeightVarName, Command.Options, TablesRefMap, Statistics);
       Freqs := EpicurveTable.UnstratifiedTable;
       xLow := Freqs.RowVariable.AsInteger[0];
       xHigh := Freqs.RowVariable.AsInteger[Freqs.RowCount - 1];
       for i:= 0 to Freqs.RowCount - 1 do
         begin
           doAddTableSeries(Freqs, i);
         end;
     end
  else
    begin
      F := TFreqCommand.Create(FExecutor, FOutputCreator);
      EpicurveFreq := F.CalcFreq(Datafile, VarNames[0],TablesRefMap);
      xLow := EpicurveFreq.Categ.AsInteger[0];
      xHigh := EpicurveFreq.Categ.AsInteger[EpicurveFreq.Size - 1];
      doAddFreqSeries(EpicurveFreq);
    end;

  // Create the titles
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  VariableLabelType := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle('Epidemic Curve for ' + XVar.GetVariableLabel(VariableLabelType))
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelType))
    .SetYAxisTitle('Count');

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);
  FChart.BottomAxis.Marks.AtDataOnly := true;
  FChart.BottomAxis.Range.Min := xLow;
  FChart.BottomAxis.Range.Max := xHigh;
  FChart.BottomAxis.Grid.Style := psClear;
  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration();
  FChart.LeftAxis.Grid.Style := psClear;
  FChart.LeftAxis.Marks.AtDataOnly:=true;
  FChart.Frame.Visible := false;
  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(FChart, ChartConfiguration);
  AllVariables.Free;
end;

initialization
  RegisterChartCommand(stEpicurve, TEpicurveChart);

end.
