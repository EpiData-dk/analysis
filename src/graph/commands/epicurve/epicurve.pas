unit epicurve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration;

type

  { TEpicurveChart }

  TEpicurveChart = class(TInterfacedObject, IChartCommand)
  private
    FChartFactory: IChartFactory;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  TASeries, TATypes, Graphics, charttitles, ast_types, epidatafilestypes,
  tables_types, tables,
  epifields_helper, options_utils;

{ TEpicurveChart }

procedure TEpicurveChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TEpicurveChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  TimeSeries: TBarSeries;
  EpicurveSource: TListChartSource;
  VarNames: TStrings;
  Chart: TChart;
  Titles: IChartTitleConfiguration;
  DataFile: TEpiDataFile;
  XVar, ByVar: TEpiField;
  ChartConfiguration: IChartConfiguration;
  VariableLabelType: TEpiGetVariableLabelType;
  T: TTables;
  F: TTable;
  Statistics: TTableStatistics;
  TablesRefMap: TEpiReferenceMap;
  EpicurveTable: TTwowayTable;
  StratVariable,
  WeightVarName: UTF8String;
  EpicurveCounts: array of array of Byte;
  Boxes: array of Float;
  ix, iy: Integer;
begin
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;

  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(VarNames, VarNames);
  XVar := Datafile.Fields.FieldByName[VarNames[0]];
  Varnames.Free;
  StratVariable := '';
  WeightVarName := '';
  // Get frequencies by time variable
  T := TTables.Create(FExecutor, FOutputCreator);
  F := TTables.UnstratifiedTable;
  EpicurveTable  := T.CalcTables(Datafile, VarNames,
    StratVariable, WeightVarName, ST.Options, TablesRefMap, Statistics);

  // populate EpicurveCounts
  EpicurveCounts := DoCounts(EpicurveTable);

  // Create the charts
  Chart := FChartFactory.NewChart();


  // Create the line/point series
  TimeSeries := TBarSeries.Create(Chart);
  for ix := 0 to F.Size -1 do
    begin
      Boxes := [];
      for iy := 1 to F.RowVariable[0].Count do
        Boxes := Concat(Boxes,1);
      EpicurveSource.AddXYList(F.ColVariable.AsFloat, Boxes);
    end;

  TimeSeries.Stacked := true;
  TimeSeries.Source := EpicurveSource;

  // Add series to the chart
  Chart.AddSeries(TimeSeries);

  // Create the titles
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  VariableLabelType := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle('Epidemic Curve for ' + XVar.GetVariableLabel(VariableLabelType)
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelType));

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);

  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration();

  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart, ChartConfiguration);
end;

initialization
  RegisterChartCommand(stEpicurve, TEpicurveChart);

end.
