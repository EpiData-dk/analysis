unit bar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration;

type

  { TBarChart }

  TBarChart = class(TInterfacedObject, IChartCommand)
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
  TASeries, TATypes, Graphics, charttitles, ast_types, barsource, epidatafilestypes,
  epifields_helper, options_utils;

{ TBarChart }

procedure TBarChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TBarChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  BarSeries: TBarSeries;
  BarSource: TBarSource;
  VarNames: TStrings;
  Chart: TChart;
  Titles: IChartTitleConfiguration;
  DataFile: TEpiDataFile;
  XVar, YVar: TEpiField;
  ChartConfiguration: IChartConfiguration;
  VariableLabelType: TEpiGetVariableLabelType;
begin
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;

  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(VarNames, VarNames);
  XVar := Datafile.Fields.FieldByName[VarNames[0]];
  YVar := Datafile.Fields.FieldByName[Varnames[1]];
  Varnames.Free;

  // Create the charts
  Chart := FChartFactory.NewChart();

  // Create our own datasource
  // - datasource is destroyed by the chart, so we let it handle the datafile destruction
  //   otherwise we would leak memory.
  BarSource := TBarSource.Create(Chart);
  BarSource.Datafile := DataFile;
  BarSource.XVariableName := XVar.Name;
  BarSource.YVariableName := YVar.Name;

  // Create the line/point series
  BarSeries := TBarSeries.Create(Chart);
  BarSeries.Source := BarSource;
  BarSeries.BarWidthPercent:=5;

  // Add series to the chart
  Chart.AddSeries(BarSeries);

  // Create the titles
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  VariableLabelType := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle(XVar.GetVariableLabel(VariableLabelType) + ' by ' + YVar.GetVariableLabel(VariableLabelType))
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelType))
    .SetYAxisTitle(YVar.GetVariableLabel(VariableLabelType));

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);

  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration()
    .SetShowAxisMarksAsDates(YVar.FieldType in DateFieldTypes);

  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart, ChartConfiguration);
end;

initialization
  RegisterChartCommand(stBar, TBarChart);

end.
