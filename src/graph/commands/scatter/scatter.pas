unit scatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration;

type

  { TScatterChart }

  TScatterChart = class(TInterfacedObject, IChartCommand)
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
  TASeries, TATypes, Graphics, charttitles, ast_types, scattersource, epidatafilestypes,
  epifields_helper, options_utils;

{ TScatterChart }

procedure TScatterChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TScatterChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  LineSeries: TLineSeries;
  ScatterSource: TScatterSource;
  VarNames: TStrings;
  Chart: TChart;
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
  ScatterSource := TScatterSource.Create(Chart);
  ScatterSource.Datafile := DataFile;
  ScatterSource.XVariableName := XVar.Name;
  ScatterSource.YVariableName := YVar.Name;

  // Create the line/point series
  LineSeries := TLineSeries.Create(Chart);
  LineSeries.ShowPoints := true;
  LineSeries.LineType := ltNone;
  LineSeries.Pointer.Brush.Style := bsClear;
  LineSeries.Pointer.Pen.Color := clBlack;
  LineSeries.Pointer.Style := psCircle;
  LineSeries.Source := ScatterSource;

  // Add series to the chart
  Chart.AddSeries(LineSeries);

  // Create the titles
  VariableLabelType := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  ChartConfiguration.GetTitleConfiguration()
    .SetTitle(XVar.GetVariableLabel(VariableLabelType) + ' vs. ' + YVar.GetVariableLabel(VariableLabelType))
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
  RegisterChartCommand(stScatter, TScatterChart);

end.
