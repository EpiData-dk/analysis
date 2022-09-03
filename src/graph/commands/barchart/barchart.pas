unit barchart;

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
  TASeries, TASources, TATypes, TAChartUtils, Graphics, charttitles, ast_types, epidatafilestypes,
  epifields_helper, options_utils, barchartsource;

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
  LabelSeries: TListChartSource;
  VarNames: TStrings;
  Chart: TChart;
  Titles: IChartTitleConfiguration;
  DataFile: TEpiDataFile;
  XVar, YVar: TEpiField;
  ChartConfiguration: IChartConfiguration;
  VariableLabelType: TEpiGetVariableLabelType;
  i: Integer;
begin
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;

  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(VarNames, VarNames);
  XVar := Datafile.Fields.FieldByName[VarNames[1]];
  YVar := Datafile.Fields.FieldByName[Varnames[0]];
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

  // Create the bar series
  BarSeries := TBarSeries.Create(Chart);
  BarSeries.Source := BarSource;
  BarSeries.BarWidthPercent:=75;

  // Create the bar labels
  LabelSeries := TListChartSource.Create(Chart);
  for i := 0 to DataFile.Size - 1 do
    LabelSeries.Add(XVar.AsFloat[i], YVar.AsFloat[i], XVar.AsString[i]);   // TODO: use value label for 3rd parameter!

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

  Chart.BottomAxis.Marks.Source := LabelSeries;
  Chart.BottomAxis.Marks.Style := smsLabel;

  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart, ChartConfiguration);
end;

initialization
  RegisterChartCommand(stBarchart, TBarChart);

end.
