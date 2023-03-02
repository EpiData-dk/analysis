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
  epifields_helper, options_utils, chart_options;

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
  sColor: TColorMap;
  VariableLabelType: TEpiGetVariableLabelType;
  sPoints, sLine: Boolean;
  msg: UTF8String;
begin
  sColor := ChartColorsFromOptions(Command.Options, FExecutor.SetOptions, msg);
  if (msg <> '') then
  begin
    FExecutor.Error(msg);
    exit;
  end;
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;
  // Create the charts
  Chart := FChartFactory.NewChart();
  Result := FChartFactory.NewGraphCommandResult();

  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(VarNames, VarNames);
  if (DataFile.Size < 1) then
    FExecutor.Error('No Data')
  else
  begin  // create chart
    XVar := Datafile.Fields.FieldByName[VarNames[0]];
    YVar := Datafile.Fields.FieldByName[Varnames[1]];
    DataFile.SortRecords(XVar);
    Varnames.Free;


    // Create our own datasource
    // - datasource is destroyed by the chart, so we let it handle the datafile destruction
    //   otherwise we would leak memory.
    ScatterSource := TScatterSource.Create(Chart);
    ScatterSource.Datafile := DataFile;
    ScatterSource.XVariableName := XVar.Name;
    ScatterSource.YVariableName := YVar.Name;
    ScatterSource.Sorted := true;

    // Get options

    sLine := Command.HasOption('l');
    sPoints := (not Command.HasOption('l')) or Command.HasOption('p');

    // Create the line/point series
    LineSeries := TLineSeries.Create(Chart);
    with LineSeries do
      begin
        Source := ScatterSource;
        ShowPoints := sPoints;
        if (sPoints) then
        begin
          Pointer.Pen.Color := sColor[0];
          Pointer.Style := psCircle;
          Linetype := ltNone;
        end;
        if (Sline) then
        begin
          LineType := ltFromPrevious;
          LinePen.Color:= sColor[0];
        end;
      end;
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

      with Chart do
      begin
        BottomAxis.Grid.Style   := psClear;
        BottomAxis.Margin       := 0;
        LeftAxis.Grid.Style     := psDot;
        LeftAxis.Grid.Color     := clSilver;
        LeftAxis.Margin         := 0;
        Frame.Visible           := false;
      end;

    // Create the command result
    Result.AddChart(Chart, ChartConfiguration);
  end;   // create chart
end;

initialization
  RegisterChartCommand(stScatter, TScatterChart);

end.
