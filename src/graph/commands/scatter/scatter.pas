unit scatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, executor, outputcreator,
  ast, epidatafiles, TASources, TACustomSource, chartcommand, chartfactory;

type

  { TScatterChart }

  TScatterChart = class(TAbstractChartCommand)
  private
    FChartFactory: IChartFactory;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  public
    destructor destroy; override;
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator); override;
    function Execute(Command: TCustomGraphCommand): IChartCommandResult; override;
    function GetObject(): TObject; override;
  end;

implementation

uses
  TASeries, TATypes, Graphics, charttitles, ast_types, scattersource;

{ TScatterChart }

destructor TScatterChart.destroy;
begin
  inherited destroy;
end;

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
  Titles: IChartTitleConfiguration;
  DataFile: TEpiDataFile;
  XVar, YVar: TEpiField;
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
  Titles := FChartFactory.NewChartTitleConfiguration()
    .SetTitle(XVar.Question.Text + ' vs. ' + YVar.Question.Text)
    .SetFootnote('')
    .SetXAxisTitle(XVar.Question.Text)
    .SetYAxisTitle(YVar.Question.Text)
    .OverrideFromOptions(Command.Options);

  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart);
  Result.SetChartTitles(Chart, Titles);
end;

function TScatterChart.GetObject(): TObject;
begin
  Result := Self;
end;

initialization
  RegisterAbstractChartCommandClass(stScatter, TScatterChart);

end.

