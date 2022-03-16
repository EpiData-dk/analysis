unit scatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartfactory, TAGraph, executor, outputcreator,
  ast, epidatafiles, TASources, TACustomSource;

type

  { TScatter }

  TScatter = class(IGraphCommand)
  private
    FChartFactory: IChartFactory;
    FDataFile: TEpiDataFile;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FXVar: TEpiField;
    FYVar: TEpiField;
    procedure GetScatterDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  public
    destructor destroy; override;
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IGraphCommandResult;
    function GetObject(): TObject;
  end;

implementation

uses
  TASeries, TATypes, Graphics;

{ TScatter }

procedure TScatter.GetScatterDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FXVar.AsFloat[AIndex];
  AItem.Y := FYVar.AsFloat[AIndex];
end;

destructor TScatter.destroy;
begin
  FDataFile.Free;
  inherited destroy;
end;

procedure TScatter.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TScatter.Execute(Command: TCustomGraphCommand): IGraphCommandResult;
var
  LineSeries: TLineSeries;
  ScatterSource: TUserDefinedChartSource;
  VarNames: TStrings;
  Chart: TChart;
  Titles: IChartTitleConfiguration;
begin
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;

  // Get the data and fields. Datafile is retained so we can free memory later
  FDataFile := FExecutor.PrepareDatafile(VarNames, VarNames);
  FXVar := FDataFile.Field[0];
  FYVar := FDataFile.Field[1];

  // Create the charts
  Chart := FChartFactory.NewChart();

  // Create our own datasource
  ScatterSource := TUserDefinedChartSource.Create(Chart);
  ScatterSource.OnGetChartDataItem := @GetScatterDataItem;
  ScatterSource.PointsNumber := FXVar.Size;

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
    .SetTitle(FXVar.Question.Text + ' vs. ' + FYVar.Question.Text)
    .SetFootnote('')
    .SetXAxisTitle(FXVar.Question.Text)
    .SetYAxisTitle(FYVar.Question.Text);

  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart);
  Result.SetChartTitles(Chart, Titles);
end;

function TScatter.GetObject(): TObject;
begin
  Result := Self;
end;

end.

