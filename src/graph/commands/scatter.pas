unit scatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphcommand, TAGraph, executor, outputcreator,
  ast, epidatafiles;

type

  { TScatter }

  TScatter = class(IGraphCommand)
  private
    FChart: TChart;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IGraphCommandResult;
  end;

implementation

uses
  TASeries;

{ TScatter }

procedure TScatter.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChart := ChartFactory.NewChart();
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TScatter.Execute(Command: TCustomGraphCommand): IGraphCommandResult;
var
  line: TLineSeries;
  XVar, YVar: TEpiField;
  i: Integer;
begin
  line := TLineSeries.Create(FChart);
  line.ShowPoints := true;
  line.ShowLines := false;

  XVar := FExecutor.DataFile.Fields.FieldByName[command.VariableList[0].Ident];
  YVar := FExecutor.DataFile.Fields.FieldByName[command.VariableList[1].Ident];

  for i := 0 to Xvar.Size -1  do
    line.AddXY(xvar.AsFloat[i], yvar.AsFloat[i]);

  FChart.AddSeries(line);
end;

end.

