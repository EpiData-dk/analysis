unit epicurve;

{$mode objfpc}{$H+}

interface
// epicurve is a synonym for histogram
// histogram can detect how it was invoked and behave accordingly
uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, ast,
  chartcommand, chartfactory, histogram;

type

  { TEpicurveChart }

  TEpicurveChart = class(TInterfacedObject, IChartCommand)
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  ast_types;

{ TEpicurveChart }

procedure TEpicurveChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin

end;

function TEpicurveChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
begin
  result := nil; // function will never be called in any case
end;

initialization
  RegisterChartCommand(stEpicurve, THistogramChart); //TEpicurveChart);

end.
