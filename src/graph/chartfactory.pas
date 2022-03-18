unit chartfactory;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, TAGraph, charttitles, graphcommandresult;

type
  IChartFactory = interface['{1701243D-0E60-427B-9142-35D22A4467E0}']
    function NewChart(): TChart;
    function NewGraphCommandResult(): IGraphCommandResult;
    function NewChartTitleConfiguration(): IChartTitleConfiguration;
  end;

var
  TheChartFactory: IChartFactory;

implementation

uses
  chartfactory.impl;

initialization
  TheChartFactory := TChartFactory.Create;

end.

