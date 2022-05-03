unit chartfactory;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, TAGraph, charttitles, chartcommandresult, chartconfiguration;

type
  IChartFactory = interface['{1701243D-0E60-427B-9142-35D22A4467E0}']
    // Create a new chart. A chart is series agnostic, so any kind of data can be applied
    // - Add the new chart to the commandresult in order to display/save it.
    function NewChart(): TChart;

    // Create a new i CommandResult object to transfer data from the command to
    // the graph execution unit.
    function NewGraphCommandResult(): IChartCommandResult;

    function NewChartConfiguration(): IChartConfiguration;

    // Create a new Title Configuration to store char title information.
    // - Add the new configuration to the commandresult, together with the chart itself
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

