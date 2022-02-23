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

    // Create a new Chart Configuration. Look at the configuration itself for
    // more information on how/what to configure
    // - Add the new configuration to the commandresult, together with the chart itself
    function NewChartConfiguration(): IChartConfiguration;
  end;

var
  TheChartFactory: IChartFactory;

implementation

uses
  chartfactory.impl;

initialization
  TheChartFactory := TChartFactory.Create;

end.

