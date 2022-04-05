unit chartcommandresult;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, charttitles, fgl, TAGraph;

type
  TChartList = specialize TFPGObjectList<TChart>;

  IChartCommandResult = interface['{7FC61DE9-532D-45BC-A182-321B190EEB8A}']
    procedure AddChart(AChart: TChart);
    function GetCharts(): TChartList;
    procedure SetChartTitles(AChart: TChart; Titles: IChartTitles);
    function GetChartTitles(AChart: TChart): IChartTitles;
  end;

implementation

end.

