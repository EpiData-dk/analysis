unit chartcommandresult;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, chartconfiguration, chartpair;

type

  { IChartCommandResult }

  IChartCommandResult = interface['{7FC61DE9-532D-45BC-A182-321B190EEB8A}']
    procedure AddChart(Pair: TChartPair);
    procedure AddChart(AChart: TChart; Configuration: IChartConfiguration = nil);
    function GetChartPairs(): TChartPairList;
  end;

implementation

end.

