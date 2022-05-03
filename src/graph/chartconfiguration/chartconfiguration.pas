unit chartconfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, charttitles, chartaxesconfiguration;

type
  IChartConfiguration = interface['{B629C98B-BCD8-4C80-8E83-DE6A7A8803A8}']
    function GetTitleConfiguration(): IChartTitleConfiguration;
    function GetAxesConfiguration(): IChartAxesConfiguration;
  end;

implementation

end.

