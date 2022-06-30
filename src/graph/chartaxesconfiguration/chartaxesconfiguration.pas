unit chartaxesconfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  IChartAxisConfiguration = interface['{0C0B2630-40CA-4FA1-B3D1-9497DE34F396}']
    function SetShowAxisMarksAsDates(Value: boolean): IChartAxisConfiguration;
    function GetShowAxisMarksAsDates: boolean;
  end;

  IChartAxesConfiguration = interface['{D061E5F7-4767-4FE0-9A56-45CB88128E36}']
    function GetXAxisConfiguration(): IChartAxisConfiguration;
    function GetYAxisConfiguration(): IChartAxisConfiguration;
  end;

implementation

end.
