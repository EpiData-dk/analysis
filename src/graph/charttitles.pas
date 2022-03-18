unit charttitles;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils;

type
  IChartTitles = interface['{4978DB6A-4587-4537-8987-CB26C98ECFFA}']
    function GetTitle(): UTF8String;
    function GetFootnote(): UTF8String;
    function GetXAxisTitle(): UTF8String;
    function GetYAxisTitle(): UTF8String;
  end;

  IChartTitleConfiguration = interface(IChartTitles)['{4DCBD46E-C9EB-46A4-9313-7B3C943F7B82}']
    function SetTitle(Text: UTF8String): IChartTitleConfiguration;
    function SetFootnote(Text: UTF8String): IChartTitleConfiguration;
    function SetXAxisTitle(Text: UTF8String): IChartTitleConfiguration;
    function SetYAxisTitle(Text: UTF8String): IChartTitleConfiguration;
  end;

implementation

end.

