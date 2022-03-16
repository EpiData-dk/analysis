unit chartfactory;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, TAGraph, ast, executor, outputcreator, forms, fgl;

type
  TChartList = specialize TFPGObjectList<TChart>;

  IChartFactory = interface;
  IChartTitles = interface;
  IChartTitleConfiguration = interface;
  IGraphForm = interface;
  IGraphFormFactory = interface;
  IGraphCommandResult = interface;
  IGraphCommand = interface;

  IChartFactory = interface['{1701243D-0E60-427B-9142-35D22A4467E0}']
    function NewChart(): TChart;
    function NewGraphCommandResult(): IGraphCommandResult;
    function NewChartTitleConfiguration(): IChartTitleConfiguration;
  end;

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

  IGraphForm = interface['{9E9A5CBB-717A-49DC-8D31-EB00018EA376}']
    procedure SetCommandResult(ACommandResult: IGraphCommandResult);
    function GetForm: TCustomForm;
  end;

  IGraphFormFactory = interface['{60911789-540E-46EF-8D5E-E047A066BD27}']
    function NewGraphForm(): IGraphForm;
    procedure CloseAllOpenForms();
  end;

  IGraphCommandResult = interface['{7FC61DE9-532D-45BC-A182-321B190EEB8A}']
    procedure AddChart(AChart: TChart);
    function GetCharts(): TChartList;
    procedure SetChartTitles(AChart: TChart; Titles: IChartTitles);
    function GetChartTitles(AChart: TChart): IChartTitles;
  end;

  IGraphCommand = interface['{B7C62325-DFF7-4E0B-A8B7-6D090DDFEA1A}']
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IGraphCommandResult;
    function GetObject(): TObject;
  end;

var
  TheChartFactory: IChartFactory;

implementation

end.

