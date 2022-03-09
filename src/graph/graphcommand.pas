unit graphcommand;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, TAGraph, ast, executor, outputcreator, forms;

type
  IChartFactory = interface['{1701243D-0E60-427B-9142-35D22A4467E0}']
    function NewChart: TChart;
  end;

  IGraphForm = interface['{9E9A5CBB-717A-49DC-8D31-EB00018EA376}']
    function GetChartFactory: IChartFactory;
    function GetForm: TCustomForm;
  end;

  IGraphFormFactory = interface['{60911789-540E-46EF-8D5E-E047A066BD27}']
    function NewGraphForm(): IGraphForm;
    procedure CloseAllOpenForms();
  end;

  IGraphCommandResult = interface['{7FC61DE9-532D-45BC-A182-321B190EEB8A}']
  end;

  IGraphCommand = interface['{B7C62325-DFF7-4E0B-A8B7-6D090DDFEA1A}']
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IGraphCommandResult;
    function GetObject(): TObject;
  end;

implementation

end.

