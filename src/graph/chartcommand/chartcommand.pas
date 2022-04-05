unit chartcommand;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, chartfactory, chartcommandresult, executor, outputcreator,
  ast, ast_types;

type
  // Implement this interface when adding new graph command
  IChartCommand = interface['{B7C62325-DFF7-4E0B-A8B7-6D090DDFEA1A}']
    // Initialize the command object
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);

    // Called for doing the actual graph calculation. See eg. TScatter on how to use.
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;

    // Should always return the object itself. Used for freeing memory!
    function GetObject(): TObject;
  end;

  { TCustomGraphCommand }

  TAbstractChartCommand = class(TObject, IChartCommand)
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator); virtual; abstract;
    function Execute(Command: TCustomGraphCommand): IChartCommandResult; virtual; abstract;
    function GetObject(): TObject; virtual; abstract;
  end;
  TAbstractChartCommandClass = class of TAbstractChartCommand;

// Register the
procedure RegisterAbstractChartCommandClass(AStatementType: TASTStatementType; AGraphCommandClass: TAbstractChartCommandClass);
function GetAbstractChartCommandClass(AStatementType: TASTStatementType): TAbstractChartCommandClass;

implementation

uses
  gmap;

type

  { TASTStatementTypeCompare }

  TASTStatementTypeCompare = class
    class function c(Left, Right: TASTStatementType): boolean;
  end;

  TGraphCommandClassMap = specialize TMap<TASTStatementType, TAbstractChartCommandClass, TASTStatementTypeCompare>;

var
  GraphCommandClassMap: TGraphCommandClassMap;

{ TASTStatementTypeCompare }

class function TASTStatementTypeCompare.c(Left, Right: TASTStatementType): boolean;
begin
  result := Left < Right;
end;

procedure RegisterAbstractChartCommandClass(AStatementType: TASTStatementType;
  AGraphCommandClass: TAbstractChartCommandClass);
begin
  if (not Assigned(GraphCommandClassMap)) then
    GraphCommandClassMap := TGraphCommandClassMap.Create;

  GraphCommandClassMap.Insert(AStatementType, AGraphCommandClass);
end;

function GetAbstractChartCommandClass(AStatementType: TASTStatementType
  ): TAbstractChartCommandClass;
begin
  Result := nil;

  if (Assigned(GraphCommandClassMap)) then
    Result := GraphCommandClassMap.GetValue(AStatementType);
end;

end.

