unit graphcommand;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, chartfactory, graphcommandresult, executor, outputcreator,
  ast, ast_types;

type
  // Implement this interface when adding new graph command
  IGraphCommand = interface['{B7C62325-DFF7-4E0B-A8B7-6D090DDFEA1A}']
    // Initialize the command object
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);

    // Called for doing the actual graph calculation. See eg. TScatter on how to use.
    function Execute(Command: TCustomGraphCommand): IGraphCommandResult;

    // Should always return the object itself. Used for freeing memory!
    function GetObject(): TObject;
  end;

  { TCustomGraphCommand }

  TAbstractGraphCommand = class(TObject, IGraphCommand)
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator); virtual; abstract;
    function Execute(Command: TCustomGraphCommand): IGraphCommandResult; virtual; abstract;
    function GetObject(): TObject; virtual; abstract;
  end;
  TAbstractGraphCommandClass = class of TAbstractGraphCommand;

// Register the
procedure RegisterAbstractGraphCommandClass(AStatementType: TASTStatementType; AGraphCommandClass: TAbstractGraphCommandClass);
function GetAbstractGraphCommandClass(AStatementType: TASTStatementType): TAbstractGraphCommandClass;

implementation

uses
  gmap;

type

  { TASTStatementTypeCompare }

  TASTStatementTypeCompare = class
    class function c(Left, Right: TASTStatementType): boolean;
  end;

  TGraphCommandClassMap = specialize TMap<TASTStatementType, TAbstractGraphCommandClass, TASTStatementTypeCompare>;

var
  GraphCommandClassMap: TGraphCommandClassMap;

{ TASTStatementTypeCompare }

class function TASTStatementTypeCompare.c(Left, Right: TASTStatementType): boolean;
begin
  result := Left < Right;
end;

procedure RegisterAbstractGraphCommandClass(AStatementType: TASTStatementType;
  AGraphCommandClass: TAbstractGraphCommandClass);
begin
  if (not Assigned(GraphCommandClassMap)) then
    GraphCommandClassMap := TGraphCommandClassMap.Create;

  GraphCommandClassMap.Insert(AStatementType, AGraphCommandClass);
end;

function GetAbstractGraphCommandClass(AStatementType: TASTStatementType
  ): TAbstractGraphCommandClass;
begin
  Result := nil;

  if (Assigned(GraphCommandClassMap)) then
    Result := GraphCommandClassMap.GetValue(AStatementType);
end;

end.

