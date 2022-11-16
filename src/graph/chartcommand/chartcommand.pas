unit chartcommand;

{$mode objfpc}{$H+}

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
  end;

  ERegisterChartException = class(Exception);

// Register the
procedure RegisterChartCommand(AStatementType: TASTStatementType; AGraphCommandClass: TInterfacedClass);
function GetChartCommand(AStatementType: TASTStatementType): IChartCommand;

implementation

uses
  gmap;

type

  { TASTStatementTypeCompare }

  TASTStatementTypeCompare = class
    class function c(Left, Right: TASTStatementType): boolean;
  end;

  TGraphCommandClassMap = specialize TMap<TASTStatementType, TInterfacedClass, TASTStatementTypeCompare>;

var
  GraphCommandClassMap: TGraphCommandClassMap;

{ TASTStatementTypeCompare }

class function TASTStatementTypeCompare.c(Left, Right: TASTStatementType): boolean;
begin
  result := Left < Right;
end;

procedure RegisterChartCommand(AStatementType: TASTStatementType;
  AGraphCommandClass: TInterfacedClass);
var
  S: String;
begin
  if (not (AStatementType in ASTGraphCommands)) then
    begin
      WriteStr(S, AStatementType);
      Raise ERegisterChartException.Create('AStatementType: "' + S + '" is not a graph enum!');
    end;

  if (not Supports(AGraphCommandClass, IChartCommand)) then
    Raise ERegisterChartException.Create('AGraphCommandClass: "' + AGraphCommandClass.ClassName + '" does not implement the IChartCommand interface!');

  if (not Assigned(GraphCommandClassMap)) then
    GraphCommandClassMap := TGraphCommandClassMap.Create;

  GraphCommandClassMap.Insert(AStatementType, AGraphCommandClass);
end;

function GetChartCommand(AStatementType: TASTStatementType
  ): IChartCommand;
begin
  Result := nil;
  if (Assigned(GraphCommandClassMap)) then
    Result := GraphCommandClassMap.GetValue(AStatementType).Create as IChartCommand;
end;

end.
