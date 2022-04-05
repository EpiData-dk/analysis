unit graphcommandexecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ast, executor, chartcommandresult, graphform,
  outputcreator;

type

  { TGraphCommandExecutor }

  TGraphCommandExecutor = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    procedure Execute(ST: TCustomGraphCommand);
  end;

implementation

uses
  chartcommand, chartfactory, graphformfactory, savegraphaction;

{ TGraphCommandExecutor }

constructor TGraphCommandExecutor.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

procedure TGraphCommandExecutor.Execute(ST: TCustomGraphCommand);
var
  Command: TAbstractChartCommand;
  CommandResult: IChartCommandResult;
  GraphForm: IGraphForm;
  Form: TCustomForm;
  Opt: TOption;
  SaveAction: TSaveGraphAction;
begin
  Command := GetAbstractChartCommandClass(ST.StatementType).Create;
  Command.Init(TheChartFactory, FExecutor, FOutputCreator);
  CommandResult := Command.Execute(ST);

  if (ST.HasOption(['export', 'S', 'E'], Opt)) then
    begin
      SaveAction := TSaveGraphAction.Create(nil);
      SaveAction.Chart    := CommandResult.GetCharts().First;
      SaveAction.Filename := Opt.Expr.AsString;
      SaveAction.Execute;

      FOutputCreator.DoInfoAll('Graph saved as: ' + SaveAction.Filename);

      SaveAction.Free;
    end
  else
    begin
      GraphForm := TheGraphFormFactory.NewGraphForm();
      GraphForm.SetCommandResult(CommandResult);
      Form := GraphForm.GetForm;
      Form.Show;
    end;

  Command.GetObject().Free;
end;

end.

