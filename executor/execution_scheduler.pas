unit execution_scheduler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, executor, outputcreator;

type

  { TExecutionScheduler }

  TExecutionScheduler = class
  public
    constructor Create(AOutputCreator: TOutputCreator);
    procedure EnqueueProgram(TheProgram: TStatementList);
  end;

implementation

{ TExecutionScheduler }

constructor TExecutionScheduler.Create(AOutputCreator: TOutputCreator);
begin
  FExecutor := TExecutor.Create(AOutputCreator);
end;

procedure TExecutionScheduler.EnqueueProgram(TheProgram: TStatementList);
begin

end;

end.

