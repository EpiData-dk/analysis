unit tables_statdialog_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor;

type

  { TTableStatDialgoModel }

  TTableStatDialogVariableModel = class
  private
    FExecutor: TExecutor;
  public
    constructor Create(Executor: TExecutor);
  end;

implementation

{ TTableStatDialgoModel }

constructor TTableStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

end.

