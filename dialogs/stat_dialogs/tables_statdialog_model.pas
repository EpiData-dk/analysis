unit tables_statdialog_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epidatafiles;

type

  { TTableStatDialogVariableModel }

  TTableStatDialogVariableModel = class
  private
    FByVariableA: TEpiField;
    FByVariableB: TEpiField;
    FByVariableC: TEpiField;
    FExecutor: TExecutor;
    FXVariable: TEpiField;
    FYVariable: TEpiField;
    procedure SetByVariableA(AValue: TEpiField);
    procedure SetByVariableB(AValue: TEpiField);
    procedure SetByVariableC(AValue: TEpiField);
    procedure SetXVariable(AValue: TEpiField);
    procedure SetYVariable(AValue: TEpiField);
  public
    constructor Create(Executor: TExecutor);
    property XVariable: TEpiField read FXVariable write SetXVariable;
    property YVariable: TEpiField read FYVariable write SetYVariable;
    property ByVariableA: TEpiField read FByVariableA write SetByVariableA;
    property ByVariableB: TEpiField read FByVariableB write SetByVariableB;
    property ByVariableC: TEpiField read FByVariableC write SetByVariableC;
  end;

implementation

{ TTableStatDialgoModel }

procedure TTableStatDialogVariableModel.SetByVariableA(AValue: TEpiField);
begin
  if FByVariableA = AValue then Exit;
  FByVariableA := AValue;
end;

procedure TTableStatDialogVariableModel.SetByVariableB(AValue: TEpiField);
begin
  if FByVariableB = AValue then Exit;
  FByVariableB := AValue;
end;

procedure TTableStatDialogVariableModel.SetByVariableC(AValue: TEpiField);
begin
  if FByVariableC = AValue then Exit;
  FByVariableC := AValue;
end;

procedure TTableStatDialogVariableModel.SetXVariable(AValue: TEpiField);
begin
  if FXVariable = AValue then Exit;
  FXVariable := AValue;
end;

procedure TTableStatDialogVariableModel.SetYVariable(AValue: TEpiField);
begin
  if FYVariable = AValue then Exit;
  FYVariable := AValue;
end;

constructor TTableStatDialogVariableModel.Create(Executor: TExecutor);
begin
  FExecutor := Executor;
end;

end.

