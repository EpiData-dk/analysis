unit stat_dialog_action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog, stat_dialog_contribution, executor;

type

  { TStatDialogAction }

  TStatDialogAction = class(TBasicAction)
  private
    FContribution: IStatDialogContribution;
    FExecutor: TExecutor;
    procedure StartDialog(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; Contribution: IStatDialogContribution;
      Executor: TExecutor);
  end;

implementation

uses
  main, UITypes;
{ TStatDialogAction }

procedure TStatDialogAction.StartDialog(Sender: TObject);
var
  Dialog: TStatDialog;
  Script: UTF8String;
  DialogResult: Integer;
begin
  // TODO: Should instead open a file
  if (not Assigned(FExecutor.DataFile)) then
    Exit;

  Dialog := TStatDialog.Create(Self, FContribution, FExecutor);
  DialogResult := Dialog.ShowModal;

  if (DialogResult = mrOK) then
    begin
      Script := FContribution.GenerateScript();
      MainForm.InterfaceRunCommand(Script);
    end;

  Dialog.Free;
end;

constructor TStatDialogAction.Create(AOwner: TComponent;
  Contribution: IStatDialogContribution; Executor: TExecutor);
begin
  inherited Create(AOwner);

  FContribution := Contribution;
  FExecutor := Executor;

  OnExecute := @StartDialog;
end;

end.

