unit stat_dialog_action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog, stat_dialog_contribution, executor, fgl,
  StrHashMap;

type
  { TStatDialogAction }

  TStatDialogAction = class(TBasicAction)
  private
    class var FDialogMap: TStringHashMap;
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
  main;

{ TStatDialogAction }

procedure TStatDialogAction.StartDialog(Sender: TObject);
var
  Dialog: TStatDialog;
  Index: Integer;
begin
  // TODO: Should instead open a file
  if (not Assigned(FExecutor.DataFile)) then
    Exit;

  Dialog := nil;
  if (not FDialogMap.Find(FContribution.GetCaption(), Dialog)) then
    begin
      Dialog := TStatDialog.Create(Self, FContribution, FExecutor);
      Dialog.ScriptRunner := MainForm;
      FDialogMap.Add(FContribution.GetCaption(), Dialog);
    end;

  Dialog.Show;
end;

constructor TStatDialogAction.Create(AOwner: TComponent;
  Contribution: IStatDialogContribution; Executor: TExecutor);
begin
  inherited Create(AOwner);

  if (not Assigned(FDialogMap)) then
    FDialogMap := TStringHashMap.Create();

  FContribution := Contribution;
  FExecutor := Executor;

  OnExecute := @StartDialog;
end;

end.

