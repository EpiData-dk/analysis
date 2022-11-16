unit stat_dialog_action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog, stat_dialog_contribution, executor,
  StrHashMap;

type

  { TStatDialogFactory }

  TStatDialogFactory = class(TComponent)
  private
    FDialogMap: TStringHashMap;
    FExecutor: TExecutor;
    function CloseDialog(AUserData: PUserData; const AStr: string; var APtr: PData): Boolean;
    function ResetDialog(AUserData: PUserData; const AStr: string; var APtr: PData): Boolean;
    function RestoreDialogPos(AUserData: PUserData; const AStr: string; var APtr: PData): Boolean;
  protected
    procedure ShowDialog(Contribution: IStatDialogContribution);
  public
    constructor Create(AOwner: TComponent; Executor: TExecutor);
    function CreateDialogAction(Contribution: IStatDialogContribution): TBasicAction;
    procedure ResetAllDialogs();
    procedure CloseAllDialogs();
    procedure RestoreDefaultPos();
  end;

implementation

uses
  main;

type
  { TStatDialogAction }

  TStatDialogAction = class(TBasicAction)
  private
    FContribution: IStatDialogContribution;
    FFactory: TStatDialogFactory;
    procedure StartDialog(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; Contribution: IStatDialogContribution);
  end;


{ TStatDialogFactory }

function TStatDialogFactory.CloseDialog(AUserData: PUserData;
  const AStr: string; var APtr: PData): Boolean;
begin
  TStatDialog(APtr).Free;
  result := true;
end;

function TStatDialogFactory.ResetDialog(AUserData: PUserData;
  const AStr: string; var APtr: PData): Boolean;
begin
  TStatDialog(APtr).ResetViews();
  Result := true;
end;

function TStatDialogFactory.RestoreDialogPos(AUserData: PUserData;
  const AStr: string; var APtr: PData): Boolean;
var
  Dialog: TStatDialog;
begin
  Dialog := TStatDialog(APtr);
  TStatDialog.RestoreDefaultPos(Dialog);
  Result := True;
end;

procedure TStatDialogFactory.ShowDialog(Contribution: IStatDialogContribution);
var
  Dialog: TStatDialog;
begin
  Dialog := nil;

  if (not Assigned(FExecutor.DataFile)) then
    begin
      Mainform.InterfaceRunCommand('read;');

      if (not Assigned(FExecutor.DataFile)) then
        Exit;
    end;

  if (not FDialogMap.Find(Contribution.GetCaption(), Dialog)) then
    begin
      Dialog := TStatDialog.Create(Self, Contribution, FExecutor);
      Dialog.ScriptRunner := MainForm;
      FDialogMap.Add(Contribution.GetCaption(), Dialog);
    end
  else
    Dialog.ResetViews();
  Dialog.Show;
end;

constructor TStatDialogFactory.Create(AOwner: TComponent; Executor: TExecutor);
begin
  inherited Create(AOwner);
  FExecutor := Executor;
  FDialogMap := TStringHashMap.Create(1023, false);
end;

function TStatDialogFactory.CreateDialogAction(
  Contribution: IStatDialogContribution): TBasicAction;
begin
  Result := TStatDialogAction.Create(Self, Contribution);
  TStatDialogAction(Result).FFactory := Self;
end;

procedure TStatDialogFactory.ResetAllDialogs();
begin
  FDialogMap.IterateMethod(nil, @ResetDialog);
end;

procedure TStatDialogFactory.CloseAllDialogs();
begin
  FDialogMap.IterateMethod(nil, @CloseDialog);
  FDialogMap.Clear;
end;

procedure TStatDialogFactory.RestoreDefaultPos();
begin
  FDialogMap.IterateMethod(nil, @RestoreDialogPos);
end;

{ TStatDialogAction }

procedure TStatDialogAction.StartDialog(Sender: TObject);
begin
  FFactory.ShowDialog(FContribution);
end;

constructor TStatDialogAction.Create(AOwner: TComponent;
  Contribution: IStatDialogContribution);
begin
  inherited Create(AOwner);

  FContribution := Contribution;
  OnExecute := @StartDialog;
end;

end.

