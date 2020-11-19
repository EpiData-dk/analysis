unit stat_dialog_action;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog, stat_dialog_contribution;

type

  { TStatDialogAction }

  TStatDialogAction = class(TBasicAction)
  private
    FContribution: IStatDialogContribution;
    procedure StartDialog(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; Contribution: IStatDialogContribution);
  end;

implementation

{ TStatDialogAction }

procedure TStatDialogAction.StartDialog(Sender: TObject);
var
  dialog: TStatDialog;
begin
  dialog := TStatDialog.Create(Self, FContribution);
  dialog.ShowModal;
  dialog.Free;
end;

constructor TStatDialogAction.Create(AOwner: TComponent;
  Contribution: IStatDialogContribution);
begin
  inherited Create(AOwner);
  FContribution := Contribution;
  OnExecute := @StartDialog;
end;

end.

