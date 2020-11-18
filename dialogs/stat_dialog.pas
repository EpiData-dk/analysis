unit stat_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, stat_dialog_contribution;

type

  { TStatDialog }

  TStatDialog = class(TForm)
  private
    FContribution: IStatDialogContribution;

  public
    constructor Create(TheOwner: TComponent; Contribution: IStatDialogContribution);
  end;

var
  StatDialog: TStatDialog;

implementation

{$R *.lfm}

{ TStatDialog }

constructor TStatDialog.Create(TheOwner: TComponent;
  Contribution: IStatDialogContribution);
begin
  FContribution := IStatDialogContribution;
end;

end.

