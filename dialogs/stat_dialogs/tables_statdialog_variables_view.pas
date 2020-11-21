unit tables_statdialog_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls, ExtCtrls,
  StdCtrls;

type

  { TTableStatDialogVariablesView }

  TTableStatDialogVariablesView = class(TPanel, IStatDialogView)
  private
    FXVariableCombo: TComboBox;
    procedure UpdateView();
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView();
    function ExitView(): boolean;
    function GetControl(): TControl;
    function GetViewCaption: UTF8String;
  end;

implementation

{ TTableStatDialogVariablesView }

procedure TTableStatDialogVariablesView.UpdateView();
begin

end;

constructor TTableStatDialogVariablesView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FXVariableCombo := TComboBox.Create(TheOwner);
  FXVariableCombo.Parent := self;
  FXVariableCombo.AnchorParallel(akLeft, 10, Self);
  FXVariableCombo.AnchorParallel(akRight, 10, Self);
  FXVariableCombo.AnchorParallel(akTop, 10, Self);
end;

procedure TTableStatDialogVariablesView.EnterView();
begin
  UpdateView();
end;

function TTableStatDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TTableStatDialogVariablesView.GetControl(): TControl;
begin
  result := self;
end;

function TTableStatDialogVariablesView.GetViewCaption: UTF8String;
begin
  result := 'Variables';
end;

end.


