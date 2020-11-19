unit tables_statdialog_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls;

type

  { TTableStatDialogView }

  TTableStatDialogView = class(IStatDialogView)
  public
    procedure enterView();
    function exitView(): boolean;
    function getControl(Owner: TComponent): TControl;
    function getViewCaption: UTF8String;
  end;

implementation

uses
  ExtCtrls;

{ TTableStatDialogView }

procedure TTableStatDialogView.enterView();
begin
  //
end;

function TTableStatDialogView.exitView(): boolean;
begin
  result := true;
end;

function TTableStatDialogView.getControl(Owner: TComponent): TControl;
begin
  result := TPanel.Create(Owner);
  result.Caption := 'TADA';
end;

function TTableStatDialogView.getViewCaption: UTF8String;
begin
  result := 'Variables';
end;

end.


