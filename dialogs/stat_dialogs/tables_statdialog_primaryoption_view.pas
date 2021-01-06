unit tables_statdialog_primaryoption_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, stat_dialog_contribution;

type

  { TTableStatPrimaryOptionsView }

  TTableStatPrimaryOptionsView = class(TPanel, IStatDialogView)
  private
    FOnModified: IStatDiaglogViewModified;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView();
    function ExitView(): boolean;
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    procedure ResetView();
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
  end;

implementation

uses
  StdCtrls;

{ TTableStatPrimaryOptionsView }

constructor TTableStatPrimaryOptionsView.Create(TheOwner: TComponent);
var
  CheckBox: TCheckBox;
  PrevControl: TControl;
begin
  inherited Create(TheOwner);

  CheckBox := TCheckBox.Create(Self);
  CheckBox.Parent := self;
  CheckBox.Caption := 'Row';
  CheckBox.Anchors := [];
  CheckBox.AnchorParallel(akTop, 5, Self);
  CheckBox.AnchorParallel(akLeft, 5, Self);
  PrevControl := CheckBox;

  CheckBox := TCheckBox.Create(Self);
  CheckBox.Parent := self;
  CheckBox.Caption := 'Column';
  CheckBox.Anchors := [];
  CheckBox.AnchorToNeighbour(akTop, 5, PrevControl);
  CheckBox.AnchorParallel(akLeft, 5, Self);
end;

procedure TTableStatPrimaryOptionsView.EnterView();
begin
  //
end;

function TTableStatPrimaryOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TTableStatPrimaryOptionsView.GetControl(): TControl;
begin
  result := self;
end;

function TTableStatPrimaryOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Options';
end;

procedure TTableStatPrimaryOptionsView.ResetView();
begin

end;

procedure TTableStatPrimaryOptionsView.SetOnModified(
  OnModified: IStatDiaglogViewModified);
begin
  FOnModified := OnModified;
end;

end.

