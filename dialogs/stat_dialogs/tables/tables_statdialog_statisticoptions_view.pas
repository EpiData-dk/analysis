unit tables_statdialog_statisticoptions_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls,
  Controls, tables_statdialog_statisticoptions_model;

type

  { TTableStatDialogStatisticOptionsView }

  TTableStatDialogStatisticOptionsView = class(TPanel, IStatDialogView)
  private
    FDataModel: TTableStatDialogStatisticOptionsModel;
    FOnModified: IStatDiaglogViewModified;
  protected
    procedure DoModified();
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView();
    function ExitView(): boolean;
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    function IsDefined(): boolean;
    procedure ResetView();
    procedure SetModel(DataModel: TTableStatDialogStatisticOptionsModel);
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
  end;

implementation

{ TTableStatDialogStatisticOptionsView }

procedure TTableStatDialogStatisticOptionsView.DoModified();
begin
  if (Assigned(FOnModified)) then
    FOnModified.OnViewModified(Self);
end;

constructor TTableStatDialogStatisticOptionsView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Statistics Options';
end;

procedure TTableStatDialogStatisticOptionsView.EnterView();
begin
  //
end;

function TTableStatDialogStatisticOptionsView.ExitView(): boolean;
begin
  result := true;
end;

function TTableStatDialogStatisticOptionsView.GetControl(): TControl;
begin
  result := self;
end;

function TTableStatDialogStatisticOptionsView.GetViewCaption(): UTF8String;
begin
  result := 'Statistics'
end;

function TTableStatDialogStatisticOptionsView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TTableStatDialogStatisticOptionsView.ResetView();
begin
  //
end;

procedure TTableStatDialogStatisticOptionsView.SetModel(
  DataModel: TTableStatDialogStatisticOptionsModel);
begin
  FDataModel := DataModel;
end;

procedure TTableStatDialogStatisticOptionsView.SetOnModified(
  OnModified: IStatDiaglogViewModified);
begin
  FOnModified := OnModified;
end;

end.

