unit tables_statdialog_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  tables_statdialog_model;

type
  
  { TTableStatDialogContribution }

  TTableStatDialogContribution = class(IStatDialogContribution)
  private
    FVariablesModel: TTableStatDialogVariableModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  tables_statdialog_variables_view;

{ TTableStatDialogContribution }

function TTableStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TTableStatDialogVariablesView;
begin
  View := TTableStatDialogVariablesView.Create(Owner, Executor);
  FVariablesModel := TTableStatDialogVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TTableStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'tables ' +
    FVariablesModel.GenerateScript() +
    ';';
end;

function TTableStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Tables';
end;

function TTableStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.add(CreateMainView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TTableStatDialogContribution.Create);

end.

