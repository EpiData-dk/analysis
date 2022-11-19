unit pareto_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, pareto_variables_model;

type

  { TParetoStatDialogContribution }

  TParetoStatDialogContribution = class(IStatDialogContribution)
  private
    FVariablesModel: TParetoStatVariableModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  pareto_main_view;

{ TParetoStatDialogContribution }

function TParetoStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TParetoMainDialogView;
begin
  View := TParetoMainDialogView.Create(Owner);
  FVariablesModel := TParetoStatVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TParetoStatDialogContribution.GenerateScript(): UTF8String;
begin
  Result := 'pareto ' +
    FVariablesModel.GenerateScript() +
    ';';
end;

function TParetoStatDialogContribution.GetCaption(): UTF8String;
begin
  Result := 'Pareto';
end;

function TParetoStatDialogContribution.GetHelpText(): UTF8String;
begin
  Result :=
    '1: Select Variables' + LineEnding;
end;

function TParetoStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  Result := TStatDialogContributionViewList.Create;
  Result.Add(CreateMainView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TParetoStatDialogContribution.Create, cdGraphs);

end.
