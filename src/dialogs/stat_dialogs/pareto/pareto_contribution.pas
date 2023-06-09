unit pareto_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor,
  pareto_variables_model, chart_options_model;

type

  { TParetoStatDialogContribution }

  TParetoStatDialogContribution = class(IStatDialogContribution)
  private
    FVariablesModel: TParetoStatVariableModel;
    FChartOptionsModel: TChartOptionsModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
    function CreateChartOptionsView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  end;

implementation

uses
  pareto_main_view,
  chart_options_view;

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

function TParetoStatDialogContribution.CreateChartOptionsView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create();
  FChartOptionsModel.MinMax := [];
  FChartOptionsModel.UseY := false;
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function TParetoStatDialogContribution.GenerateScript(): UTF8String;
begin
  Result := 'pareto ' +
    FVariablesModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
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
  result.Add(CreateChartOptionsView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TParetoStatDialogContribution.Create, cdGraphs);

end.
