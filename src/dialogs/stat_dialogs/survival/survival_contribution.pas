unit survival_contribution;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  survival_model, survival_primaryoption_model,survival_statisticoptions_model,
  chart_options_model;

type

  { TSurvivalDialogContribution }

  TSurvivalDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TSurvivalDialogPrimaryOptionModel;
    FStatisticsOptionsModel: TSurvivalDialogStatisticOptionsModel;
    FVariablesModel: TSurvivalDialogVariableModel;
    FChartOptionsModel: TChartOptionsModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreatePrimaryOptionView(Owner: TComponent;  Executor: TExecutor): IStatDialogView;
    function CreateStatisticOptionView(Owner: TComponent): IStatDialogView;
    function CreateChartOptionsView(Owner: TComponent): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  survival_variables_view, survival_primaryoption_view,
  survival_statisticoptions_view, chart_options_view;

{ TSurvivalDialogContribution }

function TSurvivalDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TSurvivalDialogVariablesView;
begin
  FVariablesModel := TSurvivalDialogVariableModel.Create(Executor);
  View := TSurvivalDialogVariablesView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function TSurvivalDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TSurvivalStatPrimaryOptionsView;
begin
  FPrimaryOptionsModel := TSurvivalDialogPrimaryOptionModel.Create(Executor);
  View := TSurvivalStatPrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function TSurvivalDialogContribution.CreateStatisticOptionView(
  Owner: TComponent): IStatDialogView;
var
  View: TSurvivalDialogStatisticOptionsView;
begin
  View := TSurvivalDialogStatisticOptionsView.Create(Owner);
  FStatisticsOptionsModel := TSurvivalDialogStatisticOptionsModel.Create();
  View.SetModel(FStatisticsOptionsModel);
  Result := View;
end;

function TSurvivalDialogContribution.CreateChartOptionsView(Owner: TComponent): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create();
  FChartOptionsModel.MinMax := [];
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;
function TSurvivalDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'survival ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FStatisticsOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function TSurvivalDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Survival';
end;

function TSurvivalDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TSurvivalDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  result.Add(CreateStatisticOptionView(Owner));
  result.Add(CreateChartOptionsView(Owner));
end;

initialization
  RegisterStatDialogContribution(TSurvivalDialogContribution.Create, cdGraphs);

end.
