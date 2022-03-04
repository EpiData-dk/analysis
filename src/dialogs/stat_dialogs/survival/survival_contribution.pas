unit survival_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  survival_model, survival_primaryoption_model,
  survival_statisticoptions_model;

type

  { TSurvivalStatDialogContribution }

  TSurvivalStatDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TSurvivalStatDialogPrimaryOptionModel;
    FStatisticsOptionsModel: TSurvivalStatDialogStatisticOptionsModel;
    FVariablesModel: TSurvivalStatDialogVariableModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreatePrimaryOptionView(Owner: TComponent;  Executor: TExecutor): IStatDialogView;
    function CreateStatisticOptionView(Owner: TComponent): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  survival_variables_view, survival_primaryoption_view,
  survival_statisticoptions_view;

{ TSurvivalStatDialogContribution }

function TSurvivalStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TSurvivalStatDialogVariablesView;
begin
  View := TSurvivalStatDialogVariablesView.Create(Owner);
  FVariablesModel := TSurvivalStatDialogVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TSurvivalStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TSurvivalStatPrimaryOptionsView;
begin
  View := TSurvivalStatPrimaryOptionsView.Create(Owner);
  FPrimaryOptionsModel := TSurvivalStatDialogPrimaryOptionModel.Create(Executor);

  View.SetModel(FPrimaryOptionsModel);

  Result := View;
end;

function TSurvivalStatDialogContribution.CreateStatisticOptionView(
  Owner: TComponent): IStatDialogView;
var
  View: TSurvivalStatDialogStatisticOptionsView;
begin
  View := TSurvivalStatDialogStatisticOptionsView.Create(Owner);
  FStatisticsOptionsModel := TSurvivalStatDialogStatisticOptionsModel.Create();

  View.SetModel(FStatisticsOptionsModel);

  Result := View;
end;

function TSurvivalStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'survival ' +
    FVariablesModel.GenerateScript() + ' ' +
    FPrimaryOptionsModel.GenerateScript() +
    FStatisticsOptionsModel.GenerateScript() +
    ';';
end;

function TSurvivalStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Survival';
end;

function TSurvivalStatDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TSurvivalStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  result.Add(CreateStatisticOptionView(Owner));
end;

initialization
  RegisterStatDialogContribution(TSurvivalStatDialogContribution.Create);

end.
