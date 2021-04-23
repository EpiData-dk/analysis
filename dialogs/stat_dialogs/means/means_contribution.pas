unit means_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  means_model, means_primaryoption_model,
  means_statisticoptions_model, common_select_model;

type

  { TMeansStatDialogContribution }

  TMeansStatDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TMeansStatDialogPrimaryOptionModel;
    FStatisticsOptionsModel: TMeansStatDialogStatisticOptionsModel;
    FVariablesModel: TMeansStatDialogVariableModel;
    FSelectModel: TCommonSelectModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreatePrimaryOptionView(Owner: TComponent): IStatDialogView;
    function CreateStatisticOptionView(Owner: TComponent): IStatDialogView;
    function CreateSelectView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  means_variables_view, means_primaryoption_view,
  means_statisticoptions_view, common_select_view;

{ TMeansStatDialogContribution }

function TMeansStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TMeansStatDialogVariablesView;
begin
  View := TMeansStatDialogVariablesView.Create(Owner);
  FVariablesModel := TMeansStatDialogVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TMeansStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent
  ): IStatDialogView;
var
  View: TMeansStatPrimaryOptionsView;
begin
  View := TMeansStatPrimaryOptionsView.Create(Owner);
  FPrimaryOptionsModel := TMeansStatDialogPrimaryOptionModel.Create();

  View.SetModel(FPrimaryOptionsModel);

  Result := View;
end;

function TMeansStatDialogContribution.CreateStatisticOptionView(
  Owner: TComponent): IStatDialogView;
var
  View: TMeansStatDialogStatisticOptionsView;
begin
  View := TMeansStatDialogStatisticOptionsView.Create(Owner);
  FStatisticsOptionsModel := TMeansStatDialogStatisticOptionsModel.Create();

  View.SetModel(FStatisticsOptionsModel);

  Result := View;
end;

function TMeansStatDialogContribution.CreateSelectView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TCommonSelectView;
begin
  View := TCommonSelectView.Create(Owner);
  FSelectModel := TCommonSelectModel.Create(Executor);

  View.SetModel(FSelectModel);

  Result := View;
end;

function TMeansStatDialogContribution.GenerateScript(): UTF8String;
begin
  result :=
    FSelectModel.GenerateScript() + ' ' +
    'means ' +
    FVariablesModel.GenerateScript() + ' ' +
    FPrimaryOptionsModel.GenerateScript() +
    FStatisticsOptionsModel.GenerateScript() +
    ';';
end;

function TMeansStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Means';
end;

function TMeansStatDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Click percentage, test, sorting etc.' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TMeansStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner));
  result.Add(CreateStatisticOptionView(Owner));
  result.Add(CreateSelectView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TMeansStatDialogContribution.Create);

end.

