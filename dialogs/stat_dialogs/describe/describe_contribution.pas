unit describe_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  describe_variable_model, describe_primaryoption_model,
  describe_statisticoptions_model, common_select_model;

type

  { TDescribeContribution }

  TDescribeContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TDescribePrimaryOptionModel;
    FStatisticsOptionsModel: TDescribeStatisticOptionsModel;
    FVariablesModel: TDescribeVariableModel;
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
  describe_variable_view, describe_primaryoption_view,
  describe_statisticoptions_view, common_select_view;

{ TDescribeContribution }

function TDescribeContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TDescribeVariablesView;
begin
  View := TDescribeVariablesView.Create(Owner);
  FVariablesModel := TDescribeVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TDescribeContribution.CreatePrimaryOptionView(Owner: TComponent
  ): IStatDialogView;
var
  View: TDescribeStatPrimaryOptionsView;
begin
  View := TDescribeStatPrimaryOptionsView.Create(Owner);
  FPrimaryOptionsModel := TDescribePrimaryOptionModel.Create();

  View.SetModel(FPrimaryOptionsModel);

  Result := View;
end;

function TDescribeContribution.CreateStatisticOptionView(
  Owner: TComponent): IStatDialogView;
var
  View: TDescribeStatisticOptionsView;
begin
  View := TDescribeStatisticOptionsView.Create(Owner);
  FStatisticsOptionsModel := TDescribeStatisticOptionsModel.Create();

  View.SetModel(FStatisticsOptionsModel);

  Result := View;
end;

function TDescribeContribution.CreateSelectView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TCommonSelectView;
begin
  View := TCommonSelectView.Create(Owner);
  FSelectModel := TCommonSelectModel.Create(Executor);

  View.SetModel(FSelectModel);

  Result := View;
end;

function TDescribeContribution.GenerateScript(): UTF8String;
begin
  result :=
    FSelectModel.GenerateScript() + ' ' +
    'describe ' +
    FVariablesModel.GenerateScript() + ' ' +
    FPrimaryOptionsModel.GenerateScript() + ' ' +
    FStatisticsOptionsModel.GenerateScript() +
    ';';
end;

function TDescribeContribution.GetCaption(): UTF8String;
begin
  result := 'Describe';
end;

function TDescribeContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Choose variables' + LineEnding +
    '2: Choose statistics (default is mean, sd, minimum, median, maximum)' + LineEnding +
    '3: Choose label options (default is labels only for variables and values)' + LineEnding +
    '4: Select a subset of data (optional)' + LineEnding +
    '5: Run (close this form), Execute (keep this form open) or Paste command';
end;

function TDescribeContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;

begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreateStatisticOptionView(Owner));
  result.Add(CreatePrimaryOptionView(Owner));
  result.Add(CreateSelectView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TDescribeContribution.Create);

end.
