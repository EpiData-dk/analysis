unit ctable_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  ctable_model, ctable_primaryoption_model,
  ctable_statisticoptions_model, common_select_model;

type

  { TCtableStatDialogContribution }

  TCtableStatDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TCtableStatDialogPrimaryOptionModel;
    FStatisticsOptionsModel: TCtableStatDialogStatisticOptionsModel;
    FVariablesModel: TCtableStatDialogVariableModel;
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
  ctable_variables_view, ctable_primaryoption_view,
  ctable_statisticoptions_view, common_select_view;

{ TCtableStatDialogContribution }

function TCtableStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TCtableStatDialogVariablesView;
begin
  View := TCtableStatDialogVariablesView.Create(Owner);
  FVariablesModel := TCtableStatDialogVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TCtableStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent
  ): IStatDialogView;
var
  View: TCtableStatPrimaryOptionsView;
begin
  View := TCtableStatPrimaryOptionsView.Create(Owner);
  FPrimaryOptionsModel := TCtableStatDialogPrimaryOptionModel.Create();

  View.SetModel(FPrimaryOptionsModel);

  Result := View;
end;

function TCtableStatDialogContribution.CreateStatisticOptionView(
  Owner: TComponent): IStatDialogView;
var
  View: TCtableStatDialogStatisticOptionsView;
begin
  View := TCtableStatDialogStatisticOptionsView.Create(Owner);
  FStatisticsOptionsModel := TCtableStatDialogStatisticOptionsModel.Create();

  View.SetModel(FStatisticsOptionsModel);

  Result := View;
end;

function TCtableStatDialogContribution.CreateSelectView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TCommonSelectView;
begin
  View := TCommonSelectView.Create(Owner);
  FSelectModel := TCommonSelectModel.Create(Executor);

  View.SetModel(FSelectModel);

  Result := View;
end;

function TCtableStatDialogContribution.GenerateScript(): UTF8String;
begin
  result :=
    FSelectModel.GenerateScript() + ' ' +
    'ctable ' +
    FVariablesModel.GenerateScript() + ' ' +
    FPrimaryOptionsModel.GenerateScript() +
    FStatisticsOptionsModel.GenerateScript() +
    ';';
end;

function TCtableStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Compact Tables';
end;

function TCtableStatDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Click percentage, test, sorting etc.' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TCtableStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner));
  result.Add(CreateStatisticOptionView(Owner));
  result.Add(CreateSelectView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TCtableStatDialogContribution.Create);

end.
