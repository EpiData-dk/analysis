unit scatter_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor,
  scatter_variables_model, scatter_primaryoption_model, chart_options_model;

type

  { TScatterStatDialogContribution }

  TScatterStatDialogContribution = class(IStatDialogContribution)
  private
    FVariablesModel: TScatterStatVariableModel;
    FPrimaryOptionsModel: TScatterStatDialogPrimaryOptionModel;
    FChartOptionsModel: TChartOptionsModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreatePrimaryOptionView(Owner: TComponent;  Executor: TExecutor): IStatDialogView;
    function CreateChartOptionsView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  scatter_main_view,
  scatter_primaryoption_view,
  chart_options_view;

{ TScatterStatDialogContribution }

function TScatterStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TScatterMainDialogView;
begin
  View := TScatterMainDialogView.Create(Owner);
  FVariablesModel := TScatterStatVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TScatterStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TScatterStatPrimaryOptionsView;
begin
  FPrimaryOptionsModel := TScatterStatDialogPrimaryOptionModel.Create(Executor);
  View := TScatterStatPrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function TScatterStatDialogContribution.CreateChartOptionsView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create(Executor);
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function TScatterStatDialogContribution.GenerateScript(): UTF8String;
begin
  Result := 'scatter ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function TScatterStatDialogContribution.GetCaption(): UTF8String;
begin
  Result := 'Scatter';
end;

function TScatterStatDialogContribution.GetHelpText(): UTF8String;
begin
  Result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TScatterStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  Result := TStatDialogContributionViewList.Create;
  Result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  result.Add(CreateChartOptionsView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TScatterStatDialogContribution.Create, cdGraphs);

end.
