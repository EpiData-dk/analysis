unit scatter_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, scatter_variables_model,
  chart_options_model;

type

  { TScatterStatDialogContribution }

  TScatterStatDialogContribution = class(IStatDialogContribution)
  private
    FVariablesModel: TScatterStatVariableModel;
    FChartOptionsModel: TChartOptionsModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreateChartOptionsView(Owner: TComponent): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  scatter_main_view, chart_options_view;

{ TScatterStatDialogContribution }

function TScatterStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TScatterMainDialogView;
begin
  FVariablesModel := TScatterStatVariableModel.Create(Executor);
  View := TScatterMainDialogView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function TScatterStatDialogContribution.CreateChartOptionsView(Owner: TComponent): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create();
  FChartOptionsModel.MinMax := [mmtXMin, mmtXMax, mmtYMin, mmtYMax];
  // Variables model must be aware of chart options model, to set X and Y variables
  FVariablesModel.ChartOptions := FChartOptionsModel;
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function TScatterStatDialogContribution.GenerateScript(): UTF8String;
begin
  Result := 'scatter ' +
    FVariablesModel.GenerateScript() +
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
    '2: Choose Options' + LineEnding;
end;

function TScatterStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  Result := TStatDialogContributionViewList.Create;
  Result.Add(CreateMainView(Owner, Executor));
  Result.Add(CreateChartOptionsView(Owner));
end;

initialization
  RegisterStatDialogContribution(TScatterStatDialogContribution.Create, cdGraphs);

end.
