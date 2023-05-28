unit epicurve_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  epicurve_model, epicurve_primaryoption_model, chart_options_model;

type

  { TEpicurveDialogContribution }

  TEpicurveDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TEpicurveDialogPrimaryOptionModel;
    FVariablesModel: TEpicurveDialogVariableModel;
    FChartOptionsModel: TChartOptionsModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreatePrimaryOptionView(Owner: TComponent;  Executor: TExecutor): IStatDialogView;
    function CreateChartOptionsView(Owner: TComponent): IStatDialogView;
 public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  Epicurve_variables_view, Epicurve_primaryoption_view, chart_options_view;

{ TEpicurveDialogContribution }

function TEpicurveDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TEpicurveDialogVariablesView;
begin
  FVariablesModel := TEpicurveDialogVariableModel.Create(Executor);
  View := TEpicurveDialogVariablesView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function TEpicurveDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TEpicurvePrimaryOptionsView;
begin
  FPrimaryOptionsModel := TEpicurveDialogPrimaryOptionModel.Create(Executor);
  View := TEpicurvePrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function TEpicurveDialogContribution.CreateChartOptionsView(Owner: TComponent): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create();
  FChartOptionsModel.MinMax := [mmtXMin, mmtXMax, mmtYMax];
  FChartOptionsModel.UseY := true;
  FVariablesModel.ChartOptions := FChartOptionsModel;
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function TEpicurveDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'epicurve ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function TEpicurveDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Epicurve';
end;

function TEpicurveDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TEpicurveDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  Result.Add(CreateChartOptionsView(Owner));
end;

initialization
  RegisterStatDialogContribution(TEpicurveDialogContribution.Create, cdGraphs);

end.
