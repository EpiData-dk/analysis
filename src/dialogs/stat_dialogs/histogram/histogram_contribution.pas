unit histogram_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  histogram_model, histogram_primaryoption_model, chart_options_model;

type

  { THistogramDialogContribution }

  THistogramDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: THistogramDialogPrimaryOptionModel;
    FVariablesModel: THistogramDialogVariableModel;
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
  histogram_variables_view, histogram_primaryoption_view, chart_options_view;

{ THistogramDialogContribution }

function THistogramDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: THistogramDialogVariablesView;
begin
  FVariablesModel := THistogramDialogVariableModel.Create(Executor);
  View := THistogramDialogVariablesView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function THistogramDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: THistogramPrimaryOptionsView;
begin
  FPrimaryOptionsModel := THistogramDialogPrimaryOptionModel.Create(Executor);
  View := THistogramPrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function THistogramDialogContribution.CreateChartOptionsView(Owner: TComponent): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create();
  FChartOptionsModel.MinMax := HistogramMinMax;
  FChartOptionsModel.UseY := true;
  FVariablesModel.ChartOptions := FChartOptionsModel;
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function THistogramDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'histogram ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function THistogramDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Histogram';
end;

function THistogramDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function THistogramDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  result.Add(CreateChartOptionsView(Owner));
end;

initialization
  RegisterStatDialogContribution(THistogramDialogContribution.Create, cdGraphs);

end.
