unit histogram_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  histogram_model, histogram_primaryoption_model, chart_options_model;

type

  { THistogramStatDialogContribution }

  THistogramStatDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: THistogramStatDialogPrimaryOptionModel;
    FVariablesModel: THistogramStatDialogVariableModel;
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
  histogram_variables_view,
  histogram_primaryoption_view,
  chart_options_view;

{ THistogramStatDialogContribution }

function THistogramStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: THistogramStatDialogVariablesView;
begin
  FVariablesModel := THistogramStatDialogVariableModel.Create(Executor);
  View := THistogramStatDialogVariablesView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function THistogramStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: THistogramStatPrimaryOptionsView;
begin
  FPrimaryOptionsModel := THistogramStatDialogPrimaryOptionModel.Create(Executor);
  View := THistogramStatPrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function THistogramStatDialogContribution.CreateChartOptionsView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create(Executor, %1011);
  FChartOptionsModel.SetVariableModel(FVariablesModel);
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function THistogramStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'histogram ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function THistogramStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Histogram';
end;

function THistogramStatDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function THistogramStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  result.Add(CreateChartOptionsView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(THistogramStatDialogContribution.Create, cdGraphs);

end.
