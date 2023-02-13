unit fbarchart_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  fbarchart_model, fbarchart_primaryoption_model, chart_options_model;

type

  { TFBarChartStatDialogContribution }

  TFBarChartStatDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TFBarChartStatDialogPrimaryOptionModel;
    FVariablesModel: TFBarChartStatDialogVariableModel;
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
  fbarchart_variables_view,
  fbarchart_primaryoption_view,
  chart_options_view;

{ TFBarChartStatDialogContribution }

function TFBarChartStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TFBarChartStatDialogVariablesView;
begin
  FVariablesModel := TFBarChartStatDialogVariableModel.Create(Executor);
  View := TFBarChartStatDialogVariablesView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function TFBarChartStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TFBarChartStatPrimaryOptionsView;
begin
  FPrimaryOptionsModel := TFBarChartStatDialogPrimaryOptionModel.Create(Executor);
  View := TFBarChartStatPrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function TFBarChartStatDialogContribution.CreateChartOptionsView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create(Executor);
  FChartOptionsModel.SetVariableModel(FVariablesModel);
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function TFBarChartStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'barchart ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function TFBarChartStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'FBarchart';
end;

function TFBarChartStatDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TFBarChartStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  result.Add(CreateChartOptionsView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TFBarChartStatDialogContribution.Create, cdGraphs);

end.
