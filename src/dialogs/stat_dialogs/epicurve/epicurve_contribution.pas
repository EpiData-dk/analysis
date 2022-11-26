unit epicurve_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  epicurve_model, epicurve_primaryoption_model, chart_options_model;

type

  { TEpicurveStatDialogContribution }

  TEpicurveStatDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TEpicurveStatDialogPrimaryOptionModel;
    FVariablesModel: TEpicurveStatDialogVariableModel;
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
  Epicurve_variables_view,
  Epicurve_primaryoption_view,
  chart_options_view;

{ TEpicurveStatDialogContribution }

function TEpicurveStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TEpicurveStatDialogVariablesView;
begin
  FVariablesModel := TEpicurveStatDialogVariableModel.Create(Executor);
  View := TEpicurveStatDialogVariablesView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function TEpicurveStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TEpicurveStatPrimaryOptionsView;
begin
  FPrimaryOptionsModel := TEpicurveStatDialogPrimaryOptionModel.Create(Executor);
  View := TEpicurveStatPrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function TEpicurveStatDialogContribution.CreateChartOptionsView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create(Executor);
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function TEpicurveStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'epicurve ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function TEpicurveStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Epicurve';
end;

function TEpicurveStatDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TEpicurveStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  result.Add(CreateChartOptionsView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TEpicurveStatDialogContribution.Create, cdGraphs);

end.
