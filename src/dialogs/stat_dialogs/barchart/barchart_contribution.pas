unit barchart_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  barchart_model, barchart_primaryoption_model, chart_options_model;

type

  { TBarchartStatDialogContribution }

  TBarchartStatDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TBarchartStatDialogPrimaryOptionModel;
    FVariablesModel: TBarchartStatDialogVariableModel;
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
  barchart_variables_view,
  barchart_primaryoption_view,
  chart_options_view;

{ TBarchartStatDialogContribution }

function TBarchartStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TBarchartStatDialogVariablesView;
begin
  FVariablesModel := TBarchartStatDialogVariableModel.Create(Executor);
  View := TBarchartStatDialogVariablesView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function TBarchartStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent;
  Executor: TExecutor ): IStatDialogView;
var
  View: TBarchartStatPrimaryOptionsView;
begin
  FPrimaryOptionsModel := TBarchartStatDialogPrimaryOptionModel.Create(Executor);
  View := TBarchartStatPrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function TBarchartStatDialogContribution.CreateChartOptionsView(Owner: TComponent;
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

function TBarchartStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'barchart ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function TBarchartStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Barchart';
end;

function TBarchartStatDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TBarchartStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner, Executor));
  result.Add(CreateChartOptionsView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TBarchartStatDialogContribution.Create, cdGraphs);

end.
