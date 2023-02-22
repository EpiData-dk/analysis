unit barchart_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  barchart_model, barchart_primaryoption_model, chart_options_model;

type

  { TBarchartDialogContribution }

  TBarchartDialogContribution = class(IStatDialogContribution)
  private
    FVariablesModel: TBarchartDialogVariableModel;
    FPrimaryOptionsModel: TBarchartDialogPrimaryOptionModel;
    FChartOptionsModel: TChartOptionsModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreatePrimaryOptionsView(Owner: TComponent;  Executor: TExecutor): IStatDialogView;
    function CreateChartOptionsView(Owner: TComponent): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  barchart_variables_view, barchart_primaryoption_view,
  chart_options_view;

{ TBarchartDialogContribution }

function TBarchartDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TBarchartDialogVariablesView;
begin
  FVariablesModel := TBarchartDialogVariableModel.Create(Executor);
  View := TBarchartDialogVariablesView.Create(Owner);
  View.SetModel(FVariablesModel);
  Result := View;
end;

function TBarchartDialogContribution.CreatePrimaryOptionsView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TBarchartPrimaryOptionsView;
begin
  FPrimaryOptionsModel := TBarchartDialogPrimaryOptionModel.Create(Executor);
  View := TBarchartPrimaryOptionsView.Create(Owner);
  View.SetModel(FPrimaryOptionsModel);
  Result := View;
end;

function TBarchartDialogContribution.CreateChartOptionsView(Owner: TComponent): IStatDialogView;
var
  View: TChartOptionsView;
begin
  FChartOptionsModel := TChartOptionsModel.Create();
  FChartOptionsModel.MinMax := BarchartMinMax;
  FChartOptionsModel.UseY := true;
  View := TChartOptionsView.Create(Owner);
  View.SetModel(FChartOptionsModel);
  Result := View;
end;

function TBarchartDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'barchart ' +
    FVariablesModel.GenerateScript() +
    FPrimaryOptionsModel.GenerateScript() +
    FChartOptionsModel.GenerateScript() +
    ';';
end;

function TBarchartDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Barchart';
end;

function TBarchartDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Choose options' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TBarchartDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.Add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionsView(Owner, Executor));
  result.Add(CreateChartOptionsView(Owner));
end;

initialization
  RegisterStatDialogContribution(TBarchartDialogContribution.Create, cdGraphs);

end.
