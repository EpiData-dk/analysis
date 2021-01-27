unit tables_statdialog_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  tables_statdialog_model, tables_statdialog_primaryoption_model,
  tables_statdialog_statisticoptions_model;

type
  
  { TTableStatDialogContribution }

  TTableStatDialogContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TTableStatDialogPrimaryOptionModel;
    FStatisticsOptionsModel: TTableStatDialogStatisticOptionsModel;
    FVariablesModel: TTableStatDialogVariableModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreatePrimaryOptionView(Owner: TComponent): IStatDialogView;
    function CreateStatisticOptionView(Owner: TComponent): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  tables_statdialog_variables_view, tables_statdialog_primaryoption_view,
  tables_statdialog_statisticoptions_view;

{ TTableStatDialogContribution }

function TTableStatDialogContribution.CreateMainView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TTableStatDialogVariablesView;
begin
  View := TTableStatDialogVariablesView.Create(Owner);
  FVariablesModel := TTableStatDialogVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TTableStatDialogContribution.CreatePrimaryOptionView(Owner: TComponent
  ): IStatDialogView;
var
  View: TTableStatPrimaryOptionsView;
begin
  View := TTableStatPrimaryOptionsView.Create(Owner);
  FPrimaryOptionsModel := TTableStatDialogPrimaryOptionModel.Create();

  View.SetModel(FPrimaryOptionsModel);

  Result := View;
end;

function TTableStatDialogContribution.CreateStatisticOptionView(
  Owner: TComponent): IStatDialogView;
var
  View: TTableStatDialogStatisticOptionsView;
begin
  View := TTableStatDialogStatisticOptionsView.Create(Owner);
  FStatisticsOptionsModel := TTableStatDialogStatisticOptionsModel.Create();

  View.SetModel(FStatisticsOptionsModel);

  Result := View;
end;

function TTableStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'tables ' +
    FVariablesModel.GenerateScript() + ' ' +
    FPrimaryOptionsModel.GenerateScript() +
    ';';
end;

function TTableStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Tables';
end;

function TTableStatDialogContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1: Select Variables' + LineEnding +
    '2: Click percentage, test, sorting etc.' + LineEnding +
    '3: Run, Execute or Paste command';
end;

function TTableStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.add(CreateMainView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner));
  result.Add(CreateStatisticOptionView(Owner));
end;

initialization
  RegisterStatDialogContribution(TTableStatDialogContribution.Create);

end.

