unit scatter_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, scatter_variables_model;

type

  { TScatterStatDialogContribution }

  TScatterStatDialogContribution = class(IStatDialogContribution)
  private
    FVariablesModel: TScatterStatVariableModel;
    function CreateMainView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
  end;

implementation

uses
  scatter_main_view;

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

function TScatterStatDialogContribution.GenerateScript(): UTF8String;
begin
  Result := 'scatter ' +
    FVariablesModel.GenerateScript() +
    ';';
end;

function TScatterStatDialogContribution.GetCaption(): UTF8String;
begin
  Result := 'Scatter';
end;

function TScatterStatDialogContribution.GetHelpText(): UTF8String;
begin
  Result :=
    '1: Select Variables' + LineEnding;
end;

function TScatterStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  Result := TStatDialogContributionViewList.Create;
  Result.Add(CreateMainView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TScatterStatDialogContribution.Create, cdGraphs);

end.
