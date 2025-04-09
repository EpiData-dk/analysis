unit regress_contribution;


{ TODO
  Dialogs set up OK. Still to do:

  Model valid if X1 selected on type view
  Disable X var on type view if type can have multiple vars
  Disable variables view if X selected on type view
  Show / hide Degree if poly / other selected
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, executor,
  regress_type_model, regress_variables_model, regress_primaryoption_model,
  regress_statisticoptions_model;

type

  { TRegressContribution }

  TRegressContribution = class(IStatDialogContribution)
  private
    FPrimaryOptionsModel: TRegressPrimaryOptionModel;
    FStatisticsOptionsModel: TRegressStatisticOptionsModel;
    FVariablesModel: TRegressVariableModel;
    FTypeModel: TRegressTypeModel;
    function CreateTypeView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreateVariablesView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
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
  regress_type_view, regress_variables_view, regress_primaryoption_view, regress_statisticoptions_view;

{ TRegressContribution }

function TRegressContribution.CreateTypeView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TRegressTypeView;
begin
  View := TRegressTypeView.Create(Owner);
  FTypeModel := TRegressTypeModel.Create(Executor);

  View.SetModel(FTypeModel);

  Result := View;
end;

function TRegressContribution.CreateVariablesView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TRegressVariableView;
begin
  View := TRegressVariableView.Create(Owner);
  FVariablesModel := TRegressVariableModel.Create(Executor);

  View.SetModel(FVariablesModel);

  Result := View;
end;

function TRegressContribution.CreatePrimaryOptionView(Owner: TComponent
  ): IStatDialogView;
var
  View: TRegressStatPrimaryOptionsView;
begin
  View := TRegressStatPrimaryOptionsView.Create(Owner);
  FPrimaryOptionsModel := TRegressPrimaryOptionModel.Create();

  View.SetModel(FPrimaryOptionsModel);

  Result := View;
end;

function TRegressContribution.CreateStatisticOptionView(
  Owner: TComponent): IStatDialogView;
var
  View: TRegressStatisticOptionsView;
begin
  View := TRegressStatisticOptionsView.Create(Owner);
  FStatisticsOptionsModel := TRegressStatisticOptionsModel.Create();

  View.SetModel(FStatisticsOptionsModel);

  Result := View;
end;

function TRegressContribution.GenerateScript(): UTF8String;
var
  yVars: UTF8String;
begin
  result := 'regress ' +
    FTypeModel.GenerateScript() + ' ' +
    FPrimaryOptionsModel.GenerateScript() +
    FStatisticsOptionsModel.GenerateScript() +
    ';';
  yVars := FVariablesModel.GenerateScript();
  if trim(yVars) <> '' then
    if pos('%indepvars%', result) > 0 then
      result := StringReplace(result,'%indepvars%',yVars, [rfIgnoreCase]);
end;

function TRegressContribution.GetCaption(): UTF8String;
begin
  result := 'regress';
end;

function TRegressContribution.GetHelpText(): UTF8String;
begin
  result :=
    '1. Select type of regression and dependant variable' + LineEnding +
    '2: Select Variables' + LineEnding +
    '3: Click percentage, test, sorting etc.' + LineEnding +
    '4: Run, Execute or Paste command';
end;

function TRegressContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.add(CreateTypeView(Owner, Executor));
  result.add(CreateVariablesView(Owner, Executor));
  result.Add(CreatePrimaryOptionView(Owner));
  result.Add(CreateStatisticOptionView(Owner));
end;

initialization
  RegisterStatDialogContribution(TRegressContribution.Create);

end.

