unit freq_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor,
  freq_variable_model, freq_mainoptions_model, common_select_model;

type

  { TFreqStatDialogContribution }

  TFreqStatDialogContribution = class(IStatDialogContribution)
  private
    FVariableModel: TFreqVariableModel;
    FMainOptionsModel: IStatDialogModel;
    FSelectModel: TCommonSelectModel;
    function CreateMainOptionsView(Owner: TComponent): IStatDialogView;
    function CreateVariablesView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
    function CreateSelectView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;

  end;

implementation

uses
  freq_variable_view, freq_mainoptions_view, common_select_view;

{ TFreqStatDialogContribution }

function TFreqStatDialogContribution.CreateVariablesView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TFreqVariableView;
  Model: TFreqVariableModel;
begin
  View := TFreqVariableView.Create(Owner);
  Model := TFreqVariableModel.Create(Executor);
  View.SetModel(Model);

  FVariableModel := Model;
  Result := View;
end;

function TFreqStatDialogContribution.CreateMainOptionsView(Owner: TComponent
  ): IStatDialogView;
var
  View: TFreqMainOptionsView;
  Model: TFreqMainOptionsModel;
begin
  View := TFreqMainOptionsView.Create(Owner);
  Model := TFreqMainOptionsModel.Create();
  View.SetDatamodel(Model);

  FMainOptionsModel := Model;
  Result := View;
end;

function TFreqStatDialogContribution.CreateSelectView(Owner: TComponent;
  Executor: TExecutor): IStatDialogView;
var
  View: TCommonSelectView;
begin
  View := TCommonSelectView.Create(Owner);
  FSelectModel := TCommonSelectModel.Create(Executor);

  View.SetModel(FSelectModel);

  Result := View;
end;

function TFreqStatDialogContribution.GenerateScript(): UTF8String;
begin
  result :=
    FSelectModel.GenerateScript() + ' ' +
    'freq ' +
    FVariableModel.GenerateScript() + ' ' +
    FMainOptionsModel.GenerateScript() +
    ';';
end;

function TFreqStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Frequency';
end;

function TFreqStatDialogContribution.GetHelpText(): UTF8String;
begin
  result := 'Help For Freq!';
end;

function TFreqStatDialogContribution.GetViews(Owner: TComponent;
  Executor: TExecutor): TStatDialogContributionViewList;
begin
  Result := TStatDialogContributionViewList.Create;
  Result.Add(CreateVariablesView(Owner, Executor));
  Result.Add(CreateMainOptionsView(Owner));
  result.Add(CreateSelectView(Owner, Executor));
end;

initialization
  RegisterStatDialogContribution(TFreqStatDialogContribution.Create);

end.

