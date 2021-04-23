unit freq_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor,
  freq_variable_view, freq_variable_model, freq_mainoptions_view, freq_mainoptions_model;

type

  { TFreqStatDialogContribution }

  TFreqStatDialogContribution = class(IStatDialogContribution)
  private
    FVariableModel: TFreqVariableModel;
    FMainOptionsModel: IStatDialogModel;
    function CreateMainOptionsView(Owner: TComponent): IStatDialogView;
    function CreateVariablesView(Owner: TComponent; Executor: TExecutor): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor
      ): TStatDialogContributionViewList;

  end;

implementation




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

function TFreqStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := 'freq ' +
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
end;

initialization
  RegisterStatDialogContribution(TFreqStatDialogContribution.Create);

end.

