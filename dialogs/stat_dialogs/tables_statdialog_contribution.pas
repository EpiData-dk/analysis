unit tables_statdialog_contribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls;

type
  
  { TTableStatDialogContribution }

  TTableStatDialogContribution = class(IStatDialogContribution)
  private
    function CreateMainView(Owner: TComponent): IStatDialogView;
  public
    function GenerateScript(): UTF8String;
    function GetCaption(): UTF8String;
    function GetViews(Owner: TComponent): TStatDialogContributionViewList;
  end;

implementation

uses
  tables_statdialog_variables_view, tables_statdialog_model;

{ TTableStatDialogContribution }

function TTableStatDialogContribution.CreateMainView(Owner: TComponent): IStatDialogView;
var
  View: TTableStatDialogVariablesView;
begin
  View := TTableStatDialogVariablesView.Create(Owner);
  View.SetModel(TTableStatDialogVariableModel.Create(nil));
end;

function TTableStatDialogContribution.GenerateScript(): UTF8String;
begin
  result := '';
end;

function TTableStatDialogContribution.GetCaption(): UTF8String;
begin
  result := 'Tables';
end;

function TTableStatDialogContribution.GetViews(Owner: TComponent
  ): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.add(CreateMainView(Owner));
end;

initialization
  RegisterStatDialogContribution(TTableStatDialogContribution.Create);

end.

