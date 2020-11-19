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
    function generateScript(): UTF8String;
    function getCaption(): UTF8String;
    function getViews(Owner: TComponent): TStatDialogContributionViewList;
  end;

implementation

uses
  tables_statdialog_view;

{ TTableStatDialogContribution }

function TTableStatDialogContribution.CreateMainView(Owner: TComponent): IStatDialogView;
begin
  result := TTableStatDialogView.Create;
end;

function TTableStatDialogContribution.generateScript(): UTF8String;
begin
  result := '';
end;

function TTableStatDialogContribution.getCaption(): UTF8String;
begin
  result := 'Tables';
end;

function TTableStatDialogContribution.getViews(Owner: TComponent
  ): TStatDialogContributionViewList;
begin
  result := TStatDialogContributionViewList.Create;
  result.add(CreateMainView(Owner));
  result.add(CreateMainView(Owner));
end;

initialization
  RegisterStatDialogContribution(TTableStatDialogContribution.Create);

end.

