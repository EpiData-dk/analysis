unit stat_dialog_contribution;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl, ExtCtrls, Controls, executor;

type
  IStatDialogModel = interface['{6C0D5628-6749-4785-9028-05956F1C00C8}']
    function IsDefined(): boolean;
    function GenerateScript(): UTF8String;
  end;

  IStatDiaglogViewModified = interface['{7FCE1351-BFDB-449A-8BD7-0EB6F9527033}']
    procedure OnViewModified(DataModel: IStatDialogModel);
  end;

  IStatDialogView = interface['{93D8C128-08B0-4C02-98CA-FF9DBA1AF79F}']
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    procedure EnterView();
    function ExitView(): boolean;
    procedure ResetView();
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
  end;
  TStatDialogContributionViewList = specialize TFPGList<IStatDialogView>;

  { IStatDialogContribution }

  IStatDialogContribution = interface['{72DC9405-CBF8-4166-8710-F51A816F46CA}']
    function GetCaption(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
    function GenerateScript(): UTF8String;
  end;
  TStatDialogContributionList = specialize TFPGList<IStatDialogContribution>;

  procedure RegisterStatDialogContribution(Contribution: IStatDialogContribution);
  function GetStatDialogContributionList: TStatDialogContributionList;

implementation

var
  ContributionList: TStatDialogContributionList;

function GetContributionList: TStatDialogContributionList;
begin
  if (not Assigned(ContributionList)) then
    ContributionList := TStatDialogContributionList.Create;

  result := ContributionList;
end;

procedure RegisterStatDialogContribution(Contribution: IStatDialogContribution);
begin
  GetContributionList.Add(Contribution);
end;

function GetStatDialogContributionList: TStatDialogContributionList;
begin
  result := GetContributionList;
end;

end.

