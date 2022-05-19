unit stat_dialog_contribution;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl, ExtCtrls, Controls, executor;

type

  IStatDialogView = interface;

  { IStatDialogModel }

  IStatDialogModel = interface['{6C0D5628-6749-4785-9028-05956F1C00C8}']
    function IsDefined(): boolean;
    function GenerateScript(): UTF8String;
  end;

  { IStatDiaglogViewModified }

  IStatDiaglogViewModified = interface['{7FCE1351-BFDB-449A-8BD7-0EB6F9527033}']
    procedure OnViewModified(View: IStatDialogView);
  end;

  { IStatDialogView }

  IStatDialogView = interface['{93D8C128-08B0-4C02-98CA-FF9DBA1AF79F}']
    function IsDefined(): boolean;
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    procedure EnterView();
    function ExitView(): boolean;
    procedure ResetView();
    procedure AddOnModified(OnModified: IStatDiaglogViewModified);
    procedure RemoveOnModified(OnModified: IStatDiaglogViewModified);
  end;
  TStatDialogContributionViewList = specialize TFPGList<IStatDialogView>;

  { IStatDialogContribution }

  IStatDialogContribution = interface['{72DC9405-CBF8-4166-8710-F51A816F46CA}']
    function GetCaption(): UTF8String;
    function GetHelpText(): UTF8String;
    function GetViews(Owner: TComponent; Executor: TExecutor): TStatDialogContributionViewList;
    function GenerateScript(): UTF8String;
  end;
  TStatDialogContributionList = specialize TFPGList<IStatDialogContribution>;

  TDialogContributionDomain = (cdStatistics, cdGraphs);
  procedure RegisterStatDialogContribution(Contribution: IStatDialogContribution; ContributionDomain: TDialogContributionDomain = cdStatistics);
  function GetStatDialogContributionList(ContributionDomain: TDialogContributionDomain): TStatDialogContributionList;

implementation

var
  ContributionLists: array[TDialogContributionDomain] of TStatDialogContributionList;

function GetContributionList(ContributionDomain: TDialogContributionDomain): TStatDialogContributionList;
begin
  if (not Assigned(ContributionLists[ContributionDomain])) then
    ContributionLists[ContributionDomain] := TStatDialogContributionList.Create;

  result := ContributionLists[ContributionDomain];
end;

procedure RegisterStatDialogContribution(Contribution: IStatDialogContribution;
  ContributionDomain: TDialogContributionDomain);
begin
  GetContributionList(ContributionDomain).Add(Contribution);
end;

function GetStatDialogContributionList(ContributionDomain: TDialogContributionDomain): TStatDialogContributionList;
begin
  result := GetContributionList(ContributionDomain);
end;

end.

