unit stat_dialog_contribution;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl, ExtCtrls, Controls;

type

  IStatDialogView = interface['{93D8C128-08B0-4C02-98CA-FF9DBA1AF79F}']
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    procedure EnterView();
    function ExitView(): boolean;
  end;
  TStatDialogContributionViewList = specialize TFPGList<IStatDialogView>;

  IStatDialogContribution = interface['{72DC9405-CBF8-4166-8710-F51A816F46CA}']
    function GetCaption(): UTF8String;
    function GetViews(Owner: TComponent): TStatDialogContributionViewList;
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

