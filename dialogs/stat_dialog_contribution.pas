unit stat_dialog_contribution;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl, ExtCtrls;

type

  TStatDialogContributionViewList = specialize TFPGList<TPanel>;

  IStatDialogContribution = interface['{72DC9405-CBF8-4166-8710-F51A816F46CA}']
    function getCaption(): UTF8String;
    function getViews(Owner: TComponent): TStatDialogContributionViewList;
    function generateScript(): UTF8String;
  end;

  TContributionList = specialize TFPGList<IStatDialogContribution>;

  procedure RegisterStatDialogContribution(Contribution: IStatDialogContribution);
  function GetStatDialogContributionList: TContributionList;

implementation

var
  ContributionList: TContributionList;

function GetContributionList: TContributionList;
begin
  if (not Assigned(ContributionList)) then
    ContributionList := TContributionList.Create;

  result := ContributionList;
end;

procedure RegisterStatDialogContribution(Contribution: IStatDialogContribution);
begin
  GetContributionList.Add(Contribution);
end;

function GetStatDialogContributionList: TContributionList;
begin
  result := GetContributionList;
end;

end.

