unit stat_dialog_contribution;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl, ExtCtrls;

type

  TStatDialogContributionViewList = specialize TFPGList<TPanel>;

  IStatDialogContribution = interface['{72DC9405-CBF8-4166-8710-F51A816F46CA}']
    function getViews(): TStatDialogContributionViewList;
    function generateScript(): UTF8String;
  end;


implementation

end.

