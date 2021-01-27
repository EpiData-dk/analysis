unit tables_statdialog_statisticoptions_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution;

type

  { TTableStatDialogStatisticOptionsModel }

  TTableStatDialogStatisticOptionsModel = class(IStatDialogModel)
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  end;


implementation

{ TTableStatDialogStatisticOptionsModel }

function TTableStatDialogStatisticOptionsModel.GenerateScript(): UTF8String;
begin

end;

function TTableStatDialogStatisticOptionsModel.IsDefined(): boolean;
begin

end;

end.

