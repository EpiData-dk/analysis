unit survival_statisticoptions_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution;

type

  TStatisticType = (pChi);
  TStatisticTypes = set of TStatisticType ;

  { TSurvivalStatDialogStatisticOptionsModel }

  TSurvivalStatDialogStatisticOptionsModel = class(IStatDialogModel)
  private
    FStatisticTypes: TStatisticTypes;
    procedure SetStatistics(AValue: TStatisticTypes);

  public
    constructor Create();
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    property StatisticTypes: TStatisticTypes read FStatisticTypes write SetStatistics;
  end;


implementation

uses
  LazUTF8;

{ TSurvivalStatDialogStatisticOptionsModel }

constructor TSurvivalStatDialogStatisticOptionsModel.Create();
begin
  //
end;

procedure TSurvivalStatDialogStatisticOptionsModel.SetStatistics(AValue: TStatisticTypes);
begin
  if FStatisticTypes = AValue then Exit;
  FStatisticTypes := AValue;
end;

function TSurvivalStatDialogStatisticOptionsModel.GenerateScript(): UTF8String;
begin
  Result := '';

  if (pChi in FStatisticTypes) then Result += ' !t';

//  Result := UTF8Trim(Result);
end;

function TSurvivalStatDialogStatisticOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
