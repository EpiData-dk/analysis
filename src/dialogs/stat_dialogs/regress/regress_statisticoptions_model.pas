unit regress_statisticoptions_model;

{$mode objfpc}{$H+}

interface

{
 Valid options
!nocon : no constant term
!anova : ANOVA
!fit:=<variable> : GOES into type_model unit
}
uses
  Classes, SysUtils, stat_dialog_contribution;

type

  TStatisticType   = (pA, pC);
  TStatisticTypes  = set of TStatisticType ;

  { TRegressStatisticOptionsModel }

  TRegressStatisticOptionsModel = class(IStatDialogModel)
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

{ TRegressStatisticOptionsModel }

constructor TRegressStatisticOptionsModel.Create();
begin
  //
end;

procedure TRegressStatisticOptionsModel.SetStatistics(AValue: TStatisticTypes);
begin
  if FStatisticTypes = AValue then Exit;
  FStatisticTypes := AValue;
end;

function TRegressStatisticOptionsModel.GenerateScript(): UTF8String;
begin
  Result := '';
  if (pC in FStatisticTypes) then Result += ' !nocon';
  if (pA in FStatisticTypes) then Result += ' !anova';
end;

function TRegressStatisticOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.

