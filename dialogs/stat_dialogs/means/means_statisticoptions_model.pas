unit means_statisticoptions_model;

{$mode objfpc}{$H+}

interface
// JH comment - remove this
{
 Valid options

Data and output:
!m   : Include observations with missing data (.) (se variables tab?)
!w := <variable> : Use number of observations in the variable as frequency weight (see variables tab)

Estimation and testing:
!t   : ANOVA
}
uses
  Classes, SysUtils, stat_dialog_contribution;

type

  TStatisticType   = (pT);
  TStatisticTypes  = set of TStatisticType ;

  { TMeansStatDialogStatisticOptionsModel }

  TMeansStatDialogStatisticOptionsModel = class(IStatDialogModel)
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

{ TMeansStatDialogStatisticOptionsModel }

constructor TMeansStatDialogStatisticOptionsModel.Create();
begin
  //
end;

procedure TMeansStatDialogStatisticOptionsModel.SetStatistics(AValue: TStatisticTypes);
begin
  if FStatisticTypes = AValue then Exit;
  FStatisticTypes := AValue;
end;

function TMeansStatDialogStatisticOptionsModel.GenerateScript(): UTF8String;
begin
  Result := '';
  if (pT in FStatisticTypes) then Result += ' !t';
end;

function TMeansStatDialogStatisticOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.

