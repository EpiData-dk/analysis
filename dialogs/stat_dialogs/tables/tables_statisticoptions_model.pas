unit tables_statisticoptions_model;

{$mode objfpc}{$H+}

interface
// JH comment - remove this
{
 Valid options

Data and output:
!m   : Include observations with missing data (.) (se variables tab?)
!w := <variable> : Use number of observations in the variable as frequency weight (see variables tab)

Estimation and testing:
!t   : Chi2 and p-value
!ex  : Fisher Exact test for 2x2 tables only
!odds: Odds Ratio for 2x2 tables, including Mantel-Haenszel adjustment for stratified data
!rr  : Risk Ratio for 2x2 tables, including Mantel-Haenszel adjustment for stratified data

Attack rate table
!ar  : Show unstratified 2x2 tables, attack rates and risk ratios
!en  : Show unstratified 2x2 tables

}
uses
  Classes, SysUtils, stat_dialog_contribution;

type

  TStatisticType = (pChi, pFET, pOR, pRR); //, pAR, pEN);
  TStatisticTypes = set of TStatisticType ;

//  TTableSort = (sortName, sortLabel, sortStatistic);

  { TTableStatDialogStatisticOptionsModel }

  TTableStatDialogStatisticOptionsModel = class(IStatDialogModel)
  private
    FStatisticTypes: TStatisticTypes;
 //   FTableSort: TTableSort;
    procedure SetStatistics(AValue: TStatisticTypes);
//    procedure SetTableSort(AValue: TTableSort);

  public
    constructor Create();
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    property StatisticTypes: TStatisticTypes read FStatisticTypes write SetStatistics;
//    property TableSort: TTableSort read FTableSort write SetTableSort;
  end;


implementation

uses
  LazUTF8;

{ TTableStatDialogStatisticOptionsModel }

constructor TTableStatDialogStatisticOptionsModel.Create();
begin
  //
end;

procedure TTableStatDialogStatisticOptionsModel.SetStatistics(AValue: TStatisticTypes);
begin
  if FStatisticTypes = AValue then Exit;
  FStatisticTypes := AValue;
end;

{procedure TTableStatDialogStatisticOptionsModel.SetTableSort(AValue: TTableSort);
begin
  if FTableSort = AValue then Exit;
  FTableSort := AValue;
end;
}

function TTableStatDialogStatisticOptionsModel.GenerateScript(): UTF8String;
begin
  Result := '';

{  case FTableSort of
    sortName:      Result += '!sn ';
    sortLabel:     Result += '!sl ';
    sortStatistic: Result += '!ss ';
  end;
}
  if (pChi in FStatisticTypes) then Result += '!t ';
  if (pFET in FStatisticTypes) then Result += '!ex ';
  if (pOR  in FStatisticTypes) then Result += '!odds ';
  if (pRR  in FStatisticTypes) then Result += '!rr ';
//  if (pAR  in FStatisticTypes) then Result += '!ar ';
//  if (pEN  in FStatisticTypes) then Result += '!en ';

  Result := UTF8Trim(Result);
end;

function TTableStatDialogStatisticOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.

