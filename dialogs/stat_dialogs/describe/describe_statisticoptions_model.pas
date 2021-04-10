unit describe_statisticoptions_model;

{$mode objfpc}{$H+}

interface
{
 Valid options

 Use any combination of options to customize the output

 !msd:  mean, standard deviation and sum
 !mci:  mean and confidence interval. See set to change the confidence interval
 !rm:   minimum, median, maximum
 !idr:  10th percentile, median, 90th percentile
 !iqr:  25th percentile, median, 75th percentile
 !fh:   5 most frequent values
 !fl:   5 least frequent values
 !fb:   5 most frequent and 5 least frequent values
 !ct:   force one row per variable when the above options are specified.
        This will be ignored if one of !fh !fl !fb is specified.
 !m:    number of missing values
}
uses
  Classes, SysUtils, stat_dialog_contribution;

type

  TMeansType     = (pMSD, pMCI, pRM, pIDR, pIQR);
  TMeansTypes    = set of TMeansType ;
  TFreqType      = (pFH, pFL);
  TFreqTypes     = set of TFreqType ;
  TMissingType   = (pMissing);
  TMissingTypes  = set of TMissingType;
  TCompactType   = (pCT);
  TCompactTypes  = set of TCompactType;

  { TDescribeStatisticOptionsModel }

  TDescribeStatisticOptionsModel = class(IStatDialogModel)
  private
    FMeansTypes:   TMeansTypes;
    FFreqTypes:    TFreqTypes;
    FMissingTypes: TMissingTypes;
    FCompactTypes: TCompactTypes;
    procedure SetMeanOptions(AValue: TMeansTypes);
    procedure SetFreqOptions(AValue: TFreqTypes);
    procedure SetMissing(AValue: TMissingTypes);
    procedure SetCompact(AValue: TCompactTypes);

  public
    constructor Create();
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    property MeanTypes: TMeansTypes read FMeansTypes write SetMeanOptions;
    property FreqTypes: TFreqTypes read FFreqTypes write SetFreqOptions;
    property MissingTypes: TMissingTypes read FMissingTypes write SetMissing;
    property CompactTypes: TCompactTypes read FCompactTypes write SetCompact;
  end;


implementation

uses
  LazUTF8;

{ TDescribeStatisticOptionsModel }

constructor TDescribeStatisticOptionsModel.Create();
begin
  //
end;

procedure TDescribeStatisticOptionsModel.SetMeanOptions(AValue: TMeansTypes);
begin
  if FMeansTypes = AValue then Exit;
  FMeansTypes := AValue;
end;

procedure TDescribeStatisticOptionsModel.SetFreqOptions(AValue: TFreqTypes);
begin
  if FFreqTypes = AValue then Exit;
  FFreqTypes := AValue;
end;

procedure TDescribeStatisticOptionsModel.SetMissing(AValue: TMissingTypes);
begin
  if FMissingTypes = AValue then Exit;
  FMissingTypes := AValue;
end;

procedure TDescribeStatisticOptionsModel.SetCompact(AValue: TCompactTypes);
begin
  if FCompactTypes = AValue then Exit;
  FCompactTypes := AValue;
end;


function TDescribeStatisticOptionsModel.GenerateScript(): UTF8String;
begin
  Result := '';

  if (pMSD in FMeansTypes) then Result += '!msd ';
  if (pMCI in FMeansTypes) then Result += '!mci ';
  if (pRM  in FMeansTypes) then Result += '!rm ';
  if (pIDR  in FMeansTypes) then Result += '!idr ';
  if (pIQR  in FMeansTypes) then Result += '!iqr ';

  if (pFH  in FFreqTypes) then Result += '!fh ';
  if (pFL  in FFreqTypes) then Result += '!fl ';

  if (pMissing in FMissingTypes) then Result += '!m ';

  if (pCT in FCompactTypes) then Result += '!ct ';

  Result := UTF8Trim(Result);
end;

function TDescribeStatisticOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
