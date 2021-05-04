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

 !m:    number of missing values
 !st:   force separate tables.
}
uses
  Classes, SysUtils, stat_dialog_contribution;

type

  TMeansType     = (pMSD, pMCI);
  TMeansTypes    = set of TMeansType ;
  TFreqType      = (pFH, pFL);
  TFreqTypes     = set of TFreqType ;
  TRangeType     = (pRM, pIDR, pIQR);
  TRangeTypes    = set of TRangeType;
  TOtherType   = (pMissing, pST);
  TOtherTypes  = set of TOtherType;

  { TDescribeStatisticOptionsModel }

  TDescribeStatisticOptionsModel = class(IStatDialogModel)
  private
    FMeansTypes:   TMeansTypes;
    FFreqTypes:    TFreqTypes;
    FRangeTypes:   TRangeTypes;
    FOtherTypes: TOtherTypes;
    procedure SetMeanOptions(AValue: TMeansTypes);
    procedure SetFreqOptions(AValue: TFreqTypes);
    procedure SetRange(AValue: TRangeTypes);
    procedure SetOther(AValue: TOtherTypes);

  public
    constructor Create();
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    property MeanTypes: TMeansTypes read FMeansTypes write SetMeanOptions;
    property FreqTypes: TFreqTypes read FFreqTypes write SetFreqOptions;
    property RangeTypes: TRangeTypes read FRangeTypes write SetRange;
    property OtherTypes: TOtherTypes read FOtherTypes write SetOther;
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

procedure TDescribeStatisticOptionsModel.SetRange(AValue: TRangeTypes);
begin
  if FRangeTypes = AValue then Exit;
  FRangeTypes := AValue;
end;

procedure TDescribeStatisticOptionsModel.SetOther(AValue: TOtherTypes);
begin
  if FOtherTypes = AValue then Exit;
  FOtherTypes := AValue;
end;


function TDescribeStatisticOptionsModel.GenerateScript(): UTF8String;
begin
  Result := '';

  if (pMSD in FMeansTypes) then Result += '!msd ';
  if (pMCI in FMeansTypes) then Result += '!mci ';

  if (pFH  in FFreqTypes) then Result += '!fh ';
  if (pFL  in FFreqTypes) then Result += '!fl ';

  if (pRM   in FRangeTypes) then Result += '!rm ';
  if (pIDR  in FRangeTypes) then Result += '!idr ';
  if (pIQR  in FRangeTypes) then Result += '!iqr ';

  if (pMissing  in FOtherTypes) then Result += '!m ';
  if (pST in FOtherTypes) then Result += '!st ';

  Result := UTF8Trim(Result);
end;

function TDescribeStatisticOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
