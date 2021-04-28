unit freq_mainoptions_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, executor, epidatafiles;

type

  { TFreqMainOptionsModel }

  TFreqMainOptionsModel = class(IStatDialogModel)
  private
    FShowConfidenceInterval: boolean;
    FShowCumulativePercentage: boolean;
    FShowMissing: boolean;
    FShowRowPercentage: boolean;
    procedure SetShowConfidenceInterval(AValue: boolean);
    procedure SetShowCumulativePercentage(AValue: boolean);
    procedure SetShowMissing(AValue: boolean);
    procedure SetShowRowPercentage(AValue: boolean);
  public
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    constructor Create();
    property ShowRowPercentage: boolean read FShowRowPercentage write SetShowRowPercentage;
    property ShowCumulativePercentage: boolean read FShowCumulativePercentage write SetShowCumulativePercentage;
    property ShowMissing: boolean read FShowMissing write SetShowMissing;
    property ShowConfidenceInterval: boolean read FShowConfidenceInterval write SetShowConfidenceInterval;
  end;

implementation

{ TFreqMainOptionsModel }

procedure TFreqMainOptionsModel.SetShowConfidenceInterval(AValue: boolean);
begin
  if FShowConfidenceInterval = AValue then Exit;
  FShowConfidenceInterval := AValue;
end;

procedure TFreqMainOptionsModel.SetShowCumulativePercentage(AValue: boolean);
begin
  if FShowCumulativePercentage = AValue then Exit;
  FShowCumulativePercentage := AValue;
end;

procedure TFreqMainOptionsModel.SetShowMissing(AValue: boolean);
begin
  if FShowMissing = AValue then Exit;
  FShowMissing := AValue;
end;

procedure TFreqMainOptionsModel.SetShowRowPercentage(AValue: boolean);
begin
  if FShowRowPercentage = AValue then Exit;
  FShowRowPercentage := AValue;
end;

function TFreqMainOptionsModel.GenerateScript(): UTF8String;
begin
  Result := '';

  if (ShowRowPercentage) then result += '!pc ';
  if (ShowCumulativePercentage) then result += '!cum ';
  if (ShowMissing) then result += '!m ';
  if (ShowConfidenceInterval) then result += '!ci ';

  Result := Trim(result);
end;

function TFreqMainOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

constructor TFreqMainOptionsModel.Create();
begin

end;

end.
