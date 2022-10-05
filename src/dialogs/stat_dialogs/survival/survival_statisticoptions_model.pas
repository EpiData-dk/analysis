unit survival_statisticoptions_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution;

type

  TStatisticType = (pChi);
  TStatisticTypes = set of TStatisticType ;
  TCIType = (ciDefault, ciLine, ciBand, ciNone);
  TCITypes = set of TCIType;

  { TSurvivalStatDialogStatisticOptionsModel }

  TSurvivalStatDialogStatisticOptionsModel = class(IStatDialogModel)
  private
    FStatisticTypes: TStatisticTypes;
    FCIType: TCIType;
    procedure SetStatistics(AValue: TStatisticTypes);
    procedure SetCIType(AValue: TCIType);
  public
    constructor Create();
    function GenerateScript(): UTF8String;
    function IsDefined(): boolean;
  public
    property StatisticTypes: TStatisticTypes read FStatisticTypes write SetStatistics;
    property CIType: TCIType read FCIType write SetCIType;
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

procedure TSurvivalStatDialogStatisticOptionsModel.SetCIType(AValue: TCIType);
begin
  if (FCIType = AValue) then exit;
  FCIType := AValue;
end;

function TSurvivalStatDialogStatisticOptionsModel.GenerateScript(): UTF8String;
begin
  Result := '';

  if (pChi in FStatisticTypes) then Result += ' !t';

  case FCIType of
    ciLine:
      Result += ' !cil';
    ciBand:
      Result += ' !cib';
    ciNone:
      Result += ' !cin';
    ciDefault:
      ;
  end;
//  Result := UTF8Trim(Result);
end;

function TSurvivalStatDialogStatisticOptionsModel.IsDefined(): boolean;
begin
  result := true;
end;

end.
