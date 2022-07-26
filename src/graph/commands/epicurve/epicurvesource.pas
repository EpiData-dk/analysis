unit epicurvesource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TACustomSource, epidatafiles, freq, tables, tables_types, outputcreator;

type
  floatArray = array of Double;
  freqArray = array of array of Double;
  { TEpicurveSource }

  TEpicurveSource = class(TUserDefinedChartSource)
  private
    FMsg: TOutputCreator;
    FFreqs: freqArray;
    FX0, FXn: Integer;
    FMaxCount: Integer;
    FBoxes: boolean;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    function doBoxes(f: Double): floatArray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property test: freqArray read FFreqs;
    property Msg: TOutputCreator write FMsg;
    property X0: Integer read FX0;
    property Xn: Integer read FXn;
    property maxCount: Integer read FMaxCount;
    property boxes: Boolean read FBoxes write FBoxes;
    procedure FromFreq(F: TFreqDataFile);
    procedure FromTable(T: TTwoWayTable);
  end;

implementation

uses
  Math;
{ TEpicurveSource }

procedure TEpicurveSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  FMsg.DoInfoShort('Get ' + AIndex.ToString);
  AItem.X := (FX0 + AIndex).ToDouble;
  if (FBoxes) then
    AItem.YList := doBoxes(FFreqs[AIndex, 0])
  else
    AItem.YList := FFreqs[AIndex];
end;

procedure TEpicurveSource.FromFreq(F: TFreqDataFile);
var
  i, index, k, l: Integer;
begin
  FmaxCount := 0;
  FX0      := F.Categ.AsInteger[0];
  FXn      := F.Categ.AsInteger[F.Size - 1];
  setLength(FFreqs, FXn - FX0 + 1, 1);
  index := 0;
  for i := FX0 to FXn do
    begin
      k := F.Categ.AsInteger[index];
      l := F.Count.AsInteger[index];
      if (i < F.Categ.AsInteger[index]) then
        begin
          FFreqs[i - FX0, 0] := 0;
        end
      else
        begin
          FFreqs[i - FX0, 0] := F.Count.AsFloat[index];
          FmaxCount := Math.Max(FmaxCount, F.Count.AsInteger[index]);
          index += 1;
        end;
    end;
  k := FXn - FX0 + 1;
  PointsNumber := k;
  YCount := FmaxCount;
  FMsg.DoInfoShort('Points: ' +  k.ToString + ' YCount: ' + FmaxCount.ToString);
end;

procedure TEpicurveSource.FromTable(T: TTwoWayTable);
var
  i, j, k, l, index: Integer;
begin
  FmaxCount := T.ColTotal[0];
  FX0 := T.ColVariable.AsInteger[0];
  FXn := T.ColVariable.AsInteger[T.ColCount - 1];
  setLength(FFreqs, FXn - FX0 + 1, T.RowCount);
  index := 0;
  for i := FX0 to FXn do
    begin
      k := T.ColVariable.AsInteger[index];
      l := T.ColTotal[index];
      if (i < T.ColVariable.AsInteger[index]) then
        for j := 0 to T.RowCount - 1 do
          FFreqs[i - FX0, j] := 0
      else
        begin
          FmaxCount := Math.Max(FmaxCount, T.ColTotal[index]);
          for j := 0 to T.RowCount - 1 do
            FFreqs[i - FX0, j] := T.Cell[index, j].N.ToDouble;
          index += 1;
        end;
      end;
  k := FXn - FX0 + 1;
  PointsNumber := k;
  YCount := FmaxCount;
  FMsg.DoInfoShort('Points: ' +  k.ToString + ' YCount: ' + FmaxCount.ToString);
end;

function TEpicurveSource.doBoxes(f: Double): floatArray;
var
  i: Integer;
  n: Integer;
begin
  n := trunc(f);
  setLength(result, n);
  for i := 0 to n - 1 do
    result[i] := 1.0;
end;

constructor TEpicurveSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
  FBoxes := false;
end;

destructor TEpicurveSource.Destroy;
begin
  inherited Destroy;
end;

end.
