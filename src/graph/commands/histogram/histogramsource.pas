unit histogramsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, TASources, TACustomSource, epidatafiles, freq, tables, tables_types, outputcreator;

type
  freqArray = array of array of Double;
  { THistogramSource }

  THistogramSource = class(TUserDefinedChartSource)
  private
    FFreqs: freqArray;
    FX0, FXn: Integer;
    FMaxCount: Integer;
    FBoxes: boolean;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property test: freqArray read FFreqs;
    property X0: Integer read FX0;
    property Xn: Integer read FXn;
    property maxCount: Integer read FMaxCount;
    property boxes: Boolean read FBoxes write FBoxes;
    procedure FromFreq(F: TFreqDataFile);
    procedure FromTable(T: TTwoWayTable);
    function AddAxisScales(Chart: TChart): TListChartSource;
  end;

implementation

uses
  Math;
{ THistogramSource }

procedure THistogramSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  i: Integer;
begin
  AItem.X := (FX0 + AIndex).ToDouble;
  // NB: for barchart, must set individual Y values; do not use AItem.YList property!!
  if (FBoxes) then
    for i := 0 to (trunc(FFreqs[AIndex, 0]) - 1) do
      AItem.SetY(i, 1)
  else
    begin
      if (YCount = 1) then
        AItem.Y := FFreqs[AIndex,0]
      else
        begin
          for i := 0 to high(FFreqs[AIndex]) do
            AItem.SetY(i, FFreqs[AIndex, i]);
        end;
    end;
end;

procedure THistogramSource.FromFreq(F: TFreqDataFile);
var
  i, index: Integer;
begin
  FmaxCount := 0;
  FX0      := F.Categ.AsInteger[0];
  FXn      := F.Categ.AsInteger[F.Size - 1];
  setLength(FFreqs, FXn - FX0 + 1, 1);
  index := 0;
  for i := FX0 to FXn do
    begin
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
  PointsNumber := length(FFreqs);
  if (FBoxes) then
    YCount := FmaxCount
  else
    YCount := 1;
end;

// TODO: Jamie-use boxes option to make individual boxes
//       requires vector of maxcount for strata
//       then must fill in zeros for lower strata and colour them appropriately
//       Must also create a dummy series for the legend; one point (FX0,0) for each stratum
//       Colours for the dummy series need to match
procedure THistogramSource.FromTable(T: TTwoWayTable);
var
  i, j, index: Integer;
begin
  FmaxCount := T.ColTotal[0];
  FX0 := T.ColVariable.AsInteger[0];
  FXn := T.ColVariable.AsInteger[T.ColCount - 1];
  setLength(FFreqs, FXn - FX0 + 1, T.RowCount);
  index := 0;
  for i := 0 to high(FFreqs) do
    begin
      if (i < (T.ColVariable.AsInteger[index] - FX0)) then
//      // it is sufficient to set the first to zero
        FFreqs[i, 0] := 0
      else
        begin
          FmaxCount := Math.Max(FmaxCount, T.ColTotal[index]);
          for j := 0 to T.RowCount - 1 do
            FFreqs[i, j] := T.Cell[index, j].N.ToDouble;
          index += 1;
        end;
      end;
  PointsNumber := length(FFreqs);
  YCount := T.RowCount;
end;

function THistogramSource.AddAxisScales(Chart: TChart): TListChartSource;
var
  i: Integer;
  tick: Double;
begin
  result := TListChartSource.Create(Chart);
  for i := X0 to Xn do
    begin
      tick := i.ToDouble;
      result.Add(tick, tick);
    end;
end;

constructor THistogramSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
  FBoxes := false;
end;

destructor THistogramSource.Destroy;
begin
  inherited Destroy;
end;

end.
