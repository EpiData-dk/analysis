unit histogramsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, TASources, TACustomSource, epidatafiles, freq, tables, tables_types;

type
  freqArray = array of array of Double;
  { THistogramSource }

  THistogramSource = class(TUserDefinedChartSource)
  private
    FFreqs: freqArray;
    Fstrata: Integer;
    FX0, FXn: Integer;
    FMaxCount: Integer;
    FRowEnd: array of Integer;
    FRowStart: array of Integer;
    FBoxes: boolean;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    function getRowHeight(Index: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property X0: Integer read FX0;
    property Xn: Integer read FXn;
    property RowHeight[Index: Integer]: Integer read getRowHeight;
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
  i, j, s: Integer;
begin
  AItem.X := (FX0 + AIndex).ToDouble;
  // NB: for barchart, must set individual Y values; do not use AItem.YList property!!
  if (FBoxes) then
    if (Fstrata = 1) then
      begin
        for i := 0 to (trunc(FFreqs[AIndex, 0]) - 1) do
          AItem.SetY(i, 1);
      end
    else
      begin
        for s := 0 to Fstrata - 1 do
          begin
            j := FRowStart[s];
            for i := j to (trunc(FFreqs[AIndex,s]) + j - 1) do
              AItem.SetY(i, 1);
            for i := (trunc(FFreqs[AIndex, s]) + j) to FRowEnd[s] do
              AItem.SetY(i, 0);
          end;
      end
  else
    begin
      for i := 0 to high(FFreqs[AIndex]) do
        AItem.SetY(i, FFreqs[AIndex, i]);
    end;
end;

procedure THistogramSource.FromFreq(F: TFreqDataFile);
var
  i, index: Integer;
begin
  Fstrata   := 1;
  FmaxCount := 0;
  FX0       := F.Categ.AsInteger[0];
  FXn       := F.Categ.AsInteger[F.Categ.Size - 1];
  setLength(FFreqs, FXn - FX0 + 1, 1);
  // FFreqs[i] has [0] initially
  // copy counts into elements
  for i := 0 to F.Categ.Size - 1 do
    begin
      FFreqs[F.Categ.AsInteger[i] - FX0, 0] := F.Count.AsFloat[i];
      FmaxCount := Math.Max(FmaxCount, F.Count.AsInteger[i]);
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
// plan: get max count for each stratum (loop across T.Rows)
//       set length of FFreqs to sum of maximum counts
//       loop across freqs, filling in zeros for empty counts;
//       for non-zero counts, loop across rows to fill with N ones and max-N zeros
procedure THistogramSource.FromTable(T: TTwoWayTable);
var
  i, j, col, index, row, n, prevEnd: Integer;
begin
  Fstrata := T.RowCount;
  setLength(FRowEnd, Fstrata);
  setLength(FRowStart, Fstrata);
  FX0 := T.ColVariable.AsInteger[0];
  FXn := T.ColVariable.AsInteger[T.ColCount - 1];
  setLength(FFreqs, FXn - FX0 + 1, T.RowCount);

  // get max counts (by row if boxes)
  if (FBoxes) then
    begin
      FMaxCount := 0;
      prevEnd := -1;
      for row := 0 to Fstrata - 1 do
        begin
          FRowStart[row] := prevEnd + 1;
          n := 0;
          for col := 0 to T.ColCount - 1 do
             n := Math.Max(n, T.Cell[col, row].N);
          FRowEnd[row] := FRowStart[row] + n - 1;
          FMaxCount := FMaxCount + n;
          prevEnd := FRowEnd[row];
        end;
    end
  else
    begin
      for row := 0 to Fstrata - 1 do
        begin
          FRowStart[row] := i;
          FRowEnd[row] := i
        end;
      FmaxCount := T.RowCount;
    end;

  // fill frequencies
  for i := 0 to T.ColCount - 1 do
    begin
      index := T.ColVariable.AsInteger[i] - FX0;
        for j := 0 to T.RowCount - 1 do
          begin
            FFreqs[index, j] := T.Cell[i, j].N.ToDouble;
          end;
      end;
  PointsNumber := length(FFreqs);
  YCount := FMaxCount;
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

function THistogramSource.getRowHeight(Index: Integer): Integer;
begin
  if ((Index >= 0) and (Index < Length(FRowStart))) then
    result := FRowEnd[Index] - FRowStart[Index] +1
  else
    result := 0;
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
  FFreqs := nil;
end;

end.
