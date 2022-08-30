unit histogramdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, epifields_helper,
  ast, epidatafiles, epicustombase, chartfactory,
  TAGraph, TALegend, tables_types, tables, freq;

type

  countArray = array of Integer;

  { THistogram }

  THistogram = class(TObject)
    private
      FExecutor: TExecutor;
      FValueLabelOutput: TEpiGetValueLabelType;
      FBins:    array of countArray;
      FBase:    integer;   // lowest x value
      FCount:   Integer;   // (highest x - lowest x)/interval + 1
      FTotal:   Integer;   // sum of counts
      FPct:     Boolean;   // flag for percentages
      FStrata:  Integer;
      FStrataName: array of UTF8String;
      FXValue: countArray;
      FMaxCounts: array of Integer;
      FInterval: Integer;
      function getSlotCounts(index: Integer):  countArray;
      function getMaxCount(stratum: Integer): Integer;
      function getXValue(index: Integer): Integer;
      function getName(index: Integer): UTF8String;
      function getInterval(): Integer;
      function getSlot(x: Integer): Integer;
      procedure setInterval(i : Integer);
      procedure initBins(s, lo, hi: Integer);
      procedure setMaxCount(stratum: Integer);
    public
      constructor Create(Executor: TExecutor; Command: TCustomGraphCommand);
      destructor Destroy; override;
      property Base:     Integer read FBase write FBase;
      property Interval: Integer read getInterval write setInterval;
      property Strata:   Integer read FStrata;
      property Count:    Integer read FCount;
      property Total:    Integer read FTotal;
      property PctCalc:  Boolean read FPct write FPct;
      property SlotCounts[index: Integer]: countArray read getSlotCounts;
      property XValue[index: Integer]:     Integer read getXValue;
      property Name[index: Integer]:       UTF8String read getName;
      property MaxCount[stratum: Integer]: Integer read getMaxCount;
      procedure Fill(F: TFreqDataFile);
      procedure Fill(T: TTwoWayTable);
      procedure HistogramToEpicurve;
  end;

implementation

uses
  TASeries, Graphics, ast_types, math,
  options_utils;

{ THistogram }

// THistogram handles frequencies for all integer values from min to max of the column variable

constructor THistogram.Create(Executor: TExecutor; Command: TCustomGraphCommand);
begin
  FExecutor := Executor;
  FValueLabelOutput := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FBase := 0;
  FCount := 0;
  FInterval := 1;
  FPct  := false;
end;

procedure THistogram.initBins(s, lo, hi: Integer);
var
  i: Integer;
begin
  FStrata := s;
  FBase   := lo;
  FCount  := 1 + (hi - lo) div FInterval;
  setLength(FBins, FCount, s);
  setLength(FMaxCounts, s);
  setLength(FStrataName, s);
  setLength(FXValue, FCount);
  for i := 0 to high(FXvalue) do
    FXValue[i] :=  FBase + i * FInterval;
end;

procedure THistogram.setMaxCount(stratum: Integer);
var
  n, slot: Integer;
begin
  n := 0;
  for slot := 0 to high (FBins) do
    n := math.Max(n, FBins[slot,stratum]);
  FMaxCounts[stratum] := n;
end;

function THistogram.getSlot(x: Integer): Integer;
begin
  result := (x - FBase) div FInterval;
end;

function THistogram.getSlotCounts(index: Integer): countArray;
begin
  if (index < 0) then
    result := [0]
  else
    begin
      setLength(result, FStrata);
      result := FBins[index];
    end;
end;

function THistogram.getXValue(index: Integer): Integer;
begin
  if (index < 0) then
    result := 0
  else
    result := FXValue[index];
end;

function THistogram.getName(index: Integer): UTF8String;
begin
  if (index < 0) then
    result := ''
  else
    result := FStrataName[index];
end;

function THistogram.getMaxCount(stratum: Integer): Integer;
begin
  result := FMaxCounts[stratum];
end;

function THistogram.getInterval(): Integer;
begin
  result := FInterval;
end;

procedure THistogram.SetInterval(i: Integer);
begin
  if (i < 0) then
    raise Exception.Create('Negative Interval specified.');
  if (FCount > 0) then
    raise Exception.Create('Histogram--SetInterval called before Fill.');
  FInterval := i;
end;

procedure THistogram.Fill(F: TFreqDataFile);
var
  i: Integer;
begin
  initBins(1, F.Categ.AsInteger[0], F.Categ.AsInteger[F.Categ.Size - 1]);
  FStrataName[0] := '';
  for i := 0 to F.Categ.Size - 1 do
    begin
      FBins[getSlot(F.Categ.AsInteger[i]), 0] += F.Count[i];
    end;
  setMaxCount(0);
  FTotal := F.Sum;
end;

procedure THistogram.Fill(T: TTwoWayTable);
var
  col, stratum: Integer;
begin
  initBins(T.RowCount, T.ColVariable.AsInteger[0], T.ColVariable.AsInteger[T.ColCount - 1]);
  for stratum := 0 to Fstrata - 1 do
    begin
      FStrataName[stratum] := T.RowVariable.GetValueLabelFormatted(stratum, FValueLabelOutput);
      for col := 0 to T.ColCount - 1 do
        begin
          FBins[getSlot(T.ColVariable.AsInteger[col]), stratum] += T.Cell[col, stratum].N;
        end;
      setMaxCount(stratum);
    end;
  FTotal := T.Total;
end;

procedure THistogram.HistogramToEpicurve;
var
  i, j, k, ioffset, stratum, maxboxes ,oldstrata: Integer;
  offset: array of Integer;
begin
  // change counts within a stratum to series of ones
  // filling in with zeros to make the number of Y points the same within strata
  // first, get maximum number of boxes for all strata
  maxboxes := 0;
  oldstrata := FStrata - 1;
  setLength(offset, FStrata);
  for stratum := 0 to FStrata - 1 do
    begin
      offset[stratum] := maxboxes;
      maxboxes += FMaxCounts[stratum];
    end;
  // resize FBins
  setLength(FBins, FCount, maxboxes);
  FStrata := maxboxes;
  // shift counts into series of ones
  for stratum := oldstrata downto 0 do
    begin
      ioffset :=  offset[stratum];
      for i := 0 to FCount - 1 do
        begin
          k := FBins[i, stratum];
          for j := ioffset to ioffset + k -1 do
            FBins[i, j] := 1;
          for j := ioffset + k to maxBoxes - 1 do
            FBins[i, j] := 0;
        end;
      maxBoxes := maxBoxes - FMaxCounts[stratum];
    end;
end;

destructor THistogram.Destroy;
begin
  FBins := nil;
  inherited Destroy;
end;

end.
