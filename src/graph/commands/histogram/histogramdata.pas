unit histogramdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TASources, TALegend, tables_types, tables, freq;

type

  countArray = array of Integer;

  { THistogram }

  THistogram = class(TObject)
    private
      FExecutor: TExecutor;
      FOutputCreator: TOutputCreator;
      FValueLabelOutput: TEpiGetValueLabelType;
      FBins:    array of countArray;
      FBase:    integer;   // lowest x value
      FXMax:    integer;   // highest x value
      FCount:   Integer;   // (highest x - lowest x)/interval + 1
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
      procedure setInterval(i : Integer);
      procedure initBins;
      procedure initXValues;
    public
      constructor Create(Executor: TExecutor; Command: TCustomGraphCommand);
      destructor Destroy; override;
      property Base:     Integer read FBase write FBase;
      property Interval: Integer read getInterval write setInterval;
      property Strata:   Integer read FStrata;
      property Count:    Integer read FCount;
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
  TASeries, TATypes, TAStyles, Graphics, charttitles, ast_types, math,
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
end;

procedure THistogram.initBins;
var
  i, j: Integer;
begin
  for i := low(FBins) to high(FBins) do
    for j := low(FBins[0]) to high(FBins[0]) do
      FBins[i,j] := 0;
end;

procedure THistogram.initXValues;
var
  i: Integer;
begin
  for i := 0 to high(FXvalue) do
    FXValue[i] :=  (FBase + i) div FInterval;
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
  if (i < 0) or (i > (FXMax - FBase)) then
    exit; // error - interval is too wide
  if (FCount > 0) then
    exit; // error - must set interval before filling slots
  FInterval := i;
end;

procedure THistogram.Fill(F: TFreqDataFile);
var
  i, n: Integer;
  xMin, xMax: Double;
  slot: Integer;
begin
  Fstrata   := 1;
  FBase     := F.Categ.AsInteger[0];
  FXMax     := F.Categ.AsInteger[F.Categ.Size - 1];
  xMin      := FBase.ToDouble;
  xMax      := FXMax.ToDouble;
  FCount    := round((xMax - xMin) / FInterval) + 1;
  setLength(FStrataName, 1);
  setLength(FBins, FCount, 1);
  initBins;
  setLength(FXValue, FCount);
  initXValues;
  setLength(FMaxCounts, 1);

  // set up Bins[*,0] information and slotMap
  setLength(FStrataName, 1);
  FStrataName[0] := '';
//  n := 0;
  for i := 0 to F.Categ.Size - 1 do
    begin
      slot := round((F.Categ.AsInteger[i] - FBase).ToDouble / FInterval);
      FBins[slot, 0] += F.Count[i];
    end;
// get max height of slots
  n := 0;
  for i := 0 to high (FBins) do
    n := Math.Max(n, FBins[i,0]);
  FMaxCounts[0] := n;
end;

procedure THistogram.Fill(T: TTwoWayTable);
var
  i, col, stratum, n, slot: Integer;
  xmin, xmax: Double;
begin
  FBase := T.ColVariable.AsInteger[0];
  xMin := FBase.ToDouble;
  xMax := T.ColVariable.AsFloat[T.ColCount - 1];
  FCount := round((xMax - xMin) / FInterval) + 1;
  FStrata := T.RowCount;
  setLength(FBins, FCount, Fstrata);
  setLength(FXValue, FCount);
  setLength(FMaxCounts, FStrata);
  setLength(FStrataName, FStrata);
  initXValues;

// get strata information and fill frequencies
  for stratum := 0 to Fstrata - 1 do
    begin
      FStrataName[stratum] := T.RowVariable.GetValueLabelFormatted(stratum, FValueLabelOutput);
      for col := 0 to T.ColCount - 1 do
        begin
          slot :=round((T.ColVariable.AsFloat[col] - xMin) / FInterval);
          FBins[slot, stratum] := T.Cell[col, stratum].N;
        end;
      n := 0;
      for col := 0 to FCount - 1 do
        n := Math.Max(n, FBins[col, stratum]);
      FMaxCounts[stratum] := n;
    end;
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
  inherited Destroy;
  FBins := nil;
end;

end.
