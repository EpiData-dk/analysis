unit histogramdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TASources, TALegend, tables_types, tables, freq;

type

  countArray = array of Double;
  bin        = record
    name:      UTF8String;
    count:     integer;
    max:       integer;
    n:         countArray;
  end;
  bins       = array of bin;

  { THistogram }

  THistogram = class(TObject)
    private
      FExecutor: TExecutor;
      FOutputCreator: TOutputCreator;
      FValueLabelOutput: TEpiGetValueLabelType;
      FBins:    bins;
      FBase:    integer;   // lowest x value
      FCount:   Integer;   // highest x - lowest x + 1
      FStrata:  Integer;
      FSlotValue: countArray;
      FSlotMap: array of Integer;
      function getIndex(i: Integer): Integer;
      function getSlot(i: Integer):  countArray;
      function getSlotValue(i: Integer): Double;
      function getName(i: Integer): UTF8String;
      function getMax(i: Integer):   Integer;
      procedure initSlotMap;
    public
      constructor Create(Executor: TExecutor; Command: TCustomGraphCommand);
      destructor Destroy; override;
      property Base:              Integer read FBase;
      property Strata:            Integer read FStrata;
      property Count:             Integer read FCount;
      property Slot[i: Integer]:  countArray read getSlot;
      property SlotValue[i: Integer]: Double read getSlotValue;
      property Name[i: Integer]: UTF8String read getName;
      property Max[i: Integer]:   Integer read getMax;
      procedure Fill(F: TFreqDataFile);
      procedure Fill(T: TTwoWayTable);
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
end;

function THistogram.getIndex(i: Integer): Integer;
begin
  if (i < 0) or (i >= FCount) then
    result := -1
  else
    result := FSlotMap[i];
end;

procedure THistogram.initSlotMap;
var
  i: Integer;
begin
  for i := low(FSlotMap) to high(FSlotMap) do
    FSlotMap[i] := -1;
end;

function THistogram.getSlot(i: Integer): countArray;
var
  stratum, index: Integer;
  aSlot: array of double;
begin
  index := getIndex(i);
  if (index < 0) then
    result := [0]
  else
    begin
      setLength(result, FStrata);
      for stratum := 0 to FStrata - 1 do
        result[stratum] := FBins[stratum].N[index];
    end;
end;

function THistogram.getSlotValue(i: Integer): Double;
var
  index: Integer;
begin
  index := getIndex(i);
  if (index < 0) then
    result := 0
  else
    result := FSlotValue[index];
end;

function THistogram.getName(i: Integer): UTF8String;
var
  index: Integer;
begin
  index := getIndex(i);
  if (index < 0) then
    result := ''
  else
    result := FBins[index].name;
end;

function THistogram.getMax(i: Integer): Integer;
var
  index: Integer;
begin
  index := getIndex(i);
  if (index < 0) then
    result := 0
  else
    result := FBins[index].max;
end;

procedure THistogram.Fill(F: TFreqDataFile);
var
  i, n: Integer;
begin
  Fstrata   := 1;
  FBase     := F.Categ.AsInteger[0];
  FCount    := F.Categ.AsInteger[F.Categ.Size - 1] - FBase + 1;
  setLength(FBins, 1);
  setLength(FSlotMap, FCount);
  initSlotMap;

  // set up Bin[0] information and slotMap
  FBins[0].name := '';
  n := 0;
  setLength(FBins[0].N, F.Categ.Size);
  setLength(FSlotValue, F.Categ.Size);
  for i := 0 to F.Categ.Size - 1 do
    begin
      n := Math.Max(n, F.Count.AsInteger[i]);
      FBins[0].N[i] := F.Count.AsFloat[i];
      FSlotValue[i] := F.Categ.AsFloat[i];
      FSlotMap[F.Categ.AsInteger[i] - FBase] := i;
    end;
  FBins[0].Max := n;
end;

procedure THistogram.Fill(T: TTwoWayTable);
var
  i, col, row, n: Integer;
begin
  Fstrata := T.RowCount;
  setLength(FBins, Fstrata);
  FBase := T.ColVariable.AsInteger[0];
  FCount := T.ColVariable.AsInteger[T.ColCount - 1] - FBase + 1;
  setLength(FBins, T.RowCount);
  setLength(FSlotMap, FCount);
  setLength(FSlotValue, FCount);
  initSlotMap;

  // set up SlotMap
  for col := 0 to T.ColCount - 1 do
    begin
      FSlotMap[T.ColVariable.AsInteger[col] - FBase] := col;
      FSlotValue[col] := T.ColVariable.AsFloat[col];
    end;

  // get strata information and fill frequencies
  for row := 0 to Fstrata - 1 do
    begin
      setLength(FBins[row].N, FCount);
      FBins[row].name := T.RowVariable.GetValueLabelFormatted(row, FValueLabelOutput);
      n := 0;
      for col := 0 to T.ColCount - 1 do
        begin
          FBins[row].N[col] := T.Cell[col, row].N.ToDouble;
          n := Math.Max(n, T.Cell[col, row].N);
        end;
      FBins[row].Max := n;
    end;
end;

destructor THistogram.Destroy;
begin
  inherited Destroy;
  FBins := nil;
end;

end.
