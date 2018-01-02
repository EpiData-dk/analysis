unit interval_types;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epicustombase, epidatafilestypes;

type

  { TIntervalDecriptivesDatafile }

  TIntervalDecriptivesDatafile = class(TEpiDataFile)
  private
    FAvgDev: TEpiField;
    FCategory: TEpiField;
    FCfiH: TEpiField;
    FCfiL: TEpiField;
    FCurt: TEpiField;
    FMax: TEpiField;
    FMean: TEpiField;
    FMedian: TEpiField;
    FMin: TEpiField;
    FN: TEpiField;
    FP10: TEpiField;
    FP25: TEpiField;
    FP5: TEpiField;
    FP75: TEpiField;
    FP90: TEpiField;
    FP95: TEpiField;
    FSkew: TEpiField;
    FStdDev: TEpiField;
    FStdErr: TEpiField;
    FStdVar: TEpiField;
    FSum: TEpiField;
    FSumSq: TEpiField;
    FSumSS: TEpiField;
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    property Sum:      TEpiField read FSum;
    property SumSq:    TEpiField read FSumSq;
    property SumSS:    TEpiField read FSumSS;
    property Mean:     TEpiField read FMean;
    property AvgDev:   TEpiField read FAvgDev;
    property StdVar:   TEpiField read FStdVar;
    property StdDev:   TEpiField read FStdDev;
    property StdErr:   TEpiField read FStdErr;
    property Skew:     TEpiField read FSkew;
    property Curt:     TEpiField read FCurt;
    property Min:      TEpiField read FMin;
    property P5:       TEpiField read FP5;
    property P10:      TEpiField read FP10;
    property P25:      TEpiField read FP25;
    property Median:   TEpiField read FMedian;
    property P75:      TEpiField read FP75;
    property P90:      TEpiField read FP90;
    property P95:      TEpiField read FP95;
    property Max:      TEpiField read FMax;
    property CfiL:     TEpiField read FCfiL;
    property CfiH:     TEpiField read FCfiH;
    property N:        TEpiField read FN;
    property Category: TEpiField read FCategory;
  end;

  TAnovaRecord = record
    SSB,       // Sum of Squares between instruments
    MSB,       // Mean squares between instruments
    SSW,       // Sum of squares within instruments
    MSW,       // Mean squares within instruments
    SST,
    MST:
      EpiFloat;
    DFB,       // Degrees of freedom between instruments
    DFW,       // Degrees of freedom within instruments
    DFT:
      EpiInteger;
    F,         // F statistics
    PROB:      //
      EpiFloat;
  end;

implementation

uses
  strutils;

{ TIntervalDecriptivesDatafile }

constructor TIntervalDecriptivesDatafile.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);

  FCategory := NewField(ftString);
  FN        := NewField(ftInteger);

  FAvgDev   := NewField(ftFloat);
  FCfiH     := NewField(ftFloat);
  FCfiL     := NewField(ftFloat);
  FCurt     := NewField(ftFloat);
  FMax      := NewField(ftFloat);
  FMean     := NewField(ftFloat);
  FMedian   := NewField(ftFloat);
  FMin      := NewField(ftFloat);
  FP10      := NewField(ftFloat);
  FP25      := NewField(ftFloat);
  FP5       := NewField(ftFloat);
  FP75      := NewField(ftFloat);
  FP90      := NewField(ftFloat);
  FP95      := NewField(ftFloat);
  FSkew     := NewField(ftFloat);
  FStdDev   := NewField(ftFloat);
  FStdErr   := NewField(ftFloat);
  FStdVar   := NewField(ftFloat);
  FSum      := NewField(ftFloat);
  FSumSq    := NewField(ftFloat);
  FSumSS    := NewField(ftFloat);
end;

end.

