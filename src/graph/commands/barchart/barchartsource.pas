unit barchartsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TACustomSource, executor,
  epidatafiles, epifields_helper, tables_types, tables, freq;

type

  { TBarSource }

  TBarSource = class(TUserDefinedChartSource)
  private
    FPct:    Boolean;
    FFreqs:  Boolean;
    FFreq: TFreqDataFile;
    FTable: TTwoWayTable;
    FValueLabelOutput: TEpiGetValueLabelType;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
  public
    property Pct: Boolean read FPct write FPct;
    constructor Create(AOwner: TComponent); override;
    procedure SetSource(F: TFreqDataFile);
    procedure SetSource(T: TTwoWayTable; V: TEpiGetValueLabelType);
    destructor Destroy; override;
  end;

implementation

{ TBarSource }

procedure TBarSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  i: Integer;
begin
  if (FFreqs) then
    begin
      AItem.X := AIndex.ToDouble;
      if (FPct) then
        AItem.Y := FFreq.Percent.AsFloat[AIndex]
      else
        AItem.Y := FFreq.Count.AsFloat[AIndex];
    end
  else
    begin
      AItem.X := AIndex.ToDouble;
      if (FPct) then
        for i := 0 to YCount - 1 do
          AItem.SetY(i, 100 * FTable.Cell[AIndex, i].ColPct)
      else
        for i := 0 to YCount - 1 do
          AItem.SetY(i, FTable.Cell[AIndex, i].N.ToDouble);
    end;
end;

procedure TBarSource.SetSource(F: TFreqDataFile);
begin
  FFreqs := true;
  FFreq  := F;
  YCount := 1;
  PointsNumber := F.Count.Size;
end;

procedure TBarSource.SetSource(T: TTwoWayTable; V: TEpiGetValueLabelType);
begin
  FFreqs := false;
  FValueLabelOutput := V;
  FTable  := T;
  YCount := T.RowCount;
  PointsNumber := T.ColCount;
end;

constructor TBarSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TBarSource.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTable);
  FreeAndNil(FFreq);
end;

end.
