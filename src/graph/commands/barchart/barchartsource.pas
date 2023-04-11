unit barchartsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TACustomSource, executor,
  epidatafiles, epifields_helper, tables_types, tables;

type

  { TBarSource }

  TBarSource = class(TUserDefinedChartSource)
  private
    FData: array of array of double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetCountSource(T: TTwoWayTable; Pct: Boolean);
    procedure SetValueSource(DF: TEpiDataFile; VarNames: TStrings);
    procedure GetData(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    destructor Destroy; override;
  end;

implementation

{ TBarSource }

procedure TBarSource.GetData(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  iy: Integer;
begin
  AItem.X := AIndex.ToDouble;
    for iy := 0 to YCount - 1 do
      AItem.SetY(iy, FData[AIndex, iy]);
end;

procedure TBarSource.SetCountSource(T: TTwoWayTable; Pct: Boolean);
var
  ix, iy: Integer;
begin
  YCount := T.RowCount;
  PointsNumber := T.ColCount;
  SetLength(FData, T.ColCount, T.RowCount);
  for ix := 0 to PointsNumber - 1 do
    for  iy := 0 to YCount - 1 do
      if (Pct) then
        if (YCount = 1) then
          FData[ix, iy] := 100 * T.Cell[ix, 0].RowPct
        else
          FData[ix, iy] := 100 * T.Cell[ix, iy].ColPct
      else
        FData[ix, iy] := T.Cell[ix, iy].N.ToDouble;
  OnGetChartDataItem := @GetData;
end;

procedure TBarSource.SetValueSource(DF: TEpiDataFile; VarNames: TStrings); //, YVarName: UTF8String);
var
  ix, iy: Integer;
  yVar: TEpiField;
  xVar: TEpiField;
begin
  YCount := VarNames.Count - 1;
  PointsNumber := DF.Size;
  SetLength(FData, DF.Size, VarNames.Count - 1);
  xVar := DF.Fields.FieldByName[VarNames[0]];
  for iy := 0 to YCount - 1 do
    begin
    yVar := DF.Fields.FieldByName[VarNames[iy + 1]];
    for ix := 0 to PointsNumber - 1 do
      if yVar.IsMissing[ix] then
      // TODO: this should be manageable by user... e.g. use XMin or calculated min(x)
        FData[ix, iy] := 0
      else
        FData[ix, iy] := yVar.AsFloat[ix];
    end;
  OnGetChartDataItem := @GetData;

end;

constructor TBarSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBarSource.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

end.
