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
    procedure SetValueSource(DF: TEpiDataFile; XVarName: UTF8String; YVarName: UTF8String);
    procedure GetCountData(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure GetValueData(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    destructor Destroy; override;
  end;

implementation

{ TBarSource }

procedure TBarSource.GetCountData(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  iy: Integer;
begin
  AItem.X := AIndex.ToDouble;
    for iy := 0 to YCount - 1 do
      AItem.SetY(iy, FData[AIndex, iy]);
end;

procedure TBarSource.GetValueData(ASource: TUserDefinedChartSource; AIndex: Integer;
  var AItem: TChartDataItem);
begin
  AItem.X := AIndex.ToDouble;
  AItem.Y := FData[AIndex, 0];
end;

procedure TBarSource.SetCountSource(T: TTwoWayTable; Pct: Boolean);
var
  ix, iy: Integer;
begin
  YCount := T.RowCount;
  PointsNumber := T.ColCount;
  setlength(FData, PointsNumber, YCount);
  for ix := 0 to PointsNumber - 1 do
    for  iy := 0 to YCount - 1 do
      if (Pct) then
        if (YCount = 1) then
          FData[ix, iy] := 100 * T.Cell[ix, 0].RowPct
        else
          FData[ix, iy] := 100 * T.Cell[ix, iy].ColPct
      else
        FData[ix, iy] := T.Cell[ix, iy].N.ToDouble;
  OnGetChartDataItem := @GetCountData;
end;

procedure TBarSource.SetValueSource(DF: TEpiDataFile; XVarName, YVarName: UTF8String);
var
  ix: Integer;
  yVar: TEpiField;
  xVar: TEpiField;
begin
  // Each value of X should be unique, but the source doesn't care; just takes the x values as they come
  YCount := 1;
  PointsNumber := DF.Size;
  setLength(FData, PointsNumber, 1);
  xVar := DF.Fields.FieldByName[XVarName];
  yVar := DF.Fields.FieldByName[YVarName];
  for ix := 0 to PointsNumber - 1 do
    if yVar.IsMissing[ix] then
      FData[ix, 0] := 0       // this should be manageable by user... (e.g. XMin)
    else
      FData[ix, 0] := yVar.AsFloat[ix];
  OnGetChartDataItem := @GetValueData;
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
