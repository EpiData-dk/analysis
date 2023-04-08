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
//    FValue,
//    FPct:    Boolean;
//    FDatafile: TEpiDataFile;
//    FYVariableName: UTF8String;
//    FYVar: TEpiField;
    FData: array of array of double;
//    FTable: TTwoWayTable;
//    FValueLabelOutput: TEpiGetValueLabelType;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
//    procedure SetDatafile(AValue: TEpiDataFile);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetSource(T: TTwoWayTable; Pct: Boolean);
    procedure SetSource(DF: TEpiDataFile; XVarName: UTF8String; YVarNames: array of UTF8String);
//    procedure SetYVariableName(AValue: UTF8String);
    destructor Destroy; override;
//    property Pct: Boolean read FPct write FPct;
//    property YVariableName: UTF8String read FYVariableName write SetYVariableName;
//    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
  end;

implementation

{ TBarSource }

procedure TBarSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  i: Integer;
begin
  AItem.X := AIndex.ToDouble;
 { if (FValue) then
    begin
      if (FYVar.IsMissing[AIndex]) then
        AItem.Y := 0
      else
        AItem.Y := FYVar.AsFloat[AIndex];
      exit;
    end;
}
{  if (FPct) then
    begin
      if (Ycount > 1) then
        // % with strata is column % (proportion of x-var that is in each stratum)
        for i := 0 to YCount - 1 do
          AItem.SetY(i, 100 * FTable.Cell[AIndex, i].ColPct)
      else
        // % with no strata is row % (proportion of each x value count overall)
        AItem.Y := 100 * FTable.Cell[AIndex, 0].RowPct;
    end
  else  }
    for i := 0 to YCount - 1 do
      AItem.SetY(i, FData[AIndex, i]);
end;

procedure TBarSource.SetSource(T: TTwoWayTable; Pct: Boolean);
var
  x, y: Integer;
begin
//  FValueLabelOutput := V;
//  FTable  := T;
  YCount := T.RowCount;
  PointsNumber := T.ColCount;
  setlength(FCount, YCount, PointsNumber);
  for y := 0 to YCount - 1 do
    for x := 0 to PointsNumber - 1 do
      if (Pct) then
        if (YCount = 1) then
          FData[y, x] := 100 * FTable.Cell[x, 0].RowPct
        else
          FData[y, x] := 100 * FTable.Cell[x, y].ColPct
      else
        FData[y, x] := FTable.Cell[x, y].N.ToDouble;

end;

procedure TBarSource.SetSource(DF: TEpiDataFile; XVarName: UTF8String; YVarNames: array of UTF8String);
var
  x, y: Integer;
  yVar: TEpiField;
  yValues: array of TEpiFloat;
  xVar: TEpiField;
begin
  // X variable should not be missing, but will be ignored if it is
  // Each value of X should be have unique value for each Y; otherwise only the first will be used
  YCount := length(YVarNames);
  PointsNumber := DF.Size;
  setlength(FCount, YCount);
  setLength(yValues, PointsNumber);
  xVar := DF.Fields.FieldByName[XVarName];
  for x := 0 to PointsNumber - 1 do
    begin
      for y := 0 to YCount - 1 do
        begin
          yVar := DF.Fields.FieldByName[YVarNames[y]];
          if yVar.IsMissing[x] then
            FData[y, x] := 0
          else
            FData[y, x] := yVar.AsFloat[x];
        end;
    end;
end;

{procedure TBarSource.SetYVariableName(AValue: UTF8String);
begin
  if FYVariableName=AValue then Exit;
  FYVariableName:=AValue;
  FYVar := FDatafile.Fields.FieldByName[FYVariableName];
  FTable  := nil;
  YCount := 1;
  FValue := true;
end;
}
{procedure TBarSource.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;
  PointsNumber := FDatafile.Size;
end;
}
constructor TBarSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TBarSource.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

end.
