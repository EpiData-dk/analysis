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
    FValue,
    FPct:    Boolean;
    FDatafile: TEpiDataFile;
    FYVariableName: UTF8String;
    FYVar: TEpiField;
    FTable: TTwoWayTable;
    FValueLabelOutput: TEpiGetValueLabelType;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure SetDatafile(AValue: TEpiDataFile);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetSource(T: TTwoWayTable; V: TEpiGetValueLabelType);
    procedure SetYVariableName(AValue: UTF8String);
    destructor Destroy; override;
    property Pct: Boolean read FPct write FPct;
    property YVariableName: UTF8String read FYVariableName write SetYVariableName;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
  end;

implementation

{ TBarSource }

procedure TBarSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  i: Integer;
begin
  AItem.X := AIndex.ToDouble;
  if (FValue) then
    begin
      if (FYVar.IsMissing[AIndex]) then
        AItem.Y := 0
      else
        AItem.Y := FYVar.AsFloat[AIndex];
      exit;
    end;
  if (FPct) then
    for i := 0 to YCount - 1 do
      AItem.SetY(i, 100 * FTable.Cell[AIndex, i].ColPct)
  else
    for i := 0 to YCount - 1 do
      AItem.SetY(i, FTable.Cell[AIndex, i].N.ToDouble);
end;

procedure TBarSource.SetSource(T: TTwoWayTable; V: TEpiGetValueLabelType);
begin
  FValueLabelOutput := V;
  FTable  := T;
  YCount := T.RowCount;
  PointsNumber := T.ColCount;
  FValue := false;
  FDataFile := nil;
  FYVar := nil;
end;

procedure TBarSource.SetYVariableName(AValue: UTF8String);
begin
  if FYVariableName=AValue then Exit;
  FYVariableName:=AValue;
  FYVar := FDatafile.Fields.FieldByName[FYVariableName];
  FTable  := nil;
  YCount := 1;
  FValue := true;
end;

procedure TBarSource.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;
  PointsNumber := FDatafile.Size;
end;

constructor TBarSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TBarSource.Destroy;
begin
  FreeAndNil(FTable);
  inherited Destroy;
end;

end.
