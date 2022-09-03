unit barchartsource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TACustomSource, epidatafiles;

type

  { TBarSource }

  TBarSource = class(TUserDefinedChartSource)
  private
    FDatafile: TEpiDataFile;
    FXVar: TEpiField;
    FXVariableName: UTF8String;
    FYVar: TEpiField;
    FYVariableName: UTF8String;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure SetDatafile(AValue: TEpiDataFile);
    procedure SetXVariableName(AValue: UTF8String);
    procedure SetYVariableName(AValue: UTF8String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property XVariableName: UTF8String read FXVariableName write SetXVariableName;
    property YVariableName: UTF8String read FYVariableName write SetYVariableName;
  end;

implementation

{ TBarSource }

procedure TBarSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FXVar.AsFloat[AIndex];
  AItem.Y := FYVar.AsFloat[AIndex];
end;

procedure TBarSource.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;

  PointsNumber := FDatafile.Size;
end;

procedure TBarSource.SetXVariableName(AValue: UTF8String);
begin
  if FXVariableName=AValue then Exit;
  FXVariableName:=AValue;

  FXVar := FDatafile.Fields.FieldByName[FXVariableName];
end;

procedure TBarSource.SetYVariableName(AValue: UTF8String);
begin
  if FYVariableName=AValue then Exit;
  FYVariableName:=AValue;

  FYVar := FDatafile.Fields.FieldByName[FYVariableName];
end;

constructor TBarSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TBarSource.Destroy;
begin
  FXVar := nil;
  FYVar := nil;
  FreeAndNil(FDatafile);
  inherited Destroy;
end;

end.
