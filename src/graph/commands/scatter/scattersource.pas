unit scattersource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TACustomSource, epidatafiles;

type

  { TScatterSource }

  TScatterSource = class(TUserDefinedChartSource)
  private
    FDatafile: TEpiDataFile;
    FXVar: TEpiField;
    FXVariableName: UTF8String;
    FYVar: TEpiField;
    FYVariableName: UTF8String;
    FOffset,
    FSize: Integer;
   procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure SetDatafile(AValue: TEpiDataFile);
    procedure SetXVariableName(AValue: UTF8String);
    procedure SetYVariableName(AValue: UTF8String);
    procedure SetSize(AValue : Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property XVariableName: UTF8String read FXVariableName write SetXVariableName;
    property YVariableName: UTF8String read FYVariableName write SetYVariableName;
    property Offset: Integer write FOffset;
    property Size:   Integer write SetSize;
  end;

implementation

{ TScatterSource }

procedure TScatterSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FXVar.AsFloat[AIndex + FOffset];
  AItem.Y := FYVar.AsFloat[AIndex + FOffset];
end;

procedure TScatterSource.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;

  PointsNumber := FDatafile.Size;
end;

procedure TScatterSource.SetSize(AValue : Integer);
begin
  PointsNumber := AValue;
end;

procedure TScatterSource.SetXVariableName(AValue: UTF8String);
begin
  if FXVariableName=AValue then Exit;
  FXVariableName:=AValue;

  FXVar := FDatafile.Fields.FieldByName[FXVariableName];
end;

procedure TScatterSource.SetYVariableName(AValue: UTF8String);
begin
  if FYVariableName=AValue then Exit;
  FYVariableName:=AValue;

  FYVar := FDatafile.Fields.FieldByName[FYVariableName];
end;

constructor TScatterSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TScatterSource.Destroy;
begin
  FXVar := nil;
  FYVar := nil;
  FreeAndNil(FDatafile);
  inherited Destroy;
end;

end.

