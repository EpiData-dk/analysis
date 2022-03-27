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
    FYVar: TEpiField;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure SetDatafile(AValue: TEpiDataFile);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
  end;

implementation

{ TScatterSource }

procedure TScatterSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FXVar.AsFloat[AIndex];
  AItem.Y := FYVar.AsFloat[AIndex];
end;

procedure TScatterSource.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;

  PointsNumber := FDatafile.Size;
  FXVar := FDataFile.Field[0];
  FYVar := FDataFile.Field[1];
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

