unit epicurvesource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TACustomSource, epidatafiles;

type

  { TEpicurveSource }

  TEpicurveSource = class(TUserDefinedChartSource)
  private
    FDatafile: TEpiDataFile;
    FXVar: TEpiField;
    FXVariableName: UTF8String;
    FByVar: TEpiField;
    FByVariableName: UTF8String;
    procedure GetDataItem(ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure SetDatafile(AValue: TEpiDataFile);
    procedure SetXVariableName(AValue: UTF8String);
    procedure SetByVariableName(AValue: UTF8String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property XVariableName: UTF8String read FXVariableName write SetXVariableName;
    property ByVariableName: UTF8String read FByVariableName write SetByVariableName;
  end;

implementation

{ TEpicurveSource }

procedure TEpicurveSource.GetDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FXVar.AsFloat[AIndex];
  AItem.Y := FByVar.AsFloat[AIndex];
end;

procedure TEpicurveSource.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;

  PointsNumber := FDatafile.Size;
end;

procedure TEpicurveSource.SetXVariableName(AValue: UTF8String);
begin
  if FXVariableName=AValue then Exit;
  FXVariableName:=AValue;

  FXVar := FDatafile.Fields.FieldByName[FXVariableName];
end;

procedure TEpicurveSource.SetByVariableName(AValue: UTF8String);
begin
  if FByVariableName=AValue then Exit;
  FByVariableName:=AValue;

  FByVar := FDatafile.Fields.FieldByName[FByVariableName];
end;

constructor TEpicurveSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnGetChartDataItem := @GetDataItem;
end;

destructor TEpicurveSource.Destroy;
begin
  FXVar := nil;
  FByVar := nil;
  FreeAndNil(FDatafile);
  inherited Destroy;
end;

end.
