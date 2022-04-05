unit savegraphaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, TAGraph, Graphics;

type

  { TSaveGraphAction }

  TGraphExportType = (
    etSVG,
    etPNG,
    etJPG
  );

  { TCustomSaveGraphAction }

  TCustomSaveGraphAction = class(TCustomAction)
  private
    FChart: TChart;
    FFilename: UTF8String;
    FGraphExportType: TGraphExportType;
    procedure SaveToRaster(ImageClass: TRasterImageClass; Filename: UTF8String);
    procedure SetChart(AValue: TChart);
    procedure SetFilename(AValue: UTF8String);
  protected
    procedure UpdateReadyState(); virtual;
    procedure SaveGraphExecute(Sender: TObject); virtual;
    property Chart: TChart read FChart write SetChart;
    property GraphExportType: TGraphExportType read FGraphExportType write FGraphExportType;
    property Filename: UTF8String read FFilename write SetFilename;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSaveGraphAction = class(TCustomSaveGraphAction)
  public
    property Chart;
    property GraphExportType;
    property Filename;
  end;

implementation

uses
  TADrawerSVG;

{ TSaveGraphAction }

procedure TCustomSaveGraphAction.SaveGraphExecute(Sender: TObject);
begin
  case GraphExportType of
    etSVG: FChart.SaveToSVGFile(FileName);
    etPNG: SaveToRaster(TPortableNetworkGraphic, FileName);
    etJPG: SaveToRaster(TJPEGImage, FileName);
  end;
end;

procedure TCustomSaveGraphAction.SetChart(AValue: TChart);
begin
  if FChart = AValue then Exit;
  FChart := AValue;

  UpdateReadyState();
end;

procedure TCustomSaveGraphAction.SetFilename(AValue: UTF8String);
begin
  if FFilename = AValue then Exit;
  FFilename := AValue;

  UpdateReadyState();
end;

procedure TCustomSaveGraphAction.UpdateReadyState();
begin
  Enabled := (FFilename <> '') and
             (Assigned(FChart));
end;

procedure TCustomSaveGraphAction.SaveToRaster(ImageClass: TRasterImageClass;
  Filename: UTF8String);
var
  Image: TRasterImage;
begin
  Image := ImageClass.Create;
  Image.Width := 1024;
  Image.Height := 768;
  FChart.PaintOnCanvas(Image.Canvas, Rect(0, 0, Image.Width, Image.Height));
  Image.SaveToFile(Filename);
  Image.Free;
end;

constructor TCustomSaveGraphAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGraphExportType := etSVG;

  OnExecute := @SaveGraphExecute;
  Caption := 'Save as ...';
end;

end.

