unit savegraphaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, TAGraph, Graphics, Types;

type

  { TSaveGraphAction }

  TGraphExportType = (
    etNone,
    etSVG,
    etPNG,
    etJPG
  );

const
  ExportExt: array[TGraphExportType] of string = (
    '',
    '.svg',
    '.png',
    '.jpg'
  );

type

  EIncorrectGraphExtension = class(Exception);

  { TCustomSaveGraphAction }

  TCustomSaveGraphAction = class(TCustomAction)
  private
    FChart: TChart;
    FFilename: UTF8String;
    FGraphExportType: TGraphExportType;
    FGraphSize: TSize;
    procedure SaveToRaster(ImageClass: TRasterImageClass; Filename: UTF8String);
    procedure SetChart(AValue: TChart);
    procedure SetFilename(AValue: UTF8String);
    procedure UpdateExportType();
  protected
    procedure UpdateReadyState(); virtual;
    procedure SaveGraphExecute(Sender: TObject); virtual;
    property Chart: TChart read FChart write SetChart;
    property GraphExportType: TGraphExportType read FGraphExportType write FGraphExportType;
    property GraphSize: TSize read FGraphSize write FGraphSize;
    property Filename: UTF8String read FFilename write SetFilename;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSaveGraphAction = class(TCustomSaveGraphAction)
  public
    property Chart;
    property GraphExportType;
    property Filename;
    property GraphSize;
  end;

implementation

uses
  TADrawerSVG, LazFileUtils;

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
  FFilename := ExpandFileNameUTF8(AValue);

  UpdateExportType();
  UpdateReadyState();
end;

procedure TCustomSaveGraphAction.UpdateExportType();
var
  Ext: UTF8String;
begin
  if (FFilename = '') then
    Exit;

  if (FGraphExportType <> etNone) then
    Exit;


  Ext := ExtractFileExt(FFilename);
  case Ext of
    '.svg':
      FGraphExportType := etSVG;
    '.png':
      FGraphExportType := etPNG;
    '.jpg',
    '.jpeg':
      FGraphExportType := etJPG;
  else
    raise EIncorrectGraphExtension.Create('"' + ext + '" is not a supported filetype!');
  end;
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
  Image.Width := FGraphSize.Width;
  Image.Height := FGraphSize.Height;
  FChart.PaintOnCanvas(Image.Canvas, Rect(0, 0, Image.Width, Image.Height));
  Image.SaveToFile(Filename);
  Image.Free;
end;

constructor TCustomSaveGraphAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGraphExportType := etNone;
  FGraphSize := TSize.Create(1024, 768);

  OnExecute := @SaveGraphExecute;
  Caption := 'Save as ...';
end;

end.

