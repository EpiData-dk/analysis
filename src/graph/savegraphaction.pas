unit savegraphaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, TAGraph, Graphics, Types, ChartPair;

type

  { TSaveGraphAction }

  TGraphExportType = (
    etSVG,
    etPNG,
    etJPG
  );

const
  ExportExt: array[TGraphExportType] of string = (
    '.svg',
    '.png',
    '.jpg'
  );

type

  EIncorrectGraphExtension = class(Exception);

  { TCustomSaveGraphAction }

  TCustomSaveGraphAction = class(TCustomAction)
  private
    FCharts: array of TChart;
    FCount: Integer;
    FFilename: UTF8String;
    FExtensionOK: Boolean;
    FGraphExportType: TGraphExportType;
    FGraphSize: TSize;
    procedure SaveToRaster(ImageClass: TRasterImageClass; Filename: UTF8String);
    procedure SaveToSVG(Filename: UTF8String);
    procedure SetFilename(AValue: UTF8String);
    procedure UpdateExportType();
    function GetFileName(AValue: UTF8String; AIndex: Integer): UTF8String;
  protected
    procedure UpdateReadyState(); virtual;
    procedure SaveGraphExecute(Sender: TObject); virtual;
    property GraphExportType: TGraphExportType read FGraphExportType write FGraphExportType;
    property GraphSize: TSize read FGraphSize write FGraphSize;
    property Filename: UTF8String read FFilename write SetFilename;
    property ExtensionOK: Boolean read FExtensionOK write FExtensionOK;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddChart(AValue: TChart);
    destructor Destroy; override;
  end;

  TSaveGraphAction = class(TCustomSaveGraphAction)
  public
    property GraphExportType;
    property Filename;
    property GraphSize;
  end;

implementation

uses
  TADrawerSVG, LazFileUtils {$IFDEF DARWIN}, TAFonts{$ENDIF};

{ TSaveGraphAction }

procedure TCustomSaveGraphAction.SaveGraphExecute(Sender: TObject);
begin
  case GraphExportType of
    etSVG: SaveToSVG(FileName);
    etPNG: SaveToRaster(TPortableNetworkGraphic, FileName);
    etJPG: SaveToRaster(TJPEGImage, FileName);
  end;
end;

procedure TCustomSaveGraphAction.AddChart(AValue: TChart);
begin
  inc(FCount);
  setLength(FCharts, FCount);
  FCharts[FCount - 1] := AValue;

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
  ext: UTF8String;
begin
  if (FFilename = '') then
    Exit;
  ext := ExtractFileExt(FFilename);
  FExtensionOK := true;
  case ext of
    '.svg':
      FGraphExportType := etSVG;
    '.png':
      FGraphExportType := etPNG;
    '.jpg',
    '.jpeg':
      FGraphExportType := etJPG;
    else
      FExtensionOK := false;
  end;
end;

procedure TCustomSaveGraphAction.UpdateReadyState();
begin
  Enabled := (FFilename <> '') and
             (FCount > 0);
end;

procedure TCustomSaveGraphAction.SaveToRaster(ImageClass: TRasterImageClass;
  Filename: UTF8String);
var
  Image: TRasterImage;
  aChart: TChart;
  i: Integer;
begin
  for i := 0 to high(FCharts) do
  begin
    Image := ImageClass.Create;
    Image.Width := FGraphSize.Width;
    Image.Height := FGraphSize.Height;
    FCharts[i].PaintOnCanvas(Image.Canvas, Rect(0, 0, Image.Width, Image.Height));
    Image.SaveToFile(GetFileName(Filename, i));
    Image.Free;
  end;
end;

procedure TCustomSaveGraphAction.SaveToSVG(Filename: UTF8String);
var
  i: Integer;
begin
  {$IFDEF DARWIN}
  // this is necessary to save to SVG, but causes font exceptions
  // known problem with Mac font NISC18030.ttf
  // No impact for users.
  InitFonts('/System/Library/Fonts');
  {$ENDIF}
  for i := 0 to high(FCharts) do begin
    FCharts[i].Width := FGraphSize.Width;
    FCharts[i].Height:= FGraphSize.Height;
    FCharts[i].SaveToSVGFile(GetFileName(Filename, i));
  end;
end;

function TCustomSaveGraphAction.GetFilename(AValue: UTF8String; AIndex: Integer): UTF8String;
var
  ext: UTF8String;
begin
  result := AValue;
  if (AIndex = 0) then exit;
  ext := ExtractFileExt(AValue);
  result := copy(AValue, 0, length(AValue) - length(ext)) + '-' + AIndex.ToString + ext;
end;

destructor TCustomSaveGraphAction.Destroy;
var
  i: integer;
begin
  // remove charts before freeing
  for i := 0 to high(FCharts) do
    RemoveComponent(FCharts[i]);
  inherited Destroy;
end;

constructor TCustomSaveGraphAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraphSize := TSize.Create(1024, 768);
  OnExecute := @SaveGraphExecute;
  Caption := 'Save as ...';
  FCount := 0;
end;

end.

