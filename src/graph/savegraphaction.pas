unit savegraphaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, TAGraph, Graphics, Types;

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
    FChart: TChart;
    FFilename: UTF8String;
    FExtensionOK: Boolean;
    FGraphExportType: TGraphExportType;
    FGraphSize: TSize;
    procedure SaveToRaster(ImageClass: TRasterImageClass);
    procedure SaveToVector();
    procedure SetChart(AValue: TChart);
    procedure SetFilename(AValue: UTF8String);
    procedure UpdateExportType();
  protected
    procedure UpdateReadyState(); virtual;
    procedure SaveGraphExecute(Sender: TObject); virtual;
    property Chart: TChart read FChart write SetChart;
    property Filename: UTF8String read FFilename write SetFilename;
    property GraphExportType: TGraphExportType read FGraphExportType write FGraphExportType;
    property GraphSize: TSize read FGraphSize write FGraphSize;
    property ExtensionOK: Boolean read FExtensionOK write FExtensionOK;
  public
    constructor Create(AOwner: TComponent); override;
    function SaveGraphs(): Boolean;
    destructor Destroy; override;
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
  TADrawerSVG, LazFileUtils {$IFDEF DARWIN}, TAFonts{$ENDIF};

{ TSaveGraphAction }

// when invoked here by graphCommandExecutor
// must have set Filename for every chart before calling
function TCustomSaveGraphAction.SaveGraphs(): Boolean;
begin
  result := FExtensionOK;
  if (not FExtensionOK) then
    exit;
  case GraphExportType of
    etSVG: SaveToVector();
    etPNG: SaveToRaster(TPortableNetworkGraphic);
    etJPG: SaveToRaster(TJPEGImage);
    else
      result := false;
  end;
end;

// invoked here by graph save dialog
procedure TCustomSaveGraphAction.SaveGraphExecute(Sender: TObject);
begin
  SaveGraphs();
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
             (Assigned(FChart));
end;

procedure TCustomSaveGraphAction.SaveToRaster(ImageClass: TRasterImageClass);
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

procedure TCustomSaveGraphAction.SaveToVector();
begin
  {$IFDEF DARWIN}
  // this is necessary to save to SVG, but causes font exceptions
  // known problem with Mac font NISC18030.ttf
  // No impact for users.
  InitFonts('/System/Library/Fonts');
  {$ENDIF}
  FChart.Width  := FGraphSize.Width;
  FChart.Height := FGraphSize.Height;
  FChart.SaveToSVGFile(FileName);
end;

constructor TCustomSaveGraphAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraphSize := TSize.Create(1024, 768);
  OnExecute := @SaveGraphExecute;
  Caption := 'Save as ...';
end;

destructor TCustomSaveGraphAction.Destroy;
begin
  // remove chart before freeing
  RemoveComponent(FChart);
  inherited Destroy;
end;

end.

