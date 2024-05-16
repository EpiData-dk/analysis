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
  // invalid characters in filenames for each OS
  InvalidChars: TSysCharSet = [
  {$IFDEF WINDOWS}
  '<','>',':','"','/','\','|','?','*'
  {$ENDIF}
  {$IFDEF DARWIN}
  ':','/'
  {$ENDIF}
  {$IFDEF LINUX}
  '/'
  {$ENDIF}
  ];
type

  EIncorrectGraphExtension = class(Exception);

  { TCustomSaveGraphAction }

  TCustomSaveGraphAction = class(TCustomAction)
  private
    FCharts: array of TChart;
    FCount: Integer;
    FStratumValues: array of UTF8String;
    FFileName: UTF8String;
    FExtensionOK: Boolean;
    FGraphExportType: TGraphExportType;
    FGraphSize: TSize;
    procedure SaveToRaster(ImageClass: TRasterImageClass; Filename: UTF8String);
    procedure SaveToVector(Filename: UTF8String);
    procedure SetFilename(AValue: UTF8String);
    procedure UpdateExportType();
  protected
    procedure UpdateReadyState(); virtual;
    procedure SaveGraphExecute(Sender: TObject); virtual;
    property Filename: UTF8String read FFilename write setFilename;
    property ChartCount: Integer read FCount;
    property GraphExportType: TGraphExportType read FGraphExportType write FGraphExportType;
    property GraphSize: TSize read FGraphSize write FGraphSize;
    property ExtensionOK: Boolean read FExtensionOK write FExtensionOK;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddChart(AChart: TChart; AText: UTF8String);
    function SaveGraphs(): Boolean;
    destructor Destroy; override;
  end;

  {TSaveGraphAction}

  TSaveGraphAction = class(TCustomSaveGraphAction)
  public
    property ChartCount;
    property FileName;
    property GraphExportType;
    property GraphSize;
  end;

  {helper function}
  function GetSaveChartFilename(AFileName: UTF8String; AValue: UTF8String): UTF8String;

implementation

uses
  TADrawerSVG, LazFileUtils {$IFDEF DARWIN}, TAFonts{$ENDIF};

{ TSaveGraphAction }

// when invoked here by graphCommandExecutor
// must have set Filename for the set of charts before calling
function TCustomSaveGraphAction.SaveGraphs(): Boolean;
begin
  result := FExtensionOK;
  if (not FExtensionOK) then
    exit;
  case GraphExportType of
    etSVG: SaveToVector(FFileName);
    etPNG: SaveToRaster(TPortableNetworkGraphic, FFileName);
    etJPG: SaveToRaster(TJPEGImage, FFileName);
    else
      result := false;
  end;
end;

// invoked here by graph save dialog
procedure TCustomSaveGraphAction.SaveGraphExecute(Sender: TObject);
begin
  SaveGraphs();
end;

procedure TCustomSaveGraphAction.AddChart(AChart: TChart; AText: UTF8String);
begin
  inc(FCount);
  setLength(FCharts, FCount);
  FCharts[FCount - 1] := AChart;
  setLength(FStratumValues, FCount);
  FStratumValues[FCount - 1] := AText;

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
  i: integer;
begin
  for i := 0 to FCount - 1 do
  begin
    achart := FCharts[i];
    Image := ImageClass.Create;
    Image.Width := FGraphSize.Width;
    Image.Height := FGraphSize.Height;
    aChart.PaintOnCanvas(Image.Canvas, Rect(0, 0, Image.Width, Image.Height));
    Image.SaveToFile(GetSaveChartFilename(FFilename, FStratumValues[i]));
    Image.Free;
  end;
end;

procedure TCustomSaveGraphAction.SaveToVector(Filename: UTF8String);
var
  aChart: TChart;
  i: integer;
begin
  {$IFDEF DARWIN}
  // this is necessary to save to SVG, but causes font exceptions
  // known problem with Mac font NISC18030.ttf
  // No impact for users.
  InitFonts('/System/Library/Fonts');
  {$ENDIF}
  for i := 0 to FCount - 1 do
    begin
      aChart := FCharts[i];
      aChart.Width := FGraphSize.Width;
      aChart.Height:= FGraphSize.Height;
      aChart.SaveToSVGFile(GetSaveChartFilename(FFilename, FStratumValues[i]));
    end;
end;

constructor TCustomSaveGraphAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraphSize := TSize.Create(1024, 768);
  OnExecute := @SaveGraphExecute;
  Caption := 'Save as ...';
  FCount := 0;
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

// create a save file name from base name and optional text, which should be stratum value (not label)
function GetSaveChartFilename(AFileName: UTF8String; AValue: UTF8String): UTF8String;
var
  ext,
  qual: UTF8String;
  aChar: Char;
  i: Integer;
begin
  result := AFileName;
  if (AValue = '') then
    exit;
  ext := ExtractFileExt(AFileName);
  // remove illegal characters in AValue based on OS (see InvalidChars def above)
  qual := '';
  for aChar in AValue do
    if CharInSet(aChar, InvalidChars) then
      qual += '-'
    else
      qual += aChar;
  // insert clean AValue before extension
  qual := '-' + qual;
  insert(qual, AFileName, pos(ext, AFileName));
  result := AFileName;
end;


end.

