unit savegraphaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, Dialogs, TAGraph, Graphics;

type

  { TSaveGraphAction }

  TSaveGraphAction = class(TCustomAction)
  private
    FChart: TChart;
    FSaveDialog: TSaveDialog;
    procedure FilterChange(Sender: TObject);
    procedure SaveToRaster(ImageClass: TRasterImageClass; Filename: UTF8String);
    procedure SaveGraphExecute(Sender: TObject);
    procedure SetChart(AValue: TChart);
  public
    constructor Create(AOwner: TComponent); override;
    property Chart: TChart read FChart write SetChart;
  end;

implementation

uses
  TADrawerSVG, LazFileUtils;

{ TSaveGraphAction }

procedure TSaveGraphAction.SaveGraphExecute(Sender: TObject);
begin
  if (not FSaveDialog.Execute) then
    Exit;

  case FSaveDialog.FilterIndex of
    1: FChart.SaveToSVGFile(FSaveDialog.FileName);
    2: SaveToRaster(TPortableNetworkGraphic, FSaveDialog.FileName);
    3: SaveToRaster(TJPEGImage, FSaveDialog.FileName);
  end;
end;

procedure TSaveGraphAction.SetChart(AValue: TChart);
begin
  if FChart = AValue then Exit;
  FChart := AValue;

  Enabled := Assigned(FChart);
end;

procedure TSaveGraphAction.FilterChange(Sender: TObject);
begin
  case FSaveDialog.FilterIndex of
    1: FSaveDialog.DefaultExt := '.svg';
    2: FSaveDialog.DefaultExt := '.png';
    3: FSaveDialog.DefaultExt := '.jpg';
  end;
end;

procedure TSaveGraphAction.SaveToRaster(ImageClass: TRasterImageClass;
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

constructor TSaveGraphAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSaveDialog := TSaveDialog.Create(Self);
  FSaveDialog.Filter := 'SVG File|*.svg|PNG File|*.png|JPEG File|*.jpg;*.jpeg';
  FSaveDialog.FilterIndex := 0;
  FSaveDialog.OnTypeChange := @FilterChange;
  FSaveDialog.InitialDir := GetCurrentDirUTF8;
  FSaveDialog.Options := [ofOverwritePrompt, ofPathMustExist, ofEnableSizing];

  OnExecute := @SaveGraphExecute;
  Caption := 'Save as ...';
end;

end.

