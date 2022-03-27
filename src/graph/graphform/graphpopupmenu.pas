unit graphpopupmenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Menus, TAGraph;

type

  { TGraphPopupMenu }

  TGraphPopupMenu = class(TPopupMenu)
  private
    FChart: TChart;
    FCloseAllItem: TMenuItem;
    FCloseItem: TMenuItem;
    FDivider: TMenuItem;
    FForm: TCustomForm;
    FSaveAsSVG: TMenuItem;
    procedure CloseAllWindow(Sender: TObject);
    procedure CloseWindow(Sender: TObject);
    procedure SaveSVG(Sender: TObject);
    procedure SetChart(AValue: TChart);
    procedure UpdateItems;
  public
    constructor Create(AOwner: TCustomForm);
    property Chart: TChart read FChart write SetChart;
  end;

implementation

uses
  graphformfactory, TADrawerSVG, Dialogs, LazFileUtils;

{ TGraphPopupMenu }

procedure TGraphPopupMenu.CloseAllWindow(Sender: TObject);
begin
  TheGraphFormFactory.CloseAllOpenForms();
end;

procedure TGraphPopupMenu.CloseWindow(Sender: TObject);
begin
  FForm.Close;
end;

procedure TGraphPopupMenu.SaveSVG(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  SaveDialog.DefaultExt := '.svg';
  SaveDialog.Options := [ofOverwritePrompt, ofPathMustExist, ofEnableSizing];
  SaveDialog.InitialDir := GetCurrentDirUTF8;

  if (SaveDialog.Execute) then
    FChart.SaveToSVGFile(SaveDialog.FileName);
end;

procedure TGraphPopupMenu.SetChart(AValue: TChart);
begin
  if FChart = AValue then Exit;
  FChart := AValue;

  UpdateItems;
end;

procedure TGraphPopupMenu.UpdateItems;
begin
  FSaveAsSVG.Enabled := Assigned(FChart);
  FSaveAsSVG.Visible := Assigned(FChart);

  FDivider.Enabled := Assigned(FChart);
  FDivider.Visible := Assigned(FChart);
end;

constructor TGraphPopupMenu.Create(AOwner: TCustomForm);
begin
  inherited Create(AOwner);
  FForm := AOwner;

  FSaveAsSVG := TMenuItem.Create(Self);
  FSaveAsSVG.Caption := 'Save as SVG';
  FSaveAsSVG.OnClick := @SaveSVG;
  Items.Add(FSaveAsSVG);

  FDivider := TMenuItem.Create(Self);
  FDivider.Caption := '-';
  Items.Add(FDivider);

  FCloseItem := TMenuItem.Create(self);
  FCloseItem.Caption := 'Close';
  FCloseItem.OnClick := @CloseWindow;
  Items.Add(FCloseItem);

  FCloseAllItem := TMenuItem.Create(self);
  FCloseAllItem.Caption := 'Close All Graphs';
  FCloseAllItem.OnClick := @CloseAllWindow;
  Items.Add(FCloseAllItem);

  UpdateItems;
end;

end.

