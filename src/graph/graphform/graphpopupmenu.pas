unit graphpopupmenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Menus, TAGraph, Dialogs, savegraphaction,
  ActnList;

type

  { TGraphPopupMenu }

  TGraphPopupMenu = class(TPopupMenu)
  private
    FChart: TChart;
    FCloseAllItem: TMenuItem;
    FCloseItem: TMenuItem;
    FDivider: TMenuItem;
    FForm: TCustomForm;
    FSaveAs: TMenuItem;
    procedure CloseAllWindow(Sender: TObject);
    procedure CloseWindow(Sender: TObject);
    function GetSaveAsAction: TBasicAction;
    procedure SetChart(AValue: TChart);
    procedure SetSaveAsAction(AValue: TBasicAction);
    procedure UpdateItems;
  public
    constructor Create(AOwner: TCustomForm);
    property Chart: TChart read FChart write SetChart;
    property SaveAsAction: TBasicAction read GetSaveAsAction write SetSaveAsAction;
  end;

implementation

uses
  graphformfactory, TADrawerSVG, Graphics;

{ TGraphPopupMenu }

procedure TGraphPopupMenu.CloseAllWindow(Sender: TObject);
begin
  TheGraphFormFactory.CloseAllOpenForms();
end;

procedure TGraphPopupMenu.CloseWindow(Sender: TObject);
begin
  FForm.Close;
end;

function TGraphPopupMenu.GetSaveAsAction: TBasicAction;
begin
  Result := FSaveAs.Action;
end;

procedure TGraphPopupMenu.SetChart(AValue: TChart);
begin
  if FChart = AValue then Exit;
  FChart := AValue;

  UpdateItems;
end;

procedure TGraphPopupMenu.SetSaveAsAction(AValue: TBasicAction);
begin
  FSaveAs.Action := AValue;
end;

procedure TGraphPopupMenu.UpdateItems;
begin
  FSaveAs.Enabled := Assigned(FChart);
  FSaveAs.Visible := Assigned(FChart);

  FDivider.Enabled := Assigned(FChart);
  FDivider.Visible := Assigned(FChart);
end;

constructor TGraphPopupMenu.Create(AOwner: TCustomForm);
begin
  inherited Create(AOwner);
  FForm := AOwner;

  FSaveAs := TMenuItem.Create(Self);
  Items.Add(FSaveAs);

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

