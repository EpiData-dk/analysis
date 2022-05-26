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
    FCloseAllItem: TMenuItem;
    FCloseItem: TMenuItem;
    FDivider: TMenuItem;
    FForm: TCustomForm;
    FSaveAs: TMenuItem;
    procedure CloseAllWindow(Sender: TObject);
    procedure CloseWindow(Sender: TObject);
    function GetSaveAsAction: TBasicAction;
    procedure SetSaveAsAction(AValue: TBasicAction);
  public
    constructor Create(AOwner: TCustomForm);
    property SaveAsAction: TBasicAction read GetSaveAsAction write SetSaveAsAction;
  end;

implementation

uses
  graphformfactory, TADrawerSVG, Graphics, LCLType, Controls;

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

procedure TGraphPopupMenu.SetSaveAsAction(AValue: TBasicAction);
begin
  FSaveAs.Action := AValue;
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
  FCloseItem.ShortCut := ShortCut(VK_W, [ssModifier]);
  FCloseItem.OnClick := @CloseWindow;
  Items.Add(FCloseItem);

  FCloseAllItem := TMenuItem.Create(self);
  FCloseAllItem.Caption := 'Close All Graphs';
  FCloseAllItem.ShortCut := ShortCut(VK_Q, [ssModifier]);
  FCloseAllItem.OnClick := @CloseAllWindow;
  Items.Add(FCloseAllItem);
end;

end.

