unit cocoahtmlviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, outputviewer_types, outputgenerator_base,
  IPHtml,
  executor, menus;

type

  { TCocoaHtmlSheet }

  TCocoaHtmlSheet = class(TTabSheet, IAnaOutputViewer)
  private
    FHtmlView: TIpHtmlPanel;

    procedure CopyAllClipboardClick(Sender: TObject);
    procedure CopySelectClipBoardClick(Sender: TObject);

  public
    procedure InvalidateView;
    procedure Initialize;
    procedure LoadFromStream(ST: TStream);
    procedure UpdateFontAndSize(AExecutor: TExecutor);

    function GetLineAtCaret: String;
    function GetSelectedText: String;
    function GetCaretPos: TPoint;
    function IsFocused: Boolean;
    function GetContextMenu: TOutputViewerPopup;

end;

implementation

uses
  Controls, Graphics, outputgenerator_html, Clipbrd;

{ TCocoaHtmlSheet }

procedure TCocoaHtmlSheet.CopySelectClipBoardClick(Sender: TObject);
begin
//  Clipboard.AsText := FHtmlView.SelText;
  if FHtmlView.HaveSelection then
     FHtmlView.CopyToClipboard;
end;

procedure TCocoaHtmlSheet.CopyAllClipboardClick(Sender: TObject);
begin
  FHtmlView.SelectAll;
  FHtmlView.CopyToClipboard;
end;

procedure TCocoaHtmlSheet.InvalidateView;
begin
  //
end;

procedure TCocoaHtmlSheet.Initialize;
begin
  FHtmlView := TIpHtmlPanel.Create(Self);
  FHtmlView.Align := alClient;
  FHtmlView.Parent := Self;

  with FHtmlView do
  begin
    TextColor       := clwindowText;
    VLinkColor      := clFuchsia;
    BgColor         := clWindow;
    AllowTextSelect := true;
  end;

  FHtmlView.PopupMenu := TOutputViewerPopup.Create(Self);
  TOutputViewerPopup(FHtmlView.PopupMenu).OnCopySelectedClick := @CopySelectClipBoardClick;
  TOutputViewerPopup(FHtmlView.PopupMenu).OnCopyAllClick      := @CopyAllClipboardClick;

end;

procedure TCocoaHtmlSheet.LoadFromStream(ST: TStream);
var
  FHtml: TIpHtml;
begin
  FHtml := TIpHtml.Create;
  FHtmlView.SetHtml(FHtml);
  FHtml.LoadFromStream(ST);
end;

procedure TCocoaHtmlSheet.UpdateFontAndSize(AExecutor: TExecutor);
begin
  //
end;

function TCocoaHtmlSheet.GetLineAtCaret: String;
begin
  result := '';
end;

function TCocoaHtmlSheet.GetSelectedText: String;
begin
  result := '';
end;

function TCocoaHtmlSheet.GetCaretPos: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

function TCocoaHtmlSheet.IsFocused: Boolean;
begin
  result := false;
end;

function TCocoaHtmlSheet.GetContextMenu: TOutputViewerPopup;
begin
  result := TOutputViewerPopup(FHtmlView.PopupMenu);
end;

end.

