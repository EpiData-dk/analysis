unit cocoawebviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, outputviewer_types, outputgenerator_base,
  LCLIntf, CocoaAll, WebKit, Forms, macoshtmlviewer, DateUtils,
  executor, menus;

type

  { TCocoaWebSheet }

  TCocoaWebSheet = class(TTabSheet, IAnaOutputViewer)
  private
    FHtmlView: TWebBrowser;
    procedure CopyAllClipboardClick(Sender: TObject);
    procedure CopySelectClipBoardClick(Sender: TObject);
//    procedure ScrollToBottom(Sender: TObject);

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
  Controls, Graphics, outputgenerator_html, Clipbrd, LCLType, LMessages;

{ TCocoaWebSheet }

procedure TCocoaWebSheet.CopySelectClipBoardClick(Sender: TObject);
var
  clipText: String;
begin
end;

procedure TCocoaWebSheet.CopyAllClipboardClick(Sender: TObject);
begin
end;
{
procedure TCocoaWebSheet.ScrollToBottom(Sender: TObject);
begin
  FHtmlView.ScrollToEnd(Self);
end;
}
procedure TCocoaWebSheet.InvalidateView;
begin
  //
end;

procedure TCocoaWebSheet.Initialize;
begin
  FHtmlView := TWebBrowser.Create(Self);
  FHtmlView.Parent := Self;
  FHtmlView.Align := alClient;

  FHtmlView.PopupMenu := TOutputViewerPopup.Create(Self);
  TOutputViewerPopup(FHtmlView.PopupMenu).OnCopySelectedClick := @CopySelectClipBoardClick;
  TOutputViewerPopup(FHtmlView.PopupMenu).OnCopyAllClick      := @CopyAllClipboardClick;
//  FHtmlView.OnPageLoaded := @ScrollToBottom;
end;

procedure TCocoaWebSheet.LoadFromStream(ST: TStream);
begin
  FHtmlView.LoadFromStream(ST);
// wait for page to load before scroll to end
  while (FHtmlView.PageLoading(self))
    and (not Application.Terminated) do
      Application.ProcessMessages;
  FHtmlView.ScrollToEnd(self);
end;

procedure TCocoaWebSheet.UpdateFontAndSize(AExecutor: TExecutor);
begin
  //
end;

function TCocoaWebSheet.GetLineAtCaret: String;
begin
  result := '';
end;

function TCocoaWebSheet.GetSelectedText: String;
begin
  result := '';
end;

function TCocoaWebSheet.GetCaretPos: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

function TCocoaWebSheet.IsFocused: Boolean;
begin
  result := false;
end;

function TCocoaWebSheet.GetContextMenu: TOutputViewerPopup;
begin
  result := TOutputViewerPopup(FHtmlView.PopupMenu);
end;

end.

