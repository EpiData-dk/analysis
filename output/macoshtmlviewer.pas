unit macoshtmlviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, outputviewer_types, outputgenerator_base,
  LCLIntf, WebBrowser, Forms, // outputcreator,
  executor, menus;

type

  { TMacOSWebBrowser }

  TMacOSWebBrowser = class(TTabSheet, IAnaOutputViewer)
  private
    FHtmlView: TWebBrowser;
//    FEventLog: TEventLog;
//    FBottomLine: TOutputLine;
    FScrollVertical: TScrollBar;
    procedure CopyAllClipboardClick(Sender: TObject);
    procedure CopySelectClipBoardClick(Sender: TObject);
    procedure ScrollToBottom(Sender: TObject);

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

{ TMacOSWebBrowser }

procedure TMacOSWebBrowser.CopySelectClipBoardClick(Sender: TObject);
var
  clipText: String;
begin
//  Clipboard.AsText := FHtmlView.SelText;
   ClipText := FHtmlView.GetAccessibleObject.ToString;  //.HaveSelection then
//   ClipText..CopyToClipboard;
end;

procedure TMacOSWebBrowser.CopyAllClipboardClick(Sender: TObject);
begin
//  FHtmlView.SelectAll;
//  FHtmlView.CopyToClipboard;
end;

procedure TMacOSWebBrowser.ScrollToBottom(Sender: TObject);
begin
/  FScrollVertical.Position := FScrollVertical.Max;         //does nothing
  FHtmlView.RePaint;
end;

procedure TMacOSWebBrowser.InvalidateView;
begin
  //
end;

procedure TMacOSWebBrowser.Initialize;
begin
  FHtmlView := TWebBrowser.Create(Self);
  FHtmlView.Parent := Self;
  FHtmlView.Align := alClient;
  FScrollVertical := TScrollBar.Create(Self);
  FScrollVertical.Kind := sbVertical;
{  FEventLog := TEventLog.Create(Self);
  FEventLog.LogType := ltFile;
  FEventLog.FileName:= '/Users/Jamie/downloads/ed.log';
  FEventLog.Active := true;
}
{  with FHtmlView do
  begin
    TextColor       := clwindowText;
    VLinkColor      := clFuchsia;
    BgColor         := clWindow;
    AllowTextSelect := true;
  end;
}
  FHtmlView.PopupMenu := TOutputViewerPopup.Create(Self);
  TOutputViewerPopup(FHtmlView.PopupMenu).OnCopySelectedClick := @CopySelectClipBoardClick;
  TOutputViewerPopup(FHtmlView.PopupMenu).OnCopyAllClick      := @CopyAllClipboardClick;
  FHtmlView.OnPageLoaded := @ScrollToBottom;
end;

procedure TMacOSWebBrowser.LoadFromStream(ST: TStream);
var
  FHtml: String;
  FHtmlStream: TStringStream;
begin
  FHtmlStream := TStringStream.Create;
  FHtmlStream.CopyFrom(ST, 0); // copy entire stream
  FHtml := FHtmlStream.DataString;
  FHtmlView.LoadPageFromHtml(FHtml);

end;

procedure TMacOSWebBrowser.UpdateFontAndSize(AExecutor: TExecutor);
begin
  //
end;

function TMacOSWebBrowser.GetLineAtCaret: String;
begin
  result := '';
end;

function TMacOSWebBrowser.GetSelectedText: String;
begin
  result := '';
end;

function TMacOSWebBrowser.GetCaretPos: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

function TMacOSWebBrowser.IsFocused: Boolean;
begin
  result := false;
end;

function TMacOSWebBrowser.GetContextMenu: TOutputViewerPopup;
begin
  result := TOutputViewerPopup(FHtmlView.PopupMenu);
end;

end.

