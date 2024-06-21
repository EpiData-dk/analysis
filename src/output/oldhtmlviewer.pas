unit oldhtmlviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, outputviewer_types, HtmlView, outputgenerator_base,
  executor, menus, script_runner;

type

  { TOldHtmlSheet }

  TOldHtmlSheet = class(TTabSheet, IAnaOutputViewer)
  private
    FHtmlView: THtmlViewer;
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
  Controls, Graphics, outputgenerator_html, Clipbrd, regexpr, ana_globals;

{ TOldHtmlSheet }

procedure TOldHtmlSheet.CopySelectClipBoardClick(Sender: TObject);
begin
  Clipboard.AsText := FHtmlView.SelText;
end;

procedure TOldHtmlSheet.CopyAllClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := FHtmlView.GetTextByIndices(0, MaxInt);
end;

procedure TOldHtmlSheet.InvalidateView;
begin
  //
end;

procedure TOldHtmlSheet.Initialize;
begin
  FHtmlView := THtmlViewer.Create(Self);
  FHtmlView.Align := alClient;
  FHtmlView.Parent := Self;

  with FHtmlView do
  begin
    BorderStyle := htFocused;
    DefBackground := clWindow;
    DefFontColor := clWindowText;
    DefOverLinkColor := clFuchsia;
    NoSelect := false;
  end;

  FHtmlView.PopupMenu := TOutputViewerPopup.Create(Self);
  TOutputViewerPopup(FHtmlView.PopupMenu).OnCopySelectedClick := @CopySelectClipBoardClick;
  TOutputViewerPopup(FHtmlView.PopupMenu).OnCopyAllClick      := @CopyAllClipboardClick;
end;

procedure TOldHtmlSheet.LoadFromStream(ST: TStream);
begin
  FHtmlView.LoadFromStream(ST);
  FHtmlView.Position := MaxInt;
end;

procedure TOldHtmlSheet.UpdateFontAndSize(AExecutor: TExecutor);
var
  regex: TRegExpr;
  regexPattern: string = '(\.body\s*\{font-family\:\s*")(.{1,30})(")';
begin
  // Add output font name to standard CSS
  regex := TRegExpr.Create;
  try
     regex.ModifierI := True;
     regex.Expression := regexPattern;
     if (regex.Exec(HTML_OUTPUT_CSS)) then
       HTML_OUTPUT_CSS := ReplaceRegExpr(regexPattern, HTML_OUTPUT_CSS,
                         '$1' + AExecutor.GetSetOptionValue(ANA_SO_OUTPUT_FONT_NAME) +
                         '$3', true)
     else
       HTML_OUTPUT_CSS := StringReplace(HTML_OUTPUT_CSS, '.body {',
                         '.body {font-family: "' +
                          AExecutor.GetSetOptionValue(ANA_SO_OUTPUT_FONT_NAME) +
                         '"; ', []);
  finally
     regex.Destroy;
  end;
end;

function TOldHtmlSheet.GetLineAtCaret: String;
begin
  result := '';
end;

function TOldHtmlSheet.GetSelectedText: String;
begin
  result := '';
end;

function TOldHtmlSheet.GetCaretPos: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

function TOldHtmlSheet.IsFocused: Boolean;
begin
  result := false;
end;

function TOldHtmlSheet.GetContextMenu: TOutputViewerPopup;
begin
  result := TOutputViewerPopup(FHtmlView.PopupMenu);
end;

end.

