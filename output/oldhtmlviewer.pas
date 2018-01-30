unit oldhtmlviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, outputviewer_types, HtmlView, outputgenerator_base,
  executor;

type

  { TOldHtmlSheet }

  TOldHtmlSheet = class(TTabSheet, IAnaOutputViewer)
  private
    FHtmlView: THtmlViewer;
  public
    procedure InvalidateView;
    procedure Initialize;
    procedure LoadFromStream(ST: TStream);
    procedure UpdateFontAndSize(AExecutor: TExecutor);
    function GetOutputGeneratorClass: TOutputGeneratorClass;
    function GetLineAtCaret: String;
    function GetSelectedText: String;
    function GetCaretPos: TPoint;
    function IsFocused: Boolean;
  end;

implementation

uses
  Controls, Graphics, outputgenerator_html;

{ TOldHtmlSheet }

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
end;

procedure TOldHtmlSheet.LoadFromStream(ST: TStream);
begin
  FHtmlView.LoadFromStream(ST);
  FHtmlView.Position := MaxInt;
end;

procedure TOldHtmlSheet.UpdateFontAndSize(AExecutor: TExecutor);
begin
  //
end;

function TOldHtmlSheet.GetOutputGeneratorClass: TOutputGeneratorClass;
begin
  result := TOutputGeneratorHTML;
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

end.

