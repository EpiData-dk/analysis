unit htmlviewer_osr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ExtCtrls, outputviewer_types, cef3osr, cef3types,
  cef3intf, executor,
  outputgenerator_base, Controls;

type

  { THTMLViewerOSR }

  THTMLViewerOSR = class(TTabSheet, IAnaOutputViewer)
  private
    // Internal vars
    FFileName: string;
    FChromiumOSR: TChromiumOSR;
    FPanel: TPanel;
    FPaintBox: TPaintBox;
  private
    // Helper methods
    function BuildMouseEvent(Btn: TMouseButton; ShiftState: TShiftState; X, Y: Integer; out CefBtn: TCefMouseButtonType): TCefMouseEvent; overload;
    function BuildMouseEvent(Btn: TMouseButton; ShiftState: TShiftState; P: TPoint; out CefBtn: TCefMouseButtonType): TCefMouseEvent; overload;
  private
    // Events
    // - panel
    procedure PanelEnter(Sender: TObject);
    procedure PanelExit(Sender: TObject);
    procedure PanelKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PanelKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // - paintbox
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxResize(Sender: TObject);
    // - chromium
    procedure ChromiumBeforeContextMenu(Sender: TObject;
      const Browser: ICefBrowser; const Frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure ChromiumGetRootScreenRect(Sender: TObject;
      const Browser: ICefBrowser; rect: PCefRect; out Result: Boolean);
    procedure ChromiumGetViewRect(Sender: TObject; const Browser: ICefBrowser;
      rect: PCefRect; out Result: Boolean);
    procedure ChromiumPaint(Sender: TObject; const Browser: ICefBrowser;
      kind: TCefPaintElementType; dirtyRectsCount: Cardinal;
      const dirtyRects: TCefRectArray; const buffer: Pointer; awidth,
      aheight: Integer);
    procedure ChromiumLoadEnd(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; httpStatusCode: Integer);
  public
    // IAnaOutputViewer
    function GetCaretPos: TPoint;
    function GetLineAtCaret: String;
    function GetSelectedText: String;
    function IsFocused: Boolean;
    procedure InvalidateView;
  public
    procedure Initialize;
    procedure LoadFromStream(ST: TStream);
    procedure UpdateFontAndSize(AExecutor: TExecutor);
  end;

implementation

uses
  outputgenerator_html, Forms, BGRABitmap, BGRABitmapTypes, LazFileUtils, Graphics;

function GetCefModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := [];
  if ssShift in Shift then Include(Result, EVENTFLAG_SHIFT_DOWN);
  if ssAlt in Shift then Include(Result, EVENTFLAG_ALT_DOWN);
  if ssCtrl in Shift then Include(Result, EVENTFLAG_CONTROL_DOWN);
  if ssLeft in Shift then Include(Result, EVENTFLAG_LEFT_MOUSE_BUTTON);
  if ssRight in Shift then Include(Result, EVENTFLAG_RIGHT_MOUSE_BUTTON);
  if ssMiddle in Shift then Include(Result, EVENTFLAG_MIDDLE_MOUSE_BUTTON);
end;

function GetCefButton(Button: TMouseButton): TCefMouseButtonType;
begin
  Case Button of
    TMouseButton.mbLeft: Result := MBT_LEFT;
    TMouseButton.mbRight: Result := MBT_RIGHT;
    TMouseButton.mbMiddle: Result := MBT_MIDDLE;
  end;
end;

{ THTMLViewerOSR }

procedure THTMLViewerOSR.ChromiumBeforeContextMenu(Sender: TObject;
  const Browser: ICefBrowser; const Frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.Clear;
end;

procedure THTMLViewerOSR.ChromiumGetRootScreenRect(Sender: TObject;
  const Browser: ICefBrowser; rect: PCefRect; out Result: Boolean);
begin
  rect^.x := 0;
  rect^.y := 0;
  rect^.width := FPaintBox.Width;
  rect^.height := FPaintBox.Height;
  Result := True;
end;

procedure THTMLViewerOSR.ChromiumGetViewRect(Sender: TObject;
  const Browser: ICefBrowser; rect: PCefRect; out Result: Boolean);
Var
  Point: TPoint;
begin
  Point.X := 0;
  Point.Y := 0;

  Point := FPaintBox.ClientToScreen(Point);

  rect^.x := Point.X;
  rect^.y := Point.Y;
  rect^.height := FPaintBox.Height;
  rect^.width := FPaintBox.Width;

  Result := True;
end;

procedure THTMLViewerOSR.ChromiumPaint(Sender: TObject;
  const Browser: ICefBrowser; kind: TCefPaintElementType;
  dirtyRectsCount: Cardinal; const dirtyRects: TCefRectArray;
  const buffer: Pointer; awidth, aheight: Integer);
Var
  Rect : TRect;
  BM: TBitmap;

begin
  {$IFDEF WINDOWS}
  Rect.Bottom := aheight;
  Rect.Top := 0;
  {$ELSE}
  Rect.Bottom := 0;
  Rect.Top := aheight;
  {$ENDIF}
  Rect.Left := 0;
  Rect.Right := awidth;

  FPaintBox.Canvas.Lock;
  BGRABitmapDraw(FPaintBox.Canvas, Rect, buffer, False, awidth, aheight, false);
  FPaintBox.Canvas.Unlock;
end;

procedure THTMLViewerOSR.ChromiumLoadEnd(Sender: TObject;
  const Browser: ICefBrowser; const Frame: ICefFrame; httpStatusCode: Integer);
begin
  Frame.ExecuteJavaScript('window.scrollTo(0,document.body.scrollHeight);', 'about:blank', 0);
end;

function THTMLViewerOSR.GetCaretPos: TPoint;
begin
  result := Point(1,1);
end;

function THTMLViewerOSR.GetLineAtCaret: String;
begin
  result := '';
end;

function THTMLViewerOSR.GetSelectedText: String;
begin
  result := '';
end;

function THTMLViewerOSR.IsFocused: Boolean;
begin
  result := false;
end;

procedure THTMLViewerOSR.InvalidateView;
begin
//  FChromiumOSR.Load('file://' + FFileName);
end;

procedure THTMLViewerOSR.PanelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  KeyEvent: TCefKeyEvent;
begin
  KeyEvent.kind := KEYEVENT_KEYDOWN;
  KeyEvent.modifiers := GetCefModifiers(Shift);
  KeyEvent.windows_key_code := Key;
  FChromiumOSR.Browser.Host.SendKeyEvent(KeyEvent);
end;

procedure THTMLViewerOSR.PanelKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  KeyEvent: TCefKeyEvent;
begin
  KeyEvent.kind := KEYEVENT_KEYUP;
  KeyEvent.modifiers := GetCefModifiers(Shift);
  KeyEvent.windows_key_code := Key;
  FChromiumOSR.Browser.Host.SendKeyEvent(KeyEvent);
end;

function THTMLViewerOSR.BuildMouseEvent(Btn: TMouseButton;
  ShiftState: TShiftState; X, Y: Integer; out CefBtn: TCefMouseButtonType
  ): TCefMouseEvent;
begin
  result := BuildMouseEvent(Btn, ShiftState, Point(X, Y), CefBtn);
end;

function THTMLViewerOSR.BuildMouseEvent(Btn: TMouseButton;
  ShiftState: TShiftState; P: TPoint; out CefBtn: TCefMouseButtonType
  ): TCefMouseEvent;
begin
  if (Btn = mbRight) then
    P := FPaintBox.ClientToScreen(P);

  Result.x := P.X;
  Result.y := P.Y;
  Result.modifiers := GetCefModifiers(ShiftState);
  CefBtn := GetCefButton(Btn);
end;

procedure THTMLViewerOSR.PanelEnter(Sender: TObject);
begin
  FChromiumOSR.Browser.Host.SendFocusEvent(true);
end;

procedure THTMLViewerOSR.PanelExit(Sender: TObject);
begin
  FChromiumOSR.Browser.Host.SendFocusEvent(false);
end;

procedure THTMLViewerOSR.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ME: TCefMouseEvent;
  CefBtn: TCefMouseButtonType;
begin
  FPanel.SetFocus;
  ME := BuildMouseEvent(Button, Shift, X, Y, CefBtn);
  FChromiumOSR.Browser.Host.SendMouseClickEvent(ME, CefBtn, false, 1);
end;

procedure THTMLViewerOSR.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ME: TCefMouseEvent;
  CefBtn: TCefMouseButtonType;
begin
  ME := BuildMouseEvent(mbLeft, Shift, X, Y, CefBtn);
  FChromiumOSR.Browser.Host.SendMouseMoveEvent(ME, FPaintBox.MouseInClient);
  FChromiumOSR.Browser.Host.SendFocusEvent(false);
end;

procedure THTMLViewerOSR.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ME: TCefMouseEvent;
  CefBtn: TCefMouseButtonType;
begin
  FPanel.SetFocus;
  ME := BuildMouseEvent(Button, Shift, X, Y, CefBtn);
  FChromiumOSR.Browser.Host.SendMouseClickEvent(ME, CefBtn, true, 1);
end;

procedure THTMLViewerOSR.PaintBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  ME: TCefMouseEvent;
  CefBtn: TCefMouseButtonType;
begin
  ME := BuildMouseEvent(mbLeft, Shift, MousePos, CefBtn);
  FChromiumOSR.Browser.Host.SendMouseWheelEvent(ME, 0, WheelDelta);
end;

procedure THTMLViewerOSR.PaintBoxResize(Sender: TObject);
var
  B: ICefBrowser;
  H: ICefBrowserHost;
begin
  B := FChromiumOSR.Browser;
  if (Not (assigned(B))) then Exit;
  B.Host.WasResized;
  Application.ProcessMessages;
end;

type
  TAccessChromiumOSR = class(TChromiumOSR);

procedure THTMLViewerOSR.Initialize;
begin
  DisableAutoSizing;
  FPanel := TPanel.Create(self);
  FPanel.Align := alClient;
  FPanel.Parent := self;
  FPanel.OnEnter := @PanelEnter;
  FPanel.OnExit := @PanelExit;
  FPanel.OnKeyDown := @PanelKeyDown;
  FPanel.OnKeyUp := @PanelKeyUp;

  FPaintBox := TPaintBox.Create(FPanel);
  FPaintBox.Align := alClient;
  FPaintBox.Parent := FPanel;
  FPaintBox.OnMouseDown := @PaintBoxMouseDown;
  FPaintBox.OnMouseMove := @PaintBoxMouseMove;
  FPaintBox.OnMouseUp := @PaintBoxMouseUp;
  FPaintBox.OnMouseWheel := @PaintBoxMouseWheel;
  FPaintBox.OnResize := @PaintBoxResize;

  FChromiumOSR := TChromiumOSR.Create(Self);
  FChromiumOSR.BackgroundColor := clWhite;
  FChromiumOSR.DefaultUrl      := 'about:blank';
  FChromiumOSR.WindowlessFrameRate := 30;
  FChromiumOSR.OnBeforeContextMenu := @ChromiumBeforeContextMenu;
  FChromiumOSR.OnGetRootScreenRect := @ChromiumGetRootScreenRect;
  FChromiumOSR.OnGetViewRect       := @ChromiumGetViewRect;
  FChromiumOSR.OnPaint             := @ChromiumPaint;
  FChromiumOSR.OnLoadEnd           := @ChromiumLoadEnd;
  TAccessChromiumOSR(FChromiumOSR).Loaded;

  EnableAutoSizing;
end;

procedure THTMLViewerOSR.LoadFromStream(ST: TStream);
var
  Fn: String;
  FS: TFileStream;
begin
  if not (ST is TFileStream) then
  begin
    FFileName := GetTempFileNameUTF8('','');
    ST.Position := 0;

    FS := TFileStream.Create(FFileName, fmCreate);
    FS.CopyFrom(ST, ST.Size);
    FS.Free;
    FN := FFileName;
  end else
    FN := TFileStream(ST).FileName;

  FChromiumOSR.Load('file://' + FN);
end;

procedure THTMLViewerOSR.UpdateFontAndSize(AExecutor: TExecutor);
begin

end;

end.

