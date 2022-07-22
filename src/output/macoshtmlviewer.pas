unit macoshtmlviewer;

{$mode objfpc}{$H+}
{$modeswitch objectiveC1}

{
  LCL web browser for Cocoa
  uses MacOS api calls via TCocoaWSWebBrowser
  This stripped down version for EpiData Analysis does not load web pages

  June 2021
}
interface

uses
  Interfaces, // this includes the LCL widgetset
  Classes, SysUtils, StdCtrls, ComCtrls,
  LCLIntf, LCLType,
  Controls, WSControls, WSLCLClasses, CocoaAll, CocoaPrivate, WebKit,
  Graphics, LMessages;

type

  { TMacOSWebBrowser }

  TMacOSWebBrowser = class(TWinControl)
  private
  protected
    class procedure WSRegisterClass; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure LoadFromStream(ST: TStream);
    procedure ScrollToEnd(Sender: TObject);
    function PageLoading(Sender: TObject) : boolean;
  end;

  TWebBrowser = class(TMacOSWebBrowser)
  published
    property Align;
    property Anchors;
    property Constraints;
  end;

   TCocoaWSWebBrowser = class(TWSWinControl)
   published
     class function CreateHandle(const AWinControl : TWinControl;
                                 const AParams     : TCreateParams): TLCLIntfHandle; override;
     class procedure DestroyHandle(const AWinControl : TWinControl); override;
     class function LoadHtml(const AWinControl : TWinControl;
                             const Html        : string) : Boolean;
     class function ScrollToEnd(const AWinControl : TWinControl) : Boolean;
     class function PageLoading(const AWinControl : TWinControl) : Boolean;
   end;


implementation

{ TCustomWebBrowser }

constructor TMacOSWebBrowser.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle     := ControlStyle + [csClickEvents, csFramed];
end;

procedure TMacOSWebBrowser.LoadFromStream(ST: TStream);
var
  FHtml: String;
  FHtmlStream: TStringStream;
begin
    FHtmlStream := TStringStream.Create;
    FHtmlStream.CopyFrom(ST, 0); // copy entire stream to string
    FHtml := FHtmlStream.DataString;
    TCocoaWSWebBrowser.LoadHtml(self, FHtml) // string required
end;

class procedure TMacOSWebBrowser.WSRegisterClass;
begin
  inherited;
  RegisterWSComponent(TMacOSWebBrowser, TCocoaWSWebBrowser);
end;

procedure TMacOSWebBrowser.ScrollToEnd(Sender: TObject);
begin
   TCocoaWSWebBrowser.ScrollToEnd(self);
end;

function TMacOSWebBrowser.PageLoading(Sender: TObject) : boolean;
begin
   result := TCocoaWSWebBrowser.PageLoading(self);
end;

class function TCocoaWSWebBrowser.CreateHandle(const AWinControl : TWinControl;
                                               const AParams     : TCreateParams): TLCLIntfHandle;
var
  AWebView: WebView;
begin
  AWebView := WebView(NSView(WebView.alloc).lclInitWithCreateParams(AParams));
  Result   := TLCLIntfHandle(AWebView);
end;


class procedure TCocoaWSWebBrowser.DestroyHandle(const AWinControl : TWinControl);
begin
  if (AWinControl.HandleAllocated) then begin
    WebView (AWinControl.Handle).frameLoadDelegate.release;
    NSObject(AWinControl.Handle).release
  end
end;

class function TCocoaWSWebBrowser.LoadHtml(const aWinControl : TWinControl;
                                           const Html        : String): Boolean;
begin
   if not aWinControl.HandleAllocated then
      aWinControl.HandleNeeded;
   if aWinControl.HandleAllocated then begin
      WebView(aWinControl.Handle).mainFrame.loadHTMLString_baseURL(
          NSSTR(PAnsiChar(Html))
          ,nil);
   end;
   Result := True;
end;

class function TCocoaWSWebBrowser.ScrollToEnd(const aWinControl : TWinControl): Boolean;
begin
   if not aWinControl.HandleAllocated then
      aWinControl.HandleNeeded;
   if aWinControl.HandleAllocated then begin
      WebView(aWinControl.Handle).scrollToEndOfDocument(nil);
   end;
   Result := True;
end;

class function TCocoaWSWebBrowser.PageLoading(const aWinControl : TWinControl): Boolean;
begin
   Result := False;
   if not aWinControl.HandleAllocated then
      aWinControl.HandleNeeded;
   if aWinControl.HandleAllocated then begin
      Result := WebView(aWinControl.Handle).isLoading;
   end;
end;

end.

