Unit htmlviewer;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, ComCtrls, FileUtil, Forms, LCLProc, Graphics, Dialogs,
  LazUTF8, LazFileUtils, strutils, outputviewer_types,
  cef3types, cef3lib, cef3intf, cef3own, cef3lcl, cef3api, LCLType, outputgenerator_base,
  executor;
//  FaviconGetter;

Type

  { TWebPanel }

  TWebPanel = class(TTabSheet, IAnaOutputViewer)
  private
    fChromium: TChromium;
    fUrl: String;

    procedure ChromiumGotFocus(Sender: TObject; const Browser: ICefBrowser);
    procedure ChromiumSetFocus(Sender: TObject; const Browser: ICefBrowser;
      Source: TCefFocusSource; out Result: Boolean);
    procedure ChromiumTakeFocus(Sender: TObject; const Browser: ICefBrowser; next_: Boolean);
    procedure PreKeyEvent(Sender: TObject; const Browser: ICefBrowser;
      const event: PCefKeyEvent; osEvent: TCefEventHandle; out
      isKeyboardShortcut: Boolean; out Result: Boolean);
    procedure LoadEnd(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; httpStatusCode: Integer);
  protected
    FMyActiveControl: TControl;
    procedure DoHide; override;
    procedure DoShow; override;
  public
    // IAnaOutputViewer
    function GetCaretPos: TPoint;
    function GetLineAtCaret: String;
    function GetSelectedText: String;
    function IsFocused: Boolean;

  public
    destructor Destroy; override;
    procedure InvalidateView;
    procedure Initialize;
    procedure RequestClose;
    procedure LoadFromStream(ST: TStream);
    procedure UpdateFontAndSize(AExecutor: TExecutor);


    property Url: String read fUrl write fUrl;
  end;

function IsHTMLSupported: boolean;

Implementation

Uses cef3ref, cef3scp, outputgenerator_html, regexpr, ana_globals;

var
  fHtmlSupported: boolean;

function IsHTMLSupported: boolean;
begin
  result := fHtmlSupported;
end;

{ TWebPanel }

procedure TWebPanel.ChromiumTakeFocus(Sender: TObject; const Browser: ICefBrowser; next_: Boolean);
Var
  NextPageIndex: Integer;
begin
//  Writeln('TakeFocus: ', next_);
end;

procedure TWebPanel.ChromiumSetFocus(Sender: TObject;
  const Browser: ICefBrowser; Source: TCefFocusSource; out Result: Boolean);
begin
//  Writeln('SetFocus: ', Source);
  Result := (Source = FOCUS_SOURCE_NAVIGATION);//
end;

procedure TWebPanel.ChromiumGotFocus(Sender: TObject; const Browser: ICefBrowser
  );
begin
  //Writeln('GotFocus');
end;

procedure TWebPanel.PreKeyEvent(Sender: TObject; const Browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle; out
  isKeyboardShortcut: Boolean; out Result: Boolean);
var
  Key: Word;
begin
  Key := event^.windows_key_code;
//  WriteLn('PreKeyEvent: ', Key);
  Application.NotifyKeyDownBeforeHandler(Self, Key, []);

  Result := (Key = VK_UNKNOWN);
end;

procedure TWebPanel.LoadEnd(Sender: TObject; const Browser: ICefBrowser;
  const Frame: ICefFrame; httpStatusCode: Integer);
begin
  Frame.ExecuteJavaScript('window.scrollTo(0,document.body.scrollHeight);', 'about:blank', 0);
end;

procedure TWebPanel.DoHide;
begin
  inherited DoHide;

  If Assigned(fChromium) then fChromium.Hide;
end;

procedure TWebPanel.DoShow;
begin
  inherited DoShow;

  If Assigned(fChromium) then fChromium.Show;
end;

function TWebPanel.GetCaretPos: TPoint;
begin
  result := Point(1,1);
end;

function TWebPanel.GetLineAtCaret: String;
begin
  result := '';
end;

function TWebPanel.GetSelectedText: String;
begin
  result := '';
end;

function TWebPanel.IsFocused: Boolean;
begin
  result := false;
end;

destructor TWebPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TWebPanel.InvalidateView;
begin
  //
end;

procedure TWebPanel.Initialize;
begin
  If not Assigned(fChromium) then
  begin
    fChromium := TChromium.Create(Self);
    fChromium.TabStop := True;
    fChromium.Parent := Self;
{    fChromium.Anchors := [];
    fChromium.AnchorParallel(akTop,   25, Self);
    fChromium.AnchorParallel(akLeft,   0, Self);
    fChromium.AnchorParallel(akRight,  0, Self);
    fChromium.AnchorParallel(akBottom, 0, Self); }
    fChromium.AnchorAsAlign(alClient, 0);

    fChromium.DefaultUrl := 'about:blank';

    // Register callbacks
    fChromium.OnTakeFocus := @ChromiumTakeFocus;
    fChromium.OnSetFocus := @ChromiumSetFocus;
    fChromium.OnGotFocus := @ChromiumGotFocus;
    fChromium.OnLoadEnd := @LoadEnd;

    fChromium.OnPreKeyEvent := @PreKeyEvent;
  end
  Else raise Exception.Create('Chromium already initialized.');
end;

procedure TWebPanel.RequestClose;
begin
  fChromium.Browser.Host.CloseBrowser(False);
end;

procedure TWebPanel.LoadFromStream(ST: TStream);
var
  Fn: String;
  FS: TFileStream;
begin
  if not (ST is TFileStream) then
  begin
    Fn := GetTempFileNameUTF8('','');
    ST.Position := 0;

    FS := TFileStream.Create(FN, fmCreate);
    FS.CopyFrom(ST, ST.Size);
    FS.Free;
  end else
    FN := TFileStream(ST).FileName;

  fChromium.Load('file://' + FN);
end;

procedure TWebPanel.UpdateFontAndSize(AExecutor: TExecutor);
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

var
  Path: ustring;
  Lang, FallbackLang: string;

initialization
  begin
    try
      Path := UTF8Decode(GetCurrentDirUTF8 + DirectorySeparator + 'CEF' + DirectorySeparator);
      CefResourcesDirPath := Path + UTF8Decode( 'Resources' + DirectorySeparator);
      CefLocalesDirPath   := Path + UTF8Decode('Resources' + DirectorySeparator + 'locales' + DirectorySeparator);
      LazGetLanguageIDs(Lang, FallbackLang);
      CefLocale := UTF8Decode(FallbackLang);
      if CefInitialize then
        fHtmlSupported := true
      else
        fHtmlSupported := false;
    except
      fHtmlSupported := false;
    end;
  end;

finalization
  try
    if (not fHtmlSupported) then exit;

    CefShutDown;
    CefCloseLibrary;
  except
    // Do nothing
  end;

end.

