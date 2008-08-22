(*****************************************
 Sorry, no comments.
 Any question, feel free to e-mail me :
 Cezar Lenci
 sintesis@dglnet.com.br
******************************************)
unit PageSetup;

interface

Uses
    Windows,Forms,CommDlg,Printers,SysUtils,Messages,Dialogs,Classes;

Type

TPageSetupFlags = (poDefaultMinMargins,poDisableMargins,poMargins,poMinMargins,
                   poDisableOrientation,poDisablePaper,poDisablePrinter,
                   poHundredthsOfMillimeters,poThousandthsOfInches);
TPageOptions    = Set Of TPageSetupFlags;

TPageSetupDialog = class(TCommonDialog)
private
    FOptions : TPageOptions;
    FFlags   : Longint;
    FMarginLeft,
    FMarginTop,
    FMarginRight,
    FMarginBottom,
    FMinMarginLeft,
    FMinMarginTop,
    FMinMarginRight,
    FMinMarginBottom : Integer;
    FPaperLength,
    FPaperWidth      : Short;
    procedure SetOptions(Value : TPageOptions);
    procedure SetLeft(Value : Integer);
    procedure SetTop(Value : Integer);
    procedure SetRight(Value : Integer);
    procedure SetBottom(Value : Integer);
    procedure SetMinLeft(Value : Integer);
    procedure SetMinTop(Value : Integer);
    procedure SetMinRight(Value : Integer);
    procedure SetMinBottom(Value : Integer);
public
    constructor Create(AOwner : TComponent); override;
    procedure Execute;
    procedure GetDefaults;
Published
    Property Options : TPageOptions     Read FOptions         Write SetOptions;
    Property MarginLeft      : Integer  Read FMarginLeft      Write SetLeft;
    Property MarginTop       : Integer  Read FMarginTop       Write SetTop;
    Property MarginRight     : Integer  Read FMarginRight     Write SetRight;
    Property MarginBottom    : Integer  Read FMarginBottom    Write SetBottom;
    Property MinMarginLeft   : Integer  Read FMinMarginLeft   Write SetMinLeft;
    Property MinMarginTop    : Integer  Read FMinMarginTop    Write SetMinTop;
    Property MinMarginRight  : Integer  Read FMinMarginRight  Write SetMinRight;
    Property MinMarginBottom : Integer  Read FMinMarginBottom Write SetMinBottom;
    Property PaperLength     : Short    Read FPaperLength;
    Property PaperWidth      : Short    Read FPaperWidth;
end;

Procedure Register;

implementation

procedure CenterWindow(Wnd: HWnd);
var
  Rect: TRect;
begin
  GetWindowRect(Wnd, Rect);
  SetWindowPos(Wnd, 0,
    (GetSystemMetrics(SM_CXSCREEN) - Rect.Right + Rect.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom + Rect.Top) div 3,
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;

function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  Result := 0;
  case Msg of
    WM_INITDIALOG:
      begin
        CenterWindow(Wnd);
        Result := 1;
      end;
  end;
end;

function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
type
    TDialogFunc = function(var DialogData): Bool stdcall;
var
    ActiveWindow: HWnd;
    WindowList: Pointer;
begin
    ActiveWindow := GetActiveWindow;
    WindowList := DisableTaskWindows(0);
    try
        Result := TDialogFunc(DialogFunc)(DialogData);
    finally
        EnableTaskWindows(WindowList);
        SetActiveWindow(ActiveWindow);
    end;
end;

procedure GetPrinter(var DeviceMode, DeviceNames: THandle);
var
  Device, Driver, Port: array[0..79] of char;
  DevNames: PDevNames;
  Offset: PChar;
begin
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
  if DeviceMode <> 0 then
  begin
    DeviceNames := GlobalAlloc(GHND, SizeOf(TDevNames) +
     StrLen(Device) + StrLen(Driver) + StrLen(Port) + 3);
    DevNames := PDevNames(GlobalLock(DeviceNames));
    try
      Offset := PChar(DevNames) + SizeOf(TDevnames);
      with DevNames^ do
      begin
        wDriverOffset := Longint(Offset) - Longint(DevNames);
        Offset := StrECopy(Offset, Driver) + 1;
        wDeviceOffset := Longint(Offset) - Longint(DevNames);
        Offset := StrECopy(Offset, Device) + 1;
        wOutputOffset := Longint(Offset) - Longint(DevNames);;
        StrCopy(Offset, Port);
      end;
    finally
      GlobalUnlock(DeviceNames);
    end;
  end;
end;


function CopyData(Handle: THandle): THandle;
var
  Src, Dest: PChar;
  Size: Integer;
begin
  if Handle <> 0 then
  begin
    Size := GlobalSize(Handle);
    Result := GlobalAlloc(GHND, Size);
    if Result <> 0 then
      try
        Src := GlobalLock(Handle);
        Dest := GlobalLock(Result);
        if (Src <> nil) and (Dest <> nil) then Move(Src^, Dest^, Size);
      finally
        GlobalUnlock(Handle);
        GlobalUnlock(Result);
      end
  end
  else Result := 0;
end;

constructor TPageSetupDialog.Create(AOwner : TComponent); 
begin
    inherited Create(AOwner);
    FOptions := [poDefaultMinMargins,poHundredthsOfMillimeters];
End;

procedure TPageSetupDialog.Execute;
var
  PageDlgRec: TPageSetupDlg;
  DevHandle: THandle;
begin
  FillChar(PageDlgRec, SizeOf(PageDlgRec), 0);
  with PageDlgRec do
  begin
    lStructSize := SizeOf(PageDlgRec);
    hInstance   := SysInit.HInstance;
    GetPrinter(DevHandle,hDevNames);
    hDevMode    := CopyData(DevHandle);
    rtMargin    := Rect(MarginLeft,MarginTop,MarginRight,MarginBottom);
    rtMinMargin := Rect(MinMarginLeft,MinMarginTop,MinMarginRight,MinMarginBottom);
    Flags       := PSD_ENABLEPAGESETUPHOOK or FFlags;
    hWndOwner   := Application.Handle;
    lpfnPageSetupHook := DialogHook;
  End;
  TaskModalDialog(@PageSetupDlg, PageDlgRec);
  with PageDlgRec do
  begin
    MarginLeft   := rtMargin.Left;
    MarginTop    := rtMargin.Top;
    MarginRight  := rtMargin.Right;
    MarginBottom := rtMargin.Bottom;
  End;
end;

procedure TPageSetupDialog.GetDefaults;
var
  PageDlgRec: TPageSetupDlg;
  DevHandle: THandle;
begin
  FillChar(PageDlgRec, SizeOf(PageDlgRec), 0);
  with PageDlgRec do
  begin
    lStructSize := SizeOf(PageDlgRec);
    hInstance   := SysInit.HInstance;
    GetPrinter(DevHandle,hDevNames);
    rtMargin    := Rect(MarginLeft,MarginTop,MarginRight,MarginBottom);
    rtMinMargin := Rect(MinMarginLeft,MinMarginTop,MinMarginRight,MinMarginBottom);
    Flags       := PSD_RETURNDEFAULT or PSD_ENABLEPAGESETUPHOOK or FFlags;
    hWndOwner   := Application.Handle;
    lpfnPageSetupHook := DialogHook;
  End;
  TaskModalDialog(@PageSetupDlg, PageDlgRec);
  with PageDlgRec do
  begin
    MarginLeft   := rtMargin.Left;
    MarginTop    := rtMargin.Top;
    MarginRight  := rtMargin.Right;
    MarginBottom := rtMargin.Bottom;
  End;
end;


procedure TPageSetupDialog.SetOptions(Value : TPageOptions);
Begin
    If (poDefaultMinMargins in Value) And Not (poDefaultMinMargins in FOptions) Then
        Value := Value - [poMinMargins];
    If (poMinMargins in Value) And Not (poMinMargins in FOptions) Then
        Value := Value - [poDefaultMinMargins];
    If (poHundredthsOfMillimeters in Value) And Not (poHundredthsOfMillimeters in FOptions) Then
        Value := Value - [poThousandthsOfInches];
    If (poThousandthsOfInches in Value) And Not (poThousandthsOfInches in FOptions) Then
        Value := Value - [poHundredthsOfMillimeters];
    FOptions := Value;
    FFlags := 0;
    If poDefaultMinMargins in FOptions then
        FFlags := FFlags or PSD_DEFAULTMINMARGINS;
    If poDisableMargins in FOptions then
        FFlags := FFlags or PSD_DISABLEMARGINS;
    If poMargins in FOptions then
        FFlags := FFlags or PSD_MARGINS;
    If poMinMargins in FOptions then
        FFlags := FFlags or PSD_MINMARGINS;
    If poDisableOrientation in FOptions then
        FFlags := FFlags or PSD_DISABLEORIENTATION;
    If poDisablePaper in FOptions then
        FFlags := FFlags or PSD_DISABLEPAPER;
    If poDisablePrinter in FOptions then
        FFlags := FFlags or PSD_DISABLEPRINTER;
    If poHundredthsOfMillimeters in FOptions then
        FFlags := FFlags or PSD_INHUNDREDTHSOFMILLIMETERS;
    If poThousandthsOfInches in FOptions then
        FFlags := FFlags or PSD_INTHOUSANDTHSOFINCHES;
End;

procedure TPageSetupDialog.SetLeft(Value : Integer);
Begin
    If Value > FMinMarginLeft Then
        FMarginLeft := Value;
End;

procedure TPageSetupDialog.SetTop(Value : Integer);
Begin
    If Value >= FMinMarginTop Then
        FMarginTop := Value;
End;

procedure TPageSetupDialog.SetRight(Value : Integer);
Begin
    If Value >= FMinMarginRight Then
        FMarginRight := Value;
End;

procedure TPageSetupDialog.SetBottom (Value : Integer);
Begin
    If Value >= FMinMarginBottom Then
        FMarginBottom := Value;
End;

procedure TPageSetupDialog.SetMinLeft(Value : Integer);
Begin
    If Value <= FMarginLeft Then
        FMinMarginLeft := Value;
End;

procedure TPageSetupDialog.SetMinTop(Value : Integer);
Begin
    If Value <= FMarginTop Then
        FMinMarginTop := Value;
End;

procedure TPageSetupDialog.SetMinRight(Value : Integer);
Begin
    If Value <= FMarginRight Then
        FMinMarginRight := Value;
End;

procedure TPageSetupDialog.SetMinBottom (Value : Integer);
Begin
    If Value <= FMarginBottom Then
        FMinMarginBottom := Value;
    Self.GetDefaults;
End;

Procedure Register;
Begin
    RegisterComponents('Dialogs',[TPageSetupDialog]);
End;


end.
