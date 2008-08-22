{+--------------------------------------------------------------------------+
 | Unit:        mwSupportClasses
 | Last change: 1999-11-17
 | Description: Supporting classes for mwCustomEdit.
 | Version:     0.90
 +--------------------------------------------------------------------------+}

unit mwSupportClasses;

{$I MWEDIT.INC}

interface

uses Windows, Classes, Graphics, Controls, SysUtils;

type
  TmwSelectedColor = class(TPersistent)
  private
    fBG: TColor;
    fFG: TColor;
    fOnChange: TNotifyEvent;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
  public
    constructor Create;
  published
    property Background: TColor read fBG write SetBG default clHighLight;
    property Foreground: TColor read fFG write SetFG default clHighLightText;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TmwGutter = class(TPersistent)
  private
    fColor: TColor;
    fWidth: integer;
    fShowLineNumbers: boolean;
    fDigitCount: integer;
    fLeadingZeros: boolean;
    fZeroStart: boolean;
    fLeftOffset: integer;
    fRightOffset: integer;
    fOnChange: TNotifyEvent;
    fCursor: TCursor;
    procedure SetColor(const Value: TColor);
    procedure SetDigitCount(Value: integer);
    procedure SetLeadingZeros(const Value: boolean);
    procedure SetLeftOffset(Value: integer);
    procedure SetRightOffset(Value: integer);
    procedure SetShowLineNumbers(const Value: boolean);
    procedure SetWidth(Value: integer);
    procedure SetZeroStart(const Value: boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function FormatLineNumber(Line: integer): string;
    function RealGutterWidth(CharWidth: integer): integer;
  published
    property Color: TColor read fColor write SetColor default clBtnFace;
    property Cursor: TCursor read fCursor write fCursor default crDefault;
    property DigitCount: integer read fDigitCount write SetDigitCount
                                                  default 4;
    property LeadingZeros: boolean read fLeadingZeros write SetLeadingZeros
                                                      default FALSE;
    property LeftOffset: integer read fLeftOffset write SetLeftOffset
                                                  default 16;
    property RightOffset: integer read fRightOffset write SetRightOffset
                                                    default 2;
    property ShowLineNumbers: boolean read fShowLineNumbers
                                      write SetShowLineNumbers default FALSE;
    property Width: integer read fWidth write SetWidth default 30;
    property ZeroStart: boolean read fZeroStart write SetZeroStart default FALSE;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TmwBookMarkOpt = class(TPersistent)
  private
    fBookmarkImages: TImageList;
    fEnableKeys: Boolean;
    fGlyphsVisible: Boolean;
    fLeftMargin: Integer;
    fOwner: TComponent;
    fXoffset: integer;
    fOnChange: TNotifyEvent;
    procedure SetBookmarkImages(const Value: TImageList);
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetXOffset(Value: integer);
  public
    constructor Create(AOwner: TComponent);
  published
    property BookmarkImages: TImageList
               read fBookmarkImages write SetBookmarkImages;
    property EnableKeys: Boolean
               read fEnableKeys write fEnableKeys default True;
    property GlyphsVisible: Boolean
               read fGlyphsVisible write SetGlyphsVisible default True;
    property LeftMargin: Integer read fLeftMargin write SetLeftMargin default 2;
    property Xoffset: integer read fXoffset write SetXOffset default 12;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  { TmwMethodChain }

  EmwMethodChain = class(Exception);
  TmwExceptionEvent = procedure (Sender: TObject; E: Exception;
    var DoContinue: Boolean) of object;

  TmwMethodChain = class
  private
    FNotifyProcs: TList;
    FExceptionHandler: TmwExceptionEvent;
  protected
    procedure DoFire(AEvent: TMethod); virtual; abstract;
    function DoHandleException(E: Exception): Boolean; virtual;
    property ExceptionHandler: TmwExceptionEvent read FExceptionHandler
      write FExceptionHandler;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AEvent: TMethod);
    procedure Remove(AEvent: TMethod);
    procedure Fire;
  end;

  { TmwNotifyEventChain }

  TmwNotifyEventChain = class(TmwMethodChain)
  private
    FSender: TObject;
  protected
    procedure DoFire(AEvent: TMethod); override;
  public
    constructor CreateEx(ASender: TObject);
    procedure Add(AEvent: TNotifyEvent);
    procedure Remove(AEvent: TNotifyEvent);
    property ExceptionHandler;
    property Sender: TObject read FSender write FSender;
  end;

  TmwInternalImage = class(TObject)
  public
    constructor Create(const Name: string; Count: integer);
    destructor Destroy; override;
    procedure DrawMark(ACanvas: TCanvas; Number, X, Y, LineHeight: integer);
  end;

implementation

uses {SysUtils, }mwSupportProcs;

{ TmwSelectedColor }

constructor TmwSelectedColor.Create;
begin
  inherited Create;
  fBG := clHighLight;
  fFG := clHighLightText;
end;

procedure TmwSelectedColor.SetBG(Value: TColor);
begin
  if (fBG <> Value) then begin
    fBG := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwSelectedColor.SetFG(Value: TColor);
begin
  if (fFG <> Value) then begin
    fFG := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TmwGutter }

constructor TmwGutter.Create;
begin
  inherited Create;
  fColor := clBtnFace;
  fWidth := 30;
  fShowLineNumbers := FALSE;
  fLeadingZeros := FALSE;
  fZeroStart := FALSE;
  fLeftOffset := 16;
  fDigitCount := 4;
  fRightOffset := 2;
end;

procedure TmwGutter.Assign(Source: TPersistent);
var src: TmwGutter absolute Source;
begin
  if Assigned(Source) and (Source is TmwGutter) then begin
    fColor := src.fColor;
    fWidth := src.fWidth;
    fShowLineNumbers := src.fShowLineNumbers;
    fLeadingZeros := src.fLeadingZeros;
    fZeroStart := src.fZeroStart;
    fLeftOffset := src.fLeftOffset;
    fDigitCount := src.fDigitCount;
    fRightOffset := src.fRightOffset;
    if Assigned(fOnChange) then fOnChange(Self);
  end else
    inherited;
end;

function TmwGutter.FormatLineNumber(Line: integer): string;
var i: integer;
begin
  if fZeroStart then Dec(Line);
  Str(Line : fDigitCount, Result);
  if fLeadingZeros then
    for i := 1 to fDigitCount - 1 do begin
      if (Result[i] <> ' ') then break;
      Result[i] := '0';
    end;
end;

function TmwGutter.RealGutterWidth(CharWidth: integer): integer;
begin
  if fShowLineNumbers then
    Result := fLeftOffset + fRightOffset + fDigitCount * CharWidth + 2
  else
    Result := fWidth;
end;

procedure TmwGutter.SetColor(const Value: TColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwGutter.SetDigitCount(Value: integer);
begin
  Value := MinMax(Value, 2, 12);
  if fDigitCount <> Value then begin
    fDigitCount := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwGutter.SetLeadingZeros(const Value: boolean);
begin
  if fLeadingZeros <> Value then begin
    fLeadingZeros := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwGutter.SetLeftOffset(Value: integer);
begin
  Value := Max(0, Value);
  if fLeftOffset <> Value then begin
    fLeftOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwGutter.SetRightOffset(Value: integer);
begin
  Value := Max(0, Value);
  if fRightOffset <> Value then begin
    fRightOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwGutter.SetShowLineNumbers(const Value: boolean);
begin
  if fShowLineNumbers <> Value then begin
    fShowLineNumbers := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwGutter.SetWidth(Value: integer);
begin
  Value := Max(0, Value);
  if fWidth <> Value then begin
    fWidth := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwGutter.SetZeroStart(const Value: boolean);
begin
  if fZeroStart <> Value then begin
    fZeroStart := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TmwBookMarkOpt }

constructor TmwBookMarkOpt.Create(AOwner: TComponent);
begin
  inherited Create;
  fEnableKeys := True;
  fGlyphsVisible := True;
  fLeftMargin := 2;
  fOwner := AOwner;
  fXOffset := 12;
end;

procedure TmwBookMarkOpt.SetBookmarkImages(const Value: TImageList);
begin
  if fBookmarkImages <> Value then begin
    fBookmarkImages := Value;
    if Assigned(fBookmarkImages) then fBookmarkImages.FreeNotification(fOwner);
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwBookMarkOpt.SetGlyphsVisible(Value: Boolean);
begin
  if fGlyphsVisible <> Value then begin
    fGlyphsVisible := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwBookMarkOpt.SetLeftMargin(Value: Integer);
begin
  if fLeftMargin <> Value then begin
    fLeftMargin := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TmwBookMarkOpt.SetXOffset(Value: integer);
begin
  if fXOffset <> Value then begin
    fXOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TmwMethodChain }

procedure TmwMethodChain.Add(AEvent: TMethod);
begin
  if not Assigned(@AEvent) then
    raise EmwMethodChain.CreateFmt(
      '%s.Entry : the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    Add(Code);
    Add(Data);
  end
end;

constructor TmwMethodChain.Create;
begin
  inherited;
  FNotifyProcs := TList.Create;
end;

destructor TmwMethodChain.Destroy;
begin
  FNotifyProcs.Free;
  inherited;
end;

function TmwMethodChain.DoHandleException(E: Exception): Boolean;
begin
  if not Assigned(FExceptionHandler) then
    raise E
  else
    try
      Result := True;
      FExceptionHandler(Self, E, Result);
    except
      raise EmwMethodChain.CreateFmt(
        '%s.DoHandleException : MUST NOT occur any kind of exception in '+
        'ExceptionHandler', [ClassName]);
    end;
end;

procedure TmwMethodChain.Fire;
var
  AMethod: TMethod;
  i: Integer;
begin
  i := 0;
  with FNotifyProcs, AMethod do
    while i < Count do
      try
        repeat
          Code := Items[i];
          Inc(i);
          Data := Items[i];
          Inc(i);

          DoFire(AMethod)
        until i >= Count;
      except
        on E: Exception do
          if not DoHandleException(E) then
            i := MaxInt;
      end;
end;

procedure TmwMethodChain.Remove(AEvent: TMethod);
var
  i: Integer;
begin
  if not Assigned(@AEvent) then
    raise EmwMethodChain.CreateFmt(
      '%s.Remove: the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    i := Count - 1;
    while i > 0 do
      if Items[i] <> Data then
        Dec(i, 2)
      else
      begin
        Dec(i);
        if Items[i] = Code then
        begin
          Delete(i);
          Delete(i);
        end;
        Dec(i);
      end;
  end;
end;

{ TmwNotifyEventChain }

procedure TmwNotifyEventChain.Add(AEvent: TNotifyEvent);
begin
  inherited Add(TMethod(AEvent));
end;

constructor TmwNotifyEventChain.CreateEx(ASender: TObject);
begin
  inherited Create;
  FSender := ASender;
end;

procedure TmwNotifyEventChain.DoFire(AEvent: TMethod);
begin
  TNotifyEvent(AEvent)(FSender);
end;

procedure TmwNotifyEventChain.Remove(AEvent: TNotifyEvent);
begin
  inherited Remove(TMethod(AEvent));
end;

var
  InternalImages: TBitmap;
  InternalImagesUsers: integer;
  IIWidth, IIHeight: integer;
  IICount: integer;

constructor TmwInternalImage.Create(const Name: string; Count: integer);
begin
  inherited Create;
  Inc(InternalImagesUsers);
  if InternalImagesUsers = 1 then begin
    InternalImages := TBitmap.Create;
    InternalImages.LoadFromResourceName(HInstance, Name);
    IIWidth := (InternalImages.Width + Count shr 1) div Count;
    IIHeight := InternalImages.Height;
    IICount := Count;
  end;
end;

destructor TmwInternalImage.Destroy;
begin
  Dec(InternalImagesUsers);
  if InternalImagesUsers = 0 then begin
    InternalImages.Free;
    InternalImages := nil;
  end;
  inherited Destroy;
end;

procedure TmwInternalImage.DrawMark(ACanvas: TCanvas;
                                    Number, X, Y, LineHeight: integer);
var rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < IICount) then
  begin
    if LineHeight >= IIHeight then begin
      rcSrc := Rect(Number * IIWidth, 0, (Number + 1) * IIWidth, IIHeight);
      Inc(Y, (LineHeight - IIHeight) div 2);
      rcDest := Rect(X, Y, X + IIWidth, Y + IIHeight);
    end else begin
      rcDest := Rect(X, Y, X + IIWidth, Y + LineHeight);
      Y := (IIHeight - LineHeight) div 2;
      rcSrc := Rect(Number * IIWidth, Y, (Number + 1) * IIWidth, Y + LineHeight);
    end;
    ACanvas.CopyRect(rcDest, InternalImages.Canvas, rcSrc);
  end;
end;

end.
