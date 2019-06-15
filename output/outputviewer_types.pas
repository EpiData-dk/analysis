unit outputviewer_types;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, outputgenerator_base, executor, Menus;

type

  TOutputViewerPopup = class;

  { IAnaOutputViewer }

  IAnaOutputViewer = interface ['IAnaOutputViewer']
  // Viewing content
    procedure Initialize;
    procedure InvalidateView;
    procedure LoadFromStream(ST: TStream);
    procedure UpdateFontAndSize(AExecutor: TExecutor);
    function GetOutputGeneratorClass: TOutputGeneratorClass;

  // Context menu
    function GetContextMenu: TOutputViewerPopup;

  // Methods for help context system
    function GetLineAtCaret: String;
    function GetSelectedText: String;
    function GetCaretPos: TPoint;
    function IsFocused: Boolean;
  end;

  { TOutputViewerPopup }

  TOutputViewerPopup = class(TPopupMenu)
  private
    FOnClearClick: TNotifyEvent;
    FOnCopyAllClick: TNotifyEvent;
    FOnCopySelectedClick: TNotifyEvent;
    FOnSaveOutputClick: TNotifyEvent;
    procedure ClearOutputClick(Sender: TObject);
    procedure CopyAllClipboardClick(Sender: TObject);
    procedure CopySelectClipBoardClick(Sender: TObject);
    procedure SaveOutputClick(Sender: TObject);
    procedure SetOnClearClick(AValue: TNotifyEvent);
    procedure SetOnCopyAllClick(AValue: TNotifyEvent);
    procedure SetOnCopySelectedClick(AValue: TNotifyEvent);
    procedure SetOnSaveOutputClick(AValue: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    // Callback when Copy Selected to Clipboard is clicked
    property OnCopySelectedClick: TNotifyEvent read FOnCopySelectedClick write SetOnCopySelectedClick;
    // Callback when Copy All to Clipboard is clicked
    property OnCopyAllClick: TNotifyEvent read FOnCopyAllClick write SetOnCopyAllClick;
    // Callback when Clear Output is clicked. There is a default handler for this
    // so it may not need to be assigned.
    property OnClearClick: TNotifyEvent read FOnClearClick write SetOnClearClick;
    // Callback when Save Output is clicked.
    property OnSaveOutputClick: TNotifyEvent read FOnSaveOutputClick write SetOnSaveOutputClick;
  end;

implementation

uses
  main;

{ TOutputViewerPopup }

procedure TOutputViewerPopup.SetOnClearClick(AValue: TNotifyEvent);
begin
  if FOnClearClick = AValue then Exit;
  FOnClearClick := AValue;
end;

procedure TOutputViewerPopup.CopySelectClipBoardClick(Sender: TObject);
begin
  if Assigned(OnCopySelectedClick) then
    OnCopySelectedClick(Sender);
end;

procedure TOutputViewerPopup.SaveOutputClick(Sender: TObject);
begin
  if Assigned(OnSaveOutputClick) then
    OnSaveOutputClick(Sender);
end;

procedure TOutputViewerPopup.CopyAllClipboardClick(Sender: TObject);
begin
  if Assigned(OnCopyAllClick) then
    OnCopyAllClick(Sender);
end;

procedure TOutputViewerPopup.ClearOutputClick(Sender: TObject);
begin
  if Assigned(OnClearClick) then
    OnClearClick(Sender)
  else
    MainForm.InterfaceRunCommand('cls;');
end;

procedure TOutputViewerPopup.SetOnCopyAllClick(AValue: TNotifyEvent);
begin
  if FOnCopyAllClick = AValue then Exit;
  FOnCopyAllClick := AValue;
end;

procedure TOutputViewerPopup.SetOnCopySelectedClick(AValue: TNotifyEvent);
begin
  if FOnCopySelectedClick = AValue then Exit;
  FOnCopySelectedClick := AValue;
end;

procedure TOutputViewerPopup.SetOnSaveOutputClick(AValue: TNotifyEvent);
begin
  if FOnSaveOutputClick = AValue then Exit;
  FOnSaveOutputClick := AValue;
end;

constructor TOutputViewerPopup.Create(AOwner: TComponent);
var
  Item: TMenuItem;
begin
  inherited Create(AOwner);

  Item := TMenuItem.Create(Self);
  Item.Caption := 'Copy selected to clipboard';
  Item.OnClick := @CopySelectClipBoardClick;
  Items.Add(Item);

  Item := TMenuItem.Create(Self);
  Item.Caption := 'Copy all to clipboard';
  Item.OnClick := @CopyAllClipboardClick;
  Items.Add(Item);

  Item := TMenuItem.Create(Self);
  Item.Caption := '-';
  Items.Add(Item);

  Item := TMenuItem.Create(Self);
  Item.Caption := 'Save Output';
  Item.OnClick := @SaveOutputClick;
  Items.Add(Item);

  Item := TMenuItem.Create(Self);
  Item.Caption := 'Clear Output';
  Item.OnClick := @ClearOutputClick;
  Items.Add(Item);
end;

end.

