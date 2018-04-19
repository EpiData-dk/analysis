unit textviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, SynEdit, outputviewer_types, outputgenerator_base, SynEditHighlighter,
  executor, Menus;

type

  { TTextPanel }

  TTextPanel = class(TTabSheet, IAnaOutputViewer)
  private
    FPopupMenu: TPopupMenu;
    FHighlighter: TSynCustomHighlighter;
    FEdit: TSynEdit;
    procedure CopySelectClipBoardClick(Sender: TObject);
    procedure CopyAllClipboardClick(Sender: TObject);
    procedure ClearOutputClick(Sender: TObject);
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
  Controls, outputgenerator_txt, ana_globals, StdCtrls, LCLType, options_fontoptions, Graphics,
  options_utils, Clipbrd, main;

type

  { TTextViewHighlighter }

  TTextViewHighlighter = class(TSynCustomHighlighter)
  private
    FTokenPos, FTokenEnd: Integer;
    FLineText: STring;
    FAttr: TSynHighlighterAttributes;
  protected
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateAttributes(AFont: TFont);
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function GetEol: Boolean; override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  end;

{ TTextViewHighlighter }

function TTextViewHighlighter.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  Result := FAttr;
end;

procedure TTextViewHighlighter.UpdateAttributes(AFont: TFont);
begin
  FAttr.BeginUpdate;
  FAttr.Foreground := AFont.Color;
  FAttr.Style      := AFont.Style;
  FAttr.EndUpdate;
end;

procedure TTextViewHighlighter.SetLine(const NewValue: String;
  LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);
  FLineText := NewValue;
  FTokenEnd := 1;
  Next;
end;

constructor TTextViewHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAttr := TSynHighlighterAttributes.Create('all', 'all');
  AddAttribute(FAttr);

  SetAttributesOnChange(@DefHighlightChange);
end;

function TTextViewHighlighter.GetEol: Boolean;
begin
  result := FTokenPos > Length(FLineText);
end;

function TTextViewHighlighter.GetToken: String;
begin
  result := FLineText;
end;

function TTextViewHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  result := FAttr;
end;

procedure TTextViewHighlighter.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenStart := @FLineText[1];
  TokenLength := Length(FLineText);
end;

function TTextViewHighlighter.GetTokenKind: integer;
begin
  Result := 1;
end;

function TTextViewHighlighter.GetTokenPos: Integer;
begin
  Result := 0;
end;

procedure TTextViewHighlighter.Next;
begin
  FTokenPos := FTokenEnd;
  FTokenEnd := Length(FLineText) + 1;
end;

{ TTextPanel }

procedure TTextPanel.CopySelectClipBoardClick(Sender: TObject);
begin
  Clipboard.AsText := FEdit.SelText;
end;

procedure TTextPanel.CopyAllClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := FEdit.Text;
end;

procedure TTextPanel.ClearOutputClick(Sender: TObject);
begin
  MainForm.InterfaceRunCommand('cls;');
end;

procedure TTextPanel.InvalidateView;
begin
  //
end;

procedure TTextPanel.Initialize;
var
  i, Idx: Integer;
  Item: TMenuItem;
begin
  FEdit := TSynEdit.Create(Self);
  FEdit.Align := alClient;
  FEdit.Parent := Self;
  FEdit.ReadOnly := true;

  FEdit.ScrollBars := ssBoth;
  FEdit.Gutter.Visible := false;
  FEdit.RightGutter.Visible := false;
  FEdit.RightEdge := -1;

  FHighlighter := TTextViewHighlighter.Create(FEdit);
  FEdit.Highlighter := FHighlighter;

  for i := 1 to 9 do
    begin
      Idx := FEdit.Keystrokes.FindKeycode(VK_0 + i, [ssCtrl, ssShift]);
      if (Idx >= 0) then
        FEdit.Keystrokes.Delete(Idx);
    end;

  FEdit.Options := [eoAutoIndent, eoGroupUndo, eoSmartTabs, eoTabsToSpaces];


  FPopupMenu := TPopupMenu.Create(Self);
  FEdit.PopupMenu := FPopupMenu;

  Item := TMenuItem.Create(FPopupMenu);
  Item.Caption := 'Copy selected to clipboard';
  Item.OnClick := @CopySelectClipBoardClick;
  FPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FPopupMenu);
  Item.Caption := 'Copy all to clipboard';
  Item.OnClick := @CopyAllClipboardClick;
  FPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FPopupMenu);
  Item.Caption := '-';
  FPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FPopupMenu);
  Item.Caption := 'Clear Output';
  Item.OnClick := @ClearOutputClick;
  FPopupMenu.Items.Add(Item);
end;

procedure TTextPanel.LoadFromStream(ST: TStream);
var
  SS: TStringStream;
begin
  ST.Position := 0;

  if not (ST is TStringStream) then
  begin
    SS := TStringStream.Create('');
    SS.CopyFrom(ST, ST.Size);
    FEdit.Text := SS.DataString;
    SS.Free;
  end else
    FEdit.Text := TStringStream(ST).DataString;

  FEdit.TopLine := MaxInt;
end;

procedure TTextPanel.UpdateFontAndSize(AExecutor: TExecutor);
var
  AFont: TFont;
begin
  AFont := FontFromSetOptions(
             ANA_SO_OUTPUT_FONT_NAME,
             ANA_SO_OUTPUT_FONT_SIZE,
             ANA_SO_OUTPUT_FONT_COLOR,
             ANA_SO_OUTPUT_FONT_STYLE,
             AExecutor.SetOptions
           );

  FEdit.Font.Assign(AFont);
  FEdit.Color := TFontColorOption(AExecutor.SetOptions[ANA_SO_OUTPUT_BG_COLOR]).GetBGRValue;

  TTextViewHighlighter(FHighlighter).UpdateAttributes(AFont);

  AFont.Free;
end;

function TTextPanel.GetOutputGeneratorClass: TOutputGeneratorClass;
begin
  result := TOutputGeneratorTXT;
end;

function TTextPanel.GetLineAtCaret: String;
begin
  result := FEdit.LineText;
end;

function TTextPanel.GetSelectedText: String;
begin
  result := FEdit.SelText;
end;

function TTextPanel.GetCaretPos: TPoint;
begin
  result := FEdit.CaretXY;
end;

function TTextPanel.IsFocused: Boolean;
begin
  result := FEdit.Focused;
end;

end.

