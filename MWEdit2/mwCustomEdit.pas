{+-----------------------------------------------------------------------------+
 | Class:       TmwCustomEdit
 | Created:     1998-11
 | Last change: 1999-11-18
 | Author:      Martin Waldenburg
 | Description: study on how to create a custom edit control without using
 |              a Windows edit control.
 | Version:     0.90 public beta (see VERSION.RTF for version history)
 | Copyright (c) 1998 Martin Waldenburg
 | All rights reserved.
 |
 | Thanks to : Woo Young Bum, Angus Johnson, Michael Trier, James Jacobson,
 |             Thomas Kurz, Primoz Gabrijelcic, Michael Beck, Andy Jeffries,
 |             Edward Kreis, Brad Stowers, Willo van der Merwe, Bernt Levinsson,
 |             Ted Berg, Michael Hieke, Dragan Grbic, Lucifer, Kees van Spelde,
 |             Hideo Koiso, Albert Research, Theodoros Bebekis, Heedong Lim,
 |             xyeyu, ArentJan Banck, Alexander Reiter, Tohru Hanai,
 |             Winfried Schoettler, Daniel Rodríguez Herrera, Hiep Ma,
 |             Nur Ismail, Milan Nikolic, Wynand Breytenbach
 |
 | LICENCE CONDITIONS
 |
 | USE OF THE ENCLOSED SOFTWARE
 | INDICATES YOUR ASSENT TO THE
 | FOLLOWING LICENCE CONDITIONS.
 |
 |
 |
 | These Licence Conditions are exlusively
 | governed by the Law and Rules of the
 | Federal Republic of Germany.
 |
 | Redistribution and use in source and binary form, with or without
 | modification, are permitted provided that the following conditions
 | are met:
 |
 | 1. Redistributions of source code must retain the above copyright
 |    notice, this list of conditions and the following disclaimer.
 |    If the source is modified, the complete original and unmodified
 |    source code has to distributed with the modified version.
 |
 | 2. Redistributions in binary form must reproduce the above
 |    copyright notice, these licence conditions and the disclaimer
 |    found at the end of this licence agreement in the documentation
 |    and/or other materials provided with the distribution.
 |
 | 3. Software using this code must contain a visible line of credit.
 |
 | 4. If my code is used in a "for profit" product, you have to donate
 |    to a registered charity in an amount that you feel is fair.
 |    You may use it in as many of your products as you like.
 |    Proof of this donation must be provided to the author of
 |    this software.
 |
 | 5. If you for some reasons don't want to give public credit to the
 |    author, you have to donate three times the price of your software
 |    product, or any other product including this component in any way,
 |    but no more than $500 US and not less than $200 US, or the
 |    equivalent thereof in other currency, to a registered charity.
 |    You have to do this for every of your products, which uses this
 |    code separately.
 |    Proof of this donations must be provided to the author of
 |    this software.
 |
 |
 | DISCLAIMER:
 |
 | THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS'.
 |
 | ALL EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 | THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 | PARTICULAR PURPOSE ARE DISCLAIMED.
 |
 | IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 | INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 | (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 | OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 | INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 | WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 | NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 | THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 |
 |  Martin.Waldenburg@T-Online.de
 |
 | Known problems:
 |   - dragging cannot be canceled with the chord clicking - don't know how to
 |     fix!
 |
 +----------------------------------------------------------------------------+}

{$I MWEDIT.INC}

unit mwCustomEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  mwKeyCmds,
  mwEditSearch,
  mwLocalStr, mwSupportProcs, mwSupportClasses,
  Printers,
  {$IFDEF MWE_MBCSSUPPORT}
  Imm,
  {$ENDIF}
  ExtCtrls, Forms, StdCtrls, Clipbrd, mwHighlighter,
  ShellAPI,
  uTextDrawer;

const
  DIGIT = ['0'..'9'];
// ALPHA            = ['A'..'Z', 'a'..'z'];
// break these up because we exceed the 4 byte limit when combined.
  ALPHA_UC = ['A'..'Z'];
  ALPHA_LC = ['a'..'z'];

   // not defined in all Delphi versions
  WM_MOUSEWHEEL = $020A;

   // maximum scroll range
  MAX_SCROLL = 32767;

// Max number of book/gutter marks returned from GetMarksForLine - that really
// should be enough.
  maxMarks = 16;

  MWEDIT_CLIPBOARD_FORMAT = 'mwEdit Control Block Type';

var
  mwEditClipboardFormat: UINT;

  {$IFDEF MWE_MBCSSUPPORT}
  {$IFNDEF MWE_COMPILER_4_UP}
{Windows.pas in D4}
const
  C3_NONSPACING = 1; { nonspacing character }
  C3_DIACRITIC = 2; { diacritic mark }
  C3_VOWELMARK = 4; { vowel mark }
  C3_SYMBOL = 8; { symbols }
  C3_KATAKANA = $0010; { katakana character }
  C3_HIRAGANA = $0020; { hiragana character }
  C3_HALFWIDTH = $0040; { half width character }
  C3_FULLWIDTH = $0080; { full width character }
  C3_IDEOGRAPH = $0100; { ideographic character }
  C3_KASHIDA = $0200; { Arabic kashida character }
  C3_LEXICAL = $0400; { lexical character }
  C3_ALPHA = $8000; { any linguistic char (C1_ALPHA) }
  C3_NOTAPPLICABLE = 0; { ctype 3 is not applicable }
  {$ENDIF}
  {$ENDIF}

type
  TmwEditExporter = (cfRTF, cfHTML);
  TmwEditExporters = set of TmwEditExporter;

  TmwSearchOption = (mwsoMatchCase, mwsoWholeWord, mwsoBackwards,
    mwsoEntireScope, mwsoSelectedOnly, mwsoReplace, mwsoReplaceAll, mwsoPrompt);
  TmwSearchOptions = set of TmwSearchOption;

  TmwReplaceAction = (mwraCancel, mwraSkip, mwraReplace, mwraReplaceAll);

  EmwEditError = class(Exception);

  PSelectionMode = ^TSelectionMode;
  TSelectionMode = (smNormal, smColumn, smLine);

  TIndexEvent = procedure(Index: Integer) of object;
  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;
  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: integer;
    var Special: boolean; var FG, BG: TColor) of object;
  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    string; Line, Column: integer; var Action: TmwReplaceAction) of object;

  TDropFilesEvent = procedure(Sender: TObject; X, Y: integer; Files: TStrings)
    of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TmwEditorCommand;
    var AChar: char; Data: pointer) of object;

  TCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  // added mw as a prefix otherwise crNone conflicts with the
  // crNone cursor constant.
  TChangeReason = (mwcrInsert, mwcrPaste, mwcrDragDropInsert,
    mwcrDeleteAfterCursor, mwcrDelete, mwcrSelDelete,
    mwcrDragDropDelete, mwcrLineBreak, mwcrNone);

  TChangePtr = ^TChange;
  TChange = record
    ChangeStr: PChar;
    ChangeReason: TChangeReason;
    ChangeStartPos,
      ChangeEndPos: TPoint;
    ChangeSelMode: TSelectionMode;
  end;

  TmwPrintStatus = (psBegin, psNewPage, psEnd);
  TPrintStatusEvent = procedure(Sender: TObject; Status: TmwPrintStatus;
    PageNumber: integer; var Abort: boolean) of object;

  TmwMarginUnits = (muPixels, muThousandthsOfInches, muMillimeters);
  TmwPrintOptions = record
    SelectedOnly: boolean;
    Highlighted: boolean;
    WrapLongLines: boolean;
    IgnoreColors: boolean;
    Copies: integer;
    MarginUnits: TmwMarginUnits;
    Margins: TRect;
    PrintRange: TRect;
    Title: string;
    Header: TStringList;
    Footer: TStringList;
  end;

  TmwHeaderFooterAlign = (hfaLeft, hfaRight, hfaCenter);

  TmwStateFlag = (mwsfCaretChanged, mwsfScrollbarChanged,
    mwsfLinesChanging, mwsfInScrollLoop,
    mwsfCaretVisible, mwsfDblClicked, mwsfWaitForDragging);
  TmwStateFlags = set of TmwStateFlag;

  TmwEditorOption = (mweoAutoIndent, mweoDragDropEditing, mweoDropFiles,
    mweoHalfPageScroll, mweoScrollPastEol, mweoShowScrollHint,
    mweoTabsToSpaces, mweoSmartTabs);
  TmwEditorOptions = set of TmwEditorOption;

const
  MWEDIT_DEFAULT_OPTIONS = [mweoAutoIndent, mweoDragDropEditing,
    mweoScrollPastEol, mweoShowScrollHint,
    mweoSmartTabs, mweoTabsToSpaces];

type

  TmwStatusChange = (mwscCaretX, mwscCaretY, mwscLeftChar, mwscTopLine,
    mwscInsertMode);
  TmwStatusChanges = set of TmwStatusChange;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TmwStatusChanges)
    of object;

  TmwCustomEdit = class;

  TMark = class
  protected
    fLine, fColumn, fImage: Integer;
    fEdit: TmwCustomEdit;
    fVisible: boolean;
    fInternalImage: boolean;
    fBookmarkNum: integer;
    function GetEdit: TmwCustomEdit; virtual;
    procedure SetColumn(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: boolean);
    procedure SetInternalImage(const Value: boolean);
    function GetIsBookmark: boolean;
    procedure SetIsBookmark(const Value: boolean);
  public
    constructor Create(owner: TmwCustomEdit);
    property Line: integer read fLine write SetLine;
    property Column: integer read fColumn write SetColumn;
    property ImageIndex: integer read fImage write SetImage;
    property BookmarkNumber: integer read fBookmarkNum write fBookmarkNum;
    property Visible: boolean read fVisible write SetVisible;
    property InternalImage: boolean read fInternalImage write SetInternalImage;
    property IsBookmark: boolean read GetIsBookmark write SetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var mark: TMark) of object;

  TMarks = array[1..maxMarks] of TMark;

  { A list of mark objects. Each object cause a litle picture to be drawn in the
    gutter. }
  TMarkList = class(TList)
  protected
    fEdit: TmwCustomEdit;
    fOnChange: TNotifyEvent;
    function Get(Index: Integer): TMark;
    procedure Put(Index: Integer; Item: TMark);
    procedure DoChange;
  public
    constructor Create(owner: TmwCustomEdit);
    function Add(Item: TMark): Integer;
    function First: TMark;
    function Last: TMark;
    procedure Insert(Index: Integer; Item: TMark);
    function Remove(Item: TMark): Integer;
    procedure Delete(Index: Integer);
    procedure ClearLine(line: integer);
    procedure Place(mark: TMark);
    procedure GetMarksForLine(line: integer; var marks: TMarks);
    property Items[Index: Integer]: TMark read Get write Put; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TUndoList = class
  private
    fList: TList;
    fCanUndo: Integer;
    fMaxUndo: Integer;
    fOwner: TmwCustomEdit;
    fUndoLocked: Boolean;
    function GetCanUndo: Integer;
    procedure SetMaxUndo(const Value: Integer);
  protected
    procedure RemoveChange(index: Integer);
  public
    constructor Create(AOwner: TmwCustomEdit);
    destructor Destroy; override;
    procedure AddChange(ChangeReason: TChangeReason; ChangeStartPos,
      ChangeEndPos: TPoint; ChangeStr: PChar; ChangeSelMode: TSelectionMode);
    function GetChange(var ChangeStartPos, ChangeEndPos: TPoint;
      var ChangeStr: PChar; var ChangeSelMode: TSelectionMode): TChangeReason;
    {$IFDEF UNDO_DEBUG}
    function GetChange2(var ChangeStartPos, ChangeEndPos: TPoint;
      var ChangeStr: PChar; var ChangeSelMode: TSelectionMode; i: Integer):
        TChangeReason;
    {$ENDIF}
    function GetChangeReason: TChangeReason;
    procedure ClearList;
    procedure LockUndo;
    procedure UnLockUndo;
    property CanUndo: Integer read GetCanUndo;
    property MaxUndo: Integer read FMaxUndo write SetMaxUndo;
  end;

  TmwEditList = class(TStringList)
  private
    FOnAdded: TNotifyEvent;
    fOnCleared: TNotifyEvent;
    FOnDeleted: TIndexEvent;
    FOnInserted: TIndexEvent;
    FOnLoaded: TNotifyEvent;
    fOnPutted: TIndexEvent;
    fOnScanRanges: TNotifyEvent;
    nLoading: integer;
  protected
    procedure BeginLoading;
    procedure EndLoading;
    procedure Put(Index: Integer; const S: string); override;
  public
    function Add(const S: string): Integer; override;

    procedure AddStrings(Strings: TStrings); override;
    procedure Assign(Source: TPersistent); override;

    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    property OnAdded: TNotifyEvent read FOnAdded write FOnAdded;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    property OnDeleted: TIndexEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TIndexEvent read FOnInserted write FOnInserted;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnPutted: TIndexEvent read FOnPutted write FOnPutted;
    property OnScanRanges: TNotifyEvent read fOnScanRanges write fOnScanRanges;
  end;

  TmwCustomEdit = class(TCustomControl)
  private
    fBlockBegin: TPoint;
    fBlockEnd: TPoint;
    fCaretX: Integer;
    fCaretY: Integer;
    fCharsInWindow: Integer;
    fCharWidth: Integer;
    fFontDummy: TFont;
    fGutterWidth: Integer;
    {$IFDEF MWE_MBCSSUPPORT}
    fImeCount: Integer;
    fMBCSStepAside: Boolean;
    {$ENDIF}
    fInserting: Boolean;
    fLines: TStrings;
    fLinesInWindow: Integer;
    fLeftChar: Integer;
    fMaxLeftChar: Integer;
    fPaintLock: Integer;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    FScrollBars: TScrollStyle;
    fTextHeight: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    fHighLighter: TmwCustomHighLighter;
    fSelectedColor: TmwSelectedColor;
    {$IFNDEF UNDO_DEBUG}
    fUndoList: TUndoList;
    fRedoList: TUndoList;
    {$ENDIF}
    fBookMarks: array[0..9] of TMark;
    fDragBlockBegin: TPoint;
    fDragBlockEnd: TPoint;
    fMouseDownX: integer;
    fMouseDownY: integer;
    fBookMarkOpt: TmwBookMarkOpt;
    fOnPaint: TPaintEvent;
    fOnChange: TNotifyEvent;
    fSelectionChange: TNotifyEvent;
    fBorderStyle: TBorderStyle;
    fHideSelection: boolean;
    fMouseWheelAccumulator: integer;
    fOverwriteCaret: TCaretType;
    fInsertCaret: TCaretType;
    fCaretOffset: TPoint;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fKeyStrokes: TmwKeyStrokes;
    fModified: Boolean;
    fMarkList: TMarkList;
    fOnPlaceMark: TPlaceMarkEvent;
    fExtraLineSpacing: integer;
    fOnCommandDone: TNotifyEvent;
    fSelectionMode: TSelectionMode;
    fOnPrintStatus: TPrintStatusEvent;
    fWantTabs: boolean;
    fGutter: TmwGutter;
    fClipboardFormats: TmwEditExporters;
    fTabWidth: integer;
    fOnSpecialLineColors: TSpecialLineColorsEvent;
    fTextDrawer: TheTextDrawer;
    fInvalidateRect: TRect;
    fStateFlags: TmwStateFlags;
    fOptions: TmwEditorOptions;
    fInternalImage: TmwInternalImage;
    fStatusChanges: TmwStatusChanges;
    fOnStatusChange: TStatusChangeEvent;
    fLastKey: word;
    fLastShiftState: TShiftState;
    fProcChar: char;
    fOnDropFiles: TDropFilesEvent;
    fTSearch: TmwEditSearch;
    fOnReplaceText: TReplaceTextEvent;
    procedure ComputeCaret(X, Y: Integer);
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCaretX: Integer;
    function GetCaretY: Integer;
    function GetFont: TFont;
    function GetLeftChar: Integer;
    function GetLineCount: Integer;
    function GetLineText: string;
    function GetSelAvail: Boolean;
    function GetText: string;
    function GetTopLine: Integer;
    procedure FontChanged(Sender: TObject);
    function LeftSpaces(const Line: string): Integer;
    procedure LinesChanging(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TPoint);
    procedure SetBlockEnd(Value: TPoint);
    procedure SetWordBlock(Value: TPoint);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure SetGutterWidth(Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TStrings);
    procedure SetLineText(Value: string);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetText(const Value: string);
    procedure SetTopLine(Value: Integer);
    procedure UpdateCaret;
    procedure UpdateScrollBars(Force: boolean);
    function GetSelText: string;
    procedure SetSelText(const Value: string);
    function ScanFrom(Index: integer): integer;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetCanPaste: Boolean;
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetMaxUndo(const Value: Integer);
    function GetMaxUndo: Integer;
    procedure SetHighlighter(const Value: TmwCustomHighLighter);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetHideSelection(const Value: boolean);
    procedure SetInsertMode(const Value: boolean);
    procedure SetInsertCaret(const Value: TCaretType);
    procedure SetOverwriteCaret(const Value: TCaretType);
    function GetCaretXY: TPoint;
    procedure SetKeystrokes(const Value: TmwKeyStrokes);
    procedure SetMaxLeftChar(Value: integer);
    procedure SetExtraLineSpacing(const Value: integer);
    procedure SetSelTextExternal(const Value: string);
    procedure SetSelectionMode(const Value: TSelectionMode);
    procedure SetGutter(const Value: TmwGutter);
    procedure SelectedColorsChanged(Sender: TObject);
    procedure GutterChanged(Sender: TObject);
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure SetWantTabs(const Value: boolean);
    procedure SetTabWidth(Value: integer);
    procedure LockUndo;
    procedure UnLockUndo;
    function IsPointInSelection(Value: TPoint): boolean;
    procedure SetOptions(Value: TmwEditorOptions);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure MoveCaretHorz(DX: integer; SelectionCommand: boolean);
    procedure MoveCaretVert(DY: integer; SelectionCommand: boolean);
    procedure MoveCaretAndSelection(ptBefore, ptAfter: TPoint;
      SelectionCommand: boolean);
    procedure StatusChanged(AChanges: TmwStatusChanges);
    procedure DoTabKey;
  private
    procedure WMDropFiles(var Message: TMessage); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMScroll); message WM_HSCROLL;
    {$IFDEF MWE_MBCSSUPPORT}
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Message: TMessage); message WM_IME_NOTIFY;
    {$ENDIF}
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DecPaintLock;
    procedure HideCaret;
    procedure IncPaintLock;
    procedure ListAdded(Sender: TObject);
    procedure ListCleared(Sender: TObject);
    procedure ListDeleted(Index: Integer);
    procedure ListInserted(Index: Integer);
    procedure ListLoaded(Sender: TObject);
    procedure ListPutted(Index: Integer);
    procedure ListScanRanges(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure PaintGutter(AClip: TRect; FirstLine, LastLine: integer); virtual;
    procedure PaintTextLines(AClip: TRect; FirstLine, LastLine,
      FirstCol, LastCol: integer); virtual;
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
    procedure ShowCaret;
    procedure Loaded; override;
    function CoorToIndex(CPos: TPoint): integer;
    function IndexToCoor(ind: integer): TPoint;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure InitializeCaret;
    procedure MarkListChange(Sender: TObject);
    procedure ProcessCommand(var Command: TmwEditorCommand; var AChar: char;
      Data: pointer); virtual;
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState;
      var Data: pointer): TmwEditorCommand;
    procedure SelectionChange; virtual;
    procedure SetSelTextPrimitive(PasteMode: TSelectionMode; Value: PChar;
      Tag: PInteger);
    {$IFDEF MWE_MBCSSUPPORT}
    procedure MBCSGetSelRangeInLineWhenColumnSelectionMode(const s: string;
      var ColFrom, ColTo: Integer);
    {$ENDIF}
    procedure PrintStatus(Status: TmwPrintStatus; PageNumber: integer;
      var Abort: boolean); virtual;
    // note: FirstLine and LastLine don't need to be in correct order
    procedure InvalidateGutter(FirstLine, LastLine: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure RecalcCharExtent;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure SetCaretXY(Value: TPoint); virtual;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    {$IFDEF UNDO_DEBUG}
    fUndoList: TUndoList;
    fRedoList: TUndoList;
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateLine(Line: integer);
    procedure WndProc(var msg: TMessage); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function CaretXPix: Integer;
    function CaretYPix: Integer;
    procedure DoCopyToClipboard(const SText: string);
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure ClearAll;
    procedure Undo;
    procedure Redo;
    procedure ClearBookMark(BookMark: Integer);
    procedure GotoBookMark(BookMark: Integer);
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    function GetBookMark(BookMark: integer; var X, Y: integer): boolean;
    function IsBookmark(BookMark: integer): boolean;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure EnsureCursorPosVisible;
    procedure CommandProcessor(Command: TmwEditorCommand; AChar: char;
      Data: pointer); virtual;
    function NextWordPos: TPoint; virtual;
    function PrevWordPos: TPoint; virtual;
    procedure SetDefaultKeystrokes; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetSelEnd: integer;
    function GetSelStart: integer;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelWord;
    procedure ClearUndo;
    procedure RefreshAllTokens;
    // Pass NIL in PrintFont to use editor's current font.
    procedure Print(PrintFont: TFont; Options: TmwPrintOptions);
    procedure SaveStreamToClipboardFormat(const ClipboardFormat: Word;
      Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure ExportToFile(const FileName, ATitle: string;
      WithBackground: boolean; Format: TmwEditExporter);
    procedure ExportToClipboard(Format: TmwEditExporter);
    procedure CopyToClipboardEx;
    procedure SetOptionFlag(Flag: TmwEditorOption; Value: boolean);
    procedure ReadState(Reader: TReader); override;
    procedure AddKey(Command: TmwEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word; SS2: TShiftState);
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TmwSearchOptions): integer;
  public
    property BlockBegin: TPoint read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TPoint read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretX: Integer read GetCaretX write SetCaretX;
    property CaretY: Integer read GetCaretY write SetCaretY;
    property CaretXY: TPoint read GetCaretXY write SetCaretXY;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: integer read fCharWidth;
    property ClipboardFormats: TmwEditExporters read FClipboardFormats
      write FClipboardFormats;
    property LeftChar: Integer read GetLeftChar write SetLeftChar;
    property LineHeight: integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow;
    property LineText: string read GetLineText write SetLineText;
    property Marks: TMarkList read fMarkList;
    property MaxLeftChar: integer read fMaxLeftChar write SetMaxLeftChar;
    property Modified: Boolean read fModified write fModified;
    property PaintLock: Integer read fPaintLock;
    property SelAvail: Boolean read GetSelAvail;
    property SelText: string read GetSelText write SetSelTextExternal;
    property Text: string read GetText write SetText;
    property TopLine: Integer read GetTopLine write SetTopLine;
  published
    property Align;
    {$IFDEF MWE_COMPILER_4_UP}
    property Anchors;
    property Constraints;
    {$ENDIF}
    property Color;
    property Ctl3D;
    property Enabled;
    property Height;
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property Width;
    property BookMarkOptions: TmwBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property ExtraLineSpacing: integer
      read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Font: TFont read GetFont write SetFont;
    property Gutter: TmwGutter read fGutter write SetGutter;
    property HideSelection: boolean read fHideSelection write SetHideSelection
      default false;
    property HighLighter: TmwCustomHighLighter
      read fHighLighter write SetHighlighter;
    property InsertCaret: TCaretType read FInsertCaret write SetInsertCaret
      default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property Keystrokes: TmwKeyStrokes
      read FKeystrokes write SetKeystrokes;
    property LineCount: Integer read GetLineCount;
    property Lines: TStrings read fLines write SetLines;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo;
    property Options: TmwEditorOptions read fOptions write SetOptions
      default MWEDIT_DEFAULT_OPTIONS;
    property OverwriteCaret: TCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctBlock;
    property ReadOnly: Boolean read fReadOnly write fReadOnly;
    property RightEdge: Integer read fRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read fRightEdgeColor write SetRightEdgeColor default clSilver;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property SelectedColor: TmwSelectedColor
      read FSelectedColor write FSelectedColor;
    property SelectionMode: TSelectionMode
      read FSelectionMode write SetSelectionMode default smNormal;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantTabs: boolean read fWantTabs write SetWantTabs;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF MWE_COMPILER_4_UP}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF MWE_COMPILER_4_UP}
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCommandDone: TNotifyEvent
      read fOnCommandDone write fOnCommandDone;
    property OnDropFiles: TDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnPrintStatus: TPrintStatusEvent
      read FOnPrintStatus write FOnPrintStatus;
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    property OnSelectionChange: TNotifyEvent
      read fSelectionChange write fSelectionChange;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
  end;

procedure Register;

implementation

{$R mwCustomEdit.RES}

uses mwExport, mwHTMLExport, mwRTFExport;

procedure Register;
begin
  RegisterComponents(MWS_ComponentsPage, [TmwCustomEdit]);
end;

function maxPoint(P1, P2: TPoint): TPoint;
begin
  Result := P1;
  if (P2.y > P1.y) or ((P2.y = P1.y) and (P2.x > P1.x)) then Result := P2;
end;

function minPoint(P1, P2: TPoint): TPoint;
begin
  Result := P1;
  if (P2.y < P1.y) or ((P2.y = P1.y) and (P2.x < P1.x)) then Result := P2;
end;

function Roundoff(X: Extended): Longint;
begin
  if (x >= 0) then begin
    Result := Trunc(x + 0.5)
  end else begin
    Result := Trunc(x - 0.5);
  end;
end;

{ TmwEditList }

function TmwEditList.Add(const S: string): Integer;
begin
  BeginUpdate;
  Result := inherited Add(S);
  if Assigned(FOnAdded) then FOnAdded(Self);
  EndUpdate;
end;

procedure TmwEditList.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      inherited Add(Strings[I]);
    if Assigned(fOnScanRanges) then
      fOnScanRanges(Self);
  finally
    EndUpdate;
  end;
end;

procedure TmwEditList.Assign(Source: TPersistent);
begin
  BeginLoading;
  try
    inherited Assign(Source);
  finally
    EndLoading;
  end;
end;

procedure TmwEditList.BeginLoading;
begin
  Inc(nLoading);
end;

procedure TmwEditList.Clear;
begin
  if Assigned(fOnCleared) then fOnCleared(Self);
  inherited Clear;
  if (nLoading = 0) and (Count = 0) then Add('');
end;

procedure TmwEditList.Delete(Index: Integer);
begin
  BeginUpdate;
  if (Index = 0) and (Count = 1) then
    inherited Put(0, '')
  else
    inherited Delete(Index);
  if Assigned(FOnDeleted) then fOnDeleted(Index);
  EndUpdate;
end;

procedure TmwEditList.EndLoading;
begin
  if (nLoading > 0) then begin
    Dec(nLoading);
    if (nLoading = 0) and (Count = 0) then Add('');
  end;
end;

procedure TmwEditList.Insert(Index: Integer; const S: string);
begin
  BeginUpdate;
  inherited Insert(Index, S);
  if Assigned(FOnInserted) then fOnInserted(Index);
  EndUpdate;
end;

procedure TmwEditList.LoadFromStream(Stream: TStream);
var
  evtOnAdded: TNotifyEvent;
begin
  BeginLoading;
  BeginUpdate;
  try
    if Assigned(fOnScanRanges) then begin
      evtOnAdded := fOnAdded;
      try
        fOnAdded := nil;
        inherited LoadFromStream(Stream);
        fOnScanRanges(Self);
      finally
        fOnAdded := evtOnAdded;
      end;
    end else
      inherited LoadFromStream(Stream);
  finally
    EndLoading;
    EndUpdate;
  end;
end;

procedure TmwEditList.Put(Index: Integer; const S: string);
begin
  BeginUpdate;
  inherited Put(Index, S);
  if Assigned(FOnPutted) then fOnPutted(Index);
  EndUpdate;
end;

procedure TmwEditList.SetTextStr(const Value: string);
begin
  BeginUpdate;
  inherited SetTextStr(Value);
  if Assigned(FOnLoaded) then FOnLoaded(Self);
  EndUpdate;
end;

{ TmwCustomEdit }

procedure TmwCustomEdit.ComputeCaret(X, Y: Integer);
{$IFDEF MWE_MBCSSUPPORT}
var
  pt: TPoint;
  s: string;
  f: Single;
  {$ENDIF}
begin
  {$IFDEF MWE_MBCSSUPPORT}
  f := (X + LeftChar * fCharWidth - fGutterWidth - 2) / fCharWidth;
  pt := Point(Roundoff(f), Y div fTextHeight + TopLine);
  if ((pt.Y - 1) < Lines.Count) then begin
    s := Lines[pt.Y - 1];
    if (Length(s) >= pt.x) and (ByteType(s, pt.X) = mbTrailByte) then
      if Frac(f) >= 0.5 then
        Dec(pt.X)
      else
        Inc(pt.X);
  end;
  fMBCSStepAside := False;
  CaretXY := pt;
  {$ELSE}
  CaretXY := Point(
    Roundoff((X + LeftChar * fCharWidth - FGutterWidth - 2) / fCharWidth),
    Y div fTextHeight + TopLine
    );
  {$ENDIF}
end;

procedure TmwCustomEdit.DoCopyToClipboard(const SText: string);
var
  Mem: HGLOBAL;
  P: PChar;
  SLen: integer;
  Failed: boolean;
begin
  if SText <> '' then begin
    Failed := TRUE; // assume the worst.
    SLen := Length(SText);
    // Open and Close are the only TClipboard methods we use because TClipboard
    // is very hard (impossible) to work with if you want to put more than one
    // format on it at a time.
    Clipboard.Open;
    try
      // Clear anything already on the clipboard.
      EmptyClipboard;
      // Put it on the clipboard as normal text format so it can be pasted into
      // things like notepad or Delphi.
      Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen + 1);
      if Mem <> 0 then begin
        P := GlobalLock(Mem);
        try
          if P <> nil then begin
            Move(PChar(SText)^, P^, SLen + 1);
            // Put it on the clipboard in text format
            Failed := SetClipboardData(CF_TEXT, Mem) = 0;
          end;
        finally
          GlobalUnlock(Mem);
        end;
      end;
      // Don't free Mem!  It belongs to the clipboard now, and it will free it
      // when it is done with it.
      if not Failed then begin
        // Copy it in our custom format so we know what kind of block it is.
        // That effects how it is pasted in.
        Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen +
          SizeOf(TSelectionMode) + 1);
        P := GlobalLock(Mem);
        try
          if P <> nil then begin
            // Our format:  TSelectionMode value followed by text.
            PSelectionMode(P)^ := SelectionMode;
            inc(P, SizeOf(TSelectionMode));
            Move(PChar(SText)^, P^, SLen + 1);
            Failed := SetClipboardData(mwEditClipboardFormat, Mem) = 0;
          end;
        finally
          GlobalUnlock(Mem);
        end;
        // Don't free Mem!  It belongs to the clipboard now, and it will free it
        // when it is done with it.
      end;
    finally
      Clipboard.Close;
      if Failed then
        raise EmwEditError.Create('Clipboard copy operation failed');
    end;
  end;
end;

procedure TmwCustomEdit.CopyToClipboard;
var
  SText: string;
begin
  if SelAvail then begin
    SText := SelText;
    DoCopyToClipboard(SText);
  end;
end;

procedure TmwCustomEdit.CutToClipboard;
var
  SText: string;
begin
  if SelAvail then begin
    SText := SelText;
    DoCopyToClipboard(SText);
    FUndoList.AddChange(mwcrDelete, fBlockBegin, fBlockEnd, PChar(SText),
      SelectionMode);
    LockUndo;
    SelText := '';
    UnLockUndo;
  end;
end;

constructor TmwCustomEdit.Create(AOwner: TComponent);
begin
  fLines := TmwEditList.Create;
  fFontDummy := TFont.Create;
  fUndoList := TUndoList.Create(self);
  fRedoList := TUndoList.Create(self);
  inherited Create(AOwner);
  {$IFDEF MWE_COMPILER_4_UP}
  DoubleBuffered := false;
  {$ENDIF}
  if not (csDesigning in ComponentState) then fLines.Add('');
  TmwEditList(fLines).OnAdded := ListAdded;
  TmwEditList(fLines).OnCleared := ListCleared;
  TmwEditList(fLines).OnDeleted := ListDeleted;
  TmwEditList(fLines).OnInserted := ListInserted;
  TmwEditList(fLines).OnLoaded := ListLoaded;
  TmwEditList(fLines).OnPutted := ListPutted;
  TmwEditList(fLines).OnScanRanges := ListScanRanges;
  fSelectedColor := TmwSelectedColor.Create;
  fSelectedColor.OnChange := SelectedColorsChanged;
  fBookMarkOpt := TmwBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := BookMarkOptionsChanged;
// fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TmwGutter.Create;
  fGutter.OnChange := GutterChanged;
  fGutterWidth := fGutter.Width;
  fTextOffset := fGutterWidth + 2;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Color := clWindow;
  fFontDummy.Name := 'Courier New';
  fFontDummy.Size := 10;
  {$IFDEF MWE_COMPILER_3_UP}
  fFontDummy.CharSet := DEFAULT_CHARSET;
  {$ENDIF}
  fTextDrawer := TheTextDrawer.Create([fsBold], fFontDummy);
  Font.Assign(fFontDummy);
  Font.OnChange := FontChanged;
  FontChanged(nil);
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  TStringList(Lines).OnChanging := LinesChanging;
  TStringList(Lines).OnChange := LinesChanged;
  fInserting := True;
  fMaxLeftChar := 1024;
  fScrollBars := ssBoth;
  fBorderStyle := bsSingle;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FSelectionMode := smNormal;
  fKeystrokes := TmwKeyStrokes.Create(Self);
  fMarkList := TMarkList.Create(self);
  fMarkList.OnChange := MarkListChange;
  SetDefaultKeystrokes;
  fRightEdgeColor := clSilver;
  {$IFDEF MWE_MBCSSUPPORT}
  fImeCount := 0;
  fMBCSStepAside := False;
  {$ENDIF}
  fWantTabs := False;
  fTabWidth := 8;
  // find / replace
  fTSearch := TmwEditSearch.Create;
  fOptions := MWEDIT_DEFAULT_OPTIONS;
end;

procedure TmwCustomEdit.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := Style or ScrollBar[FScrollBars] or BorderStyles[fBorderStyle]
      or WS_CLIPCHILDREN;
    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TmwCustomEdit.DecPaintLock;
begin
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then begin
    if mwsfScrollbarChanged in fStateFlags then UpdateScrollbars(FALSE);
    if mwsfCaretChanged in fStateFlags then UpdateCaret;
    if fStatusChanges <> [] then StatusChanged([]);
  end;
end;

destructor TmwCustomEdit.Destroy;
begin
  Highlighter := nil;
  fKeyStrokes.Free;
  fFontDummy.Free;
  Lines.Free;
  fSelectedColor.Free;
  fBookMarkOpt.Free;
  fMarkList.Free;
  fUndoList.Free;
  fRedoList.Free;
  fGutter.Free;
  fTextDrawer.Free;
  fInternalImage.Free;
  fTSearch.Free;
  inherited Destroy;
end;

function TmwCustomEdit.GetBlockBegin: TPoint;
begin
  Result := fBlockBegin;
  if fBlockEnd.Y < fBlockBegin.Y then Result := fBlockEnd else
    if fBlockEnd.Y = fBlockBegin.Y then
      if fBlockEnd.X < fBlockBegin.X then Result := fBlockEnd;
end;

function TmwCustomEdit.GetBlockEnd: TPoint;
begin
  Result := fBlockEnd;
  if fBlockEnd.Y < fBlockBegin.Y then Result := fBlockBegin else
    if fBlockEnd.Y = fBlockBegin.Y then
      if fBlockEnd.X < fBlockBegin.X then Result := fBlockBegin;
end;

function TmwCustomEdit.GetCaretX: Integer;
begin
  Result := fCaretX + 1;
end;

function TmwCustomEdit.CaretXPix: Integer;
begin
  Result := fCaretX * fCharWidth + fTextOffset;
end;

function TmwCustomEdit.GetCaretY: Integer;
begin
  Result := fCaretY + 1;
end;

function TmwCustomEdit.CaretYPix: Integer;
begin
  Result := fCaretY * fTextHeight - fTopLine * fTextHeight + 1;
end;

procedure TmwCustomEdit.FontChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);
end;

function TmwCustomEdit.GetFont: TFont;
begin
  Result := inherited Font;
end;

function TmwCustomEdit.GetLineCount: Integer;
begin
  Result := Lines.Count;
end;

function TmwCustomEdit.GetLeftChar: Integer;
begin
  Result := fLeftChar + 1;
end;

function TmwCustomEdit.GetLineText: string;
begin
  Result := '';
  if (Lines.Count > 0) and (fCaretY < Lines.Count) then
    Result := Lines[fCaretY];
end;

function TmwCustomEdit.GetSelAvail: Boolean;
begin
  Result := (fBlockBegin.X <> fBlockEnd.X) or
    ((fBlockBegin.Y <> fBlockEnd.Y) and (fSelectionMode <> smColumn));
end;

function TmwCustomEdit.GetSelText: string;

  function CopyPadded(const S: string; Index, Count: integer): string;
  var
    SrcLen: Integer;
    DstLen: integer;
    P: PChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PChar(Result);
      StrPCopy(P, Copy(S, Index, Count));
      Inc(P, Length(S));
      FillChar(P^, DstLen - Srclen, $20);
    end;
  end;

  procedure CopyAndForward(const S: string; Index, Count: Integer; var P:
    PChar);
  var
    pSrc: PChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then begin
      Dec(Index);
      pSrc := PChar(S) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen);
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

  procedure CopyPaddedAndForward(const S: string; Index, Count: Integer;
    var P: PChar);
  var
    OldP: PChar;
    Len: Integer;
  begin
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);
    FillChar(P^, Len, #$20);
    Inc(P, Len);
  end;

const
  sLineBreak = #$0D#$0A;
var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
  {$IFDEF MWE_MBCSSUPPORT}
  l, r: Integer;
  s: string;
  {$ELSE}
  ColLen: integer;
  {$ENDIF}
  P: PChar;
begin
  if not SelAvail then
    Result := ''
  else begin
    with BlockBegin do begin
      ColFrom := X;
      First := Y - 1;
    end;
    with BlockEnd do begin
      ColTo := X;
      Last := Y - 1;
    end;
    TotalLen := 0;
    case SelectionMode of
      smNormal:
        if (First = Last) then
          Result := Copy(Lines[First], ColFrom, ColTo - ColFrom)
        else begin
          // step1: calclate total length of result string
          TotalLen := Max(0, Length(Lines[First]) - ColFrom + 1);
          for i := First + 1 to Last - 1 do
            Inc(TotalLen, Length(Lines[i]));
          Inc(TotalLen, ColTo - 1);
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          CopyAndForward(Lines[First], ColFrom, MaxInt, P);
          CopyAndForward(sLineBreak, 1, MaxInt, P);
          for i := First + 1 to Last - 1 do begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, ColTo - 1, P);
        end;
      smColumn:
        begin
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);
          // step1: calclate total length of result string
          {$IFNDEF MWE_MBCSSUPPORT}
          ColLen := ColTo - ColFrom;
          TotalLen := ColLen + (ColLen + Length(sLineBreak)) * (Last - First);
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            CopyPaddedAndForward(Lines[i], ColFrom, ColLen, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyPaddedAndForward(Lines[Last], ColFrom, ColLen, P);
          {$ELSE} //MWE_MBCSSUPPORT
          for i := First to Last do begin
            s := Lines[i];
            l := ColFrom;
            r := ColTo;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
            Inc(TotalLen, r - l);
          end;
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            s := Lines[i];
            l := ColFrom;
            r := ColTo;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
            CopyPaddedAndForward(s, l, r - l, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          s := Lines[Last];
          l := ColFrom;
          r := ColTo;
          MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
          CopyPaddedAndForward(Lines[Last], l, r - l, P);
          {$ENDIF}
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calclate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(Lines[i]) + Length(sLineBreak));
          if Last = Lines.Count then
            Dec(TotalLen, Length(sLineBreak));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, MaxInt, P);
          if (Last + 1) < Lines.Count then
            CopyAndForward(sLineBreak, 1, MaxInt, P);
        end;
    end;
  end;
end;

function TmwCustomEdit.GetText: string;
begin
  Result := Lines.Text;
end;

function TmwCustomEdit.GetTopLine: Integer;
begin
  Result := fTopLine + 1;
end;

procedure TmwCustomEdit.HideCaret;
begin
  if mwsfCaretVisible in fStateFlags then
    if Windows.HideCaret(Handle) then Exclude(fStateFlags, mwsfCaretVisible);
end;

{$IFDEF MWE_MBCSSUPPORT}

procedure TmwCustomEdit.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  p: PChar;
begin
  if ((Msg.LParam and GCS_RESULTSTR) <> 0) then begin
    imc := ImmGetContext(Handle);
    try
      fImeCount := ImmGetCompositionString(imc, GCS_RESULTSTR, nil, 0);
      GetMem(p, fImeCount + 1);
      try
        ImmGetCompositionString(imc, GCS_RESULTSTR, p, fImeCount + 1);
        p[fImeCount] := #0;
        CommandProcessor(ecImeStr, #0, p);
      finally
        FreeMem(p, fImeCount + 1);
      end;
    finally
      ImmReleaseContext(Handle, imc);
    end;
  end;
  inherited;
end;

procedure TmwCustomEdit.WMImeNotify(var Message: TMessage);
var
  imc: HIMC;
  logFont: TLogFont;
begin
  with Message do begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          imc := ImmGetContext(Handle);
          if (imc <> 0) then begin
            GetObject(Font.Handle, SizeOf(TLogFont), @logFont);
            ImmSetCompositionFont(imc, @logFont);

            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;
{$ENDIF}

procedure TmwCustomEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TmwCustomEdit.InvalidateGutter(FirstLine, LastLine: integer);
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := Rect(0, 0, fGutterWidth, ClientHeight);
      if mwsfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(Handle, @rcInval, FALSE);
    end else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        rcInval := Rect(0, fTextHeight * (FirstLine - TopLine),
          fGutterWidth, fTextHeight * (LastLine - TopLine + 1));
        if mwsfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(Handle, @rcInval, FALSE);
      end;
    end;
end;

procedure TmwCustomEdit.InvalidateLines(FirstLine, LastLine: integer);
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := ClientRect;
      rcInval.Left := fGutterWidth;
      if mwsfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(Handle, @rcInval, FALSE);
    end else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        rcInval := Rect(fGutterWidth, fTextHeight * (FirstLine - TopLine),
          ClientWidth, fTextHeight * (LastLine - TopLine + 1));
        if mwsfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(Handle, @rcInval, FALSE);
      end;
    end;
end;

procedure TmwCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: char;
  Cmd: TmwEditorCommand;
begin
  inherited;
//  OnKeyDown is already called in ancestor
  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then begin
      Key := 0; // eat it.
      CommandProcessor(Cmd, C, Data);
    end;
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
end;

procedure TmwCustomEdit.Loaded;
begin
  inherited Loaded;
  GutterChanged(Self);
end;

procedure TmwCustomEdit.KeyPress(var Key: Char);
begin
  {$IFDEF MWE_MBCSSUPPORT}
  if (fImeCount > 0) then begin
    Dec(fImeCount);
    Exit;
  end;
  {$ENDIF}
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);

  if fProcChar = Key then
    fProcChar := #0
  else
    CommandProcessor(ecChar, Key, nil);
end;

function TmwCustomEdit.LeftSpaces(const Line: string): Integer;
var
  p: PChar;
begin
  p := pointer(Line);
  if Assigned(p) and (mweoAutoIndent in fOptions) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      Inc(p);
      Inc(Result);
    end;
  end else
    Result := 0;
end;

procedure TmwCustomEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, mwsfLinesChanging);
end;

procedure TmwCustomEdit.LinesChanged(Sender: TObject);
begin
  Exclude(fStateFlags, mwsfLinesChanging);
  if HandleAllocated then begin
    UpdateScrollBars(FALSE);
    SetBlockBegin(CaretXY);
    InvalidateRect(Handle, @fInvalidateRect, False);
    FillChar(fInvalidateRect, SizeOf(TRect), 0);
  end;
end;

procedure TmwCustomEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  bWasSel: boolean;
  bStartDrag: boolean;
begin
  if (Button = mbRight) and (Shift = [ssRight]) and
    Assigned(PopupMenu) and SelAvail
    then
    exit;
  bWasSel := false;
  bStartDrag := FALSE;
  if Button = mbLeft then begin
    if ssDouble in Shift then Exit;
    if SelAvail then begin
        //remember selection state, as it will be cleared later
      bWasSel := true;
      fDragBlockBegin := BlockBegin;
      fDragBlockEnd := BlockEnd;
      fMouseDownX := X;
      fMouseDownY := Y;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
  ComputeCaret(X, Y);
  if Button = mbLeft then begin
    MouseCapture := True;
      //if mousedown occured in selected block then begin drag operation
    Exclude(fStateFlags, mwsfWaitForDragging);
    if bWasSel and (mweoDragDropEditing in fOptions) and
      IsPointInSelection(CaretXY)
      then
      bStartDrag := TRUE;
  end;
  if (Button = mbLeft) and bStartDrag then
    Include(fStateFlags, mwsfWaitForDragging)
  else begin
    if not (mwsfDblClicked in fStateFlags) then begin
      if ssShift in Shift then
        SetBlockEnd(CaretXY)
      else
        SetBlockBegin(CaretXY);
    end;
  end;
  Windows.SetFocus(Handle);
end;

procedure TmwCustomEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  bDoSleep: boolean;
  nDelta: integer;

  function IsInEditRect(bGetCoor: boolean): boolean;
  var
    ptMouse: TPoint;
  begin
    if bGetCoor then begin
      GetCursorPos(ptMouse);
      ptMouse := ScreenToClient(ptMouse);
      X := ptMouse.X;
      Y := ptMouse.Y;
    end;
    Result := (X >= fGutterWidth) and (X < ClientWidth) and
      (Y >= 0) and (Y < ClientHeight);
  end;

begin
  inherited MouseMove(Shift, x, y);
  if MouseCapture and (mwsfWaitForDragging in fStateFlags) and not ReadOnly then
    begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG)) or
      (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG)) then begin
      Exclude(fStateFlags, mwsfWaitForDragging);
      BeginDrag(false);
    end;
  end
  else begin
    if not (mwsfInScrollLoop in fStateFlags) and
      (ssLeft in Shift) and MouseCapture
      then begin
      Include(fStateFlags, mwsfInScrollLoop);
      try
        if IsInEditRect(FALSE) then ComputeCaret(X, Y);
        SetBlockEnd(CaretXY);
        // InScrollLoop is to prevent reentrancy of this code block here
        Application.ProcessMessages;
        // we have to constantly update the mouse position!
        while MouseCapture and not IsInEditRect(TRUE) do begin
          bDoSleep := TRUE;
          // changes to line / column in one go
          IncPaintLock;
          try
            // horizontal scrolling
            if X < fGutterWidth then begin
              nDelta := fGutterWidth - X;
              if nDelta < 10 then LeftChar := LeftChar - 1
              else LeftChar := LeftChar - 8;
              if nDelta >= 42 then bDoSleep := FALSE;
              CaretX := LeftChar;
              SetBlockEnd(CaretXY);
            end else if X > ClientWidth then begin
              nDelta := X - ClientWidth;
              if nDelta < 10 then LeftChar := LeftChar + 1
              else LeftChar := LeftChar + 8;
              if nDelta >= 42 then bDoSleep := FALSE;
              CaretX := LeftChar + CharsInWindow;
              SetBlockEnd(CaretXY);
            end;
            // vertical scrolling
            if Y < 0 then begin
              if Y <= -10 then TopLine := TopLine - LinesInWindow
              else TopLine := TopLine - 1;
              if Y <= -42 then bDoSleep := FALSE;
              CaretY := TopLine;
              SetBlockEnd(CaretXY);
            end else if Y > ClientHeight then begin
              nDelta := Y - ClientHeight;
              if nDelta >= 10 then TopLine := TopLine + LinesInWindow
              else TopLine := TopLine + 1;
              if nDelta >= 42 then bDoSleep := FALSE;
              CaretY := TopLine + LinesInWindow - 1;
              SetBlockEnd(CaretXY);
            end;
          finally
            DecPaintLock;
            Update;
          end;
          // paint and update cursor and so on...
          Application.ProcessMessages;
          if bDoSleep then Sleep(100);
        end;
      finally
        Exclude(fStateFlags, mwsfInScrollLoop);
      end;
    end;
  end;
end;

procedure TmwCustomEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu)
    then exit;
  MouseCapture := False;
  if fStateFlags * [mwsfDblClicked, mwsfWaitForDragging] = [mwsfWaitForDragging]
  then begin
    ComputeCaret(X, Y);
    SetBlockBegin(CaretXY);
    SetBlockEnd(CaretXY);
    Exclude(fStateFlags, mwsfWaitForDragging);
  end;
  Exclude(fStateFlags, mwsfDblClicked);
end;

// Complete rewrite of the painting code again. Removed Brads code with the
// Windows regions, since it made painting selected text very slow (the lines
// were scanned twice by the assigned highlighter) - on my system it took up to
// threetimes as long to paint the window if everything was selected.
// Splitting the paint into two operations caused weird effects on slower
// systems when scrolling in the column selection mode.
// The new code features the following:
// - No more doublebuffering of the drawing operations. The bitmap member has
//   been removed completely.
// - The highlighters don't touch the canvas of the control any more. Instead
//   the active token attribute is read from the highlighter, and only the
//   necessary changes to the font and the brush are made. This removes a lot
//   of color changes for selected text. Also the font color for spaces is
//   ignored, since no chars are visible anyway.
// - Tokens with the same font and brush attributes (also depending on the
//   selection state) are concatenated and painted in one go, this reduces
//   the number of ExtTextOut calls.
// - New event to assign special font and background colors to a line. Can be
//   used to paint breakpoint or current lines like seen in the Delphi IDE.
// - The font width array for the ExtTextOut calls is dynamically allocated
//   (only the maximum number of visible chars, not fCharsInWindow) and filled
//   with the fCharWidth value. This should save memory... Thanks to James
//   Jacobson for spotting this one.

// A lot of changes have been made by Tohru Hanai before the new version was
// released. Speed is now considerably higher, thanks to the new text painter
// classes and the changes in the client area invalidation code.
// I did however not mark the changes, but removed all the commented out lines
// for the sake of clarity. Please consider the painting code a joint effort of
// Tohru and me. The gains in speed come mostly from Tohrus new code.

procedure TmwCustomEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: integer;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  rcClip := Canvas.ClipRect;
  // columns
  nC1 := LeftChar;
  if (rcClip.Left > fGutterWidth + 2) then
    Inc(nC1, (rcClip.Left - fGutterWidth - 2) div CharWidth);
  nC2 := nC1 +
    (rcClip.Right - fGutterWidth - 2 + CharWidth - 1) div CharWidth;
  // lines
  nL1 := Max(TopLine + rcClip.Top div fTextHeight, TopLine);
  nL2 := Min(TopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight,
    Lines.Count);
  // Now paint everything while the caret is hidden.
  HideCaret;
  try
    // First paint the gutter area if it was (partly) invalidated.
    if (rcClip.Left < fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Right := fGutterWidth;
      PaintGutter(rcDraw, nL1, nL2);
    end;
    // Then paint the text area if it was (partly) invalidated.
    if (rcClip.Right > fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, fGutterWidth);
      PaintTextLines(rcDraw, nL1, nL2, nC1, nC2);
    end;
    // If there is a custom paint handler call it.
    if Assigned(fOnPaint) then begin
      Font.Assign(Font);
      Brush.Color := Color;
      fOnPaint(Self, Canvas);
    end;
  finally
    UpdateCaret;
  end;
end;

procedure TmwCustomEdit.PaintGutter(AClip: TRect; FirstLine, LastLine: integer);
var
  i, iLine: integer;
  rcLine: TRect;
  bHasOtherMarks: boolean;
  aGutterOffs: PIntArray;
  s: string;
  dc: HDC;

  procedure DrawMark(iMark: integer);
  var
    iLine: integer;
  begin
    if Assigned(fBookMarkOpt.BookmarkImages) and not Marks[i].InternalImage then
      begin
      if Marks[iMark].ImageIndex <= fBookMarkOpt.BookmarkImages.Count then begin
        iLine := Marks[iMark].Line - TopLine;
        with fBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs^[iLine],
            iLine * fTextHeight, Marks[iMark].ImageIndex);
        Inc(aGutterOffs^[iLine], fBookMarkOpt.XOffset);
      end;
    end else begin
      if Marks[iMark].ImageIndex in [0..9] then begin
        iLine := Marks[iMark].Line - TopLine;
        if not Assigned(fInternalImage) then
          fInternalImage := TmwInternalImage.Create('mweInternalImages', 10);
        fInternalImage.DrawMark(Canvas, Marks[iMark].ImageIndex,
          fBookMarkOpt.LeftMargin + aGutterOffs^[iLine],
          iLine * fTextHeight, fTextHeight);
        Inc(aGutterOffs^[iLine], fBookMarkOpt.XOffset);
      end;
    end;
  end;

begin
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  Canvas.Brush.Color := Gutter.Color;
  // If we have to draw the line numbers then we don't want to erase
  // the background first. Do it line by line with TextRect instead
  // and fill only the area after the last visible line.
  dc := Canvas.Handle;
  if fGutter.ShowLineNumbers then begin
    fTextDrawer.BeginDrawing(dc);
    try
      fTextDrawer.SetBackColor(fGutter.Color);
      fTextDrawer.SetForeColor(Self.Font.Color);
      fTextDrawer.Style := [];
      // prepare the rect initially
      rcLine := AClip;
      rcLine.Right := Max(rcLine.Right, fGutterWidth - 2);
      rcLine.Bottom := (FirstLine - TopLine) * fTextHeight;
      for iLine := FirstLine to LastLine do begin
        // next line rect
        rcLine.Top := rcLine.Bottom;
        Inc(rcLine.Bottom, fTextHeight);
        // erase the background and draw the line number string in one go
        s := fGutter.FormatLineNumber(iLine);
        Windows.ExtTextOut(DC, fGutter.LeftOffset, rcLine.Top, ETO_OPAQUE,
          @rcLine, PChar(s), Length(s), nil);
      end;
      // now erase the remaining area if any
      if AClip.Bottom > rcLine.Bottom then begin
        rcLine.Top := rcLine.Bottom;
        rcLine.Bottom := AClip.Bottom;
        with rcLine do
          fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, rcLine, nil, 0);
      end;
    finally
      fTextDrawer.EndDrawing;
    end;
  end else
    InternalFillRect(dc, AClip);
  // the gutter separator if visible
  if AClip.Right >= fGutterWidth - 2 then
    with Canvas do begin
      Pen.Color := clBtnHighlight;
      Pen.Width := 1;
      with AClip do begin
        MoveTo(fGutterWidth - 2, Top);
        LineTo(fGutterWidth - 2, Bottom);
        Pen.Color := clBtnShadow;
        MoveTo(fGutterWidth - 1, Top);
        LineTo(fGutterWidth - 1, Bottom);
      end;
    end;
  // now the gutter marks
  if BookMarkOptions.GlyphsVisible and (Marks.Count > 0) then begin
    aGutterOffs := AllocMem((fLinesInWindow + 1) * SizeOf(integer));
    try
      // Instead of making a two pass loop we look while drawing the bookmarks
      // whether there is any other mark to be drawn
      bHasOtherMarks := FALSE;
      for i := 0 to Marks.Count - 1 do begin
        if not Marks[i].Visible or (Marks[i].Line < FirstLine) or
          (Marks[i].Line > LastLine)
          then continue;
        if Marks[i].IsBookmark then bHasOtherMarks := TRUE
        else DrawMark(i);
      end;
      if bHasOtherMarks then for i := 0 to Marks.Count - 1 do
          with Marks[i] do
            if Visible and IsBookmark and
              (Line >= FirstLine) and (Line <= LastLine)
              then DrawMark(i);
    finally
      FreeMem(aGutterOffs);
    end;
  end;

end;

procedure TmwCustomEdit.PaintTextLines(AClip: TRect; FirstLine, LastLine,
  FirstCol, LastCol: integer);
var
  bDoRightEdge: boolean; // right edge
  nRightEdge: integer;
    // selection info
  bAnySelection: boolean; // any selection visible?
  nSelL1, nSelCol1: integer; // start of selected area
  nSelL2, nSelCol2: integer; // end of selected area
    // info about normal and selected text and background colors
  bSpecialLine, bLineSelected: boolean;
  colFG, colBG: TColor;
  colSelFG, colSelBG: TColor;
    // info about selction of the current line
  nSelStart, nSelEnd: integer;
  bComplexLine: boolean;
    // painting the background and the text
  rcLine, rcToken: TRect;
  TokenAccu: record
    // Note: s is not managed as a string, it will only grow!!!
    // Never use AppendStr or "+", use Len and MaxLen instead and
    // copy the string chars directly. This is for efficiency.
    Len, MaxLen, CharsBefore: integer;
    s: string;
    FG, BG: TColor;
    Style: TFontStyles;
  end;
  dc: HDC;

{ local procedures }

  procedure ComputeSelectionInfo;
  begin
    bAnySelection := FALSE;
    // Only if selection is visible anyway.
    if (not HideSelection or Self.Focused) then begin
      bAnySelection := TRUE;
      // Get the *real* start of the selected area.
      if (fBlockBegin.Y < fBlockEnd.Y) then begin
        nSelL1 := fBlockBegin.Y;
        nSelCol1 := fBlockBegin.X;
        nSelL2 := fBlockEnd.Y;
        nSelCol2 := fBlockEnd.X;
      end else if (fBlockBegin.Y > fBlockEnd.Y) then begin
        nSelL2 := fBlockBegin.Y;
        nSelCol2 := fBlockBegin.X;
        nSelL1 := fBlockEnd.Y;
        nSelCol1 := fBlockEnd.X;
      end else if (fBlockBegin.X <> fBlockEnd.X) then begin
        // No selection at all, or it is only on this line.
        nSelL1 := fBlockBegin.Y;
        nSelL2 := nSelL1;
        if (fBlockBegin.X < fBlockEnd.X) then begin
          nSelCol1 := fBlockBegin.X;
          nSelCol2 := fBlockEnd.X;
        end else begin
          nSelCol2 := fBlockBegin.X;
          nSelCol1 := fBlockEnd.X;
        end;
      end else
        bAnySelection := FALSE;
      // If there is any visible selection so far, then test if there is an
      // intersection with the area to be painted.
      if bAnySelection then begin
      // Don't care if the selection is not visible.
        bAnySelection := (nSelL2 >= FirstLine) and (nSelL1 <= LastLine);
      // In the column selection mode sort the begin and end of the selection,
      // this makes the painting code simpler.
        if (SelectionMode = smColumn) and (nSelCol1 > nSelCol2) then
          SwapInt(nSelCol1, nSelCol2);
      end;
    end;
  end;

  procedure SetDrawingColors(Selected: boolean);
  begin
    with fTextDrawer do
      if Selected then begin
        SetBackColor(colSelBG);
        SetForeColor(colSelFG);
      end else begin
        SetBackColor(colBG);
        SetForeColor(colFG);
      end;
  end;

  function ColumnToXValue(Col: integer): integer;
  begin
    Result := fTextOffset + Pred(Col) * fCharWidth;
  end;

  // CharsBefore tells if Token starts at column one or not

  procedure PaintToken(const Token: string;
    TokenLen, CharsBefore, First, Last: integer);
  var
    pszText: PChar;
    nX, nCharsToPaint: integer;
  const
    ETOOptions = ETO_CLIPPED or ETO_OPAQUE;
  begin
    if (Last >= First) and (rcToken.Right > rcToken.Left) then begin
      nX := ColumnToXValue(First);
      Dec(First, CharsBefore);
      Dec(Last, CharsBefore);
      if (First > TokenLen) then begin
        pszText := nil;
        nCharsToPaint := 0;
      end else begin
        pszText := PChar(@Token[First]);
        nCharsToPaint := Min(Last - First + 1, TokenLen - First + 1);
      end;
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, rcToken,
        pszText, nCharsToPaint);
      rcToken.Left := rcToken.Right;
    end;
  end;

  procedure PaintHighlightToken(bFillToEOL: boolean);
  var
    bComplexToken: boolean;
    nC1, nC2, nC1Sel, nC2Sel: integer;
    bU1, bSel, bU2: boolean;
    nX1, nX2: integer;
  begin
    // Compute some helper variables.
    nC1 := Max(FirstCol, TokenAccu.CharsBefore + 1);
    nC2 := Min(LastCol, TokenAccu.CharsBefore + TokenAccu.Len + 1);
    if bComplexLine then begin
      bU1 := (nC1 < nSelStart);
      bSel := (nC1 < nSelEnd) and (nC2 >= nSelStart);
      bU2 := (nC2 >= nSelEnd);
      bComplexToken := bSel and (bU1 or bU2);
    end else begin
      bU1 := FALSE; // to shut up Compiler warning Delphi 2
      bSel := bLineSelected;
      bU2 := FALSE; // to shut up Compiler warning Delphi 2
      bComplexToken := FALSE;
    end;
    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then begin
      // Initialize the colors and the font style.
      if not bSpecialLine then begin
        colBG := TokenAccu.BG;
        colFG := TokenAccu.FG;
      end;
      fTextDrawer.SetStyle(TokenAccu.Style);
      // Paint the chars
      if bComplexToken then begin
        // first unselected part of the token
        if bU1 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ColumnToXValue(nSelStart);
          with TokenAccu do PaintToken(s, Len, CharsBefore, nC1, nSelStart);
        end;
        // selected part of the token
        SetDrawingColors(TRUE);
        nC1Sel := Max(nSelStart, nC1);
        nC2Sel := Min(nSelEnd, nC2);
        rcToken.Right := ColumnToXValue(nC2Sel);
        with TokenAccu do PaintToken(s, Len, CharsBefore, nC1Sel, nC2Sel);
        // second unselected part of the token
        if bU2 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ColumnToXValue(nC2);
          with TokenAccu do PaintToken(s, Len, CharsBefore, nSelEnd, nC2);
        end;
      end else begin
        SetDrawingColors(bSel);
        rcToken.Right := ColumnToXValue(nC2);
        with TokenAccu do PaintToken(s, Len, CharsBefore, nC1, nC2);
      end;
    end;
    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then begin
      if not bSpecialLine then colBG := Color;
      if bComplexLine then begin
        nX1 := ColumnToXValue(nSelStart);
        nX2 := ColumnToXValue(nSelEnd);
        if (rcToken.Left < nX1) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := nX1;
          InternalFillRect(dc, rcToken);
          rcToken.Left := nX1;
        end;
        if (rcToken.Left < nX2) then begin
          SetDrawingColors(TRUE);
          rcToken.Right := nX2;
          InternalFillRect(dc, rcToken);
          rcToken.Left := nX2;
        end;
        if (rcToken.Left < rcLine.Right) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := rcLine.Right;
          InternalFillRect(dc, rcToken);
        end;
      end else begin
        SetDrawingColors(bLineSelected);
        rcToken.Right := rcLine.Right;
        InternalFillRect(dc, rcToken);
      end;
    end;
  end;

  procedure AddHighlightToken(const Token: AnsiString;
    CharsBefore, TokenLen: integer;
    Foreground, Background: TColor;
    Style: TFontStyles);
  var
    bCanAppend: boolean;
    bSpacesTest, bIsSpaces: boolean;
    i: integer;

    function TokenIsSpaces: boolean;
    var
      pTok: PChar;
    begin
      if not bSpacesTest then begin
        bSpacesTest := TRUE;
        pTok := PChar(Token);
        while (pTok^ <> #0) do begin
          if (pTok^ <> ' ') then break;
          Inc(pTok);
        end;
        bIsSpaces := (pTok^ = #0);
      end;
      Result := bIsSpaces;
    end;

  begin
    if Background = clNone then Background := Color;
    if Foreground = clNone then Foreground := Font.Color;
    // Do we have to paint the old chars first, or can we just append?
    bCanAppend := FALSE;
    bSpacesTest := FALSE;
    if (TokenAccu.Len > 0) then begin
      // font style must be the same or token is only spaces
      if (TokenAccu.Style = Style) or TokenIsSpaces then
      // either special colors or same colors
        if bSpecialLine or bLineSelected or
        // background color must be the same and
        ((TokenAccu.BG = Background) and
          // foreground color must be the same or token is only spaces
          ((TokenAccu.FG = Foreground) or TokenIsSpaces))
          then bCanAppend := TRUE;
      // If we can't append it, then we have to paint the old token chars first.
      if not bCanAppend then PaintHighlightToken(FALSE);
    end;
    // Don't use AppendStr because it's more expensive.
    if bCanAppend then begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do TokenAccu.s[TokenAccu.Len + i] := Token[i];
      Inc(TokenAccu.Len, TokenLen);
    end else begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do TokenAccu.s[i] := Token[i];
      TokenAccu.CharsBefore := CharsBefore;
      TokenAccu.FG := Foreground;
      TokenAccu.BG := Background;
      TokenAccu.Style := Style;
    end;
  end;

  procedure PaintLines;
  var
    nLine: integer; // line index for the loop
    sLine: string; // the current line (expanded)
    pConvert: TConvertTabsProc;
    sToken: string; // highlighter token info
    nTokenPos, nTokenLen: integer;
    attr: TmwHighLightAttributes;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := (FirstLine - TopLine) * fTextHeight;
    // Make sure the token accumulator string doesn't get reassigned to often.
    if Assigned(fHighlighter) then begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow);
      SetLength(TokenAccu.s, TokenAccu.MaxLen);
    end;
    // Find the fastest function for the tab expansion.
    pConvert := GetBestConvertTabsProc(fTabWidth);
    // Now loop through all the lines. The indices are valid for Lines.
    for nLine := FirstLine to LastLine do begin
      // Get the expanded line.
      sLine := pConvert(Lines[nLine - 1], fTabWidth);
      // Get the information about the line selection. Three different parts
      // are possible (unselected before, selected, unselected after), only
      // unselected or only selected means bComplexLine will be FALSE. Start
      // with no selection, compute based on the visible columns.
      bComplexLine := FALSE;
      nSelStart := 0;
      nSelEnd := 0;
      // Does the selection intersect the visible area?
      if bAnySelection and (nLine >= nSelL1) and (nLine <= nSelL2) then begin
        // Default to a fully selected line. This is correct for the smLine
        // selection mode and a good start for the smNormal mode.
        nSelStart := FirstCol;
        nSelEnd := LastCol + 1;
        if (SelectionMode = smColumn) or
          ((SelectionMode = smNormal) and (nLine = nSelL1))
          then
          if (nSelCol1 > LastCol) then begin
            nSelStart := 0;
            nSelEnd := 0;
          end else if (nSelCol1 > FirstCol) then begin
            nSelStart := nSelCol1;
            bComplexLine := TRUE;
          end;
        if (SelectionMode = smColumn) or
          ((SelectionMode = smNormal) and (nLine = nSelL2))
          then
          if (nSelCol2 < FirstCol) then begin
            nSelStart := 0;
            nSelEnd := 0;
          end else if (nSelCol2 < LastCol) then begin
            nSelEnd := nSelCol2;
            bComplexLine := TRUE;
          end;
        {$IFDEF MWE_MBCSSUPPORT}
        if (SelectionMode = smColumn) then
          MBCSGetSelRangeInLineWhenColumnSelectionMode(sLine, nSelStart,
            nSelEnd);
        {$ENDIF}
      end;
      // Update the rcLine rect to this line.
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, fTextHeight);
      // Initialize the text and background colors, maybe the line should
      // use special values for them.
      bSpecialLine := FALSE;
      colFG := Font.Color;
      colBG := Color;
      if Assigned(fOnSpecialLineColors) then
        fOnSpecialLineColors(Self, nLine, bSpecialLine, colFG, colBG);
      if bSpecialLine then begin
        // The selection colors are just swapped, like seen in Delphi.
        colSelFG := colBG;
        colSelBG := colFG;
      end else begin
        colSelFG := fSelectedColor.Foreground;
        colSelBG := fSelectedColor.Background;
      end;
      // Paint the lines depending on the assigned highlighter.
      bLineSelected := not bComplexLine and (nSelStart > 0);
      rcToken := rcLine;
      if not Assigned(fHighlighter) then begin
        // Note: The PaintToken procedure will take care of invalid parameters
        // like empty token rect or invalid indices into sLine.
        nTokenLen := Length(sLine);
        if bComplexLine then begin
          SetDrawingColors(FALSE);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(FirstCol));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(nSelStart));
          PaintToken(sLine, nTokenLen, 0, FirstCol, nSelStart);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(nSelEnd));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(LastCol));
          PaintToken(sLine, nTokenLen, 0, nSelEnd, LastCol);
          SetDrawingColors(TRUE);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(nSelStart));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(nSelEnd));
          PaintToken(sLine, nTokenLen, 0, nSelStart, nSelEnd);
        end else begin
          SetDrawingColors(bLineSelected);
          PaintToken(sLine, nTokenLen, 0, FirstCol, LastCol);
        end;
      end else begin
        // Initialize highlighter with line text and range info. It is
        // necessary because we probably did not scan to the end of the last
        // line - the internal highlighter range might be wrong.
        fHighlighter.SetRange(Lines.Objects[nLine - 1]);
        fHighlighter.SetLine(sLine, nLine - 1);
        // Try to concatenate as many tokens as possible to minimize the count
        // of ExtTextOut calls necessary. This depends on the selection state
        // or the line having special colors. For spaces the foreground color
        // is ignored as well.
        TokenAccu.Len := 0;
        while not fHighLighter.GetEol do begin
          // Test first whether anything of this token is visible.
          nTokenPos := fHighLighter.GetTokenPos; // zero-based
          sToken := fHighLighter.GetToken;
          nTokenLen := Length(sToken);
          if (nTokenPos + nTokenLen >= FirstCol) then begin
            // It's at least partially visible. Get the token attributes now.
            attr := fHighlighter.GetTokenAttribute;
            // Store the token chars with the attributes in the TokenAccu
            // record. This will paint any chars already stored if there is
            // a (visible) change in the attributes.
            if Assigned(attr) then
              AddHighlightToken(sToken, nTokenPos, nTokenLen, attr.Foreground,
                attr.Background, attr.Style)
            else
              AddHighlightToken(sToken, nTokenPos, nTokenLen, colFG, colBG,
                Font.Style);
          end;
          // Let the highlighter scan the next token.
          fHighlighter.Next;
        end;
        // Draw anything that's left in the TokenAccu record. Fill to the end
        // of the invalid area with the correct colors.
        PaintHighlightToken(TRUE);
      end;
      // Now paint the right edge if necessary. We do it line by line to reduce
      // the flicker. Should not cost very much anyway, compared to the many
      // calls to ExtTextOut.
      if bDoRightEdge then begin
        Windows.MoveToEx(dc, nRightEdge, rcLine.Top, nil);
        Windows.LineTo(dc, nRightEdge, rcLine.Bottom + 1);
      end;
    end;
  end;

{ end local procedures }

begin
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  bDoRightEdge := FALSE;
  if (fRightEdge > 0) then begin // column value
    nRightEdge := fTextOffset + fRightEdge * fCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then begin
      bDoRightEdge := TRUE;
      Canvas.Pen.Color := fRightEdgeColor;
      Canvas.Pen.Width := 1;
    end;
  end;
  // Do everything else with API calls. This (maybe) realizes the new pen color.
  dc := Canvas.Handle;
  // If anything of the two pixel space before the text area is visible, then
  // fill it with the component background color.
  if (AClip.Left < fGutterWidth + 2) then begin
    rcToken := AClip;
    rcToken.Left := Max(AClip.Left, fGutterWidth);
    rcToken.Right := fGutterWidth + 2;
    SetBkColor(dc, ColorToRGB(Self.Color));
    InternalFillRect(dc, rcToken);
    // Adjust the invalid area to not include this area.
    AClip.Left := rcToken.Right;
  end;
  // Paint the visible text lines. To make this easier, compute first the
  // necessary information about the selected area: is there any visible
  // selected area, and what are its lines / columns?
  // Moved to two local procedures to make it easier to read.
  if (LastLine >= FirstLine) then begin
    ComputeSelectionInfo;
    fTextDrawer.BeginDrawing(dc);
    try
      PaintLines;
    finally
      fTextDrawer.EndDrawing;
    end;
  end;
  // If there is anything visible below the last line, then fill this as well.
  rcToken := AClip;
  rcToken.Top := (LastLine - TopLine + 1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then begin
    SetBkColor(dc, ColorToRGB(Self.Color));
    InternalFillRect(dc, rcToken);
    // Draw the right edge if necessary.
    if bDoRightEdge then begin
      Windows.MoveToEx(dc, nRightEdge, rcToken.Top, nil);
      Windows.LineTo(dc, nRightEdge, rcToken.Bottom + 1);
    end;
  end;
end;

procedure TmwCustomEdit.PasteFromClipboard;
var
  StartOfBlock: TPoint;
  EndOfBlock: TPoint;
  PasteMode: TSelectionMode;
  Mem: HGLOBAL;
  P: PChar;
  Tag: Integer;

  function MakeSpace(count: integer):string;
  var i: integer;
  begin
    result := '';
    for i := 1 to count do
      result := result + ' ';
  end;

  function ConvertTabs(const Str: string):string;
  var
    Pos, Count: integer;
  begin
    result := copy(str, 1, length(str));
    pos := 1;
    count := 0;
    while pos <= length(result) do
    begin
      if result[pos] = #9 then
      begin
        delete(result, pos, 1);
        insert(MakeSpace(8-count), result, pos);
        pos := pos + (8-count);
        count := 0;
      end else
      if result[pos] = #13 then
        count := -1
      else
        count := (count+1) mod 8;
      inc(pos);
    end;
  end;


begin
  // Check for our special format first.
  if Clipboard.HasFormat(mwEditClipboardFormat) then begin
    Clipboard.Open;
    try
      Mem := Clipboard.GetAsHandle(mwEditClipboardFormat);
      P := GlobalLock(Mem);
      if P <> nil then begin
        if SelAvail then
          FUndoList.AddChange(mwcrSelDelete, fBlockBegin, fBlockEnd,
            PChar(SelText), SelectionMode);
        // Our format: SelectionMode value followed by text. See CopyToClipboard
        PasteMode := PSelectionMode(P)^;
        inc(P, SizeOf(TSelectionMode));
        if SelAvail then begin
          StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
          EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
          fBlockBegin := StartOfBlock;
          fBlockEnd := EndOfBlock;
          if SelectionMode = smLine then
            // Pasting always occurs at column 0 when current selection is
            // smLine type
            StartOfBlock.X := 1;
        end else
          StartOfBlock := Point(CaretX, CaretY);
        Tag := 0;
        SetSelTextPrimitive(PasteMode, P, @Tag);
        EndOfBlock := BlockEnd;
        if PasteMode <> smLine then
          FUndoList.AddChange(mwcrPaste, StartOfBlock, EndOfBlock,
            PChar(SelText), PasteMode)
        else
          if CaretX = 1 then
            FUndoList.AddChange(mwcrPaste,
              Point(1, StartOfBlock.y),
              Point(CharsInWindow, EndOfBlock.y - 1),
              PChar(SelText), smLine)
          else
            FUndoList.AddChange(mwcrPaste,
              Point(1, StartOfBlock.y),
              EndOfBlock,
              PChar(SelText), smNormal);

        if PasteMode = smColumn then
          CaretXY := Point(
            Min(StartOfBlock.X, EndOfBlock.X),
            Max(StartOfBlock.Y, EndOfBlock.Y) + 1);
      end else
        raise EmwEditError.Create('Clipboard paste operation failed.');
    finally
      Clipboard.Close;
    end;
    EnsureCursorPosVisible;
  // If our special format isn't there, check for regular text format.
  end else if Clipboard.HasFormat(CF_TEXT) then begin
    // Normal text is much easier...
    if SelAvail then
      FUndoList.AddChange(mwcrSelDelete, fBlockBegin, fBlockEnd, PChar(SelText),
        SelectionMode);
    StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
    EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
    fBlockBegin := StartOfBlock;
    fBlockEnd := EndOfBlock;
    LockUndo;
    SelText := ConvertTabs(Clipboard.AsText);
    UnLockUndo;
    FUndoList.AddChange(mwcrPaste, StartOfBlock, BlockEnd, PChar(SelText),
      smNormal);
    EnsureCursorPosVisible;
  end;
end;

procedure TmwCustomEdit.SelectAll;
begin
  SetBlockBegin(Point(1, 1));
  if Lines.Count > 0 then
    SetBlockEnd(Point(Length(Lines[Lines.Count - 1]) + 1, Lines.Count { - 1}));
  CaretXY := Point(Length(Lines[Lines.Count - 1]) + 1, Lines.Count);
end;

procedure TmwCustomEdit.SetBlockBegin(Value: TPoint);
var
  nInval1, nInval2: integer;
begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  if (SelectionMode = smNormal) then
    if (Value.y >= 1) and (Value.y <= Lines.Count) then
      Value.x := Min(Value.x, Length(Lines[Value.y - 1]) + 1)
    else
      Value.x := 1;
  if SelAvail then begin
    if fBlockBegin.Y < fBlockEnd.Y then begin
      nInval1 := Min(Value.Y, fBlockBegin.Y);
      nInval2 := Max(Value.Y, fBlockEnd.Y);
    end else begin
      nInval1 := Min(Value.Y, fBlockEnd.Y);
      nInval2 := Max(Value.Y, fBlockBegin.Y);
    end;
    fBlockBegin := Value;
    fBlockEnd := Value;
    InvalidateLines(nInval1, nInval2);
  end else begin
    fBlockBegin := Value;
    fBlockEnd := Value;
  end;
end;

procedure TmwCustomEdit.SetBlockEnd(Value: TPoint);
var
  nLine: integer;
  {$IFDEF MWE_MBCSSUPPORT}
  s: string;
  {$ENDIF}
begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  if (SelectionMode = smNormal) then
    if (Value.y >= 1) and (Value.y <= Lines.Count) then
      Value.x := Min(Value.x, Length(Lines[Value.y - 1]) + 1)
    else
      Value.x := 1;
  if (Value.X <> fBlockEnd.X) or (Value.Y <> fBlockEnd.Y) then begin
    {$IFDEF MWE_MBCSSUPPORT}
    if Value.Y <= Lines.Count then begin
      s := Lines[Value.Y - 1];
      if (Length(s) >= Value.X) and (mbTrailByte = ByteType(s, Value.X)) then
        Dec(Value.X);
    end;
    {$ENDIF}
    if (Value.X <> fBlockEnd.X) or (Value.Y <> fBlockEnd.Y) then
      if (SelectionMode = smColumn) and (Value.X <> fBlockEnd.X) then begin
        InvalidateLines(
          Min(fBlockBegin.Y, Min(fBlockEnd.Y, Value.Y)),
          Max(fBlockBegin.Y, Max(fBlockEnd.Y, Value.Y)));
        fBlockEnd := Value;
      end else begin
        nLine := fBlockEnd.Y;
        fBlockEnd := Value;
        if (SelectionMode <> smColumn) or (fBlockBegin.X <> fBlockEnd.X) then
          InvalidateLines(nLine, fBlockEnd.Y);
      end;
  end;
end;

procedure TmwCustomEdit.SetCaretX(Value: Integer);
begin
  SetCaretXY(Point(Value, fCaretY + 1));
end;

procedure TmwCustomEdit.SetCaretY(Value: Integer);
begin
  SetCaretXY(Point(fCaretX + 1, Value));
end;

function TmwCustomEdit.GetCaretXY: TPoint;
begin
  Result := Point(fCaretX + 1, fCaretY + 1);
end;

procedure TmwCustomEdit.SetCaretXY(Value: TPoint);
var
  nMaxX: integer;
begin
  nMaxX := fMaxLeftChar - fCharsInWindow - 1;
  if Value.Y > Lines.Count then Value.Y := Lines.Count;
  Dec(Value.X);
  Dec(Value.Y);
  if Value.Y < 0 then begin
    // this is just to make sure if Lines stringlist should be empty
    Value.Y := 0;
    if not (mweoScrollPastEol in fOptions) then nMaxX := 0;
  end else
    if not (mweoScrollPastEol in fOptions) then nMaxX := Length(Lines[Value.Y]);
  if Value.X > nMaxX then Value.X := nMaxX;
  if Value.X < 0 then Value.X := 0;
  // Value is zero-based now!
  if (Value.X <> fCaretX) or (Value.Y <> fCaretY) then begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if fCaretX <> Value.X then begin
        fCaretX := Value.X;
        Include(fStatusChanges, mwscCaretX);
      end;
      if fCaretY <> Value.Y then begin
        fCaretY := Value.Y;
        Include(fStatusChanges, mwscCaretY);
      end;
      EnsureCursorPosVisible;
      Include(fStateFlags, mwsfCaretChanged);
      Include(fStateFlags, mwsfScrollbarChanged);
    finally
      DecPaintLock;
    end;
    SelectionChange;
  end;
end;

procedure TmwCustomEdit.SetFont(const Value: TFont);
var
  DC: HDC;
  Save: THandle;
  Metrics: TTextMetric;
  AveCW, MaxCW: Integer;
begin
  DC := GetDC(0);
  Save := SelectObject(DC, Value.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, Save);
  ReleaseDC(0, DC);
  with Metrics do begin
    AveCW := tmAveCharWidth;
    MaxCW := tmMaxCharWidth;
  end;
  case AveCW = MaxCW of
    True: inherited Font := Value;
    False:
      begin
        with fFontDummy do begin
          Color := Value.Color;
          Pitch := fpFixed;
          Size := Value.Size;
          Style := Value.Style;
        end;
        inherited Font := fFontDummy;
      end;
  end;
  if fGutter.ShowLineNumbers then GutterChanged(Self);
end;

procedure TmwCustomEdit.SetGutterWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if fGutterWidth <> Value then begin
    fGutterWidth := Value;
    fTextOffset := fGutterWidth + 2 - fLeftChar * fCharWidth;
    if HandleAllocated then begin
      fCharsInWindow := (ClientWidth - fGutterWidth - 2) div fCharWidth;
      UpdateScrollBars(FALSE);
      Invalidate;
    end;
  end;
end;

procedure TmwCustomEdit.SetLeftChar(Value: Integer);
begin
  Value := MinMax(Value, 1, fMaxLeftChar - fCharsInWindow) - 1;
  if Value <> fLeftChar then begin
    fLeftChar := Value;
    fTextOffset := fGutterWidth + 2 - fLeftChar * fCharWidth;
    UpdateScrollBars(FALSE);
    InvalidateLines(-1, -1);
    StatusChanged([mwscLeftChar]);
  end;
end;

procedure TmwCustomEdit.SetLines(Value: TStrings);
begin
  if HandleAllocated then
    Lines.Assign(Value);
end;

procedure TmwCustomEdit.SetLineText(Value: string);
begin
  if (Lines.Count > 0) and
    (fCaretY <= Lines.Count - 1) then Lines[fCaretY] := Value;
end;

procedure TmwCustomEdit.SetName(const Value: TComponentName);
var
  OldName: string;
begin
  OldName := Name;
  inherited SetName(Value);
  if (csDesigning in ComponentState) and
    (not (csLoading in ComponentState)) then
    if OldName = TrimRight(Text) then
      Text := Value;
end;

procedure TmwCustomEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then begin
    FScrollBars := Value;
    RecreateWnd;
    UpdateScrollBars(FALSE);
    Invalidate;
  end;
end;

procedure TmwCustomEdit.SetSelText(const Value: string);
begin
  SetSelTextPrimitive(smNormal, PChar(Value), nil);
end;

{begin}                                                                         //gp 1999-12-03
// This is really a last minute change and I hope I did it right.
// Reason for this modification: next two lines will loose the CaretX position
// if mweoScrollPastEol is not set in Options. That is not really a good idea
// as we would typically want the cursor to stay where it is.
// To fix this (in the absence of a better idea), I changed the code in
// DeleteSelection not to trim the string if mweoScrollPastEol is not set.
{end}                                                                           //gp 1999-12-03
procedure TmwCustomEdit.SetSelTextPrimitive(PasteMode: TSelectionMode;
  Value: PChar; Tag: PInteger);
var
  BB, BE: TPoint;
  TempString: string;

  procedure DeleteSelection;
  var
    x, MarkOffset: Integer;
    UpdateMarks: boolean;
    {$IFDEF MWE_MBCSSUPPORT}
    l, r: Integer;
    {$ENDIF}
  begin
    UpdateMarks := FALSE;
    MarkOffset := 0;
    case SelectionMode of
      smNormal:
        begin
            // Create a string that contains everything on the first line up to
            // the selection mark, and everything on the last line after the
            // selection mark.
          TempString := Copy(Lines[BB.Y - 1], 1, BB.X - 1) +
            Copy(Lines[BE.Y - 1], BE.X, MaxInt);
            // Delete all lines in the selection range.
          for x := BE.Y - 1 downto BB.Y do
            Lines.Delete(x);
            // Put the stuff that was outside of selection back in.
          if mweoScrollPastEol in Options then                                  //gp 1999-12-03
            TempString := TrimRight(TempString);                                //gp 1999-12-03
          Lines[BB.Y - 1] := TempString;
          UpdateMarks := TRUE;
          CaretXY := BB;
        end;
      smColumn:
        begin
            // swap X if needed
          if BB.X > BE.X then
            {$IFDEF MWE_COMPILER_3_UP}
            SwapInt(BB.X, BE.X);
          {$ELSE}begin
            x := BB.X;
            BB.X := BE.X;
            BE.X := x;
          end;
          {$ENDIF}
          for x := BB.Y - 1 to BE.Y - 1 do begin
            TempString := Lines[x];
            {$IFNDEF MWE_MBCSSUPPORT}
            Delete(TempString, BB.X, BE.X - BB.X);
            {$ELSE}
            l := BB.X;
            r := BE.X;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(TempString, l, r);
            Delete(TempString, l, r - l);
            {$ENDIF}
            Lines[x] := TrimRight(TempString);
          end;
            // Lines never get deleted completely, so keep caret at end.
          CaretXY := Point(BB.X, fBlockEnd.Y);
            // Column deletion never removes a line entirely, so no mark
            // updating is needed here.
        end;
      smLine:
        begin
          if BE.Y = Lines.Count then begin
            Lines[BE.Y - 1] := '';
            for x := BE.Y - 2 downto BB.Y - 1 do
              Lines.Delete(x);
          end else
            for x := BE.Y - 1 downto BB.Y - 1 do
              Lines.Delete(x);
            // smLine deletion always resets to first column.
          CaretXY := Point(1, BB.Y);
          UpdateMarks := TRUE;
          MarkOffset := 1;
        end;
    end;
    // Update marks
    if UpdateMarks then
      for x := 0 to Marks.Count - 1 do
        if Marks[x].Line >= BE.Y then
          Marks[x].Line := Marks[x].Line - (BE.Y - BB.Y) - MarkOffset
        else if Marks[x].Line > BB.Y then
          Marks[x].Line := BB.Y;
  end;

  procedure InsertText;

    function GetEOL(pStart: PChar): PChar;
    begin
      Result := pStart;
      while not (Result^ in [#0, #10, #13]) do
        Inc(Result);
    end;

    function InsertNormal: Integer;
    var
      sLeftSide: string;
      sRightSide: string;
      Str: string;
      Start: PChar;
      P: PChar;
    begin
      Result := 0;
      sLeftSide := Copy(LineText, 1, CaretX - 1);
      if fCaretX > Length(sLeftSide) then
        sLeftSide := sLeftSide + StringOfChar(' ', fCaretX - Length(sLeftSide));
      sRightSide := Copy(LineText, CaretX, Length(LineText) - (CaretX - 1));
      // step1: insert the first line of Value into current line
      Start := PChar(Value);
      P := GetEOL(Start);
      if P = Start then
        Lines[fCaretY] := sLeftSide
      else
        if p^ <> #0 then
          Lines[fCaretY] := sLeftSide + Copy(Value, 1, P - Start)
        else
          Lines[fCaretY] := sLeftSide + Value + sRightSide;
      // step2: insert left lines of Value
      while p^ <> #0 do begin
        if p^ = #13 then
          Inc(p);
        if p^ = #10 then
          Inc(p);
        Inc(fCaretY);
        Start := P;
        P := GetEOL(Start);
        if P = Start then begin
          if p^ <> #0 then
            Lines.Insert(fCaretY, '')
          else
            Lines.Insert(fCaretY, sRightSide);
        end
        else begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
          if p^ <> #0 then
            Lines.Insert(fCaretY, Str)
          else
            Lines.Insert(fCaretY, Str + sRightSide);
        end;
        Inc(Result);
      end;
      fCaretX := Length(Lines[fCaretY]) - Length(sRightSide);
      StatusChanged([mwscCaretX]);
    end;

    function InsertColumn: Integer;
    var
      Str: string;
      Start: PChar;
      P: PChar;
      Len: Integer;
      InsertPos: Integer;
    begin
      // Insert string at current position
      InsertPos := CaretX;
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
          if fCaretY = Lines.Count then
            Lines.Add(StringOfChar(' ', InsertPos - 1) + Str)
          else begin
            TempString := Lines[fCaretY];
            Len := Length(TempString);
            if Len < InsertPos then
              TempString :=
                TempString + StringOfChar(' ', InsertPos - Len - 1) + Str
            else begin
              {$IFDEF MWE_MBCSSUPPORT}
              if mbTrailByte = ByteType(TempString, InsertPos) then
                Insert(Str, TempString, InsertPos + 1)
              else
                {$ENDIF}
                Insert(Str, TempString, InsertPos);
            end;
            Lines[fCaretY] := TempString;
          end;
        end;
        if Tag <> nil then
          Tag^ := P - Start;
        if P^ = #13 then begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
          Inc(fCaretY);
        end;
        Start := P;
      until P^ = #0;
      Inc(fCaretX, Length(Str));
      Result := 0;
    end;

    function InsertLine: Integer;
    var
      Start: PChar;
      P: PChar;
      Str: string;
      n: Integer;
    begin
      Result := 0;
      fCaretX := 0;
      // Insert string before current line
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
        end
        else
          Str := '';
        if (P^ = #0) then begin
          n := Lines.Count;
          if (n > fCaretY) then
            Lines[fCaretY] := Str + Lines[fCaretY]
          else
            Lines.Add(Str);
          fCaretX := Length(Str);
        end else begin
          Lines.Insert(fCaretY, Str);
          Inc(fCaretY);
          Inc(Result);
          if P^ = #13 then
            Inc(P);
          if P^ = #10 then
            Inc(P);
          Start := P;
        end;
      until P^ = #0;
      StatusChanged([mwscCaretX]);
    end;

  var
    StartLine: Integer;
    InsertedLines: Integer;
    x: Integer;
  begin
    if Value = '' then
      Exit;

    // Using a TStringList to do this would be easier, but if we're dealing
    // with a large block of text, it would be very inefficient.  Consider:
    // Assign Value parameter to TStringList.Text: that parses through it and
    // creates a copy of the string for each line it finds.  That copy is passed
    // to the Add method, which in turn creates a copy.  Then, when you actually
    // use an item in the list, that creates a copy to return to you.  That's
    // 3 copies of every string vs. our one copy below.  I'd prefer no copies,
    // but we aren't set up to work with PChars that well.

    StartLine := CaretY;
    case PasteMode of
      smNormal:
        InsertedLines := InsertNormal;
      smColumn:
        InsertedLines := InsertColumn;
      smLine:
        InsertedLines := InsertLine;
    else
      InsertedLines := 0;
    end;
    // We delete selected based on the current selection mode, but paste
    // what's on the clipboard according to what it was when copied.
    // Update marks
    if InsertedLines > 0 then
      for x := 0 to Marks.Count - 1 do
        if Marks[x].Line >= StartLine then
          Marks[x].Line := Marks[x].Line + InsertedLines;
    // Force caret reset
    CaretXY := CaretXY;
  end;

begin
  IncPaintLock;
  Lines.BeginUpdate;
  try
    BB := BlockBegin;
    BE := BlockEnd;
    if SelAvail then
      DeleteSelection;
    if (Value <> nil) and (Value[0] <> #0) then
      InsertText;
    if Lines.Count = 0 then
      Lines.Add('');
    if CaretY < 1 then
      CaretY := 1;
  finally
    Lines.EndUpdate;
    DecPaintLock;
  end;
end;

procedure TmwCustomEdit.SetText(const Value: string);
begin
  TmwEditList(fLines).BeginLoading;
  try
    Lines.Text := Value;
  finally TmwEditList(fLines).EndLoading; end;
end;

procedure TmwCustomEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
begin
  // don't use MinMax here, it will fail in design mode (Lines.Count is zero,
  // but the painting code relies on TopLine >= 1)
  Value := Min(Value, Lines.Count);
  Value := Max(Value, 1);
  if Value <> fTopLine + 1 then begin
    Delta := (fTopLine + 1) - Value;
    fTopLine := Value - 1;
    UpdateScrollBars(FALSE);
    if Abs(Delta) < fLinesInWindow then
      ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil)
    else
      Invalidate;
    StatusChanged([mwscTopLine]);
  end;
end;

procedure TmwCustomEdit.ShowCaret;
begin
  if not (mwsfCaretVisible in fStateFlags) then
    if Windows.ShowCaret(Handle) then Include(fStateFlags, mwsfCaretVisible);
end;

procedure TmwCustomEdit.UpdateCaret;
var
  CX, CY: Integer;
  {$IFDEF MWE_MBCSSUPPORT}
  cf: TCompositionForm;
  {$ENDIF}
begin
// "prettified" it a little
  if (PaintLock <> 0) or not Focused then
    Include(fStateFlags, mwsfCaretChanged)
  else begin
    Exclude(fStateFlags, mwsfCaretChanged);
    CX := CaretXPix + FCaretOffset.X;
    CY := CaretYPix + FCaretOffset.Y;
    if (CX >= fGutterWidth) and (CX < ClientWidth) and
      (CY >= 0) and (CY < ClientHeight)
      then begin
      SetCaretPos(CX, CY);
      ShowCaret;
    end else begin
      HideCaret;
      SetCaretPos(CX, CY);
    end;
    {$IFDEF MWE_MBCSSUPPORT}
    cf.dwStyle := CFS_POINT;
    cf.ptCurrentPos := Point(CX, CY);
    ImmSetCompositionWindow(ImmGetContext(Handle), @cf);
    {$ENDIF}
  end;
end;

procedure TmwCustomEdit.UpdateScrollBars(Force: boolean);
var
  ScrollInfo: TScrollInfo;
  nMaxScroll: integer;
begin
  if not HandleAllocated or ((PaintLock <> 0) and not Force) then
    Include(fStateFlags, mwsfScrollbarChanged)
  else begin
    Exclude(fStateFlags, mwsfScrollbarChanged);
    if fScrollBars <> ssNone then begin
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
      ScrollInfo.nMin := 1;
      ScrollInfo.nTrackPos := 0;
      // moved here because ScrollInfo.nMin could be changed later on
      if fScrollBars in [ssBoth, ssHorizontal] then begin
        ScrollInfo.nMax := fMaxLeftChar - fCharsInWindow;
        ScrollInfo.nPage := CharsInWindow;
        ScrollInfo.nPos := LeftChar;
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
      end;
      if fScrollBars in [ssBoth, ssVertical] then begin
        nMaxScroll := LinesInWindow + Lines.Count - 1;
        if nMaxScroll <= MAX_SCROLL then begin
          ScrollInfo.nMax := Max(1, nMaxScroll);
          ScrollInfo.nPage := LinesInWindow;
          ScrollInfo.nPos := TopLine;
        end else begin
          // see Brad's description of MulDiv in the Print method
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, LinesInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, TopLine, nMaxScroll);
        end;
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      end;
    end;
  end;
end;

procedure TmwCustomEdit.WMDropFiles(var Message: TMessage);
var
  i, iNumberDropped: integer;
  szPathName: array[0..260] of char;
  Point: TPoint;
  FilesList: TStringList;
begin
  try
    if Assigned(fOnDropFiles) then begin
      FilesList := TStringList.Create;
      try
        iNumberDropped := DragQueryFile(THandle(Message.wParam), Cardinal(-1),
          nil, 0);
        DragQueryPoint(THandle(Message.wParam), Point);

        for i := 0 to iNumberDropped - 1 do begin
          DragQueryFile(THandle(Message.wParam), i, szPathName,
            SizeOf(szPathName));
          FilesList.Add(szPathName);
        end;
        fOnDropFiles(Self, Point.X, Point.Y, FilesList);
      finally
        FilesList.Free;
      end;
    end;
  finally
    Message.Result := 0;
    DragFinish(THandle(Message.wParam));
  end;
end;

procedure TmwCustomEdit.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TmwCustomEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  if fWantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
end;

procedure TmwCustomEdit.WMHScroll(var Message: TWMScroll);

begin
  case Message.ScrollCode of
      // Scrolls to start / end of the line
    SB_TOP: LeftChar := 1;
    SB_BOTTOM: LeftChar := MaxLeftChar;
      // Scrolls one char left / right
    SB_LINEDOWN: LeftChar := LeftChar + 1;
    SB_LINEUP: LeftChar := LeftChar - 1;
      // Scrolls one page of chars left / right
    SB_PAGEDOWN: LeftChar := LeftChar + CharsInWindow;
    SB_PAGEUP: LeftChar := LeftChar - CharsInWindow;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: LeftChar := Message.Pos;
      // Ends scroll = do nothing
  end;
end;

procedure TmwCustomEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  HideCaret;
  Windows.DestroyCaret;
  if FHideSelection and SelAvail then Invalidate;
end;

procedure TmwCustomEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  InitializeCaret;
  if FHideSelection and SelAvail then Invalidate;
end;

procedure TmwCustomEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  SizeOrFontChanged(FALSE);
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then begin
    ScrollHintWnd := HintWindowClass.Create(Application);
    ScrollHintWnd.Visible := FALSE;
  end;
  Result := ScrollHintWnd;
end;

procedure TmwCustomEdit.WMVScroll(var Message: TWMScroll);

var
  s: ShortString;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
begin
  case Message.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP: TopLine := 1;
    SB_BOTTOM: TopLine := Lines.Count;
      // Scrolls one line up / down
    SB_LINEDOWN: TopLine := TopLine + 1;
    SB_LINEUP: TopLine := TopLine - 1;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopLine := TopLine + fLinesInWindow;
    SB_PAGEUP: TopLine := TopLine - fLinesInWindow;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        if Lines.Count > MAX_SCROLL then
          TopLine := MulDiv(LinesInWindow + Lines.Count - 1, Message.Pos,
            MAX_SCROLL)
        else
          TopLine := Message.Pos;

        if mweoShowScrollHint in fOptions then begin
          ScrollHint := GetScrollHint;
          if not ScrollHint.Visible then begin
            ScrollHint.Color := Application.HintColor;
            ScrollHint.Visible := TRUE;
          end;
          s := Format(MWS_ScrollInfoFmt, [TopLine]);
          {$IFDEF MWE_COMPILER_3_UP}
          rc := ScrollHint.CalcHintRect(200, s, nil);
          {$ELSE}
          rc := Rect(0, 0, ScrollHint.Canvas.TextWidth(s) + 6,
            ScrollHint.Canvas.TextHeight(s) + 4);
          {$ENDIF}
          pt := ClientToScreen(Point(ClientWidth - rc.Right - 4, 10));
          OffsetRect(rc, pt.x, pt.y);
          ScrollHint.ActivateHint(rc, s);
          {$IFNDEF MWE_COMPILER_3_UP}
// this is a quick hack to get the text updated. Any better solution?
          ScrollHint.Invalidate;
          {$ENDIF}
          ScrollHint.Update;
        end;
      end;
      // Ends scrolling
    SB_ENDSCROLL:
      if mweoShowScrollHint in fOptions then with GetScrollHint do begin
        Visible := FALSE;
        ActivateHint(Rect(0, 0, 0, 0), '');
      end;
  end;
  Update;
end;

function TmwCustomEdit.ScanFrom(Index: integer): integer;
begin
  Result := Index;
  if Index >= Lines.Count - 1 then Exit;
  fHighLighter.SetLine(Lines[Result], Result);
  inc(Result);
  fHighLighter.NextToEol;
  while fHighLighter.GetRange <> fLines.Objects[Result] do begin
    Lines.Objects[Result] := fHighLighter.GetRange;
    fHighLighter.SetLine(Lines[Result], Result);
    fHighLighter.NextToEol;
    inc(Result);
    if Result = Lines.Count then break;
  end;
  Dec(Result);
end;

procedure TmwCustomEdit.ListAdded(Sender: TObject);
var
  LastIndex: Integer;
begin
  if Assigned(fHighLighter) then begin
    if Lines.Count > 1 then begin
      LastIndex := Lines.Count - 1;
      fHighLighter.SetRange(Lines.Objects[LastIndex - 1]);
      fHighLighter.SetLine(Lines[LastIndex - 1], LastIndex - 1);
      fHighLighter.NextToEol;
      Lines.Objects[LastIndex] := fHighLighter.GetRange;
    end else begin
      fHighLighter.ReSetRange;
      Lines.Objects[0] := fHighLighter.GetRange;
    end;
  end;
  LastIndex := Lines.Count;
  InvalidateLines(LastIndex, LastIndex);
  InvalidateGutter(LastIndex, LastIndex);
end;

procedure TmwCustomEdit.ListCleared(Sender: TObject);
begin
  // invalidate the *whole* client area
  FillChar(fInvalidateRect, SizeOf(TRect), 0);
  Invalidate;
  // set caret and selected block to start of text
  SetBlockBegin(Point(1, 1));
  SetCaretXY(Point(1, 1));
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
end;

procedure TmwCustomEdit.ListDeleted(Index: Integer);
begin
  if Assigned(fHighlighter) and (Lines.Count >= 1) then
    if (Index > 0) then begin
      fHighLighter.SetRange(Lines.Objects[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      fHighLighter.ReSetRange;
      Lines.Objects[0] := fHighLighter.GetRange;
      if (Lines.Count > 1) then ScanFrom(0);
    end;
  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutter(Index + 1, MaxInt);
end;

procedure TmwCustomEdit.ListInserted(Index: Integer);
begin
  if Assigned(fHighlighter) and (Lines.Count >= 1) then
    if (Index > 0) then begin
      fHighLighter.SetRange(Lines.Objects[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      fHighLighter.ReSetRange;
      Lines.Objects[0] := fHighLighter.GetRange;
      if (Lines.Count > 1) then ScanFrom(0);
    end;
  InvalidateLines(Index + 1, TopLine + LinesInWindow);
  InvalidateGutter(Index + 1, TopLine + LinesInWindow);
end;

procedure TmwCustomEdit.ListLoaded(Sender: TObject);
begin
  if (FLines.Count = 0) then FLines.Add('');
// reset of caret and selected block moved to ListCleared
  ClearUndo;
end;

procedure TmwCustomEdit.ListPutted(Index: Integer);
begin
  if Assigned(fHighLighter) then begin
    fHighLighter.SetRange(Lines.Objects[Index]);
    InvalidateLines(Index + 1, ScanFrom(Index) + 1);
  end else
    InvalidateLines(Index + 1, Index + 1);
end;

procedure TmwCustomEdit.ListScanRanges(Sender: TObject);
var
  i: integer;
begin
  if Assigned(fHighLighter) and (Lines.Count > 0) then begin
    fHighLighter.ResetRange;
    Lines.Objects[0] := fHighLighter.GetRange;
    i := 1;
    while (i < Lines.Count) do begin
      fHighLighter.SetRange(Lines.Objects[i - 1]);
      fHighLighter.SetLine(Lines[i - 1], i - 1);
      fHighLighter.NextToEol;
      Lines.Objects[i] := fHighLighter.GetRange;
      Inc(i);
    end;
  end;
end;

{$IFDEF MWE_MBCSSUPPORT}
type
  TStringType = (
    stNone,
    stHalfNumAlpha,
    stHalfSymbol,
    stHalfKatakana,
    stWideNumAlpha,
    stWideSymbol,
    stWideKatakana,
    stHiragana,
    stIdeograph,
    stControl,
    stKashida
    );

{  }

function IsStringType(Value: Word): TStringType;
begin
  Result := stNone;

  if (Value = C3_SYMBOL) then begin
    (***  Controls  ***)
    Result := stControl;
  end else
    if ((Value and C3_HALFWIDTH) <> 0) then begin
    (*** singlebyte ***)
      if (Value = C3_HALFWIDTH) or
        (Value = (C3_ALPHA or C3_HALFWIDTH)) then begin { Number & Alphabet }
        Result := stHalfNumAlpha;
      end else
        if ((Value and C3_SYMBOL) <> 0) or
          ((Value and C3_LEXICAL) <> 0) then begin { Symbol }
          Result := stHalfSymbol;
        end else
          if ((Value and C3_KATAKANA) <> 0) then begin { Japanese-KATAKANA }
            Result := stHalfKatakana;
          end;
    end else begin
    (*** doublebyte ***)
      if (Value = C3_FULLWIDTH) or
        (Value = (C3_ALPHA or C3_FULLWIDTH)) then begin { Number & Alphabet }
        Result := stWideNumAlpha;
      end
      else if ((Value and C3_SYMBOL) <> 0) or
          ((Value and C3_LEXICAL) <> 0) then begin { Symbol }
        Result := stWideSymbol;
      end
      else if ((Value and C3_KATAKANA) <> 0) then begin { Japanese-KATAKANA }
        Result := stWideKatakana;
      end
      else if ((Value and C3_HIRAGANA) <> 0) then begin { Japanese-HIRAGANA }
        Result := stHiragana;
      end
      else if ((Value and C3_IDEOGRAPH) <> 0) then begin { Ideograph }
        Result := stIdeograph;
      end;
    end;
end;

{  }

procedure TmwCustomEdit.SetWordBlock(Value: TPoint);
var
  i: Integer;
  Runner: TPoint;
  TempString: string;
  IdChars: TIdentChars;

  procedure MultiBlockScan;
  var
    i: Integer;
    wideX: Integer;
    cType: PWordArray;
    cLeng: Integer;
    stc: TStringType;
  begin
    wideX := ByteToCharIndex(TempString, Value.X - 1);

    cLeng := ByteToCharLen(TempString, Length(TempString));
    GetMem(cType, SizeOf(Word) * cLeng);
    try
      if not GetStringTypeEx(
        LOCALE_SYSTEM_DEFAULT,
        CT_CTYPE3,
        PChar(TempString),
        Length(TempString),
        cType^) then begin
        Exit;
      end;
      stc := IsStringType(cType^[wideX]);
      if (stc = stControl) then begin
        Exit;
      end;
      { search BlockEnd }
      for i := wideX + 1 to cLeng - 1 do begin
        if (IsStringType(cType^[i]) <> stc) then begin
          Runner.Y := (i + 1);
          Break;
        end;
      end;
      Runner.Y := (i + 1);
      if Runner.Y > cLeng then Runner.Y := cLeng;
      { search BlockBegin }
      for i := wideX - 1 downto 0 do begin
        if (IsStringType(cType^[i]) <> stc) then begin
          Runner.X := (i + 2);
          Break;
        end;
      end;
      Runner.X := CharToByteIndex(TempString, Runner.X);
      Runner.Y := CharToByteIndex(TempString, Runner.Y);
    finally
      FreeMem(cType);
    end;
  end;

begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  TempString := (Lines[Value.Y - 1] + #$0);
  if (Value.X >= Length(TempString)) then begin
    CaretXY := Point(Length(TempString), Value.Y);
    Exit;
  end;
  if (fHighlighter <> nil) and
    (ByteType(TempString, Value.X) <> mbLeadByte) then begin
    Runner := Point(0, Length(TempString));
    IdChars := fHighlighter.IdentChars;
    { search BlockEnd }
    for i := Value.X to Length(TempString) - 1 do begin
      if not (TempString[i] in IdChars) then begin
        Runner.Y := i;
        Break;
      end;
    end;
    { search BlockBegin }
    for i := Value.X - 1 downto 1 do begin
      if not (TempString[i] in IdChars) then begin
        Runner.X := (i + 1);
        Break;
      end;
    end;
  end else begin
    MultiBlockScan;
  end;
  fBlockBegin := Point(Runner.X, Value.Y);
  fBlockEnd := Point(Runner.Y, Value.Y);
  CaretXY := Point(Runner.Y, Value.Y);
  InvalidateLines(Value.Y, Value.Y);
end;
{$ELSE}

procedure TmwCustomEdit.SetWordBlock(Value: TPoint);
var
  Runner: TPoint;
  TempString: string;
  IdChars: TIdentChars;
begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  TempString := Lines[Value.Y - 1];
  if TempString = '' then exit;
  // Click on right side of text
  if Length(TempString) < Value.X then Value.X := Length(TempString);
  Runner := Value;
  if fHighlighter <> nil then
    IdChars := fHighlighter.IdentChars
  else
    IDchars := [#33..#255];
  if not (TempString[Runner.X] in IdChars) then begin
    // no word under cursor and next char right is not start of a word
    if (Runner.X > 1) and (not (TempString[Runner.X] in IdChars)) then begin
      // find end of word on the left side
      while Runner.X > 0 do begin
        if (TempString[Runner.X] in IdChars) then break;
        Dec(Runner.X);
      end;
    end;
    // no word on the left side, so look to the right side
    if not (TempString[Runner.X] in IdChars) then begin
      Runner := Value;
      while Runner.X < fMaxLeftChar do begin
        if (TempString[Runner.X] in IdChars) then break;
        Inc(Runner.X);
      end;
      if Runner.X > fMaxLeftChar then
        exit;
    end;
    Value := Runner;
  end;
  while Runner.X > 0 do begin
    if not (TempString[Runner.X] in IdChars) then break;
    Dec(Runner.X);
  end;
  Inc(Runner.X);
  if Runner.X < 1 then Runner.X := 1;
  fBlockBegin := Runner;
  Runner := Value;
  while Runner.X < fMaxLeftChar do begin
    if not (TempString[Runner.X] in IdChars) then break;
    Inc(Runner.X);
  end;
  if Runner.X > fMaxLeftChar then Runner.X := fMaxLeftChar;
  fBlockEnd := Runner;
// set caret to the end of selected block
  CaretXY := Runner;
  InvalidateLines(Value.Y, Value.Y);
end;
{$ENDIF}

procedure TmwCustomEdit.DblClick;
begin
  SetWordBlock(CaretXY);
  inherited;
  Include(fStateFlags, mwsfDblClicked);
end;

function TmwCustomEdit.GetCanUndo: Boolean;
begin
  result := (fUndoList.CanUndo > 0);
end;

function TmwCustomEdit.GetCanRedo: Boolean;
begin
  result := (fRedoList.CanUndo > 0);
end;

function TmwCustomEdit.GetCanPaste;
begin
  Result := Clipboard.HasFormat(CF_TEXT)
    or Clipboard.HasFormat(mwEditClipboardFormat)
end;

procedure TmwCustomEdit.Redo;
var
  ChangeStr: PChar;
  ChangeStartPos,
    ChangeEndPos: TPoint;
  ChangeReason: TChangeReason;
  OldSelMode,
    ChangeSelMode: TSelectionMode;
begin
  if not CanRedo then exit;
  OldSelMode := SelectionMode;
  ChangeReason := FRedoList.GetChange(ChangeStartPos, ChangeEndPos, ChangeStr,
    ChangeSelMode);
  SelectionMode := ChangeSelMode;
  case ChangeReason of
    mwcrInsert, mwcrPaste,
    mwcrDragDropInsert:
      begin
        IncPaintLock;
        CaretXY := ChangeStartPos;
        SetBlockBegin(ChangeStartPos);
        DecPaintLock;
        SetSelTextPrimitive(ChangeSelMode, ChangeStr, nil);
        FUndoList.AddChange(ChangeReason, ChangeStartPos, ChangeEndPos,
          PChar(GetSelText), ChangeSelMode);
        StrDispose(ChangeStr);
      end;
    mwcrDeleteAfterCursor:
      begin
        IncPaintLock;
        CaretXY := ChangeStartPos;
        SetBlockBegin(ChangeStartPos);
        SetBlockEnd(ChangeEndPos);
        DecPaintLock;
        FUndoList.AddChange(ChangeReason, ChangeStartPos, ChangeEndPos,
          PChar(GetSelText), ChangeSelMode);
        SetSelTextPrimitive(ChangeSelMode, ChangeStr, nil);
        StrDispose(ChangeStr);
        CaretXY := ChangeStartPos;
      end;
    mwcrDelete,
    mwcrDragDropDelete,
    mwcrSelDelete:
      begin
        IncPaintLock;
        SetBlockBegin(ChangeStartPos);
        SetBlockEnd(ChangeEndPos);
        DecPaintLock;
        FUndoList.AddChange(ChangeReason, ChangeStartPos, ChangeEndPos,
          PChar(GetSelText), ChangeSelMode);
        SetSelTextPrimitive(ChangeSelMode, ChangeStr, nil);
        StrDispose(ChangeStr);
        CaretXY := ChangeStartPos;
        if ((FRedoList.GetChangeReason = mwcrDragDropInsert) and
          (ChangeReason = mwcrDragDropDelete)) or
          (ChangeReason = mwcrSelDelete) then Redo;
      end;
    mwcrLineBreak:
      begin
        CommandProcessor(ecLineBreak, #13, nil);
      end;
  end;
  SelectionMode := OldSelMode;
end;

procedure TmwCustomEdit.Undo;
var
  ChangeStr: PChar;
  ChangeStartPos,
    ChangeEndPos: TPoint;
  ChangeReason: TChangeReason;
  Temp: string;
  TmpPos: TPoint;
  OldSelMode,
    ChangeSelMode: TSelectionMode;
  i: integer;
begin
  if not CanUndo then exit;
  OldSelMode := SelectionMode;
  ChangeReason := FUndoList.GetChange(ChangeStartPos, ChangeEndPos, ChangeStr,
    ChangeSelMode);
  SelectionMode := ChangeSelMode;
  IncPaintLock;
  case ChangeReason of
    mwcrInsert,
    mwcrPaste,
    mwcrDragDropInsert:
      begin
        SetBlockBegin(ChangeStartPos);
        SetBlockEnd(ChangeEndPos);
        FRedoList.AddChange(ChangeReason, ChangeStartPos, ChangeEndPos,
          PChar(GetSelText), ChangeSelMode);
        SetSelTextPrimitive(ChangeSelMode, ChangeStr, nil);
        StrDispose(ChangeStr);
        CaretXY := ChangeStartPos;
        if ((FUndoList.GetChangeReason = mwcrDragDropDelete) and
          (ChangeReason = mwcrDragDropInsert)) or
          (FUndoList.GetChangeReason = mwcrSelDelete) then Undo;
      end;
    mwcrDeleteAfterCursor,
    mwcrDelete,
    mwcrDragDropDelete,
    mwcrSelDelete:
      begin
        // If there's no selection, we have to set
        // the Caret's position manualy.
        if ChangeSelMode = smColumn then
          TmpPos := Point(
            Min(ChangeStartPos.X, ChangeEndPos.X),
            Min(ChangeStartPos.Y, ChangeEndPos.Y))
        else
          TmpPos := minPoint(ChangeStartPos, ChangeEndPos);
        if (ChangeReason = mwcrDeleteAfterCursor) and
          (TmpPos.Y > GetLineCount) then begin
          CaretXY := Point(1, GetLineCount);
          CommandProcessor(ecLineBreak, #13, nil);
        end;
        CaretXY := TmpPos;
        SetBlockBegin(TmpPos);
        SetSelTextPrimitive(ChangeSelMode, ChangeStr, nil);
        CaretXY := ChangeEndPos;
        SetBlockBegin(ChangeStartPos);
        SetBlockEnd(ChangeEndPos);
        FRedoList.AddChange(ChangeReason, ChangeStartPos, ChangeEndPos,
          nil, ChangeSelMode);
        EnsureCursorPosVisible;
        StrDispose(ChangeStr);
      end;
    mwcrLineBreak:
      begin
        // If there's no selection, we have to set
        // the Caret's position manualy.
        CaretXY := ChangeStartPos;
        FRedoList.AddChange(ChangeReason, ChangeStartPos, ChangeEndPos, nil,
          ChangeSelMode);
        if CaretY > 0 then begin
          Temp := Lines.Strings[fCaretY];
          if (Length(Temp) < fCaretX) and (LeftSpaces(ChangeStr) = 0) then
            Temp := Temp + StringOfChar(' ', fCaretX - Length(Temp));
          Lines.Delete(ChangeEndPos.y);
        end;
        CaretXY := ChangeStartPos;
        LineText := TrimRight(Temp + ChangeStr);
        StrDispose(ChangeStr);
        for i := 0 to Marks.Count - 1 do
          if Marks[i].Line > CaretY then
            Marks[i].Line := Marks[i].Line - 1;

      end;
  end;
  SelectionMode := OldSelMode;
  DecPaintLock;
end;

procedure TmwCustomEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) then begin
    FMarkList.Remove(fBookMarks[Bookmark]);
    fBookMarks[BookMark].Free;
    fBookMarks[BookMark] := nil;
  end
end;

procedure TmwCustomEdit.GotoBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) and
    (fBookMarks[BookMark].Line <= fLines.Count) then begin
    CaretXY := Point(fBookMarks[BookMark].Column, fBookMarks[BookMark].Line);
  end;
end;

procedure TmwCustomEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  i: Integer;
  mark: TMark;
begin
  if (BookMark in [0..9]) and (Y <= fLines.Count) then begin
    mark := TMark.Create(self);
    with mark do begin
      Line := Y;
      Column := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := true;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    if Assigned(FOnPlaceMark) then FOnPlaceMark(Self, mark);
    if (mark <> nil) and (BookMark in [0..9]) then begin
      for i := 0 to 9 do
        if assigned(fBookMarks[i]) and (fBookMarks[i].Line = Y) then
          ClearBookmark(i);
      if assigned(fBookMarks[BookMark]) then ClearBookmark(BookMark);
      fBookMarks[BookMark] := mark;
      FMarkList.Add(fBookMarks[BookMark]);
    end;
  end;
end;

procedure TmwCustomEdit.WndProc(var msg: TMessage);
// Prevent Alt-Backspace from beeping
const
  ALT_KEY_DOWN = $20000000;
begin
  if (msg.Msg = WM_SYSCHAR) and (msg.wParam = VK_BACK) and
    (msg.lParam and ALT_KEY_DOWN <> 0)
    then msg.Msg := 0
  else inherited;
end;

function TmwCustomEdit.CoorToIndex(CPos: TPoint): integer;
begin
  result := CharsInWindow * CPos.Y + CPos.X;
end;

function TmwCustomEdit.IndexToCoor(ind: integer): TPoint;
begin
  result.y := ind div CharsInWindow;
  result.x := ind - result.y * CharsInWindow;
end;

procedure TmwCustomEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  if Source = Self then begin
    Accept := True;
    //Ctrl is pressed => change cursor to indicate copy instead of move
    if (GetKeyState(VK_CONTROL) and $8000) <> 0 then DragCursor := crMultiDrag
    else DragCursor := crDrag;
    if State = dsDragLeave then //restore prev caret position
      ComputeCaret(FMouseDownX, FMouseDownY)
    else //position caret under the mouse cursor
      ComputeCaret(X, Y);
  end;
end;

procedure TmwCustomEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  CurInd, DragBegInd, DragEndInd: Integer;
  sWK: string;
  dropLine, oldLines, correction: integer;
  bChangeScroll: boolean;
begin
  if ReadOnly then exit;
  IncPaintLock;
  try
    inherited;
    if (Source = self) and (SelAvail) then begin
      ComputeCaret(X, Y);
      sWk := SelText; //text to be moved/copied
      CurInd := CoorToIndex(CaretXY);
      DragBegInd := CoorToIndex(fDragBlockBegin);
      DragEndInd := CoorToIndex(fDragBlockEnd);
      //copy or move text if drop occured out of selection
      if (CurInd < DragBegInd) or (CurInd > DragEndInd) then begin
        if (GetKeyState(VK_CONTROL) shr 31) = 0 then begin
          //if Ctrl is not down then text will be moved
          if (DragBegInd < CurInd) and (FDragBlockEnd.Y = CaretY) then
            //take into account deleted text if dropping occured on the same line
            CurInd := CurInd - (DragEndInd - DragBegInd);
          FUndoList.AddChange(mwcrDragDropDelete, fBlockBegin, fBlockEnd,
            PChar(sWk), SelectionMode);
          LockUndo;
          oldLines := Lines.Count;
          dropLine := CaretY;
          SelText := '';
          if dropLine > CaretY
            then correction := oldLines - Lines.Count
          else correction := 0;
        end
        else correction := 0;
        //move
        BlockBegin := IndexToCoor(CurInd);
        BlockBegin := Point(BlockBegin.X, BlockBegin.Y - correction);
        CurInd := CoorToIndex(BlockBegin);
        bChangeScroll := not (mweoScrollPastEol in fOptions);
        try
          if bChangeScroll then Include(fOptions, mweoScrollPastEol);
          CaretXY := BlockBegin;
          BlockEnd := BlockBegin;
          SelText := sWk;
        finally
          if bChangeScroll then Exclude(fOptions, mweoScrollPastEol);
        end;
        UnLockUndo;
        FUndoList.AddChange(mwcrDragDropInsert, IndexToCoor(CurInd), BlockEnd,
          PChar(SelText), SelectionMode);
      end;
    end;
  finally
    DecPaintLock;
    UnLockUndo;
  end;
end;

procedure TmwCustomEdit.SetRightEdge(Value: Integer);
begin
  if fRightEdge <> Value then begin
    fRightEdge := Value;
    Invalidate;
  end;
end;

procedure TmwCustomEdit.SetRightEdgeColor(Value: TColor);
var
  nX: integer;
  rcInval: TRect;
begin
  if fRightEdgeColor <> Value then begin
    fRightEdgeColor := Value;
    if HandleAllocated then begin
      nX := fTextOffset + fRightEdge * fCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, ClientHeight);
      InvalidateRect(Handle, @rcInval, FALSE);
    end;
  end;
end;

function TmwCustomEdit.GetMaxUndo: Integer;
begin
  result := fUndoList.MaxUndo;
end;

procedure TmwCustomEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then begin
    fUndoList.MaxUndo := Value;
    fRedoList.MaxUndo := Value;
  end;
end;

procedure TmwCustomEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = fHighLighter then begin
      fHighLighter := nil;
      RecalcCharExtent;
      InvalidateLines(-1, -1);
    end;
    if (fBookmarkOpt <> nil) then
      if (AComponent = fBookmarkOpt.BookmarkImages) then begin
        fBookmarkOpt.BookmarkImages := nil;
        InvalidateGutter(-1, -1);
      end;
  end;
end;

procedure TmwCustomEdit.SetHighlighter(const Value: TmwCustomHighLighter);
begin
  if Value <> fHighLighter then begin
    if Assigned(fHighlighter) then
      fHighlighter.UnhookAttrChangeEvent(HighlighterAttrChanged);
    if Assigned(Value) then begin
      Value.HookAttrChangeEvent(HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    fHighLighter := Value;
    RecalcCharExtent;
    ListScanRanges(Self);
    Invalidate;
  end;
end;

procedure TmwCustomEdit.SetBorderStyle(Value: TBorderStyle);
begin
  if fBorderStyle <> Value then begin
    fBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TmwCustomEdit.SetHideSelection(const Value: boolean);
begin
  if fHideSelection <> Value then begin
    FHideSelection := Value;
    Invalidate;
  end;
end;

procedure TmwCustomEdit.SetInsertMode(const Value: boolean);
begin
  if fInserting <> Value then begin
    fInserting := Value;
    if not (csDesigning in ComponentState) then
      // Reset the caret.
      InitializeCaret;
    StatusChanged([mwscInsertMode]);
  end;
end;

procedure TmwCustomEdit.InitializeCaret;
var
  ct: TCaretType;
  cw, ch: integer;
begin
  // CreateCaret automatically destroys the previous one, so we don't have to
  // worry about cleaning up the old one here with DestroyCaret.
  // Ideally, we will have properties that control what these two carets look like.
  if InsertMode then
    ct := FInsertCaret
  else
    ct := FOverwriteCaret;
  case ct of
    ctHorizontalLine:
      begin
        cw := fCharWidth;
        ch := 2;
        FCaretOffset := Point(0, fTextHeight - 2);
      end;
    ctHalfBlock:
      begin
        cw := fCharWidth;
        ch := (fTextHeight - 2) div 2;
        FCaretOffset := Point(0, ch);
      end;
    ctBlock:
      begin
        cw := fCharWidth;
        ch := fTextHeight - 2;
        FCaretOffset := Point(0, 0);
      end;
    else begin // ctVerticalLine
      cw := 2;
      ch := fTextHeight - 2;
      FCaretOffset := Point(-1, 0);
    end;
  end;
  Exclude(fStateFlags, mwsfCaretVisible);
  CreateCaret(Handle, 0, cw, ch);
  UpdateCaret;
end;

procedure TmwCustomEdit.SetInsertCaret(const Value: TCaretType);
begin
  if FInsertCaret <> Value then begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

procedure TmwCustomEdit.SetOverwriteCaret(const Value: TCaretType);
begin
  if FOverwriteCaret <> Value then begin
    FOverwriteCaret := Value;
    InitializeCaret;
  end;
end;

procedure TmwCustomEdit.SetMaxLeftChar(Value: integer);
begin
  Value := MinMax(Value, 1, MAX_SCROLL); // horz scrolling is only 16 bit
  if fMaxLeftChar <> Value then begin
    fMaxLeftChar := Value;
    Invalidate;
  end;
end;

procedure TmwCustomEdit.EnsureCursorPosVisible;
begin
  IncPaintLock;
  try
    // Make sure X is visible
    if fCaretX < (LeftChar - 1) then
      LeftChar := fCaretX + 1
    else if fCaretX >= (fCharsInWindow + LeftChar) then
      LeftChar := fCaretX - fCharsInWindow + 2;

    // Make sure Y is visible
    if fCaretY < (TopLine - 1) then
      TopLine := fCaretY + 1
    else if fCaretY > (TopLine + (LinesInWindow - 2)) then
      TopLine := fCaretY - (LinesInWindow - 2);
  finally
    DecPaintLock;
  end;
end;

procedure TmwCustomEdit.SetKeystrokes(const Value: TmwKeyStrokes);
begin
  if Value = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(Value);
end;

procedure TmwCustomEdit.SetDefaultKeystrokes;
begin
  FKeystrokes.ResetDefaults;
end;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call.  The client must call FreeMem on Data if it is not NIL.

function TmwCustomEdit.TranslateKeyCode(Code: word; Shift: TShiftState;
  var Data: pointer): TmwEditorCommand;
var
  i: integer;
  {$IFNDEF MWE_COMPILER_3_UP}
const
  VK_ACCEPT = $30;
  {$ENDIF}
begin
  i := KeyStrokes.FindKeycode2(fLastKey, fLastShiftState, Code, Shift);
  if i >= 0 then
    Result := KeyStrokes[i].Command
  else begin
    i := Keystrokes.FindKeycode(Code, Shift);
    if i >= 0 then
      Result := Keystrokes[i].Command
    else
      Result := ecNone;
  end;
  if (Result = ecNone) and (Code >= VK_ACCEPT) and (Code <= VK_SCROLL) then begin
    fLastKey := Code;
    fLastShiftState := Shift;
  end else begin
    fLastKey := 0;
    fLastShiftState := [];
  end;
end;

procedure TmwCustomEdit.CommandProcessor(Command: TmwEditorCommand; AChar: char;
  Data: pointer);
const
  ALPHANUMERIC = DIGIT + ALPHA_UC + ALPHA_LC;
  SEL_MODE: array[ecNormalSelect..ecLineSelect] of TSelectionMode = (
    smNormal, smColumn, smLine);
var
  CX: Integer;
  Len: Integer;
  Temp: string;
  Temp2: string;
  Helper: string;
  SpaceCount1: Integer;
  SpaceCount2: Integer;
  BackCounter: Integer;
  StartOfBlock: TPoint;
  bChangeScroll: boolean;
  moveBkm: boolean;
  WP: TPoint;
  Caret: TPoint;
  CaretNew: TPoint;
  i: integer;
  OldSelMode: TSelectionMode;
  {$IFDEF MWE_MBCSSUPPORT}
  s: string;
  {$ENDIF}
  counter: Integer;
begin
  ProcessCommand(Command, AChar, Data);
  if (Command = ecNone) or (Command >= ecUserFirst) then exit;
  IncPaintLock;
  try
    case Command of
// horizontal caret movement or selection
      ecLeft, ecSelLeft:
        MoveCaretHorz(-1, Command = ecSelLeft);
      ecRight, ecSelRight:
        MoveCaretHorz(1, Command = ecSelRight);
      ecPageLeft, ecSelPageLeft:
        MoveCaretHorz(-CharsInWindow, Command = ecSelPageLeft);
      ecPageRight, ecSelPageRight:
        MoveCaretHorz(CharsInWindow, Command = ecSelPageRight);
      ecLineStart, ecSelLineStart:
        MoveCaretAndSelection(CaretXY, Point(1, CaretY),
          Command = ecSelLineStart);
      ecLineEnd, ecSelLineEnd:
        MoveCaretAndSelection(CaretXY, Point(1 + Length(LineText), CaretY),
          Command = ecSelLineEnd);
// vertical caret movement or selection
      ecUp, ecSelUp:
        begin
          MoveCaretVert(-1, Command = ecSelUp);
          Update;
        end;
      ecDown, ecSelDown:
        begin
          MoveCaretVert(1, Command = ecSelDown);
          Update;
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown:
        begin
          counter := fLinesInWindow shr Ord(mweoHalfPageScroll in fOptions);
          if (Command in [ecPageUp, ecSelPageUp]) then counter := -counter;
          MoveCaretVert(counter, Command in [ecSelPageUp, ecSelPageDown]);
          Update;
        end;
      ecPageTop, ecSelPageTop:
        begin
          MoveCaretAndSelection(CaretXY, Point(CaretX, TopLine),
            Command = ecSelPageTop);
          Update;
        end;
      ecPageBottom, ecSelPageBottom:
        begin
          CaretNew := Point(CaretX, TopLine + LinesInWindow - 1);
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageBottom);
          Update;
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          MoveCaretAndSelection(CaretXY, Point(1, 1), Command = ecSelEditorTop);
          Update;
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew := Point(1, Lines.Count);
          if (CaretNew.Y > 0) then
            CaretNew.X := Length(Lines[CaretNew.Y - 1]) + 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorBottom);
          // this is new, like seen in the D4 editor...
          i := CaretNew.Y - LinesInWindow shr 1;
          if (TopLine < i) then TopLine := i;
          Update;
        end;
// goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then begin
          MoveCaretAndSelection(CaretXY, PPoint(Data)^, Command = ecSelGotoXY);
          Update;
        end;
// word selection
      ecWordLeft, ecSelWordLeft:
        begin
          Caret := CaretXY;
          CaretNew := PrevWordPos;
          MoveCaretAndSelection(Caret, CaretNew, Command = ecSelWordLeft);
        end;
      ecWordRight, ecSelWordRight:
        begin
          Caret := CaretXY;
          CaretNew := NextWordPos;
          MoveCaretAndSelection(Caret, CaretNew, Command = ecSelWordRight);
        end;
      ecSelectAll:
        begin
          SelectAll;
        end;
      ecDeleteLastChar:
        if not ReadOnly then begin
          if SelAvail then begin
            FUndoList.AddChange(mwcrDelete, fBlockBegin, fBlockEnd,
              PChar(SelText), SelectionMode);
            SetSelText('');
            exit;
          end;
          Temp := LineText;
          Len := Length(Temp);
          SpaceCount2 := 0;
          if Len + 1 >= CaretX then begin
            if CaretX > 1 then begin
              SpaceCount1 := LeftSpaces(Temp);
              if (Temp[CaretX - 1] <= #32) and (SpaceCount1 = fCaretX) then
              begin
                if SpaceCount1 > 0 then begin
                  BackCounter := fCaretY - 1;
                  if BackCounter < 0 then SpaceCount2 := 0;
                  while BackCounter >= 0 do begin
                    SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                    if SpaceCount2 < SpaceCount1 then break;
                    Dec(BackCounter);
                  end;
                end;
                if SpaceCount2 = SpaceCount1 then SpaceCount2 := 0;
                Helper := Copy(Temp, 1, SpaceCount1 - SpaceCount2);
                Delete(Temp, 1, SpaceCount1 - SpaceCount2);
                LineText := TrimRight(Temp);
                fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                StatusChanged([mwscCaretX]);
                FUndoList.AddChange(mwcrDelete, CaretXY, CaretXY, PChar(Helper),
                  smNormal);
              end else begin
                counter := 1;
                {$IFDEF MWE_MBCSSUPPORT}
                if (ByteType(Temp, CaretX - 2) = mbLeadByte) then Inc(counter);
                {$ENDIF}
                CaretX := CaretX - counter;
                Helper := Temp[CaretX];
                if (counter > 1) then begin
                  Helper := Helper + Temp[CaretX + 1];
                end;
                Caret := CaretXY;
                Delete(Temp, CaretX, counter);
                LineText := TrimRight(Temp);
                FUndoList.AddChange(mwcrDelete, Caret, Caret, PChar(Helper),
                  smNormal);
              end;
            end else begin
              if fCaretY > 0 then begin
                Lines.Delete(fCaretY);
                for i := 0 to Marks.count - 1 do
                  if Marks[i].Line >= CaretY then
                    Marks[i].Line := Marks[i].Line - 1;

                CaretY := CaretY - 1;
                CaretX := Length(LineText) + 1;
                Caret := CaretXY;
                LineText := LineText + TrimRight(Temp);
                FUndoList.AddChange(mwcrDelete, CaretXY, CaretXY, PChar(#13#10),
                  smNormal);
              end;
            end;
          end else begin
            FUndoList.AddChange(mwcrDelete, CaretXY, CaretXY, '',
              smNormal);
            CaretX := CaretX - 1;
          end;
        end;
      ecDeleteChar:
        if not ReadOnly then begin
          if SelAvail then begin
            FUndoList.AddChange(mwcrDeleteAfterCursor, fBlockBegin, fBlockEnd,
              PChar(SelText), SelectionMode);
            SetSelText('');
            exit;
          end;
          Temp := LineText;
          Len := Length(Temp);
          if Len >= CaretX then begin
            counter := 1;
            Helper := Temp[CaretX];
            {$IFDEF MWE_MBCSSUPPORT}
            if ByteType(Temp, CaretX) = mbLeadByte then begin
              Inc(counter);
              Helper := Helper + Temp[CaretX + 1];
            end;
            {$ENDIF}
            FUndoList.AddChange(mwcrDeleteAfterCursor, CaretXY, CaretXY,
              PChar(Helper), smNormal);
            Delete(Temp, CaretX, counter);
            LineText := TrimRight(Temp);
          end else begin
            if fCaretY < Lines.Count - 1 then begin
              Helper := StringOfChar(' ', fCaretX - Len);
              LineText := TrimRight(Temp + Helper + Lines[CaretY]);

              FUndoList.AddChange(mwcrDeleteAfterCursor, CaretXY, CaretXY,
                #13#10, smNormal);

              Lines.Delete(CaretY);

              for i := Marks.Count - 1 downto 0 do
                if Marks[i].Line >= CaretY then
                  Marks[i].Line := Marks[i].Line - 1;
            end;
          end;
        end;
      ecDeleteWord:
        if not ReadOnly then begin
          WP := NextWordPos;
          if (CaretX <> WP.x) or (CaretY <> WP.y) then begin
            OldSelMode := fSelectionMode;
            try
              fSelectionMode := smNormal;
              SetBlockBegin(CaretXY);
              SetBlockEnd(WP);
              FUndoList.AddChange(mwcrDeleteAfterCursor, CaretXY, CaretXY,
                PChar(SelText), smNormal);
              SetSelText('');
            finally
              fSelectionMode := OldSelMode;
            end;
            CaretXY := CaretXY; // Fix up cursor position
          end;
        end;
      ecDeleteLastWord:
        if not ReadOnly then begin
          WP := PrevWordPos;
          if (CaretX <> WP.x) or (CaretY <> WP.y) then begin
            OldSelMode := fSelectionMode;
            try
              fSelectionMode := smNormal;
              SetBlockBegin(WP);
              SetBlockEnd(CaretXY);
              FUndoList.AddChange(mwcrDeleteAfterCursor, WP, CaretXY,
                PChar(SelText), smNormal);
              SetSelText('');
            finally
              fSelectionMode := OldSelMode;
            end;
            CaretXY := CaretXY; // Fix up cursor position
          end;
        end;
      ecDeleteBOL:
        if not ReadOnly then begin
          FUndoList.AddChange(mwcrDelete, Point(0, CaretY), Point(0, CaretY),
            PChar(Copy(LineText, 1, CaretX - 1)), smNormal);
          OldSelMode := fSelectionMode;
          try
            fSelectionMode := smNormal;
            SetBlockBegin(Point(1, CaretY));
            SetBlockEnd(CaretXY);
            SetSelText('');
          finally
            fSelectionMode := OldSelMode;
          end;
          CaretXY := Point(1, CaretY); // Fix up cursor position
        end;
      ecDeleteEOL:
        if not ReadOnly then begin
          FUndoList.AddChange(mwcrDeleteAfterCursor, CaretXY, CaretXY,
            PChar(Copy(LineText, CaretX, Length(LineText) - CaretX + 1)),
              smNormal);
          OldSelMode := fSelectionMode;
          try
            fSelectionMode := smNormal;
            SetBlockBegin(CaretXY);
            SetBlockEnd(Point(Length(LineText) + 1, CaretY));
            SetSelText('');
          finally
            fSelectionMode := OldSelMode;
          end;
          CaretXY := CaretXY; // Fix up cursor position
        end;
      ecDeleteLine:
        if not ReadOnly then begin
          if SelAvail then
            SetBlockBegin(CaretXY);
          FUndoList.AddChange(mwcrDeleteAfterCursor, Point(1, CaretY), CaretXY,
            PChar(LineText + #13#10), smNormal);
          Lines.Delete(CaretY - 1);
          CaretXY := CaretXY; // Fix up cursor position
        end;
      ecClearAll:
        begin
          if not ReadOnly then ClearAll;
        end;
      ecInsertLine,
      ecLineBreak:
        begin
          if ReadOnly then exit;
          if SelAvail then begin
            FUndoList.AddChange(mwcrDelete, fBlockBegin, fBlockEnd,
              PChar(SelText), SelectionMode);
            SetSelText('');
          end;
          SpaceCount2 := 0;
          Temp := LineText;
          Temp2 := LineText;
          Len := Length(Temp);
          if Len > 0 then begin
            if Len >= CaretX then begin
              if CaretX > 1 then begin
                SpaceCount1 := LeftSpaces(Temp);
                if fCaretX <= SpaceCount1 then begin
                  Lines[fCaretY] := '';
                  Lines.Insert(CaretY, Temp);
                  FUndoList.AddChange(mwcrLineBreak, Point(CaretX, CaretY - 1),
                    CaretXY, PChar(Temp), smNormal);
                  if Command = ecLineBreak then
                    CaretY := CaretY + 1;
                end else begin
                  Temp := Copy(LineText, 1, fCaretX);
                  LineText := TrimRight(Temp);
                  Helper := StringOfChar(' ', SpaceCount1);
                  Delete(Temp2, 1, fCaretX);
                  FUndoList.AddChange(mwcrLineBreak, CaretXY, CaretXY,
                    PChar(Temp2), smNormal);
                  Temp2 := Helper + Temp2;
                  Lines.Insert(CaretY, Temp2);
                  if Command = ecLineBreak then
                    CaretXY := Point(SpaceCount1 + 1, CaretY + 1);
                end;
              end else begin
                BackCounter := fCaretY - 1;
                while BackCounter >= 0 do begin
                  if Length(Lines[BackCounter]) > 0 then break;
                  dec(BackCounter);
                end;
                Lines[fCaretY] := '';
                Lines.Insert(CaretY, Temp);
                FUndoList.AddChange(mwcrLineBreak, CaretXY, CaretXY,
                  PChar(Temp2), smNormal);
                if Command = ecLineBreak then
                  CaretY := CaretY + 1;
              end;
            end else begin
              BackCounter := fCaretY;
              while BackCounter >= 0 do begin
                SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                if Length(Lines[BackCounter]) > 0 then break;
                dec(BackCounter);
              end;
              FUndoList.AddChange(mwcrLineBreak, CaretXY, CaretXY, nil,
                smNormal);
              if Command = ecLineBreak then
                CaretX := SpaceCount2 + 1;
              if (Command = ecInsertLine) or (mweoScrollPastEol in fOptions)
                then
                Lines.Insert(CaretY, '');
              if Command = ecLineBreak then begin
                fCaretY := fCaretY + 1;
                StatusChanged([mwscCaretY]);
              end;
              if (Command = ecLineBreak) and
                (fOptions * [mweoAutoIndent, mweoScrollPastEol] =
                  [mweoAutoIndent])
                then begin
                Lines.Insert(fCaretY, StringOfChar(' ', SpaceCount2));
                CaretX := SpaceCount2 + 1;
              end;
            end;
          end else begin
            BackCounter := fCaretY;
            while BackCounter >= 0 do begin
              SpaceCount2 := LeftSpaces(Lines[BackCounter]);
              if Length(Lines[BackCounter]) > 0 then break;
              dec(BackCounter);
            end;
            FUndoList.AddChange(mwcrLineBreak, CaretXY, CaretXY, nil, smNormal);
            if Command = ecLineBreak then
              CaretX := SpaceCount2 + 1;
            Lines.Insert(fCaretY, '');
            if Command = ecLineBreak then
              CaretY := CaretY + 1;
          end;
          for i := 0 to Marks.count - 1 do
            if Marks[i].Line >= CaretY then
              Marks[i].Line := Marks[i].Line + 1;
        end;
      ecChar:
        if not ReadOnly then begin
// here should be information available if the Shift key is held down...
// GetKeyState will NOT do the trick if we ever come to have a macro recorder
          if (AChar = #9) then DoTabKey
          else if (AChar >= #32) and (AChar <> #127) then begin
            if SelAvail then begin
              FUndoList.AddChange(mwcrSelDelete, fBlockBegin, fBlockEnd,
                PChar(SelText),
                SelectionMode);
              if SelectionMode = smLine then
                StartOfBlock := Point(1, BlockBegin.Y)
              else
                StartOfBlock := BlockBegin;
              SetSelText(AChar);
              FUndoList.AddChange(mwcrInsert, StartOfBlock, fBlockEnd, nil,
                smNormal);
            end else begin
              Temp := LineText;
              Len := Length(Temp);
              if Len < CaretX then
                Temp := Temp + StringOfChar(' ', CaretX - Len);
              bChangeScroll := not (mweoScrollPastEol in fOptions);
              try
                if bChangeScroll then Include(fOptions, mweoScrollPastEol);
                StartOfBlock := CaretXY;
                if fInserting then begin
                  Insert(AChar, Temp, CaretX);
                  CaretX := CaretX + 1;
                  LineText := TrimRight(Temp);
                  FUndoList.AddChange(mwcrInsert, StartOfBlock, CaretXY, nil,
                    smNormal);
                end else begin
// Processing of case character covers on LeadByte.
                  counter := 1;
                  {$IFDEF MWE_MBCSSUPPORT}
                  if (ByteType(Temp, CaretX) = mbLeadByte) then begin
                    Inc(counter);
                  end;
                  {$ENDIF}
                  Helper := Copy(Temp, CaretX, counter);
                  Temp[CaretX] := AChar;
                  {$IFDEF MWE_MBCSSUPPORT}
                  if (counter > 1) then begin
                    Temp[CaretX + 1] := ' ';
                  end;
                  {$ENDIF}
                  CaretNew := Point((CaretX + counter), CaretY);
                  LineText := TrimRight(Temp);
                  FUndoList.AddChange(mwcrInsert, StartOfBlock, CaretNew,
                    PChar(Helper), smNormal);
                  CaretX := CaretX + 1;
                end;
                if CaretX >= LeftChar + fCharsInWindow then
                  LeftChar := LeftChar + min(25, fCharsInWindow - 1);
              finally
                if bChangeScroll then Exclude(fOptions, mweoScrollPastEol);
              end;
            end;
          end;
        end;
      ecUndo:
        begin
          if not ReadOnly then Undo;
        end;
      ecRedo:
        begin
          if not ReadOnly then Redo;
        end;
      ecGotoMarker0..ecGotoMarker9:
        begin
          if BookMarkOptions.EnableKeys then
            GotoBookMark(Command - ecGotoMarker0);
        end;
      ecSetMarker0..ecSetMarker9:
        begin
          if BookMarkOptions.EnableKeys then begin
            CX := Command - ecSetMarker0;
            if assigned(fBookMarks[CX]) then begin
              moveBkm := (fBookMarks[CX].Line <> CaretY);
              ClearBookMark(CX);
              if moveBkm then
                SetBookMark(CX, CaretX, CaretY);
            end else
              SetBookMark(CX, CaretX, CaretY);
          end; // if BookMarkOptions.EnableKeys
        end;
      ecCut:
        begin
          if (not ReadOnly) and SelAvail then
            CutToClipboard;
        end;
      ecCopy:
        begin
          CopyToClipboard;
        end;
      ecPaste:
        begin
          if not ReadOnly then PasteFromClipboard;
        end;
      ecScrollUp:
        begin
          TopLine := TopLine - 1;
          if CaretY > TopLine + LinesInWindow - 1 then
            CaretY := TopLine + LinesInWindow - 1;
          Update;
        end;
      ecScrollDown:
        begin
          TopLine := TopLine + 1;
          if CaretY < TopLine then
            CaretY := TopLine;
          Update;
        end;
      ecScrollLeft:
        begin
          LeftChar := LeftChar - 1;
          if CaretX > LeftChar + CharsInWindow then
            CaretX := LeftChar + CharsInWindow;
          Update;
        end;
      ecScrollRight:
        begin
          LeftChar := LeftChar + 1;
          if CaretX < LeftChar then
            CaretX := LeftChar;
          Update;
        end;
      ecInsertMode:
        begin
          InsertMode := TRUE;
        end;
      ecOverwriteMode:
        begin
          InsertMode := FALSE;
        end;
      ecToggleMode:
        begin
          InsertMode := not InsertMode;
        end;
(*    // will be in version 0.91
      ecBlockIndent:
        begin
        end;
      ecBlockUnindent:
        begin
        end;
*)
      ecNormalSelect,
      ecColumnSelect,
      ecLineSelect:
        begin
          SelectionMode := SEL_MODE[Command];
        end;
      {$IFDEF MWE_MBCSSUPPORT}
      ecImeStr:
        if not ReadOnly then begin
          SetString(s, PChar(Data), StrLen(Data));
          if SelAvail then begin
            FUndoList.AddChange(mwcrDelete, fBlockBegin, fBlockEnd,
              PChar(Helper), smNormal);
            StartOfBlock := fBlockBegin;
            SetSelText(s);
            FUndoList.AddChange(mwcrInsert, fBlockBegin, fBlockEnd,
              PChar(Helper), smNormal);
          end else begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
              Temp := Temp + StringOfChar(' ', CaretX - Len);
            bChangeScroll := not (mweoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, mweoScrollPastEol);
              StartOfBlock := CaretXY;
// Processing of case character covers on LeadByte.
              Len := Length(s);
              if not fInserting then begin
                i := (CaretX + Len);
                if (ByteType(Temp, i) = mbTrailByte) then begin
                  s := s + Temp[i - 1];
                  Helper := Copy(Temp, CaretX, Len - 1);
                end else
                  Helper := Copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(s, Temp, CaretX);
              CaretX := (CaretX + Len);
              LineText := TrimRight(Temp);
              if fInserting then
                FUndoList.AddChange(mwcrInsert, StartOfBlock, CaretXY,
                  nil, smNormal)
              else
                FUndoList.AddChange(mwcrInsert, StartOfBlock, CaretXY,
                  PChar(Helper), smNormal);
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, mweoScrollPastEol);
            end;
          end;
        end;
      {$ENDIF}
    end;
  finally
    DecPaintLock;
  end;

  // Add to macro recorder stuff here?  Can't without rewriting several "exit"
  // statements above, or putting it in the finally section.

  if assigned(fOnCommandDone) then
    fOnCommandDone(Self);

end;

procedure TmwCustomEdit.ProcessCommand(var Command: TmwEditorCommand;
  var AChar: char; Data: pointer);
begin
  if Command < ecUserFirst then begin
    if assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end else begin
    if assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

procedure TmwCustomEdit.ClearAll;
begin
  Lines.Clear;
end;

function TmwCustomEdit.NextWordPos: TPoint;
var
  CX, CY, LineLen: integer;
  Line: string;
  IdentChars, WhiteChars: TIdentChars;
begin
  CX := CaretX;
  CY := CaretY;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    if Assigned(Highlighter) then
      IdentChars := Highlighter.IdentChars
    else
      IdentChars := [#33..#255];
    WhiteChars := [#1..#255] - IdentChars;
    LineLen := Length(Line);
    if CX >= LineLen then begin
      // find first IdentChar in the next line
      if CY < Lines.Count then begin
        Line := Lines[CY];
        Inc(CY);
        CX := Max(1, StrScanForCharInSet(Line, 1, IdentChars));
      end;
    end else begin
      // find first "whitespace" if next char is an IdentChar
      if Line[CX] in IdentChars then
        CX := StrScanForCharInSet(Line, CX, WhiteChars);
      // if "whitespace" found find the first IdentChar behind
      if CX > 0 then
        CX := StrScanForCharInSet(Line, CX, IdentChars);
      // if one of those failed just position at the end of the line
      if CX = 0 then
        CX := LineLen + 1;
    end;
  end;
  Result := Point(CX, CY);
end;

function TmwCustomEdit.PrevWordPos: TPoint;
var
  CX, CY: integer;
  Line: string;
  IdentChars, WhiteChars: TIdentChars;
begin
  CX := CaretX;
  CY := CaretY;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);
    if Assigned(Highlighter) then
      IdentChars := Highlighter.IdentChars
    else
      IdentChars := [#33..#255];
    WhiteChars := [#1..#255] - IdentChars;
    if CX <= 1 then begin
      // find last IdentChar in the previous line
      if CY > 1 then begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
      end;
    end else begin
      // if previous char is a "whitespace" search for the last IdentChar
      if Line[CX - 1] in WhiteChars then
        CX := StrRScanForCharInSet(Line, CX - 1, IdentChars);
      if CX > 0 then
        // search for the first IdentChar of this "word"
        CX := StrRScanForCharInSet(Line, CX - 1, WhiteChars) + 1
      else
        // just position at the end of the previous line
        if CY > 1 then begin
          Dec(CY);
          Line := Lines[CY - 1];
          CX := Length(Line) + 1;
        end;
    end;
  end;
  Result := Point(CX, CY);
end;

procedure TmwCustomEdit.SetSelectionMode(const Value: TSelectionMode);
begin
  if FSelectionMode <> Value then begin
    FSelectionMode := Value;
    if SelAvail then
      Invalidate;
  end;
end;

procedure TmwCustomEdit.BeginUpdate;
begin
  IncPaintLock;
end;

procedure TmwCustomEdit.EndUpdate;
begin
  DecPaintLock;
end;

// Pass NIL in PrintFont to use editor's current font.

procedure TmwCustomEdit.Print(PrintFont: TFont; Options: TmwPrintOptions);

var
  PageNumber: integer;
  timeStr: string;
  dateStr: string;

  function PaintLines(UseHighlighter: boolean; MarginPixels: TRect; StartLine,
    StopLine, StartCol, StopCol: integer; ColsApplyToAll: boolean): boolean;
  var
    PrinterETO: PIntArray;

    procedure PrintHeaderFooter(Strs: TStringList; XOffset: integer;
      var YOffset: integer; LineHeight, CharWidth: integer);

      function ParseScript(s: string): string;
      var
        i, j: integer;
        v: string;
      begin
        i := 1;
        Result := '';
        while i <= Length(s) do begin
          if s[i] = '$' then begin
            for j := i + 1 to Length(s) do
              if s[j] = '$' then break;
            if i + 1 = j then v := '$'
            else v := Copy(s, i + 1, j - i - 1);

            v := UpperCase(Trim(v));
            if v = 'PAGENUM' then v := IntToStr(PageNumber)
            else if v = 'TITLE' then v := Options.Title
            else if v = 'TIME' then v := timeStr
            else if v = 'DATE' then v := dateStr
            else if v = 'DATETIME' then v := dateStr + ' ' + timeStr
            else if v = 'TIMEDATE' then v := timeStr + ' ' + dateStr;

            Result := Result + v;
            i := j + 1;
          end
          else begin
            Result := Result + s[i];
            Inc(i);
          end;
        end;
      end;

    var
      x: integer;
      TextRect: TRect;

      parse: string;
      al: TmwHeaderFooterAlign;
      xoffs: integer;
      w: integer;

    begin
      if (Strs <> nil) then begin
        if (Strs.Count > 0) then begin
          for x := 0 to Strs.Count - 1 do begin
          // right margin!!!
            TextRect := Rect(0, YOffset, Printer.PageWidth - MarginPixels.Right,
              YOffset + LineHeight);

            parse := ParseScript(Strs[x]);
            w := Printer.Canvas.TextWidth(parse);
            al := TmwHeaderFooterAlign(Strs.Objects[x]);
            xoffs := XOffset;
            case al of
              hfaRight: Inc(xoffs, TextRect.Right - XOffset - w);
              hfaCenter: Inc(xoffs, (TextRect.Right - XOffset - w) div 2);
            end;
            ExtTextOut(Printer.Canvas.Handle, xoffs, YOffset, ETO_CLIPPED,
              @TextRect, PChar(parse), Length(parse), @PrinterETO^[0]);
            inc(YOffset, LineHeight);
          end; // for x:=0
        end; // if (Strs.count >0)
      end; // if (Strs <> nil)
    end;

  var
    StartIndex, StopIndex, XOffset, YOffset, LineIndex, LineLen, x: integer;
    PrinterCharWidth, PrinterTextHeight: integer;
    Metrics: TTextMetric;
    SaveFontStyle: TFontStyles;
    TokenPos, TokenIndex, TokenLen: integer;
    Token: string;
    TextRect: TRect;
    PrinterBackground: TColor;
    PrinterFont: TFont;
    Abort: boolean;
    attr: TmwHighLightAttributes;
  begin
    Result := FALSE;
    PageNumber := 1;
    if UseHighlighter then begin
      SaveFontStyle := Printer.Canvas.Font.Style;
      try
        Printer.Canvas.Font.Style := Printer.Canvas.Font.Style +
          [fsBold, fsItalic];
        GetTextMetrics(Printer.Canvas.Handle, Metrics);
      finally
        Printer.Canvas.Font.Style := SaveFontStyle;
      end;
    end else begin
      GetTextMetrics(Printer.Canvas.Handle, Metrics);
    end;
    with Metrics do begin
      // Note:  Through trial-and-error I found that tmAveCharWidth should be
      // used instead of tmMaxCharWidth as you would think.  I'm basing this
      // behavior on the Delphi IDE editor behavior.  If tmMaxCharWidth is used,
      // we end up with chars being much farther apart than the same font in the
      // IDE.
      PrinterCharWidth := tmAveCharWidth;
      PrinterTextHeight := tmHeight + tmExternalLeading + fExtraLineSpacing;
    end;
    PrinterBackground := Printer.Canvas.Brush.Color;
    PrinterFont := TFont.Create;
    PrinterFont.Assign(Printer.Canvas.Font);
    PrinterETO := GetIntArray(MaxLeftChar, PrinterCharWidth);
    try
      YOffset := MarginPixels.Top;
      Abort := FALSE;
      PrintStatus(psNewPage, PageNumber, Abort);
      if Abort then begin
        Printer.Abort;
        exit;
      end;
      // Print initial header, if any
      PrintHeaderFooter(Options.Header, MarginPixels.Left, YOffset,
        PrinterTextHeight, PrinterCharWidth);
      if UseHighlighter then begin
        // Set up the highlighter
        fHighLighter.SetRange(Lines.Objects[StartLine - 1]);
      end;
      // Loop through all the lines requested.
      for LineIndex := StartLine - 1 to StopLine - 1 do begin
        LineLen := Length(Lines[LineIndex]);

        if ColsApplyToAll or ((LineIndex = StartLine - 1) and (StartCol > 0))
          then
          StartIndex := StartCol
        else
          StartIndex := 1;
// word wrap
        if ColsApplyToAll or ((LineIndex = StopLine - 1) and (StopCol > 0)) then
          StopIndex := Min(LineLen, StopCol)
        else
          StopIndex := LineLen;

        if StartIndex <= LineLen then begin
          if UseHighlighter then begin
            fHighLighter.SetLine(Lines[LineIndex], LineIndex);

            XOffset := MarginPixels.Left;
            if ColsApplyToAll or (StartIndex = 1) then
              Dec(XOffset, ((StartIndex - 1) * PrinterCharWidth));
            // Repeat until we get to the end of the line.
            while not fHighLighter.GetEol do begin
              // Get the current token (string) to be painted.
              Token := fHighLighter.GetToken;
              // Get it's index in the line string
              TokenPos := fHighLighter.GetTokenPos;
              TokenLen := Length(Token);
              // If this token comes entirely before the left of the printing
              // area or starts to the right of it, we don't need to do anything
              // with it.
              if (TokenPos + TokenLen >= StartIndex) and
                (TokenPos < StopIndex) then begin
                // Only use background color if different from editor's normal
                // background color.  Not everyone's clWindow color is white...
                // The Next method of the highlighters does not set the
                // canvas properties any longer, so we have to do it here:
                attr := fHighlighter.GetTokenAttribute;
                if Assigned(attr) then begin
                  Printer.Canvas.Font.Style := attr.Style;
                  // Should the printout use colors?
                  if not Options.IgnoreColors then begin
                    Printer.Canvas.Font.Color := attr.Foreground;
                    // Only use background color if different from editor's
                    // normal background color.  Not everyone's clWindow color
                    // is white...
                    if attr.Background = Color then
                      Printer.Canvas.Brush.Color := PrinterBackground
                    else
                      Printer.Canvas.Brush.Color := attr.Background;
                  end;
                end;
                if (StartIndex > TokenPos + 1) and
                  (StartIndex < TokenPos + TokenLen + 1) then begin
                  TokenIndex := StartIndex - TokenPos;
                  Inc(TokenPos, TokenIndex - 1);
                  TokenLen := Min(TokenLen - (TokenIndex - 1),
                    StopIndex - StartIndex + 1);
                end else if (StopIndex >= TokenPos + 1) and
                  (StopIndex < TokenPos + TokenLen + 1) then begin
                  TokenIndex := 1;
                  TokenLen := StopIndex - TokenPos;
                end else
                  TokenIndex := 1;
                TextRect := Rect(0, YOffset, Printer.PageWidth -
                  MarginPixels.Right,
                  YOffset + PrinterTextHeight);
                // Paint the token string
                ExtTextOut(Printer.Canvas.Handle, XOffset + TokenPos *
                  PrinterCharWidth, YOffset, ETO_CLIPPED, @TextRect,
                  @Token[TokenIndex], TokenLen, @PrinterETO[0]);
              end;
              // Tell highlighter to find the next token. When it does, it will
              // set the correct colors and font style for that token.
              fHighLighter.Next;
            end;
          end else begin
            // Plain text, no highlighting
            if ColsApplyToAll or (StartIndex = 1) then
              XOffset := MarginPixels.Left
            else
              XOffset := MarginPixels.Left + PrinterCharWidth * (StartIndex -
                1);
            // Don't bother if there isn't anything to draw.
            if StopIndex >= StartIndex then begin
              TextRect := Rect(0, YOffset, Printer.PageWidth -
                MarginPixels.Right,
                YOffset + PrinterTextHeight);
              ExtTextOut(Printer.Canvas.Handle, XOffset, YOffset, ETO_CLIPPED,
                @TextRect, @Lines[LineIndex][StartIndex],
                StopIndex - StartIndex + 1, @PrinterETO[0]);
            end;
          end;
        end;
        inc(YOffset, PrinterTextHeight);
        // Will an entire line fit horizontally in space remaining?  First,
        // figure out where the bottom of the next line would be printed
        x := YOffset + PrinterTextHeight;
        // Add space for any footer lines
        if (Options.Footer <> nil) then
          inc(x, Options.Footer.Count * PrinterTextHeight);
        // Will it all fit on the page?
        if x > (Printer.PageHeight - MarginPixels.Bottom) then begin
          // Highlighter may have changed the font/color.  Reset them.
          if UseHighlighter then begin
            Printer.Canvas.Font := PrinterFont;
            Printer.Canvas.Brush.Color := PrinterBackground;
          end;
          // Nope, finish up this page and start the next.
          if (Options.Footer <> nil) then begin
            YOffset := Printer.PageHeight - MarginPixels.Bottom -
              (Options.Footer.Count * PrinterTextHeight);
            PrintHeaderFooter(Options.Footer, MarginPixels.Left, YOffset,
              PrinterTextHeight, PrinterCharWidth);
          end;
          Printer.NewPage;

          inc(PageNumber);
          PrintStatus(psNewPage, PageNumber, Abort);
          if Abort then begin
            Printer.Abort;
            exit;
          end;

          YOffset := MarginPixels.Top;
          PrintHeaderFooter(Options.Header, MarginPixels.Left, YOffset,
            PrinterTextHeight, PrinterCharWidth);
        end;
      end;
      // Print final footer, if any
      if (Options.Footer <> nil) then begin
        // Highlighter may have changed the font/color.  Reset them.
        if UseHighlighter then begin
          Printer.Canvas.Font := PrinterFont;
          Printer.Canvas.Brush.Color := PrinterBackground;
        end;
        YOffset := Printer.PageHeight - MarginPixels.Bottom -
          (Options.Footer.Count * PrinterTextHeight);
        PrintHeaderFooter(Options.Footer, MarginPixels.Left, YOffset,
          PrinterTextHeight, PrinterCharWidth);
      end;
    finally
      FreeMem(PrinterETO);
      PrinterFont.Free;
    end;
    Result := TRUE;
  end;

  function MarginsInPixels(MarginUnits: TmwMarginUnits;
    const Margins: TRect): TRect;
  var
    XPix, YPix: integer;
    XOffset, YOffset: integer;
  begin
    // Pixels per inch
    XPix := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX);
    YPix := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX);
    // I use MulDiv because it's faster/safer/etc that doing the floating point
    // math directly.  When you see MulDiv, you can translate this way:
    //   R := MulDiv(X, Y, Z);
    //       is
    //   R := Round((X * Y) / Z)
    // MulDiv is safer because Delphi would truncate (X * Y) to a 32-bit value
    // before dividing it by Z.  This often results in very hard to find bugs.
    // For example, if x and y were 90,000 and z were 10, using the direct
    // method you would get a value of -48,993,459 instead of the 810,000,000
    // value you would expect.  90,000 multiplied by 90,000 results in a value
    // that won't fit into a 32-bit value, so it get's "lopped off" at 32-bits.
    // This is documented behavior, so it is not a bug in Delphi.
    // MulDiv stores the multiplication result in a 64-bit value, so it doesn't
    // have this behavior.
    // In practice, we aren't likely to exceed a 32-bit value in our
    // multiplications here, but it's a very good habit to get into.  Would
    // *you* want to figure out what went wrong given the above description?
    // I wouldn't.
    case MarginUnits of
      muThousandthsOfInches:
        begin
          Result.Left := MulDiv(Margins.Left, XPix, 1000);
          Result.Right := MulDiv(Margins.Right, XPix, 1000);
          Result.Top := MulDiv(Margins.Top, YPix, 1000);
          Result.Bottom := MulDiv(Margins.Bottom, YPix, 1000);
        end;
      muMillimeters:
        begin
          // Same as inches, but divide by 0.0254 (25.4 mm per inch)
          Result.Left := MulDiv(MulDiv(Margins.Left, XPix, 1000), 10000, 254);
          Result.Right := MulDiv(MulDiv(Margins.Right, XPix, 1000), 10000, 254);
          Result.Top := MulDiv(MulDiv(Margins.Top, YPix, 1000), 10000, 254);
          Result.Bottom := MulDiv(MulDiv(Margins.Bottom, YPix, 1000), 10000,
            254);
        end;
    else // muPixels
      Result := Margins;
    end;
    // Account for printer limitations.  All printers have an area surrounding
    // the paper that physical can not be printed on.  Delphi's TPrinter
    // object starts at that area when you print at 0,0 on it.  So, we have to
    // subtract the size of this non-printable area out of the margins so that
    // it accounts for it.
    XOffset := GetDeviceCaps(Printer.Canvas.Handle, PHYSICALOFFSETX);
    YOffset := GetDeviceCaps(Printer.Canvas.Handle, PHYSICALOFFSETY);
    Dec(Result.Left, XOffset);
    Dec(Result.Right, XOffset);
    Dec(Result.Top, YOffset);
    Dec(Result.Bottom, YOffset);
    Result.Left := Max(Result.Left, 0);
    Result.Right := Max(Result.Right, 0);
    Result.Top := Max(Result.Top, 0);
    Result.Bottom := Max(Result.Bottom, 0);
  end;
var
  PrintArea: TRect;
  Abort: boolean;
begin
  if Options.SelectedOnly and not SelAvail then exit;

  datestr := DateToStr(Date);
  timestr := TimeToStr(Time);

  if PrintFont = nil then
    Printer.Canvas.Font := Font
  else
    Printer.Canvas.Font := PrintFont;

  Printer.Copies := Options.Copies;
  Printer.Title := Options.Title;
  Printer.BeginDoc;
  try
    Abort := FALSE;
    PrintStatus(psBegin, 0, Abort);
    if Abort then begin
      Printer.Abort;
      exit;
    end;
    if Options.SelectedOnly then begin
      if SelectionMode = smLine then
        PrintArea := Rect(0, BlockBegin.Y, 0, BlockEnd.Y)
      else
        PrintArea := Rect(BlockBegin.X, BlockBegin.Y, BlockEnd.X - 1,
          BlockEnd.Y);
    end else if not IsRectEmpty(Options.PrintRange) then
      PrintArea := Options.PrintRange
    else
      // Paint all the text
      PrintArea := Rect(0, 1, 0, Lines.Count);
    Abort := not PaintLines(Options.Highlighted,
      MarginsInPixels(Options.MarginUnits, Options.Margins), PrintArea.Top,
      PrintArea.Bottom, PrintArea.Left, PrintArea.Right,
      Options.SelectedOnly and (SelectionMode = smColumn));
  finally
    if not Abort then
      Printer.EndDoc;
    PrintStatus(psEnd, 0, Abort);
    // We're done, we don't care about abort at this point.
  end;
end;

procedure TmwCustomEdit.PrintStatus(Status: TmwPrintStatus; PageNumber: integer;
  var Abort: boolean);
begin
  if Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, Status, PageNumber, Abort);
end;

procedure TmwCustomEdit.ReadState(Reader: TReader);
begin
  TmwEditList(fLines).BeginLoading;
  try
    inherited ReadState(Reader);
  finally TmwEditList(fLines).EndLoading; end;
end;

procedure TmwCustomEdit.AddKey(Command: TmwEditorCommand;
  Key1: word; SS1: TShiftState; Key2: word; SS2: TShiftState);
var
  Key: TmwKeyStroke;
begin
  Key := Keystrokes.Add;
  Key.Command := Command;
  Key.Key := Key1;
  Key.Shift := SS1;
  Key.Key2 := Key2;
  Key.Shift2 := SS2;
end;

{ Called by FMarkList if change }
procedure TmwCustomEdit.MarkListChange(Sender: TObject);
begin
  InvalidateGutter(-1, -1);
end;

function TmwCustomEdit.GetSelStart: integer;

function llen(data: string): integer;
  begin
    result := length(Data) + 2;
  end;
var
  loop: integer;
  x, y: integer;
begin
  x := BlockBegin.X;
  y := BlockBegin.Y;

  result := 0;
  loop := 0;
  while loop < (Y - 1) do begin
    Result := result + llen(lines.strings[loop]);
    inc(loop);
  end;

  result := result + Min(X, llen(lines.strings[loop]));
end;

procedure TmwCustomEdit.SetSelStart(const Value: integer);

function llen(data: string): integer;
  begin
    result := length(Data) + 2;
  end;
var
  loop: integer;
  count: integer;
begin
  loop := 0;
  count := 0;
  while (count + llen(lines.strings[loop]) < value) do begin
    count := count + llen(lines.strings[loop]);
    inc(loop);
  end;
  CaretX := value - count;
  CaretY := loop + 1;

  fBlockBegin.X := CaretX;
  fBlockBegin.Y := CaretY;
end;

function TmwCustomEdit.GetSelEnd: integer;

function llen(data: string): integer;
  begin
    result := length(Data) + 2;
  end;
var
  loop: integer;
  x, y: integer;
begin
  x := BlockEnd.x;
  y := BlockEnd.y;

  result := 0;
  loop := 0;
  while loop < (y - 1) do begin
    Result := result + llen(lines.strings[loop]);
    inc(loop);
  end;
  result := result + x;
end;

procedure TmwCustomEdit.SetSelEnd(const Value: integer);

function llen(data: string): integer;
  begin
    result := length(Data) + 2;
  end;
var
  p: tpoint;
  loop: integer;
  count: integer;
begin
  loop := 0;
  count := 0;
  while (count + llen(lines.strings[loop]) < value) do begin
    count := count + llen(lines.strings[loop]);
    inc(loop);
  end;
  p.X := value - count; p.y := loop + 1;
  Blockend := p;
end;

procedure TmwCustomEdit.SetSelWord;
begin
  SetWordBlock(CaretXY);
end;

procedure TmwCustomEdit.SetExtraLineSpacing(const Value: integer);
begin
  fExtraLineSpacing := Value;
  FontChanged(self);
end;

function TmwCustomEdit.GetBookMark(BookMark: integer; var X, Y: integer):
  boolean;
var
  i: integer;
begin
  Result := false;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then begin
        X := Marks[i].Column;
        Y := Marks[i].Line;
        Result := true;
        Exit;
      end;
end;

function TmwCustomEdit.IsBookmark(BookMark: integer): boolean;
var
  x, y: integer;
begin
  Result := GetBookMark(BookMark, x, y);
end;

procedure TmwCustomEdit.ClearUndo;
begin
  fUndoList.ClearList;
  fRedoList.ClearList;
end;

procedure TmwCustomEdit.SetSelTextExternal(const Value: string);
var
  StartOfBlock: TPoint;
begin
  if SelAvail then
    FUndoList.AddChange(mwcrSelDelete, fBlockBegin, fBlockEnd,
      PChar(GetSelText),
      SelectionMode);
  StartOfBlock := fBlockBegin;
  SetSelText(Value);
  FUndoList.AddChange(mwcrInsert, StartOfBlock, fBlockBegin, PChar(GetSelText),
    SelectionMode);
end;

procedure TmwCustomEdit.RefreshAllTokens;
var
  I: Integer;
begin
  if Assigned(fHighlighter) then
    if Assigned(fHighlighter.OnToken) then
      for i := 0 to Pred(Lines.Count) do
        fHighlighter.ScanAllLineTokens(Lines[i], i);
end;

procedure TmwCustomEdit.SetGutter(const Value: TmwGutter);
begin
  fGutter.Assign(Value);
end;

procedure TmwCustomEdit.GutterChanged(Sender: TObject);
var
  nW: integer;
begin
  if not (csLoading in ComponentState) then begin
    nW := fGutter.RealGutterWidth(fCharWidth);
    if nW = fGutterWidth then InvalidateGutter(-1, -1)
    else SetGutterWidth(nW);
  end;
end;

procedure TmwCustomEdit.LockUndo;
begin
  FUndoList.LockUndo;
  FRedoList.LockUndo;
end;

procedure TmwCustomEdit.UnLockUndo;
begin
  FUndoList.UnLockUndo;
  FRedoList.UnLockUndo;
end;

procedure TmwCustomEdit.WMMouseWheel(var Msg: TMessage);
var
  nDelta: integer;
  nWheelClicks: integer;
  {$IFNDEF MWE_COMPILER_4_UP}
const
  LinesToScroll = 3;
  WHEEL_DELTA = 120;
  WHEEL_PAGESCROLL = MAXDWORD;
  {$ENDIF}
begin
  if csDesigning in ComponentState then exit;

  {$IFDEF MWE_COMPILER_4_UP}
  if GetKeyState(VK_CONTROL) >= 0 then nDelta := Mouse.WheelScrollLines
    {$ELSE}
  if GetKeyState(VK_CONTROL) >= 0 then nDelta := LinesToScroll
    {$ENDIF}
    else nDelta := LinesInWindow shr Ord(mweoHalfPageScroll in fOptions);

  Inc(fMouseWheelAccumulator, SmallInt(Msg.wParamHi));
  nWheelClicks := fMouseWheelAccumulator div WHEEL_DELTA;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DELTA;
  if (nDelta = integer(WHEEL_PAGESCROLL)) or (nDelta > LinesInWindow) then
    nDelta := LinesInWindow;
  TopLine := TopLine - (nDelta * nWheelClicks);
  Update;
end;

procedure TmwCustomEdit.SetWantTabs(const Value: boolean);
begin
  fWantTabs := Value;
end;

procedure TmwCustomEdit.SetTabWidth(Value: integer);
begin
  Value := MinMax(Value, 0, 16);
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    Invalidate; // to redraw text containing tab chars
  end;
end;

procedure TmwCustomEdit.SelectionChange;
begin
  if assigned(fSelectionChange) then
    fSelectionChange(Self);
end;

function GetExporter(ExportFormat: TmwEditExporter): TmwCustomExport;
begin
  case ExportFormat of
    cfRTF: result := TmwRTFExport.Create(nil);
    cfHTML: result := TmwHtmlExport.Create(nil);
  else
    result := nil;
  end;
end;

// Adds data from a stream to the clipboard in specified format.

procedure TmwCustomEdit.SaveStreamToClipboardFormat(const ClipboardFormat: Word;
  Stream: TStream);
var
  Data: THandle;
  DataPtr: Pointer;
  Size: Integer;
begin
  Stream.Position := 0;
  Size := Stream.Size;

  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Size);
  try
    DataPtr := GlobalLock(Data);
    try
      Stream.ReadBuffer(DataPtr^, Size);
      Clipboard.SetAsHandle(ClipboardFormat, Data);
    finally
      GlobalUnlock(Data);
    end;
  except
    on E: Exception do begin
      GlobalFree(Data);
    end;
  end;
end;

procedure TmwCustomEdit.SaveToFile(const FileName: string);
begin
  Lines.SaveToFile(FileName);
end;

procedure TmwCustomEdit.ExportToFile(const FileName, ATitle: string;
  WithBackground: boolean; Format: TmwEditExporter);
begin
  with GetExporter(Format) do begin
    Title := ATitle;
    UseBackGround := WithBackground;
    RunExport(0, Lines.Count - 1, Self, Highlighter);
    SaveToFile(FileName);
    Free;
  end;
end;

procedure TmwCustomEdit.CopyToClipboardEx;
begin
  if not (SelAvail) then
    Exit;
  Clipboard.Open;
  try
    Clipboard.Clear;
    ClipBoard.SetTextBuf(PChar(SelText));
    if cfHTML in ClipBoardFormats then
      with GetExporter(cfHTML) do begin
        CopyToClipboard(Self, HighLighter);
        Free;
      end;
    if cfRTF in ClipBoardFormats then
      with GetExporter(cfRTF) do begin
        CopyToClipboard(Self, HighLighter);
        Free;
      end;
  finally
    Clipboard.Close;
  end;
end;

// Save in a specified to the clipboard as text

procedure TmwCustomEdit.ExportToClipboard(Format: TmwEditExporter);
begin
  with GetExporter(Format) do try
    Clipboard.Open;
    try
      Clipboard.Clear;
      CopyToClipboardFormat(Self, HighLighter, CF_TEXT);
    finally
      Clipboard.Close;
    end;
  finally
    Free;
  end;
end;

procedure TmwCustomEdit.SelectedColorsChanged(Sender: TObject);
begin
  Invalidate;
end;

// find / replace

function TmwCustomEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TmwSearchOptions): integer;
var
  ptStart, ptEnd: TPoint; // start and end of the search range
  ptCurrent: TPoint; // current search position
  nSearchLen, nReplaceLen, n, nFound: integer;
  nInLine: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  nAction: TmwReplaceAction;

  function InValidSearchRange(First, Last: integer): boolean;
  begin
    Result := TRUE;
    case fSelectionMode of
      smNormal:
        if ((ptCurrent.Y = ptStart.Y) and (First < ptStart.X)) or
          ((ptCurrent.Y = ptEnd.Y) and (Last > ptEnd.X)) then Result := FALSE;
      smColumn:
        Result := (First >= ptStart.X) and (Last <= ptEnd.X);
    end;
  end;

begin
  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then exit;
  // get the text range to search in, ignore the "Search in selection only"
  // option if nothing is selected
  bBackward := (mwsoBackwards in AOptions);
  bPrompt := (mwsoPrompt in AOptions);
  bReplace := (mwsoReplace in AOptions);
  bReplaceAll := (mwsoReplaceAll in AOptions);
  bFromCursor := not (mwsoEntireScope in AOptions);
  if not SelAvail then Exclude(AOptions, mwsoSelectedOnly);
  if (mwsoSelectedOnly in AOptions) then begin
    ptStart := BlockBegin;
    ptEnd := BlockEnd;
    // search the whole line in the line selection mode
    if (fSelectionMode = smLine) then begin
      ptStart.X := 1;
      ptEnd.X := Length(Lines[ptEnd.Y - 1]) + 1;
    end else if (fSelectionMode = smColumn) then
      // make sure the start column is smaller than the end column
      if (ptStart.X > ptEnd.X) then begin
        nFound := ptStart.X;
        ptStart.X := ptEnd.X;
        ptEnd.X := nFound;
      end;
    // ignore the cursor position when searching in the selection
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end else begin
    ptStart := Point(1, 1);
    ptEnd.Y := Lines.Count;
    ptEnd.X := Length(Lines[ptEnd.Y - 1]) + 1;
    if bFromCursor then
      if bBackward then ptEnd := CaretXY else ptStart := CaretXY;
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end;
  // initialize the search engine
  fTSearch.Pattern := ASearch;
  fTSearch.Sensitive := mwsoMatchCase in AOptions;
  fTSearch.Whole := mwsoWholeWord in AOptions;
  // search while the current search position is inside of the search range
  nSearchLen := Length(ASearch);
  nReplaceLen := Length(AReplace);
  if bReplaceAll then IncPaintLock;
  try
    while (ptCurrent.Y >= ptStart.Y) and (ptCurrent.Y <= ptEnd.Y) do begin
      nInLine := fTSearch.FindAll(Lines[ptCurrent.Y - 1]);
      if bBackward then n := Pred(fTSearch.ResultCount) else n := 0;
      // Operate on all results in this line.
      while nInLine > 0 do begin
        nFound := fTSearch.Results[n];
        if bBackward then Dec(n) else Inc(n);
        Dec(nInLine);
        // Is the search result entirely in the search range?
        if not InValidSearchRange(nFound, nFound + nSearchLen) then continue;
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.
        ptCurrent.X := nFound;
        BlockBegin := ptCurrent;
        if bBackward then CaretXY := ptCurrent;
        Inc(ptCurrent.X, nSearchLen);
        BlockEnd := ptCurrent;
        if not bBackward then CaretXY := ptCurrent;
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if (bPrompt) and (Assigned(fOnReplaceText)) then begin
          nAction := mwraCancel;
          fOnReplaceText(Self, ASearch, AReplace, ptCurrent.Y, nFound, nAction);
          if nAction = mwraCancel then exit;
        end else
          nAction := mwraReplace;
        if not (nAction = mwraSkip) then begin
            // user has been prompted and has requested to silently replace all
            // so turn off prompting
          if nAction = mwraReplaceAll then begin
            if not bReplaceAll then begin
              bReplaceAll := TRUE;
              IncPaintLock;
            end;
            bPrompt := False;
          end;
          SetSelTextExternal(AReplace);
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          CaretX := nFound + nReplaceLen;
          fTSearch.FixResults(nFound, nSearchLen - nReplaceLen);
        end;
        if not bReplaceAll then Exit;
      end;
      // search next / previous line
      if bBackward then Dec(ptCurrent.Y) else Inc(ptCurrent.Y);
    end;
  finally
    if bReplaceAll then DecPaintLock;
  end;
end;

{$IFDEF MWE_MBCSSUPPORT}

procedure TmwCustomEdit.MBCSGetSelRangeInLineWhenColumnSelectionMode(
  const s: string; var ColFrom, ColTo: Integer);
  // --ColFrom and ColTo are in/out parameter. their range
  //    will be from 1 to MaxInt.
  // --a range of selection means:  Copy(s, ColFrom, ColTo - ColFrom);
  //    be careful what ColTo means.
var
  Len: Integer;
begin
  Len := Length(s);
  if (0 < ColFrom) and (ColFrom <= Len) then
    if mbTrailByte = ByteType(s, ColFrom) then
      Inc(ColFrom);
  if (0 < ColTo) and (ColTo <= Len) then
    if mbTrailByte = ByteType(s, ColTo) then
      Inc(ColTo);
end;

{$ENDIF}

function TmwCustomEdit.IsPointInSelection(Value: TPoint): boolean;
var
  ptBegin, ptEnd: TPoint;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Y >= ptBegin.Y) and (Value.Y <= ptEnd.Y) and
    ((ptBegin.Y <> ptEnd.Y) or (ptBegin.X <> ptEnd.X))
    then begin
    if SelectionMode = smLine then
      Result := TRUE
    else if (SelectionMode = smColumn) then begin
      if (ptBegin.X > ptEnd.X) then
        Result := (Value.X >= ptEnd.X) and (Value.X < ptBegin.X)
      else if (ptBegin.X < ptEnd.X) then
        Result := (Value.X >= ptBegin.X) and (Value.X < ptEnd.X)
      else
        Result := FALSE;
    end else
      Result := ((Value.Y > ptBegin.Y) or (Value.X >= ptBegin.X)) and
        ((Value.Y < ptEnd.Y) or (Value.X < ptEnd.X));
  end else
    Result := FALSE;
end;

procedure TmwCustomEdit.WMSetCursor(var Msg: TWMSetCursor);
var
  ptCursor, ptLineCol: TPoint;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
  if (ptCursor.X < fGutterWidth) then
    SetCursor(Screen.Cursors[fGutter.Cursor])
  else begin
    ptLineCol.X := (LeftChar * fCharWidth + ptCursor.X - fGutterWidth - 2)
      div fCharWidth;
    ptLineCol.Y := TopLine + ptCursor.Y div fTextHeight;
    if (mweoDragDropEditing in fOptions) and IsPointInSelection(ptLineCol) then
      SetCursor(Screen.Cursors[crDefault])
    else
      inherited;
  end;
end;

procedure TmwCustomEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter(-1, -1);
end;

procedure TmwCustomEdit.SetOptions(Value: TmwEditorOptions);
var
  bSetDrag: boolean;
begin
  if (Value <> fOptions) then begin
    bSetDrag := (mweoDropFiles in fOptions) <> (mweoDropFiles in Value);
    fOptions := Value;
    // Reset column position in case Cursor is past EOL.
    if not (mweoScrollPastEol in fOptions) then CaretX := CaretX;
    // (un)register HWND as drop target
    if bSetDrag and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (mweoDropFiles in fOptions));
  end;
end;

procedure TmwCustomEdit.SetOptionFlag(Flag: TmwEditorOption; Value: boolean);
begin
  if (Value <> (Flag in fOptions)) then begin
    if Value then Include(fOptions, Flag) else Exclude(fOptions, Flag);
    if (Flag = mweoScrollPastEol) and not Value then CaretX := CaretX;
    if (Flag = mweoDropFiles) then
      if not (csDesigning in ComponentState) and HandleAllocated then
        DragAcceptFiles(Handle, Value);
  end;
end;

procedure TmwCustomEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated then begin
    fCharsInWindow := (ClientWidth - fGutterWidth - 2) div fCharWidth;
    fLinesInWindow := ClientHeight div fTextHeight;
    if bFont then begin
      if Gutter.ShowLineNumbers then GutterChanged(Self)
      else UpdateScrollbars(FALSE);
      InitializeCaret;
      Exclude(fStateFlags, mwsfCaretChanged);
      Invalidate;
    end else
      UpdateScrollbars(FALSE);
    Exclude(fStateFlags, mwsfScrollbarChanged);
  end;
end;

procedure TmwCustomEdit.MoveCaretHorz(DX: integer; SelectionCommand: boolean);
var
  ptO, ptDst: TPoint;
  s: string;
  nLineLen: integer;
  bChangeY: boolean;
begin
  ptO := CaretXY;
  ptDst := ptO;
  s := LineText;
  nLineLen := Length(s);
  // only moving or selecting one char can change the line
  bChangeY := not (mweoScrollPastEol in fOptions);
  if bChangeY and (DX = -1) and (ptO.X = 1) and (ptO.Y > 1) then begin
    // end of previous line
    Dec(ptDst.Y);
    ptDst.X := Length(Lines[ptDst.Y - 1]) + 1;
  end else
    if bChangeY and (DX = 1) and (ptO.X > nLineLen) and (ptO.Y < Lines.Count)
      then begin
    // start of next line
      Inc(ptDst.Y);
      ptDst.X := 1;
    end else begin
      ptDst.X := Max(1, ptDst.X + DX);
      // don't go past last char when ScrollPastEol option not set
      if (DX > 0) and bChangeY then ptDst.X := Min(ptDst.X, nLineLen + 1);
  {$IFDEF MWE_MBCSSUPPORT}
      // prevent from getting inside of a doublebyte char
      if (ptDst.X > 1) and (ptDst.X <= nLineLen) then begin
        DX := ptDst.X - ptO.X;
        if (DX < 0) then begin
          if ByteType(s, ptDst.X) = mbTrailByte then Dec(ptDst.X);
        end else if (DX > 0) then begin
          if ByteType(s, ptDst.X) = mbTrailByte then Inc(ptDst.X);
        end;
      end;
    end;
    fMBCSStepAside := False;
  {$ELSE}
    end;
  {$ENDIF}
  // set caret and block begin / end
  MoveCaretAndSelection(ptO, ptDst, SelectionCommand);
end;

procedure TmwCustomEdit.MoveCaretVert(DY: integer; SelectionCommand: boolean);
var
  ptO, ptDst: TPoint;
  {$IFDEF MWE_MBCSSUPPORT}
  s: string;
  {$ENDIF}
begin
  ptO := CaretXY;
  ptDst := ptO;
  with ptDst do begin
    Inc(Y, DY);
    if DY >= 0 then begin
      if (Y > Lines.Count) or (ptO.Y > Y) then
        Y := Lines.Count;
    end else
      if (Y < 1) or (ptO.Y < Y) then
        Y := 1;
  end;
  {$IFDEF MWE_MBCSSUPPORT}
  if (ptO.Y <> ptDst.Y) then begin
    if fMBCSStepAside then Inc(ptDst.X);
    fMBCSStepAside := FALSE;
    s := Lines[ptDst.Y - 1];
    if (ptDst.X <= Length(s)) then
      if (ByteType(s, ptDst.X) = mbTrailByte) then begin
        fMBCSStepAside := TRUE;
        Dec(ptDst.X);
      end;
  end;
  {$ENDIF}
  // set caret and block begin / end
  MoveCaretAndSelection(ptO, ptDst, SelectionCommand);
end;

procedure TmwCustomEdit.MoveCaretAndSelection(ptBefore, ptAfter: TPoint;
  SelectionCommand: boolean);
begin
  if SelectionCommand then begin
    if not SelAvail then SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  end else
    SetBlockBegin(ptAfter);
  CaretXY := ptAfter;
end;

procedure TmwCustomEdit.RecalcCharExtent;

  function UseBoldStyle: Boolean;
  var
    i: Integer;
  begin
    if Assigned(fHighlighter) then with fHighlighter do begin
        for i := 0 to AttrCount - 1 do
          if (fsBold in Attribute[i].Style) then begin
            Result := True;
            Exit;
          end;
        Result := False;
      end else
      Result := (fsBold in Font.Style)
  end;

const
  BaseStyles: array[Boolean] of TFontStyles = ([], [fsBold]);
begin
  with fTextDrawer do begin
    BaseFont := Self.Font;
    BaseStyle := BaseStyles[UseBoldStyle];
    fCharWidth := CharWidth;
    fTextHeight := CharHeight + fExtraLineSpacing;
  end;
end;

procedure TmwCustomEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
  invalidate;
end;

procedure TmwCustomEdit.StatusChanged(AChanges: TmwStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if (PaintLock = 0) and Assigned(fOnStatusChange) then begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TmwCustomEdit.DoTabKey;
var
  StartOfBlock: TPoint;
  i, MinLen, iLine: integer;
  PrevLine,
    Spaces: string;
  p: PChar;
begin
  i := 0;
  if mweoSmartTabs in fOptions then begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then begin
      Dec(iLine);
      MinLen := CaretX;
      repeat
// NOTE mh: after throwing in real tabs we have to use:
//      PrevLine := pConvert(Lines[iLine], TabWidth);
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MinLen) then begin
          p := @PrevLine[MinLen];
          // scan over non-whitespaces
          repeat
            if p^ = #32 then break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if p^ <> #32 then break;
              Inc(i);
              Inc(p);
            until p^ = #0;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
      if (i = 0) then exit;
    end;
  end;
  if i = 0 then begin
    i := (CaretX - 1) mod TabWidth;
    if i = 0 then i := TabWidth;
  end;
  Spaces := StringOfChar(' ', i);
  if GetSelAvail then
    FUndoList.AddChange(mwcrDelete, fBlockBegin, fBlockEnd, PChar(SelText),
      SelectionMode);
  StartOfBlock := fBlockBegin;
  SetSelText(Spaces);
  FUndoList.AddChange(mwcrInsert, StartOfBlock, fBlockBegin, PChar(GetSelText),
    SelectionMode);
end;

procedure TmwCustomEdit.CreateWnd;
begin
  inherited;
  if (mweoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, TRUE);
end;

procedure TmwCustomEdit.DestroyWnd;
begin
  if (mweoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, FALSE);
  inherited;
end;

procedure TmwCustomEdit.InvalidateLine(Line: integer);
var
  rcInval: TRect;
begin
  if Visible and (Line >= TopLine) and (Line <= TopLine + LinesInWindow) and
     (Line <= Lines.Count) and HandleAllocated
  then begin
    // we invalidate gutter and text area of this line
    rcInval := Rect(0, fTextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
    InvalidateRect(Handle, @rcInval, FALSE);
  end;
end;

{ TUndoList }

procedure TUndoList.AddChange(ChangeReason: TChangeReason; ChangeStartPos,
  ChangeEndPos: TPoint; ChangeStr: PChar; ChangeSelMode: TSelectionMode);
var
  chngptr: TChangePtr;
begin
  if fUndoLocked then Exit;
  if FList.Count >= FMaxUndo then RemoveChange(0);
  try
    GetMem(chngptr, SizeOf(TChange));
    try
      chngptr^.ChangeStr := StrNew(ChangeStr);
    except
      FreeMem(chngptr);
      exit;
    end;
  except
    exit;
  end;
  chngptr^.ChangeReason := ChangeReason;
  chngptr^.ChangeStartPos := ChangeStartPos;
  chngptr^.ChangeEndPos := ChangeEndPos;
  chngptr^.ChangeSelMode := ChangeSelMode;
  FList.Add(chngptr);
  fOwner.Modified := true;
  if Assigned(FOwner.OnChange) then FOwner.OnChange(FOwner);
end;

constructor TUndoList.Create(AOwner: TmwCustomEdit);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
  FMaxUndo := 10;
  fUndoLocked := False;
end;

destructor TUndoList.Destroy;
begin
  ClearList;
  FList.Free;
  inherited Destroy;
end;

function TUndoList.GetChange(var ChangeStartPos, ChangeEndPos: TPoint;
  var ChangeStr: PChar; var ChangeSelMode: TSelectionMode): TChangeReason;
begin
  if FList.Count = 0 then begin
    result := mwcrNone;
    exit;
  end;
  ChangeStartPos := TChangePtr(FList.Items[FList.Count - 1])^.ChangeStartPos;
  ChangeEndPos := TChangePtr(FList.Items[FList.Count - 1])^.ChangeEndPos;
  ChangeStr := StrNew(TChangePtr(FList.Items[FList.Count - 1])^.ChangeStr);
  ChangeSelMode := TChangePtr(FList.Items[FList.Count - 1])^.ChangeSelMode;
  result := TChangePtr(FList.Items[FList.Count - 1])^.ChangeReason;
  RemoveChange(FList.Count - 1);
end;

{$IFDEF UNDO_DEBUG}

function TUndoList.GetChange2(var ChangeStartPos, ChangeEndPos: TPoint;
  var ChangeStr: PChar; var ChangeSelMode: TSelectionMode; i: Integer):
    TChangeReason;
begin
  if FList.Count = 0 then begin
    result := mwcrNone;
    exit;
  end;
  ChangeStartPos := TChangePtr(FList.Items[i])^.ChangeStartPos;
  ChangeEndPos := TChangePtr(FList.Items[i])^.ChangeEndPos;
  ChangeStr := StrNew(TChangePtr(FList.Items[i])^.ChangeStr);
  ChangeSelMode := TChangePtr(FList.Items[i])^.ChangeSelMode;
  result := TChangePtr(FList.Items[i])^.ChangeReason;
   //RemoveChange(FList.Count-1);
end;
{$ENDIF}

function TUndoList.GetCanUndo: Integer;
begin
  FCanUndo := FList.Count;
  result := FCanUndo;
end;

procedure TUndoList.SetMaxUndo(const Value: Integer);
var
  i: Integer;
begin
  if Value < FList.Count - 1 then
    for i := 0 to FList.Count - Value - 1 do
      RemoveChange(0);
  FMaxUndo := Value;
end;

procedure TUndoList.RemoveChange(index: Integer);
var
  chngptr: TChangePtr;
begin
  if (index > -1) and (index < FList.Count) then begin
    chngptr := FList.Items[Index];
    try
      StrDispose(chngptr^.ChangeStr);
      try
        FreeMem(chngptr);
      except
        exit;
      end;
    except
      exit;
    end;
    Flist.Delete(index);
  end;
end;

function TUndoList.GetChangeReason: TChangeReason;
begin
  if FList.Count = 0 then
    result := mwcrNone
  else
    result := TChangePtr(FList.Items[FList.Count - 1])^.ChangeReason;
end;

procedure TUndoList.ClearList;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    RemoveChange(i);
end;

procedure TUndoList.LockUndo;
begin
  fUndoLocked := True;
end;

procedure TUndoList.UnLockUndo;
begin
  fUndoLocked := False;
end;

{ TMark }

function TMark.GetEdit: TmwCustomEdit;
begin
  if FEdit <> nil then try
    if FEdit.Marks.IndexOf(self) = -1 then
      FEdit := nil;
  except
    FEdit := nil;
  end;
  Result := FEdit;
end;

function TMark.GetIsBookmark: boolean;
begin
  Result := (fBookmarkNum >= 0);
end;

procedure TMark.SetIsBookmark(const Value: boolean);
begin
  if Value then fBookmarkNum := 0
  else fBookmarkNum := -1;
end;

procedure TMark.SetColumn(const Value: Integer);
begin
  FColumn := Value;
end;

procedure TMark.SetImage(const Value: Integer);
begin
  FImage := Value;
  if fVisible and Assigned(fEdit) then fEdit.InvalidateGutter(fLine, fLine);
end;

procedure TMark.SetInternalImage(const Value: boolean);
begin
  fInternalImage := Value;
  if fVisible and Assigned(fEdit) then fEdit.InvalidateGutter(fLine, fLine);
end;

procedure TMark.SetLine(const Value: Integer);
begin
  if fVisible and Assigned(fEdit) then begin
    if fLine > 0 then fEdit.InvalidateGutter(fLine, fLine);
    fLine := Value;
    fEdit.InvalidateGutter(fLine, fLine);
  end else
    fLine := Value;
end;

procedure TMark.SetVisible(const Value: boolean);
begin
  if fVisible <> Value then begin
    fVisible := Value;
    if Assigned(fEdit) then fEdit.InvalidateGutter(fLine, fLine);
  end;
end;

constructor TMark.Create(owner: TmwCustomEdit);
begin
  inherited Create;
  fEdit := owner;
end;

{ TMarkList }

function TMarkList.Add(Item: TMark): Integer;
begin
  Result := inherited Add(Item);
  DoChange;
end;

procedure TMarkList.ClearLine(Line: integer);
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    if not Items[i].IsBookmark and (Items[i].Line = Line) then Delete(i);
end;

constructor TMarkList.Create(owner: TmwCustomEdit);
begin
  inherited Create;
  fEdit := owner;
end;

procedure TMarkList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  DoChange;
end;

procedure TMarkList.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TMarkList.First: TMark;
begin
  result := inherited First;
end;

function TMarkList.Get(Index: Integer): TMark;
begin
  result := inherited Get(Index);
end;

//Returns up to maxMarks book/gutter marks for a chosen line.

procedure TMarkList.GetMarksForLine(line: integer; var marks: TMarks);
var
  cnt: integer;
  i: integer;
begin
  FillChar(marks, SizeOf(marks), 0);
  cnt := 0;
  for i := 0 to Count - 1 do begin
    if Items[i].Line = line then begin
      Inc(cnt);
      marks[cnt] := Items[i];
      if cnt = maxMarks then break;
    end;
  end;
end;

procedure TMarkList.Insert(Index: Integer; Item: TMark);
begin
  inherited Insert(Index, Item);
  DoChange;
end;

function TMarkList.Last: TMark;
begin
  result := inherited Last;
end;

procedure TMarkList.Place(mark: TMark);
begin
  if assigned(fEdit) then
    if assigned(fEdit.OnPlaceBookmark) then fEdit.OnPlaceBookmark(fEdit, mark);
  if assigned(mark) then
    Add(mark);
  DoChange;
end;

procedure TMarkList.Put(Index: Integer; Item: TMark);
begin
  inherited Put(Index, Item);
  DoChange;
end;

function TMarkList.Remove(Item: TMark): Integer;
begin
  result := inherited Remove(Item);
  DoChange;
end;

initialization
  mwEditClipboardFormat := RegisterClipboardFormat(MWEDIT_CLIPBOARD_FORMAT);
end.

