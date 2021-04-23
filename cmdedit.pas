unit cmdedit;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, history,
  executor, ExtCtrls, LCLType;

type

  TCmdEditRunCommandEvent = function(Sender: TObject; Const S: UTF8String): boolean of object;

  { TCmdEdit }

  TCmdEdit = class(TCustomEdit, IFPObserver)
  private
    // POP-UP variable list
    FPrefix: UTF8String;
    FPanel: TPanel;
    FListBox: TListBox;
    FOldCaretPos: TPoint;
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ListBoxExit(Sender: TObject);
  private
    FHistIdx: Integer;
    FHistory: THistory;
    FOnRunCommand: TCmdEditRunCommandEvent;
    FExecutor: TExecutor;
    procedure SetHistory(AValue: THistory);
    procedure SetOnRunCommand(AValue: TCmdEditRunCommandEvent);
    procedure DoOnRunCommand(Const S: UTF8String);
    procedure SeekHistory(Counts: Integer);
    function  LookupIdentifier(Const prefix: UTF8String): boolean;
    procedure SetExecutor(AValue: TExecutor);
  protected
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property OnRunCommand: TCmdEditRunCommandEvent read FOnRunCommand write SetOnRunCommand;
    property History: THistory read FHistory write SetHistory;
    property Executor: TExecutor read FExecutor write SetExecutor;
  end;

implementation

uses
  LazUTF8, Graphics, LCLIntf, Forms, math;

{ TCmdEdit }

procedure TCmdEdit.SetOnRunCommand(AValue: TCmdEditRunCommandEvent);
begin
  if FOnRunCommand = AValue then Exit;
  FOnRunCommand := AValue;
end;

procedure TCmdEdit.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S, T: String;
begin

  case Key of
    VK_TAB,
    VK_ESCAPE:
      begin
        if CanFocus then
          SetFocus;

        Key := VK_UNKNOWN;
      end;

    VK_RETURN:
      begin
        if CanFocus then
          SetFocus;

        S := Text;
        T := copy(S, 1, FOldCaretPos.X - UTF8Length(FPrefix)) +
             FListBox.Items[FListBox.ItemIndex] +
             copy(S, FOldCaretPos.X + 1, Length(S));

        Text := T;
        SelText := '';
        CaretPos := Point(FOldCaretPos.X - UTF8Length(FPrefix) + Length(FListBox.Items[FListBox.ItemIndex]), 1);
      end;
  end;
end;

procedure TCmdEdit.ListBoxExit(Sender: TObject);
begin
  if Assigned(FPanel) then
    Application.ReleaseComponent(FPanel);

  FPanel := nil;
end;

procedure TCmdEdit.SetHistory(AValue: THistory);
begin
  if FHistory = AValue then Exit;
  FHistory := AValue;
  FHistory.Lines.FPOAttachObserver(Self);
end;

procedure TCmdEdit.DoOnRunCommand(const S: UTF8String);
var
  St: String;
begin
  if (S = '') then
    Exit;

  St := UTF8Trim(S, []);
  if (St[Length(St)] <> ';') then
    St := St + ';';

  FHistory.AddLine(St);

  if Assigned(OnRunCommand) and
     OnRunCommand(Self, St)
  then
    Text := '';
end;

procedure TCmdEdit.SeekHistory(Counts: Integer);
begin
  if (not Assigned(History)) then exit;

  FHistIdx := FHistIdx + Counts;

  if (FHistIdx < 0) then
    FHistIdx := 0;

  if (FHistIdx >= History.Lines.Count) then
    FHistIdx := History.Lines.Count;

  if (FHistIdx = History.Lines.Count) then
    Text := ''
  else
    Text := FHistory.Lines[FHistIdx];

  CaretPos := Point(Length(Text), 1);
end;

function TCmdEdit.LookupIdentifier(const prefix: UTF8String): boolean;
var
  L: TStrings;
  Len, StartPos: PtrInt;
  S: String;
  W: Integer;
  P: TPoint;
  BM: TBitmap;
begin
  result := false;

  L := FExecutor.VariableNamesFromPrefix(prefix);

  if (L.Count = 0) then
    exit;

  Len := UTF8Length(prefix);
  StartPos := CaretPos.X - Len;

  if (L.Count = 1) then
  begin
    result := true;

    SelStart := StartPos;
    SelLength := Len;

    SelText := L[0];

    Exit;
  end;

  FOldCaretPos := CaretPos;
  FPrefix := prefix;

  S := Text;
  S := Copy(S, 1, CaretPos.X);

  BM := TBitmap.Create;
  BM.Canvas.Font.Assign(Self.Font);
  W := BM.Canvas.GetTextWidth(S);
  BM.Free;

  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self.Parent;
  FPanel.Left := W + 5;
  FPanel.Anchors := [];
  FPanel.AnchorParallel(akBottom, 5, Self);
  FPanel.BorderStyle := bsNone;
  FPanel.BringToFront;


  FListBox := TListBox.Create(FPanel);
  for S in L do
    FListBox.Items.Add(S);

  FListBox.Parent := FPanel;
  FListBox.Align :=  alClient;
  FListBox.ExtendedSelect := false;
  FListBox.OnKeyDown := @ListBoxKeyDown;
  FListBox.OnExit := @ListBoxExit;
  FListBox.TopIndex := 0;
  FListBox.ItemIndex := 0;

  FPanel.Height := FListBox.ItemHeight * (Min(FListBox.Count, 5)) + 6;
  FPanel.Width  := FListBox.ItemRect(0).Right + 2;

  FListBox.SetFocus;
end;

procedure TCmdEdit.SetExecutor(AValue: TExecutor);
begin
  if FExecutor = AValue then Exit;
  FExecutor := AValue;
end;

procedure TCmdEdit.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  // In here we detect additions to lines in THistory

  case Operation of
    ooChange,
    ooAddItem,
    ooDeleteItem:
      // This will position the seek index at 1 point after the last entry.
      FHistIdx := FHistory.Lines.Count;

    ooFree: ;
    ooCustom: ;
  end;
end;


procedure TCmdEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  StartPos, CurrPos: LongInt;
  S: TCaption;
begin
  if (Shift = []) then
    begin
      case Key of
        VK_RETURN:
          begin
            DoOnRunCommand(Text);
            Key := 0;
          end;

        VK_UP:
          SeekHistory(-1);

        VK_DOWN:
          SeekHistory(1);

        VK_PRIOR:
          SeekHistory(-10);

        VK_NEXT:
          SeekHistory(10);

        VK_TAB:
          begin
            StartPos := CaretPos.X;
            CurrPos  := StartPos;
            S := Text;

            while (CurrPos > 0) and
                  (S[CurrPos] in ['a'..'z', 'A'..'Z', '0'..'9', '$', '_'])
            do
              Dec(CurrPos);

            S := Copy(S, CurrPos + 1, StartPos - CurrPos);

            LookupIdentifier(S);
            Key := VK_UNKNOWN;
          end;

        VK_ESCAPE:
          Text := '';
      else
        inherited KeyDown(Key, Shift);
      end;
    end;

  if (Shift = [ssAlt]) and
     (Key = VK_N)
  then
    begin
      StartPos := CaretPos.X;

      S := Text;
      S := copy(S, 1, CaretPos.X) + '[_n]' + copy(S, CaretPos.X + 1, Length(S));

      Text := S;
      CaretPos := Point(StartPos + 4, 1);
    end;
end;

constructor TCmdEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHistIdx := 0;
  AutoSelect := false;
end;

end.

