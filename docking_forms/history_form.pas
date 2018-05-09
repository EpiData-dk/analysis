unit history_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  history, Types;

type

  { THistoryForm }

  THistoryForm = class(TForm)
    HistoryListBox: TListBox;
    HistoryPopupMenu: TPopupMenu;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    procedure HistoryListBoxDblClick(Sender: TObject);
    procedure HistoryListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure HistoryListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FHistory: THistory;
  public
    constructor Create(TheOwner: TComponent; AHistory: THistory);
    procedure UpdateHistory;
  end;

var
  HistoryForm: THistoryForm;

implementation

{$R *.lfm}

uses
  LCLType, Clipbrd;

{ THistoryForm }

procedure THistoryForm.HistoryListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ACanvas: TCanvas;
  TS: TTextStyle;
begin
  //  FHistory.Lines.Objects[Index];//
  ACanvas := HistoryListBox.Canvas;
  ACanvas.FillRect(ARect);

  if (Index < 0) or (Index > HistoryListBox.Count) then exit;

  if FHistory.Custom[Index] then
    ACanvas.Font.Color := clBlue;

  if FHistory.Failed[Index] then
    ACanvas.Font.Color := clRed;

  TS := ACanvas.TextStyle;
  TS.Layout := tlCenter;
  ACanvas.TextStyle := TS;
  ACanvas.TextRect(ARect, ARect.Left + 2, ARect.Top, HistoryListBox.Items[Index]);
end;

procedure THistoryForm.HistoryListBoxDblClick(Sender: TObject);
begin
  if HistoryListBox.ItemIndex <> -1 then
    begin
{      FCmdEdit.Text := HistoryListBox.Items[HistoryListBox.ItemIndex];

      Application.QueueAsyncCall(@DelayCmdEditFocus, 0);       }
    end;
end;

procedure THistoryForm.HistoryListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  S: String;
begin
  if (Key = VK_RETURN) and
     (Shift = [])
  then
    if HistoryListBox.ItemIndex <> -1 then
      begin
{        FCmdEdit.Text := HistoryListBox.Items[HistoryListBox.ItemIndex];
        if FCmdEdit.CanFocus then
          FCmdEdit.SetFocus; }
      end;

  if (Key = VK_C) and
     (Shift = [ssCtrl])
  then
    if HistoryListBox.ItemIndex <> -1 then
      begin
        S := '';
        for i := 0 to HistoryListBox.Count - 1 do
          if (HistoryListBox.Selected[i]) then
            S := S + LineEnding + HistoryListBox.Items[i];

        S := TrimLeft(S);
        Clipboard.AsText := S;
      end;
end;

constructor THistoryForm.Create(TheOwner: TComponent; AHistory: THistory);
begin
  inherited Create(TheOwner);
  FHistory := AHistory;
end;

procedure THistoryForm.UpdateHistory;
begin
  HistoryListBox.Items.BeginUpdate;

  HistoryListBox.Clear;
  HistoryListBox.Items.Assign(FHistory.Lines);

  HistoryListBox.Items.EndUpdate;
  HistoryListBox.TopIndex := HistoryListBox.Items.Count - 1;
end;

end.

