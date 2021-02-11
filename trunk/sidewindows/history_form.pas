unit history_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ActnList, Types, history, auto_position_form;

type

  THistoryFormLineAction = procedure (Sender: TObject; LineText: UTF8String) of object;
  THistoryFormClearHistoryAction = procedure (Sender: TObject) of object;

  { THistoryForm }

  THistoryForm = class(TCustomAutoPositionForm)
  private
    FHistoryListBox: TListBox;
    procedure ClearHistoryActionExecute(Sender: TObject);
    procedure CopyAllHistoryActionExecute(Sender: TObject);
    procedure CopySelectedHistoryActionExecute(Sender: TObject);
    procedure HistoryListBoxDblClick(Sender: TObject);
    procedure HistoryListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure HistoryListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FHistory: THistory;
    FOnClearHistoryAction: THistoryFormClearHistoryAction;
    FOnLineAction: THistoryFormLineAction;
    procedure DoLineAction(LineText: UTF8String);
    procedure DoClearHistoryAction;
    procedure DoCopySelectedHistory;
  public
    constructor Create(TheOwner: TComponent; History: THistory);
    procedure UpdateHistory;
    property OnLineAction: THistoryFormLineAction read FOnLineAction write FOnLineAction;
    property OnClearHistoryAction: THistoryFormClearHistoryAction read FOnClearHistoryAction write FOnClearHistoryAction;
  end;

implementation

uses
  Clipbrd, LCLType, VirtualTrees, ana_procs;

{ THistoryForm }

procedure THistoryForm.HistoryListBoxDblClick(Sender: TObject);
begin
  if (FHistoryListBox.ItemIndex <> -1) then
    DoLineAction(FHistoryListBox.Items[FHistoryListBox.ItemIndex]);
end;

procedure THistoryForm.CopyAllHistoryActionExecute(Sender: TObject);
begin
  Clipboard.AsText := FHistory.Lines.Text
end;

procedure THistoryForm.ClearHistoryActionExecute(Sender: TObject);
begin
  DoClearHistoryAction;
end;

procedure THistoryForm.CopySelectedHistoryActionExecute(Sender: TObject);
begin
  DoCopySelectedHistory;
end;

procedure THistoryForm.HistoryListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ACanvas: TCanvas;
  TS: TTextStyle;
begin
  ACanvas := FHistoryListBox.Canvas;
  ACanvas.FillRect(ARect);

  if (Index < 0) or (Index > FHistoryListBox.Count) then exit;

  if FHistory.Custom[Index] then
    ACanvas.Font.Color := clBlue;

  if FHistory.Failed[Index] then
    ACanvas.Font.Color := clRed;

  TS := ACanvas.TextStyle;
  TS.Layout := tlCenter;
  ACanvas.TextStyle := TS;
  ACanvas.TextRect(ARect, ARect.Left + 2, ARect.Top, FHistoryListBox.Items[Index]);
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
    if FHistoryListBox.ItemIndex <> -1 then
      DoLineAction(FHistoryListBox.Items[FHistoryListBox.ItemIndex]);

  if (Key = VK_C) and
     (Shift = [ssCtrlOS])
  then
    DoCopySelectedHistory;
end;

procedure THistoryForm.DoLineAction(LineText: UTF8String);
begin
  if (Assigned(OnLineAction)) then
    OnLineAction(self, LineText);
end;

procedure THistoryForm.DoClearHistoryAction;
begin
  if (Assigned(OnClearHistoryAction)) then
    OnClearHistoryAction(self);
end;

procedure THistoryForm.DoCopySelectedHistory;
var
  S: String;
  i: Integer;
begin
  if FHistoryListBox.ItemIndex <> -1 then
    begin
      S := '';
      for i := 0 to FHistoryListBox.Count - 1 do
        if (FHistoryListBox.Selected[i]) then
          S := S + LineEnding + FHistoryListBox.Items[i];

      S := TrimLeft(S);
      Clipboard.AsText := S;
    end;
end;

constructor THistoryForm.Create(TheOwner: TComponent; History: THistory);
var
  HistoryPopupMenu: TPopupMenu;

  procedure CreateActionAndMenuItem(Caption: string; ExecuteMethod: TNotifyEvent);
  var
    AAction: TAction;
    MenuItem: TMenuItem;
  begin
    AAction := TAction.Create(Self);
    AAction.Caption := Caption;
    AAction.OnExecute := ExecuteMethod;

    MenuItem := TMenuItem.Create(HistoryPopupMenu);
    MenuItem.Action := AAction;

    HistoryPopupMenu.Items.Add(MenuItem);
  end;

begin
  inherited Create(TheOwner);

  Caption := 'History';

  HistoryPopupMenu := TPopupMenu.Create(Self);
  CreateActionAndMenuItem('Copy all to clipboard', @CopyAllHistoryActionExecute);
  CreateActionAndMenuItem('Copy selected to clipboard', @CopySelectedHistoryActionExecute);
  CreateActionAndMenuItem('Clear History', @ClearHistoryActionExecute);

  FHistoryListBox := TListBox.Create(Self);
  with FHistoryListBox do
  begin
    Parent      := Self;
    Align       := alClient;
    MultiSelect := True;
    OnDblClick  := @HistoryListBoxDblClick;
    OnDrawItem  := @HistoryListBoxDrawItem;
    OnKeyDown   := @HistoryListBoxKeyDown;
    PopupMenu   := HistoryPopupMenu;
    Style       := lbOwnerDrawFixed;
  end;

  FHistory := History;
end;

procedure THistoryForm.UpdateHistory;
begin
  FHistoryListBox.Items.BeginUpdate;

  FHistoryListBox.Clear;
  FHistoryListBox.Items.Assign(FHistory.Lines);

  FHistoryListBox.Items.EndUpdate;
  FHistoryListBox.TopIndex := FHistoryListBox.Items.Count - 1;
end;

end.

