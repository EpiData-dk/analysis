unit history_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ActnList, Types, history;

type

  THistoryFormLineAction = procedure (Sender: TObject; LineText: UTF8String) of object;
  THistoryFormClearHistoryAction = procedure (Sender: TObject) of object;

  { THistoryForm }

  THistoryForm = class(TForm)
    CopyAllHistoryAction: TAction;
    CopySelectedHistoryAction: TAction;
    ClearHistoryAction: TAction;
    HistoryListActionList: TActionList;
    HistoryListBox: TListBox;
    HistoryPopupMenu: TPopupMenu;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    procedure ClearHistoryActionExecute(Sender: TObject);
    procedure CopyAllHistoryActionExecute(Sender: TObject);
    procedure CopySelectedHistoryActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

var
  HistoryForm: THistoryForm;

implementation

{$R *.lfm}

uses
  Clipbrd, LCLType, VirtualTrees, ana_procs;

{ THistoryForm }

procedure THistoryForm.HistoryListBoxDblClick(Sender: TObject);
begin
  if (HistoryListBox.ItemIndex <> -1) then
    DoLineAction(HistoryListBox.Items[HistoryListBox.ItemIndex]);
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

procedure THistoryForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  SaveFormPosition(Self, Self.Name);
end;

procedure THistoryForm.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(Self, Self.Name);
end;

procedure THistoryForm.FormShow(Sender: TObject);
begin
  LoadFormPosition(Self, Self.Name);
end;

procedure THistoryForm.HistoryListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ACanvas: TCanvas;
  TS: TTextStyle;
begin
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
      DoLineAction(HistoryListBox.Items[HistoryListBox.ItemIndex]);

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

constructor THistoryForm.Create(TheOwner: TComponent; History: THistory);
begin
  inherited Create(TheOwner);
  FHistory := History;
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

