unit stat_dialog_footer;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls;

const
  RUN_BUTTON_ID = 0;
  EXECUTE_BUTTON_ID = 1;
  PASTE_BUTTON_ID = 2;
  HELP_BUTTON_ID = 3;
  RESET_BUTTON_ID = 4;
  CANCEL_BUTTON_ID = 5;

type
  IClickAction = interface['{B4B7132F-0790-B559-8DFD-0BB62C05C88F}']
    procedure ActionPerformed(Sender: TButton);
  end;

  TStatDialogButton = (sdbRun, sdbExecute, sdbCancel, sdbHelp, sdbReset, sdbPaste);
  TStatDialogButtons = set of TStatDialogButton;

  { TStatDiaglogFooterPanel }

  TStatDiaglogFooterPanel = class(TCustomPanel)
  private
    FOnCancelClick: IClickAction;
    FOnExecuteClick: IClickAction;
    FOnHelpClick: IClickAction;
    FOnPasteClick: IClickAction;
    FOnResetClick: IClickAction;
    FOnRunClick: IClickAction;
    procedure ButtonClick(Sender: TObject);
  private
    FEnabledButtons: TStatDialogButtons;
    FButtons: array[TStatDialogButton] of TButton;
    procedure SetEnabledButtons(AValue: TStatDialogButtons);
    procedure InitButtons();
  public
    constructor Create(TheOwner: TComponent); override;
    property OnRunClick: IClickAction read FOnRunClick write FOnRunClick;
    property OnExecuteClick: IClickAction read FOnExecuteClick write FOnExecuteClick;
    property OnCancelClick: IClickAction read FOnCancelClick write FOnCancelClick;
    property OnHelpClick: IClickAction read FOnHelpClick write FOnHelpClick;
    property OnResetClick: IClickAction read FOnResetClick write FOnResetClick;
    property OnPasteClick: IClickAction read FOnPasteClick write FOnPasteClick;
    property EnabledButtons: TStatDialogButtons read FEnabledButtons write SetEnabledButtons;
  end;

implementation

uses
  Controls;

{ TStatDiaglogFooterPanel }

procedure TStatDiaglogFooterPanel.ButtonClick(Sender: TObject);
var
  Button: TButton;
  ClickAction: IClickAction;
begin
  Button := TButton(Sender);
  case Button.Tag of
    RUN_BUTTON_ID: ClickAction     := FOnRunClick;
    EXECUTE_BUTTON_ID: ClickAction := FOnExecuteClick;
    PASTE_BUTTON_ID: ClickAction   := FOnPasteClick;
    HELP_BUTTON_ID: ClickAction    := FOnHelpClick;
    RESET_BUTTON_ID: ClickAction   := FOnResetClick;
    CANCEL_BUTTON_ID: ClickAction  := FOnCancelClick;
  end;

  if (Assigned(ClickAction)) then
    ClickAction.ActionPerformed(Button);
end;

procedure TStatDiaglogFooterPanel.SetEnabledButtons(AValue: TStatDialogButtons);
var
  Item: TStatDialogButton;
begin
  if FEnabledButtons = AValue then Exit;
  FEnabledButtons := AValue;

  DisableAutoSizing;

  for Item in TStatDialogButton do
    FButtons[Item].Enabled := false;

  for Item in FEnabledButtons do
    FButtons[Item].Enabled := true;

  EnableAutoSizing;
end;

procedure TStatDiaglogFooterPanel.InitButtons();
var
  Button: TButton;

  function InitButton(StatDialogButton: TStatDialogButton; Id: PtrInt; Caption: UTF8String): TButton;
  begin
    Result := TButton.Create(self);
    Result.Caption := Caption;
    Result.Tag := Id;
    Result.OnClick := @ButtonClick;
    Result.AutoSize := true;
    Result.Parent := Self;
    Result.Anchors := [];
    FButtons[StatDialogButton] := Result;
  end;

begin
  Button := InitButton(sdbRun, RUN_BUTTON_ID, '&Run');
  Button.AnchorParallel(akTop, 10, self);
  Button.AnchorParallel(akBottom, 10, self);
  Button.AnchorParallel(akLeft, 10, self);

  Button := InitButton(sdbExecute, EXECUTE_BUTTON_ID, '&Execute');
  Button.AnchorToNeighbour(akLeft, 5, FButtons[sdbRun]);
  Button.AnchorParallel(akTop, 10, self);
  Button.AnchorParallel(akBottom, 10, self);

  Button := InitButton(sdbPaste, PASTE_BUTTON_ID, '&Paste');
  Button.AnchorToNeighbour(akLeft, 5, FButtons[sdbExecute]);
  Button.AnchorParallel(akTop, 10, self);
  Button.AnchorParallel(akBottom, 10, self);

  Button := InitButton(sdbHelp, HELP_BUTTON_ID, '&?');
  Button.AnchorParallel(akTop, 10, self);
  Button.AnchorParallel(akBottom, 10, self);
  Button.AnchorToNeighbour(akLeft, 5, FButtons[sdbPaste]);

  Button := InitButton(sdbCancel, CANCEL_BUTTON_ID, '&Cancel');
  Button.AnchorParallel(akTop, 10, self);
  Button.AnchorParallel(akBottom, 10, self);
  Button.AnchorParallel(akRight, 10, self);

  Button := InitButton(sdbReset, RESET_BUTTON_ID, '&Reset');
  Button.AnchorParallel(akTop, 10, self);
  Button.AnchorParallel(akBottom, 10, self);
  Button.AnchorToNeighbour(akRight, 5, FButtons[sdbCancel]);
end;

constructor TStatDiaglogFooterPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Align      := alBottom;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Caption    := '';
  AutoSize   := True;

  InitButtons();

  EnabledButtons := [sdbRun, sdbExecute, sdbCancel, sdbHelp, sdbReset, sdbPaste];
end;

end.

