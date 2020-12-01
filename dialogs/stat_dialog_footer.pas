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

  { TStatDiaglogFooterPanel }

  TStatDiaglogFooterPanel = class(TCustomPanel)
  private
    FOnCancelClick: TClickAction;
    FOnExecuteClick: TClickAction;
    FOnHelpClick: TClickAction;
    FOnPasteClick: TClickAction;
    FOnResetClick: TClickAction;
    FOnRunClick: TClickAction;
    procedure ButtonClick(Sender: TObject);
  private
    FRunButton: TButton;
    FExecuteButton: TButton;
    FPasteButton: TButton;
    FHelpButton: TButton;
    FResetButton: TButton;
    FCancelButton: TButton;
  public
    constructor Create(TheOwner: TComponent); override;
    property OnRunClick: TClickAction read FOnRunClick write FOnRunClick;
    property OnExecuteClick: TClickAction read FOnExecuteClick write FOnExecuteClick;
    property OnCancelClick: TClickAction read FOnCancelClick write FOnCancelClick;
    property OnHelpClick: TClickAction read FOnHelpClick write FOnHelpClick;
    property OnResetClick: TClickAction read FOnResetClick write FOnResetClick;
    property OnPasteClick: TClickAction read FOnPasteClick write FOnPasteClick;
  end;

implementation

uses
  Controls;

{ TStatDiaglogFooterPanel }

procedure TStatDiaglogFooterPanel.ButtonClick(Sender: TObject);
var
  Button: TButton;
  ClickAction: TClickAction;
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

constructor TStatDiaglogFooterPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Align      := alBottom;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Caption    := '';
  AutoSize   := True;

  FRunButton := TButton.Create(self);
  FRunButton.Caption := '&Run';
  FRunButton.Tag := RUN_BUTTON_ID;
  FRunButton.OnClick := @ButtonClick;
  FRunButton.AnchorParallel(akTop, 10, self);
  FRunButton.AnchorParallel(akBottom, 10, self);
  FRunButton.AnchorParallel(akLeft, 10, self);
  FRunButton.AutoSize := true;
  FRunButton.Parent := Self;

  FExecuteButton := TButton.Create(self);
  FExecuteButton.Caption := '&Execute';
  FExecuteButton.Tag := EXECUTE_BUTTON_ID;
  FExecuteButton.OnClick := @ButtonClick;
  FExecuteButton.AnchorParallel(akTop, 10, self);
  FExecuteButton.AnchorParallel(akBottom, 10, self);
  FExecuteButton.AnchorToNeighbour(akLeft, 5, FRunButton);
  FExecuteButton.AutoSize := true;
  FExecuteButton.Parent := Self;

  FPasteButton := TButton.Create(self);
  FPasteButton.Caption := '&Paste';
  FPasteButton.Tag := PASTE_BUTTON_ID;
  FPasteButton.OnClick := @ButtonClick;
  FPasteButton.AnchorParallel(akTop, 10, self);
  FPasteButton.AnchorParallel(akBottom, 10, self);
  FPasteButton.AnchorToNeighbour(akLeft, 5, FExecuteButton);
  FPasteButton.AutoSize := true;
  FPasteButton.Parent := Self;

  FHelpButton := TButton.Create(self);
  FHelpButton.Caption := '&?';
  FHelpButton.Tag := HELP_BUTTON_ID;
  FHelpButton.OnClick := @ButtonClick;
  FHelpButton.AnchorParallel(akTop, 10, self);
  FHelpButton.AnchorParallel(akBottom, 10, self);
  FHelpButton.AnchorToNeighbour(akLeft, 5, FPasteButton);
  FHelpButton.AutoSize := true;
  FHelpButton.Parent := Self;

end;

end.

