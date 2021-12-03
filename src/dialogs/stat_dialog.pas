unit stat_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, auto_position_form, stat_dialog_contribution,
  ComCtrls, ExtCtrls, executor, stat_dialog_footer, StdCtrls, script_runner,
  Controls;

type
  { TStatDialog }

  TStatDialog = class(TCustomAutoPositionForm, IStatDiaglogViewModified, IClickAction)
  private
    FButtonFooter: TStatDiaglogFooterPanel;
    FExecutor: TExecutor;
    FPageControl: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HintWindowClick(Sender: TObject);
    procedure HintWindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PageControlChange(Sender: TObject);
  private
    FScriptRunner: IScriptMediator;
    FViews: TStatDialogContributionViewList;
    FContribution: IStatDialogContribution;
    procedure SetScriptRunner(AValue: IScriptMediator);
    procedure SetupViews();
    procedure UpdateButtonPanel();
  private
    // Actions
    procedure ExecuteScript(CloseAfterRun: Boolean);
    procedure PasteScript();
    procedure ShowHelp();
  protected
    procedure ActionPerformed(Sender: TButton);
    procedure OnViewModified(View: IStatDialogView);
  public
    constructor Create(TheOwner: TComponent; Contribution: IStatDialogContribution; Executor: TExecutor);
    procedure ResetViews();
    property ScriptRunner: IScriptMediator read FScriptRunner write SetScriptRunner;
  end;

var
  StatDialog: TStatDialog;

implementation

uses
  LCLType, LCLProc, Forms;

type

  { TDialogViewTabSheet }

  TDialogViewTabSheet = class(TTabSheet)
  private
    FDialogView: IStatDialogView;
  public
    property DialogView: IStatDialogView read FDialogView write FDialogView;
  end;

{ TStatDialog }

procedure TStatDialog.FormCreate(Sender: TObject);
begin
  Caption := FContribution.getCaption();

  SetupViews();
  UpdateButtonPanel();
end;

procedure TStatDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then
    begin
      Close;
      Exit;
    end;
end;

procedure TStatDialog.HintWindowClick(Sender: TObject);
begin
  //
end;

procedure TStatDialog.HintWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  THintWindow(Sender).Close;
end;

procedure TStatDialog.OKButtonClick(Sender: TObject);
begin
  FContribution.generateScript();
end;

procedure TStatDialog.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange := TDialogViewTabSheet(FPageControl.ActivePage).DialogView.exitView();
end;

procedure TStatDialog.PageControlChange(Sender: TObject);
begin
  TDialogViewTabSheet(FPageControl.ActivePage).DialogView.enterView();
end;

procedure TStatDialog.SetupViews();
var
  View: IStatDialogView;
  NewSheet: TDialogViewTabSheet;
  ViewControl: TControl;
begin
  FViews := FContribution.GetViews(self, FExecutor);
  for View in FViews do
  begin
    NewSheet := TDialogViewTabSheet.Create(FPageControl);
    NewSheet.PageControl := FPageControl;
    NewSheet.DialogView := View;
    NewSheet.Caption := View.GetViewCaption();
    ViewControl := View.GetControl();
    ViewControl.Parent := NewSheet;
    ViewControl.Align := alClient;

    View.AddOnModified(Self);
  end;

  ResetViews();
end;

procedure TStatDialog.SetScriptRunner(AValue: IScriptMediator);
begin
  if FScriptRunner = AValue then Exit;
  FScriptRunner := AValue;
end;

procedure TStatDialog.UpdateButtonPanel();
var
  IsDefined: Boolean;
  i: Integer;
begin
  IsDefined := true;

  for i := 0 to FPageControl.PageCount - 1 do
    IsDefined := IsDefined and TDialogViewTabSheet(FPageControl.Pages[i]).DialogView.IsDefined();

  if IsDefined then
    FButtonFooter.EnabledButtons := FButtonFooter.EnabledButtons + [sdbRun, sdbExecute, sdbPaste]
  else
    FButtonFooter.EnabledButtons := FButtonFooter.EnabledButtons - [sdbRun, sdbExecute, sdbPaste];
end;

procedure TStatDialog.ResetViews();
var
  View: IStatDialogView;
begin
  for View in FViews do
    View.ResetView();
end;

procedure TStatDialog.ExecuteScript(CloseAfterRun: Boolean);
begin
  if (Assigned(ScriptRunner)) then
    ScriptRunner.RunScript(FContribution.GenerateScript());

  if (CloseAfterRun) then
    Close;
end;

procedure TStatDialog.PasteScript();
begin
  if (Assigned(ScriptRunner)) then
    ScriptRunner.PasteScript(FContribution.GenerateScript());
end;

procedure TStatDialog.ShowHelp();
var
  HintWindow: THintWindow;
  HintRect: TRect;
  HintPoint: TPoint;
begin
  HintWindow := THintWindow.Create(self);
  HintWindow.HideInterval := 5000;
  HintWindow.OnMouseDown := @HintWindowMouseDown;
  HintWindow.OnClick := @HintWindowClick;

  HintRect := HintWindow.CalcHintRect(0, FContribution.GetHelpText(), nil);
  HintPoint := ClientToScreen(Point(FButtonFooter.Left, FButtonFooter.Top));
  OffsetRect(HintRect, HintPoint.X, HintPoint.Y - HintRect.Bottom);
  HintWindow.ActivateHint(HintRect, FContribution.GetHelpText());
end;

procedure TStatDialog.ActionPerformed(Sender: TButton);
begin
  case Sender.Tag of
    RUN_BUTTON_ID:     ExecuteScript(true);
    EXECUTE_BUTTON_ID: ExecuteScript(false);
    PASTE_BUTTON_ID:   PasteScript();
    HELP_BUTTON_ID:    ShowHelp();
    RESET_BUTTON_ID:   ResetViews();
    CANCEL_BUTTON_ID:  Close;
  end;
end;

constructor TStatDialog.Create(TheOwner: TComponent;
  Contribution: IStatDialogContribution; Executor: TExecutor);
begin
  inherited Create(TheOwner);

  FContribution := Contribution;
  FExecutor := Executor;

  FButtonFooter := TStatDiaglogFooterPanel.Create(self);
  FButtonFooter.Parent := self;
  FButtonFooter.OnCancelClick := self;
  FButtonFooter.OnExecuteClick := self;
  FButtonFooter.OnHelpClick := self;
  FButtonFooter.OnPasteClick := self;
  FButtonFooter.OnResetClick := self;
  FButtonFooter.OnRunClick := self;
  FButtonFooter.EnabledButtons := [sdbCancel, sdbHelp, sdbReset];

  FPageControl := TPageControl.Create(self);
  FPageControl.Align := alClient;
  FPageControl.Parent := self;
  FPageControl.OnChanging := @PageControlChanging;
  FPageControl.OnChange := @PageControlChange;

  OnCreate := @FormCreate;
  OnKeyDown := @FormKeyDown;
  KeyPreview := true;
end;

procedure TStatDialog.OnViewModified(View: IStatDialogView);
begin
  UpdateButtonPanel();
end;

end.

