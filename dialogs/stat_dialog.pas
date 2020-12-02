unit stat_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, auto_position_form, stat_dialog_contribution,
  ComCtrls, ExtCtrls, executor, stat_dialog_footer, StdCtrls;

type
  { TStatDialog }

  TStatDialog = class(TCustomAutoPositionForm, IStatDiaglogViewModified, IClickAction)
  private
    FButtonFooter: TStatDiaglogFooterPanel;
    FExcutor: TExecutor;
    FPageControl: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PageControlChange(Sender: TObject);
  private
    FViews: TStatDialogContributionViewList;
    FContribution: IStatDialogContribution;
    procedure SetupViews();
    procedure UpdateButtonPanel();
    procedure ResetViews();
  protected
    procedure ActionPerformed(Sender: TButton);
    procedure OnViewModified(DataModel: IStatDialogModel);
  public
    constructor Create(TheOwner: TComponent; Contribution: IStatDialogContribution;
      Executor: TExecutor);
  end;

var
  StatDialog: TStatDialog;

implementation

uses
  Controls, LCLType;

type

  { TDialogViewTabSheet }

  TDialogViewTabSheet = class(TTabSheet)
  private
    FDialogView: IStatDialogView;
    FIsDefined: boolean;
  public
    property DialogView: IStatDialogView read FDialogView write FDialogView;
    property IsDefined: boolean read FIsDefined write FIsDefined;
  end;

{ TStatDialog }

procedure TStatDialog.FormCreate(Sender: TObject);
begin
  Caption := FContribution.getCaption();
  SetupViews();
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
  FViews := FContribution.GetViews(self, FExcutor);
  for View in FViews do
  begin
    NewSheet := TDialogViewTabSheet.Create(FPageControl);
    NewSheet.PageControl := FPageControl;
    NewSheet.DialogView := View;
    NewSheet.Caption := View.getViewCaption;
    ViewControl := View.getControl();
    ViewControl.Parent := NewSheet;
    ViewControl.Align := alClient;

    View.SetOnModified(Self);
  end;
end;

procedure TStatDialog.UpdateButtonPanel();
var
  IsDefined: Boolean;
  i: Integer;
begin
  IsDefined := true;

  for i := 0 to FPageControl.PageCount - 1 do
    IsDefined := IsDefined and TDialogViewTabSheet(FPageControl.Pages[i]).IsDefined;

  if IsDefined then
    FButtonFooter.EnabledButtons := FButtonFooter.EnabledButtons + [sdbRun, sdbExecute]
  else
    FButtonFooter.EnabledButtons := FButtonFooter.EnabledButtons - [sdbRun, sdbExecute];
end;

procedure TStatDialog.ResetViews();
var
  View: IStatDialogView;
begin
  for View in FViews do
    View.ResetView();
end;

procedure TStatDialog.ActionPerformed(Sender: TButton);
begin
  case Sender.Tag of
    RUN_BUTTON_ID: ;
    EXECUTE_BUTTON_ID: ;
    PASTE_BUTTON_ID: ;
    HELP_BUTTON_ID: ;
    RESET_BUTTON_ID: ResetViews();
    CANCEL_BUTTON_ID: Close;
  end;
end;

constructor TStatDialog.Create(TheOwner: TComponent;
  Contribution: IStatDialogContribution; Executor: TExecutor);
begin
  inherited Create(TheOwner);

  FContribution := Contribution;
  FExcutor := Executor;

  FButtonFooter := TStatDiaglogFooterPanel.Create(self);
  FButtonFooter.Parent := self;
  FButtonFooter.OnCancelClick := self;
  FButtonFooter.OnExecuteClick := self;
  FButtonFooter.OnHelpClick := self;
  FButtonFooter.OnPasteClick := self;
  FButtonFooter.OnResetClick := self;
  FButtonFooter.OnRunClick := self;
  FButtonFooter.EnabledButtons := [sdbCancel, sdbHelp, sdbReset, sdbPaste];

  FPageControl := TPageControl.Create(self);
  FPageControl.Align := alClient;
  FPageControl.Parent := self;
  FPageControl.OnChanging := @PageControlChanging;
  FPageControl.OnChange := @PageControlChange;

  OnCreate := @FormCreate;
  OnKeyDown := @FormKeyDown;
  KeyPreview := true;
end;

procedure TStatDialog.OnViewModified(DataModel: IStatDialogModel);
begin
  TDialogViewTabSheet(FPageControl.ActivePage).IsDefined := DataModel.IsDefined();
  UpdateButtonPanel();
end;

end.

