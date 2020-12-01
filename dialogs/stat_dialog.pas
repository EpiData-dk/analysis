unit stat_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, auto_position_form, stat_dialog_contribution, ButtonPanel,
  ComCtrls, ExtCtrls, executor, stat_dialog_footer;

type
  { TStatDialog }

  TStatDialog = class(TCustomAutoPositionForm, IStatDiaglogViewModified, IClickAction)
  private
    FButtonFooter: TStatDiaglogFooterPanel;
    FButtonPanel: TButtonPanel;
    FExcutor: TExecutor;
    FPageControl: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PageControlChange(Sender: TObject);
  private
    FContribution: IStatDialogContribution;
    procedure SetupViews;
    procedure UpdateButtonPanel;
  public
    procedure ActionPerformed(Sender: TButton);
    constructor Create(TheOwner: TComponent; Contribution: IStatDialogContribution;
      Executor: TExecutor);
    procedure OnViewModified(DataModel: IStatDialogModel);
  end;

var
  StatDialog: TStatDialog;

implementation

uses
  Controls;

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

  SetupViews;
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

procedure TStatDialog.SetupViews;
var
  View: IStatDialogView;
  NewSheet: TDialogViewTabSheet;
  ViewControl: TControl;
begin
  for View in FContribution.GetViews(self, FExcutor) do
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

procedure TStatDialog.UpdateButtonPanel;
var
  OkEnabled: Boolean;
  i: Integer;
begin
  OkEnabled := true;

  for i := 0 to FPageControl.PageCount - 1 do
    OkEnabled := OkEnabled and TDialogViewTabSheet(FPageControl.Pages[i]).IsDefined;

  FButtonPanel.OKButton.Enabled := OkEnabled;
end;

procedure TStatDialog.ActionPerformed(Sender: TButton);
begin
  //
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

  FPageControl := TPageControl.Create(self);
  FPageControl.Align := alClient;
  FPageControl.Parent := self;
  FPageControl.OnChanging := @PageControlChanging;
  FPageControl.OnChange := @PageControlChange;

  OnCreate := @FormCreate;
end;

procedure TStatDialog.OnViewModified(DataModel: IStatDialogModel);
begin
  TDialogViewTabSheet(FPageControl.ActivePage).IsDefined := DataModel.IsDefined();
  UpdateButtonPanel;
end;

end.

