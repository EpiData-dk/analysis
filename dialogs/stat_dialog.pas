unit stat_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, auto_position_form, stat_dialog_contribution, ButtonPanel,
  ComCtrls, ExtCtrls;

type
  { TStatDialog }

  TStatDialog = class(TCustomAutoPositionForm)
  private
    ButtonPanel: TButtonPanel;
    PageControl: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PageControlChange(Sender: TObject);
  private
    FContribution: IStatDialogContribution;
    procedure SetupViews;
  public
    constructor Create(TheOwner: TComponent; Contribution: IStatDialogContribution);
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
  public
    property DialogView: IStatDialogView read FDialogView write FDialogView;
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
  AllowChange := TDialogViewTabSheet(PageControl.ActivePage).DialogView.exitView();
end;

procedure TStatDialog.PageControlChange(Sender: TObject);
begin
  TDialogViewTabSheet(PageControl.ActivePage).DialogView.enterView();
end;

procedure TStatDialog.SetupViews;
var
  View: IStatDialogView;
  NewSheet: TDialogViewTabSheet;
  ViewControl: TControl;
begin
  for View in FContribution.getViews(self) do
  begin
    NewSheet := TDialogViewTabSheet.Create(PageControl);
    NewSheet.PageControl := PageControl;
    NewSheet.DialogView := View;
    NewSheet.Caption := View.getViewCaption;
    ViewControl := View.getControl();
    ViewControl.Parent := NewSheet;
    ViewControl.Align := alClient;
  end;
end;

constructor TStatDialog.Create(TheOwner: TComponent;
  Contribution: IStatDialogContribution);
begin
  inherited Create(TheOwner);

  FContribution := Contribution;
  ButtonPanel := TButtonPanel.Create(self);
  ButtonPanel.Parent := self;

  PageControl := TPageControl.Create(self);
  PageControl.Align := alClient;
  PageControl.Parent := self;
  PageControl.OnChanging := @PageControlChanging;
  PageControl.OnChange := @PageControlChange;

  OnCreate := @FormCreate;
end;

end.

