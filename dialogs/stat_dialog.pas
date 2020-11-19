unit stat_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  ExtCtrls,
  stat_dialog_contribution;

type

  { TStatDialog }

  TStatDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    PageControl1: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FContribution: IStatDialogContribution;
    procedure SetupViews;
  public
    constructor Create(TheOwner: TComponent; Contribution: IStatDialogContribution);
  end;

var
  StatDialog: TStatDialog;

implementation

{$R *.lfm}

{ TStatDialog }

procedure TStatDialog.FormCreate(Sender: TObject);
begin
  Caption := FContribution.getCaption();
end;

procedure TStatDialog.OKButtonClick(Sender: TObject);
begin
  FContribution.generateScript();
end;

procedure TStatDialog.SetupViews;
var
  View: TPanel;
  NewSheet: TTabSheet;
begin
  for View in FContribution.getViews(self) do
  begin
    NewSheet := PageControl1.AddTabSheet;
    View.Parent := NewSheet;
    View.Align := alClient;
  end;
end;

constructor TStatDialog.Create(TheOwner: TComponent;
  Contribution: IStatDialogContribution);
begin
  inherited Create(TheOwner);

  FContribution := Contribution;
end;

end.

